#include <string_view>
#include <sstream>
#include <filesystem>
#include "common/context.hpp"
#include "common/logger.hpp"
#include "common/errorsystem.hpp"
#include "common/perf.hpp"
#include "common/utils.hpp"
#include "common/qualname.hpp"
#include "module/module.hpp"
#include "tokens/lexer.hpp"
#include "ast/parser.hpp"
#include "ast/ast.hpp"
#include "ast/ast-printer.hpp"
#include "il/il-interp.hpp"
#include "il/il-printer.hpp"
#include "il/processing/il-processing.hpp"
#include "itr/itr-printer.hpp"
#include "module/encode.hpp"
#include "semantic/semantic-analysis.hpp"

//
// core package build steps
//
// build "../noct/src/core/marker.nx"
// build "../noct/src/core/intrin/compintrin.nx"
// build "../noct/src/core/result.nx"
// build "../noct/src/core/ops/arith.nx" "../noct/src/core/ops/deref.nx" "../noct/src/core/ops/cmp.nx" "../noct/src/core/ops/index.nx" "../noct/src/core/ops/concat.nx" -I "./"
// build "../noct/src/core/convert.nx" -I "./"
//
// build "../noct/src/core/clone.nx"
// build "../noct/src/core/default.nx"
// 
// build "../noct/src/core/iter.nx"
//
// build "../noct/src/core/mem.nx" -I "./"
// build "../noct/src/core/array.nx" -I "./"
// build "../noct/src/core/slice.nx" -I "./"
// 
// build "../noct/src/core/consts.nx"
// build "../noct/src/core/ffi.nx"
//
//
// Misc:
//
// build test.nx -I "./"
// interpret main -I "./"
//

void ProcessBuild()
{
	Noctis::Timer buildTimer(true);

	g_Logger.SetOutFile("log.txt");
	g_Logger.Log("Processing build command\n");

	StdUnorderedMap<Noctis::QualNameSPtr, Noctis::ModuleSPtr>& modules = g_Ctx.modules;

	const Noctis::BuildOptions& buildOptions = g_Ctx.options.GetBuildOptions();
	
	Noctis::Timer timer{ true };
	
	const StdVector<StdString>& filepaths = buildOptions.buildFiles;

	for (const StdString& filepath : filepaths)
	{
		g_ErrorSystem.SetCurrentFile(filepath);

		StdString content;
		bool couldRead = Noctis::ReadFileAsString(filepath, content);
		if (!couldRead)
		{
			g_ErrorSystem.Error("Failed to open file '%s'", filepath.c_str());
			continue;
		}

		g_Logger.Log("-- FILE: %s\n", filepath.c_str());
		Noctis::Timer fileTimer(true);
		
		Noctis::Lexer lexer{};
		lexer.Lex(filepath, content);
		
		timer.Stop();
		if (buildOptions.logTokens)
			lexer.LogTokens();

		g_Logger.Log("%16s : lexer\n", timer.GetSMSFormat().c_str());

		timer.Start();
		Noctis::Parser parser{ lexer.Tokens() };

		Noctis::AstTree astTree;
		astTree.filepath = filepath;
		astTree.nodes = parser.Parse();

		if (!astTree.nodes.empty())
		{
			Noctis::AstStmtSPtr firstStmt = astTree.nodes[0];
			if (astTree.nodes[0]->stmtKind == Noctis::AstStmtKind::Decl)
			{
				Noctis::AstDecl& firstDecl = *static_cast<Noctis::AstDecl*>(firstStmt.get());
				if (firstDecl.declKind == Noctis::AstDeclKind::Module)
				{
					Noctis::AstModuleDecl& modDecl = static_cast<Noctis::AstModuleDecl&>(firstDecl);
					astTree.moduleScope = Noctis::QualName::Create(modDecl.moduleIdens);
				}
			}
		}
		if (!astTree.moduleScope)
			astTree.moduleScope = buildOptions.moduleQualName;

		timer.Stop();

		if (buildOptions.logParsedAst)
		{
			Noctis::AstPrinter printer;
			printer.Visit(astTree);
		}

		g_Logger.Log("%16s : parser\n", timer.GetSMSFormat().c_str());
		if (astTree.nodes.empty())
		{
			//g_ErrorSystem.Error("No module name is defined and no default name is passed to the compiler!");
			continue;
		}

		Noctis::QualNameSPtr moduleQualName;
		if (astTree.nodes.size() != 0 && astTree.nodes[0]->stmtKind == Noctis::AstStmtKind::Decl)
		{

			Noctis::AstDecl* pDecl = static_cast<Noctis::AstDecl*>(astTree.nodes[0].get());
			if (pDecl->declKind == Noctis::AstDeclKind::Module)
			{
				Noctis::AstModuleDecl* pModDecl = static_cast<Noctis::AstModuleDecl*>(pDecl);
				moduleQualName = Noctis::QualName::Create(pModDecl->moduleIdens);
			}
		}

		if (!moduleQualName)
			moduleQualName = buildOptions.moduleQualName;

		if (!moduleQualName)
		{
			g_ErrorSystem.Error("No module name is defined and no default name is passed to the compiler!");
			continue;
		}

		auto it = modules.find(moduleQualName);
		if (it == modules.end())
		{
			it = modules.insert(std::pair{ moduleQualName, Noctis::ModuleSPtr{ new Noctis::Module{ moduleQualName } } }).first;
		}
		else if (it->second->isImported)
		{
			// If a previous version of the module is already discovered, make sure that we will replace it
			it->second->isImported = false;
			//it->second.reset(new Noctis::Module{ moduleQualName, &context });
		}
		it->second->trees.push_back(astTree);

		fileTimer.Stop();
		g_Logger.Log("-- TOOK %s\n", fileTimer.GetSMSFormat().c_str());
	}

	for (StdPair<const Noctis::QualNameSPtr, Noctis::ModuleSPtr>& pair : modules)
	{
		if (pair.second->isImported)
			continue;
		
		g_Ctx.activeModule = pair.second;
		
		Noctis::AstSemanticAnalysis astSemAnalysis;

		for (Noctis::AstTree& tree : pair.second->trees)
		{
			astSemAnalysis.Run(tree);
		}

		if (buildOptions.logLoweredITr)
		{
			Noctis::ITrPrinter printer;
			printer.Print(pair.second->itrModule);
		}

		Noctis::ITrSemanticAnalysis itrSemAnalysis;
		itrSemAnalysis.Run(pair.second->itrModule);

		pair.second->symTable.RemoveImpl();

		g_Logger.SetCanWriteToStdOut(false);
		pair.second->symTable.Log(false);
		g_Logger.SetCanWriteToStdOut(true);

		Noctis::ILProcessing ilProcessing;
		ilProcessing.Process(pair.second->ilMod);
		
		{
			g_Logger.SetCanWriteToStdOut(false);
			Noctis::ILPrinter printer;
			printer.Print(pair.second->ilMod);
			g_Logger.SetCanWriteToStdOut(true);
		}

		{
			Noctis::ModuleEncode encoder;
			StdVector<u8> encoded = encoder.Encode(*pair.second);

			StdString path = pair.second->qualName->ToString();
			Noctis::StringReplace(path, "::", ".");
			path += ".nxm";
			std::ofstream out(path.c_str(), std::ios::binary);
			if (out.is_open())
			{
				out.write((char*)encoded.data(), encoded.size());
				out.close();
			}
		}
		
	}

	buildTimer.Stop();
	g_Logger.Log("build took %s\n", buildTimer.GetSMSFormat().c_str());
}

void ProcessInterpret()
{
	const Noctis::InterpretOptions& interpOptions = g_Ctx.options.GetInterpretOptions();


	Noctis::QualNameSPtr toInterp = interpOptions.moduleToInterpret;

	auto it = g_Ctx.modules.find(toInterp);
	if (it == g_Ctx.modules.end())
	{
		g_ErrorSystem.Error("Could not interpret module '', since it cannot be located\n");
		return;
	}
	Noctis::Module& mod = *it->second;

	Noctis::Timer timer{ true };
	
	// Decode module to interpret (also decodes required imports)
	Noctis::ModuleDecode decode;
	decode.Decode(mod);

	timer.Stop();
	g_Logger.Log(Noctis::Format("decoding modules took %fms\n", timer.GetTimeMS()));
	timer.Start();
	
	g_Ctx.typeReg.CalculateSizeAlign();

	timer.Stop();
	g_Logger.Log(Noctis::Format("calculating sizes took %fms\n", timer.GetTimeMS()));

	timer.Start();
	
	Noctis::ILInterp interp;

	Noctis::QualNameSPtr mainQUalName = toInterp->Append("__main");
	interp.Interp(mainQUalName);

	timer.Stop();
	g_Logger.Log(Noctis::Format("interpreting took %fms\n", timer.GetTimeMS()));
}

int main(int argc, char* argv[])
{
	if (!g_Ctx.options.ParseOptions(argc, argv))
	{
		return -1;
	}

	Noctis::Timer timer{ true };

	// Load all headers for all available modules
	{
		Noctis::ModuleDecode decode;

		const StdVector<StdString>& modulePaths = g_Ctx.options.ModulePaths();
		for (const StdString& modulePath : modulePaths)
		{
			std::filesystem::path path{ modulePath };

			if (std::filesystem::is_directory(path))
			{
				for (const std::filesystem::directory_entry& entry : std::filesystem::directory_iterator(path))
				{
					if (entry.is_regular_file() && entry.path().extension() == ".nxm")
					{
						std::string s = entry.path().u8string();
						StdString tmp;
						tmp.insert(tmp.begin(), s.begin(), s.end());
						Noctis::ModuleSPtr mod = decode.CreateModuleWithHeader(tmp);
						g_Ctx.modules.try_emplace(mod->qualName, mod);
					}
				}
			}
			else if (path.extension() == "nxm")
			{
				Noctis::ModuleSPtr mod = decode.CreateModuleWithHeader(modulePath);
				g_Ctx.modules.try_emplace(mod->qualName, mod);
			}
			else
			{
				g_ErrorSystem.Error("Invalid module path: %s\n", modulePath.c_str());
			}

		}

		// In self hosted version, the working directory should automatically be checked for modules
	}

	timer.Stop();
	g_Logger.Log(Noctis::Format("Module discovery took %fms\n", timer.GetTimeMS()));

	switch (g_Ctx.options.Mode())
	{
	case Noctis::ToolMode::Build:
		ProcessBuild();
		break;
	case Noctis::ToolMode::Interpret:
		ProcessInterpret();
		break;
	case Noctis::ToolMode::Run:
		break;
	default: ;
	}

	return 0;
}
