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
#include "il/il-printer.hpp"
#include "itr/itr-printer.hpp"
#include "module/encode.hpp"
#include "semantic/semantic-analysis.hpp"

void ProcessBuild(Noctis::Context& context)
{
	Noctis::Timer buildTimer(true);

	g_Logger.SetOutFile("log.txt");
	g_Logger.Log("Processing build command\n");

	StdUnorderedMap<Noctis::QualNameSPtr, Noctis::ModuleSPtr>& modules = context.modules;

	const Noctis::BuildOptions& buildOptions = context.options.GetBuildOptions();
	
	Noctis::Timer timer{ true };
	
	const StdVector<StdString>& filepaths = buildOptions.buildFiles;

	// Load all headers for all available modules
	{
		Noctis::ModuleDecode decode{ &context };
		const StdVector<StdString>& modulePaths = buildOptions.modulePaths;

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
						context.modules.try_emplace(mod->qualName, mod);
					}
				}
			}
			else if (path.extension() == "nxm")
			{
				Noctis::ModuleSPtr mod = decode.CreateModuleWithHeader(modulePath);
				context.modules.try_emplace(mod->qualName, mod);
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

	for (const StdString& filepath : filepaths)
	{
		g_ErrorSystem.SetCurrentFile(filepath);

		StdString content;
		bool couldRead = Noctis::ReadFileAsString(filepath, content);
		if (!couldRead)
		{
			const char* pFilePath = filepath.c_str();
			g_ErrorSystem.Error("Failed to open file '%s'", pFilePath);
			continue;
		}
		
		Noctis::Lexer lexer{ &context };

		lexer.Lex(filepath, content);
		timer.Stop();

		if (buildOptions.logTokens)
			lexer.LogTokens();

		g_Logger.Log(Noctis::Format("Lexer took %fms\n", timer.GetTimeMS()));

		Noctis::Parser parser{ lexer.Tokens(), &context };

		timer.Start();

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

		g_Logger.Log(Noctis::Format("Parser took %fms\n", timer.GetTimeMS()));

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
			it = modules.insert(std::pair{ moduleQualName, Noctis::ModuleSPtr{ new Noctis::Module{ moduleQualName, &context } } }).first;
		}
		it->second->trees.push_back(astTree);		
	}

	for (StdPair<const Noctis::QualNameSPtr, Noctis::ModuleSPtr>& pair : modules)
	{
		if (pair.second->isImported)
			continue;
		
		context.activeModule = pair.second;
		
		Noctis::AstSemanticAnalysis astSemAnalysis{ &context };

		for (Noctis::AstTree& tree : pair.second->trees)
		{
			timer.Start();
			astSemAnalysis.Run(tree);
			timer.Stop();

			g_Logger.Log(Noctis::Format("AST semantic analysis took %fms\n", timer.GetTimeMS()));

			if (buildOptions.logAst)
			{
				Noctis::AstPrinter printer;
				printer.Visit(tree);
			}
		}

		if (buildOptions.logLoweredITr)
		{
			Noctis::ITrPrinter printer{ &context };
			printer.Print(pair.second->itrModule);
		}

		Noctis::ITrSemanticAnalysis itrSemAnalysis{ &context };
		itrSemAnalysis.Run(pair.second->itrModule);

		g_Logger.SetCanWriteToStdOut(false);
		pair.second->symTable.Log();
		

		{
			Noctis::ILPrinter printer(&context);
			for (Noctis::ILFuncDefSPtr func : pair.second->ilMod.funcs)
			{
				printer.Visit(*func);
			}
		}
		g_Logger.SetCanWriteToStdOut(true);

		{
			Noctis::ModuleEncode encoder(&context);
			StdVector<u8> encoded = encoder.Encode(*pair.second);

			StdString path = pair.second->qualName->ToString();
			path.erase(path.begin(), path.begin() + 2);
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
	g_Logger.Log(Noctis::Format("build took %fms\n", buildTimer.GetTimeMS()));
}

int main(int argc, char* argv[])
{
	Noctis::Context context;
	if (!context.options.ParseOptions(argc, argv))
	{
		return -1;
	}

	switch (context.options.Mode())
	{
	case Noctis::ToolMode::Build:
		ProcessBuild(context);
		break;
	case Noctis::ToolMode::Run: break;
	default: ;
	}

	return 0;
}
