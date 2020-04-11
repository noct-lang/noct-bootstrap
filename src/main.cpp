#include <string_view>
#include <sstream>
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
#include "itr/itr-printer.hpp"
#include "semantic/semantic-analysis.hpp"

void ProcessBuild(Noctis::Context& context)
{
	Noctis::Timer buildTimer(true);
	
	g_Logger.Log("Processing build command\n");

	StdUnorderedMap<Noctis::QualNameSPtr, Noctis::ModuleSPtr>& modules = context.modules;
	Noctis::Timer timer{ true };

	const StdVector<StdString>& filepaths = context.options.GetBuildOptions().buildFiles;
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

		if (context.options.GetBuildOptions().logTokens)
			lexer.LogTokens();

		g_Logger.Log(Noctis::Format("Lexer took %fms\n", timer.GetTimeMS()));

		Noctis::Parser parser{ lexer.Tokens(), &context };

		timer.Start();

		Noctis::AstTree astTree;
		astTree.filepath = filepath;
		astTree.nodes = parser.Parse();

		timer.Stop();

		if (context.options.GetBuildOptions().logParsedAst)
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
			moduleQualName = context.options.GetBuildOptions().moduleQualName;

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
		context.activeModule = pair.second;
		
		Noctis::AstSemanticAnalysis astSemAnalysis{ &context };

		for (Noctis::AstTree& tree : pair.second->trees)
		{
			timer.Start();
			astSemAnalysis.Run(tree);
			timer.Stop();

			g_Logger.Log(Noctis::Format("AST semantic analysis took %fms\n", timer.GetTimeMS()));

			if (context.options.GetBuildOptions().logAst)
			{
				Noctis::AstPrinter printer;
				printer.Visit(tree);
			}
		}

		if (context.options.GetBuildOptions().logLoweredITr)
		{
			Noctis::ITrPrinter printer{ &context };
			printer.Print(pair.second->itrModule);
		}

		Noctis::ITrSemanticAnalysis itrSemAnalysis{ &context };
		itrSemAnalysis.Run(pair.second->itrModule);

		pair.second->symTable.Log();
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
