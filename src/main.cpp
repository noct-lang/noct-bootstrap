#include "common/context.hpp"
#include "common/logger.hpp"

#include <string_view>
#include "comp/lexer.hpp"
#include "comp/token.hpp"
#include <sstream>
#include "comp/compcontext.hpp"
#include "common/errorsystem.hpp"
#include "common/perf.hpp"
#include "common/utils.hpp"
#include "comp/AST/parser.hpp"
#include "comp/AST/ast.hpp"
#include "comp/AST/astprinter.hpp"

void ProcessBuild(Noctis::Context& context)
{
	Noctis::Timer buildTimer(true);
	
	g_Logger.Log("Processing build command\n");

	context.pCompContext = new Noctis::CompContext{};

	const StdString& file = context.options.GetBuildOptions().buildFiles[0];
	context.pCompContext->spanManager.SetCurFile(file);
	g_ErrorSystem.SetCurrentFile(file);

	StdString content = Noctis::ReadFileAsString(file);

	Noctis::Lexer lexer{ &context };

	Noctis::Timer timer{ true };
	lexer.Lex(content);
	timer.Stop();

	if (context.options.GetBuildOptions().logTokens)
		lexer.LogTokens();

	g_Logger.Log(Noctis::Format("Lexer took %fms\n", timer.GetTimeMS()));

	Noctis::Parser parser{ lexer.Tokens(), &context };

	timer.Start();

	Noctis::AstTree astTree;
	astTree.filepath = file;
	astTree.nodes = parser.Parse();
	
	timer.Stop();

	if (context.options.GetBuildOptions().LogAst)
	{
		Noctis::AstPrinter printer;
		printer.Visit(astTree);
	}
	
	g_Logger.Log(Noctis::Format("Parser took %fms\n", timer.GetTimeMS()));


	delete context.pCompContext;

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
