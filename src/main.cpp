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

void ProcessBuild(Noctis::Context& context)
{
	g_Logger.Log("Processing build command\n");

	context.pCompContext = new Noctis::CompContext{};

	const StdString& file = context.options.FilesToBuild()[0];
	context.pCompContext->spanManager.SetCurFile(file);
	g_ErrorSystem.SetCurrentFile(file);

	StdString content = Noctis::ReadFileAsString(file);

	Noctis::Lexer lexer{ &context };

	Noctis::Timer timer{ true };
	lexer.Lex(content);
	timer.Stop();

	lexer.LogTokens();

	g_Logger.Log(Noctis::Format("Lexer took %f microseconds", timer.GetTimeUS()));


	delete context.pCompContext;
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
