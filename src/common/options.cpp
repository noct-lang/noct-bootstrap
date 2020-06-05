#include "options.hpp"

#pragma warning(push)
#pragma warning(disable: 26451 26478 26439)
#define ARGS_NOEXCEPT
#include "3rdparty/args.hxx"
#pragma warning(pop)

#include "logger.hpp"

namespace Noctis
{
	Options::Options()
		: m_ToolMode(ToolMode::None)
		, m_TabWidth(4)
		, m_BuildOptions()
	{
	}

	bool Options::ParseOptions(i32 argc, char* argv[])
	{
		args::ArgumentParser parser{ "Noct compiler and tools" };

		args::Group toolmodeGroup{ parser, "Toolmodes" };

		args::Command buildCommand{ toolmodeGroup, "build", "Build command", std::bind(&Options::ParseBuild, this, std::placeholders::_1) };
		args::Command interpretCommand{ toolmodeGroup, "interpret", "Interpret command", std::bind(&Options::ParseInterpret, this, std::placeholders::_1) };

		args::HelpFlag help{ parser, "help", "Show help", { 'h', "help" } };
		args::ValueFlag<u8> tabWidth{ parser, "tab-width", "Width of a tab (in spaces)", { "tab-width" }, 4 };

		bool res = parser.ParseCLI(argc, argv);

		if (help.Get())
		{
			const std::string& origHelpStr = parser.Help();
			StdString helpStr;
			helpStr.append(origHelpStr.begin(), origHelpStr.end());
			g_Logger.Log(helpStr);
			m_ToolMode = ToolMode::None;
		}
		else if (!res)
		{
			args::Error err = parser.GetError();

			StdString msg;
			msg.append(parser.GetErrorMsg().begin(), parser.GetErrorMsg().end());
			g_Logger.Log(msg);

			return false;
		}

		m_TabWidth = tabWidth.Get();

		return true;
	}

	void Options::ParseBuild(args::Subparser& parser)
	{
		args::PositionalList<StdString> buildFiles{ parser, "File names", "File names" };
		args::ValueFlag<StdString> moduleIdens{ parser, "Module name", "Module name if no module is defined in source", { "module-name" }, "main" };
		args::Flag logTokens{ parser, "log tokens", "Log tokens", { "log-tokens" } };
		args::Flag logParsedAst{ parser, "log parsed ast", "Log parsed ast", { "log-parsed-ast" } };
		args::Flag logAst{ parser, "log ast", "Log ast", { "log-ast" } };
		args::Flag logLoweredITr{ parser, "log lowered itr", "Log lowered itr", { "log-lowered-itr" } };

		args::Flag noIL{ parser, "no IL", "Don't encode IL", {"no-il"} };
		args::Flag optIL{ parser, "optional IL", "IL is not encoded in a module, but optionally linked", {"optional-il"} };

		args::ValueFlagList<StdString> modulePaths{ parser, "module paths", "Available module paths", { 'I' } };


		parser.Parse();

		m_ToolMode = ToolMode::Build;

		m_ModulePaths.assign(modulePaths.begin(), modulePaths.end());

		StdString tempModuleIdens = moduleIdens.Get();
		StdVector<StdString> splitTempModuleIdens = SplitString(tempModuleIdens, '.');
		m_BuildOptions.moduleQualName = QualName::Create(splitTempModuleIdens);
		
		m_BuildOptions.buildFiles.assign(buildFiles.begin(), buildFiles.end());
		
		
		
		m_BuildOptions.logTokens = logTokens.Get();
		m_BuildOptions.logParsedAst = logParsedAst.Get();
		m_BuildOptions.logAst = logAst.Get();
		m_BuildOptions.logLoweredITr = logLoweredITr.Get();
		m_BuildOptions.encodeIL = !noIL.Get();
		m_BuildOptions.optIL = optIL.Get();
		
	}

	void Options::ParseInterpret(args::Subparser& parser)
	{
		args::Positional<StdString> toInterpret{ parser, "Module", "Module to interpret" };

		args::ValueFlagList<StdString> modulePaths{ parser, "module paths", "Available module paths", { 'I' } };


		parser.Parse();

		m_ToolMode = ToolMode::Interpret;

		m_ModulePaths.assign(modulePaths.begin(), modulePaths.end());

		StdString tempModuleIdens = toInterpret.Get();
		StdVector<StdString> splitTempModuleIdens = SplitString(tempModuleIdens, '.');
		m_InterpretOptions.moduleToInterpret = QualName::Create(splitTempModuleIdens);
	}
}
