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
	{
	}

	bool Options::ParseOptions(i32 argc, char* argv[])
	{
		args::ArgumentParser parser{ "Noct compiler and tools" };

		args::Group toolmodeGroup{ parser, "Toolmodes" };

		args::Command buildCommand{ toolmodeGroup, "build", "Build command", std::bind(&Options::ParseBuild, this, std::placeholders::_1) };

		args::HelpFlag help{ parser, "help", "Show help", { 'h', "help" } };

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

		return true;
	}

	void Options::ParseBuild(args::Subparser& parser)
	{
		args::PositionalList<std::string> buildFiles{ parser, "File names", "File names" };
		args::ValueFlag<u8> tabWidth{ parser, "tab-width", "Width of a tab (in spaces)", { "tab-width" }, 4 };

		parser.Parse();

		m_ToolMode = ToolMode::Build;
		m_FilesToBuild.assign(buildFiles.begin(), buildFiles.end());
		m_TabWidth = tabWidth;
		
	}
}
