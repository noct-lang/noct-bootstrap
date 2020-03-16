#pragma once
#include "defs.hpp"
#include "qualname.hpp"

namespace args
{
	class Subparser;
}

namespace Noctis
{

	enum class ToolMode
	{
		None,
		Build,
		Run
	};

	struct BuildOptions
	{
		QualNameSPtr moduleQualName;
		StdVector<StdString> buildFiles;
		
		bool logTokens : 1;
		bool logParsedAst : 1;
		bool logAst : 1;
		bool logLoweredITr : 1;
	};
	
	class Options
	{		
	public:
		Options();

		bool ParseOptions(i32 argc, char* argv[]);

		ToolMode Mode() const { return m_ToolMode; }
		u8 TabWidth() const { return m_TabWidth; }

		const BuildOptions& GetBuildOptions() const { return m_BuildOptions; }
		
	private:

		void ParseBuild(args::Subparser& parser);

		ToolMode m_ToolMode;
		u8 m_TabWidth;

		BuildOptions m_BuildOptions;
	};
	
}
