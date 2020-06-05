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
		Interpret,
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


		bool encodeIL : 1;
		bool optIL : 1;
	};

	struct InterpretOptions
	{
		QualNameSPtr moduleToInterpret;
	};
	
	class Options
	{		
	public:
		Options();

		bool ParseOptions(i32 argc, char* argv[]);

		ToolMode Mode() const { return m_ToolMode; }
		u8 TabWidth() const { return m_TabWidth; }
		const StdVector<StdString>& ModulePaths() const { return m_ModulePaths; }

		const BuildOptions& GetBuildOptions() const { return m_BuildOptions; }
		const InterpretOptions& GetInterpretOptions() const { return m_InterpretOptions; }
		
	private:

		void ParseBuild(args::Subparser& parser);
		void ParseInterpret(args::Subparser& parser);

		ToolMode m_ToolMode;
		u8 m_TabWidth;

		StdVector<StdString> m_ModulePaths;

		BuildOptions m_BuildOptions;
		InterpretOptions m_InterpretOptions;
	};
	
}
