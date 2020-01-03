#pragma once
#include "defs.hpp"

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

	class Options
	{
	public:
		Options();

		bool ParseOptions(i32 argc, char* argv[]);

		ToolMode Mode() const { return m_ToolMode; }
		u8 TabWidth() const { return m_TabWidth; }

		const StdVector<StdString>& FilesToBuild() const { return m_FilesToBuild; }
		
	private:

		void ParseBuild(args::Subparser& parser);

		ToolMode m_ToolMode;
		u8 m_TabWidth;

		// build
		StdVector<StdString> m_FilesToBuild;
	};
	
}
