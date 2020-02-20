#pragma once
#include "common/defs.hpp"
#include "macro.hpp"
#include "ast/ast.hpp"

namespace Noctis
{

	struct Module
	{
		QualNameSPtr qualName;

		StdVector<AstTree> trees;
		
		MacroContext macroCtx;
		
	};
	using ModuleSPtr = StdSharedPtr<Module>;
	
}
