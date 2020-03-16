#pragma once
#include "common/defs.hpp"
#include "macro.hpp"
#include "ast/ast.hpp"
#include "itr/itr.hpp"

namespace Noctis
{

	struct Module
	{
		QualNameSPtr qualName;

		StdVector<AstTree> trees;
		
		MacroContext macroCtx;

		ITrModule itrModule;
	};
	using ModuleSPtr = StdSharedPtr<Module>;
	
}
