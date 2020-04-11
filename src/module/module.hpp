#pragma once
#include "common/defs.hpp"
#include "macro.hpp"
#include "ast/ast.hpp"
#include "itr/itr.hpp"
#include "symbol.hpp"

namespace Noctis
{

	struct Module
	{
		Module(QualNameSPtr qualName, Context* pCtx);

		StdVector<AstTree> trees;
		MacroContext macroCtx;
		ITrModule itrModule;

		ModuleSymbolTable symTable;
		
		QualNameSPtr qualName;
	};
	using ModuleSPtr = StdSharedPtr<Module>;
	
}
