#pragma once
#include "common/defs.hpp"
#include "common/qualname.hpp"

namespace Noctis
{
	struct AstTree;
	struct Context;

	StdUnorderedSet<QualNameSPtr> ExtractImportModules(AstTree& tree, Context* pCtx);
	
}
