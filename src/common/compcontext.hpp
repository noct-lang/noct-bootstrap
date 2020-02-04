#pragma once
#include "tokens/span.hpp"
#include "qualname.hpp"

namespace Noctis
{

	struct CompContext
	{

		SpanManager spanManager;
		StdUnorderedMap<QualNameSPtr, void*> importModules;
		
	};
	
}
