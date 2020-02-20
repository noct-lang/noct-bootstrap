#pragma once
#include "tokens/span.hpp"
#include "qualname.hpp"
#include "module/module.hpp"

namespace Noctis
{

	struct CompContext
	{
		SpanManager spanManager;
		StdUnorderedMap<QualNameSPtr, ModuleSPtr> modules;

		// When multi-threaded, an active module for each thread to work on should exist
		ModuleSPtr activeModule;
	};
	
}
