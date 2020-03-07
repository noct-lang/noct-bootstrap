#pragma once
#include "options.hpp"
#include "tokens/span.hpp"

namespace Noctis
{
	struct CompContext;

	FWDECL_STRUCT_SPTR(Module);

	struct Context
	{
		Options options;

		SpanManager spanManager;
		StdUnorderedMap<QualNameSPtr, ModuleSPtr> modules;

		// When multi-threaded, an active module for each thread to work on should exist
		ModuleSPtr activeModule;
	};
	
}
