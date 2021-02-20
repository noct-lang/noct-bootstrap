#pragma once
#include "options.hpp"
#include "tokens/span.hpp"
#include "type.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(Module);
	struct Context
	{
		Options options;

		TypeRegistry typeReg;
		
		SpanManager spanManager;
		StdUnorderedMap<QualNameSPtr, ModuleSPtr> modules;

		// When multi-threaded, an active module for each thread to work on should exist
		ModuleSPtr activeModule;
	};

	Context& GetContext();
	
}

#define g_Ctx Noctis::GetContext()
#define g_TypeReg (g_Ctx.typeReg)
#define g_SpanManager (g_Ctx.spanManager)
