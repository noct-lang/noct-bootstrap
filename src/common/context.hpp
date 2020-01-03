#pragma once
#include "options.hpp"

namespace Noctis
{
	struct CompContext;

	struct Context
	{
		Options options;

		CompContext* pCompContext = nullptr;
	};
	
}
