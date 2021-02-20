#include "context.hpp"

namespace Noctis
{
	Context& GetContext()
	{
		static Context ctx;
		return ctx;
	}

}
