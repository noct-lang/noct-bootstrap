#include "module.hpp"

namespace Noctis
{
	Module::Module(QualNameSPtr qualName, Context* pCtx)
		: symTable(pCtx)
		, qualName(qualName)
		, opTable(pCtx)
	{
	}
}
