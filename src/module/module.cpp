#include "module.hpp"

namespace Noctis
{
	Module::Module(QualNameSPtr qualName, Context* pCtx)
		: symTable(pCtx)
		, qualName(qualName)
		, opTable(pCtx)
		, isDecoded(false)
		, isImported(false)
	{
		memset(&header, 0, sizeof(ModuleHeader));
	}
}
