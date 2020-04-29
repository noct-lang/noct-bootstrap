#include "type-resolution.hpp"


#include "common/context.hpp"
#include "common/type.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"

namespace Noctis
{
	TypealiasReplacing::TypealiasReplacing(Context* pCtx)
		: ITrSemanticPass("typealias replacement", pCtx)
	{
	}

	void TypealiasReplacing::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			if (!node.type)
				return;
			TypeHandle alias = node.sym.lock()->type;
			TypeHandle handle = node.type->handle;
			m_pCtx->typeReg.SetAliasType(alias, handle);
		});
	}
}
