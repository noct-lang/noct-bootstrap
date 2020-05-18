#include "misc-passes.hpp"


#include "common/name-mangling.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"

namespace Noctis
{
	NameManglePass::NameManglePass(Context* pCtx)
		: ITrSemanticPass("name mangle pass", pCtx)
	{
	}

	void NameManglePass::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
			SymbolSPtr sym = node.sym.lock();
			sym->mangledName = NameMangling::Mangle(m_pCtx, sym);
		});
	}
}
