#include "il-dependency-pass.hpp"

#include "module/module.hpp"
#include "common/context.hpp"

namespace Noctis
{
	ILDependencyPass::ILDependencyPass(Context* pCtx)
		: ILPass("dependency pass", pCtx)
	{
	}

	void ILDependencyPass::Process(ILModule& mod)
	{
		for (ILFuncDefSPtr func : mod.funcs)
		{
			m_FuncNode = m_pCtx->activeModule->dependencyGraph.GetOrAddFuncDependency(func->mangleName);
			m_FuncNode->sym = func->sym;
			
			ILVisitor::Visit(*func);
		}
	}

	void ILDependencyPass::Visit(ILFuncCall& node)
	{
		m_FuncNode->GetOrAddDependency(node.func);
	}
}
