#include "il-dependency-pass.hpp"

#include "il/il.hpp"
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
			m_Func = func;
			
			m_FuncNode = m_pCtx->activeModule->dependencyGraph.GetOrAddFuncDependency(func->qualName);
			m_FuncNode->sym = func->sym;
			
			ILVisitor::Visit(*func);
		}
	}

	void ILDependencyPass::Visit(ILBlock& node)
	{
		m_BlockNode = m_Func->graph.GetOrAddBlockDependency(node.label);


		ILVisitor::Visit(node);
	}

	void ILDependencyPass::Visit(ILFuncCall& node)
	{
		//m_FuncNode->GetOrAddDependency(node.func);
	}

	void ILDependencyPass::Visit(ILIf& node)
	{
		ILBlockDependencyNodeSPtr trueNode = m_BlockNode->GetOrAddReference(node.trueLabel);
		(void)trueNode->GetOrAddRefBy(m_BlockNode->id);
		
		ILBlockDependencyNodeSPtr falseNode = m_BlockNode->GetOrAddReference(node.falseLabel);
		(void)falseNode->GetOrAddRefBy(m_BlockNode->id);
	}

	void ILDependencyPass::Visit(ILSwitch& node)
	{
		for (StdPair<ILVar, u32>& pair : node.cases)
		{
			ILBlockDependencyNodeSPtr depNode = m_BlockNode->GetOrAddReference(pair.second);
			(void)depNode->GetOrAddRefBy(m_BlockNode->id);
		}
		ILBlockDependencyNodeSPtr defNode = m_BlockNode->GetOrAddReference(node.defCase);
		(void)defNode->GetOrAddRefBy(m_BlockNode->id);
	}

	void ILDependencyPass::Visit(ILGoto& node)
	{
		ILBlockDependencyNodeSPtr depNode = m_BlockNode->GetOrAddReference(node.label);
		(void)depNode->GetOrAddRefBy(m_BlockNode->id);
	}
}
