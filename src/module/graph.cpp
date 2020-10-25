#include "graph.hpp"

#include "symbol.hpp"

namespace Noctis
{
	FuncDependencyNode::FuncDependencyNode(DependencyGraph* pGraph)
		: pGraph(pGraph)
	{
	}

	FuncDependencyNodeSPtr FuncDependencyNode::GetOrAddDependency(const StdString& mangledName)
	{
		auto it = dependencies.find(mangledName);
		if (it != dependencies.end())
			return it->second;

		FuncDependencyNodeSPtr node = GetOrAddDependency(mangledName);
		dependencies.try_emplace(mangledName, node);
		return node;
	}

	DependencyGraph::DependencyGraph(Noctis::Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	FuncDependencyNodeSPtr DependencyGraph::GetOrAddFuncDependency(const StdString& mangledName)
	{
		auto it = m_FuncDependencies.find(mangledName);
		if (it != m_FuncDependencies.end())
			return it->second;

		FuncDependencyNodeSPtr node{ new FuncDependencyNode{ this } };
		m_FuncDependencies.try_emplace(mangledName, node);
		return node;
	}
}
