#include "graph.hpp"

#include "symbol.hpp"

namespace Noctis
{
	FuncDependencyNode::FuncDependencyNode(DependencyGraph* pGraph)
		: pGraph(pGraph)
	{
	}

	FuncDependencyNodeSPtr FuncDependencyNode::GetOrAddDependency(QualNameSPtr qualName)
	{
		auto it = dependencies.find(qualName);
		if (it != dependencies.end())
			return it->second;

		FuncDependencyNodeSPtr node = pGraph->GetOrAddFuncDependency(qualName);
		dependencies.try_emplace(qualName, node);
		return node;
	}

	FuncDependencyNodeSPtr DependencyGraph::GetOrAddFuncDependency(QualNameSPtr qualName)
	{
		auto it = m_FuncDependencies.find(qualName);
		if (it != m_FuncDependencies.end())
			return it->second;

		FuncDependencyNodeSPtr node{ new FuncDependencyNode{ this } };
		m_FuncDependencies.try_emplace(qualName, node);
		return node;
	}

	ILBlockDependencyNode::ILBlockDependencyNode(u32 id, ILDependencyGraph* pGraph)
		: id(id)
		, pGraph(pGraph)
		, canTouch(false)
		, onlyGoto(false)
		, canMerge(false)
	{
	}

	ILBlockDependencyNodeSPtr ILBlockDependencyNode::GetOrAddRefBy(u32 id)
	{
		auto it = refBy.find(id);
		if (it != refBy.end())
			return it->second;

		ILBlockDependencyNodeSPtr node = pGraph->GetOrAddBlockDependency(id);
		refBy.try_emplace(id, node);
		return node;
	}

	ILBlockDependencyNodeSPtr ILBlockDependencyNode::GetOrAddReference(u32 id)
	{
		auto it = references.find(id);
		if (it != references.end())
			return it->second;

		ILBlockDependencyNodeSPtr node = pGraph->GetOrAddBlockDependency(id);
		references.try_emplace(id, node);
		return node;
	}

	void ILBlockDependencyNode::ChangeRef(u32 oldId, u32 newId)
	{
		if (oldId == newId)
			return;
		
		ILBlockDependencyNodeSPtr depNode = references.find(oldId)->second;

		references.erase(oldId);
		depNode->refBy.erase(oldId);

		references.try_emplace(newId, depNode);
		depNode->refBy.try_emplace(newId, depNode);
	}

	void ILBlockDependencyNode::PropagateCanTouch()
	{
		if (canTouch)
			return;
		
		canTouch = true;

		for (StdPair<u32, ILBlockDependencyNodeSPtr> pair : references)
		{
			pair.second->PropagateCanTouch();
		}
	}

	ILDependencyGraph::ILDependencyGraph()
	{
	}

	ILBlockDependencyNodeSPtr ILDependencyGraph::GetOrAddBlockDependency(u32 id)
	{
		if (m_Blocks.size() <= id)
			m_Blocks.resize(id + 1);

		if (!m_Blocks[id])
			m_Blocks[id].reset(new ILBlockDependencyNode{ id, this });
		return m_Blocks[id];
	}
}
