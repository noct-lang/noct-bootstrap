#pragma once
#include "common/defs.hpp"

namespace Noctis
{
	struct Context;
	class DependencyGraph;

	FWDECL_CLASS_SPTR(QualName);
	FWDECL_STRUCT_WPTR(Symbol);

	FWDECL_STRUCT_SPTR(FuncDependencyNode);
	struct FuncDependencyNode
	{
		FuncDependencyNode(DependencyGraph* pGraph);
		
		FuncDependencyNodeSPtr GetOrAddDependency(const StdString& mangledName);

		SymbolWPtr sym;
		StdUnorderedMap<StdString, FuncDependencyNodeSPtr> dependencies;

		DependencyGraph* pGraph;
	};

	class DependencyGraph
	{
	public:
		DependencyGraph(Context* pCtx);

		FuncDependencyNodeSPtr GetOrAddFuncDependency(const StdString& mangledName);

	private:

		StdUnorderedMap<StdString, FuncDependencyNodeSPtr> m_FuncDependencies;

		StdVector<QualNameSPtr> m_FuncsInExprGenerics;

		Context* m_pCtx;
	};

	class ILDependencyGraph;

	FWDECL_STRUCT_SPTR(ILBlockDependencyNode);
	struct ILBlockDependencyNode
	{
		ILBlockDependencyNode(u32 id, ILDependencyGraph* pGraph);

		ILBlockDependencyNodeSPtr GetOrAddRefBy(u32 id);
		ILBlockDependencyNodeSPtr GetOrAddReference(u32 id);

		void ChangeRef(u32 oldId, u32 newId);

		void PropagateCanTouch();
		
		u32 id;
		ILDependencyGraph* pGraph;

		StdUnorderedMap<u32, ILBlockDependencyNodeSPtr> refBy;
		StdUnorderedMap<u32, ILBlockDependencyNodeSPtr> references;

		bool canTouch : 1;
		bool onlyGoto : 1;
		bool canMerge : 1;
	};

	class ILDependencyGraph
	{
	public:
		ILDependencyGraph(Context* pCtx);

		ILBlockDependencyNodeSPtr GetOrAddBlockDependency(u32 id);

	private:
		Context* m_pCtx;
		
		StdVector<ILBlockDependencyNodeSPtr> m_Blocks;
	};
	
}
