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
		
		SymbolWPtr sym;
		StdUnorderedMap<StdString, FuncDependencyNodeSPtr> dependencies;

		DependencyGraph* pGraph;

		FuncDependencyNodeSPtr GetOrAddDependency(const StdString& mangledName);
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

	
}
