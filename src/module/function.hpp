#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);

	FWDECL_STRUCT_SPTR(LocalVarScope);
	FWDECL_STRUCT_SPTR(LocalVarData);
	
	struct LocalVarData
	{
		IdenSPtr iden;
		TypeHandle type;
	};

	struct LocalVarScope
	{
		StdUnorderedMap<IdenSPtr, StdPair<u64, StdVector<LocalVarDataSPtr>>> vars;
		StdUnorderedMap<IdenSPtr, LocalVarDataSPtr> curMapping;

		StdUnorderedMap<StdString, LocalVarScopeSPtr> subScopes;

		void AddLocalVarDeclSPtr(const StdVector<StdString>& scopeNames, LocalVarDataSPtr var, u64 curDepth = 0);
		LocalVarDataSPtr ActivateNextVar(const StdVector<StdString>& scopeNames, IdenSPtr iden, u64 curDepth = 0);
		LocalVarDataSPtr GetLocalVarData(const StdVector<StdString>& scopeNames, IdenSPtr varIden, u64 curDepth = 0);
	};

	struct FuncContext
	{
		LocalVarScope localVars;
	};

	
}
