#pragma once
#include <functional>

#include "common/defs.hpp"
#include "common/type.hpp"
#include "il/il.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(Iden);

	FWDECL_STRUCT_SPTR(LocalVarScope);
	FWDECL_STRUCT_SPTR(LocalVarData);
	
	struct LocalVarData
	{
		LocalVarData(IdenSPtr iden);
		LocalVarData(IdenSPtr iden, TypeHandle handle, bool isParam);
		
		IdenSPtr iden;
		TypeHandle type;
		ILVar ilVar;
		bool isParam;
	};

	struct LocalVarScope
	{
		StdUnorderedMap<IdenSPtr, StdPair<u64, StdVector<LocalVarDataSPtr>>> vars;
		StdUnorderedMap<IdenSPtr, LocalVarDataSPtr> curMapping;

		StdUnorderedMap<StdString, LocalVarScopeSPtr> subScopes;

		StdVector<LocalVarDataSPtr> linearMapping;

		void AddLocalVarDeclSPtr(const StdVector<StdString>& scopeNames, LocalVarDataSPtr var, u64 curDepth = 0);
		LocalVarDataSPtr ActivateNextVar(const StdVector<StdString>& scopeNames, IdenSPtr iden, u64 curDepth = 0);
		LocalVarDataSPtr GetLocalVarData(const StdVector<StdString>& scopeNames, IdenSPtr varIden, u64 curDepth = 0);

		void Foreach(const std::function<void(LocalVarDataSPtr varData)>& lambda);
	};

	struct FuncContext
	{
		LocalVarScope localVars;
		StdPairVector<TypeSPtr, QualNameSPtr> errHandlers;
	};

	
}
