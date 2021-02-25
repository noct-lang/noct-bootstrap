#pragma once
#include <functional>

#include "common/defs.hpp"
#include "common/type.hpp"
#include "il/il.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(LocalVarScope);
	FWDECL_STRUCT_SPTR(LocalVarData);
	
	struct LocalVarData
	{
		LocalVarData(const StdString& iden);
		LocalVarData(const StdString& iden, TypeHandle handle, bool isParam);
		
		StdString iden;
		TypeHandle type;
		ILVar ilVar;
		bool isParam;
	};

	struct LocalVarScope
	{
		StdUnorderedMap<StdString, StdPair<u64, StdVector<LocalVarDataSPtr>>> vars;
		StdUnorderedMap<StdString, LocalVarDataSPtr> curMapping;

		StdUnorderedMap<StdString, LocalVarScopeSPtr> subScopes;

		StdVector<LocalVarDataSPtr> linearMapping;

		void AddLocalVarDeclSPtr(const StdVector<StdString>& scopeNames, LocalVarDataSPtr var, u64 curDepth = 0);
		LocalVarDataSPtr ActivateNextVar(const StdVector<StdString>& scopeNames, const StdString& iden, u64 curDepth = 0);
		LocalVarDataSPtr GetLocalVarData(const StdVector<StdString>& scopeNames, const StdString& varIden, u64 curDepth = 0);

		void Foreach(const std::function<void(LocalVarDataSPtr varData)>& lambda);

		void ResetActiveVars();
	};

	struct FuncContext
	{
		LocalVarScope localVars;
		StdPairVector<TypeSPtr, QualNameSPtr> errHandlers;
	};

	
}
