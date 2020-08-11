#include "function.hpp"

namespace Noctis
{
	LocalVarData::LocalVarData(IdenSPtr iden)
		: iden(iden)
		, isParam(false)
	{
	}

	LocalVarData::LocalVarData(IdenSPtr iden, TypeHandle type, bool isParam)
		: iden(iden)
		, type(type)
		, isParam(isParam)
	{
	}

	void LocalVarScope::AddLocalVarDeclSPtr(const StdVector<StdString>& scopeNames, LocalVarDataSPtr var, u64 curDepth)
	{
		if (!scopeNames.empty() && curDepth != scopeNames.size() - 1)
		{
			auto it = subScopes.find(scopeNames[curDepth]);
			if (it == subScopes.end())
				it = subScopes.try_emplace(scopeNames[curDepth], new LocalVarScope{}).first;
			return it->second->AddLocalVarDeclSPtr(scopeNames, var, curDepth + 1);
		}

		auto it = vars.find(var->iden);
		if (it == vars.end())
			it = vars.try_emplace(var->iden, std::pair{ 0, StdVector<LocalVarDataSPtr>{} }).first;
		it->second.second.push_back(var);

		linearMapping.push_back(var);
	}

	LocalVarDataSPtr LocalVarScope::ActivateNextVar(const StdVector<StdString>& scopeNames, IdenSPtr iden, u64 curDepth)
	{
		if (!scopeNames.empty() && curDepth != scopeNames.size() - 1)
		{
			auto it = subScopes.find(scopeNames[curDepth]);
			if (it == subScopes.end())
				it = subScopes.try_emplace(scopeNames[curDepth], new LocalVarScope{}).first;
			return it->second->ActivateNextVar(scopeNames, iden, curDepth + 1);
		}
		
		auto varIt = vars.find(iden);
		if (varIt == vars.end())
			return nullptr;
		
		StdPair<u64, StdVector<LocalVarDataSPtr>>& vars = varIt->second;
		LocalVarDataSPtr newData = vars.second[vars.first];

		auto curIt = curMapping.find(iden);
		if (curIt != curMapping.end())
		{
			curIt->second = newData;
		}
		else
		{
			curMapping.try_emplace(iden, newData);
		}
		++vars.first;
		return newData;
	}

	LocalVarDataSPtr LocalVarScope::GetLocalVarData(const StdVector<StdString>& scopeNames, IdenSPtr varIden, u64 curDepth)
	{
		if (!scopeNames.empty() && curDepth != scopeNames.size() - 1)
		{
			auto it = subScopes.find(scopeNames[curDepth]);
			if (it != subScopes.end())
			{
				LocalVarDataSPtr tmp = it->second->GetLocalVarData(scopeNames, varIden, curDepth + 1);
				if (tmp)
					return tmp;
			}
		}

		auto it = curMapping.find(varIden);
		if (it != curMapping.end())
			return it->second;
		return nullptr;
	}

	void LocalVarScope::Foreach(const std::function<void(LocalVarDataSPtr varData)>& lambda)
	{
		for (LocalVarDataSPtr var : linearMapping)
		{
			lambda(var);
		}

		for (StdPair<const StdString, LocalVarScopeSPtr>& pair : subScopes)
		{
			pair.second->Foreach(lambda);
		}
	}
}
