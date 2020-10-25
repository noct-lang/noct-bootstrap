#include "function-processing.hpp"


#include "common/context.hpp"
#include "itr/itr.hpp"
#include "module/function.hpp"
#include "module/module.hpp"
#include "semantic/ast/misc/iden-scope-pass.hpp"

namespace Noctis
{
	LocalVarCollection::LocalVarCollection(Context* pCtx)
		: ITrSemanticPass("local var collection", pCtx)
	{
	}

	void LocalVarCollection::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Any, [&, this](ITrFunc& node)
		{
			m_FuncCtx = node.ctx;
			ITrBodySPtr body = mod.GetBody(node);
			if (!body)
				return;

			StdVector<StdString> scopeNames{};
			for (ITrParamSPtr param : node.params)
			{
				TypeInfo typeInfo = { param->type->handle };
				if (!m_FuncCtx->genAssocs.empty())
				{
					TypeSPtr type = typeInfo.handle.Type();
					
					if (type->typeKind == TypeKind::Generic)
					{
						StdString name = type->AsGeneric().iden->Name();
						auto it = m_FuncCtx->genAssocs.find(name);
						if (it != m_FuncCtx->genAssocs.end())
							typeInfo.genInfo = it->second;
					}
				}
				
				LocalVarDataSPtr var{ new LocalVarData{ param->iden, typeInfo, true } };
				m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
			}

			for (ITrStmtSPtr stmt : body->stmts)
			{
				ITrVisitor::Visit(stmt);
			}
		});
	}

	void LocalVarCollection::Visit(ITrBlock& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void LocalVarCollection::Visit(ITrLoop& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void LocalVarCollection::Visit(ITrSwitch& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void LocalVarCollection::Visit(ITrLocalVar& node)
	{
		for (IdenSPtr iden : node.idens)
		{
			LocalVarDataSPtr var{ new LocalVarData{ iden } };
			m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
		}
	}
}
