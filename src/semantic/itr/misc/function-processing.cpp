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
				LocalVarDataSPtr var{ new LocalVarData{ param->iden, param->type->handle, true } };
				m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
			}

			for (ITrStmtSPtr stmt : body->stmts)
			{
				ITrVisitor::Visit(stmt);
			}
		});

		Foreach(ITrVisitorDefKind::Any, [&, this](ITrErrHandler& node)
		{
			m_FuncCtx = node.ctx;
			ITrBodySPtr body = mod.GetBody(node);
			if (!body)
				return;

			StdVector<StdString> scopeNames{};
			{
				LocalVarDataSPtr var{ new LocalVarData{ node.errIden, {}, true } };
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
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrForRange& node)
	{
		m_ScopeNames.push_back(node.scopeName);

		for (const StdString& iden : node.idens)
		{
			LocalVarDataSPtr var{ new LocalVarData{ iden } };
			m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
		}
		
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
		for (const StdString& iden : node.idens)
		{
			LocalVarDataSPtr var{ new LocalVarData{ iden } };
			m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
		}
	}

	ErrorHandlerCollectionPass::ErrorHandlerCollectionPass(Context* pCtx)
		: ITrSemanticPass("Error Handler Collection Pass", pCtx)
	{
	}

	void ErrorHandlerCollectionPass::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [&](ITrStruct& node)
		{
			Walk(node);
		});
	}

	void ErrorHandlerCollectionPass::Visit(ITrErrHandler& node)
	{
	}
}
