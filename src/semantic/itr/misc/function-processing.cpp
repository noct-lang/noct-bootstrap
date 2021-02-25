#include "function-processing.hpp"


#include "common/context.hpp"
#include "common/utils.hpp"
#include "itr/itr.hpp"
#include "module/function.hpp"
#include "module/module.hpp"
#include "semantic/ast/misc/iden-scope-pass.hpp"

namespace Noctis
{
	LocalVarCollection::LocalVarCollection()
		: ITrSemanticPass("local var collection")
		, m_SwitchImmIdx(0)
	{
	}

	void LocalVarCollection::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Any, [&, this](ITrFunc& node)
		{
			ITrBodySPtr body = mod.GetBody(node);
			if (!body)
				return;

			m_FuncCtx = node.ctx;
			m_SwitchImmIdx = 0;
			
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
			ITrBodySPtr body = mod.GetBody(node);
			if (!body)
				return;
			
			m_FuncCtx = node.ctx;
			m_SwitchImmIdx = 0;

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

		for (ITrSwitchCase& case_ : node.cases)
		{
			m_ScopeNames.push_back(case_.block->scopeName);

			ITrVisitor::Visit(case_.pattern);

			SaveRestore savRes{ m_SwitchImmIdx, u64(0) };
			for (ITrStmtSPtr stmt : case_.block->stmts)
			{
				ITrVisitor::Visit(stmt);
			}

			m_ScopeNames.pop_back();
		}
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

	void LocalVarCollection::Visit(ITrAdtAggrEnumPattern& node)
	{
		node.imm = Format("__imm%u", m_SwitchImmIdx++);
		LocalVarDataSPtr var{ new LocalVarData{ node.imm } };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrAdtTupleEnumPattern& node)
	{
		node.imm = Format("__imm%u", m_SwitchImmIdx++);
		LocalVarDataSPtr var{ new LocalVarData{ node.imm } };
		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(scopeNames, var);
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrAggrPattern& node)
	{
		node.imm = Format("__imm%u", m_SwitchImmIdx++);
		LocalVarDataSPtr var{ new LocalVarData{ node.imm } };
		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(scopeNames, var);
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node)
	{
		node.imm = Format("__imm%u", m_SwitchImmIdx++);
		LocalVarDataSPtr var{ new LocalVarData{ node.imm } };
		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(scopeNames, var);
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrSlicePattern& node)
	{
		node.imm = Format("__imm%u", m_SwitchImmIdx++);
		LocalVarDataSPtr var{ new LocalVarData{ node.imm } };
		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(scopeNames, var);
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrTuplePattern& node)
	{
		node.imm = Format("__imm%u", m_SwitchImmIdx++);
		LocalVarDataSPtr var{ new LocalVarData{ node.imm } };
		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(scopeNames, var);
		Walk(node);
	}

	void LocalVarCollection::Visit(ITrValueBindPattern& node)
	{
		LocalVarDataSPtr var{ new LocalVarData{ node.iden } };
		m_FuncCtx->localVars.AddLocalVarDeclSPtr(m_ScopeNames, var);
	}

	ErrorHandlerCollectionPass::ErrorHandlerCollectionPass()
		: ITrSemanticPass("Error Handler Collection Pass")
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
