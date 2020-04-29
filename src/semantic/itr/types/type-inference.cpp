#include "type-inference.hpp"



#include "ast/ast.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "itr/itr.hpp"
#include "module/function.hpp"
#include "module/module.hpp"

namespace Noctis
{
	TypeInference::TypeInference(Context* pCtx)
		: ITrSemanticPass("type inference", pCtx)
	{
	}

	void TypeInference::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			ITrBodySPtr body = mod.GetBody(node);
			if (!body)
				return;

			m_FuncCtx = node.ctx;
			StdVector<StdString> scopeNames{};
			for (ITrParamSPtr param : node.params)
			{
				LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, param->iden);
			}
			
			for (ITrStmtSPtr stmt : body->stmts)
			{
				ITrVisitor::Visit(stmt);
			}
		});
		
	}

	void TypeInference::Visit(ITrBlock& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void TypeInference::Visit(ITrLocalVar& node)
	{
		ITrVisitor::Visit(node);

		usize idenCount = node.idens.size();
		StdVector<TypeHandle> types;
		types.reserve(idenCount);
		
		if (node.init)
		{
			if (idenCount == 1)
			{
				types.push_back(node.init->typeHandle); 
			}
			else
			{
				TypeSPtr type = m_pCtx->typeReg.GetType(node.init->typeHandle);
			}
		}

		if (node.type)
		{
			if (types.empty())
			{
				for (usize i = 0; i < idenCount; ++i)
					types.push_back(node.type->handle);
			}
			else
			{
				// TODO
			}
		}
		
		for (IdenSPtr iden : node.idens)
		{
			LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, iden);
		}
	}

	void TypeInference::Visit(ITrAssign& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;
		
		ITrVisitor::Visit(node);

		TypeHandle lTypeHandle = node.lExpr->typeHandle;
		TypeHandle rTypeHandle = node.rExpr->typeHandle;
		
		if (node.op == OperatorKind::Eq)
		{
			TypeHandle baseRTypeHandle = m_pCtx->typeReg.Mod(TypeMod::None, rTypeHandle);
			if (!m_pCtx->typeReg.AreTypesEqual(lTypeHandle, baseRTypeHandle))
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Condition should be of type 'bool'\n");
			}
		}
		else
		{
			Operator& op = m_pCtx->activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle);
			if (!op.sym)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdStringView opName = GetOpName(node.op);
				StdString lTypeName = m_pCtx->typeReg.ToString(lTypeHandle);
				StdString rTypeName = m_pCtx->typeReg.ToString(rTypeHandle);
				g_ErrorSystem.Error(span, "Binary operator '%s' not found for '%s' and '%s'\n", opName.data(), lTypeName.c_str(), rTypeName.c_str());
			}
		}

		node.typeHandle = lTypeHandle;
	}

	void TypeInference::Visit(ITrTernary& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;
		
		ITrVisitor::Visit(node);

		TypeHandle condTypeHandle = node.cond->typeHandle;
		condTypeHandle = m_pCtx->typeReg.Mod(TypeMod::None, condTypeHandle);
		TypeSPtr condType = m_pCtx->typeReg.GetType(condTypeHandle);
		if (condType->typeKind != TypeKind::Builtin ||
			condType->AsBuiltin().builtin != BuiltinTypeKind::Bool)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.cond->astNode)->ctx->startIdx);
			g_ErrorSystem.Error(span, "Condition should be of type 'bool'\n");
		}

		TypeHandle tTypeHandle = node.tExpr->typeHandle;
		TypeHandle fTypeHandle = node.fExpr->typeHandle;
		if (!m_pCtx->typeReg.AreTypesEqual(tTypeHandle, fTypeHandle))
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			g_ErrorSystem.Error(span, "Both sides need to be of the same type\n");
		}

		node.typeHandle = tTypeHandle;
	}

	void TypeInference::Visit(ITrBinary& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		ITrVisitor::Visit(node);

		TypeHandle lTypeHandle = node.lExpr->typeHandle;
		TypeHandle rTypeHandle = node.rExpr->typeHandle;

		Operator& op = m_pCtx->activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle);
		node.typeHandle = op.result;

		if (!op.sym)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			StdStringView opName = GetOpName(node.op);
			StdString lTypeName = m_pCtx->typeReg.ToString(lTypeHandle);
			StdString rTypeName = m_pCtx->typeReg.ToString(rTypeHandle);
			g_ErrorSystem.Error(span, "Binary operator '%s' not found for '%s' and '%s'\n", opName.data(), lTypeName.c_str(), rTypeName.c_str());
		}
	}

	void TypeInference::Visit(ITrUnary& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		ITrVisitor::Visit(node);

		TypeHandle exprTypeHandle = node.expr->typeHandle;

		Operator& op = m_pCtx->activeModule->opTable.GetOperator(node.op, exprTypeHandle);
		node.typeHandle = op.result;

		if (!op.sym)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			StdStringView opName = GetOpName(node.op);
			StdString typeName = m_pCtx->typeReg.ToString(exprTypeHandle);
			g_ErrorSystem.Error(span, "Unary operator '%s' not found for '%s'\n", opName.data(), typeName.c_str());
		}
	}

	void TypeInference::Visit(ITrQualNameExpr& node)
	{
		QualNameSPtr qualName = node.qualName;

		if (!qualName->Disambiguation() && !qualName->Base())
		{
			LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, qualName->Iden());
			if (local)
			{
				node.typeHandle = local->type;
				return;
			}
		}

		if (qualName->Disambiguation())
		{
			
		}
		
	}

	void TypeInference::Visit(ITrIndexSlice& node)
	{
	}

	void TypeInference::Visit(ITrFuncCall& node)
	{
	}

	void TypeInference::Visit(ITrAdtTupleEnumInit& node)
	{
	}

	void TypeInference::Visit(ITrMemberAccess& node)
	{
	}

	void TypeInference::Visit(ITrTupleAccess& node)
	{
	}

	void TypeInference::Visit(ITrLiteral& node)
	{
	}

	void TypeInference::Visit(ITrAggrInit& node)
	{
	}

	void TypeInference::Visit(ITrAdtAggrEnumInit& node)
	{
	}

	void TypeInference::Visit(ITrTupleInit& node)
	{
	}

	void TypeInference::Visit(ITrArrayInit& node)
	{
	}

	void TypeInference::Visit(ITrCast& node)
	{
	}

	void TypeInference::Visit(ITrBlockExpr& node)
	{
	}

	void TypeInference::Visit(ITrUnsafeExpr& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		ITrVisitor::Visit(node);

		node.typeHandle = node.expr->typeHandle;
	}

	void TypeInference::Visit(ITrComma& node)
	{
	}

	void TypeInference::Visit(ITrClosure& node)
	{
	}

	void TypeInference::Visit(ITrMove& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		ITrVisitor::Visit(node);
		
		node.typeHandle = node.expr->typeHandle;
	}

	void TypeInference::Visit(ITrIs& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		ITrVisitor::Visit(node);

		node.typeHandle = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);
	}

	void TypeInference::Visit(ITrTry& node)
	{
	}

	void TypeInference::Visit(ITrSpecKw& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		switch (node.kw)
		{
		case TokenType::SLine:
		{
			node.typeHandle = m_pCtx->typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::USize);
			break;
		}
		case TokenType::SFile:
		case TokenType::SFileFullPath:
		case TokenType::SModule:
		case TokenType::SFullModule:
		case TokenType::SPackage:
		case TokenType::SFunc:
		case TokenType::SFuncName:
		case TokenType::SPrettyFunc:
		{
			TypeHandle tmp = m_pCtx->typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::Char);
			node.typeHandle = m_pCtx->typeReg.Slice(TypeMod::Const, tmp);
			break;
		}
		default:;
		}
	}

	void TypeInference::Visit(ITrCompRun& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		ITrVisitor::Visit(node);

		node.typeHandle = node.expr->typeHandle;
	}
}
