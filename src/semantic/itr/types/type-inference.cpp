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
		, m_Prepass(false)
	{
	}

	void TypeInference::SetPrepass()
	{
		m_Prepass = true;
		m_Name = "func type prepass";
	}

	void TypeInference::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_FuncScope = node.qualName;
			m_FuncCtx = node.ctx;

			m_InterfaceQualname = nullptr;
			SymbolSPtr parent = node.sym.lock()->parent.lock();
			if (parent &&
				parent->kind != SymbolKind::StrongInterface &&
				parent->kind != SymbolKind::WeakInterface &&
				!parent->impls.empty())
				m_InterfaceQualname = parent->impls[0].first->qualName;

			if (m_Prepass)
			{
				StdVector<TypeHandle> paramTypes;
				paramTypes.reserve(node.params.size());
				for (ITrParamSPtr param : node.params)
				{
					Visit(*param->type);
					paramTypes.push_back(param->type->handle);
				}

				TypeHandle retType = TypeHandle(-1);
				if (node.retType)
				{
					Visit(*node.retType);
					retType = node.retType->handle;
				}

				TypeHandle type = m_pCtx->typeReg.Func(TypeMod::None, paramTypes, retType);
				node.sym.lock()->type = type;
			}
			else
			{
				ITrBodySPtr body = mod.GetBody(node);
				if (!body)
					return;

				for (ITrParamSPtr param : node.params)
				{
					LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, param->iden);
				}

				for (ITrStmtSPtr stmt : body->stmts)
				{
					ITrVisitor::Visit(stmt);
				}
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
		node.isBuiltinOp = op.isBuiltin;

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
		TypeRegistry& typeReg = m_pCtx->typeReg;
		
		switch (node.lit.Type())
		{
		case TokenType::CharLit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::Char);
			break;
		}
		case TokenType::F16Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::F16);
			break;
		}
		case TokenType::F32Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::F32);
			break;
		}
		case TokenType::F64Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::F64);
			break;
		}
		case TokenType::F128Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::F128);
			break;
		}
		case TokenType::I8Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::I8);
			break;
		}
		case TokenType::I16Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::I16);
			break;
		}
		case TokenType::I32Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::I32);
			break;
		}
		case TokenType::I64Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::I64);
			break;
		}
		case TokenType::I128Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::I128);
			break;
		}
		case TokenType::StringLit:
		{
			TypeHandle charType = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::Char);
			node.typeHandle = typeReg.Slice(TypeMod::Const, charType);
			break;
		}
		case TokenType::U8Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::U8);
			break;
		}
		case TokenType::U16Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::U16);
			break;
		}
		case TokenType::U32Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::U32);
			break;
		}
		case TokenType::U64Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::U64);
			break;
		}
		case TokenType::U128Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::U128);
			break;
		}
		default: ;
		}
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

	void TypeInference::Visit(ITrType& node)
	{
		for (ITrTypeSPtr subType : node.subTypes)
		{
			Visit(*subType);
		}
		if (node.expr)
			ITrVisitor::Visit(node.expr);
		
		TypeSPtr type = m_pCtx->typeReg.GetType(node.handle);
		switch (type->typeKind)
		{
		case TypeKind::Builtin:
			break;
		case TypeKind::Iden:
		{
			QualNameSPtr scope = m_FuncScope; 
			for (StdString& scopeName : m_ScopeNames)
			{
				scope = QualName::Create(m_FuncScope, Iden::Create(scopeName));
			}

			SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(scope, type->AsIden().qualName, m_InterfaceQualname);
			node.handle = sym->type;
			break;
		}
		case TypeKind::Ptr:
		{
			node.handle = m_pCtx->typeReg.Ptr(type->mod, node.subTypes[0]->handle);
			break;
		}
		case TypeKind::Ref:
		{
			node.handle = m_pCtx->typeReg.Ref(type->mod, node.subTypes[0]->handle);
			break;
		}
		case TypeKind::Slice:
		{
			node.handle = m_pCtx->typeReg.Slice(type->mod, node.subTypes[0]->handle);
			break;
		}
		case TypeKind::Array:
		{
			node.handle = m_pCtx->typeReg.Array(type->mod, node.subTypes[0]->handle, type->AsArray().size);
			break;
		}
		case TypeKind::Tuple:
		{
			StdVector<TypeHandle> subTypes;
			for (ITrTypeSPtr subType : node.subTypes)
			{
				subTypes.push_back(subType->handle);
			}
			
			node.handle = m_pCtx->typeReg.Tuple(type->mod, subTypes);
			break;
		}
		case TypeKind::Opt:
		{
			node.handle = m_pCtx->typeReg.Opt(type->mod, node.subTypes[0]->handle);
			break;
		}
		case TypeKind::Func:
		{
			// TODO
			//StdVector<TypeHandle> subTypes;
			//for (ITrTypeSPtr subType : node.subTypes)
			//{
			//	subTypes.push_back(subType->handle);
			//}
			//
			//node.handle = m_pCtx->typeReg.Func(type->mod, subTypes);
			break;
		}
		default: ;
		}
		
	}
}
