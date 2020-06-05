#include "type-inference.hpp"



#include "ast/ast.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "itr/itr.hpp"
#include "module/function.hpp"
#include "module/module.hpp"


// TODO: Inference needs to work in the opposite direction, e.g. should infer type of return from func, type of the arg from the expected type, etc
namespace Noctis
{

	TypeInference::TypeInference(Context* pCtx)
		: ITrSemanticPass("type inference", pCtx)
		, m_Prepass(false)
		, m_SelfType(TypeHandle(-1))
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

		if (m_Prepass)
		{
			Foreach(ITrVisitorDefKind::Any, [&](ITrStruct& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node.genDecl, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrUnion& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node.genDecl, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrAdtEnum& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node.genDecl, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrStrongInterface& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node.genDecl, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrWeakInterface& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node.genDecl, node.qualName->Iden());
			});
		}

		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_Scope = node.qualName;
			m_FuncCtx = node.ctx;
			m_SelfType = node.selfType;

			if (node.genDecl)
				HandleGenerics(node.genDecl, node.qualName->Iden());

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

				for (ITrStmtSPtr& stmt : body->stmts)
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
		Walk(node);

		usize idenCount = node.idens.size();
		StdVector<TypeHandle> types;
		types.reserve(idenCount);
		
		if (node.init)
		{
			if (idenCount == 1)
			{
				TypeSPtr type = m_pCtx->typeReg.GetType(node.init->typeHandle);
				if (type->typeKind == TypeKind::Func)
				{
					types.push_back(type->AsFunc().retType);
				}
				else
				{
					types.push_back(node.init->typeHandle); 
				}
			}
			else
			{
				TypeSPtr type = m_pCtx->typeReg.GetType(node.init->typeHandle);
				// TODO
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

		for (usize i = 0; i < node.idens.size(); ++i)
		{
			IdenSPtr iden = node.idens[i];
			LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, iden);

			TypeHandle type = m_pCtx->typeReg.Mod(TypeMod::None, types[0]);

			if (node.attribs)
			{
				if (ENUM_IS_SET(node.attribs->attribs, Attribute::Const))
				{
					type = m_pCtx->typeReg.Mod(TypeMod::Const, type);
				}
				else if (ENUM_IS_SET(node.attribs->attribs, Attribute::Immutable))
				{
					// TODO
				}
					
			}
			
			localVar->type = type;
		}
	}

	void TypeInference::Visit(ITrAssign& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;
		
		Walk(node);

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

			node.operator_ = op;
		}

		node.typeHandle = lTypeHandle;
	}

	void TypeInference::Visit(ITrTernary& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;
		
		Walk(node);

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

		Walk(node);

		TypeHandle lTypeHandle = node.lExpr->typeHandle;
		TypeHandle rTypeHandle = node.rExpr->typeHandle;

		Operator& op = m_pCtx->activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle);
		node.typeHandle = op.result;
		node.operator_ = op;

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

		Walk(node);

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
			// TODO

			return;
		}

		SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), qualName);
		if (!sym)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString varName = qualName->ToString();
			g_ErrorSystem.Error(span, "Cannot find '%s'", varName);
			return;
		}
		
		node.sym = sym;
		node.typeHandle = node.sym->SelfType();
	}

	void TypeInference::Visit(ITrIndexSlice& node)
	{
	}

	void TypeInference::Visit(ITrExprSPtr& ptr, ITrAmbiguousCall node)
	{
		Walk(node);

		if (node.expr->sym)
		{
			SymbolSPtr sym = node.expr->sym;
			if (sym->kind == SymbolKind::ValEnum)
			{
				// TODO
			}
			else
			{
				ptr.reset(new ITrFuncCall{ node.expr, std::move(node.args) });
				ptr->sym = sym;
				ptr->typeHandle = sym->SelfType();
			}
		}
		else
		{
			// TODO
		}
	}

	void TypeInference::Visit(ITrAdtTupleEnumInit& node)
	{
	}

	void TypeInference::Visit(ITrMemberAccess& node)
	{
		Walk(node);

		TypeSPtr type = m_pCtx->typeReg.GetType(node.expr->typeHandle);
		if (type->typeKind == TypeKind::Ref)
		{
			type = m_pCtx->typeReg.GetType(type->AsRef().subType);
		}
		else if (type->typeKind == TypeKind::Ptr)
		{
			// TODO
		}

		if (type->typeKind != TypeKind::Iden)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString typeName = m_pCtx->typeReg.ToString(type);
			g_ErrorSystem.Error(span, "Cannot use a member access on a value of type '%s'", typeName);
			return;
		}

		SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), type->AsIden().qualName);
		if (!sym)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString typeName = m_pCtx->typeReg.ToString(type);
			g_ErrorSystem.Error(span, "Cannot find symbol for type '%s'", typeName);
			return;
		}

		SymbolSPtr child = sym->children->FindChild(nullptr, node.iden);
		if (!child || child->kind != SymbolKind::Var)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString typeName = m_pCtx->typeReg.ToString(type);
			StdString varName = node.iden->Name();
			g_ErrorSystem.Error(span, "'%s' does not have a member named '%s'", typeName, varName);
			return;
		}

		node.typeHandle = child->type;
	}

	void TypeInference::Visit(ITrTupleAccess& node)
	{
		Walk(node);
		
		TypeSPtr type = m_pCtx->typeReg.GetType(node.expr->typeHandle);
		if (type->typeKind == TypeKind::Ref)
		{
			type = m_pCtx->typeReg.GetType(type->AsRef().subType);
		}
		else if (type->typeKind == TypeKind::Ptr)
		{
			// TODO
		}

		if (type->typeKind != TypeKind::Tuple)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString typeName = m_pCtx->typeReg.ToString(node.expr->typeHandle);
			g_ErrorSystem.Error(span, "Cannot use a tuple access on a value of type '%s'", typeName);
			return;
		}

		TupleType& tupType = type->AsTuple();
		if (node.index >= tupType.subTypes.size())
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			g_ErrorSystem.Error(span, "Tuple index out of range, trying to use index %u on a tuple with size %u", node.index, tupType.subTypes.size());
			return;
		}

		node.typeHandle = tupType.subTypes[node.index];
	}

	void TypeInference::Visit(ITrLiteral& node)
	{
		TypeRegistry& typeReg = m_pCtx->typeReg;
		
		switch (node.lit.Type())
		{
		case TokenType::True:
		case TokenType::False:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);
			break;
		}
		case TokenType::CharLit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char);
			break;
		}
		case TokenType::F16Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F16);
			break;
		}
		case TokenType::F32Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F32);
			break;
		}
		case TokenType::F64Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F64);
			break;
		}
		case TokenType::F128Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F128);
			break;
		}
		case TokenType::I8Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I8);
			break;
		}
		case TokenType::I16Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I16);
			break;
		}
		case TokenType::I32Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I32);
			break;
		}
		case TokenType::I64Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I64);
			break;
		}
		case TokenType::I128Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I128);
			break;
		}
		case TokenType::StringLit:
		{
			TypeHandle charType = typeReg.Builtin(TypeMod::Const, BuiltinTypeKind::Char);
			node.typeHandle = typeReg.Slice(TypeMod::None, charType);
			break;
		}
		case TokenType::U8Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U8);
			break;
		}
		case TokenType::U16Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16);
			break;
		}
		case TokenType::U32Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U32);
			break;
		}
		case TokenType::U64Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U64);
			break;
		}
		case TokenType::U128Lit:
		{
			node.typeHandle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U128);
			break;
		}
		default: ;
		}
	}

	void TypeInference::Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node)
	{
		Walk(node);

		//StdVector<TypeHandle> subTypes;
		//for (ITrExprSPtr expr : node.args)
		//{
		//	subTypes.push_back(expr->typeHandle);
		//}

		TypeSPtr objType = m_pCtx->typeReg.GetType(node.type->handle);
		if (objType->typeKind != TypeKind::Iden)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			StdString typeName = m_pCtx->typeReg.ToString(objType);
			g_ErrorSystem.Error(span, "'%s' is not a valid type in an aggregate initializer\n", node.args.size());
			return;
		}

		IdenType& idenType = objType->AsIden();
		SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), idenType.qualName);

		bool checkAggrArgs = false;
		if (sym->kind == SymbolKind::AdtEnumMember)
		{
			// TODO: member with tuple

			TypeSPtr type = m_pCtx->typeReg.GetType(sym->type);
			if (type->typeKind == TypeKind::Tuple)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString typeName = m_pCtx->typeReg.ToString(objType);
				g_ErrorSystem.Error(span, "'%s' needs to be initialized with parenthesis\n", node.args.size());
				return;
			}
			ptr.reset(new ITrAdtAggrEnumInit{ node.type, std::move(node.args) });
			ptr->typeHandle = sym->SelfType();
			ptr->sym = sym;
			checkAggrArgs = m_pCtx->typeReg.IsType(sym->type, TypeKind::Iden);

			ITrAdtAggrEnumInit& adtNode = *reinterpret_cast<ITrAdtAggrEnumInit*>(ptr.get());

			StdVector<SymbolSPtr> children;
			children.reserve(sym->orderedVarChildren.size());
			StdUnorderedMap<StdString, u32> childrenNameMapping;

			for (SymbolWPtr childW : sym->orderedVarChildren)
			{
				SymbolSPtr child = childW.lock();

				childrenNameMapping.try_emplace(child->qualName->Iden()->Name(), u32(children.size()));
				children.push_back(child);
			}

			bool hasIden = false;

			StdVector<TypeHandle> argTypes;
			argTypes.resize(children.size(), TypeHandle(-1));

			if (adtNode.args.size() > children.size())
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Found more arguments then expected, expected %u, found %u\n", adtNode.args.size(), children.size());
				return;
			}

			for (usize i = 0; i < adtNode.args.size(); ++i)
			{
				ITrArgSPtr arg = adtNode.args[i];
				if (arg->iden)
				{
					if (i == 0)
					{
						hasIden = true;
					}
					else if (!hasIden)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an aggregate\n");
						return;
					}

					auto it = childrenNameMapping.find(arg->iden->Name());
					if (it == childrenNameMapping.end())
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "The aggregate does not contain any variable named '%s'\n", name.c_str());
						return;
					}

					u32 idx = it->second;
					if (argTypes[idx] != TypeHandle(-1))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "Variable '%s' has already been assigned\n", name.c_str());
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeHandle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
					argTypes[idx] = arg->expr->typeHandle;

					adtNode.argOrder.push_back(idx);
				}
				else
				{
					if (hasIden && i != 0)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an aggregate\n");
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeHandle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
				}
			}

			u32 argCount;
			if (hasIden)
			{
				u32 argCount = 0;
				for (TypeHandle argType : argTypes)
				{
					argCount += u32(argType != TypeHandle(-1));
				}
			}
			else
			{
				argCount = u32(adtNode.args.size());
			}

			if (argCount < children.size())
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Not all arguments have been initialized\n");
				return;
			}
		}
		else if (sym->kind == SymbolKind::Struct)
		{
			ptr.reset(new ITrStructInit{ node.type, std::move(node.args), false, nullptr });
			ptr->typeHandle = sym->SelfType();
			ptr->sym = sym;
			checkAggrArgs = true;

			ITrStructInit& structNode = *reinterpret_cast<ITrStructInit*>(ptr.get());

			StdVector<SymbolSPtr> children;
			children.reserve(sym->orderedVarChildren.size());
			StdUnorderedMap<StdString, u32> childrenNameMapping;

			for (SymbolWPtr childW : sym->orderedVarChildren)
			{
				SymbolSPtr child = childW.lock();

				childrenNameMapping.try_emplace(child->qualName->Iden()->Name(), u32(children.size()));
				children.push_back(child);
			}

			bool hasIden = false;

			StdVector<TypeHandle> argTypes;
			argTypes.resize(children.size(), TypeHandle(-1));

			if (structNode.args.size() > children.size())
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Found more arguments then expected, expected %u, found %u\n", structNode.args.size(), children.size());
				return;
			}

			for (usize i = 0; i < structNode.args.size(); ++i)
			{
				ITrArgSPtr arg = structNode.args[i];
				if (arg->iden)
				{
					if (i == 0)
					{
						hasIden = true;
					}
					else if (!hasIden)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an aggregate\n");
						return;
					}

					auto it = childrenNameMapping.find(arg->iden->Name());
					if (it == childrenNameMapping.end())
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "The aggregate does not contain any variable named '%s'\n", name.c_str());
						return;
					}

					u32 idx = it->second;
					if (argTypes[idx] != TypeHandle(-1))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "Variable '%s' has already been assigned\n", name.c_str());
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeHandle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
					argTypes[idx] = arg->expr->typeHandle;

					structNode.argOrder.push_back(idx);
				}
				else
				{
					if (hasIden && i != 0)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an aggregate\n");
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeHandle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
				}
			}

			u32 argCount;
			if (hasIden)
			{
				argCount = 0;
				for (TypeHandle argType : argTypes)
				{
					argCount += u32(argType != TypeHandle(-1));
				}
			}
			else
			{
				argCount = u32(structNode.args.size());
			}

			if (argCount < children.size())
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Not all arguments have been initialized\n");
				return;
			}
		}
		else if (sym->kind == SymbolKind::Union)
		{
			ptr.reset(new ITrUnionInit{ node.type, std::move(node.args) });
			ptr->typeHandle = sym->SelfType();
			ptr->sym = sym;

			ITrUnionInit& unionNode = *reinterpret_cast<ITrUnionInit*>(ptr.get());

			StdVector<SymbolSPtr> children;
			children.reserve(sym->orderedVarChildren.size());
			StdUnorderedMap<StdString, u32> childrenNameMapping;

			for (SymbolWPtr childW : sym->orderedVarChildren)
			{
				SymbolSPtr child = childW.lock();

				childrenNameMapping.try_emplace(child->qualName->Iden()->Name(), u32(children.size()));
				children.push_back(child);
			}

			if (unionNode.args.size() > 0)
			{
				ITrArgSPtr arg = unionNode.args[0];

				if (!arg->iden)
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(unionNode.astNode)->ctx->startIdx);
					g_ErrorSystem.Error(span, "Argument used to initialize a union require an identifier\n");
					return;
				}

				SymbolSPtr foundChild;
				for (SymbolSPtr child : children)
				{
					const StdString& name = child->qualName->Iden()->Name();
					if (name == arg->iden->Name())
					{
						foundChild = child;
						break;
					}
				}

				if (foundChild)
				{
					TypeHandle expected = foundChild->type;
					TypeHandle argType = arg->expr->typeHandle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}

					if (unionNode.args.size() > 1)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(unionNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot initialize more than 1 union member\n");
						return;
					}
				}
				else
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(unionNode.astNode)->ctx->startIdx);
					const StdString& name = arg->iden->Name();
					g_ErrorSystem.Error(span, "Nesting of anonymous structures is not available in the bootstrap compiler or no member with the name '%s' exists\n", name.c_str());
					return;
				}
			}
			else
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(unionNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Cannot initialize a union without any members\n");
				return;
			}
		}
		else
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			StdString typeName = m_pCtx->typeReg.ToString(objType);
			g_ErrorSystem.Error(span, "'%s' is not an aggregate or a adt-enum\n", node.args.size());
			return;
		}
	}

	void TypeInference::Visit(ITrStructInit& node)
	{
		Walk(node);

		TypeSPtr objType = m_pCtx->typeReg.GetType(node.type->handle);
		if (objType->typeKind != TypeKind::Iden)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			StdString typeName = m_pCtx->typeReg.ToString(objType);
			g_ErrorSystem.Error(span, "'%s' is not a valid type in an aggregate initializer\n", node.args.size());
			return;
		}

		IdenType& idenType = objType->AsIden();
		SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), idenType.qualName);

		// Collect children
		StdVector<SymbolSPtr> children;
		children.reserve(sym->orderedVarChildren.size());
		StdUnorderedMap<StdString, u32> childrenNameMapping;

		for (SymbolWPtr childW : sym->orderedVarChildren)
		{
			SymbolSPtr child = childW.lock();

			childrenNameMapping.try_emplace(child->qualName->Iden()->Name(), u32(children.size()));
			children.push_back(child);
		}

		bool hasIden = false;

		StdVector<TypeHandle> argTypes;
		argTypes.resize(children.size(), TypeHandle(-1));

		if (node.args.size() > children.size())
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			g_ErrorSystem.Error(span, "Found more arguments then expected, expected %u, found %u\n", node.args.size(), children.size());
			return;
		}
		
		for (usize i = 0; i < node.args.size(); ++i)
		{
			ITrArgSPtr arg = node.args[i];
			if (arg->iden)
			{
				if (i == 0)
				{
					hasIden = true;
				}
				else if (!hasIden)
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
					g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an aggregate\n");
					return;
				}

				auto it = childrenNameMapping.find(arg->iden->Name());
				if (it == childrenNameMapping.end())
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
					const StdString& name = arg->iden->Name();
					g_ErrorSystem.Error(span, "The aggregate does not contain any variable named '%s'\n", name.c_str());
					return;
				}

				u32 idx = it->second;
				if (argTypes[idx] != TypeHandle(-1))
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
					const StdString& name = arg->iden->Name();
					g_ErrorSystem.Error(span, "Variable '%s' has already been assigned\n", name.c_str());
					return;
				}

				argTypes[idx] = arg->expr->typeHandle;
				node.argOrder.push_back(idx);
			}
			else
			{
				if (hasIden && i != 0)
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
					g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an aggregate\n");
					return;
				}

				TypeHandle expected = children[i]->type;
				TypeHandle argType = arg->expr->typeHandle;
				if (!m_pCtx->typeReg.CanPassTo(expected, argType))
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
					StdString expectedName = m_pCtx->typeReg.ToString(expected);
					StdString argName = m_pCtx->typeReg.ToString(argType);
					g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
					return;
				}
			}
		}

		u32 argCount;
		if (hasIden)
		{
			u32 argCount = 0;
			for (TypeHandle argType : argTypes)
			{
				argCount += u32(argCount != TypeHandle(-1));
			}
		}
		else
		{
			argCount = u32(node.args.size());
		}

		if (argCount < children.size())
		{
			if (node.hasDefInit)
			{
				QualNameSPtr defInterfaceQualName = QualName::Create(StdVector<StdString>{ "core", "default", "Default" });

				bool implsDefault = false;
				for (StdPair<SymbolSPtr, bool>& pair : sym->impls)
				{
					if (pair.first->qualName == defInterfaceQualName)
					{
						implsDefault = true;
						break;
					}
				}

				if (!implsDefault)
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
					StdString symName = sym->qualName->ToString();
					g_ErrorSystem.Error(span, "'%s' does not implement 'core.default.Default' or 'std.default.Default'\n", symName.c_str());
				}
			}
			else if (node.defExpr)
			{
				TypeHandle defType = node.defExpr->typeHandle;
				defType = m_pCtx->typeReg.Mod(TypeMod::None, defType);

				bool validDef = false;
				if (!m_pCtx->typeReg.AreTypesEqual(defType, sym->type))
				{
					TypeSPtr type = m_pCtx->typeReg.GetType(defType);
					if (type->typeKind == TypeKind::Ref)
					{
						defType = type->AsRef().subType;
						defType = m_pCtx->typeReg.Mod(TypeMod::None, defType);

						validDef = m_pCtx->typeReg.AreTypesEqual(defType, sym->type);
					}
				}
				else
				{
					validDef = true;
				}

				if (!validDef)
				{
					Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
					StdString typeName = m_pCtx->typeReg.ToString(defType);
					g_ErrorSystem.Error(span, "cannot initialize unspecified members from an expression with type '%s'\n", typeName.c_str());
				}
				
			}
			else
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Not all arguments have been initialized\n");
				return;
			}
		}
	}

	void TypeInference::Visit(ITrTupleInit& node)
	{
		Walk(node);

		StdVector<TypeHandle> subTypes;
		for (ITrExprSPtr expr : node.exprs)
		{
			subTypes.push_back(expr->typeHandle);
		}

		node.typeHandle = m_pCtx->typeReg.Tuple(TypeMod::None, subTypes);
	}

	void TypeInference::Visit(ITrArrayInit& node)
	{
	}

	void TypeInference::Visit(ITrCast& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		Walk(node);

		TypeHandle fromType = node.expr->typeHandle;
		TypeHandle toType = node.type->handle;

		Operator op;
		switch (node.castKind)
		{
		
		case ITrCastKind::SafeCast:
		case ITrCastKind::NullPanicCast:
		{
			op = m_pCtx->activeModule->opTable.GetOperator(OperatorKind::TryCast, fromType, toType);
			node.castToTryCast = !op.sym;
			// Fallthrough
		}
		case ITrCastKind::Cast:
		{
			if (!op.sym)
				op = m_pCtx->activeModule->opTable.GetOperator(OperatorKind::Cast, fromType, toType);

			node.typeHandle = op.result;
			node.operator_ = op;

			if (!op.sym)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString lTypeName = m_pCtx->typeReg.ToString(fromType);
				StdString rTypeName = m_pCtx->typeReg.ToString(toType);
				StdString castName;
				switch (node.castKind)
				{
				case ITrCastKind::Cast: castName = "Cast"; break;
				case ITrCastKind::SafeCast: castName = "Safe cast"; break;
				case ITrCastKind::NullPanicCast: castName = "Null-panicking cast"; break;
				default:;
				}

				g_ErrorSystem.Error(span, "%s operator not found for '%s' -> '%s'\n", castName.c_str(), lTypeName.c_str(), rTypeName.c_str());
			}
			
			break;
		}
		case ITrCastKind::Transmute:
		{
			u64 fromSize = m_pCtx->typeReg.GetType(fromType)->size;
			u64 toSize = m_pCtx->typeReg.GetType(fromType)->size;

			if (fromSize != toSize)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString lTypeName = m_pCtx->typeReg.ToString(fromType);
				StdString rTypeName = m_pCtx->typeReg.ToString(toType);
				g_ErrorSystem.Error(span, "cannot transmute %s -> %s, since they are different sizes\n", lTypeName.c_str(), rTypeName.c_str());
				return;
			}

			node.typeHandle = toType;
			break;
		}
		default: ;
		}
	}

	void TypeInference::Visit(ITrBlockExpr& node)
	{
	}

	void TypeInference::Visit(ITrUnsafeExpr& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		Walk(node);

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

		Walk(node);
		
		node.typeHandle = node.expr->typeHandle;
	}

	void TypeInference::Visit(ITrIs& node)
	{
		if (node.typeHandle != TypeHandle(-1))
			return;

		Walk(node);

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

		Walk(node);

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
			QualNameSPtr scope = m_Scope; 
			for (StdString& scopeName : m_ScopeNames)
			{
				scope = QualName::Create(m_Scope, Iden::Create(scopeName));
			}

			QualNameSPtr qualName = type->AsIden().qualName;
			if (qualName->Iden()->Name() == "Self" &&
				m_SelfType != TypeHandle(-1))
			{
				type = m_pCtx->typeReg.GetType(m_SelfType);
				qualName = type->AsIden().qualName;
			}

			SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(scope, qualName, m_InterfaceQualname);
			node.handle = sym->SelfType();

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
		case TypeKind::Compound:
		{
			StdVector<TypeHandle> subTypes;
			for (ITrTypeSPtr subType : node.subTypes)
			{
				subTypes.push_back(subType->handle);
			}

			node.handle = m_pCtx->typeReg.Compound(type->mod, subTypes);
			break;
		}
		default: ;
		}
		
	}

	void TypeInference::HandleGenerics(ITrGenDeclSPtr decl, IdenSPtr iden)
	{
		StdVector<IdenGeneric>& generics = iden->Generics();
		
		for (ITrGenTypeBoundSPtr bound : decl->bounds)
		{
 			Visit(*bound->bound);

            for (IdenGeneric& generic : generics)
            {
	            if (generic.isType && !generic.isSpecialized &&
					generic.iden == bound->type)
	            {
					generic.typeConstraints.push_back(bound->bound->handle);
	            	break;
	            }
            }
		}
	}

	QualNameSPtr TypeInference::GetCurScope()
	{
		if (m_ScopeNames.empty())
			return m_Scope;

		QualNameSPtr qualName = m_Scope;
		for (StdString& scopeName : m_ScopeNames)
		{
			qualName = QualName::Create(qualName, scopeName);
		}
		return qualName;
	}
}
