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

		ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

		if (m_Prepass)
		{
			Foreach(ITrVisitorDefKind::Any, [&](ITrImpl& node)
			{
				m_Scope = node.qualName;
				m_Impl = node.ptr.lock();
				
				if (node.genDecl)
					HandleGenerics(node, node.qualName->Iden());
				Visit(*node.type);

				SymbolSPtr implSym = node.sym.lock();
				node.sym.lock()->type = node.type->handle;
				
				if (node.interface.first)
				{
					QualNameSPtr qualName = node.interface.first;
					SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(node.qualName, qualName);
					sym = sym->baseVariant.lock();

					IdenSPtr iden = qualName->Iden();
					IdenSPtr baseIden = sym->qualName->Iden();

					usize size = iden->Generics().size();
					for (usize i = 0; i < size; ++i)
					{
						IdenGeneric& baseGen = baseIden->Generics()[i];
						if (!baseGen.isType)
							continue;
						
						IdenGeneric& gen = iden->Generics()[i];

						node.genMapping.try_emplace(baseGen.iden, gen.type);
					}
				}
			});
			
			Foreach(ITrVisitorDefKind::Any, [&](ITrStruct& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrUnion& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrAdtEnum& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrStrongInterface& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node, node.qualName->Iden());
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrWeakInterface& node)
			{
				m_Scope = node.qualName;
				if (node.genDecl)
					HandleGenerics(node, node.qualName->Iden());
			});
		}

		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_Scope = node.qualName;
			m_FuncCtx = node.ctx;
			m_SelfType = node.selfType;
			m_Impl = node.impl;

			if (node.funcKind == ITrFuncKind::Method)
				m_DebugMethodName = node.qualName->Iden()->Name();
			
			if (m_Prepass)
			{
				if (m_DebugMethodName == "opBoolConv")
					int br = 0;
				
				HandleGenerics(node, node.qualName->Iden());
				if (!node.genDecl && node.impl)
					node.genMapping = node.impl->genMapping;
			}
			m_GenMapping = node.genMapping;
			
			m_InterfaceQualname = nullptr;
			m_SubInterfaceQualNames.clear();

			SymbolSPtr sym = node.sym.lock();
			SymbolSPtr parent = node.sym.lock()->parent.lock();
			if (parent)
			{
				if (parent->kind == SymbolKind::StrongInterface)
				{
					for (StdPair<QualNameSPtr, SymbolWPtr>& pair : parent->interfaces)
					{
						m_SubInterfaceQualNames.push_back(pair.second.lock()->qualName);
					}
				}
				else if (parent->kind != SymbolKind::WeakInterface &&
					!sym->interfaces.empty())
				{
					SymbolSPtr tmp = sym->interfaces[0].second.lock();
					m_InterfaceQualname = tmp->qualName;

					for (StdPair<QualNameSPtr, SymbolWPtr>& pair : tmp->interfaces)
					{
						m_SubInterfaceQualNames.push_back(pair.second.lock()->qualName);
					}
				}
			}

			if (m_Prepass)
			{	
				StdVector<TypeHandle> paramTypes;
				paramTypes.reserve(node.params.size());
				for (ITrParamSPtr param : node.params)
				{
					Visit(*param->type);
					paramTypes.push_back(param->type->handle);
				}

				TypeHandle retType;
				if (node.retType)
				{
					Visit(*node.retType);
					retType = node.retType->handle;
				}

				TypeHandle type = m_pCtx->typeReg.Func(TypeMod::None, paramTypes, retType);
				sym->type = type;
			}
			else
			{
				ITrBodySPtr body = mod.GetBody(node);
				if (!body)
					return;

				m_ReturnHandle = node.sym.lock()->type->AsFunc().retType;
				Expect(nullptr);

				for (ITrParamSPtr param : node.params)
				{
					LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, param->iden);
				}

				for (ITrStmtSPtr& stmt : body->stmts)
				{
					ITrVisitor::Visit(stmt);
				}
			}

			m_Impl = nullptr;
			m_InterfaceQualname = nullptr;
		});
	}

	void TypeInference::Visit(ITrBlock& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void TypeInference::Visit(ITrReturn& node)
	{
		Expect(m_ReturnHandle);
		Walk(node);
	}

	void TypeInference::Visit(ITrLocalVar& node)
	{
		Walk(node);

		usize idenCount = node.idens.size();
		StdVector<TypeInfo> types;
		types.reserve(idenCount);
		
		if (node.init)
		{
			if (idenCount == 1)
			{
				TypeSPtr type = node.init->typeInfo.handle->type;
				if (type->typeKind == TypeKind::Func)
				{
					// We don't need generic info for function returns
					types.push_back(TypeInfo{ type->AsFunc().retType });
				}
				else
				{
					types.push_back(node.init->typeInfo);
				}
			}
			else
			{
				TypeSPtr type = node.init->typeInfo.handle->type;
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

			TypeHandle type = m_pCtx->typeReg.Mod(TypeMod::None, types[i].handle);

			if (node.attribs)
			{
				if (ENUM_IS_SET(node.attribs->attribs, Attribute::Mut))
				{
					type = m_pCtx->typeReg.Mod(TypeMod::Mut, type);
				}	
			}
			
			localVar->typeInfo = TypeInfo{ type, types[i].genInfo };
		}
	}

	void TypeInference::Visit(ITrAssign& node)
	{
		if (node.typeInfo.handle)
			return;
		
		Walk(node);

		TypeHandle lTypeHandle = node.lExpr->typeInfo.handle;
		TypeHandle rTypeHandle = node.rExpr->typeInfo.handle;
		
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
			const Operator& op = m_pCtx->activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle);
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

		node.typeInfo = node.lExpr->typeInfo;
	}

	void TypeInference::Visit(ITrTernary& node)
	{
		if (node.typeInfo.handle)
			return;
		
		Walk(node);

		TypeHandle condTypeHandle = node.cond->typeInfo.handle;
		condTypeHandle = m_pCtx->typeReg.Mod(TypeMod::None, condTypeHandle);
		TypeSPtr condType = condTypeHandle->type;
		if (condType->typeKind != TypeKind::Builtin ||
			condType->AsBuiltin().builtin != BuiltinTypeKind::Bool)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.cond->astNode)->ctx->startIdx);
			g_ErrorSystem.Error(span, "Condition should be of type 'bool'\n");
		}

		TypeHandle tTypeHandle = node.tExpr->typeInfo.handle;
		TypeHandle fTypeHandle = node.fExpr->typeInfo.handle;
		if (!m_pCtx->typeReg.AreTypesEqual(tTypeHandle, fTypeHandle))
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			g_ErrorSystem.Error(span, "Both sides need to be of the same type\n");
		}

		node.typeInfo = node.tExpr->typeInfo;
	}

	void TypeInference::Visit(ITrBinary& node)
	{
		if (node.typeInfo.handle)
			return;

		Walk(node);

		TypeHandle lTypeHandle = node.lExpr->typeInfo.handle;
		TypeHandle rTypeHandle = node.rExpr->typeInfo.handle;

		const Operator& op = m_pCtx->activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle);
		node.typeInfo.handle = op.result;
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
		if (node.typeInfo.handle)
			return;
		
		Walk(node);

		if (m_DebugMethodName == "opBoolConv" && node.op == OperatorKind::Not)
			int br = 0;

		TypeHandle exprTypeHandle = node.expr->typeInfo.handle;

		OperatorKind opKind = node.op;
		if (opKind == OperatorKind::Deref && m_Expected)
		{
			if (m_Expected->typeKind == TypeKind::Ref)
			{
				TypeHandle subHandle = m_Expected->AsRef().subType;
				if (subHandle->type->mod == TypeMod::Mut)
					opKind = OperatorKind::MutDeref;
			}
			else if (m_Expected->mod == TypeMod::Mut)
			{
				opKind = OperatorKind::MutDeref;
			}
		}

		const Operator& op = m_pCtx->activeModule->opTable.GetOperator(opKind, exprTypeHandle);
		node.typeInfo.handle = op.result;
		node.operator_ = op;

		if (!op.left && !node.expr->typeInfo.genInfo.bounds.empty())
		{
			for (TypeHandle boundType : node.expr->typeInfo.genInfo.bounds)
			{
				const Operator& tmpOp = m_pCtx->activeModule->opTable.GetOperator(opKind, boundType);
				if (tmpOp.left)
				{
					node.typeInfo.handle = tmpOp.result;
					node.operator_ = tmpOp;
					break;
				}
			}
		}

		if (!node.operator_.left)
		{
			Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
			StdStringView opName = GetOpName(node.op);
			StdString typeName = m_pCtx->typeReg.ToString(exprTypeHandle);
			g_ErrorSystem.Error(span, "Unary operator '%s' not found for '%s'\n", opName.data(), typeName.c_str());
		}

		if (node.operator_.isInterfaceOp)
		{
			TypeHandle srcType = exprTypeHandle;
			if (srcType->type->typeKind == TypeKind::Ref)
				srcType = srcType->AsRef().subType;

			if (srcType->type->typeKind == TypeKind::Generic)
			{
				// unary ops return T::ResultT
				const StdString& name = srcType->AsGeneric().iden->Name();
				auto it = m_FuncCtx->genAssocs.find(name);
				if (it != m_FuncCtx->genAssocs.end())
				{
					auto subIt = it->second.subInfo.find(node.operator_.left->type);
					if (subIt != it->second.subInfo.end())
					{
						auto resTIt = subIt->second.find("ResultT");
						if (resTIt != subIt->second.end())
						{
							node.typeInfo.genInfo = resTIt->second;
						}
					}
				}
			}
			else
			{
				GenTypeInfo& exprGenInfo = node.expr->typeInfo.genInfo;
				if (!exprGenInfo.subInfo.empty())
				{
					auto it = exprGenInfo.subInfo.find(node.operator_.left->type);
					if (it != exprGenInfo.subInfo.end())
					{
						auto resTIt = it->second.find("ResultT");
						if (resTIt != it->second.end())
						{
							node.typeInfo.genInfo = resTIt->second;
						}
					}
					
				}
			}
		}

		NarrowGenBound(node.typeInfo);
	}

	void TypeInference::Visit(ITrQualNameExpr& node)
	{
		QualNameSPtr qualName = node.qualName;

		if (m_FuncCtx)
		{
			if (qualName->IsBase())
			{
				LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, qualName->Iden());
				if (local)
				{
					node.typeInfo = local->typeInfo;
					return;
				}
			}
		}

		if (qualName->Disambiguation())
		{
			// TODO

			return;
		}

		SymbolSPtr sym;
		if (m_Impl)
		{
			sym = m_Impl->sym.lock()->children->FindChild(nullptr, qualName->Iden());
		}

		if (!sym)
			sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), qualName);
		if (!sym)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString varName = qualName->ToString();
			g_ErrorSystem.Error(span, "Cannot find '%s'", varName.c_str());
			return;
		}
		
		node.sym = sym;
		node.typeInfo.handle = node.sym->SelfType();
	}

	void TypeInference::Visit(ITrIndexSlice& node)
	{
		Walk(node);

		TypeHandle exprTypeHandle = node.expr->typeInfo.handle;
		
		if (node.to)
		{
			
		}
		else if (node.explicitSlice)
		{
			
		}
		else
		{
			OperatorKind opKind = node.expr->typeInfo.handle->type->mod == TypeMod::Mut ? OperatorKind::MutIndex : OperatorKind::Index;
			Operator op = m_pCtx->activeModule->opTable.GetOperator(opKind, exprTypeHandle, node.index->typeInfo.handle);

			if (opKind == OperatorKind::MutIndex && !op.sym)
			{
				op = m_pCtx->activeModule->opTable.GetOperator(opKind, exprTypeHandle, node.index->typeInfo.handle);
			}
			
			node.typeInfo.handle = op.result;
			node.operator_ = op;

			if (!op.sym)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdStringView opName = GetOpName(opKind);
				StdString typeName = m_pCtx->typeReg.ToString(exprTypeHandle);
				g_ErrorSystem.Error(span, "Index operator '%s' not found for '%s'\n", opName.data(), typeName.c_str());
			}
		}
	}

	void TypeInference::Visit(ITrExprSPtr& ptr, ITrAmbiguousCall node)
	{
		Walk(node);

		if (node.expr->sym)
		{
			SymbolSPtr sym = node.expr->sym;
			if (sym->kind == SymbolKind::AdtEnumMember)
			{
				auto astNode = node.astNode;
				ptr.reset(new ITrAdtTupleEnumInit{ node.expr, std::move(node.args) });
				ptr->sym = sym;
				ptr->typeInfo.handle = sym->parent.lock()->SelfType();
				ptr->astNode = astNode;
			}
			else
			{
				auto astNode = node.astNode;
				ptr.reset(new ITrFuncCall{ node.expr, std::move(node.args) });
				ptr->sym = sym;
				ptr->typeInfo.handle = sym->SelfType();
				ptr->astNode = astNode;
			}
		}
		else
		{
			// TODO
		}
	}

	// TODO: named args
	void TypeInference::Visit(ITrFuncCall& node)
	{
		// Should only have method calls

		Walk(node);

		StdVector<StdString> paramNames;
		for (ITrArgSPtr arg : node.args)
		{
			if (arg->iden)
				paramNames.push_back(arg->iden->Name());
			else
				paramNames.push_back("");
		}
		
		
		if (node.isMethod)
		{
			paramNames.insert(paramNames.begin(), "self");
			IdenSPtr searchIden = Iden::Create(node.iden->Name(), node.iden->Generics(), paramNames);
			
			TypeSPtr type = node.callerOrFunc->typeInfo.handle->type;

			
			SymbolSPtr callerSym, methodSym;
			if (type->typeKind == TypeKind::Ref)
			{
				callerSym = m_pCtx->activeModule->symTable.Find(type);
				if (callerSym)
					methodSym = callerSym->children->FindChild(nullptr, searchIden);

				if (!methodSym && type->mod == TypeMod::Mut)
				{
					TypeHandle nonMutHandle = m_pCtx->typeReg.Mod(TypeMod::None, node.callerOrFunc->typeInfo.handle);
					TypeSPtr nonMutType = nonMutHandle->type;
					callerSym = m_pCtx->activeModule->symTable.Find(type);
					if (callerSym)
						methodSym = callerSym->children->FindChild(nullptr, searchIden);
				}

				if (!methodSym)
				{
					type = type->AsRef().subType->type;
					if (type->typeKind == TypeKind::Iden)
					{
						callerSym = m_pCtx->activeModule->symTable.Find(GetCurScope(), type->AsIden().qualName);
					}
					else
					{
						callerSym = m_pCtx->activeModule->symTable.Find(type);
					}

					methodSym = callerSym->children->FindChild(nullptr, searchIden);
					if (!methodSym && callerSym->kind == SymbolKind::StrongInterface)
					{
						for (StdPair<QualNameSPtr, SymbolWPtr> pair : callerSym->interfaces)
						{
							methodSym = callerSym->children->FindChild(pair.first, searchIden);
							if (methodSym)
								break;
						}
					}
				}
			}
			else
			{
				if (type->typeKind == TypeKind::Iden)
					callerSym = m_pCtx->activeModule->symTable.Find(GetCurScope(), type->AsIden().qualName);
				else
					callerSym = m_pCtx->activeModule->symTable.Find(type);
				
				if (callerSym)
					methodSym = callerSym->children->FindChild(nullptr, searchIden);

				if (!methodSym)
				{
					if (type->mod == TypeMod::Mut)
					{
						TypeHandle constHandle = node.callerOrFunc->typeInfo.handle;
						TypeSPtr constType = constHandle->type;
						callerSym = m_pCtx->activeModule->symTable.Find(type);
						if (callerSym)
							methodSym = callerSym->children->FindChild(nullptr, searchIden);
					}

					if (!methodSym)
					{
						TypeHandle refHandle = m_pCtx->typeReg.Ref(TypeMod::None, node.callerOrFunc->typeInfo.handle);
						TypeSPtr refType = refHandle->type;
						callerSym = m_pCtx->activeModule->symTable.Find(refType);
						if (callerSym)
							methodSym = callerSym->children->FindChild(nullptr, searchIden);
					}

					
				}
			}

			if (!methodSym && type->typeKind == TypeKind::Ptr)
			{
				// TODO: Deref
			}

			if (!methodSym)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString methodName = node.iden->Name();
				StdString callerTypeName = m_pCtx->typeReg.ToString(type);
				g_ErrorSystem.Error(span, "Cannot find method '%s' with caller '%s'\n", methodName.c_str(), callerTypeName.c_str());
			}



			node.sym = methodSym;
			FuncType& funcType = methodSym->type->AsFunc();
			node.typeInfo.handle = funcType.retType;
		}
		
	}

	void TypeInference::Visit(ITrAdtTupleEnumInit& node)
	{
	}

	void TypeInference::Visit(ITrMemberAccess& node)
	{
		Walk(node);

		TypeSPtr type = node.expr->typeInfo.handle->type;
		if (type->typeKind == TypeKind::Ref)
		{
			type = type->AsRef().subType->type;
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

		node.typeInfo.handle = child->type;
	}

	void TypeInference::Visit(ITrTupleAccess& node)
	{
		Walk(node);
		
		TypeSPtr type = node.expr->typeInfo.handle->type;
		if (type->typeKind == TypeKind::Ref)
		{
			type = type->AsRef().subType->type;
		}
		else if (type->typeKind == TypeKind::Ptr)
		{
			// TODO
		}

		if (type->typeKind != TypeKind::Tuple)
		{
			MultiSpan span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx, std::get<AstExprSPtr>(node.astNode)->ctx->endIdx);
			StdString typeName = m_pCtx->typeReg.ToString(node.expr->typeInfo.handle);
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

		node.typeInfo.handle = tupType.subTypes[node.index];
	}

	void TypeInference::Visit(ITrLiteral& node)
	{
		TypeRegistry& typeReg = m_pCtx->typeReg;
		
		switch (node.lit.Type())
		{
		case TokenType::True:
		case TokenType::False:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);
			break;
		}
		case TokenType::CharLit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char);
			break;
		}
		case TokenType::F16Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F16);
			break;
		}
		case TokenType::F32Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F32);
			break;
		}
		case TokenType::F64Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F64);
			break;
		}
		case TokenType::F128Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F128);
			break;
		}
		case TokenType::I8Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I8);
			break;
		}
		case TokenType::I16Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I16);
			break;
		}
		case TokenType::I32Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I32);
			break;
		}
		case TokenType::I64Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I64);
			break;
		}
		case TokenType::I128Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I128);
			break;
		}
		case TokenType::StringLit:
		{
			TypeHandle charType = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char);
			node.typeInfo.handle = typeReg.Slice(TypeMod::None, charType);
			break;
		}
		case TokenType::U8Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U8);
			break;
		}
		case TokenType::U16Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16);
			break;
		}
		case TokenType::U32Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U32);
			break;
		}
		case TokenType::U64Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U64);
			break;
		}
		case TokenType::U128Lit:
		{
			node.typeInfo.handle = typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U128);
			break;
		}
		default: ;
		}
	}

	void TypeInference::Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node)
	{
		Walk(node);

		TypeSPtr objType = node.type->handle->type;
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

			TypeSPtr type = sym->type->type;
			if (type->typeKind == TypeKind::Tuple)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString typeName = m_pCtx->typeReg.ToString(objType);
				g_ErrorSystem.Error(span, "'%s' needs to be initialized with parenthesis\n", node.args.size());
				return;
			}

			auto astNode = ptr->astNode;
			ptr.reset(new ITrAdtAggrEnumInit{ node.type, std::move(node.args) });
			ptr->typeInfo.handle = sym->parent.lock()->SelfType();
			ptr->sym = sym;
			ptr->astNode = astNode;
			checkAggrArgs = m_pCtx->typeReg.IsType(sym->type, TypeKind::Iden);

			ITrAdtAggrEnumInit& adtNode = *reinterpret_cast<ITrAdtAggrEnumInit*>(ptr.get());

			IdenType& idenType = type->AsIden();
			SymbolSPtr structSym = idenType.sym.lock();

			StdVector<SymbolSPtr> children;
			children.reserve(structSym->orderedVarChildren.size());
			StdUnorderedMap<StdString, u32> childrenNameMapping;

			for (SymbolWPtr childW : structSym->orderedVarChildren)
			{
				SymbolSPtr child = childW.lock();

				childrenNameMapping.try_emplace(child->qualName->Iden()->Name(), u32(children.size()));
				children.push_back(child);
			}

			bool hasIden = false;

			StdVector<TypeHandle> argTypes;
			argTypes.resize(children.size(), nullptr);

			if (adtNode.args.size() > children.size())
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Found more arguments then expected, found %u, expected %u\n", adtNode.args.size(), children.size());
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
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an adt enum\n");
						return;
					}

					auto it = childrenNameMapping.find(arg->iden->Name());
					if (it == childrenNameMapping.end())
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "The adt enum does not contain any variable named '%s'\n", name.c_str());
						return;
					}

					u32 idx = it->second;
					if (argTypes[idx])
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "Variable '%s' has already been assigned\n", name.c_str());
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeInfo.handle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
					argTypes[idx] = arg->expr->typeInfo.handle;

					adtNode.argOrder.push_back(idx);
				}
				else
				{
					if (hasIden && i != 0)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(adtNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an adt enum\n");
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeInfo.handle;
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
					argCount += u32(argType != nullptr);
				}
			}
			else
			{
				argCount = u32(adtNode.args.size());
			}

			if (argCount < children.size())
			{
				if (node.hasDefInit)
				{
					QualNameSPtr defInterfaceQualName = QualName::Create(StdVector<StdString>{ "core", "default", "Default" });

					bool implsDefault = false;
					for (SymbolSPtr implSym : sym->impls)
					{
						if (implSym->qualName == defInterfaceQualName)
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
					TypeHandle defType = node.defExpr->typeInfo.handle;
					defType = m_pCtx->typeReg.Mod(TypeMod::None, defType);

					bool validDef = false;
					if (!m_pCtx->typeReg.AreTypesEqual(defType, sym->type))
					{
						TypeSPtr type = defType->type;
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
		else if (sym->kind == SymbolKind::Struct)
		{
			auto astNode = ptr->astNode;
			ptr.reset(new ITrStructInit{ node.type, std::move(node.args), false, nullptr });
			ptr->typeInfo.handle = sym->SelfType();
			ptr->sym = sym;
			ptr->astNode = astNode;
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
			argTypes.resize(children.size(), nullptr);

			if (structNode.args.size() > children.size())
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
				g_ErrorSystem.Error(span, "Found more arguments then expected, found %u, expected %u\n", structNode.args.size(), children.size());
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
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing a structure\n");
						return;
					}

					auto it = childrenNameMapping.find(arg->iden->Name());
					if (it == childrenNameMapping.end())
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "The structure does not contain any variable named '%s'\n", name.c_str());
						return;
					}

					u32 idx = it->second;
					if (argTypes[idx])
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						const StdString& name = arg->iden->Name();
						g_ErrorSystem.Error(span, "Variable '%s' has already been assigned\n", name.c_str());
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeInfo.handle;
					if (!m_pCtx->typeReg.CanPassTo(expected, argType))
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(arg->expr->astNode)->ctx->startIdx);
						StdString expectedName = m_pCtx->typeReg.ToString(expected);
						StdString argName = m_pCtx->typeReg.ToString(argType);
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
					argTypes[idx] = arg->expr->typeInfo.handle;

					structNode.argOrder.push_back(idx);
				}
				else
				{
					if (hasIden && i != 0)
					{
						Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(structNode.astNode)->ctx->startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing a structure\n");
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->typeInfo.handle;
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
					argCount += u32(argType != nullptr);
				}
			}
			else
			{
				argCount = u32(structNode.args.size());
			}

			if (argCount < children.size())
			{
				if (node.hasDefInit)
				{
					QualNameSPtr defInterfaceQualName = QualName::Create(StdVector<StdString>{ "core", "default", "Default" });

					bool implsDefault = false;
					for (SymbolSPtr implSym : sym->impls)
					{
						if (implSym->qualName == defInterfaceQualName)
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
					TypeHandle defType = node.defExpr->typeInfo.handle;
					defType = m_pCtx->typeReg.Mod(TypeMod::None, defType);

					bool validDef = false;
					if (!m_pCtx->typeReg.AreTypesEqual(defType, sym->type))
					{
						TypeSPtr type = defType->type;
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
		else if (sym->kind == SymbolKind::Union)
		{
			auto astNode = ptr->astNode;
			ptr.reset(new ITrUnionInit{ node.type, std::move(node.args) });
			ptr->typeInfo.handle = sym->SelfType();
			ptr->sym = sym;
			ptr->astNode = astNode;

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
					TypeHandle argType = arg->expr->typeInfo.handle;
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
					g_ErrorSystem.Error(span, "No member with the name '%s' exists\n", name.c_str());
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
			g_ErrorSystem.Error(span, "'%s' is not an aggregate or a adt-enum\n", typeName.c_str());
			return;
		}
	}

	void TypeInference::Visit(ITrTupleInit& node)
	{
		Walk(node);

		StdVector<TypeHandle> subTypes;
		for (ITrExprSPtr expr : node.exprs)
		{
			subTypes.push_back(expr->typeInfo.handle);
		}

		node.typeInfo.handle = m_pCtx->typeReg.Tuple(TypeMod::None, subTypes);
	}

	void TypeInference::Visit(ITrArrayInit& node)
	{
		Walk(node);
		
		TypeHandle type;
		for (ITrExprSPtr expr : node.exprs)
		{
			if (!type)
			{
				type = expr->typeInfo.handle;
			}
			else if (!m_pCtx->typeReg.AreTypesEqual(expr->typeInfo.handle, type))
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString type0Name = m_pCtx->typeReg.ToString(type);
				StdString type1Name = m_pCtx->typeReg.ToString(expr->typeInfo.handle);
				g_ErrorSystem.Error(span, "An array connot contain values of different types, found '%s' and '%s'", type0Name.c_str(), type1Name.c_str());
			}
		}

		node.typeInfo.handle = type;
	}

	void TypeInference::Visit(ITrCast& node)
	{
		if (node.typeInfo.handle)
			return;

		Walk(node);

		TypeHandle fromType = node.expr->typeInfo.handle;
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

			node.typeInfo.handle = op.result;
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
			u64 fromSize = fromType->type->size;
			u64 toSize = toType->type->size;

			if (fromSize != toSize)
			{
				Span span = m_pCtx->spanManager.GetSpan(std::get<AstExprSPtr>(node.astNode)->ctx->startIdx);
				StdString lTypeName = m_pCtx->typeReg.ToString(fromType);
				StdString rTypeName = m_pCtx->typeReg.ToString(toType);
				g_ErrorSystem.Error(span, "cannot transmute %s -> %s, since they are different sizes\n", lTypeName.c_str(), rTypeName.c_str());
				return;
			}

			node.typeInfo.handle = toType;
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
		if (node.typeInfo.handle)
			return;

		Walk(node);

		node.typeInfo.handle = node.expr->typeInfo.handle;
	}

	void TypeInference::Visit(ITrComma& node)
	{
	}

	void TypeInference::Visit(ITrClosure& node)
	{
	}

	void TypeInference::Visit(ITrMove& node)
	{
		if (node.typeInfo.handle)
			return;

		Walk(node);
		
		node.typeInfo.handle = node.expr->typeInfo.handle;
	}

	void TypeInference::Visit(ITrIs& node)
	{
		if (node.typeInfo.handle)
			return;

		Walk(node);

		node.typeInfo.handle = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);
	}

	void TypeInference::Visit(ITrTry& node)
	{
	}

	void TypeInference::Visit(ITrSpecKw& node)
	{
		if (node.typeInfo.handle)
			return;

		switch (node.kw)
		{
		case TokenType::SLine:
		{
			node.typeInfo.handle = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize);
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
			TypeHandle tmp = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char);
			node.typeInfo.handle = m_pCtx->typeReg.Slice(TypeMod::None, tmp);
			break;
		}
		default:;
		}
	}

	void TypeInference::Visit(ITrCompRun& node)
	{
		if (node.typeInfo.handle)
			return;

		Walk(node);

		node.typeInfo.handle = node.expr->typeInfo.handle;
	}

	void TypeInference::Visit(ITrType& node)
	{
		for (ITrTypeSPtr subType : node.subTypes)
		{
			Visit(*subType);
		}
		if (node.expr)
			ITrVisitor::Visit(node.expr);
		
		TypeSPtr type = node.handle->type;

		ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

		if (node.subTypes.empty())
		{
			StdVector<TypeHandle> tmp = m_pCtx->typeReg.GetSubTypes(node.handle, TypeKind::Iden);

			TypeHandle newType;
			for (TypeHandle handle : tmp)
			{
				QualNameSPtr qualName = handle->AsIden().qualName;
				if (qualName->Iden()->Name() == "Self" &&
					m_SelfType)
				{
					qualName = m_SelfType->AsIden().qualName;
				}

				{
					SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), qualName, m_InterfaceQualname);
					if (sym)
						newType = sym->SelfType();

					// If we have an impl, we need to make sure that if a matching type appears both outside the impl and as a generic, that the generic is preferred
					// this can only happen if the name of the type we are looking for has no base scope
					if (m_Impl && m_Impl->genDecl && qualName->IsBase())
					{
						for (ITrGenParamSPtr param : m_Impl->genDecl->params)
						{
							if (!param->isVar)
							{
								SymbolSPtr paramSym = param->sym.lock();
								IdenSPtr paramIden = paramSym->qualName->Iden();
								if (paramIden == qualName->Iden())
								{
									if (!sym || !sym->qualName->IsSubnameOf(m_Scope))
									{
										for (StdPair<IdenSPtr, TypeHandle> mapping : m_GenMapping)
										{
											if (mapping.first == paramIden)
											{
												newType = mapping.second;
												break;
											}
										}

										if (!newType)
											newType = paramSym->SelfType();
									}
									break;
								}
							}
						}
					}
				}

				// If no type is found here, check for types in parent interfaces of the current interface

				if (!newType)
				{
					StdVector<IdenSPtr> idens = qualName->AllIdens();
					for (QualNameSPtr interfaceQualName : m_SubInterfaceQualNames)
					{
						QualNameSPtr findQualName = QualName::Create(interfaceQualName, idens);
						SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), findQualName, nullptr);
						if (sym)
						{
							newType = sym->SelfType();
							break;
						}
					}
				}

				if (!newType)
				{
					SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), qualName);
					if (sym)
					{
						newType = sym->SelfType();
						break;
					}
				}

				if (!newType)
				{
					SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(m_pCtx->activeModule->qualName, qualName);
					if (sym)
					{
						newType = sym->SelfType();
						break;
					}
				}

				if (!newType)
				{
					for (StdPair<QualNameSPtr, ModuleSPtr> pair : m_pCtx->activeModule->imports)
					{
						SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(pair.second->qualName, qualName);
						if (sym)
						{
							newType = sym->SelfType();
							break;
						}
					}
				}

				TypeHandle replacement = m_pCtx->typeReg.Mod(type->mod, newType);
				node.handle = m_pCtx->typeReg.ReplaceSubType(node.handle, handle, replacement);
			}

			tmp = m_pCtx->typeReg.GetSubTypes(node.handle, TypeKind::Generic);

			for (TypeHandle handle : tmp)
			{
				if (!m_GenMapping.empty())
				{
					GenericType& origGenType = handle->AsGeneric();
					
					for (StdPair<IdenSPtr, TypeHandle> mapping : m_GenMapping)
					{
						if (mapping.first == origGenType.iden)
						{
							node.handle = m_pCtx->typeReg.ReplaceSubType(node.handle, handle, mapping.second);
							break;
						}
					}
				}
			}
		}
		else
		{
			switch (type->typeKind)
			{
			case TypeKind::Ptr:
			{
				if (node.subTypes.empty())
					break;

				node.handle = m_pCtx->typeReg.Ptr(type->mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Ref:
			{
				if (node.subTypes.empty())
					break;

				node.handle = m_pCtx->typeReg.Ref(type->mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Slice:
			{
				if (node.subTypes.empty())
					break;

				node.handle = m_pCtx->typeReg.Slice(type->mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Array:
			{
				if (node.subTypes.empty())
					break;

				if (type->AsArray().sizeKnown)
					node.handle = m_pCtx->typeReg.Array(type->mod, node.subTypes[0]->handle, type->AsArray().size);
				else
					node.handle = m_pCtx->typeReg.Array(type->mod, node.subTypes[0]->handle, type->AsArray().expr);
				break;
			}
			case TypeKind::Tuple:
			{
				if (node.subTypes.empty() && node.handle)
					break;

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
				if (node.subTypes.empty())
					break;

				node.handle = m_pCtx->typeReg.Opt(type->mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Func:
			{
				if (node.subTypes.empty() && node.handle)
					break;

				if (!node.subTypes.empty())
				{
					// TODO
					//StdVector<TypeHandle> subTypes;
					//for (ITrTypeSPtr subType : node.subTypes)
					//{
					//	subTypes.push_back(subType->handle);
					//}
					//
					//node.handle = m_pCtx->typeReg.Func(type->mod, subTypes);
				}
				break;
			}
			case TypeKind::Compound:
			{
				if (node.subTypes.empty())
					break;

				if (node.subTypes.size() == 1)
				{
					node.handle = node.subTypes[0]->handle;
					break;
				}

				StdVector<TypeHandle> subTypes;
				for (ITrTypeSPtr subType : node.subTypes)
				{
					subTypes.push_back(subType->handle);
				}

				node.handle = m_pCtx->typeReg.Compound(type->mod, subTypes);
				break;
			}
			default:;
			}
		}
	}

	void TypeInference::HandleGenerics(ITrDef& def, IdenSPtr iden)
	{
		StdVector<IdenGeneric>& generics = iden->Generics();

		if (def.genDecl)
		{
			for (ITrGenTypeBoundSPtr bound : def.genDecl->bounds)
			{
				Visit(*bound->bound->type);

				for (IdenGeneric& generic : generics)
				{
					if (generic.isType && !generic.isSpecialized)
					{
						TypeHandle handle = bound->type->handle;

						if (handle->type->typeKind == TypeKind::Generic)
						{
							IdenSPtr iden = handle->AsGeneric().iden;
							if (generic.iden == iden)
								generic.typeConstraints.push_back(bound->bound->type->handle);

							if (m_FuncCtx)
							{
								auto it = m_FuncCtx->genAssocs.try_emplace(iden->Name(), GenTypeInfo{}).first;
								HandleAssocTypes(*bound->bound, it->second);
							}
						}
						else
						{
							IdenType& idenType = handle->AsIden();
							SymbolSPtr sym = m_pCtx->activeModule->symTable.Find(GetCurScope(), idenType.qualName);

							// TODO
						}
						break;
					}
				}
			}

			usize size = def.genDecl->params.size();
			for (usize i = 0; i < size; ++i)
			{
				IdenGeneric& generic = generics[i];
				ITrGenParamSPtr param = def.genDecl->params[i];

				if (generic.isType)
				{
					GenericType& origGeneric = generic.type->AsGeneric();
					generic.type = m_pCtx->typeReg.Generic(TypeMod::None, origGeneric.iden, generic.typeConstraints);

					param->sym.lock()->type = generic.type;
				}

				if (!param->isVar)
				{
					ITrGenTypeParam& typeParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
					def.genMapping.try_emplace(typeParam.iden, param->sym.lock()->type);
				}
			}
		}

		// Handle generic mapping
		if (def.impl && def.impl->genDecl)
		{
			for (ITrGenParamSPtr param : def.impl->genDecl->params)
			{
				if (!param->isVar)
				{
					ITrGenTypeParam& typeParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
					def.genMapping.try_emplace(typeParam.iden, param->sym.lock()->type);
				}
			}

			for (ITrGenTypeBoundSPtr bound : def.impl->genDecl->bounds)
			{
				Visit(*bound->bound->type);

				TypeHandle handle = bound->type->handle;

				if (handle->type->typeKind == TypeKind::Generic)
				{
					IdenSPtr iden = handle->AsGeneric().iden;

					if (m_FuncCtx)
					{
						auto it = m_FuncCtx->genAssocs.try_emplace(iden->Name(), GenTypeInfo{}).first;
						HandleAssocTypes(*bound->bound, it->second);
					}
				}
				else if (handle->type->typeKind == TypeKind::Iden)
				{
					QualNameSPtr qualName = handle->AsIden().qualName;
					IdenSPtr baseIden = qualName->BaseIden();
				}
			}
		}
	}

	void TypeInference::HandleAssocTypes(ITrGenBoundType& boundType, GenTypeInfo& genInfo)
	{
		Visit(*boundType.type);
		TypeHandle type = boundType.type->handle;

		genInfo.bounds.push_back(type);
		
		for (ITrGenAssocBound& assocBound : boundType.assocBounds)
		{	
			GenTypeInfo tmp;
			HandleAssocTypes(*assocBound.type, tmp);

			auto tmpIt = genInfo.subInfo.find(type->type);
			if (tmpIt == genInfo.subInfo.end())
				tmpIt = genInfo.subInfo.try_emplace(type->type, StdUnorderedMap<StdString, GenTypeInfo>{}).first;
			tmpIt->second.try_emplace(assocBound.iden, tmp);
		}
	}

	void TypeInference::NarrowGenBound(TypeInfo& typeInfo)
	{
		if (typeInfo.genInfo.bounds.size() != 1)
			return;

		TypeHandle boundType = typeInfo.genInfo.bounds[0];
		if (boundType->type->typeKind == TypeKind::Iden)
		{
			SymbolSPtr sym = boundType->AsIden().sym.lock();
			if (sym && (sym->kind == SymbolKind::StrongInterface ||
						sym->kind == SymbolKind::WeakInterface))
				return;
		}

		typeInfo.handle = boundType;
		typeInfo.genInfo.subInfo.clear();
		typeInfo.genInfo.bounds.clear();
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

	void TypeInference::Expect(TypeHandle handle)
	{
		m_ExpectedHandle = handle;
		m_Expected = handle ? handle->type : nullptr;
	}
}
