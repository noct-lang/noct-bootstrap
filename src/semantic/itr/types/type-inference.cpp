#include "type-inference.hpp"

#include "ast/ast.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "itr/itr.hpp"
#include "module/function.hpp"
#include "module/module.hpp"
#include <algorithm>

// TODO: Inference needs to work in the opposite direction, e.g. should infer type of return from func, type of the arg from the expected type, etc
namespace Noctis
{

	TypeInference::TypeInference(bool prepass)
		: ITrSemanticPass(prepass ? "type inference prepass" : "type inference")
		, m_Prepass(prepass)
		, m_pBoundsInfo(nullptr)
	{
	}

	void TypeInference::Process(ITrModule& mod)
	{
		SetModule(mod);

		if (m_Prepass)
		{
			Foreach(ITrVisitorDefKind::Any, [&](ITrStruct& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				if (node.genDecl)
					HandleGenerics(node);

				ITrBodySPtr body = m_pMod->GetBody(node);
				for (ITrDefSPtr def : body->defs)
				{
					if (def->kind == ITrDefKind::Var)
					{
						ITrVar& var = static_cast<ITrVar&>(*def);
						Visit(*var.type);
						var.sym.lock()->type = var.type->handle;
						UpdateInstantiations(var.sym.lock());
					}
				}
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrUnion& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				if (node.genDecl)
					HandleGenerics(node);

				ITrBodySPtr body = m_pMod->GetBody(node);
				for (ITrDefSPtr def : body->defs)
				{
					if (def->kind == ITrDefKind::Var)
					{
						ITrVar& var = static_cast<ITrVar&>(*def);
						Visit(*var.type);
						var.sym.lock()->type = var.type->handle;
						UpdateInstantiations(var.sym.lock());
					}
				}
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrAdtEnum& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				if (node.genDecl)
					HandleGenerics(node);
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrStrongInterface& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				if (node.genDecl)
					HandleGenerics(node);
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrWeakInterface& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				if (node.genDecl)
					HandleGenerics(node);
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrTypealias& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				m_Impl = node.impl;
				
				if (node.type)
				{
					Visit(*node.type);
					node.sym.lock()->type = node.type->handle;
					UpdateInstantiations(node.sym.lock());
				}

				m_Impl = nullptr;
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrTypedef& node)
			{
				m_Scope = node.qualName;
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				if (node.type)
				{
					Visit(*node.type);
					node.sym.lock()->type = node.type->handle;
					UpdateInstantiations(node.sym.lock());
				}
			});

			Foreach(ITrVisitorDefKind::Any, [&](ITrImpl& node)
			{
				m_Scope = node.qualName;
				m_Impl = node.ptr.lock();
				m_pBoundsInfo = &node.sym.lock()->boundsInfo;
				m_GenMapping = node.genMapping;
				
				Visit(*node.type);
				node.sym.lock()->type = node.type->handle;
				UpdateInstantiations(node.sym.lock());

				if (node.genDecl)
					HandleGenerics(node);

				// Handle interfaces
				if (node.interface.first)
				{
					SymbolSPtr implIface = g_Ctx.activeModule->symTable.Find(GetCurScope(), node.interface.first);
					QualNameSPtr instQualName = implIface->qualName->Base()->AppendLastIden(node.interface.first);
					SymbolInstSPtr ifaceInst = implIface->GetInst(instQualName);

					StdStack<SymbolInstSPtr> toProcess;
					toProcess.push(ifaceInst);
					while (!toProcess.empty())
					{
						SymbolInstSPtr iface = toProcess.top();
						toProcess.pop();

						iface->type = InferType(iface->type, TypeMod::None);
						QualNameSPtr newQualName = iface->type.AsIden().qualName;
						node.sym.lock()->children->UpdateImplSubTableKey(iface->qualName, newQualName);
						iface->qualName = newQualName;

						for (SymbolInstWPtr subIFace : iface->ifaces)
						{
							toProcess.push(subIFace.lock());
						}
					}
				}
				
			});
		}

		Foreach(ITrVisitorDefKind::Any, [&](ITrFunc& node)
		{
			m_Scope = node.qualName;
			m_FuncCtx = node.ctx;
			m_SelfType = node.selfType;
			m_Impl = node.impl;
			m_pBoundsInfo = &node.sym.lock()->boundsInfo;
			m_GenDecl = node.genDecl;
			m_GenMapping = node.genMapping;

			if (m_Prepass)
				HandleGenerics(node);

			m_InterfaceQualname = nullptr;
			m_SubInterfaceQualNames.clear();

			SymbolSPtr sym = node.sym.lock();
			SymbolSPtr parent = node.sym.lock()->parent.lock();
			if (parent)
			{
				if (parent->kind == SymbolKind::StrongInterface)
				{
					for (SymbolInstWPtr& iface : parent->ifaces)
					{
						m_SubInterfaceQualNames.push_back(iface.lock()->qualName);
					}
				}
				else if (parent->kind != SymbolKind::WeakInterface &&
						 !sym->ifaces.empty())
				{
					SymbolInstSPtr tmp = sym->ifaces[0].lock();
					m_InterfaceQualname = tmp->qualName;

					for (SymbolInstWPtr& inst : tmp->ifaces)
					{
						m_SubInterfaceQualNames.push_back(inst.lock()->qualName);
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

				if (node.errorType)
				{
					Visit(*node.errorType);
					TypeHandle errType = node.errorType->handle;
					
					QualNameSPtr baseQualName = QualName::Create(StdVector<StdString>{ "core", "result" });
					StdVector<IdenGeneric> idenGens;
					idenGens.resize(2, IdenGeneric{});

					idenGens[0].isType = idenGens[0].isSpecialized = true;
					idenGens[0].type = retType;
					idenGens[1].isType = idenGens[1].isSpecialized = true;
					idenGens[1].type = errType;

					QualNameSPtr qualName = baseQualName->Append("Result", idenGens);
					retType = g_TypeReg.Iden(TypeMod::None, qualName);
				}

				TypeHandle type = g_TypeReg.Func(TypeMod::None, paramTypes, retType);
				sym->type = type;

				UpdateInstantiations(node.sym.lock());
			}
			else
			{
				ITrBodySPtr body = mod.GetBody(node);
				if (!body)
					return;

				m_ReturnHandle = node.sym.lock()->type.AsFunc().retType;
				m_ErrType = node.errorType ? node.errorType->handle : TypeHandle{};
				ExpectNone();

				for (ITrParamSPtr param : node.params)
				{
					LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, param->iden);
				}

				for (ITrDefSPtr def : body->defs)
				{
					if (def->kind == ITrDefKind::ErrHandler)
						Visit(static_cast<ITrErrHandler&>(*def));
				}

				for (ITrStmtSPtr& stmt : body->stmts)
				{
					ITrVisitor::Visit(stmt);
				}
			}

			m_FuncCtx->localVars.ResetActiveVars();
			m_GenDecl = nullptr;
			m_Impl = nullptr;
			m_InterfaceQualname = nullptr;
		});
	}

	void TypeInference::Visit(ITrErrHandler& node)
	{
		node.retType = m_ErrType;
		Walk(node);
	}

	void TypeInference::Visit(ITrBlock& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		Walk(node);
		m_ScopeNames.pop_back();
	}

	void TypeInference::Visit(ITrForRange& node)
	{
		m_ScopeNames.push_back(node.scopeName);
		ITrVisitor::Visit(node.range);

		SymbolSPtr typeSym = g_Ctx.activeModule->symTable.Find(node.range->handle);
		
		QualNameSPtr toItQualName = QualName::Create({ "core", "iter", "ToIterator" });
		SymbolSPtr itSym = typeSym->children->FindChild(toItQualName, "Iter");
		SymbolSPtr itTypeSym = g_Ctx.activeModule->symTable.Find(itSym->type);

		QualNameSPtr itQualName = QualName::Create({ "core", "iter", "Iterator" });
		SymbolSPtr itemSym = itTypeSym->children->FindChild(itQualName, "Item");
		TypeHandle itemType = itemSym->type;

		if (node.idens.size() > 1)
		{
			if (itemType.Kind() != TypeKind::Tuple)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "Cannot expand non-tuple Item type to multiple identifiers");
				m_ScopeNames.pop_back();
				return;
			}

			TupleType& tupType = itemType.AsTuple();
			if (tupType.subTypes.size() != node.idens.size())
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "Cannot expand a tuple with %u elements to %u identifier");
				m_ScopeNames.pop_back();
				return;
			}

			for (usize i = 0; i < tupType.subTypes.size(); ++i)
			{
				LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.idens[i]);
				var->type = tupType.subTypes[i];
			}
		}
		else
		{
			LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.idens[0]);
			var->type = itemType;
		}
		m_ScopeNames.pop_back(); 
	}

	void TypeInference::Visit(ITrSwitch& node)
	{
		ITrVisitor::Visit(node.expr);

		TypeHandle expected = node.expr->handle;
		if (expected.Kind() == TypeKind::Ref)
			expected = expected.AsRef().subType;
		
		SaveRestore tmpExpected(m_ExpectedHandle, expected);
		m_ScopeNames.push_back(node.scopeName);
		for (ITrSwitchCase& case_ : node.cases)
		{
			m_ScopeNames.push_back(case_.block->scopeName);
			SaveRestore caseExpected(m_ExpectedHandle);
			ITrVisitor::Visit(case_.pattern);
			if (case_.expr)
				Visit(case_.expr);

			for (ITrStmtSPtr stmt : case_.block->stmts)
			{
				ITrVisitor::Visit(stmt);
			}
			m_ScopeNames.pop_back();
		}
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
		StdVector<TypeHandle> types;
		types.reserve(idenCount);
		
		if (node.init)
		{
			if (idenCount == 1)
			{
				types.push_back(node.init->handle);
			}
			else
			{
				TypeHandle type = node.init->handle;
				if (type.Kind() != TypeKind::Tuple)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Error(span, "Cannot expand non-tuple type to multiple identifiers");
					return;
				}
				types = type.AsTuple().subTypes;
			}
		}

		if (node.type)
		{
			if (types.empty())
			{
				types.assign(idenCount, node.type->handle);
			}
			else
			{
				for (TypeHandle& handle : types)
				{
					if (handle != node.type->handle)
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						g_ErrorSystem.Error(span, "inferred type does not match declared type");
						return;
					}
				}
			}
		}

		for (usize i = 0; i < node.idens.size(); ++i)
		{
			TypeMod mod = TypeMod::None;
			if (node.attribs && ENUM_IS_SET(node.attribs->attribs, Attribute::Mut))
				mod = TypeMod::Mut;
			
			TypeHandle type = g_TypeReg.Mod(mod, types[i]);
			LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.idens[i]);
			localVar->type = type;
		}
	}

	void TypeInference::Visit(ITrThrow& node)
	{
		Walk(node);

		if (!m_ErrorHandle.IsValid())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			g_ErrorSystem.Error(span, "Cannot throw in a function that is not marked as 'throws'");
		}
		else if (node.expr->handle != m_ErrorHandle)
		{
			TypeHandle foundType = node.expr->handle;
			StdVector<SymbolInstWPtr>& ifaces = foundType.AsIden().sym.lock()->ifaces;
			bool found = false;
			for (SymbolInstWPtr& inst : ifaces)
			{
				if (inst.lock()->type == m_ErrorHandle)
				{
					found = true;
					break;
				}
			}
			
			if (!found)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString foundTypeName = foundType.ToString();
				StdString expectedTypeName = m_ErrorHandle.ToString();
				g_ErrorSystem.Error(span, "Trying to throw an error of type '%s', expected '%s'", foundTypeName.c_str(), expectedTypeName.c_str());
			}
		}
	}

	void TypeInference::Visit(ITrExprSPtr& node)
	{
		ITrVisitor::Visit(node);
		const Bounds& bounds = m_pBoundsInfo->GetBounds(node->handle);
		node->handle = bounds.NarrowType(node->handle);
	}

	void TypeInference::Visit(ITrAssign& node)
	{
		if (node.handle.IsValid())
			return;
		
		Walk(node);

		TypeHandle lTypeHandle = node.lExpr->handle;
		TypeHandle rTypeHandle = node.rExpr->handle;

		if (node.op == OperatorKind::Eq)
		{
			TypeHandle baseRTypeHandle = g_TypeReg.Mod(TypeMod::None, rTypeHandle);
			if (lTypeHandle != baseRTypeHandle)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "Condition should be of type 'bool'\n");
			}
		}
		else
		{
			Operator op = g_Ctx.activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle, *m_pBoundsInfo);
			if (!op.result.IsValid())
			{
				// Check if constraints allow the op
				if (m_GenDecl)
					op = g_Ctx.activeModule->opTable.GetConstriantOperator(node.op, lTypeHandle, rTypeHandle, m_GenDecl, *m_pBoundsInfo);
				if (!op.result.IsValid() && m_Impl && m_Impl->genDecl)
					op = g_Ctx.activeModule->opTable.GetConstriantOperator(node.op, lTypeHandle, rTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
			}
			
			if (!op.sym)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdStringView opName = GetOpName(node.op);
				StdString lTypeName = lTypeHandle.ToString();
				StdString rTypeName = rTypeHandle.ToString();
				g_ErrorSystem.Error(span, "Binary operator '%s' not found for '%s' and '%s'\n", opName.data(), lTypeName.c_str(), rTypeName.c_str());
			}

			node.operator_ = op;
		}

		node.handle = node.lExpr->handle;
	}

	void TypeInference::Visit(ITrTernary& node)
	{
		if (node.handle.IsValid())
			return;
		
		Walk(node);

		TypeHandle condTypeHandle = node.cond->handle;
		condTypeHandle = g_TypeReg.Mod(TypeMod::None, condTypeHandle);
		TypeSPtr condType = condTypeHandle.Type();
		if (condType->typeKind != TypeKind::Builtin ||
			condType->AsBuiltin().builtin != BuiltinTypeKind::Bool)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			g_ErrorSystem.Error(span, "Condition should be of type 'bool'\n");
		}

		TypeHandle tTypeHandle = node.tExpr->handle;
		TypeHandle fTypeHandle = node.fExpr->handle;
		if (tTypeHandle != fTypeHandle)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			g_ErrorSystem.Error(span, "Both sides need to be of the same type\n");
		}

		node.handle = node.tExpr->handle;
	}

	void TypeInference::Visit(ITrBinary& node)
	{
		if (node.handle.IsValid())
			return;

		Walk(node);

		TypeHandle lTypeHandle = node.lExpr->handle;
		TypeHandle rTypeHandle = node.rExpr->handle;

		if (node.op == OperatorKind::Range ||
			node.op == OperatorKind::IncRange)
		{
			if (lTypeHandle.Kind() == TypeKind::Ref)
				lTypeHandle = lTypeHandle.AsRef().subType;
			if (rTypeHandle.Kind() == TypeKind::Ref)
				rTypeHandle = rTypeHandle.AsRef().subType;

			if (!lTypeHandle.IsValid())
			{
				IdenGeneric idenGen;
				idenGen.isType = idenGen.isSpecialized = true;
				idenGen.type = rTypeHandle;
				StdString rangeName = node.op == OperatorKind::Range ? "RangeTo" : "RangeToInclusive";
				QualNameSPtr rangeQualName = QualName::Create({ "core", "ops", rangeName }, { idenGen });
				node.handle = g_TypeReg.Iden(TypeMod::None, rangeQualName);
			}
			else if (!rTypeHandle.IsValid())
			{
				IdenGeneric idenGen;
				idenGen.isType = idenGen.isSpecialized = true;
				idenGen.type = lTypeHandle;
				QualNameSPtr rangeQualName = QualName::Create({ "core", "ops", "RangeFrom" }, { idenGen });
			}
			else
			{
				if (lTypeHandle != rTypeHandle)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					StdString lTypeName = lTypeHandle.ToString();
					StdString rTypeName = rTypeHandle.ToString();
					g_ErrorSystem.Error(span, "Types of both sides of a range operator need to match, found '%s' and '%s'\n", lTypeName.c_str(), rTypeName.c_str());
				}

				IdenGeneric idenGen;
				idenGen.isType = idenGen.isSpecialized = true;
				idenGen.type = lTypeHandle;
				StdString rangeName = node.op == OperatorKind::Range ? "Range" : "RangeInclusive";
				QualNameSPtr rangeQualName = QualName::Create({ "core", "ops", rangeName }, { idenGen });
				node.handle = g_TypeReg.Iden(TypeMod::None, rangeQualName);
			}
			
			return;
		}

		Operator op = g_Ctx.activeModule->opTable.GetOperator(node.op, lTypeHandle, rTypeHandle, *m_pBoundsInfo);
		if (!op.result.IsValid())
		{
			// Check if constraints allow the op
			if (m_GenDecl)
				op = g_Ctx.activeModule->opTable.GetConstriantOperator(node.op, lTypeHandle, rTypeHandle, m_GenDecl, *m_pBoundsInfo);
			if (!op.result.IsValid() && m_Impl && m_Impl->genDecl)
				op = g_Ctx.activeModule->opTable.GetConstriantOperator(node.op, lTypeHandle, rTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
		}

		const Bounds& bounds = m_pBoundsInfo->GetBounds(op.result);
		op.result = bounds.NarrowType(op.result);
		
		node.handle = op.result;
		if (op.isInterfaceOp)
		{
			if (lTypeHandle.Kind() == TypeKind::Ref)
				lTypeHandle = lTypeHandle.AsRef().subType;
			if (op.left != lTypeHandle)
				node.handle = g_TypeReg.ReplaceSubType(node.handle, op.left, lTypeHandle);
		}
		
		node.operator_ = op;

		if (!op.sym)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdStringView opName = GetOpName(node.op);
			StdString lTypeName = lTypeHandle.ToString();
			StdString rTypeName = rTypeHandle.ToString();
			g_ErrorSystem.Error(span, "Binary operator '%s' not found for '%s' and '%s'\n", opName.data(), lTypeName.c_str(), rTypeName.c_str());
		}
	}

	void TypeInference::Visit(ITrUnary& node)
	{
		if (node.handle.IsValid())
			return;
		
		Walk(node);

		TypeHandle exprTypeHandle = node.expr->handle;

		OperatorKind opKind = node.op;
		if (opKind == OperatorKind::Deref && m_ExpectedHandle.IsValid())
		{
			if (m_ExpectedHandle.Kind() == TypeKind::Ref)
			{
				TypeHandle subHandle = m_ExpectedHandle.AsRef().subType;
				if (subHandle.Mod() == TypeMod::Mut)
					opKind = OperatorKind::MutDeref;
			}
			else if (m_ExpectedHandle.Mod() == TypeMod::Mut)
			{
				opKind = OperatorKind::MutDeref;
			}
		}

		Operator op = g_Ctx.activeModule->opTable.GetOperator(opKind, exprTypeHandle, *m_pBoundsInfo);
		if (!op.result.IsValid())
		{
			// Check if constraints allow the op
			if (m_GenDecl)
				op = g_Ctx.activeModule->opTable.GetConstriantOperator(node.op, exprTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
			if (!op.result.IsValid() && m_Impl && m_Impl->genDecl)
				op = g_Ctx.activeModule->opTable.GetConstriantOperator(node.op, exprTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
		}

		const Bounds& bounds = m_pBoundsInfo->GetBounds(op.result);
		op.result = bounds.NarrowType(op.result);
		
		node.handle = op.result;
		if (op.isInterfaceOp)
		{
			if (exprTypeHandle.Kind() == TypeKind::Ref)
				exprTypeHandle = exprTypeHandle.AsRef().subType;
			if (op.left != exprTypeHandle)
				node.handle = g_TypeReg.ReplaceSubType(node.handle, op.left, exprTypeHandle);
		}
		
		node.operator_ = op;

		if (!node.operator_.left.IsValid())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdStringView opName = GetOpName(node.op);
			StdString typeName = exprTypeHandle.ToString();
			g_ErrorSystem.Error(span, "Unary operator '%s' not found for '%s'\n", opName.data(), typeName.c_str());
		}
	}

	void TypeInference::Visit(ITrQualNameExpr& node)
	{
		QualNameSPtr qualName = node.qualName = InferQualNameGenerics(node.qualName);
		SymbolSPtr sym;
		if (node.itrQualName->hasColonColon)
		{
			SymbolSPtr typeSym = g_Ctx.activeModule->symTable.Find(m_ExpectedHandle);
			if (typeSym)
				sym = typeSym->children->FindChild(nullptr, node.qualName->LastIden());
		}
		else
		{
			if (m_FuncCtx && qualName->IsBase())
			{
				LocalVarDataSPtr local = m_FuncCtx->localVars.GetLocalVarData(m_ScopeNames, qualName->LastIden());
				if (local)
				{
					node.handle = local->type;
					return;
				}
			}

			if (m_Impl)
			{
				sym = m_Impl->sym.lock()->children->FindChild(nullptr, qualName->LastIden());
			}

			if (!sym)
				sym = g_Ctx.activeModule->symTable.Find(GetCurScope(), qualName);
		}
		
		if (!sym)
		{
			MultiSpan span = g_SpanManager.GetSpan(node.startIdx, node.endIdx);
			StdString varName = qualName->ToString();
			if (node.itrQualName->hasColonColon)
				varName = "::" + varName;
			g_ErrorSystem.Error(span, "Cannot find '%s'", varName.c_str());
			return;
		}

		if (sym->qualName->Depth() > qualName->Depth())
		{
			QualNameSPtr baseQualName = sym->qualName->GetBaseName(sym->qualName->Depth() - qualName->Depth());
			qualName = baseQualName->Append(qualName->Idens(), qualName->Generics());	
		}
		node.sym = sym;
		
		SymbolInstSPtr inst = sym->GetOrCreateInst(qualName);
		node.handle = inst->type;

		switch (sym->kind)
		{
		case SymbolKind::Struct:
		case SymbolKind::Union:
		case SymbolKind::ValEnum:
		case SymbolKind::ValEnumMember:
		case SymbolKind::AdtEnum:
		case SymbolKind::AdtEnumMember:
		case SymbolKind::MarkerInterface:
		case SymbolKind::WeakInterface:
		case SymbolKind::StrongInterface:
		case SymbolKind::Typedef:
			node.handle = sym->SelfType();
			break;
		case SymbolKind::Typealias:
		case SymbolKind::Func:
		case SymbolKind::Method:
		case SymbolKind::Closure:
		case SymbolKind::Type:
		case SymbolKind::AssocType:
		case SymbolKind::GenVal:
		case SymbolKind::Var:
		default:
			node.handle = InferType(node.sym->type, TypeMod::None);
		}
		
	}

	void TypeInference::Visit(ITrIndexSlice& node)
	{
		Walk(node);

		TypeHandle exprTypeHandle = node.expr->handle;
		if (exprTypeHandle.Kind() == TypeKind::Ref)
			exprTypeHandle = exprTypeHandle.AsRef().subType;

		// First, try to get the mutable index
		Operator op = g_Ctx.activeModule->opTable.GetOperator(OperatorKind::MutIndex, exprTypeHandle, *m_pBoundsInfo);
		if (!op.result.IsValid())
		{
			// Check if constraints allow the op
			if (m_GenDecl)
				op = g_Ctx.activeModule->opTable.GetConstriantOperator(OperatorKind::MutIndex, exprTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
			if (!op.result.IsValid() && m_Impl && m_Impl->genDecl)
				op = g_Ctx.activeModule->opTable.GetConstriantOperator(OperatorKind::MutIndex, exprTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
		}

		// if not found, check for the normal index
		if (!op.result.IsValid())
		{
			Operator op = g_Ctx.activeModule->opTable.GetOperator(OperatorKind::Index, exprTypeHandle, *m_pBoundsInfo);
			if (!op.result.IsValid())
			{
				// Check if constraints allow the op
				if (m_GenDecl)
					op = g_Ctx.activeModule->opTable.GetConstriantOperator(OperatorKind::Index, exprTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
				if (!op.result.IsValid() && m_Impl && m_Impl->genDecl)
					op = g_Ctx.activeModule->opTable.GetConstriantOperator(OperatorKind::Index, exprTypeHandle, m_Impl->genDecl, *m_pBoundsInfo);
			}
		}

		node.handle = op.result;
		node.operator_ = op;

		if (!op.sym)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = exprTypeHandle.ToString();
			g_ErrorSystem.Error(span, "Index operator not found for '%s'\n", typeName.c_str());
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
				ptr.reset(new ITrAdtTupleEnumInit{ node.expr, std::move(node.args), node.endIdx });
				ptr->sym = sym;
				ptr->handle = sym->parent.lock()->SelfType();
			}
			else
			{
				ptr.reset(new ITrFuncCall{ node.expr, std::move(node.args), node.endIdx });
				ptr->sym = sym;
				ptr->handle = sym->type.AsFunc().retType;
			}
		}
		else
		{
			// TODO
			int br = 0;
		}
	}

	// TODO: named args
	// TODO: Generics
	void TypeInference::Visit(ITrFuncCall& node)
	{
		// Should only have method calls

		Walk(node);

		if (node.isMethod)
		{
			TypeHandle handle = node.callerOrFunc->handle;
			StdVector<TypeHandle> possibleCallerTypes = GetPossibleCallerType(node.callerOrFunc->handle);
			SymbolSPtr callerSym, methodSym;

			// TODO: prefer mutable over immutable
			for (TypeHandle type : possibleCallerTypes)
			{
				callerSym = g_Ctx.activeModule->symTable.Find(type);
				if (!callerSym)
					continue;
				
				methodSym = callerSym->children->FindChild(nullptr, node.iden);
				if (!methodSym && callerSym->kind == SymbolKind::StrongInterface)
				{
					for (SymbolInstWPtr iface : callerSym->ifaces)
					{
						methodSym = callerSym->children->FindChild(iface.lock()->qualName, node.iden);
						if (methodSym)
							break;
					}
				}
				
				if (methodSym)
					break;
			}

			
			// If not found and we have a ptr, try to deref and see if it has the method
			if (!methodSym && handle.Kind() == TypeKind::Ptr)
			{
				// TODO: Deref
			}

			// If not found, check constrains
			TypeHandle calledBound;
			if (!methodSym)
			{
				const Bounds& bounds = m_pBoundsInfo->GetBounds(handle);
				for (TypeHandle bound : bounds.bounds)
				{
					callerSym = g_Ctx.activeModule->symTable.Find(bound);
					if (!callerSym)
						continue;
					methodSym = callerSym->children->FindChild(nullptr, node.iden);
					if (methodSym)
					{
						calledBound = bound;
						break;
					}
				}
			}
			
			if (!methodSym)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString callerTypeName = g_TypeReg.ToString(handle);
				g_ErrorSystem.Error(span, "Cannot find method '%s' with caller '%s'\n", node.iden.c_str(), callerTypeName.c_str());
			}
			
			node.sym = methodSym;
			FuncType& funcType = methodSym->type.AsFunc();
			node.handle = funcType.retType;

			if (calledBound.IsValid())
				node.handle = g_TypeReg.ReplaceSubType(node.handle, calledBound, handle);
		}
		
	}

	void TypeInference::Visit(ITrAdtTupleEnumInit& node)
	{
	}

	void TypeInference::Visit(ITrMemberAccess& node)
	{
		Walk(node);

		TypeHandle type = node.expr->handle;
		if (type.Kind() == TypeKind::Ref)
		{
			type = type.AsRef().subType;
		}
		else if (type.Kind() == TypeKind::Ptr)
		{
			// TODO
		}

		if (type.Kind() != TypeKind::Iden)
		{
			MultiSpan span = g_SpanManager.GetSpan(node.startIdx, node.endIdx);
			StdString typeName = g_TypeReg.ToString(type);
			g_ErrorSystem.Error(span, "Cannot use a member access on a value of type '%s'", typeName.c_str());
			return;
		}

		SymbolSPtr sym = g_Ctx.activeModule->symTable.Find(GetCurScope(), type.AsIden().qualName);
		if (!sym)
		{
			MultiSpan span = g_SpanManager.GetSpan(node.startIdx, node.endIdx);
			StdString typeName = g_TypeReg.ToString(type);
			g_ErrorSystem.Error(span, "Cannot find symbol for type '%s'", typeName.c_str());
			return;
		}

		SymbolSPtr child = sym->children->FindChild(nullptr, node.iden);
		if (!child || child->kind != SymbolKind::Var)
		{
			MultiSpan span = g_SpanManager.GetSpan(node.startIdx, node.endIdx);
			StdString typeName = g_TypeReg.ToString(type);
			g_ErrorSystem.Error(span, "'%s' does not have a member named '%s'", typeName.c_str(), node.iden.c_str());
			return;
		}

		node.handle = child->type;
	}

	void TypeInference::Visit(ITrTupleAccess& node)
	{
		Walk(node);
		
		TypeHandle type = node.expr->handle;
		if (type.Kind() == TypeKind::Ref)
		{
			type = type.AsRef().subType;
		}
		else if (type.Kind() == TypeKind::Ptr)
		{
			// TODO
		}

		if (type.Kind() != TypeKind::Tuple)
		{
			MultiSpan span = g_SpanManager.GetSpan(node.startIdx, node.endIdx);
			StdString typeName = node.expr->handle.ToString();
			g_ErrorSystem.Error(span, "Cannot use a tuple access on a value of type '%s'", typeName.c_str());
			return;
		}

		TupleType& tupType = type.AsTuple();
		if (node.index >= tupType.subTypes.size())
		{
			MultiSpan span = g_SpanManager.GetSpan(node.startIdx, node.endIdx);
			g_ErrorSystem.Error(span, "Tuple index out of range, trying to use index %u on a tuple with size %u", node.index, tupType.subTypes.size());
			return;
		}

		node.handle = tupType.subTypes[node.index];
	}

	void TypeInference::Visit(ITrLiteral& node)
	{
		switch (node.lit.type)
		{
		case TokenType::True:
		case TokenType::False:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool); break;
		case TokenType::CharLit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char); break;
		case TokenType::F16Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::F16); break;
		case TokenType::F32Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::F32); break;
		case TokenType::F64Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::F64); break;
		case TokenType::F128Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::F128); break;
		case TokenType::I8Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::I8); break;
		case TokenType::I16Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::I16); break;
		case TokenType::I32Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::I32); break;
		case TokenType::I64Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::I64); break;
		case TokenType::I128Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::I128); break;
		case TokenType::StringLit:
		{
			TypeHandle charType = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char);
			node.handle = g_TypeReg.Slice(TypeMod::None, charType);
			break;
		}
		case TokenType::U8Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U8); break;
		case TokenType::U16Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16); break;
		case TokenType::U32Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U32); break;
		case TokenType::U64Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U64); break;
		case TokenType::U128Lit:
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::U128); break;
		default:;
		}

		if (m_ExpectedHandle.Kind() == TypeKind::Builtin)
		{
			TypeHandle foundType = node.handle;

			if (m_ExpectedHandle.AsBuiltin().builtin == BuiltinTypeKind::Bool &&
				foundType.AsBuiltin().builtin != BuiltinTypeKind::Bool)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString foundName = foundType.ToString();
				g_ErrorSystem.Error(span, "Expected a 'bool' type, but found literal of type '%s'", foundName.c_str());
				return;
			}
			if (m_ExpectedHandle.AsBuiltin().builtin != BuiltinTypeKind::Bool &&
				foundType.AsBuiltin().builtin == BuiltinTypeKind::Bool)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString foundName = foundType.ToString();
				g_ErrorSystem.Error(span, "Expected a '%s' type, but found literal of type 'bool'", foundName.c_str());
				return;
			}

			switch (node.lit.type)
			{
			case TokenType::F16Lit:
			case TokenType::F32Lit:
			case TokenType::F64Lit:
			case TokenType::F128Lit:
			{
				if (!IsBuiltinFloat(m_ExpectedHandle.AsBuiltin().builtin))
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Warning(span, "Possible truncation from fp to integral");
				}
				break;
			}
			default:;
			}

			node.handle = m_ExpectedHandle;
		}
	}

	void TypeInference::Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node)
	{
		Walk(node);

		TypeHandle objType = node.type->handle;
		if (objType.Kind() != TypeKind::Iden)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = g_TypeReg.ToString(objType);
			g_ErrorSystem.Error(span, "'%s' is not a valid type in an aggregate initializer\n", node.args.size());
			return;
		}

		IdenType& idenType = objType.AsIden();
		SymbolSPtr sym = g_Ctx.activeModule->symTable.Find(GetCurScope(), idenType.qualName);

		if (sym->kind == SymbolKind::AdtEnumMember ||
			sym->kind == SymbolKind::Struct)
		{
			StdVector<ITrArgSPtr>* pArgs;
			StdVector<u32>* pStructArgOrder = nullptr;
			
			if (sym->kind == SymbolKind::AdtEnumMember)
			{
				if (sym->type.Kind() == TypeKind::Tuple)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					StdString typeName = g_TypeReg.ToString(objType);
					g_ErrorSystem.Error(span, "'%s' needs to be initialized with parenthesis\n", node.args.size());
					return;
				}

				ptr.reset(new ITrAdtAggrEnumInit{ node.type, std::move(node.args), node.endIdx });
				ptr->handle = sym->parent.lock()->SelfType();
				ptr->sym = sym;
				
				ITrAdtAggrEnumInit& adtNode = *reinterpret_cast<ITrAdtAggrEnumInit*>(ptr.get());
				pArgs = &adtNode.args;
			}
			else if (sym->kind == SymbolKind::Struct)
			{
				ptr.reset(new ITrStructInit{ node.type, std::move(node.args), false, nullptr, node.endIdx });
				ptr->handle = sym->SelfType();
				ptr->sym = sym;

				ITrStructInit& structNode = *reinterpret_cast<ITrStructInit*>(ptr.get());
				pArgs = &structNode.args;
				pStructArgOrder = &structNode.argOrder;
			}

			StdVector<SymbolSPtr> children;
			StdUnorderedMap<StdString, u32> childrenNameMapping;
			children.reserve(sym->orderedVarChildren.size());
			for (SymbolWPtr childW : sym->orderedVarChildren)
			{
				SymbolSPtr child = childW.lock();
				childrenNameMapping.try_emplace(child->qualName->LastIden(), u32(children.size()));
				children.push_back(child);
			}

			if (pArgs->size() > children.size())
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "Found more arguments then expected, found %u, expected %u\n", pArgs->size(), children.size());
				return;
			}

			bool hasIden = false;
			StdVector<bool> argAssigned;
			argAssigned.resize(children.size(), false);
			for (usize i = 0; i < pArgs->size(); ++i)
			{
				ITrArgSPtr arg = (*pArgs)[i];
				if (!arg->iden.empty())
				{
					hasIden |= i == 0;
					if (!hasIden)
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an adt enum\n");
						return;
					}

					auto it = childrenNameMapping.find(arg->iden);
					if (it == childrenNameMapping.end())
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						const char* symKindName = sym->kind == SymbolKind::AdtEnumMember ? "adt enum" : "struct";
							g_ErrorSystem.Error(span, "The %s does not contain any variable named '%s'\n", symKindName, arg->iden.c_str());
						return;
					}

					u32 idx = it->second;
					if (argAssigned[idx])
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						g_ErrorSystem.Error(span, "Variable '%s' has already been assigned\n", arg->iden.c_str());
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->handle;
					if (!g_TypeReg.CanPassTo(expected, argType))
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						StdString expectedName = expected.ToString();
						StdString argName = argType.ToString();
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
					argAssigned[idx] = true;

					if (pStructArgOrder)
						pStructArgOrder->push_back(idx);
				}
				else
				{
					if (hasIden)
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						g_ErrorSystem.Error(span, "Cannot mix named and unnamed arguments when initializing an adt enum\n");
						return;
					}

					TypeHandle expected = children[i]->type;
					TypeHandle argType = arg->expr->handle;
					if (!g_TypeReg.CanPassTo(expected, argType))
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						StdString expectedName = expected.ToString();
						StdString argName = argType.ToString();
						g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
						return;
					}
				}
			}
			
			if (pArgs->size() < children.size())
			{
				if (node.hasDefInit)
				{
					QualNameSPtr defInterfaceQualName = QualName::Create(StdVector<StdString>{ "core", "default", "Default" });

					bool implsDefault = false;
					for (SymbolInstWPtr iface : sym->ifaces)
					{
						if (iface.lock()->qualName == defInterfaceQualName)
						{
							implsDefault = true;
							break;
						}
					}

					if (!implsDefault)
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						StdString symName = sym->qualName->ToString();
						g_ErrorSystem.Error(span, "'%s' does not implement 'core::defaul::Default' or 'std::default::Default'\n", symName.c_str());
					}
				}
				else if (node.defExpr)
				{
					TypeHandle defType = node.defExpr->handle;
					defType = g_TypeReg.Mod(TypeMod::None, defType);

					bool validDef = defType == sym->type;
					if (!validDef && defType.Kind() == TypeKind::Ref)
					{
						defType = defType.AsRef().subType;
						defType = g_TypeReg.Mod(TypeMod::None, defType);
						validDef = defType == sym->type;
					}

					if (!validDef)
					{
						Span span = g_SpanManager.GetSpan(node.startIdx);
						StdString typeName = defType.ToString();
						g_ErrorSystem.Error(span, "cannot initialize unspecified members from an expression with type '%s'\n", typeName.c_str());
					}

				}
				else
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Error(span, "Not all arguments have been initialized\n");
					return;
				}
			}
		}
		else if (sym->kind == SymbolKind::Union)
		{
			if (node.args.size() > 1)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "A union must be initialized with exactly 1 member\n");
				return;
			}
			
			ptr.reset(new ITrUnionInit{ node.type, node.args[0], node.endIdx });
			ptr->handle = sym->SelfType();
			ptr->sym = sym;

			ITrUnionInit& unionNode = *reinterpret_cast<ITrUnionInit*>(ptr.get());

			StdVector<SymbolSPtr> children;
			children.reserve(sym->orderedVarChildren.size());
			StdUnorderedMap<StdString, u32> childrenNameMapping;

			for (SymbolWPtr childW : sym->orderedVarChildren)
			{
				SymbolSPtr child = childW.lock();
				childrenNameMapping.try_emplace(child->qualName->LastIden(), u32(children.size()));
				children.push_back(child);
			}

			if (!unionNode.arg)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "Cannot initialize a union without a member\n");
				return;
			}

			if (unionNode.arg->iden.empty())
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "Argument used to initialize a union require an identifier\n");
				return;
			}

			SymbolSPtr foundChild;
			for (SymbolSPtr child : children)
			{
				const StdString& name = child->qualName->LastIden();
				if (name == unionNode.arg->iden)
				{
					foundChild = child;
					break;
				}
			}

			if (foundChild)
			{
				TypeHandle expected = foundChild->type;
				TypeHandle argType = unionNode.arg->expr->handle;
				if (!g_TypeReg.CanPassTo(expected, argType))
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					StdString expectedName = expected.ToString();
					StdString argName = argType.ToString();
					g_ErrorSystem.Error(span, "Cannot pass '%s' to '%s'\n", argName.c_str(), expectedName.c_str());
				}
			}
			else
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				g_ErrorSystem.Error(span, "No member with the name '%s' exists\n", unionNode.arg->iden.c_str());
			}
		}
		else
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = g_TypeReg.ToString(objType);
			g_ErrorSystem.Error(span, "'%s' is not an aggregate or a adt-enum\n", typeName.c_str());
		}
	}

	void TypeInference::Visit(ITrTupleInit& node)
	{
		Walk(node);

		StdVector<TypeHandle> subTypes;
		for (ITrExprSPtr expr : node.exprs)
		{
			subTypes.push_back(expr->handle);
		}
		node.handle = g_TypeReg.Tuple(TypeMod::None, subTypes);
	}

	void TypeInference::Visit(ITrArrayInit& node)
	{
		Walk(node);
		
		TypeHandle type;
		for (ITrExprSPtr expr : node.exprs)
		{
			if (!type.IsValid())
			{
				type = expr->handle;
			}
			else if (expr->handle != type)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString type0Name = type.ToString();
				StdString type1Name = expr->handle.ToString();
				g_ErrorSystem.Error(span, "An array connot contain values of different types, found '%s' and '%s'", type0Name.c_str(), type1Name.c_str());
			}
		}
		node.handle = type;
	}

	void TypeInference::Visit(ITrCast& node)
	{
		if (node.handle.IsValid())
			return;

		Walk(node);

		TypeHandle fromType = node.expr->handle;
		TypeHandle toType = node.type->handle;

		Operator op;
		switch (node.castKind)
		{
		case ITrCastKind::SafeCast:
		case ITrCastKind::NullPanicCast:
		{
			op = g_Ctx.activeModule->opTable.GetOperator(OperatorKind::TryCast, fromType, toType, *m_pBoundsInfo);
			node.castToTryCast = !op.sym;
			// Fallthrough
		}
		case ITrCastKind::Cast:
		{
			if (!op.sym)
				op = g_Ctx.activeModule->opTable.GetOperator(OperatorKind::Cast, fromType, toType, *m_pBoundsInfo);

			node.handle = op.result;
			node.operator_ = op;

			if (!op.sym)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString lTypeName = fromType.ToString();
				StdString rTypeName = toType.ToString();
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
			u64 fromSize = fromType.Size();
			u64 toSize = toType.Size();

			if (fromSize != toSize)
			{
				Span span = g_SpanManager.GetSpan(node.startIdx);
				StdString lTypeName = fromType.ToString();
				StdString rTypeName = toType.ToString();
				g_ErrorSystem.Error(span, "cannot transmute %s -> %s, since they are different sizes\n", lTypeName.c_str(), rTypeName.c_str());
				return;
			}

			node.handle = toType;
			break;
		}
		default: ;
		}
	}

	void TypeInference::Visit(ITrBlockExpr& node)
	{
		Walk(node);
		// TODO
	}

	void TypeInference::Visit(ITrUnsafeExpr& node)
	{
		Walk(node);
		node.handle = node.expr->handle;
	}

	void TypeInference::Visit(ITrComma& node)
	{
	}

	void TypeInference::Visit(ITrClosure& node)
	{
	}

	void TypeInference::Visit(ITrMove& node)
	{
		Walk(node);
		node.handle = node.expr->handle;
	}

	void TypeInference::Visit(ITrIs& node)
	{
		Walk(node);
		node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool);
	}

	void TypeInference::Visit(ITrTry& node)
	{
		Walk(node);

		switch (node.expr->exprKind)
		{
		case ITrExprKind::FuncOrMethodCall: break;
		default:
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			g_ErrorSystem.Error(span, "'try' can only be used on a function/method call");
		}
		}

		IdenType& exprType = node.expr->handle.AsIden();
		TypeHandle handle = exprType.qualName->Generics()[0].type;

		if (node.kind == ITrTryKind::Nullable && handle.Kind() != TypeKind::Opt)
			handle = g_TypeReg.Opt(TypeMod::None, handle);
		
		node.handle = handle;
	}

	void TypeInference::Visit(ITrSpecKw& node)
	{
		switch (node.kw)
		{
		case TokenType::SLine:
		{
			node.handle = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::USize);
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
			TypeHandle tmp = g_TypeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char);
			node.handle = g_TypeReg.Slice(TypeMod::None, tmp);
			break;
		}
		default:;
		}
	}

	void TypeInference::Visit(ITrCompRun& node)
	{
		Walk(node);
		node.handle = node.expr->handle;
	}

	void TypeInference::Visit(ITrType& node)
	{
		for (ITrTypeSPtr subType : node.subTypes)
		{
			Visit(*subType);
		}
		if (node.expr)
			ITrVisitor::Visit(node.expr);
		
		TypeMod mod = node.handle.IsValid() ? node.handle.Mod() : TypeMod::None;
		if (node.attribs && ENUM_IS_SET(node.attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;

		if (node.subTypes.empty())
		{
			node.handle = InferType(node.handle, mod);
		}
		else
		{
			switch (node.handle.Kind())
			{
			case TypeKind::Ptr:
			{
				if (node.subTypes.empty())
					break;

				node.handle = g_TypeReg.Ptr(mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Ref:
			{
				if (node.subTypes.empty())
					break;

				node.handle = g_TypeReg.Ref(mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Slice:
			{
				if (node.subTypes.empty())
					break;

				node.handle = g_TypeReg.Slice(mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Array:
			{
				if (node.subTypes.empty())
					break;

				if (node.handle.AsArray().arrSize != u64(-1))
					node.handle = g_TypeReg.Array(mod, node.subTypes[0]->handle, node.handle.AsArray().size);
				else
					node.handle = g_TypeReg.Array(mod, node.subTypes[0]->handle, node.handle.AsArray().expr);
				break;
			}
			case TypeKind::Tuple:
			{
				if (node.subTypes.empty() && node.handle.IsValid())
					break;

				StdVector<TypeHandle> subTypes;
				for (ITrTypeSPtr subType : node.subTypes)
				{
					subTypes.push_back(subType->handle);
				}

				node.handle = g_TypeReg.Tuple(mod, subTypes);
				break;
			}
			case TypeKind::Opt:
			{
				if (node.subTypes.empty())
					break;

				node.handle = g_TypeReg.Opt(mod, node.subTypes[0]->handle);
				break;
			}
			case TypeKind::Func:
			{
				if (node.subTypes.empty() && node.handle.IsValid())
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
					//node.handle = g_TypeReg.Func(mod, subTypes);
				}
				break;
			}
			default:;
			}
		}

		const Bounds& bounds = m_pBoundsInfo->GetBounds(node.handle);
		node.handle = bounds.NarrowType(node.handle);
	}

	void TypeInference::Visit(ITrAdtAggrEnumPattern& node)
	{
		// Should only receive patterns like: '::iden{...}'
		if (m_ExpectedHandle.Kind() != TypeKind::Iden ||
			m_ExpectedHandle.AsIden().sym.lock()->kind != SymbolKind::AdtEnumMember)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match an adt enum pattern with '%s'\n", typeName.c_str());
			return;
		}

		// Since we do not have the name of the enum, we just get it from the expected type
		SymbolSPtr memberSym = m_ExpectedHandle.AsIden().sym.lock();
		SymbolSPtr typeSym = memberSym->type.AsIden().sym.lock();

		StdVector<SymbolSPtr> children;
		typeSym->children->Foreach([&children](SymbolSPtr sym, QualNameSPtr ifaceQualName)
		{
			if (!ifaceQualName && sym->kind == SymbolKind::Var)
				children.push_back(sym);
		});

		if (node.args.size() > children.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.args.size());
			return;
		}

		std::sort(children.begin(), children.end(), [](SymbolSPtr first, SymbolSPtr second)
		{
			return first->offset < second->offset;
		});

		bool hasWildcard = false;
		for (usize i = 0; i < node.args.size(); ++i)
		{
			StdPair<StdString, ITrPatternSPtr>& arg = node.args[i];
			if (arg.second->patternKind == ITrPatternKind::Wildcard)
			{
				if (hasWildcard)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Error(span, "Cannot have more than 1 wildcard in a pattern\n");
					return;
				}
				hasWildcard = true;
				continue;
			}

			SymbolSPtr child = !arg.first.empty() ? memberSym->children->FindChild(nullptr, arg.first) : children[i];
			if (!child)
			{
				Span span = g_SpanManager.GetSpan(arg.second->startIdx);
				StdString name = memberSym->qualName->ToString();
				g_ErrorSystem.Error(span, "adt enum '%s' does not have child '%s'\n", name.c_str(), arg.first);
			}
			else
			{
				SaveRestore saveRes(m_ExpectedHandle, child->type);
				ITrVisitor::Visit(arg.second);
			}
		}

		if (!hasWildcard && node.args.size() < children.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.args.size());
		}

		node.patternType = m_ExpectedHandle;

		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(scopeNames, node.imm);
		var->type = typeSym->type;
	}

	void TypeInference::Visit(ITrAdtTupleEnumPattern& node)
	{
		// Should only receive patterns like: '::iden(...)'
		
		if (m_ExpectedHandle.Kind() != TypeKind::Iden)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match an tuple enum pattern with '%s'\n", typeName.c_str());
			return;
		}

		// Since we do not have the name of the enum, we just get it from the expected type
		SymbolSPtr adtEnumSym = m_ExpectedHandle.AsIden().sym.lock();
		SymbolSPtr memberSym = adtEnumSym->children->FindChild(nullptr, node.qualName->LastIden());

		if (memberSym->type.Kind() != TypeKind::Tuple)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = memberSym->qualName->ToString();
			g_ErrorSystem.Error(span, "cannot match an tuple enum pattern with '%s'\n", typeName.c_str());
			return;
		}
		
		TupleType& tupType = memberSym->type.AsTuple();
		if (node.subPatterns.size() > tupType.subTypes.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.subPatterns.size());
			return;
		}

		bool hasWildcard = false;
		for (usize i = 0; i < node.subPatterns.size(); ++i)
		{
			ITrPatternSPtr subPattern = node.subPatterns[i];
			if (subPattern->patternKind == ITrPatternKind::Wildcard)
			{
				if (i != node.subPatterns.size() - 1)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Error(span, "wildcard pattern is only allowed as last sub-pattern\n");
				}
				hasWildcard = true;
				continue;
			}

			SaveRestore saveRes(m_ExpectedHandle, tupType.subTypes[i]);
			ITrVisitor::Visit(subPattern);
		}

		if (!hasWildcard && node.subPatterns.size() < tupType.subTypes.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.subPatterns.size());
		}

		node.patternType = m_ExpectedHandle;

		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(scopeNames, node.imm);
		var->type = memberSym->type;
	}

	void TypeInference::Visit(ITrAggrPattern& node)
	{
		Walk(node);
		
		if (m_ExpectedHandle.Kind() != TypeKind::Iden)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match an aggregate with '%s'\n", typeName.c_str());
			return;
		}

		SymbolSPtr handleSym = m_ExpectedHandle.AsIden().sym.lock();
		if (handleSym->kind != SymbolKind::Struct)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match an aggregate with '%s'\n", typeName.c_str());
		}

		SymbolSPtr aggrSym = node.qualName ? g_Ctx.activeModule->symTable.Find(GetCurScope(), node.qualName) : handleSym;

		StdVector<SymbolSPtr> children;
		aggrSym->children->Foreach([&children](SymbolSPtr sym, QualNameSPtr ifaceQualName)
		{
			if (ifaceQualName)
				return;
			if (sym->kind != SymbolKind::Var)
				return;
			children.push_back(sym);
		});

		if (node.args.size() > children.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.args.size());
			return;
		}

		std::sort(children.begin(), children.end(), [](SymbolSPtr first, SymbolSPtr second)
		{
			return first->offset < second->offset;
		});

		bool hasWildcard = false;
		for (usize i = 0; i < node.args.size(); ++i)
		{
			StdPair<StdString, ITrPatternSPtr>& arg = node.args[i];
			if (arg.second->patternKind == ITrPatternKind::Wildcard)
			{
				if (hasWildcard)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Error(span, "Cannot have more than 1 wildcard in a pattern\n");
					return;
				}
				hasWildcard = true;
				continue;
			}

			SymbolSPtr child = !arg.first.empty() ? aggrSym->children->FindChild(nullptr, arg.first) : children[i];
			if (!child)
			{
				Span span = g_SpanManager.GetSpan(arg.second->startIdx);
				StdString name = aggrSym->qualName->ToString();
				g_ErrorSystem.Error(span, "adt enum '%s' does not have child '%s'\n", name.c_str(), arg.first);
			}
			else
			{
				SaveRestore saveRes(m_ExpectedHandle, child->type);
				ITrVisitor::Visit(arg.second);
			}
		}

		if (!hasWildcard && node.args.size() < children.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.args.size());
		}

		node.patternType = m_ExpectedHandle;

		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(scopeNames, node.imm);
		var->type = m_ExpectedHandle;
	}

	void TypeInference::Visit(ITrLiteralPattern& node)
	{
		bool doesMatch;
		switch (node.lit.type)
		{
		case TokenType::False:
		case TokenType::True:
		{
			doesMatch = m_ExpectedHandle.Kind() == TypeKind::Builtin && m_ExpectedHandle.AsBuiltin().builtin == BuiltinTypeKind::Bool;
			break;
		}
		case TokenType::Null:
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			g_ErrorSystem.Error(span, "null literal patterns are not allowed");
			return;
		}
		case TokenType::CharLit:
		{
			doesMatch = m_ExpectedHandle.Kind() == TypeKind::Builtin && m_ExpectedHandle.AsBuiltin().builtin == BuiltinTypeKind::Char;
			break;
		}
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			g_ErrorSystem.Error(span, "Float literal patterns are not allowed");
			return;
		}
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
			doesMatch = m_ExpectedHandle.Kind() == TypeKind::Builtin && IsBuiltinInteger(m_ExpectedHandle.AsBuiltin().builtin);
			break;
		case TokenType::StringLit:
			doesMatch = false;
			break; // TODO
		default:
			doesMatch = false;
		}
		
		if (!doesMatch)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match '%s' to a literal.\n", typeName.c_str());
		}

		node.patternType = m_ExpectedHandle;
	}

	void TypeInference::Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node)
	{
		// Should only receive patterns like: 'qual_name::iden{...}'
		
		Walk(node);
		
		SymbolSPtr sym = g_Ctx.activeModule->symTable.Find(GetCurScope(), node.qualName);
		if (!sym)
		{
			return;
		}

		StdString imm = node.imm;
		if (sym->kind == SymbolKind::Struct)
		{
			ptr.reset(new ITrAggrPattern{ node.qualName, std::move(node.args), node.startIdx, node.endIdx });
			ITrVisitor::Visit(ptr);
			ptr->imm = imm;
		}
		else if (sym->kind == SymbolKind::AdtEnumMember)
		{
			ptr.reset(new ITrAdtAggrEnumPattern{ node.qualName, std::move(node.args), node.startIdx, node.endIdx });
			ITrVisitor::Visit(ptr);
			ptr->imm = imm;
		}
		else
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "unknown aggregate match pattern of type '%s'\n", typeName.c_str());
		}
		ptr->patternType = m_ExpectedHandle;
	}

	void TypeInference::Visit(ITrSlicePattern& node)
	{
		if (m_ExpectedHandle.Kind() != TypeKind::Array &&
			m_ExpectedHandle.Kind() != TypeKind::Slice)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match a slice pattern with '%s'\n", typeName.c_str());
		}

		TypeHandle subType = m_ExpectedHandle.Kind() == TypeKind::Array ? m_ExpectedHandle.AsArray().subType : m_ExpectedHandle.AsSlice().subType;
		
		SaveRestore saveRes(m_ExpectedHandle, subType);
		Walk(node);

		node.patternType = m_ExpectedHandle;

		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(scopeNames, node.imm);
		var->type = m_ExpectedHandle;
	}

	void TypeInference::Visit(ITrTuplePattern& node)
	{
		if (m_ExpectedHandle.Kind() != TypeKind::Tuple)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match a tuple pattern with '%s'\n", typeName.c_str());
		}

		TupleType& tupType = m_ExpectedHandle.AsTuple();
		if (node.subPatterns.size() > tupType.subTypes.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u elements, cannot match\n", typeName.c_str(), node.subPatterns.size());
			return;
		}

		bool hasWildcard = false;
		for (usize i = 0; i < node.subPatterns.size(); ++i)
		{
			ITrPatternSPtr subPattern = node.subPatterns[i];
			if (subPattern->patternKind == ITrPatternKind::Wildcard)
			{
				if (i != node.subPatterns.size() - 1)
				{
					Span span = g_SpanManager.GetSpan(node.startIdx);
					g_ErrorSystem.Error(span, "wildcard pattern is only allowed as last sub-pattern\n");
				}
				hasWildcard = true;
				continue;
			}

			SaveRestore saveRes(m_ExpectedHandle, tupType.subTypes[i]);
			ITrVisitor::Visit(subPattern);
		}

		if (!hasWildcard && node.subPatterns.size() < tupType.subTypes.size())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "'%s' does not have %u children, cannot match\n", typeName.c_str(), node.subPatterns.size());
		}

		node.patternType = m_ExpectedHandle;

		StdVector<StdString> scopeNames{ m_ScopeNames.begin(), m_ScopeNames.end() - 1 };
		LocalVarDataSPtr var = m_FuncCtx->localVars.ActivateNextVar(scopeNames, node.imm);
		var->type = m_ExpectedHandle;
	}

	void TypeInference::Visit(ITrTypePattern& node)
	{
		Visit(*node.type);
		if (g_TypeReg.MatchTypes(m_ExpectedHandle, node.type->handle, *g_Ctx.activeModule))
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match a type pattern with '%s'\n", typeName.c_str());
		}

		node.patternType = m_ExpectedHandle;
	}

	void TypeInference::Visit(ITrValueBindPattern& node)
	{
		Walk(node);

		LocalVarDataSPtr localVar = m_FuncCtx->localVars.ActivateNextVar(m_ScopeNames, node.iden);
		node.patternType = localVar->type = InferType(m_ExpectedHandle, m_ExpectedHandle.Mod());
	}

	void TypeInference::Visit(ITrValueEnumPattern& node)
	{
		// Should only receive patterns like: 'opt_qual_name::iden'
		
		if (m_ExpectedHandle.Kind() != TypeKind::Iden)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString typeName = m_ExpectedHandle.ToString();
			g_ErrorSystem.Error(span, "cannot match a value enum pattern with '%s'\n", typeName.c_str());
		}

		IdenType& type = m_ExpectedHandle.AsIden();
		SymbolSPtr enumSym = node.qualName->IsBase() ? type.sym.lock() : g_Ctx.activeModule->symTable.Find(GetCurScope(), node.qualName->Base());

		if (!enumSym || 
			enumSym->kind != SymbolKind::ValEnum &&
			enumSym->kind != SymbolKind::AdtEnum)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString name = enumSym->qualName->ToString();
			g_ErrorSystem.Error(span, "'%s' is not a value enum'\n", name.c_str(), node.qualName->LastIden());
			return;
		}

		SymbolSPtr child = enumSym->children->FindChild(nullptr, node.qualName->LastIden());
		if (!child)
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString name = enumSym->qualName->ToString();
			g_ErrorSystem.Error(span, "'%s' is not a valid member of '%s'", node.qualName->LastIden().c_str(), name.c_str());
		}
		
		if (enumSym->kind == SymbolKind::AdtEnum && !child->children->Empty())
		{
			Span span = g_SpanManager.GetSpan(node.startIdx);
			StdString name = enumSym->qualName->ToString();
			g_ErrorSystem.Error(span, "adt enum value '%s' has unmatched children\n", name.c_str());
		}

		node.patternType = m_ExpectedHandle;
	}

	TypeHandle TypeInference::InferType(TypeHandle handle, TypeMod mod)
	{
		ModuleSymbolTable& symTable = g_Ctx.activeModule->symTable;
		StdVector<TypeHandle> tmp = g_TypeReg.GetSubTypes(handle, TypeKind::Iden);

		TypeHandle newType;
		for (TypeHandle tmpHandle : tmp)
		{
			QualNameSPtr qualName = tmpHandle.AsIden().qualName;
			if (qualName->LastIden() == "Self" &&
				m_SelfType.IsValid())
			{
				qualName = m_SelfType.AsIden().qualName;
			}

			// First, replace all generics in the qualname
			qualName = InferQualNameGenerics(qualName);

			SymbolSPtr sym;
			{
				// Try to find the symbol with an interface
				if (m_InterfaceQualname)
					sym = symTable.Find(GetCurScope(), qualName, m_InterfaceQualname);
				
				// Otherwise look for the symbol without an interface
				if (!sym)
					sym = symTable.Find(GetCurScope(), qualName);
				
				// If no type is found here, check for types in parent interfaces of the current interface
				if (!sym)
				{
					StdVector<StdString> idens = qualName->Idens();
					for (QualNameSPtr interfaceQualName : m_SubInterfaceQualNames)
					{
						QualNameSPtr findQualName = interfaceQualName->Append(idens, qualName->Generics());
						sym = symTable.Find(GetCurScope(), findQualName);
						if (sym)
							break;
					}
				}
				
				if (sym)
				{
					QualNameSPtr baseName = sym->qualName->GetBaseName(sym->qualName->Depth() - qualName->Depth());
					QualNameSPtr tmpQualName;
					if (baseName)
						tmpQualName = baseName->Append(qualName->Idens(), qualName->Generics());
					else
						tmpQualName = qualName;
					
					newType = g_TypeReg.Iden(mod, tmpQualName);
					if (!newType.AsIden().sym.lock())
						newType.AsIden().sym = sym;
				}

				if (qualName->IsBase())
				{
					// If we have an generics, we need to make sure that if a matching type appears both outside the symbol and as a generic, that the generic is preferred
					// this can only happen if the name of the type we are looking for has no base scope
					if (m_GenDecl)
					{
						for (ITrGenParamSPtr param : m_GenDecl->params)
						{
							if (!param->isType)
								continue;

							ITrGenTypeParam& typeParam = static_cast<ITrGenTypeParam&>(*param);
							if (typeParam.iden != qualName->LastIden())
								continue;

							// Either no symbol has been found or the found symbol appears outside of the current scope
							if (!sym ||
								!sym->qualName->IsSubnameOf(m_Scope) ||
								!(newType.Kind() == TypeKind::Iden && newType.AsIden().qualName->IsSubnameOf(m_Scope)))
									newType = m_GenMapping.at(typeParam.iden);
							break;
						}
					}
					// Do  the same as above, but for types that appear outside the impl and as a generic of the impl
					else if (m_Impl && m_Impl->genDecl)
					{
						for (ITrGenParamSPtr param : m_Impl->genDecl->params)
						{
							if (!param->isType)
								continue;

							ITrGenTypeParam& typeParam = static_cast<ITrGenTypeParam&>(*param);
							if (typeParam.iden != qualName->LastIden())
								continue;

							// Either no symbol has been found or the found symbol appears outside of the current scope
							if (!sym ||
								!sym->qualName->IsSubnameOf(m_Scope) ||
								!(newType.Kind() == TypeKind::Iden && newType.AsIden().qualName->IsSubnameOf(m_Scope) ))
								newType = m_GenMapping.at(typeParam.iden);
							break;
						}
					}
				}
			}

			if (newType.IsValid())
			{
				TypeHandle replacement = g_TypeReg.Mod(mod, newType);
				handle = g_TypeReg.ReplaceSubType(handle, tmpHandle, replacement);
			}
		}
		return handle;
	}

	QualNameSPtr TypeInference::InferQualNameGenerics(QualNameSPtr origQualName)
	{
		TypeDisambiguationSPtr disambig;
		if (origQualName->Disambiguation())
		{
			disambig = InferDisambigGenerics(origQualName->Disambiguation());
		}
		StdVector<IdenGeneric> idenGens = InferGenerics(origQualName->Generics());

		QualNameSPtr qualName = QualName::Create(disambig);
		if (qualName)
			return qualName->Append(origQualName->Idens(), idenGens);
		return QualName::Create(origQualName->Idens(), idenGens);
	}

	StdVector<IdenGeneric> TypeInference::InferGenerics(const StdVector<IdenGeneric>& origGens)
	{
		if (origGens.empty())
			return origGens;

		StdVector<IdenGeneric> idenGens;
		for (const IdenGeneric& origIdenGen : origGens)
		{
			if (!origIdenGen.isType)
			{
				idenGens.push_back(origIdenGen);
				continue;
			}

			IdenGeneric idenGen = origIdenGen;
			idenGen.type = InferType(idenGen.type, TypeMod::None);
			idenGens.push_back(idenGen);
		}

		return idenGens;
	}

	TypeDisambiguationSPtr TypeInference::InferDisambigGenerics(TypeDisambiguationSPtr origDisambig)
	{
		TypeHandle type = InferType(origDisambig->Type(), TypeMod::None);
		QualNameSPtr qualName = InferQualNameGenerics(origDisambig->IfaceQualName());
		return TypeDisambiguation::Create(type, qualName);
	}

	void TypeInference::HandleGenerics(ITrDef& def)
	{
		if (def.impl)
			*m_pBoundsInfo = def.impl->sym.lock()->boundsInfo;
		
		if (def.genDecl)
		{
			for (ITrGenTypeBoundSPtr bound : def.genDecl->bounds)
			{
				// TODO: Make sure that non-generic types can only be bound when they depend on a generic
				Visit(*bound->type);
				Visit(*bound->bound->type);

				StdVector<TypeSPtr> genTypes = g_TypeReg.ExtractGenerics(bound->type->handle.Type());
				if (genTypes.empty())
				{
					Span span = g_SpanManager.GetSpan(bound->startIdx);
					g_ErrorSystem.Error(span, "Type needs to depend on at least 1 generic to be bound");
				}

				Bounds& bounds = m_pBoundsInfo->GetOrAddBounds(bound->type->handle);
				bounds.bounds.push_back(bound->bound->type->handle);
				HandleAssocTypes(bound->type->handle, *bound->bound);
			}
			m_pBoundsInfo->RemoveUnboundTypes();
		}

	}

	void TypeInference::HandleAssocTypes(TypeHandle srcType, ITrGenBoundType& boundType)
	{
		Visit(*boundType.type);
		TypeHandle type = boundType.type->handle;

		Bounds& bounds = m_pBoundsInfo->GetOrAddBounds(srcType);
		AddUnique(bounds.bounds, type);
		
		for (ITrGenAssocBound& assocBound : boundType.assocBounds)
		{
			TypeDisambiguationSPtr disambig = TypeDisambiguation::Create(srcType, type.AsIden().qualName);
			QualNameSPtr subQualName = QualName::Create(disambig);
			subQualName = subQualName->Append(assocBound.iden);

			TypeHandle toBindType = g_TypeReg.Iden(TypeMod::None, subQualName);
			
			HandleAssocTypes(toBindType, *assocBound.type);
		}
	}

	void TypeInference::UpdateInstantiations(SymbolSPtr sym)
	{
		TypeHandle baseType = sym->type;
		StdVector<IdenGeneric>& baseGens = sym->qualName->Generics();
		
		for (SymbolInstSPtr inst : sym->instantiations)
		{
			StdVector<IdenGeneric>& gens = inst->qualName->Generics();
			TypeHandle type = baseType;
			for (usize i = 0; i < gens.size(); ++i)
			{
				if (gens[i].isType)
				{
					type = g_TypeReg.ReplaceSubType(type, baseGens[i].type, gens[i].type);
				}
				else
				{
					// TODO
				}
			}
			inst->type = type;
		}
	}

	StdVector<TypeHandle> TypeInference::GetPossibleCallerType(TypeHandle type)
	{
		StdVector<TypeHandle> possibleTypes = GetPossibleCallersForType(type);
		if (type.Kind() == TypeKind::Ref)
		{
			StdVector<TypeHandle> tmp = GetPossibleCallersForType(type.AsRef().subType);
			possibleTypes.insert(possibleTypes.begin(), tmp.begin(), tmp.end());
		}
		else
		{
			TypeHandle tmpType = g_TypeReg.Ref(TypeMod::None, type);
			StdVector<TypeHandle> tmp = GetPossibleCallersForType(tmpType);
			possibleTypes.insert(possibleTypes.begin(), tmp.begin(), tmp.end());
		}

		// TODO: OpDeref && OpMutDeref
		if (type.Kind() == TypeKind::Ptr)
		{
			StdVector<TypeHandle> tmp = GetPossibleCallersForType(type.AsPtr().subType);
			possibleTypes.insert(possibleTypes.begin(), tmp.begin(), tmp.end());
		}
		return possibleTypes;
	}

	StdVector<TypeHandle> TypeInference::GetPossibleCallersForType(TypeHandle type)
	{
		StdVector<TypeHandle> possibleTypes;
		if (type.Kind() == TypeKind::Iden)
		{
			possibleTypes.push_back(type);
			if (type.Mod() == TypeMod::Mut)
				possibleTypes.push_back(g_TypeReg.Mod(TypeMod::None, type));
			return possibleTypes;
		}
		
		for (const StdPair<const TypeSPtr, SymbolSPtr>& pair : g_Ctx.activeModule->symTable.GetTypeSymbols())
		{
			if (g_TypeReg.CanPassTo(pair.second->type, type))
				possibleTypes.push_back(pair.second->type);
		}
		return possibleTypes;
	}

	QualNameSPtr TypeInference::GetCurScope()
	{
		if (m_ScopeNames.empty())
			return m_Scope;

		QualNameSPtr qualName = m_Scope;
		for (StdString& scopeName : m_ScopeNames)
		{
			qualName = qualName->Append(scopeName);
		}
		return qualName;
	}

	void TypeInference::Expect(TypeHandle handle)
	{
		m_ExpectedHandle = handle;
	}

	void TypeInference::ExpectNone()
	{
		Expect(TypeHandle{});
	}
}
