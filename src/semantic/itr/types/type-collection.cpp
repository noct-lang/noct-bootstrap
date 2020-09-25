#include "type-collection.hpp"
#include "common/context.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"
#include "module/module.hpp"
#include "common/errorsystem.hpp"

namespace Noctis
{
	TypeCollectionCommon::TypeCollectionCommon(const char* name, Context* pCtx)
		: ITrSemanticPass(name, pCtx)
		, m_InImpl(false)
		, m_InInterface(false)
	{
		m_VisitDefs = true;
	}

	void TypeCollectionCommon::Visit(ITrStruct& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Struct, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrUnion& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Union, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrValEnum& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::ValEnum, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrValEnumMember& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::ValEnumMember, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);
	}

	void TypeCollectionCommon::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AdtEnum, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrAdtEnumMember& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AdtEnumMember, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);

		if (node.type)
		{
			sym->type = node.type->handle;

			if (node.type->bodyIdx != u64(-1))
			{
				ITrBodySPtr body = m_pMod->GetBody(node.type->bodyIdx);
				for (ITrDefSPtr def : body->defs)
				{
					ITrVisitor::Visit(def);
				}
			}
		}
	}

	void TypeCollectionCommon::Visit(ITrTypealias& node)
	{
		QualNameSPtr qualName = node.qualName;
		if (m_ImplQualName)
		{
			QualNameSPtr subQualName = qualName->GetSubName(m_ImplQualName);
			qualName = QualName::Create(m_TypeQualName, subQualName->AllIdens());
			node.qualName = qualName;
		}

		if (m_InInterface)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AssocType, node.qualName, node.ptr);
			m_Syms.top()->children->AddChild(sym);
		}
		else
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typealias, node.qualName, node.ptr);
			if (!HandleImpls(sym))
				m_Syms.top()->children->AddChild(sym);
		}
	}

	void TypeCollectionCommon::Visit(ITrTypedef& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typedef, node.qualName, node.ptr);
		m_Syms.top()->children->AddChild(sym);
	}

	void TypeCollectionCommon::Visit(ITrFunc& node)
	{
		SymbolKind kind;
		switch (node.funcKind)
		{
		default:
		case ITrFuncKind::Func:
			kind = SymbolKind::Func; break;
		case ITrFuncKind::Method:
		case ITrFuncKind::EmptyMethod:
			kind = SymbolKind::Method; break;
		case ITrFuncKind::Closure:
			kind = SymbolKind::Closure; break;
		}

		QualNameSPtr qualName = node.qualName;
		if (m_ImplQualName)
		{
			QualNameSPtr subQualName = qualName->GetSubName(m_ImplQualName);
			qualName = QualName::Create(m_TypeQualName, subQualName->AllIdens());
			node.qualName = qualName;
		}

		if (!node.params.empty())
		{
			StdVector<StdString> paramNames;
			paramNames.reserve(node.params.size());
			for (ITrParamSPtr param : node.params)
			{
				if (param->label)
					paramNames.push_back(param->label->Name());
				else
					paramNames.push_back(param->iden->Name());
			}

			IdenSPtr newIden = Iden::Create(qualName->Iden()->Name(), qualName->Iden()->Generics(), paramNames);
			qualName = QualName::Create(qualName->Base(), newIden);
		}

		SymbolSPtr sym = CreateSymbol(m_pCtx, kind, qualName, node.ptr);
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);

		node.selfType = m_ImplType;

		if (m_Impl)
			node.impl = m_Impl;
	}

	void TypeCollectionCommon::Visit(ITrVar& node)
	{
		Walk(node);

		SymbolSPtr parent = m_Syms.top();

		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Var, node.qualName, node.ptr);
		sym->type = node.type->handle;

		parent->children->AddChild(sym);
		parent->orderedVarChildren.push_back(sym);
	}

	bool TypeCollectionCommon::HandleImpls(SymbolSPtr sym)
	{
		return false;
	}

	TypeCollection::TypeCollection(Context* pCtx)
		: TypeCollectionCommon("type collection", pCtx)
	{
		m_VisitDefs = true;
	}

	void TypeCollection::Process(ITrModule& mod)
	{
		SetModule(mod);

		Module& activeMod = *m_pCtx->activeModule;
		ModuleSymbolTable& symTable = activeMod.symTable;
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStruct& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Struct, node.qualName, node.ptr);
			symTable.Add(sym);
			m_Syms.push(sym);

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrUnion& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Union, node.qualName, node.ptr);
			symTable.Add(sym);
			m_Syms.push(sym);

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrValEnum& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::ValEnum, node.qualName, node.ptr);
			symTable.Add(sym);
			m_Syms.push(sym);

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrAdtEnum& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AdtEnum, node.qualName, node.ptr);
			symTable.Add(sym);
			m_Syms.push(sym);

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrMarkerInterface& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::MarkerInterface, node.qualName, node.ptr);
			symTable.Add(sym);
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrWeakInterface& node)
		{
			m_InInterface = true;
			
			m_Impl = node.ptr.lock();
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::WeakInterface, node.qualName, node.ptr);
			symTable.Add(sym);
			m_Syms.push(sym);

			m_TypeQualName = node.qualName;
			m_ImplType = sym->type;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();

			m_InInterface = false;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStrongInterface& node)
		{
			m_InInterface = true;
			
			m_Impl = node.ptr.lock();
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::StrongInterface, node.qualName, node.ptr);
			symTable.Add(sym);

			m_InInterface = false;
			
			// Members collected in ImplCollection
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypealias& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typealias, node.qualName, node.ptr);
			symTable.Add(sym);
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypedef& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typedef, node.qualName, node.ptr);
			symTable.Add(sym);
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrFunc& node)
		{
			SymbolKind kind;
			switch (node.funcKind)
			{
			default:
			case ITrFuncKind::Func:
				kind = SymbolKind::Func; break;
			case ITrFuncKind::Method:
			case ITrFuncKind::EmptyMethod:
				kind = SymbolKind::Method; break;
			case ITrFuncKind::Closure:
				kind = SymbolKind::Closure; break;
			}
			
			SymbolSPtr sym = CreateSymbol(m_pCtx, kind, node.qualName, node.ptr);
			symTable.Add(sym);

			SymbolSPtr parent = m_Syms.top();
			if (parent->kind == SymbolKind::StrongInterface)
			{
				sym->interfaces.emplace_back(parent->qualName, parent);
			}

			m_Syms.push(sym);
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}
			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Any, [&, this](ITrImpl& node)
		{
			TypeHandle handle = node.type->handle;
			TypeSPtr type = handle->type;
			SymbolSPtr sym = symTable.Find(type);

			if (!sym)
			{
				if (type->typeKind == TypeKind::Iden)
				{
					sym = CreateSymbol(m_pCtx, SymbolKind::Impl, node.qualName, node.ptr);
					sym->type = handle;
					symTable.Add(sym);
				}
				else
				{
					sym = CreateSymbol(m_pCtx, SymbolKind::Type, nullptr, node.ptr);
					sym->type = handle;
					symTable.Add(sym);
				}
			}
			node.sym = sym;
			
		});
	}

	ImplCollection::ImplCollection(Context* pCtx)
		: TypeCollectionCommon("impl collection", pCtx)
	{
	}

	void ImplCollection::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStrongInterface& node)
		{
			m_Impl = node.ptr.lock();
			m_ImplType = node.sym.lock()->type;
			TypeSPtr type = m_ImplType->type;

			ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

			IdenType& idenType = type->AsIden();
			SymbolSPtr sym = symTable.Find(node.qualName, idenType.qualName);
			
			if (type->typeKind == TypeKind::Iden)
			{
				IdenType& idenType = type->AsIden();
				sym = symTable.Find(node.qualName, idenType.qualName);
				if (sym->qualName->Iden() != idenType.qualName->Iden())
				{
					IdenSPtr iden = Iden::Create(idenType.qualName->Iden()->Name(), sym->qualName->Iden()->Generics());
					SymbolSPtr typeSym = sym->children->FindChild(nullptr, iden);
					if (typeSym)
						sym->type = typeSym->type;
				}
				m_TypeQualName = sym->qualName;
			}
			else
			{
				StdString typeName = m_pCtx->typeReg.ToString(sym->type);
				m_TypeQualName = QualName::Create(Iden::Create(typeName));
			}
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			CollectInterfaces(sym, node.qualName, node.implInterfaces);

			m_InImpl = true;
			m_ImplQualName = node.qualName;
			m_ImplSymbol = sym;
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr subDef : body->defs)
			{
				ITrVisitor::Visit(subDef);
			}
			AddMissingChildrenWithDefImpl();

			m_ImplSymbol = nullptr;
			m_Interfaces.clear();
			m_InImpl = false;

			m_Syms.pop();
			m_Impl = nullptr;
		});
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrImpl& node)
		{
			m_Impl = node.ptr.lock();

			m_ImplType = node.type->handle;
			TypeSPtr type = m_ImplType->type;

			ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

			SymbolSPtr sym = node.sym.lock();
			if (type->typeKind == TypeKind::Iden)
			{
				IdenType& idenType = type->AsIden();
				if (sym->qualName->Iden() != idenType.qualName->Iden())
				{
					SymbolSPtr typeSym = symTable.Find(node.qualName, idenType.qualName);
					if (typeSym)
						sym = typeSym;
				}
				m_TypeQualName = sym->qualName;
			}
			else
			{
				StdString typeName = m_pCtx->typeReg.ToString(sym->type);
				m_TypeQualName = QualName::Create(Iden::Create(typeName));
			}
			
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			CollectInterfaces(sym, node.qualName, node.interface);

			m_InImpl = true;
			m_ImplQualName = node.qualName;
			m_ImplSymbol = sym;
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr subDef : body->defs)
			{
				ITrVisitor::Visit(subDef);
			}
			AddMissingChildrenWithDefImpl();

			m_ImplSymbol = nullptr;
			m_Interfaces.clear();
			m_InImpl = false;

			m_Syms.pop();
			m_Impl = nullptr;

		});
	}

	bool ImplCollection::HandleImpls(SymbolSPtr sym)
	{
		bool res = false;
		for (SymbolSPtr interface : m_Interfaces)
		{
			QualNameSPtr qualName = interface->qualName;
			SymbolSPtr baseInterface = interface->baseVariant.lock();
			SymbolSPtr child;
			if (sym->kind == SymbolKind::Func ||
				sym->kind == SymbolKind::Method)
			{
				IdenSPtr iden = sym->qualName->Iden();
				StdVector<IdenSPtr> idens{ iden };
				child = baseInterface->children->Find(idens, iden->ParamNames());
			}
			else
			{
				StdVector<IdenSPtr> idens{ sym->qualName->Iden() };
				child = baseInterface->children->Find(idens, nullptr);
			}

			if (child)
			{
				StdVector<IdenSPtr> idens{ sym->qualName->Iden() };
				sym->impls.emplace_back(child);
				sym->interfaces.emplace_back(interface->qualName, interface);
				m_ImplSymbol->children->Add(qualName, sym, idens);
				res = true;

				// Remove from needed children
				auto it = m_NeededChildren.find(sym->qualName->Iden());
				if (it != m_NeededChildren.end())
					m_NeededChildren.erase(it);
			}
		}
		return res;
	}

	void ImplCollection::CollectInterfaces(SymbolSPtr sym, QualNameSPtr nodeQualName, const StdPairVector<QualNameSPtr, SpanId>& implInterfaces)
	{
		for (StdPair<QualNameSPtr, SpanId> pair : implInterfaces)
		{
			CollectInterfaces(sym, nodeQualName, pair);
		}
	}

	void ImplCollection::CollectInterfaces(SymbolSPtr sym, QualNameSPtr nodeQualName, const StdPair<QualNameSPtr, SpanId>& implInterface)
	{
		QualNameSPtr interfaceQualName = implInterface.first;

		SymbolSPtr baseInterfaceSym = m_pCtx->activeModule->symTable.Find(nodeQualName, interfaceQualName);
		SymbolSPtr interfaceSym = baseInterfaceSym;
		if (interfaceSym->IsBaseVariant() || interfaceSym->qualName->Iden() != interfaceQualName->Iden())
		{
			interfaceSym = interfaceSym->CreateVariant(interfaceQualName);
		}
		{
			auto it = std::find_if(m_Interfaces.begin(), m_Interfaces.end(), [&interfaceSym](const SymbolSPtr& sym) -> bool
			{
				return sym->qualName == interfaceSym->qualName;
			});
			if (it == m_Interfaces.end())
				m_Interfaces.push_back(interfaceSym);
		}

		sym->interfaces.emplace_back(interfaceSym->qualName, interfaceSym);
		interfaceSym->impls.emplace_back(sym);

		for (StdPair<QualNameSPtr, SymbolWPtr>& subPair : baseInterfaceSym->interfaces)
		{
			CollectInterfaces(subPair, baseInterfaceSym, sym);
		}

		CollectNeededChildren(baseInterfaceSym);
	}

	void ImplCollection::CollectInterfaces(StdPair<QualNameSPtr, SymbolWPtr>& pair, SymbolSPtr baseInterfaceSym, SymbolSPtr sym)
	{
		SymbolSPtr subInterfaceSym = pair.second.lock();

		IdenSPtr iden = pair.first->Iden();
		IdenSPtr parentIden = baseInterfaceSym->qualName->Iden();
		StdVector<IdenGeneric> generics;
		for (IdenGeneric& origGeneric : iden->Generics())
		{
			generics.push_back(GetGeneric(parentIden, origGeneric));
		}

		iden = Iden::Create(iden->Name(), generics);
		QualNameSPtr subInterfaceQualName = QualName::Create(pair.first->Base(), iden);
		
		if (subInterfaceSym->IsBaseVariant() || subInterfaceSym->qualName->Iden() != subInterfaceQualName->Iden())
		{
			subInterfaceSym = subInterfaceSym->CreateVariant(subInterfaceQualName);
		}
		{
			auto it = std::find_if(m_Interfaces.begin(), m_Interfaces.end(), [&subInterfaceSym](const SymbolSPtr& sym) -> bool
			{
				return sym->qualName == subInterfaceSym->qualName;
			});
			if (it == m_Interfaces.end())
				m_Interfaces.push_back(subInterfaceSym);
		}
		sym->interfaces.emplace_back(subInterfaceSym->qualName, subInterfaceSym);
		subInterfaceSym->impls.emplace_back(sym);

		SymbolSPtr baseInterface = subInterfaceSym->baseVariant.lock();
		for (StdPair<QualNameSPtr, SymbolWPtr>& subPair : baseInterface->interfaces)
		{
			CollectInterfaces(subPair, subInterfaceSym, sym);
		}
	}

	void ImplCollection::CollectNeededChildren(SymbolSPtr interface)
	{
		for (StdPair<QualNameSPtr, SymbolWPtr>& pair : interface->interfaces)
		{
			SymbolSPtr subInterface = pair.second.lock();
			CollectNeededChildren(subInterface);
		}

		interface->children->Foreach([&, this](SymbolSPtr sym, QualNameSPtr)
		{
			if (sym->kind != SymbolKind::Method &&
				sym->kind != SymbolKind::AssocType)
				return;
			
			IdenSPtr iden = sym->qualName->Iden();
			auto it = m_NeededChildren.find(iden);

			if (it == m_NeededChildren.end())
			{
				m_NeededChildren.try_emplace(iden, std::pair{ sym, interface });
			}
		});
	}

	void ImplCollection::AddMissingChildrenWithDefImpl()
	{	
		while (!m_NeededChildren.empty())
		{
			auto it = m_NeededChildren.begin();
			StdPair<const IdenSPtr, StdPair<SymbolSPtr, SymbolSPtr>>& pair = *it;
			
			SymbolSPtr sym = pair.second.first;
			SymbolSPtr interface = pair.second.second;
			ITrDefSPtr def = sym->associatedITr.lock();

			if (sym->kind == SymbolKind::Method)
			{
				QualNameSPtr qualName = QualName::Create(m_TypeQualName, pair.first);
				SymbolSPtr parent = sym->parent.lock();

				SymbolSPtr child = CreateSymbol(m_pCtx, sym->kind, qualName);
				child->interfaces.emplace_back(interface->qualName, interface);
				child->isDefaultImpl = true;
				HandleImpls(child);

				ITrFunc& func = static_cast<ITrFunc&>(*def);
				ITrFunc& srcDef = static_cast<ITrFunc&>(*sym->associatedITr.lock());

				StdVector<ITrParamSPtr> params;
				params.reserve(func.params.size());
				for (ITrParamSPtr origParam : func.params)
				{
					TypeHandle handle = origParam->type->handle;
					handle = m_pCtx->typeReg.ReplaceSubType(handle, parent->type, m_ImplSymbol->type);

					ITrTypeSPtr itrType{ new ITrType{ origParam->type->attribs, handle, {} } };
					params.emplace_back(new ITrParam{ origParam->attribs, origParam->label, origParam->iden, itrType });
				}
				
				ITrDefSPtr childDef{ new ITrFunc{ nullptr, nullptr, qualName, std::move(params), func.retType, srcDef.funcKind, false } };
				child->associatedITr = childDef;
				childDef->isDummyDef = true;
				childDef->bodyIdx = srcDef.bodyIdx;
				childDef->sym = child;
				childDef->impl = m_Impl;
				m_pMod->AddDefinition(childDef);
			}
			else if (sym->kind == SymbolKind::AssocType)
			{
				QualNameSPtr qualName = QualName::Create(m_TypeQualName, pair.first);
				SymbolSPtr parent = sym->parent.lock();

				SymbolSPtr child = CreateSymbol(m_pCtx, sym->kind, qualName);
				child->interfaces.emplace_back(interface->qualName, interface);
				child->isDefaultImpl = true;
				child->type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
				HandleImpls(child);

				ITrTypealias& alias = static_cast<ITrTypealias&>(*def);
				ITrTypealias& srcDef = static_cast<ITrTypealias&>(*sym->associatedITr.lock());

				ITrDefSPtr childDef{ new ITrTypealias{ nullptr, nullptr, qualName, alias.type, false } };
				child->associatedITr = childDef;
				childDef->isDummyDef = true;
				childDef->bodyIdx = srcDef.bodyIdx;
				childDef->sym = child;
				childDef->impl = m_Impl;
				m_pMod->AddDefinition(childDef);
			}
			else
			{
				g_ErrorSystem.Error("No implementation of '%s' for ''", sym->qualName->ToString().c_str());
			}
		}
	}

	IdenGeneric ImplCollection::GetGeneric(IdenSPtr parentIden, IdenGeneric origGen)
	{
		for (IdenGeneric& parentGen : parentIden->Generics())
		{
			if (parentGen.isType == origGen.isType)
			{
				if (parentGen.iden == origGen.iden)
					return parentGen;
			}
		}

		return origGen;
	}
}
