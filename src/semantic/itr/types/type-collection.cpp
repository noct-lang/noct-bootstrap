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
		, m_ImplType(TypeHandle(-1))
		, m_InImpl(false)
	{
		m_VisitDefs = true;
	}

	void TypeCollectionCommon::Visit(ITrStruct& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Struct, node.qualName);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrUnion& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Union, node.qualName);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrValEnum& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::ValEnum, node.qualName);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrValEnumMember& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::ValEnumMember, node.qualName);
		m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->type = m_Syms.top()->type;
	}

	void TypeCollectionCommon::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AdtEnum, node.qualName);
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrAdtEnumMember& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AdtEnumMember, node.qualName);
		m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

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

		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typealias, node.qualName);
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

		HandleGenerics(qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;

		m_Syms.pop();
	}

	void TypeCollectionCommon::Visit(ITrTypedef& node)
	{
		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typedef, node.qualName);

		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;

		m_Syms.pop();
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

			IdenSPtr newIden = Iden::Create(qualName->Iden()->Name(), qualName->Iden()->Generics(), m_pCtx->typeReg, paramNames);
			qualName = QualName::Create(qualName->Base(), newIden);
		}

		SymbolSPtr sym = CreateSymbol(m_pCtx, kind, qualName);
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;

		node.selfType = m_ImplType;

		HandleGenerics(qualName, node.genDecl);

		if (m_Impl)
			node.impl = m_Impl;
	}

	void TypeCollectionCommon::Visit(ITrVar& node)
	{
		Walk(node);

		SymbolSPtr parent = m_Syms.top();

		SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Var, node.qualName);
		sym->associatedITr = node.ptr;
		sym->type = node.type->handle;

		parent->children->AddChild(sym);
		parent->orderedVarChildren.push_back(sym);
	}

	void TypeCollectionCommon::HandleGenerics(QualNameSPtr baseQualName, ITrGenDeclSPtr decl)
	{
		if (!decl)
			return;

		StdVector<IdenGeneric> generics = baseQualName->Iden()->Generics();
		for (usize i = 0; i < decl->params.size(); ++i)
		{
			ITrGenParamSPtr param = decl->params[i];
			if (param->isVar)
			{
				ITrGenValParam& valParam = *reinterpret_cast<ITrGenValParam*>(param.get());
				QualNameSPtr qualName = QualName::Create(baseQualName, valParam.iden);

				SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::GenVal, qualName);
				
				m_Syms.top()->children->AddChild(sym);
				param->sym = sym;

				// TODO: type

				if (!generics.empty())
				{
					generics[i].isType = false;
				}
			}
			else
			{
				ITrGenTypeParam& typeParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
				QualNameSPtr qualName = QualName::Create(baseQualName, typeParam.iden);

				SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::GenType, qualName);
				
				if (!m_Syms.empty())
					m_Syms.top()->children->AddChild(sym);
				else
					m_pCtx->activeModule->symTable.Add(sym);
				param->sym = sym;

				StdVector<TypeHandle> typeConstraints;
				for (ITrGenTypeBoundSPtr bound : decl->bounds)
				{
					if (bound->type == typeParam.iden)
						typeConstraints.push_back(bound->bound->handle);
				}
				TypeHandle type = m_pCtx->typeReg.Generic(TypeMod::None, typeParam.iden, typeConstraints);
				sym->type = type;
				GenericType& genType = m_pCtx->typeReg.GetType(type)->AsGeneric();

				if (!generics.empty())
				{
					generics[i].isType = true;
					generics[i].type = type;
					generics[i].iden = qualName->Iden();
				}
			}
		}
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

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrImpl& impl)
		{
			if (impl.genDecl)
				HandleGenerics(impl.qualName, impl.genDecl);
		});

		QualNameSPtr structQualName = QualName::Create({ "core", "marker", "Struct" });
		SymbolSPtr structSym = m_pCtx->activeModule->symTable.Find(nullptr, structQualName);
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStruct& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Struct, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrUnion& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Union, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrValEnum& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::ValEnum, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrAdtEnum& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::AdtEnum, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrMarkerInterface& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::MarkerInterface, node.qualName);
			symTable.Add(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrWeakInterface& node)
		{
			m_Impl = node.ptr.lock();
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::WeakInterface, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			m_TypeQualName = node.qualName;
			m_ImplType = type;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStrongInterface& node)
		{
			m_Impl = node.ptr.lock();
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::StrongInterface, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			m_TypeQualName = node.qualName;
			m_ImplType = type;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypealias& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typealias, node.qualName);
			symTable.Add(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypedef& node)
		{
			SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::Typedef, node.qualName);
			symTable.Add(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;
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
			
			SymbolSPtr sym = CreateSymbol(m_pCtx, kind, node.qualName);
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			HandleGenerics(node.qualName, node.genDecl);

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});
	}

	ImplCollection::ImplCollection(Context* pCtx)
		: TypeCollectionCommon("impl collection", pCtx)
	{
	}

	void ImplCollection::Process(ITrModule& mod)
	{
		SetModule(mod);
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrImpl& node)
		{
			m_Impl = node.ptr.lock();

			m_ImplType = node.type->handle;
			TypeSPtr type = m_pCtx->typeReg.GetType(m_ImplType);

			ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

			SymbolSPtr sym;
			if (type->typeKind == TypeKind::Iden)
			{
				IdenType& idenType = type->AsIden();
				sym = symTable.Find(node.qualName, idenType.qualName);
				if (sym->IsBaseVariant() && sym->qualName->Iden() != idenType.qualName->Iden())
				{
					SymbolSPtr parent = sym;
					QualNameSPtr qualName = QualName::Create(parent->qualName->Base(), idenType.qualName->Iden());
					sym = CreateSymbol(m_pCtx, parent->kind, node.qualName);
					sym->type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
					parent->variants.push_back(sym);
				}
				m_TypeQualName = sym->qualName;
			}
			else
			{
				sym = symTable.Find(type);
				if (!sym)
				{
					sym = CreateSymbol(m_pCtx, SymbolKind::ImplType, node.qualName);
					sym->type = m_ImplType;
					symTable.Add(sym);
				}
				StdString typeName = m_pCtx->typeReg.ToString(sym->type);
				m_TypeQualName = QualName::Create(Iden::Create(typeName));
			}
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			for (StdPair<QualNameSPtr, SpanId> pair : node.interfaces)
			{
				QualNameSPtr interfaceQualName = pair.first;

				SymbolSPtr baseInterfaceSym = symTable.Find(node.qualName, interfaceQualName);
				SymbolSPtr interfaceSym = baseInterfaceSym;
				if (interfaceSym->IsBaseVariant() && interfaceSym->qualName->Iden() != interfaceQualName->Iden())
				{
					interfaceSym = interfaceSym->CreateVariant(interfaceQualName);
				}
				m_Interfaces.push_back(interfaceSym);
				sym->interfaces.emplace_back(interfaceSym->qualName, interfaceSym);
				interfaceSym->impls.emplace_back(sym);

				for (StdPair<QualNameSPtr, SymbolWPtr>& subPair : baseInterfaceSym->interfaces)
				{
					SymbolSPtr subInterfaceSym = subPair.second.lock();
					QualNameSPtr subInterfaceQualName = subPair.first;
					if (subInterfaceSym->IsBaseVariant() && subInterfaceSym->qualName->Iden() != subInterfaceQualName->Iden())
					{
						subInterfaceSym = subInterfaceSym->CreateVariant(subInterfaceQualName);
					}
					m_Interfaces.push_back(subInterfaceSym);
					sym->interfaces.emplace_back(subInterfaceSym->qualName, subInterfaceSym);
					subInterfaceSym->impls.emplace_back(sym);
				}

				CollectNeededChildren(baseInterfaceSym);
			}

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
			if (interface->qualName->Iden()->Name() == "OpPartialOrd")
				__debugbreak();
			
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
				if (child->qualName->Iden()->Name() == "opEq")
					int br = 0;
				
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

	void ImplCollection::CollectNeededChildren(SymbolSPtr interface)
	{
		for (StdPair<QualNameSPtr, SymbolWPtr>& pair : interface->interfaces)
		{
			SymbolSPtr subInterface = pair.second.lock();
			CollectNeededChildren(subInterface);
		}

		interface->children->Foreach([this](SymbolSPtr sym, QualNameSPtr)
		{
			if (sym->kind != SymbolKind::Method &&
				sym->kind != SymbolKind::Typealias)
				return;
			
			IdenSPtr iden = sym->qualName->Iden();
			auto it = m_NeededChildren.find(iden);

			if (it == m_NeededChildren.end())
			{
				m_NeededChildren.try_emplace(iden, sym);
			}
			else if (sym->kind == SymbolKind::Method)
			{
				it->second = sym;
			}
		});
	}

	void ImplCollection::AddMissingChildrenWithDefImpl()
	{	
		while (!m_NeededChildren.empty())
		{
			auto it = m_NeededChildren.begin();
			StdPair<const IdenSPtr, SymbolSPtr>& pair = *it;
			
			SymbolSPtr sym = pair.second;
			ITrDefSPtr def = sym->associatedITr.lock();

			bool found = false;
			if (sym->kind == SymbolKind::Method)
			{
				QualNameSPtr qualName = QualName::Create(m_TypeQualName, pair.first);
				SymbolSPtr parent = sym->parent.lock();

				SymbolSPtr child = CreateSymbol(m_pCtx, sym->kind, qualName);
				child->interfaces.emplace_back(parent->qualName, parent);
				child->isDefaultImpl = true;
				HandleImpls(child);
				found = true;

				ITrFunc& func = static_cast<ITrFunc&>(*def);
				StdVector<ITrParamSPtr> params = func.params;
				ITrDefSPtr childDef{ new ITrFunc{ nullptr, nullptr, qualName, std::move(params), func.retType, ITrFuncKind::Method, false } };
				child->associatedITr = childDef;
				childDef->isDummyDef = true;
				childDef->sym = child;
				m_pMod->AddDefinition(childDef);
			}
			
			if (!found)
			{

				
				g_ErrorSystem.Error("No implementation of '%s' for ''", sym->qualName->ToString().c_str());
			}
		}
	}
}
