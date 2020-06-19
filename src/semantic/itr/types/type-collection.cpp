#include "type-collection.hpp"
#include "common/context.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"
#include "module/module.hpp"

namespace Noctis
{
	TypeCollection::TypeCollection(Context* pCtx)
		: ITrSemanticPass("type collection", pCtx)
		, m_ProcessImplSym(false)
		, m_InImpl(false)
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
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Struct, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

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
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Union, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

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
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::ValEnum, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

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
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::AdtEnum, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

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
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::MarkerInterface, node.qualName } };
			symTable.Add(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrWeakInterface& node)
		{
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::WeakInterface, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			m_TypeQualName = node.qualName;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStrongInterface& node)
		{
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::StrongInterface, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;

			m_TypeQualName = node.qualName;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypealias& node)
		{
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typealias, node.qualName } };
			symTable.Add(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
			IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
			idenType.sym = sym;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypedef& node)
		{
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typedef, node.qualName } };
			symTable.Add(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

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
			
			SymbolSPtr sym{ new Symbol{ m_pCtx, kind, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;
			sym->baseVariant = sym;
			sym->SetSelf(sym);

			HandleGenerics(node.qualName, node.genDecl);

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		Foreach(ITrVisitorDefKind::Any, [&, this](ITrImpl& node)
		{
			TypeHandle typeHandle = node.type->handle;
			TypeSPtr type = m_pCtx->typeReg.GetType(typeHandle);
			
			SymbolSPtr sym;
			if (type->typeKind == TypeKind::Iden)
			{
				IdenType& idenType = type->AsIden();
				sym = symTable.Find(node.qualName, idenType.qualName);
				if (sym->IsBaseVariant() && sym->qualName->Iden() != idenType.qualName->Iden())
				{
					SymbolSPtr parent = sym;
					QualNameSPtr qualName = QualName::Create(parent->qualName->Base(), idenType.qualName->Iden());
					sym.reset(new Symbol{ m_pCtx, parent->kind, qualName });
					sym->baseVariant = sym;
					sym->SetSelf(sym);
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
					sym.reset(new Symbol{ m_pCtx, SymbolKind::ImplType, nullptr });
					sym->baseVariant = sym;
					sym->SetSelf(sym);
					sym->type = typeHandle;
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
				SymbolSPtr interfaceSym = symTable.Find(node.qualName, interfaceQualName);
				if (interfaceSym->IsBaseVariant() && interfaceSym->qualName->Iden() != interfaceQualName->Iden())
				{
					SymbolSPtr parent = interfaceSym;
					QualNameSPtr qualName = QualName::Create(parent->qualName->Base(), interfaceQualName->Iden());
					interfaceSym.reset(new Symbol{ m_pCtx, parent->kind, qualName });
					interfaceSym->type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
					interfaceSym->baseVariant = parent;
					interfaceSym->self = interfaceSym;
					parent->variants.push_back(interfaceSym);
				}
				m_Interfaces.push_back(interfaceSym);
				sym->impls.emplace_back(interfaceSym, false);
				interfaceSym->impls.emplace_back(sym, false);
			}


			m_InImpl = true;
			m_ImplQualName = node.qualName;
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				m_ProcessImplSym = true;
				ITrVisitor::Visit(def);
			}
			m_InImpl = false;
			
			m_Syms.pop();
			m_Interfaces.clear();
		});
		
	}

	void TypeCollection::Visit(ITrStruct& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Struct, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

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

	void TypeCollection::Visit(ITrUnion& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Union, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

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

	void TypeCollection::Visit(ITrValEnum& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::ValEnum, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

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

	void TypeCollection::Visit(ITrValEnumMember& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::ValEnumMember, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);
		sym->type = m_Syms.top()->type;
	}

	void TypeCollection::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::AdtEnum, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

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

	void TypeCollection::Visit(ITrAdtEnumMember& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::AdtEnumMember, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

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

	void TypeCollection::Visit(ITrTypealias& node)
	{
		QualNameSPtr qualName = node.qualName;
		if (m_ImplQualName)
		{
			QualNameSPtr subQualName = qualName->GetSubName(m_ImplQualName);
			qualName = QualName::Create(m_TypeQualName, subQualName->AllIdens());
			node.qualName = qualName;
		}
		
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typealias, qualName } };
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

		HandleGenerics(qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;
		
		m_Syms.pop();
	}

	void TypeCollection::Visit(ITrTypedef& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typealias, node.qualName } };

		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
		idenType.sym = sym;
		
		m_Syms.pop();
	}

	void TypeCollection::Visit(ITrFunc& node)
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

		SymbolSPtr sym{ new Symbol{ m_pCtx, kind, qualName } };
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);

		if (m_TypeQualName)
		{
			TypeHandle selfType = m_pCtx->typeReg.Iden(TypeMod::None, m_TypeQualName);
			node.selfType = selfType;
		}

		HandleGenerics(qualName, node.genDecl);
	}

	void TypeCollection::Visit(ITrVar& node)
	{
		Walk(node);
		
		SymbolSPtr parent = m_Syms.top();

		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Var, node.qualName } };
		sym->associatedITr = node.ptr;
		sym->baseVariant = sym;
		sym->SetSelf(sym);
		sym->type = node.type->handle;

		parent->children->AddChild(sym);
		parent->orderedVarChildren.push_back(sym);
	}

	void TypeCollection::HandleGenerics(QualNameSPtr baseQualName, ITrGenDeclSPtr decl)
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
				
				SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::GenVal, qualName } };
				m_Syms.top()->children->AddChild(sym);
				param->sym = sym;

				// TODO: type

				generics[i].isType = false;
			}
			else
			{
				ITrGenTypeParam& typeParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
				QualNameSPtr qualName = QualName::Create(baseQualName, typeParam.iden);

				SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::GenVal, qualName } };
				m_Syms.top()->children->AddChild(sym);
				param->sym = sym;

				TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
				sym->type = type;
				IdenType& idenType = m_pCtx->typeReg.GetType(type)->AsIden();
				idenType.sym = sym;

				generics[i].isType = true;
				generics[i].type = type;
				generics[i].iden = qualName->Iden();
			}
		}
	}

	bool TypeCollection::HandleImpls(SymbolSPtr sym)
	{
		if (!m_ProcessImplSym) 
			return false;
		
		m_ProcessImplSym = false;
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
				sym->impls.emplace_back(child, false);
				sym->interface = interface;
				m_Syms.top()->children->Add(qualName, sym, idens);
				res = true;
			}
		}
		return res;
	}
}
