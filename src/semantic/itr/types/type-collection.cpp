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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;

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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;

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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;

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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;

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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrWeakInterface& node)
		{
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::WeakInterface, node.qualName } };
			symTable.Add(sym);
			m_Syms.push(sym);
			node.sym = sym;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;

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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;

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

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypedef& node)
		{
			SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typedef, node.qualName } };
			symTable.Add(sym);
			node.sym = sym;

			HandleGenerics(node.qualName, node.genDecl);

			TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
			sym->type = type;
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

			HandleGenerics(node.qualName, node.genDecl);

			StdVector<TypeHandle> paramTypes;
			paramTypes.reserve(node.params.size());
			for (ITrParamSPtr param : node.params)
			{
				paramTypes.push_back(param->type->handle);
			}
			TypeHandle type = m_pCtx->typeReg.Func(TypeMod::None, paramTypes, node.retType->handle);
			sym->type = type;

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
					sym->type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
					parent->variants.push_back(sym);
				}
			}
			else
			{
				sym = symTable.Find(type);
				if (!sym)
				{
					sym.reset(new Symbol{ m_pCtx, SymbolKind::ImplType, nullptr });
					sym->type = typeHandle;
					symTable.Add(sym);					
				}
			}
			m_Syms.push(sym);
			node.sym = sym;
			
			if (!node.interfaces.empty())
			{
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
						parent->variants.push_back(interfaceSym);
					}
					m_Interfaces.push_back(interfaceSym);
					sym->impls.push_back(interfaceSym);
					interfaceSym->impls.push_back(sym);
				}
			}
			
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				m_ProcessImplSym = true;
				ITrVisitor::Visit(def);
			}
			
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

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;

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

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;

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

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;

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
		sym->type = m_Syms.top()->type;
	}

	void TypeCollection::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::AdtEnum, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollection::Visit(ITrAdtEnumMember& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::ValEnumMember, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		node.sym = sym;
		sym->type = node.type ? node.type->handle : TypeHandle(-1);
	}

	void TypeCollection::Visit(ITrWeakInterface& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::WeakInterface, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollection::Visit(ITrStrongInterface& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::StrongInterface, node.qualName } };
		m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;

		ITrBodySPtr body = m_pMod->GetBody(node);
		for (ITrDefSPtr def : body->defs)
		{
			ITrVisitor::Visit(def);
		}

		m_Syms.pop();
	}

	void TypeCollection::Visit(ITrTypealias& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typealias, node.qualName } };
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		
		m_Syms.pop();
	}

	void TypeCollection::Visit(ITrTypedef& node)
	{
		SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::Typealias, node.qualName } };

		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		m_Syms.push(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, node.qualName);
		sym->type = type;
		
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

		SymbolSPtr sym{ new Symbol{ m_pCtx, kind, node.qualName } };
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);
		node.sym = sym;

		HandleGenerics(node.qualName, node.genDecl);

		StdVector<TypeHandle> paramTypes;
		paramTypes.reserve(node.params.size());
		for (ITrParamSPtr param : node.params)
		{
			paramTypes.push_back(param->type->handle);
		}
		TypeHandle type = m_pCtx->typeReg.Func(TypeMod::None, paramTypes, node.retType->handle);
		sym->type = type;
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
				ITrGenValParam& valParam = *reinterpret_cast<ITrGenValParam*>(param.get());
				QualNameSPtr qualName = QualName::Create(baseQualName, valParam.iden);

				SymbolSPtr sym{ new Symbol{ m_pCtx, SymbolKind::GenVal, qualName } };
				m_Syms.top()->children->AddChild(sym);
				param->sym = sym;

				TypeHandle type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
				sym->type = type;

				generics[i].isType = true;
				generics[i].type = type;
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
			SymbolSPtr child;
			if (sym->kind == SymbolKind::Func ||
				sym->kind == SymbolKind::Method)
			{
				IdenSPtr iden = interface->qualName->Iden();
				StdVector<IdenSPtr> idens{ iden };
				child = interface->children->Find(idens, iden->ParamNames());
			}
			else
			{
				StdVector<IdenSPtr> idens{ interface->qualName->Iden() };
				child = interface->children->Find(idens);
			}

			if (child)
			{
				StdVector<IdenSPtr> idens{ sym->qualName->Iden() };
				m_Syms.top()->children->Add(interface->qualName, sym, idens);
				res = true;
			}
		}
		return res;
	}
}
