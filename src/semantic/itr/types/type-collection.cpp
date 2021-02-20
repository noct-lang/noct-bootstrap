#include "type-collection.hpp"
#include "common/context.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"
#include "module/module.hpp"
#include "common/errorsystem.hpp"
#include "common/name-mangling.hpp"

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
			sym->type = node.type->handle;
	}

	void TypeCollectionCommon::Visit(ITrTypealias& node)
	{
		QualNameSPtr qualName = node.qualName;
		if (m_ImplQualName)
		{
			QualNameSPtr subQualName = qualName->GetSubName(m_ImplQualName);
			qualName = m_TypeQualName->Append(subQualName);
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
			qualName = m_TypeQualName->Append(subQualName);
			node.qualName = qualName;
		}

		SymbolSPtr sym = CreateSymbol(m_pCtx, kind, qualName, node.ptr);
		if (!HandleImpls(sym))
			m_Syms.top()->children->AddChild(sym);

		node.selfType = m_ImplType;
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

			if (kind == SymbolKind::Method)
			{
				SymbolSPtr parent = m_Syms.top();
				if (parent->kind == SymbolKind::StrongInterface)
				{
					AddUnique(sym->ifaces, SymbolInstWPtr{ parent->baseInst });
				}
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
			TypeSPtr type = handle.Type();
			SymbolSPtr sym = symTable.Find(type);

			if (!sym)
			{
				sym = CreateSymbol(m_pCtx, SymbolKind::Impl, node.qualName, node.ptr);
				sym->type = handle;
				symTable.Add(sym);
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
			TypeSPtr type = m_ImplType.Type();

			ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;

			IdenType& idenType = type->AsIden();
			SymbolSPtr sym = symTable.Find(node.qualName, idenType.qualName);
			
			if (type->typeKind == TypeKind::Iden)
			{
				IdenType& idenType = type->AsIden();
				sym = symTable.Find(node.qualName, idenType.qualName);
				if (sym->qualName->LastIden() != idenType.qualName->LastIden())
				{
					SymbolSPtr typeSym = sym->children->FindChild(nullptr, idenType.qualName->LastIden());
					if (typeSym)
						sym->type = typeSym->type;
				}
				m_TypeQualName = sym->qualName;
			}
			else
			{
				StdString typeName = m_pCtx->typeReg.ToString(sym->type);
				m_TypeQualName = QualName::Create(typeName);
			}
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			CollectInterfaces(sym, node.qualName, node.implInterfaces);

			m_InImpl = true;
			m_InInterface = true;
			m_ImplQualName = node.qualName;
			m_ImplSymbol = sym;
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}
			AddMissingChildrenWithDefImpl();

			m_ImplSymbol = nullptr;
			m_Interfaces.clear();
			m_InInterface = false;
			m_InImpl = false;

			m_Syms.pop();
			m_Impl = nullptr;
		});
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrImpl& node)
		{
			m_Impl = node.ptr.lock();

			ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;
			SymbolSPtr sym = node.sym.lock();
			sym->type = node.type->handle;
			if (sym->kind == SymbolKind::Impl)
			{
				SymbolSPtr tmp = symTable.Find(sym->type);
				if (!tmp)
				{
					tmp = sym->Copy();
					tmp->kind = SymbolKind::Type;
				}
				node.sym = tmp;
				symTable.Add(tmp);
				sym = tmp;
			}

			m_ImplType = node.type->handle;
			TypeSPtr type = m_ImplType.Type();

			if (type->typeKind == TypeKind::Iden)
			{
				IdenType& idenType = type->AsIden();
				if (sym->qualName->LastIden() != idenType.qualName->LastIden())
				{
					SymbolSPtr typeSym = symTable.Find(node.qualName, idenType.qualName);
					if (typeSym)
						sym = typeSym;
				}
				m_TypeQualName = sym->qualName;
			}
			else
			{
				StdString typeName = NameMangling::Mangle(m_pCtx, sym->type);// m_pCtx->typeReg.ToString(sym->type);
				m_TypeQualName = QualName::Create(typeName);
			}
			
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			if (node.interface.first)
			{
				CollectInterfaces(sym, node.qualName, node.interface);
			}

			m_InImpl = true;
			m_ImplQualName = node.qualName;
			m_ImplSymbol = sym;
			
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
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
		for (SymbolInstSPtr inst : m_Interfaces)
		{
			QualNameSPtr qualName = inst->qualName;
			QualNameSPtr searchQualName = QualName::Create(sym->qualName->LastIden());
			
			SymbolSPtr child = inst->sym.lock()->children->Find(searchQualName, 0, nullptr);
			if (child)
			{
				AddUniquePair(sym->impls, { child }, { inst });
				AddUnique(sym->ifaces, { inst });
				m_ImplSymbol->children->AddChild(sym, qualName);
				res = true;

				// Remove from needed children
				auto it = m_NeededChildren.find(sym->qualName->LastIden());
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
		SymbolSPtr iface = m_pCtx->activeModule->symTable.Find(nodeQualName, implInterface.first);
		QualNameSPtr instQualName = iface->qualName->Base()->AppendLastIden(implInterface.first);
		SymbolInstSPtr inst = iface->GetOrCreateInst(instQualName);

		StdStack<SymbolInstSPtr> toProcess;
		toProcess.push(inst);

		while (!toProcess.empty())
		{
			SymbolInstSPtr tmp = toProcess.top();
			toProcess.pop();

			AddUnique(m_Interfaces, tmp);
			AddUnique(sym->ifaces, { tmp });
			AddUniquePair(tmp->sym.lock()->impls, { sym }, { tmp });

			for (SymbolInstWPtr subIface : tmp->ifaces)
			{
				toProcess.push(subIface.lock());
			}
		}
		
		CollectNeededChildren(inst);
	}

	void ImplCollection::CollectNeededChildren(SymbolInstSPtr ifaceInst)
	{
		for (SymbolInstWPtr& iface : ifaceInst->ifaces)
		{
			CollectNeededChildren(iface.lock());
		}

		ifaceInst->sym.lock()->children->Foreach([&, this](SymbolSPtr sym, QualNameSPtr)
		{
			if (sym->kind != SymbolKind::Method &&
				sym->kind != SymbolKind::AssocType)
				return;
			
			const StdString& iden = sym->qualName->LastIden();
			auto it = m_NeededChildren.find(iden);

			if (it == m_NeededChildren.end())
			{
				m_NeededChildren.try_emplace(iden, std::pair{ sym, ifaceInst });
			}
		});
	}

	void ImplCollection::AddMissingChildrenWithDefImpl()
	{	
		while (!m_NeededChildren.empty())
		{
			auto it = m_NeededChildren.begin();
			StdPair<const StdString, StdPair<SymbolSPtr, SymbolInstSPtr>>& pair = *it;
			
			SymbolSPtr sym = pair.second.first;
			SymbolInstSPtr ifaceInst = pair.second.second;
			ITrDefSPtr def = sym->associatedITr.lock();

			if (sym->kind == SymbolKind::Method)
			{
				StdVector<IdenGeneric> generics;
				if (sym->qualName->Generics().empty() && sym->qualName->Generics().size() > m_TypeQualName->Generics().size())
				{
					usize numGens = sym->qualName->Generics().size() - m_TypeQualName->Generics().size();
					generics.insert(generics.begin(), sym->qualName->Generics().end() - numGens, sym->qualName->Generics().end());
				}
				
				QualNameSPtr qualName = m_TypeQualName->Append(pair.first, generics);
				SymbolSPtr parent = sym->parent.lock();

				SymbolSPtr child = CreateSymbol(m_pCtx, sym->kind, qualName);
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

					ITrTypeSPtr itrType{ new ITrType{ origParam->type->attribs, handle, {}, nullptr, origParam->type->startIdx, origParam->type->endIdx } };
					params.emplace_back(new ITrParam{ origParam->attribs, origParam->iden, itrType, origParam->startIdx, origParam->endIdx });
				}
				
				ITrDefSPtr childDef{ new ITrFunc{ nullptr, nullptr, qualName, std::move(params), func.errorType, func.retType, srcDef.funcKind, srcDef.isUnsafe, false, u64(-1), u64(-1) } };
				child->associatedITr = childDef;
				childDef->isDummyDef = true;
				childDef->bodyIdx = srcDef.bodyIdx;
				childDef->sym = child;
				childDef->impl = m_Impl;
				childDef->genMapping = srcDef.genMapping;
				m_pMod->AddDefinition(childDef);
			}
			else if (sym->kind == SymbolKind::AssocType)
			{
				StdVector<IdenGeneric> generics;
				if (sym->qualName->Generics().empty() && sym->qualName->Generics().size() > m_TypeQualName->Generics().size())
				{
					usize numGens = sym->qualName->Generics().size() - m_TypeQualName->Generics().size();
					generics.insert(generics.begin(), sym->qualName->Generics().end() - numGens, sym->qualName->Generics().end());
				}
				
				QualNameSPtr qualName = m_TypeQualName->Append(pair.first, generics);
				SymbolSPtr parent = sym->parent.lock();

				SymbolSPtr child = CreateSymbol(m_pCtx, sym->kind, qualName);
				child->ifaces.push_back(ifaceInst);
				child->isDefaultImpl = true;
				sym->type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
				HandleImpls(child);

				ITrTypealias& alias = static_cast<ITrTypealias&>(*def);
				ITrTypealias& srcDef = static_cast<ITrTypealias&>(*sym->associatedITr.lock());

				ITrDefSPtr childDef{ new ITrTypealias{ nullptr, nullptr, qualName, alias.type, false, u64(-1), u64(-1) } };
				child->associatedITr = childDef;
				childDef->isDummyDef = true;
				childDef->bodyIdx = srcDef.bodyIdx;
				childDef->sym = child;
				childDef->impl = m_Impl;
				childDef->genMapping = srcDef.genMapping;
				m_pMod->AddDefinition(childDef);
			}
			else
			{
				g_ErrorSystem.Error("No implementation of '%s' for ''", sym->qualName->ToString().c_str());
			}
		}
	}

	GenericTypeCollection::GenericTypeCollection(Context* pCtx)
		: ITrSemanticPass("generic decl resolve", pCtx)
	{
	}

	void GenericTypeCollection::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrStruct& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrUnion& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrAdtEnum& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrWeakInterface& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrStrongInterface& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypedef& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrImpl& node)
		{
			m_Sym = node.sym.lock();

			if (node.genDecl)
			{
				StdVector<TypeHandle> idenTypes = m_pCtx->typeReg.GetSubTypes(node.type->handle, TypeKind::Iden);

				QualNameSPtr ifaceName = node.interface.first;
				if (ifaceName)
				{
					TypeHandle ifaceTypeDummy = m_pCtx->typeReg.Iden(TypeMod::None, ifaceName);
					StdVector<TypeHandle> tmp = m_pCtx->typeReg.GetSubTypes(ifaceTypeDummy, TypeKind::Iden);
					idenTypes.insert(idenTypes.begin(), tmp.begin(), tmp.end());
				}
				
				for (TypeHandle idenType : idenTypes)
				{
					QualNameSPtr qualName = idenType.AsIden().qualName;
					if (!qualName->IsBase())
						continue;

					const StdString& iden = qualName->LastIden();
					for (ITrGenParamSPtr param : node.genDecl->params)
					{
						if (!param->isType)
							continue;

						ITrGenTypeParam& typeParam = static_cast<ITrGenTypeParam&>(*param);
						if (typeParam.iden == iden)
						{
							auto it = node.genMapping.find(iden);
							if (it != node.genMapping.end())
								continue;

							u16 id = u16(node.genMapping.size());
							TypeHandle genType = m_pCtx->typeReg.Generic(TypeMod::None, id);
							node.genMapping.try_emplace(typeParam.iden, genType);
						}
					}
				}
			}
		});


		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

	}

	void GenericTypeCollection::HandleGenerics(QualNameSPtr qualName, ITrGenDeclSPtr decl, ITrDef& def)
	{
		if (def.impl)
			def.genMapping = def.impl->genMapping;

		if (!decl)
			return;
		
		StdVector<IdenGeneric>& generics = qualName->Generics();
		for (usize i = 0; i < decl->params.size(); ++i)
		{
			ITrGenParamSPtr param = decl->params[i];
			if (param->isType)
			{
				ITrGenTypeParam& typeParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
				
				u16 id = u16(def.genMapping.size());
				TypeHandle type = m_pCtx->typeReg.Generic(TypeMod::None, id);
				def.genMapping.try_emplace(typeParam.iden, type);

				if (!generics.empty())
				{
					generics[i].isType = true;
					generics[i].type = type;
					generics[i].iden = typeParam.iden;
				}
			}
			else
			{
				ITrGenValParam& valParam = *reinterpret_cast<ITrGenValParam*>(param.get());

				// TODO: type
				if (!generics.empty())
				{
					generics[i].isType = false;
				}
			}
		}
	}

}
