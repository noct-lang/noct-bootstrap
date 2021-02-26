#include "type-collection.hpp"
#include "common/context.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"
#include "module/module.hpp"
#include "common/errorsystem.hpp"
#include "common/name-mangling.hpp"

namespace Noctis
{
	TypeCollectionCommon::TypeCollectionCommon(const char* name)
		: ITrSemanticPass(name)
	{
		m_VisitDefs = true;
	}

	void TypeCollectionCommon::Visit(ITrStruct& node)
	{
		SymbolSPtr sym = CreateSymbol(SymbolKind::Struct, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);
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
		SymbolSPtr sym = CreateSymbol(SymbolKind::Union, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);
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
		SymbolSPtr sym = CreateSymbol(SymbolKind::ValEnum, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);
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
		SymbolSPtr sym = CreateSymbol(SymbolKind::ValEnumMember, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);
	}

	void TypeCollectionCommon::Visit(ITrAdtEnum& node)
	{
		SymbolSPtr sym = CreateSymbol(SymbolKind::AdtEnum, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);
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
		SymbolSPtr sym = CreateSymbol(SymbolKind::AdtEnumMember, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);

		if (node.type)
			sym->type = node.type->handle;
	}

	void TypeCollectionCommon::Visit(ITrTypealias& node)
	{
		if (m_ImplQualName)
		{
			QualNameSPtr subQualName = node.qualName->GetSubName(m_ImplQualName);
			node.qualName = m_TypeQualName->Append(subQualName);
		}
		if (m_Syms.top()->IsInterface())
		{
			SymbolSPtr sym = CreateSymbol(SymbolKind::AssocType, node.qualName, node.ptr);
			m_Syms.top()->children->Add(sym);
		}
		else
		{
			SymbolSPtr sym = CreateSymbol(SymbolKind::Typealias, node.qualName, node.ptr);
			if (!HandleImpls(sym))
				m_Syms.top()->children->Add(sym);
		}
	}

	void TypeCollectionCommon::Visit(ITrTypedef& node)
	{
		SymbolSPtr sym = CreateSymbol(SymbolKind::Typedef, node.qualName, node.ptr);
		m_Syms.top()->children->Add(sym);
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

		if (m_ImplQualName)
		{
			QualNameSPtr subQualName = node.qualName->GetSubName(m_ImplQualName);
			node.qualName = m_TypeQualName->Append(subQualName);
		}

		SymbolSPtr sym = CreateSymbol(kind, node.qualName, node.ptr);
		if (!HandleImpls(sym))
			m_Syms.top()->children->Add(sym);

		node.selfType = m_ImplType;
	}

	void TypeCollectionCommon::Visit(ITrVar& node)
	{
		Walk(node);

		SymbolSPtr sym = CreateSymbol(SymbolKind::Var, node.qualName, node.ptr);
		sym->type = node.type->handle;

		SymbolSPtr parent = m_Syms.top();
		parent->children->Add(sym);
		parent->orderedVarChildren.push_back(sym);
	}

	bool TypeCollectionCommon::HandleImpls(SymbolSPtr sym)
	{
		return false;
	}

	TypeCollection::TypeCollection()
		: TypeCollectionCommon("type collection")
	{
		m_VisitDefs = true;
	}

	void TypeCollection::Process(ITrModule& mod)
	{
		SetModule(mod);

		Module& activeMod = *g_Ctx.activeModule;
		ModuleSymbolTable& symTable = activeMod.symTable;
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStruct& node)
		{
			SymbolSPtr sym = CreateSymbol(SymbolKind::Struct, node.qualName, node.ptr);
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
			SymbolSPtr sym = CreateSymbol(SymbolKind::Union, node.qualName, node.ptr);
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
			SymbolSPtr sym = CreateSymbol(SymbolKind::ValEnum, node.qualName, node.ptr);
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
			SymbolSPtr sym = CreateSymbol(SymbolKind::AdtEnum, node.qualName, node.ptr);
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
			SymbolSPtr sym = CreateSymbol(SymbolKind::MarkerInterface, node.qualName, node.ptr);
			symTable.Add(sym);
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrWeakInterface& node)
		{
			SymbolSPtr sym = CreateSymbol(SymbolKind::WeakInterface, node.qualName, node.ptr);
			symTable.Add(sym);
			m_Syms.push(sym);

			m_Impl = node.ptr.lock();
			m_TypeQualName = node.qualName;
			m_ImplType = sym->type;

			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}

			m_Syms.pop();
		});

		// Members collected in ImplCollection
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStrongInterface& node)
		{
			m_Impl = node.ptr.lock();
			SymbolSPtr sym = CreateSymbol(SymbolKind::StrongInterface, node.qualName, node.ptr);
			symTable.Add(sym);
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypealias& node)
		{
			SymbolSPtr sym = CreateSymbol(SymbolKind::Typealias, node.qualName, node.ptr);
			symTable.Add(sym);
		});

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrTypedef& node)
		{
			SymbolSPtr sym = CreateSymbol(SymbolKind::Typedef, node.qualName, node.ptr);
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
			
			SymbolSPtr sym = CreateSymbol(kind, node.qualName, node.ptr);
			symTable.Add(sym);

			if (kind == SymbolKind::Method && m_Syms.top()->kind == SymbolKind::StrongInterface)
				AddUnique(sym->ifaces, SymbolInstWPtr{ m_Syms.top()->baseInst });

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
			SymbolSPtr sym = symTable.Find(handle.Type());
			if (!sym)
			{
				sym = CreateSymbol(SymbolKind::Impl, node.qualName, node.ptr);
				sym->type = handle;
				symTable.Add(sym);
			}
			node.sym = sym;
		});
	}

	ImplCollection::ImplCollection()
		: TypeCollectionCommon("impl collection")
	{
	}

	void ImplCollection::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Module, [&, this](ITrStrongInterface& node)
		{
			m_Impl = node.ptr.lock();
			m_ImplType = node.sym.lock()->type;

			ModuleSymbolTable& symTable = g_Ctx.activeModule->symTable;

			IdenType& idenType = m_ImplType.AsIden();
			SymbolSPtr sym = symTable.Find(node.qualName, idenType.qualName);
			
			if (sym->qualName->LastIden() != idenType.qualName->LastIden())
			{
				SymbolSPtr typeSym = sym->children->FindChild(nullptr, idenType.qualName->LastIden());
				if (typeSym)
					sym->type = typeSym->type;
			}
			m_TypeQualName = sym->qualName;
			
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			for (StdPair<QualNameSPtr, u64>& implInterface : node.implInterfaces)
			{
				CollectInterfaces(sym, node.qualName, implInterface.first);
			}

			m_ImplQualName = node.qualName;
			m_ImplSymbol = sym;
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}
			AddMissingChildrenWithDefImpl();

			m_Interfaces.clear();
			m_Syms.pop();
			
		});
		m_ImplSymbol = nullptr;
		
		Foreach(ITrVisitorDefKind::Module, [&, this](ITrImpl& node)
		{
			m_Impl = node.ptr.lock();

			ModuleSymbolTable& symTable = g_Ctx.activeModule->symTable;
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
			if (m_ImplType.Kind() == TypeKind::Iden)
			{
				IdenType& idenType = m_ImplType.AsIden();
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
				StdString typeName = NameMangling::Mangle(sym->type);
				m_TypeQualName = QualName::Create(typeName);
			}
			
			m_Syms.push(sym);
			node.sym = sym;
			sym->associatedITr = node.ptr;

			if (node.interface.first)
				CollectInterfaces(sym, node.qualName, node.interface.first);

			m_ImplQualName = node.qualName;
			m_ImplSymbol = sym;
			
			ITrBodySPtr body = mod.GetBody(node);
			for (ITrDefSPtr def : body->defs)
			{
				ITrVisitor::Visit(def);
			}
			AddMissingChildrenWithDefImpl();
			
			m_Interfaces.clear();
			m_Syms.pop();
		});
	}

	bool ImplCollection::HandleImpls(SymbolSPtr sym)
	{
		bool res = false;
		for (SymbolInstSPtr inst : m_Interfaces)
		{
			QualNameSPtr searchQualName = QualName::Create(sym->qualName->LastIden());	
			SymbolSPtr child = inst->sym.lock()->children->Find(searchQualName, 0, nullptr);
			if (child)
			{
				AddUniquePair(sym->impls, { child }, { inst });
				AddUnique(sym->ifaces, { inst });
				m_ImplSymbol->children->Add(sym, inst->qualName);
				res = true;

				// Remove from needed children
				auto it = m_NeededChildren.find(sym->qualName->LastIden());
				if (it != m_NeededChildren.end())
					m_NeededChildren.erase(it);
			}
		}
		return res;
	}

	void ImplCollection::CollectInterfaces(SymbolSPtr sym, QualNameSPtr nodeQualName, QualNameSPtr implIfaceQualName)
	{
		SymbolSPtr iface = g_Ctx.activeModule->symTable.Find(nodeQualName, implIfaceQualName);
		QualNameSPtr instQualName = iface->qualName->Base()->AppendLastIden(implIfaceQualName);
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
			m_NeededChildren.try_emplace(iden, std::pair{ sym, ifaceInst });
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
					generics.assign(sym->qualName->Generics().end() - numGens, sym->qualName->Generics().end());
				}
				
				QualNameSPtr qualName = m_TypeQualName->Append(pair.first, generics);
				SymbolSPtr parent = sym->parent.lock();

				SymbolSPtr child = CreateSymbol(sym->kind, qualName);
				child->isDefaultImpl = true;
				HandleImpls(child);

				ITrFunc& func = static_cast<ITrFunc&>(*def);
				ITrFunc& srcDef = static_cast<ITrFunc&>(*sym->associatedITr.lock());

				StdVector<ITrParamSPtr> params;
				params.reserve(func.params.size());
				for (ITrParamSPtr origParam : func.params)
				{
					TypeHandle handle = origParam->type->handle;
					handle = g_TypeReg.ReplaceSubType(handle, parent->type, m_ImplSymbol->type);

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

				SymbolSPtr child = CreateSymbol(sym->kind, qualName);
				child->ifaces.push_back(ifaceInst);
				child->isDefaultImpl = true;
				sym->type = g_TypeReg.Iden(TypeMod::None, qualName);
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

	GenericTypeCollection::GenericTypeCollection()
		: ITrSemanticPass("generic decl resolve")
	{
	}

	void GenericTypeCollection::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrStruct& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrUnion& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrAdtEnum& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrWeakInterface& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrStrongInterface& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypedef& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrImpl& node)
		{
			if (node.genDecl)
			{
				StdVector<TypeHandle> idenTypes = g_TypeReg.GetSubTypes(node.type->handle, TypeKind::Iden);

				QualNameSPtr ifaceName = node.interface.first;
				if (ifaceName)
				{
					TypeHandle ifaceTypeDummy = g_TypeReg.Iden(TypeMod::None, ifaceName);
					StdVector<TypeHandle> tmp = g_TypeReg.GetSubTypes(ifaceTypeDummy, TypeKind::Iden);
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
							TypeHandle genType = g_TypeReg.Generic(TypeMod::None, id);
							node.genMapping.try_emplace(typeParam.iden, genType);
						}
					}
				}
			}
		});


		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
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
				TypeHandle type = generics[i].type;
				def.genMapping.try_emplace(typeParam.iden, type);
			}
			else
			{
				ITrGenValParam& valParam = *reinterpret_cast<ITrGenValParam*>(param.get());
				// TODO
			}
		}
	}

}
