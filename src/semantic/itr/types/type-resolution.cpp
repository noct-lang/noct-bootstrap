#include "type-resolution.hpp"


#include "common/context.hpp"
#include "common/type.hpp"
#include "common/utils.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"
#include "module/module.hpp"

namespace Noctis
{
	TypealiasReplacing::TypealiasReplacing(Context* pCtx)
		: ITrSemanticPass("typealias replacement", pCtx)
	{
	}

	void TypealiasReplacing::Process(ITrModule& mod)
	{
		SetModule(mod);

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			if (!node.type)
				return;
			TypeHandle alias = node.sym.lock()->type;
			TypeHandle handle = node.type->handle;
			m_pCtx->typeReg.SetAliasType(alias, handle);
		});
	}

	GenericDeclResolve::GenericDeclResolve(Context* pCtx)
		: ITrSemanticPass("generic decl resolve", pCtx)
	{
	}

	void GenericDeclResolve::Process(ITrModule& mod)
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

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypealias& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrTypedef& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrFunc& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrImpl& node)
		{
			m_Sym = node.sym.lock();
			HandleGenerics(node.qualName, node.genDecl, node);
		});
		
	}

	void GenericDeclResolve::HandleGenerics(QualNameSPtr qualName, ITrGenDeclSPtr decl, ITrDef& def)
	{
		if (!decl)
			return;

		StdVector<IdenGeneric>& generics = qualName->LastIden()->Generics();
		for (usize i = 0; i < decl->params.size(); ++i)
		{
			ITrGenParamSPtr param = decl->params[i];
			if (param->isType)
			{
				ITrGenTypeParam& typeParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
				QualNameSPtr genQualName = QualName::Create(qualName, typeParam.iden);

				SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::GenType, genQualName);

				m_Sym->children->AddChild(sym);
				param->sym = sym;

				StdVector<TypeHandle> typeConstraints;
				StdVector<ITrGenTypeBoundSPtr> toReplaceTypes;
				for (ITrGenTypeBoundSPtr bound : decl->bounds)
				{
					TypeHandle type = bound->type->handle;
					IdenSPtr typeIden = type.AsIden().qualName->LastIden();
					if (typeIden == typeParam.iden)
					{
						toReplaceTypes.push_back(bound);
						
						if (bound->bound->type->handle.Type()->typeKind == TypeKind::Compound)
						{
							CompoundType& compound = bound->bound->type->handle.AsCompound();
							for (TypeHandle subType : compound.subTypes)
							{
								typeConstraints.push_back(subType);
							}
						}
						else
						{
							typeConstraints.push_back(bound->bound->type->handle);
						}
					}

					// TODO: non-generic bound
				}
				TypeHandle type = m_pCtx->typeReg.Generic(TypeMod::None, typeParam.iden, typeConstraints);
				sym->SetType(type);

				if (!generics.empty())
				{
					generics[i].isType = true;
					generics[i].type = type;
					generics[i].iden = typeParam.iden;
				}

				for (ITrGenTypeBoundSPtr toReplace : toReplaceTypes)
				{
					toReplace->type->handle = type;
				}
			}
			else
			{
				ITrGenValParam& valParam = *reinterpret_cast<ITrGenValParam*>(param.get());
				QualNameSPtr genQualName = QualName::Create(qualName, valParam.iden);

				SymbolSPtr sym = CreateSymbol(m_pCtx, SymbolKind::GenVal, genQualName);

				m_Sym->children->AddChild(sym);
				param->sym = sym;

				// TODO: type
				sym->SetType(valParam.type->handle);

				if (!generics.empty())
				{
					generics[i].isType = false;
				}
			}
		}
	}

	InterfaceResolve::InterfaceResolve(Context* pCtx)
		: ITrSemanticPass("interface resolve pass", pCtx)
	{
		m_VisitDefs = true;
	}

	void InterfaceResolve::Process(ITrModule& mod)
	{
		SetModule(mod);

		ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;
		
		Foreach(ITrVisitorDefKind::Any, [&, this](ITrStrongInterface& node)
		{
			if (node.implInterfaces.empty())
				return;
			
			SymbolSPtr sym = node.sym.lock();

			StdVector<SymbolSPtr> interfaces;
			for (StdPair<QualNameSPtr, SpanId>& pair : node.implInterfaces)
			{
				IdenSPtr iden = pair.first->LastIden();
				if (!iden->Generics().empty())
				{
					StdVector<IdenGeneric> generics;
					for (IdenGeneric& origGeneric : iden->Generics())
					{
						generics.push_back(GetGeneric(node.genDecl, origGeneric));
					}
					iden = Iden::Create(iden->Name(), generics);
					pair.first = QualName::Create(pair.first->Base(), iden);
				}
				
				SymbolSPtr interface = symTable.Find(node.qualName, pair.first);
				pair.first = QualName::Create(interface->qualName->Base(), iden);

				SymbolSPtr parentIFace = symTable.Find(node.qualName, pair.first);
				parentIFace = parentIFace->GetOrCreateVariant(pair.first);
				AddUniquePair(sym->interfaces, pair.first, SymbolWPtr{ parentIFace });

				// Process children

				parentIFace->children->Foreach([&](SymbolSPtr parentIfaceChild, QualNameSPtr)
				{
					SymbolSPtr defChild = sym->children->FindChild(nullptr, parentIfaceChild->qualName->LastIden());
					SymbolSPtr child = defChild;
					if (!defChild)
					{
						QualNameSPtr childQualName = QualName::Create(sym->qualName, parentIfaceChild->qualName->LastIden());
						child = CreateSymbol(m_pCtx, parentIfaceChild->kind, childQualName);

						sym->children->AddChild(child, pair.first);

						ITrFunc& func = static_cast<ITrFunc&>(*parentIfaceChild->associatedITr.lock());
						StdVector<ITrParamSPtr> params = func.params;
						ITrDefSPtr def{ new ITrFunc{ nullptr, nullptr, childQualName, std::move(params), func.retType, ITrFuncKind::EmptyMethod, false, u64(-1), u64(-1) } };
						child->associatedITr = def;
						def->isDummyDef = true;
						def->bodyIdx = defChild->associatedITr.lock()->bodyIdx;
						def->sym = child;
						m_pMod->AddDefinition(def);
					}
					AddUniquePair(child->interfaces, parentIFace->qualName, SymbolWPtr{ parentIFace });
				});
			}

		});

		Foreach(ITrVisitorDefKind::Any, [](ITrImpl& node)
		{
			QualNameSPtr ifaceQualName = node.interface.first;
			if (!ifaceQualName)
				return;

			IdenSPtr iden = ifaceQualName->LastIden();
			if (iden->Generics().empty())
				return;
			if (!node.genDecl)
				return;

			StdVector<IdenGeneric> gens;
			StdVector<ITrGenParamSPtr>& availableGenerics = node.genDecl->params;
			for (IdenGeneric& origGen : iden->Generics())
			{
				if (origGen.isType)
				{
					bool found = false;
					for (ITrGenParamSPtr param : availableGenerics)
					{
						if (param->isType)
						{
							ITrGenTypeParam& typeParam = static_cast<ITrGenTypeParam&>(*param);

							found = origGen.iden == typeParam.iden;
							if (!found)
							{
								TypeHandle type = origGen.type;
								if (type.Kind() == TypeKind::Iden &&
									type.AsIden().qualName->IsBase() &&
									type.AsIden().qualName->LastIden() == typeParam.iden)
								{
									found = true;
								}
								else if (type.Kind() == TypeKind::Generic &&
										 type.AsGeneric().iden == typeParam.iden)
								{
									found = true;
								}
							}
							
							if (found)
							{
								IdenGeneric gen;
								gen.isType = true;
								gen.isSpecialized = true;
								gen.iden = origGen.iden;
								gen.type = typeParam.sym.lock()->type;
								gens.push_back(gen);
								break;
							}
						}
					}

					if (!found)
						gens.push_back(origGen);
					
				}
				else
				{
					gens.push_back(origGen);
				}
			}

			iden = Iden::Create(iden->Name(), gens);
			node.interface.first = QualName::Create(ifaceQualName->Base(), iden);
		});
	}

	IdenGeneric InterfaceResolve::GetGeneric(ITrGenDeclSPtr genDecl, IdenGeneric origGeneric)
	{
		IdenGeneric generic;
		if (origGeneric.isSpecialized)
		{
			if (genDecl)
			{
				bool found = false;
				for (ITrGenParamSPtr genParam : genDecl->params)
				{
					if (origGeneric.isType && genParam->isType)
					{
						ITrGenTypeParam& genTypeParam = static_cast<ITrGenTypeParam&>(*genParam);

						IdenSPtr origIden = origGeneric.type.AsIden().qualName->LastIden();

						if (origIden == genTypeParam.iden)
						{
							StdVector<TypeHandle> typeConstraints;
							for (ITrGenTypeBoundSPtr bound : genDecl->bounds)
							{
								TypeHandle handle = bound->type->handle;
								if (handle.Type()->typeKind != TypeKind::Generic)
									continue;
								
								IdenSPtr iden = handle.AsGeneric().iden;

								if (iden == origIden)
									typeConstraints.push_back(bound->bound->type->handle);
							}
							
							generic.isSpecialized = true;
							generic.isType = true;
							generic.type = m_pCtx->typeReg.Generic(TypeMod::None, origIden, typeConstraints);
						}
					}
					else
					{
						// TODO
					}
				}

			}

		}
		else
		{
			generic.isSpecialized = true;
			generic.isType = origGeneric.isType;

			IdenSPtr genIden = origGeneric.iden;
			if (generic.isType)
			{
				generic.type = m_pCtx->typeReg.Generic(TypeMod::None, genIden, origGeneric.typeConstraints);
			}
			else
			{
				// TODO
			}
		}

		return generic;
	}

	CompilerImplPass::CompilerImplPass(Context* pCtx)
		: ITrSemanticPass("compiler implementation pass", pCtx)
	{
	}

	void CompilerImplPass::Process(ITrModule& mod)
	{
		SetModule(mod);

		ModuleSymbolTable& symTable = m_pCtx->activeModule->symTable;
		
		QualNameSPtr structMarkerQualName = QualName::Create({ "core", "marker", "Struct" });
		SymbolSPtr structMarkerSym = symTable.Find(nullptr, structMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrStruct& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (structMarkerSym)
				sym->markers.push_back(structMarkerSym);
		});
		
		QualNameSPtr unionMarkerQualName = QualName::Create({ "core", "marker", "Union" });
		SymbolSPtr unionMarkerSym = symTable.Find(nullptr, unionMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrUnion& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (unionMarkerQualName)
				sym->markers.push_back(unionMarkerSym);
		});
		
		QualNameSPtr valEnumMarkerQualName = QualName::Create({ "core", "marker", "ValEnum" });
		SymbolSPtr valEnumMarkerSym = symTable.Find(nullptr, valEnumMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrValEnum& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (valEnumMarkerSym)
				sym->markers.push_back(valEnumMarkerSym);
		});
		
		QualNameSPtr adtEnumMarkerQualName = QualName::Create({ "core", "marker", "AdtEnum" });
		SymbolSPtr adtEnumMarkerSym = symTable.Find(nullptr, adtEnumMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrAdtEnum& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (adtEnumMarkerSym)
				sym->markers.push_back(adtEnumMarkerSym);
		});
		
		QualNameSPtr strongInterfaceMarkerQualName = QualName::Create({ "core", "marker", "StrongInterface" });
		SymbolSPtr strongInterfaceMarkerSym = symTable.Find(nullptr, strongInterfaceMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrStrongInterface& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (strongInterfaceMarkerSym)
				sym->markers.push_back(strongInterfaceMarkerSym);
		});
		
		QualNameSPtr weakInterfaceMarkerQualName = QualName::Create({ "core", "marker", "WeakInterface" });
		SymbolSPtr  weakInterfaceMarkerSym = symTable.Find(nullptr, weakInterfaceMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrWeakInterface& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (weakInterfaceMarkerSym)
				sym->markers.push_back(weakInterfaceMarkerSym);
		});
		
		QualNameSPtr markerInterfaceMarkerQualName = QualName::Create({ "core", "marker", "MarkerInterface" });
		SymbolSPtr markerInterfaceMarkerSym = symTable.Find(nullptr, markerInterfaceMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrMarkerInterface& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (markerInterfaceMarkerSym)
				sym->markers.push_back(markerInterfaceMarkerSym);
		});

		QualNameSPtr typealiasMarkerQualName = QualName::Create({ "core", "marker", "MarkerInterface" });
		SymbolSPtr typealiasMarkerSym = symTable.Find(nullptr, typealiasMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrTypealias& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (typealiasMarkerSym)
				sym->markers.push_back(typealiasMarkerSym);
		});

		QualNameSPtr typedefMarkerQualName = QualName::Create({ "core", "marker", "MarkerInterface" });
		SymbolSPtr typedefInterfaceMarkerSym = symTable.Find(nullptr, typedefMarkerQualName);

		Foreach(ITrVisitorDefKind::Any, [&](ITrTypedef& node)
		{
			SymbolSPtr sym = node.sym.lock();

			if (typedefInterfaceMarkerSym)
				sym->markers.push_back(typedefInterfaceMarkerSym);
		});


		
	}
}
