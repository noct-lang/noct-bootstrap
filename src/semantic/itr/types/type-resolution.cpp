#include "type-resolution.hpp"


#include "common/context.hpp"
#include "common/type.hpp"
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
				SymbolSPtr interface = symTable.Find(node.qualName, pair.first);
				interfaces.push_back(interface);
			}

			for (usize i = 0; i < interfaces.size(); ++i)
			{
				SymbolSPtr interface = interfaces[i];
				for (StdPair<QualNameSPtr, SymbolWPtr> pair : interface->interfaces)
				{
					SymbolSPtr tmp = pair.second.lock();
					auto it = std::find(interfaces.begin(), interfaces.end(), tmp);
					if (it != interfaces.end())
						interfaces.push_back(tmp);
				}
			}
			
			for (SymbolSPtr interface : interfaces)
			{
				// Update qualname to connect it to the interface that implements it

				IdenSPtr iden = interface->qualName->Iden();
				StdVector<IdenGeneric> generics;
				for (IdenGeneric& origGeneric : iden->Generics())
				{
					if (origGeneric.isSpecialized)
					{
						if (node.genDecl)
						{
							bool found = false;
							for (ITrGenParamSPtr genParam : node.genDecl->params)
							{
								if (origGeneric.isType && !genParam->isVar)
								{
									ITrGenTypeParam& genTypeParam = static_cast<ITrGenTypeParam&>(*genParam);
									
									TypeSPtr origType = m_pCtx->typeReg.GetType(origGeneric.type);
									IdenSPtr origIden = origType->AsIden().qualName->Iden();

									if (origIden == genTypeParam.iden)
									{
										StdVector<TypeHandle> typeConstraints;
										for (ITrGenTypeBoundSPtr bound : node.genDecl->bounds)
										{
											if (bound->type == origIden)
												typeConstraints.push_back(bound->bound->handle);
										}
										
										IdenGeneric generic;
										generic.isSpecialized = true;
										generic.isType = true;
										generic.type = m_pCtx->typeReg.Generic(TypeMod::None, origIden, typeConstraints);
										generics.push_back(generic);
										found = true;
										break;
									}
								}
								else
								{
									// TODO
								}
							}

							if (found)
								continue;
							
						}

						
						generics.push_back(origGeneric);
						
					}
					else
					{
						IdenGeneric generic;
						generic.isSpecialized = true;
						generic.isType = origGeneric.isType;

						IdenSPtr genIden = generic.iden;
						if (generic.isType)
						{
							generic.type = m_pCtx->typeReg.Generic(TypeMod::None, genIden, origGeneric.typeConstraints);
						}
						else
						{
							// TODO
						}

						generics.push_back(generic);
					}
				}

				iden = Iden::Create(iden->Name(), generics, m_pCtx->typeReg);
				QualNameSPtr qualName = QualName::Create(interface->qualName->Base(), iden);
				SymbolSPtr parentIface = symTable.Find(node.qualName, qualName);
				
				sym->interfaces.emplace_back(qualName, parentIface);

				// Process children

				parentIface->children->Foreach([&](SymbolSPtr parentIfaceChild, QualNameSPtr)
				{
					SymbolSPtr child = sym->children->FindChild(nullptr, parentIfaceChild->qualName->Iden());
					if (!child)
					{
						QualNameSPtr childQualName = QualName::Create(sym->qualName, parentIfaceChild->qualName->Iden());
						child = CreateSymbol(m_pCtx, parentIfaceChild->kind, childQualName);
						
						sym->children->AddChild(qualName, child);

						ITrFunc& func = static_cast<ITrFunc&>(*parentIfaceChild->associatedITr.lock());
						StdVector<ITrParamSPtr> params = func.params;
						ITrDefSPtr def{ new ITrFunc{ nullptr, nullptr, childQualName, std::move(params), func.retType, ITrFuncKind::EmptyMethod, false } };
						child->associatedITr = def;
						def->isDummyDef = true;
						def->sym = child;
						m_pMod->AddDefinition(def);
					}
					child->interfaces.emplace_back(parentIface->qualName, parentIface);
				});
			}
			
		});
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
