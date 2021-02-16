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

			if (sym->qualName->LastIden()->Name() == "OpEq")
				int br = 0;

			StdVector<SymbolSPtr> interfaces;
			for (StdPair<QualNameSPtr, SpanId>& pair : node.implInterfaces)
			{
				IdenSPtr iden = pair.first->LastIden();
				if (!iden->Generics().empty())
				{
					StdVector<IdenGeneric> generics;
					for (IdenGeneric& origGeneric : iden->Generics())
					{
						generics.push_back(GetGeneric(node.genDecl, origGeneric, node.genMapping));
					}
					iden = Iden::Create(iden->Name(), generics);
					pair.first = QualName::Create(pair.first->Base(), iden);
				}
				
				SymbolSPtr interface = symTable.Find(node.qualName, pair.first);
				pair.first = QualName::Create(interface->qualName->Base(), iden);

				SymbolInstSPtr ifaceInst = interface->GetOrCreateInst(pair.first);
				AddUnique(sym->ifaces, SymbolInstWPtr{ ifaceInst });
				
				// Process children
				interface->children->Foreach([&](SymbolSPtr parentIfaceChild, QualNameSPtr)
				{
					SymbolSPtr child = sym->children->FindChild(nullptr, parentIfaceChild->qualName->LastIden());
					if (!child)
					{
						QualNameSPtr childQualName = QualName::Create(sym->qualName, parentIfaceChild->qualName->LastIden());
						child = CreateSymbol(m_pCtx, parentIfaceChild->kind, childQualName);
						sym->children->AddChild(child, pair.first);

						ITrFunc& func = static_cast<ITrFunc&>(*parentIfaceChild->associatedITr.lock());
						StdVector<ITrParamSPtr> params = func.params;
						ITrDefSPtr def{ new ITrFunc{ nullptr, nullptr, childQualName, std::move(params), func.errorType, func.retType, ITrFuncKind::EmptyMethod, false, false, u64(-1), u64(-1) } };
						child->associatedITr = def;
						def->isDummyDef = true;
						def->bodyIdx = parentIfaceChild->associatedITr.lock()->bodyIdx;
						def->sym = child;
						m_pMod->AddDefinition(def);
					}
					AddUnique(child->ifaces, SymbolInstWPtr{ ifaceInst });
				});
			}

		});

		Foreach(ITrVisitorDefKind::Any, [this](ITrImpl& node)
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
							TypeHandle genType = node.genMapping.at(typeParam.iden);

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
										 type.AsGeneric().id == genType.AsGeneric().id)
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
								gen.type = genType;
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

	IdenGeneric InterfaceResolve::GetGeneric(ITrGenDeclSPtr genDecl, IdenGeneric origGeneric, StdUnorderedMap<IdenSPtr, TypeHandle>& genMapping)
	{
		IdenGeneric generic;
		if (origGeneric.isSpecialized)
		{
			// If specialized, check if the type is a generic defined in the generic decl, if so, set the type to a generic
			if (!genDecl)
				return origGeneric;
			
			for (ITrGenParamSPtr genParam : genDecl->params)
			{
				if (origGeneric.isType && genParam->isType)
				{
					ITrGenTypeParam& genTypeParam = static_cast<ITrGenTypeParam&>(*genParam);
					QualNameSPtr qualName = origGeneric.type.AsIden().qualName;
					if (qualName->Depth() != 1)
						continue;
					
					IdenSPtr genIden = origGeneric.type.AsIden().qualName->LastIden();
					TypeHandle genType = genMapping.at(genIden);
					if (genIden == genTypeParam.iden)
					{
						generic.isSpecialized = true;
						generic.isType = true;
						generic.type = genType;
					}
				}
				else
				{
					// TODO
				}
			}
		}
		else
		{
			generic.isSpecialized = true;
			generic.isType = origGeneric.isType;

			IdenSPtr genIden = origGeneric.iden;
			TypeHandle genType = genMapping.at(genIden);
			if (generic.isType)
			{
				generic.type = genType;
			}
			else
			{
				// TODO
			}
		}

		return generic;
	}

	
}
