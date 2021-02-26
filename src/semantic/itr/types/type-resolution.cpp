#include "type-resolution.hpp"


#include "common/context.hpp"
#include "common/type.hpp"
#include "common/utils.hpp"
#include "itr/itr.hpp"
#include "module/symbol.hpp"
#include "module/module.hpp"

namespace Noctis
{
	TypealiasReplacing::TypealiasReplacing()
		: ITrSemanticPass("typealias replacement")
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
			g_TypeReg.SetAliasType(alias, handle);
		});
	}

	InterfaceResolve::InterfaceResolve()
		: ITrSemanticPass("interface resolve pass")
	{
		m_VisitDefs = true;
	}

	void InterfaceResolve::Process(ITrModule& mod)
	{
		SetModule(mod);

		ModuleSymbolTable& symTable = g_Ctx.activeModule->symTable;
		
		Foreach(ITrVisitorDefKind::Any, [&, this](ITrStrongInterface& node)
		{
			if (node.implInterfaces.empty())
				return;
			
			SymbolSPtr sym = node.sym.lock();
			for (StdPair<QualNameSPtr, SpanId>& pair : node.implInterfaces)
			{
				if (!pair.first->Generics().empty())
				{
					StdVector<IdenGeneric> generics;
					generics.reserve(pair.first->Generics().size());
					for (IdenGeneric& origGeneric : pair.first->Generics())
					{
						generics.push_back(GetGeneric(node.genDecl, origGeneric, node.genMapping));
					}
					pair.first = pair.first->WithGenerics(generics);
				}
				
				SymbolSPtr interface = symTable.Find(node.qualName, pair.first);
				pair.first = interface->qualName->Base()->AppendLastIden(pair.first);

				SymbolInstSPtr ifaceInst = interface->GetOrCreateInst(pair.first);
				AddUnique(sym->ifaces, SymbolInstWPtr{ ifaceInst });
				
				// Process children
				interface->children->Foreach([&](SymbolSPtr parentIfaceChild, QualNameSPtr)
				{
					SymbolSPtr child = sym->children->FindChild(nullptr, parentIfaceChild->qualName->LastIden());
					if (!child)
					{
						QualNameSPtr childQualName = sym->qualName->Append(parentIfaceChild->qualName->LastIden());
						child = CreateSymbol(parentIfaceChild->kind, childQualName);
						sym->children->Add(child, pair.first);

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
			if (!ifaceQualName ||
				ifaceQualName->Generics().empty() ||
				!node.genDecl)
				return;

			StdVector<IdenGeneric> gens;
			for (IdenGeneric gen : ifaceQualName->Generics())
			{
				if (!gen.isType)
					continue;
				
				for (ITrGenParamSPtr param : node.genDecl->params)
				{
					if (!param->isType)
						continue;
					
					ITrGenTypeParam& typeParam = static_cast<ITrGenTypeParam&>(*param);
					TypeHandle genType = node.genMapping.at(typeParam.iden);

					bool found = gen.iden == typeParam.iden;
					if (!found)
					{
						TypeHandle type = gen.type;
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
						gen.isSpecialized = true;
						gen.type = genType;
						break;
					}
				}
				
				gens.push_back(gen);
			}
			node.interface.first = ifaceQualName->WithGenerics(gens);
		});
	}

	IdenGeneric InterfaceResolve::GetGeneric(ITrGenDeclSPtr genDecl, IdenGeneric generic, StdUnorderedMap<StdString, TypeHandle>& genMapping)
	{
		if (generic.isSpecialized)
		{
			// If specialized, check if the type is a generic defined in the generic decl, if so, set the type to a generic
			if (!genDecl)
				return generic;
			
			for (ITrGenParamSPtr genParam : genDecl->params)
			{
				if (generic.isType && genParam->isType)
				{
					ITrGenTypeParam& genTypeParam = static_cast<ITrGenTypeParam&>(*genParam);
					QualNameSPtr qualName = generic.type.AsIden().qualName;
					if (!qualName->IsBase())
						continue;
					
					const StdString& genIden = generic.type.AsIden().qualName->LastIden();
					TypeHandle genType = genMapping.at(genIden);
					if (genIden == genTypeParam.iden)
						generic.type = genType;
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
			if (generic.isType)
			{
				generic.type = genMapping.at(generic.iden);
			}
			else
			{
				// TODO
			}
		}

		return generic;
	}
	
}
