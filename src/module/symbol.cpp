#include "symbol.hpp"

#include <cassert>


#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/logger.hpp"
#include "common/name-mangling.hpp"
#include "common/qualname.hpp"
#include "module.hpp"

namespace Noctis
{
	SymbolInst::SymbolInst(SymbolSPtr sym, QualNameSPtr qualName)
		: sym(sym)
		, qualName(qualName)
	{
		// TODO: type
	}

	TypeHandle SymbolInst::SelfType()
	{
		SymbolSPtr sym = this->sym.lock();
		switch (sym->kind)
		{
		case SymbolKind::ValEnum:
		case SymbolKind::AdtEnum:
		case SymbolKind::Typedef:
		case SymbolKind::Typealias:
			return g_TypeReg.Iden(TypeMod::None, qualName);
		case SymbolKind::ValEnumMember:
		case SymbolKind::AdtEnumMember:
			return g_TypeReg.Iden(TypeMod::None, qualName->Base(usize(-1)));
		default:
			return type;
		}
	}

	Symbol::Symbol(SymbolKind kind, QualNameSPtr qualName)
		: qualName(qualName)
		, children(new SymbolSubTable{})
		, funcDefaultStart(u16(-1))
		, size(0)
		, aligment(0)
		, offset(0)
		, kind(kind)
		, isImported(false)
		, isDefaultImpl(false)
		, comptimeOnly(false)
		, valGenSolveNeeded(false)
		, dependsOnValueGenerics(false)
		, attribs()
		, vis()
		, defImplVer(0)
	{
	}

	void Symbol::SetSelf(SymbolWPtr self)
	{
		this->self = self;
		children->SetParent(self);
	}

	TypeHandle Symbol::SelfType()
	{
		switch (kind)
		{
		case SymbolKind::ValEnum:
		case SymbolKind::AdtEnum:
		case SymbolKind::Typedef:
		case SymbolKind::Typealias:
			return g_TypeReg.Iden(TypeMod::None, qualName);
		case SymbolKind::ValEnumMember:
		case SymbolKind::AdtEnumMember:
			return g_TypeReg.Iden(TypeMod::None, qualName->Base(usize(-1)));
		default: return type;
		}
	}

	void Symbol::CalculateSizeAlignOffset()
	{
		if (size != 0)
			return;

		switch (kind)
		{
		case SymbolKind::Struct:
		{
			for (SymbolWPtr symW : orderedVarChildren)
			{
				SymbolSPtr sym = symW.lock();

				sym->CalculateSizeAlignOffset();
				sym->offset = size;

				if (aligment < sym->aligment)
					aligment = sym->aligment;

				u64 alignMask = sym->aligment - 1;
				u16 alignOffset = u16(size & alignMask);
				size += alignOffset == 0 ? 0 : sym->aligment - alignOffset;
				size += sym->size;
			}
			break;
		}
		case SymbolKind::Union:
		{
			for (SymbolWPtr symW : orderedVarChildren) 
			{
				SymbolSPtr sym = symW.lock();

				sym->CalculateSizeAlignOffset();
				sym->offset = 0;

				if (aligment < sym->aligment)
					aligment = sym->aligment;
				if (size < sym->size)
					size = sym->size;
			}
			break;
		}
		case SymbolKind::ValEnum: break; // TODO
		case SymbolKind::ValEnumMember: break; // TODO
		case SymbolKind::AdtEnum: break; // TODO
		case SymbolKind::AdtEnumMember: break; // TODO
		case SymbolKind::MarkerInterface: break; // TODO
		case SymbolKind::WeakInterface: break; // TODO
		case SymbolKind::StrongInterface: break; // TODO
		case SymbolKind::Typealias: break; // TODO
		case SymbolKind::Typedef: break; // TODO
		case SymbolKind::Func: break; // TODO
		case SymbolKind::Method: break; // TODO
		case SymbolKind::Closure: break; // TODO
		case SymbolKind::Var:
		{
			TypeSPtr actType = type.Type();
			if (!actType->Size())
				actType->CalculateSizeAlign();
			
			size = actType->Size();
			aligment = actType->Align();
			break;
		}
		default: ;
		}
		
		
	}

	SymbolInstSPtr Symbol::GetInst(QualNameSPtr qualName)
	{
		if (baseInst->qualName->Generics().empty() || baseInst->qualName == qualName)
			return baseInst;
		
		for (SymbolInstSPtr instantiation : instantiations)
		{
			if (instantiation->qualName == qualName)
				return instantiation;
		}
		return nullptr;
	}

	// Helper
	IdenGeneric GetGenericForInst(const IdenGeneric& origIdenGen, const StdVector<IdenGeneric>& baseIdenGens, const StdVector<IdenGeneric>& instIdenGens)
	{
		IdenGeneric idenGen = origIdenGen;

		StdVector<IdenGeneric> idenGens;
		for (usize i = 0; i < baseIdenGens.size(); ++i)
		{
			const IdenGeneric& baseIdenGen = baseIdenGens[i];
			if (origIdenGen.isType != baseIdenGen.isType)
				continue;

			if (origIdenGen.isType && baseIdenGen.type.Kind() == TypeKind::Generic)
			{
				idenGen.isSpecialized = true;
				idenGen.type = instIdenGens[i].type;
				break;
			}
			else
			{
				// TODO
			}
		}

		return idenGen;
	}

	SymbolInstSPtr Symbol::GetOrCreateInst(QualNameSPtr qualName)
	{
		SymbolInstSPtr inst = GetInst(qualName);
		if (inst)
			return inst;

		inst.reset(new SymbolInst{ self.lock(), qualName });

		if (type.Kind() == TypeKind::Iden &&
			type.AsIden().qualName->Idens() == qualName->Idens())
		{
			inst->type = g_TypeReg.Iden(TypeMod::None, qualName);
		}
		else
		{
			inst->type = type;
		}

		SymbolSPtr parent = this->parent.lock();
		if (parent)
			inst->parent = parent->GetOrCreateInst(qualName->Base(u16(parent->qualName->Generics().size())));
		
		if (IsInterface() && !ifaces.empty())
		{
			for (SymbolInstWPtr ifaceW : ifaces)
			{
				SymbolInstSPtr iface = ifaceW.lock();
				SymbolInstSPtr ifaceInst = iface;
				if (!iface->qualName->Generics().empty())
				{
					const StdVector<IdenGeneric>& ifaceGenerics = iface->qualName->Generics();
					const StdVector<IdenGeneric>& symIdenGens = this->qualName->Generics();
					const StdVector<IdenGeneric>& instIdenGens = qualName->Generics();
					
					// TODO value-generics
					StdVector<IdenGeneric> idenGens;
					for (const IdenGeneric& ifaceIdenGen : ifaceGenerics)
					{
						idenGens.push_back(GetGenericForInst(ifaceIdenGen, symIdenGens, instIdenGens));
					}

					QualNameSPtr newQualName = iface->qualName->Base()->Append(iface->qualName->LastIden(), idenGens);
					if (newQualName != iface->qualName)
					{
						ifaceInst = iface->sym.lock()->GetOrCreateInst(newQualName);
					}
				}
				inst->ifaces.push_back(ifaceInst);
			}
		}

		instantiations.push_back(inst);
		return inst;
	}

	void Symbol::AddInstForType(TypeHandle type, SymbolInstSPtr inst)
	{
		if (!type.IsValid())
			return;
		
		TypeSPtr tmp = type.Type();
		auto it = instsForTypes.find(tmp);
		if (it == instsForTypes.end())
			it = instsForTypes.try_emplace(tmp, StdVector<SymbolInstWPtr>{}).first;

		AddUnique(it->second, SymbolInstWPtr{ inst });
	}

	const StdVector<SymbolInstWPtr>& Symbol::GetInstForType(TypeHandle type)
	{
		static StdVector<SymbolInstWPtr> empty;

		auto it = instsForTypes.find(type.Type());
		if (it != instsForTypes.end())
			return it->second;

		return empty;
	}

	void Symbol::Log(u8 indent, bool includeImports)
	{
		if (isImported && !includeImports)
			return;
		
		StdString name = qualName ? qualName->ToString() : g_TypeReg.ToString(type);
		StdStringView kindName;
		bool isInterface = false;
		switch (kind)
		{
		case SymbolKind::Struct: kindName = "struct"; break;
		case SymbolKind::Union: kindName = "union"; break;
		case SymbolKind::ValEnum: kindName = "val-enum"; break;
		case SymbolKind::ValEnumMember: kindName = "val-enum-member"; break;
		case SymbolKind::AdtEnum: kindName = "adt-enum"; break;
		case SymbolKind::AdtEnumMember: kindName = "adt-enum-member"; break;
		case SymbolKind::MarkerInterface: kindName = "marker-interface"; isInterface = true; break;
		case SymbolKind::WeakInterface: kindName = "weak-interface"; isInterface = true; break;
		case SymbolKind::StrongInterface: kindName = "strong-interface"; isInterface = true; break;
		case SymbolKind::Typealias: kindName = "typealias"; break;
		case SymbolKind::Typedef: kindName = "typedef"; break;
		case SymbolKind::Func: kindName = "func"; break;
		case SymbolKind::Method: kindName = "method"; break;
		case SymbolKind::Var: kindName = "var"; break;
		case SymbolKind::Impl: kindName = "impl"; break;
		case SymbolKind::Type: kindName = "type"; break;
		case SymbolKind::GenVal: kindName = "gen-val"; break;
		default: ;
		}

		auto printIndent = [](u8 indent)
		{
			for (u8 i = 1; i < indent; ++i)
				g_Logger.Log(" |");
			g_Logger.Log(" +");
		};

		StdString flags;
		if (isImported)
			flags += ", imported";
		if (isDefaultImpl)
			flags += ", def_impl";
		if (comptimeOnly)
			flags += ", comptime";
		if (dependsOnValueGenerics)
			flags += ", val_gen_depend";

		printIndent(indent);
		StdString typeName = g_TypeReg.ToString(type);
		g_Logger.Log("(symbol '%s', kind='%s', type='%s'%s)\n", name.c_str(), kindName.data(), typeName.c_str(), flags.c_str());

		if (!children->Empty())
		{
			printIndent(indent + 1);
			g_Logger.Log("(children)\n");
			children->Log(indent + 1, includeImports);
		}

		if (!instantiations.empty())
		{
			printIndent(indent + 1);
			g_Logger.Log("(instantiations)\n");
			for (SymbolInstSPtr inst : instantiations)
			{
				printIndent(indent + 2);
				StdString instName = inst->qualName->ToString();
				g_Logger.Log("(inst '%s')\n", instName.c_str());
			}
		}

		for (StdPair<SymbolWPtr, SymbolInstWPtr> pair : impls)
		{
			SymbolSPtr implSym = pair.first.lock();
			if (implSym->kind != SymbolKind::Type && !implSym->qualName->IsSubnameOf(g_Ctx.activeModule->qualName))
				continue;

			//if (pair.second)
			//	continue;

			printIndent(indent + 1);
			StdString tmp = implSym->kind == SymbolKind::Type ? g_TypeReg.ToString(implSym->type) : implSym->qualName->ToString();
			g_Logger.Log(isInterface ? "(implemented by '%s')\n" : "(implements '%s')\n", tmp.c_str());
		}
	}

	bool Symbol::HasMarker(QualNameSPtr name)
	{
		for (SymbolWPtr marker : markers)
		{
			if (marker.lock()->qualName == name)
				return true;
		}
		return false;
	}

	SymbolSPtr Symbol::Copy()
	{
		assert(kind == SymbolKind::Impl);
		
		SymbolSPtr res{ new Symbol{ kind, qualName } };
		children->Foreach([&res](SymbolSPtr child, QualNameSPtr interfaceName)
		{
			res->children->Add(child, interfaceName);
		});

		res->qualName = qualName;
		res->parent = parent;
		res->markers = markers;
		res->type = type;
		res->associatedITr = associatedITr;
		res->SetSelf(res);

		res->impls.insert(res->impls.end(), impls.begin(), impls.end());
		res->ifaces.insert(res->ifaces.end(), ifaces.begin(), ifaces.end());
		res->markers.insert(res->markers.end(), markers.begin(), markers.end());

		return res;
	}

	SymbolSPtr CreateSymbol(SymbolKind kind, QualNameSPtr qualName)
	{
		SymbolSPtr sym{ new Symbol{ kind, qualName } };
		sym->SetSelf(sym);

		switch (kind)
		{
		case SymbolKind::Struct:
		case SymbolKind::Union:
		case SymbolKind::ValEnum:
		case SymbolKind::AdtEnum:
		case SymbolKind::MarkerInterface:
		case SymbolKind::WeakInterface:
		case SymbolKind::StrongInterface:
		case SymbolKind::Typealias:
		case SymbolKind::Typedef:
		case SymbolKind::AssocType:
		case SymbolKind::ValEnumMember:
		case SymbolKind::AdtEnumMember:
		{
			TypeHandle type = g_TypeReg.Iden(TypeMod::None, qualName);
			sym->type = type;
			g_TypeReg.SetIdenSym(type.AsIden().qualName, sym);
			break;
		}
		case SymbolKind::Func:
		case SymbolKind::Method:
		case SymbolKind::Closure:
		case SymbolKind::Var:
		case SymbolKind::Impl:
		case SymbolKind::Type:
		case SymbolKind::GenVal:
		default: ;
		}

		sym->baseInst.reset(new SymbolInst{ sym, qualName });
		sym->baseInst->type = sym->type;

		return sym;
	}

	SymbolSPtr CreateSymbol(SymbolKind kind, QualNameSPtr qualName, ITrDefWPtr node)
	{
		SymbolSPtr sym = CreateSymbol(kind, qualName);
		sym->associatedITr = node;

		ITrDefSPtr nodeSPtr = node.lock();
		nodeSPtr->sym = sym;
		if (nodeSPtr->attribs)
		{
			sym->vis = nodeSPtr->attribs->vis;
			sym->attribs = nodeSPtr->attribs->attribs;
		}
		return sym;
	}

	void SymbolSubTable::SetParent(SymbolWPtr parent)
	{
		m_Parent = parent;
	}

	bool SymbolSubTable::Add(SymbolSPtr sym, QualNameSPtr interfaceQualName)
	{
		sym->parent = m_Parent;
		if (interfaceQualName)
		{
			// Further overlapping is processed later on, after generic value parameters and types are processed
			for (StdPair<const QualNameSPtr, StdUnorderedMap<StdString, SymbolSPtr>>& pair : m_ImplSubtables)
			{
				if (pair.first == interfaceQualName)
					return pair.second.try_emplace(sym->qualName->LastIden(), sym).second;
			}

			auto it = m_ImplSubtables.try_emplace(interfaceQualName, StdUnorderedMap<StdString, SymbolSPtr>{}).first;
			return it->second.try_emplace(sym->qualName->LastIden(), sym).second;
		}
		else
		{
			return m_Symbols.try_emplace(sym->qualName->LastIden(), sym).second;
		}
	}

	SymbolSPtr SymbolSubTable::Find(QualNameSPtr qualName, usize idenIdx, QualNameSPtr interfaceName)
	{
		if (interfaceName)
		{
			auto it = m_ImplSubtables.find(interfaceName);
			if (it == m_ImplSubtables.end())
				return nullptr;

			auto subIt = it->second.find(qualName->LastIden());
			if (subIt == it->second.end())
				return nullptr;
			return subIt->second;
		}

		auto symIt = m_Symbols.find(qualName->LastIden());
		if (symIt != m_Symbols.end())
			return symIt->second;

		SymbolSPtr sym;
		for (StdPair<const QualNameSPtr, StdUnorderedMap<StdString, SymbolSPtr>>& implSymbol : m_ImplSubtables)
		{
			auto it = implSymbol.second.find(qualName->LastIden());
			if (it != implSymbol.second.end())
			{
				SymbolSPtr tmp = it->second;
				if (sym && sym != tmp)
				{
					// TODO: what symbol is ambiguous?
					g_ErrorSystem.Error("ambiguous symbol\n");
					return nullptr;
				}
				sym = tmp;
			}
		}
		return sym;
	}

	SymbolSPtr SymbolSubTable::FindChild(QualNameSPtr implQualName, const StdString& iden)
	{
		if (implQualName)
		{
			auto it = m_ImplSubtables.find(implQualName);
			if (it == m_ImplSubtables.end())
				return nullptr;

			auto subIt = it->second.find(iden);
			if (subIt == it->second.end())
				return nullptr;
			return subIt->second;
		}

		auto it = m_Symbols.find(iden);
		if (it != m_Symbols.end())
			return it->second;
		return nullptr;
	}

	void SymbolSubTable::Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda)
	{
		for (StdPair<const StdString, SymbolSPtr>& pair : m_Symbols)
		{
			lambda(pair.second, nullptr);
		}
		
		for (StdPair<const QualNameSPtr, StdUnorderedMap<StdString, SymbolSPtr>>& pair : m_ImplSubtables)
		{
			for (StdPair<const StdString, SymbolSPtr>& subPair : pair.second)
			{
				lambda(subPair.second, pair.first);
			}
		}
	}

	void SymbolSubTable::Merge(SymbolSubTableSPtr from)
	{
		for (StdPair<const StdString, SymbolSPtr>& pair : from->m_Symbols)
		{
			m_Symbols.try_emplace(pair.first, pair.second);
		}

		for (StdPair<const QualNameSPtr, StdUnorderedMap<StdString, SymbolSPtr>>& fromPair : from->m_ImplSubtables)
		{
			auto it = m_ImplSubtables.find(fromPair.first);
			if (it == m_ImplSubtables.end())
				it = m_ImplSubtables.try_emplace(fromPair.first, StdUnorderedMap<StdString, SymbolSPtr>{}).first;

			for (StdPair<const StdString, SymbolSPtr>& fromImpl : fromPair.second)
			{
				it->second.try_emplace(fromImpl.first, fromImpl.second);
			}
		}
	}

	bool SymbolSubTable::Empty() const
	{
		return m_Symbols.empty() && m_ImplSubtables.empty();
	}

	void SymbolSubTable::UpdateImplSubTableKey(QualNameSPtr oldKey, QualNameSPtr newKey)
	{
		if (oldKey == newKey)
			return;
		
		auto it = m_ImplSubtables.find(oldKey);
		if (it == m_ImplSubtables.end())
			return;

		StdUnorderedMap<StdString, SymbolSPtr> tmp = std::move(it->second);
		m_ImplSubtables.erase(it);
		m_ImplSubtables.try_emplace(newKey, std::move(tmp));
	}

	void SymbolSubTable::Log(u8 indent, bool includeImports)
	{
		for (StdPair<const StdString, SymbolSPtr>& pair : m_Symbols)
		{
			pair.second->Log(indent, includeImports);
		}
		
		for (StdPair<const QualNameSPtr, StdUnorderedMap<StdString, SymbolSPtr>>& pair : m_ImplSubtables)
		{
			for (u8 i = 1; i < indent; ++i)
				g_Logger.Log(" |");
			StdString implName = pair.first->ToString();
			g_Logger.Log(" +(impl for '%s')\n", implName.c_str());

			for (StdPair<const StdString, SymbolSPtr>& subPair : pair.second)
			{
				subPair.second->Log(indent + 1, includeImports);
			}
		}
	}

	ModuleSymbolTable::ModuleSymbolTable(QualNameSPtr scope)
		: m_ScopedTable(new ScopedSymbolTable{})
		, m_ModScope(scope)
	{
	}

	bool ModuleSymbolTable::Add(SymbolSPtr sym)
	{
		if (sym->kind == SymbolKind::Type)
		{

			
			TypeSPtr type = sym->type.Type();
			auto it = m_TypeSymbols.find(type);
			if (it != m_TypeSymbols.end())
				return false;

			m_TypeSymbols.try_emplace(type, sym);
			StdString typeName = NameMangling::Mangle(type);
			m_TypeNameSymbols.try_emplace(typeName, sym);
			return true;
		}
		else
		{
			return m_ScopedTable->Add(sym, 0);
		}
	}

	SymbolSPtr ModuleSymbolTable::Find(QualNameSPtr scope, QualNameSPtr qualname, QualNameSPtr interfaceQualName)
	{
		StdVector<QualNameSPtr> possibleQualNames;
		StdVector<StdString> idens = qualname->Idens();
		StdVector<StdString> scopeIdens;
		if (scope)
			scopeIdens = scope->Idens();

		while (!scopeIdens.empty())
		{
			QualNameSPtr baseName = QualName::Create(scopeIdens);
			possibleQualNames.push_back(baseName->Append(idens));
			scopeIdens.pop_back();
		}
		possibleQualNames.push_back(qualname);

		possibleQualNames.push_back(m_ModScope->Append(idens));

		for (QualNameSPtr baseName : m_ImportedModuleNames)
		{
			possibleQualNames.push_back(baseName->Append(idens));
		}

		for (QualNameSPtr possibleQualName : possibleQualNames)
		{
			SymbolSPtr sym;
			if (interfaceQualName)
				sym = FindWithInterface(possibleQualName, interfaceQualName);
			else
				sym = Find(possibleQualName);
			if (sym)
				return sym;
		}

		return nullptr;
	}

	SymbolSPtr ModuleSymbolTable::Find(QualNameSPtr qualName)
	{
		if (qualName->Disambiguation())
			return FindWithDisambiguation(qualName);
		
		const StdString& firstIden = qualName->Idens().front();

		auto it = m_TypeNameSymbols.find(firstIden);
		if (it != m_TypeNameSymbols.end())
		{
			if (qualName->Idens().size() == 1)
				return it->second;

			SymbolSPtr sym = it->second->children->Find(qualName, 1, nullptr);
			if (sym)
				return sym;
			return nullptr;
		}

		SymbolSPtr sym = m_ScopedTable->Find(qualName, 0);
		if (sym)
			return sym;

		return nullptr;
	}

	SymbolSPtr ModuleSymbolTable::Find(TypeHandle type)
	{
		return Find(type.Type());
	}

	SymbolSPtr ModuleSymbolTable::Find(TypeSPtr type)
	{
		auto it = m_TypeSymbols.find(type);
		if (it != m_TypeSymbols.end())
			return it->second;
		return nullptr;
	}

	SymbolSPtr ModuleSymbolTable::FindWithInterface(QualNameSPtr qualName, QualNameSPtr interfaceQualName)
	{		
		const StdVector<StdString>& idens = qualName->Idens();

		if (idens.size() == 1)
			return Find(qualName);
		
		for (usize i = 1; i < idens.size(); ++i)
		{
			StdVector<StdString> tmpIdens;
			tmpIdens.assign(idens.begin(), idens.begin() + i);
			QualNameSPtr baseQualName = QualName::Create(tmpIdens);

			SymbolSPtr sym = Find(baseQualName);
			if (sym)
				return sym->children->Find(qualName, i, interfaceQualName);
		}
		return nullptr;
	}

	SymbolSPtr ModuleSymbolTable::FindWithDisambiguation(QualNameSPtr qualName)
	{
		TypeDisambiguationSPtr disambig = qualName->Disambiguation();
		TypeHandle type = disambig->Type();

		StdVector<StdString> tmpIdens;
		tmpIdens.assign(disambig->IfaceQualName()->Idens().begin(), disambig->IfaceQualName()->Idens().end());

		SymbolSPtr disambigSym;
		if (type.Type()->typeKind == TypeKind::Iden)
		{
			IdenType& idenType = type.AsIden();
			SymbolSPtr idenSym = idenType.sym.lock();
			if (idenSym)
			{
				disambigSym = idenSym;
			}
			else
			{
				disambigSym = Find(idenType.qualName);
			}
		}
		else
		{
			disambigSym = Find(type);
		}

		if (!disambigSym)
			return nullptr;

		tmpIdens.assign(qualName->Idens().begin(), qualName->Idens().end());
		return disambigSym->children->Find(qualName, 0, disambig->IfaceQualName());
	}

	bool ModuleSymbolTable::RemoveType(SymbolSPtr sym)
	{
		if (sym->kind != SymbolKind::Type)
			return false;

		auto it = m_TypeSymbols.find(sym->type.Type());
		if (it != m_TypeSymbols.end())
		{
			m_TypeSymbols.erase(sym->type.Type());

			StdString typeName = g_TypeReg.ToString(sym->type);
			m_TypeNameSymbols.erase(typeName);
			return true;
		}

		return false;
	}

	void ModuleSymbolTable::RemoveImpl()
	{
		m_ScopedTable->RemoveImpl();
	}

	void ModuleSymbolTable::Merge(ModuleSymbolTable& src)
	{
		m_ScopedTable->Merge(src.m_ScopedTable);

		for (StdPair<const TypeSPtr, SymbolSPtr>& typeSymbol : src.m_TypeSymbols)
		{
			auto it = m_TypeSymbols.find(typeSymbol.first);
			if (it != m_TypeSymbols.end())
			{
				SymbolSPtr itSym = it->second;
				SymbolSubTableSPtr itChildren = itSym->children;
				
				SymbolSPtr sym = typeSymbol.second;
				SymbolSubTableSPtr symChildren = sym->children;

				itChildren->Merge(sym->children);

				if (!sym->impls.empty())
				{
					itSym->impls.insert(itSym->impls.end(), sym->impls.begin(), sym->impls.end());
				}

				// TODO: Instantiations
			}
			else
			{
				m_TypeSymbols.try_emplace(typeSymbol.first, typeSymbol.second);
			}
		}
	}

	void ModuleSymbolTable::AddImport(QualNameSPtr qualName)
	{
		m_ImportedModuleNames.push_back(qualName);
	}

	void ModuleSymbolTable::Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda)
	{
		m_ScopedTable->Foreach(lambda, nullptr);
		for (StdPair<const TypeSPtr, SymbolSPtr>& pair : m_TypeSymbols)
		{
			lambda(pair.second, nullptr);
			pair.second->children->Foreach(lambda);
		}
	}

	void ModuleSymbolTable::Log(bool includeImports)
	{
		g_Logger.Log("(module sym-table)\n");
		m_ScopedTable->Log(1, includeImports);

		for (StdPair<TypeSPtr, SymbolSPtr> pair : m_TypeSymbols)
		{
			pair.second->Log(1, includeImports);
		}

		g_Logger.Flush();
	}

	ScopedSymbolTable::ScopedSymbolTable()
	{
	}

	bool ScopedSymbolTable::Add(SymbolSPtr sym, usize idenIdx)
	{
		const StdVector<StdString>& idens = sym->qualName->Idens();
		const StdString& iden = idens[idenIdx];

		if (idenIdx == idens.size() - 1)
		{
			return m_Symbols.try_emplace(iden, sym).second;
		}
		else
		{
			auto symIt = m_Symbols.find(iden);
			if (symIt != m_Symbols.end())
			{
				sym->parent = symIt->second;
				return symIt->second->children->Add(sym, nullptr);
			}

			auto scopeIt = m_SubTables.find(iden);
			if (scopeIt == m_SubTables.end())
				scopeIt = m_SubTables.try_emplace(iden, ScopedSymbolTableSPtr{ new ScopedSymbolTable{} }).first;

			return scopeIt->second->Add(sym, idenIdx + 1);
		}
	}

	bool ScopedSymbolTable::RemoveFromCur(SymbolSPtr sym)
	{
		const StdString& iden = sym->qualName->LastIden();
		
		auto it = m_Symbols.find(iden);
		if (it != m_Symbols.end())
		{
			m_Symbols.erase(it);
			return true;
		}

		return false;
	}

	void ScopedSymbolTable::RemoveImpl()
	{
		StdVector<StdString> toRemove;
		for (StdPair<const StdString, SymbolSPtr>& pair : m_Symbols)
		{
			if (pair.second->kind == SymbolKind::Impl)
				toRemove.push_back(pair.first);
		}

		for (StdString name : toRemove)
		{
			m_Symbols.erase(name);
		}

		for (StdPair<const StdString, ScopedSymbolTableSPtr>& pair : m_SubTables)
		{
			pair.second->RemoveImpl();
		}
	}

	SymbolSPtr ScopedSymbolTable::Find(QualNameSPtr qualName)
	{
		return Find(qualName, 0);
	}

	SymbolSPtr ScopedSymbolTable::Find(QualNameSPtr qualName, usize idenIdx)
	{
		const StdString& iden = qualName->Idens()[idenIdx];

		auto symIt = m_Symbols.find(iden);
		if (symIt != m_Symbols.end())
		{
			if (idenIdx == qualName->Idens().size() - 1)
				return symIt->second;
			return symIt->second->children->Find(qualName, idenIdx + 1, nullptr);
		}

		auto subIt = m_SubTables.find(iden);
		if (subIt != m_SubTables.end())
		{
			if (idenIdx == qualName->Idens().size() - 1)
				return nullptr;
			return subIt->second->Find(qualName, idenIdx + 1);
		}

		return nullptr;
	}

	void ScopedSymbolTable::Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda, QualNameSPtr iface)
	{
		for (StdPair<const StdString, SymbolSPtr>& pair : m_Symbols)
		{
			lambda(pair.second, iface);
			pair.second->children->Foreach(lambda);
		}

		for (StdPair<const StdString, ScopedSymbolTableSPtr>& pair : m_SubTables)
		{
			pair.second->Foreach(lambda, nullptr);
		}
	}

	void ScopedSymbolTable::Merge(ScopedSymbolTableSPtr src)
	{
		for (StdPair<const StdString, SymbolSPtr>& pair : src->m_Symbols)
		{
			m_Symbols.try_emplace(pair.first, pair.second);
		}

		for (StdPair<const StdString, ScopedSymbolTableSPtr>& subTable : src->m_SubTables)
		{
			auto it = m_SubTables.find(subTable.first);
			if (it == m_SubTables.end())
				it = m_SubTables.try_emplace(subTable.first, new ScopedSymbolTable{}).first;

			it->second->Merge(subTable.second);
		}
	}

	void ScopedSymbolTable::Log(u8 indent, bool includeImports)
	{
		for (StdPair<StdString, SymbolSPtr> pair : m_Symbols)
		{
			pair.second->Log(indent + 1, includeImports);
		}

		for (StdPair<const StdString, ScopedSymbolTableSPtr>& pair : m_SubTables)
		{
			for (u8 i = 1; i < indent; ++i)
				g_Logger.Log(" |");
			g_Logger.Log(" +(sub-table '%s')\n", pair.first.c_str());
			pair.second->Log(indent + 1, includeImports);
		}
	}
}
