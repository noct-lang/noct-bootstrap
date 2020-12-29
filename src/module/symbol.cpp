#include "symbol.hpp"

#include <cassert>


#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/logger.hpp"
#include "common/qualname.hpp"
#include "module.hpp"

namespace Noctis
{
	Symbol::Symbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName)
		: qualName(qualName)
		, children(new SymbolSubTable{ pCtx })
		, funcDefaultStart(u16(-1))
		, size(0)
		, aligment(0)
		, offset(0)
		, pCtx(pCtx)
		, kind(kind)
		, isImported(false)
		, isDefaultImpl(false)
		, comptimeOnly(false)
		, dependsOnValueGenerics(false)
		, defImplVer(0)
	{
	}

	void Symbol::SetSelf(SymbolWPtr self)
	{
		this->self = self;
		children->SetParent(self);
	}

	SymbolSPtr Symbol::GetVariant(IdenSPtr iden)
	{
		if (variants.empty() ||
			qualName->LastIden() == iden)
			return self.lock();

		for (SymbolSPtr variant : variants)
		{
			if (variant->qualName->LastIden() == iden)
				return variant;
		}

		return self.lock();
	}

	bool Symbol::IsBaseVariant()
	{
		return self.lock() == baseVariant.lock();
	}

	TypeHandle Symbol::SelfType()
	{
		switch (kind)
		{
		case SymbolKind::ValEnum:
		case SymbolKind::AdtEnum:
		case SymbolKind::Typedef:
		case SymbolKind::Typealias:
			return pCtx->typeReg.Iden(TypeMod::None, qualName);
		case SymbolKind::ValEnumMember:
		case SymbolKind::AdtEnumMember:
			return pCtx->typeReg.Iden(TypeMod::None, qualName->Base());
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
				actType->CalculateSizeAlign(pCtx->typeReg);
			
			size = actType->Size();
			aligment = actType->Align();
			break;
		}
		default: ;
		}
		
		
	}

	SymbolSPtr Symbol::GetOrCreateVariant(QualNameSPtr qualName)
	{
		if (!IsBaseVariant())
			return baseVariant.lock()->GetOrCreateVariant(qualName);

		if (this->qualName->LastIden() == qualName->LastIden())
			return self.lock();

		for (SymbolSPtr variant : variants)
		{
			if (variant->qualName == qualName)
				return variant;
		}
		
		QualNameSPtr newQualName = QualName::Create(this->qualName->Base(), qualName->LastIden());
		SymbolSPtr variantSym = CreateSymbol(pCtx, kind, newQualName);
		variantSym->baseVariant = self;
		variantSym->type = pCtx->typeReg.Iden(TypeMod::None, newQualName);
		variants.push_back(variantSym);
		return variantSym;
	}

	void Symbol::Log(u8 indent, bool includeImports)
	{
		if (isImported && !includeImports)
			return;
		
		StdString name = qualName ? qualName->ToString() : pCtx->typeReg.ToString(type);
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
		case SymbolKind::GenType: kindName = "gen-type"; break;
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
		StdString typeName = pCtx->typeReg.ToString(type);
		g_Logger.Log("(symbol '%s', kind='%s', type='%s'%s)\n", name.c_str(), kindName.data(), typeName.c_str(), flags.c_str());

		if (!children->Empty())
		{
			printIndent(indent + 1);
			g_Logger.Log("(children)\n");
			children->Log(indent + 1, includeImports);
		}

		if (!variants.empty())
		{
			printIndent(indent + 1);
			g_Logger.Log("(variants)\n");
			for (SymbolSPtr variant : variants)
				variant->Log(indent + 2, includeImports);
		}

		if (!impls.empty())
		{
			for (SymbolSPtr implSym : impls)
			{
				if (implSym->kind != SymbolKind::Type && !implSym->qualName->IsSubnameOf(pCtx->activeModule->qualName))
					continue;
				
				//if (pair.second)
				//	continue;
				
				printIndent(indent + 1);
				StdString tmp = implSym->kind == SymbolKind::Type ? pCtx->typeReg.ToString(implSym->type) : implSym->qualName->ToString();
				g_Logger.Log(isInterface ? "(implemented by '%s')\n" : "(implements '%s')\n", tmp.c_str());
			}
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
		
		SymbolSPtr res{ new Symbol{ pCtx, kind, qualName } };
		children->Foreach([&res](SymbolSPtr child, QualNameSPtr interfaceName)
		{
			res->children->AddChild(child, interfaceName);
		});

		res->qualName = qualName;
		res->parent = parent;
		res->markers = markers;
		res->type = type;
		res->associatedITr = associatedITr;
		res->SetSelf(res);

		res->impls.insert(res->impls.end(), impls.begin(), impls.end());
		res->interfaces.insert(res->interfaces.end(), interfaces.begin(), interfaces.end());
		res->markers.insert(res->markers.end(), markers.begin(), markers.end());

		return res;
	}

	void Symbol::SetType(TypeHandle type)
	{
		if (kind != SymbolKind::Type)
		{
			this->type = type;
			return;
		}

		if (this->type == type)
			return;

		SymbolSPtr sym = self.lock();
		bool removed = pCtx->activeModule->symTable.RemoveType(sym);

		sym->type = type;

		if (removed)
			pCtx->activeModule->symTable.Add(sym);
	}

	SymbolSPtr CreateSymbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName)
	{
		SymbolSPtr sym{ new Symbol{ pCtx, kind, qualName } };
		sym->SetSelf(sym);
		sym->baseVariant = sym;

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
			TypeHandle type = pCtx->typeReg.Iden(TypeMod::None, qualName);
			sym->type = type;
			pCtx->typeReg.SetIdenSym(type.AsIden().qualName, sym);
			break;
		}
		case SymbolKind::Func:
		case SymbolKind::Method:
		case SymbolKind::Closure:
		case SymbolKind::Var:
		case SymbolKind::Impl:
		case SymbolKind::Type:
		case SymbolKind::GenType:
		case SymbolKind::GenVal:
		default: ;
		}

		
		return sym;
	}

	SymbolSPtr CreateSymbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName, ITrDefWPtr node)
	{
		SymbolSPtr sym = CreateSymbol(pCtx, kind, qualName);
		sym->associatedITr = node;
		node.lock()->sym = sym;
		return sym;
	}

	SymbolSubTable::SymbolSubTable(Context* pCtx)
		: m_SubTable(new ScopedSymbolTable{ pCtx })
		, m_pCtx(pCtx)
	{
	}

	void SymbolSubTable::SetParent(SymbolWPtr parent)
	{
		m_Parent = parent;
	}

	bool SymbolSubTable::Add(QualNameSPtr interfaceQualName, SymbolSPtr sym, usize idenIdx)
	{
		sym->parent = m_Parent;
		if (interfaceQualName)
		{
			// Further overlapping is processed later on, after generic value parameters and types are processed
			for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> pair : m_ImplSubtables)
			{
				if (pair.first == interfaceQualName)
					return pair.second->Add(sym, idenIdx);
			}

			auto it = m_ImplSubtables.try_emplace(interfaceQualName, ScopedSymbolTableSPtr{ new ScopedSymbolTable{ m_pCtx } }).first;
			return it->second->Add(sym, idenIdx);
		}
		else
		{
			return m_SubTable->Add(sym, idenIdx);
		}
	}

	bool SymbolSubTable::AddChild(SymbolSPtr sym, QualNameSPtr interfaceQualName)
	{
		sym->parent = m_Parent;
		return Add(interfaceQualName, sym, sym->qualName->Idens().size() - 1);
	}

	SymbolSPtr SymbolSubTable::Find(QualNameSPtr qualName, usize idenIdx, QualNameSPtr interfaceName)
	{
		if (interfaceName)
		{
			auto it = m_ImplSubtables.find(interfaceName);
			if (it == m_ImplSubtables.end())
				return nullptr; 
			return it->second->Find(qualName, idenIdx);
		}

		SymbolSPtr sym = m_SubTable->Find(qualName, idenIdx);
		if (sym)
			return sym;

		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> implSymbol : m_ImplSubtables)
		{
			SymbolSPtr tmp = implSymbol.second->Find(qualName, idenIdx);

			if (tmp)
			{
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

	SymbolSPtr SymbolSubTable::FindChild(QualNameSPtr implQualName, IdenSPtr iden)
	{
		ScopedSymbolTableSPtr subTable;
		if (implQualName)
		{
			auto it = m_ImplSubtables.find(implQualName);
			if (it == m_ImplSubtables.end())
				return nullptr;
			subTable = it->second;
		}
		else
		{
			subTable = m_SubTable;
		}
		StdVector<IdenSPtr> idens{ iden };
		return subTable->Find(QualName::Create(iden), 0);
	}

	void SymbolSubTable::RemoveChild(SymbolSPtr sym)
	{
		m_SubTable->RemoveFromCur(sym);
	}

	void SymbolSubTable::Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda)
	{
		m_SubTable->Foreach(lambda, nullptr);
		for (StdPair<const QualNameSPtr, ScopedSymbolTableSPtr>& pair : m_ImplSubtables)
		{
			pair.second->Foreach(lambda, pair.first);
		}
	}

	bool SymbolSubTable::Empty() const
	{
		return m_SubTable->Empty() && m_ImplSubtables.empty();
	}

	void SymbolSubTable::Log(u8 indent, bool includeImports)
	{
		m_SubTable->Log(indent, includeImports);
		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> pair : m_ImplSubtables)
		{
			for (u8 i = 1; i < indent; ++i)
				g_Logger.Log(" |");
			StdString implName = pair.first->ToString();
			g_Logger.Log(" +(impl for '%s')\n", implName.c_str());
			pair.second->Log(indent + 1, includeImports);
		}
	}

	ModuleSymbolTable::ModuleSymbolTable(Context* pCtx, QualNameSPtr scope)
		: m_ScopedTable(new ScopedSymbolTable{ pCtx })
		, m_ModScope(scope)
		, m_pCtx(pCtx)
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
			StdString typeName = m_pCtx->typeReg.ToString(type);
			m_TypeNameSymbols.try_emplace(Iden::Create(typeName), sym);
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
		StdVector<IdenSPtr> idens = qualname->Idens();
		StdVector<IdenSPtr> scopeIdens;
		if (scope)
			scopeIdens = scope->Idens();

		while (!scopeIdens.empty())
		{
			QualNameSPtr baseName = QualName::Create(scopeIdens);
			possibleQualNames.push_back(QualName::Create(baseName, idens));
			scopeIdens.pop_back();
		}
		possibleQualNames.push_back(qualname);

		possibleQualNames.push_back(QualName::Create(m_ModScope, idens));

		for (QualNameSPtr baseName : m_ImportedModuleNames)
		{
			possibleQualNames.push_back(QualName::Create(baseName, idens));
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
		
		IdenSPtr firstIden = qualName->Idens().front();

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
		const StdVector<IdenSPtr>& idens = qualName->Idens();

		if (idens.size() == 1)
			return Find(qualName);
		
		for (usize i = 1; i < idens.size(); ++i)
		{
			StdVector<IdenSPtr> tmpIdens;
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

		StdVector<IdenSPtr> tmpIdens;
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

			StdString typeName = m_pCtx->typeReg.ToString(sym->type);
			m_TypeNameSymbols.erase(Iden::Create(typeName));
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
				
				itChildren->m_SubTable->Merge(sym->children->m_SubTable);

				for (StdPair<const QualNameSPtr, ScopedSymbolTableSPtr>& implSubtable : symChildren->m_ImplSubtables)
				{
					auto subIt = itChildren->m_ImplSubtables.find(implSubtable.first);
					if (subIt == itChildren->m_ImplSubtables.end())
						subIt = itChildren->m_ImplSubtables.try_emplace(implSubtable.first, new ScopedSymbolTable{ m_pCtx }).first;

					subIt->second->Merge(implSubtable.second);
				}

				if (!sym->impls.empty())
				{
					itSym->impls.insert(itSym->impls.end(), sym->impls.begin(), sym->impls.end());
				}

				// TODO: Variants
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

	ScopedSymbolTable::ScopedSymbolTable(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	bool ScopedSymbolTable::Add(SymbolSPtr sym, usize idenIdx)
	{
		const StdVector<IdenSPtr>& idens = sym->qualName->Idens();
		IdenSPtr iden = idens[idenIdx];
		const StdString& name = iden->Name();

		if (idenIdx == idens.size() - 1)
		{
			return m_Symbols.try_emplace(name, sym).second;
		}
		else
		{
			auto symIt = m_Symbols.find(name);
			if (symIt != m_Symbols.end())
			{
				sym->parent = symIt->second;
				return symIt->second->children->Add(nullptr, sym, idenIdx + 1);
			}

			auto scopeIt = m_SubTables.find(name);
			if (scopeIt == m_SubTables.end())
				scopeIt = m_SubTables.try_emplace(name, ScopedSymbolTableSPtr{ new ScopedSymbolTable{ m_pCtx } }).first;

			return scopeIt->second->Add(sym, idenIdx + 1);
		}
	}

	bool ScopedSymbolTable::RemoveFromCur(SymbolSPtr sym)
	{
		IdenSPtr iden = sym->qualName->LastIden();
		
		auto it = m_Symbols.find(iden->Name());
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
		IdenSPtr iden = qualName->Idens()[idenIdx];
		const StdString& name = iden->Name();

		auto symIt = m_Symbols.find(name);
		if (symIt != m_Symbols.end())
		{
			if (idenIdx == qualName->Idens().size() - 1)
				return symIt->second;
			SymbolSPtr variant = symIt->second->GetVariant(iden);
			return variant->children->Find(qualName, idenIdx + 1, nullptr);
		}

		auto subIt = m_SubTables.find(name);
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
			for (SymbolSPtr variant : pair.second->variants)
			{
				lambda(variant, iface);
			}
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
				it = m_SubTables.try_emplace(subTable.first, new ScopedSymbolTable{ m_pCtx }).first;

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
