#include "symbol.hpp"

#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/logger.hpp"
#include "common/qualname.hpp"

namespace Noctis
{
	Symbol::Symbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName)
		: qualName(qualName)
		, children(new SymbolSubTable{ pCtx })
		, funcDefaultStart(u16(-1))
		, type(TypeHandle(-1))
		, size(0)
		, aligment(0)
		, offset(0)
		, pCtx(pCtx)
		, kind(kind)
		, isImported(false)
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
			qualName->Iden() == iden)
			return self.lock();

		for (SymbolSPtr variant : variants)
		{
			if (variant->qualName->Iden() == iden)
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
			children->Foreach([this](SymbolSPtr sym, QualNameSPtr) {
				if (sym->qualName->Base() != qualName)
					return;

				sym->CalculateSizeAlignOffset();
				sym->offset = sym->size;

				if (aligment < sym->aligment)
					aligment = sym->aligment;

				u64 alignMask = sym->aligment - 1;
				u16 alignOffset = u16(size & alignMask);
				size += alignOffset == 0 ? 0 : sym->aligment - alignOffset;
			});
			break;
		}
		case SymbolKind::Union:
		{
			children->Foreach([this](SymbolSPtr sym, QualNameSPtr) {
				if (sym->qualName->Base() != qualName)
					return;

				sym->CalculateSizeAlignOffset();
				sym->offset = 0;

				if (aligment < sym->aligment)
					aligment = sym->aligment;
				if (size < sym->size)
					size = sym->size;
			});
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
			TypeSPtr actType = pCtx->typeReg.GetType(type);
			if (!actType->size)
				actType->CalculateSizeAlign(pCtx->typeReg);
			
			size = actType->size;
			aligment = actType->alignment;
			break;
		}
		default: ;
		}
		
		
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
		case SymbolKind::ImplType: kindName = "impl"; break;
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

		printIndent(indent);
		StdString typeName = pCtx->typeReg.ToString(type);
		const char* imported = isImported ? ", import" : "";
		g_Logger.Log("(symbol '%s', kind='%s', type='%s'%s)\n", name.c_str(), kindName.data(), typeName.c_str(), imported);

		for (u8 i = 0; i < indent; ++i)
			g_Logger.Log(" |");
		g_Logger.Log(" +(children)\n");
		children->Log(indent + 1, includeImports);

		if (!variants.empty())
		{
			printIndent(indent + 1);
			g_Logger.Log("(variants)\n");
			for (SymbolSPtr variant : variants)
				variant->Log(indent + 2, includeImports);
		}

		if (!impls.empty())
		{
			for (StdPair<SymbolSPtr, bool>& pair : impls)
			{
				printIndent(indent + 1);
				SymbolSPtr impl = pair.first;
				StdString tmp = impl->kind == SymbolKind::ImplType ? pCtx->typeReg.ToString(impl->type) : impl->qualName->ToString();
				g_Logger.Log(isInterface ? "(implemented by '%s')\n" : "(implements '%s')\n", tmp.c_str());
			}
		}
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

	bool SymbolSubTable::Add(SymbolSPtr symbol, StdVector<IdenSPtr>& idens)
	{
		return m_SubTable->Add(symbol, idens);
	}

	bool SymbolSubTable::Add(QualNameSPtr interfaceQualName, SymbolSPtr sym, StdVector<IdenSPtr>& idens)
	{
		sym->parent = m_Parent;
		
		// Further overlapping is processed later on, after generic value parameters and types are processed
		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> pair : m_ImplSubtables)
		{
			if (pair.first == interfaceQualName)
				return pair.second->Add(sym, idens);
		}

		auto it = m_ImplSubtables.try_emplace(interfaceQualName, ScopedSymbolTableSPtr{ new ScopedSymbolTable{ m_pCtx } }).first;
		it->second->Add(sym, idens);
		return true;
	}

	bool SymbolSubTable::AddChild(SymbolSPtr sym)
	{
		StdVector<IdenSPtr> idens{ sym->qualName->Iden() };
		sym->parent = m_Parent;
		return Add(sym, idens);
	}

	bool SymbolSubTable::AddChild(QualNameSPtr interfaceQualName, SymbolSPtr sym)
	{
		StdVector<IdenSPtr> idens{ sym->qualName->Iden() };
		sym->parent = m_Parent;
		return Add(interfaceQualName, sym, idens);
	}

	SymbolSPtr SymbolSubTable::Find(StdVector<IdenSPtr>& idens, QualNameSPtr interfaceName)
	{
		if (idens.size() == 1 &&
			interfaceName)
		{
			auto it = m_ImplSubtables.find(interfaceName);
			if (it == m_ImplSubtables.end())
				return nullptr;
			return it->second->Find(idens, interfaceName);
		}
		
		StdVector<IdenSPtr> tmpIdens = idens;
		SymbolSPtr sym = m_SubTable->Find(idens, interfaceName);
		if (sym)
			return sym;

		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> implSymbol : m_ImplSubtables)
		{
			idens = tmpIdens;
			SymbolSPtr tmp = implSymbol.second->Find(idens, interfaceName);

			if (tmp)
			{
				if (sym)
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

	SymbolSPtr SymbolSubTable::Find(StdVector<IdenSPtr>& idens, const StdVector<StdString>& argNames)
	{
		StdVector<IdenSPtr> tmpIdens = idens;
		SymbolSPtr sym = m_SubTable->Find(idens, argNames);
		if (sym)
			return sym;

		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> implSymbol : m_ImplSubtables)
		{
			idens = tmpIdens;
			SymbolSPtr tmp = implSymbol.second->Find(idens, argNames);

			if (tmp)
			{
				if (sym)
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
		return subTable->Find(idens, nullptr);
	}

	SymbolSPtr SymbolSubTable::FindChild(QualNameSPtr implQualName, IdenSPtr iden, const StdVector<StdString>& argNames)
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
		return subTable->Find(idens, argNames);
	}

	void SymbolSubTable::Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda)
	{
		m_SubTable->Foreach(lambda, nullptr);
		for (StdPair<const QualNameSPtr, ScopedSymbolTableSPtr>& pair : m_ImplSubtables)
		{
			pair.second->Foreach(lambda, pair.first);
		}
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

	ModuleSymbolTable::ModuleSymbolTable(Context* pCtx)
		: m_ScopedTable(new ScopedSymbolTable{ pCtx })
		, m_pCtx(pCtx)
	{
	}

	bool ModuleSymbolTable::Add(SymbolSPtr sym)
	{
		if (sym->kind == SymbolKind::ImplType)
		{
			TypeSPtr type = m_pCtx->typeReg.GetType(sym->type);
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
			StdVector<IdenSPtr> idens = sym->qualName->AllIdens();
			std::reverse(idens.begin(), idens.end());
			return m_ScopedTable->Add(sym, idens);
		}
	}

	SymbolSPtr ModuleSymbolTable::Find(QualNameSPtr scope, QualNameSPtr qualname)
	{
		return Find(scope, qualname, nullptr);
	}

	SymbolSPtr ModuleSymbolTable::Find(QualNameSPtr scope, QualNameSPtr qualname, QualNameSPtr interfaceName)
	{
		StdVector<QualNameSPtr> possibleQualNames;
		StdVector<IdenSPtr> idens = qualname->AllIdens();
		StdVector<IdenSPtr> scopeIdens;
		if (scope)
			scopeIdens = scope->AllIdens();

		while (!scopeIdens.empty())
		{
			QualNameSPtr baseName = QualName::Create(scopeIdens);
			possibleQualNames.push_back(QualName::Create(baseName, idens));
			scopeIdens.pop_back();
		}
		possibleQualNames.push_back(qualname);

		for (QualNameSPtr baseName : m_ImportedModuleNames)
		{
			possibleQualNames.push_back(QualName::Create(baseName, idens));
		}

		for (QualNameSPtr possibleQualName : possibleQualNames)
		{
			StdVector<IdenSPtr> allIdens = possibleQualName->AllIdens();
			std::reverse(allIdens.begin(), allIdens.end());
			IdenSPtr firstIden = allIdens.back();

			auto it = m_TypeNameSymbols.find(firstIden);
			if (it != m_TypeNameSymbols.end())
			{
				allIdens.pop_back();
				SymbolSPtr sym = it->second->children->Find(allIdens, interfaceName);
				if (sym)
					return sym;
				continue;
			}

			SymbolSPtr sym = m_ScopedTable->Find(allIdens, interfaceName);
			if (sym)
				return sym;
		}

		return nullptr;
	}

	SymbolSPtr ModuleSymbolTable::Find(TypeSPtr type)
	{
		auto it = m_TypeSymbols.find(type);
		if (it != m_TypeSymbols.end())
			return it->second;
		return nullptr;
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

	bool ScopedSymbolTable::Add(SymbolSPtr sym, StdVector<IdenSPtr>& idens)
	{
		IdenSPtr iden = idens.back();
		idens.pop_back();
		const StdString& name = iden->Name();

		if (idens.empty())
		{
			if (sym->kind == SymbolKind::Func ||
				sym->kind == SymbolKind::Method)
			{
				StdString funcName = iden->ToFuncSymName();
				auto funcsIt = m_Functions.find(name);
				if (funcsIt == m_Functions.end())
				{
					funcsIt = m_Functions.try_emplace(name, StdUnorderedMap<StdString, SymbolSPtr>{}).first;
					funcsIt->second.try_emplace(funcName, sym);
					return true;
				}
				else
				{
					auto it = funcsIt->second.find(funcName);
					if (it != funcsIt->second.end())
					{
						// TODO: where is the symbol defined?
						StdString fullName = sym->qualName->ToString();
						g_ErrorSystem.Error("Symbol is already defined: '%s'", fullName.c_str());
						return false;
					}
					funcsIt->second.try_emplace(funcName, sym);
					return true;
				}
			}
			else
			{
				return m_Symbols.try_emplace(name, sym).second;
			}
		}
		else
		{
			auto symIt = m_Symbols.find(name);
			if (symIt != m_Symbols.end())
			{
				sym->parent = symIt->second;
				return symIt->second->children->Add(sym, idens);
			}

			auto funcIt = m_Functions.find(name);
			if (funcIt != m_Functions.end())
			{
				StdString funcName = iden->ToFuncSymName();
				auto subIt = funcIt->second.find(funcName);
				if (subIt != funcIt->second.end())
				{
					sym->parent = symIt->second;
					return subIt->second->children->Add(sym, idens);
				}
				return false;
			}

			auto scopeIt = m_SubTables.find(name);
			if (scopeIt == m_SubTables.end())
				scopeIt = m_SubTables.try_emplace(name, ScopedSymbolTableSPtr{ new ScopedSymbolTable{ m_pCtx } }).first;

			return scopeIt->second->Add(sym, idens);
		}
	}

	SymbolSPtr ScopedSymbolTable::Find(QualNameSPtr qualName)
	{
		StdVector<IdenSPtr> idens = qualName->AllIdens();
		std::reverse(idens.begin(), idens.end());
		return Find(idens, nullptr);
	}

	SymbolSPtr ScopedSymbolTable::Find(QualNameSPtr qualName, const StdVector<StdString>& argNames)
	{
		StdVector<IdenSPtr> idens = qualName->AllIdens();
		std::reverse(idens.begin(), idens.end());
		return Find(idens, argNames);
	}

	SymbolSPtr ScopedSymbolTable::Find(StdVector<IdenSPtr>& idens, QualNameSPtr interfaceName)
	{
		IdenSPtr iden = idens.back();
		idens.pop_back();
		const StdString& name = iden->Name();

		auto symIt = m_Symbols.find(name);
		if (symIt != m_Symbols.end())
		{
			if (idens.empty())
				return symIt->second;
			SymbolSPtr variant = symIt->second->GetVariant(iden);
			return variant->children->Find(idens, interfaceName);
		}

		auto funcIt = m_Functions.find(name);
		if (funcIt != m_Functions.end())
		{
			StdString funcStr = iden->ToFuncSymName();
			auto subIt = funcIt->second.find(funcStr);
			if (subIt != funcIt->second.end())
			{
				SymbolSPtr variant = subIt->second->GetVariant(iden);
				if (idens.empty())
					return variant;
				return variant->children->Find(idens, interfaceName);
			}
		}

		auto subIt = m_SubTables.find(name);
		if (subIt != m_SubTables.end())
		{
			if (idens.empty())
				return nullptr;
			return subIt->second->Find(idens, interfaceName);
		}

		return nullptr;
	}

	SymbolSPtr ScopedSymbolTable::Find(StdVector<IdenSPtr>& idens, const StdVector<StdString>& argNames)
	{
		IdenSPtr iden = idens.back();
		idens.pop_back();
		const StdString& name = iden->Name();

		if (idens.empty())
		{
			auto it = m_Functions.find(name);
			if (it == m_Functions.end())
				return nullptr;

			if (it->second.size() == 1)
			{
				SymbolSPtr funcSym = it->second.begin()->second;
				return funcSym->GetVariant(iden);
			}
			
			StdString funcName = iden->Name();
			for (const StdString& argName : argNames)
			{
				funcName += "__" + argName;
			}

			auto subIt = it->second.find(funcName);
			if (subIt != it->second.end())
				return subIt->second;
		}
		else
		{
			auto symIt = m_Symbols.find(name);
			if (symIt != m_Symbols.end())
			{
				if (idens.empty())
					return symIt->second;
				SymbolSPtr variant = symIt->second->GetVariant(iden);
				return variant->children->Find(idens, argNames);
			}

			auto funcIt = m_Functions.find(name);
			if (funcIt != m_Functions.end())
			{
				StdString funcName = iden->Name();
				for (const StdString& argName : argNames)
				{
					funcName += "__" + argName;
				}
				
				auto subIt = funcIt->second.find(funcName);
				if (subIt != funcIt->second.end())
				{
					SymbolSPtr variant = subIt->second->GetVariant(iden);
					return variant->children->Find(idens, argNames);
				}
			}

			auto subIt = m_SubTables.find(name);
			if (subIt != m_SubTables.end())
			{
				if (idens.empty())
					return nullptr;
				return subIt->second->Find(idens, argNames);
			}
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

		for (StdPair<const StdString, StdUnorderedMap<StdString, SymbolSPtr>> pair : m_Functions)
		{
			for (StdPair<const StdString, SymbolSPtr>& pair2 : pair.second)
			{
				lambda(pair2.second, iface);
				for (SymbolSPtr variant : pair2.second->variants)
				{
					lambda(variant, iface);
				}
				pair2.second->children->Foreach(lambda);
			}
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

		for (StdPair<const StdString, StdUnorderedMap<StdString, SymbolSPtr>>& funcPair : src->m_Functions)
		{
			auto it = m_Functions.find(funcPair.first);
			if (it == m_Functions.end())
				it = m_Functions.try_emplace(funcPair.first, StdUnorderedMap<StdString, SymbolSPtr>{}).first;

			for (StdPair<const StdString, SymbolSPtr>& pair : funcPair.second)
			{
				it->second.try_emplace(pair.first, pair.second);
			}
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

		for (StdPair<const StdString, StdUnorderedMap<StdString, SymbolSPtr>>& basePair : m_Functions)
		{
			for (StdPair<const StdString, SymbolSPtr>& pair : basePair.second)
			{
				pair.second->Log(indent + 1, includeImports);
			}
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
