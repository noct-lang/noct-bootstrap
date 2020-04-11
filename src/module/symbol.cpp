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
		, pCtx(pCtx)
		, kind(kind)
		, isImported(false)
	{
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

	void Symbol::Log(u8 indent)
	{
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
		const char* imported = isImported ? ", import" : "";
		g_Logger.Log("(symbol '%s', kind='%s'%s)\n", name.c_str(), kindName.data(), imported);

		for (u8 i = 0; i < indent; ++i)
			g_Logger.Log(" |");
		g_Logger.Log(" +(children)\n");
		children->Log(indent + 1);

		if (!variants.empty())
		{
			printIndent(indent);
			g_Logger.Log("(variants)\n");
			for (SymbolSPtr variant : variants)
				variant->Log(indent + 1);
		}

		if (!impls.empty())
		{
			for (SymbolSPtr impl : impls)
			{
				printIndent(indent + 1);
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

	bool SymbolSubTable::Add(SymbolSPtr symbol, StdVector<IdenSPtr>& idens)
	{
		return m_SubTable->Add(symbol, idens);
	}

	bool SymbolSubTable::Add(QualNameSPtr interfaceQualName, SymbolSPtr symbol, StdVector<IdenSPtr>& idens)
	{
		// Further overlapping is processed later on, after generic value parameters and types are processed
		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> pair : m_ImplSubtables)
		{
			if (pair.first == interfaceQualName)
			{
				pair.second->Add(symbol, idens);
				return true;
			}
		}

		m_ImplSubtables.try_emplace(interfaceQualName, ScopedSymbolTableSPtr{ new ScopedSymbolTable{ m_pCtx } });
		m_ImplSubtables.begin()->second->Add(symbol, idens);
		return true;
	}

	bool SymbolSubTable::AddChild(SymbolSPtr sym)
	{
		StdVector<IdenSPtr> idens{ sym->qualName->Iden() };
		return Add(sym, idens);
	}

	SymbolSPtr SymbolSubTable::Find(StdVector<IdenSPtr>& idens)
	{
		StdVector<IdenSPtr> tmpIdens = idens;
		SymbolSPtr sym = m_SubTable->Find(idens);
		if (sym)
			return sym;

		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> implSymbol : m_ImplSubtables)
		{
			idens = tmpIdens;
			SymbolSPtr tmp = implSymbol.second->Find(idens);

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

	void SymbolSubTable::Log(u8 indent)
	{
		m_SubTable->Log(indent);
		for (StdPair<QualNameSPtr, ScopedSymbolTableSPtr> pair : m_ImplSubtables)
		{
			for (u8 i = 1; i < indent; ++i)
				g_Logger.Log(" |");
			StdString implName = pair.first->ToString();
			g_Logger.Log(" +(impl for '%s')", implName.c_str());
			pair.second->Log(indent + 1);
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

		for (QualNameSPtr possibleQualName : possibleQualNames)
		{
			SymbolSPtr sym = m_ScopedTable->Find(possibleQualName);
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

	void ModuleSymbolTable::Log()
	{
		g_Logger.Log("(module sym-table)\n");
		m_ScopedTable->Log(1);

		for (StdPair<TypeSPtr, SymbolSPtr> pair : m_TypeSymbols)
		{
			pair.second->Log(1);
		}
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
					m_Functions.try_emplace(name, StdUnorderedMap<StdString, SymbolSPtr>{});
					m_Functions.begin()->second.try_emplace(funcName, sym);
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
				return symIt->second->children->Add(sym, idens);
			}

			auto funcIt = m_Functions.find(name);
			if (funcIt != m_Functions.end())
			{
				StdString funcName = iden->ToFuncSymName();
				auto subIt = funcIt->second.find(funcName);
				if (subIt != funcIt->second.end())
					return subIt->second->children->Add(sym, idens);
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
		return Find(idens);
	}

	SymbolSPtr ScopedSymbolTable::Find(QualNameSPtr qualName, const StdVector<StdString>& argNames)
	{
		StdVector<IdenSPtr> idens = qualName->AllIdens();
		std::reverse(idens.begin(), idens.end());
		return Find(idens, argNames);
	}

	SymbolSPtr ScopedSymbolTable::Find(StdVector<IdenSPtr>& idens)
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
			return variant->children->Find(idens);
		}

		auto funcIt = m_Functions.find(name);
		if (funcIt != m_Functions.end())
		{
			StdString funcStr = iden->ToFuncSymName();
			auto subIt = funcIt->second.find(funcStr);
			if (subIt != funcIt->second.end())
			{
				SymbolSPtr variant = subIt->second->GetVariant(iden);
				return variant->children->Find(idens);
			}
		}

		auto subIt = m_SubTables.find(name);
		if (subIt != m_SubTables.end())
		{
			if (idens.empty())
				return nullptr;
			return subIt->second->Find(idens);
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
				SymbolSPtr variant = funcSym->GetVariant(iden);
				return variant->children->Find(idens, argNames);
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

	void ScopedSymbolTable::Log(u8 indent)
	{
		for (StdPair<StdString, SymbolSPtr> pair : m_Symbols)
		{
			pair.second->Log(indent + 1);
		}

		for (StdPair<const StdString, StdUnorderedMap<StdString, SymbolSPtr>>& basePair : m_Functions)
		{
			for (StdPair<const StdString, SymbolSPtr>& pair : basePair.second)
			{
				pair.second->Log(indent + 1);
			}
		}

		for (StdPair<const StdString, ScopedSymbolTableSPtr>& pair : m_SubTables)
		{
			for (u8 i = 1; i < indent; ++i)
				g_Logger.Log(" |");
			g_Logger.Log(" +(sub-table '%s')\n", pair.first.c_str());
			pair.second->Log(indent + 1);
		}
	}
}
