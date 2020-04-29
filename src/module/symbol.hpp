#pragma once
#include "common/type.hpp"

namespace Noctis
{
	struct Context;
	
	FWDECL_CLASS_SPTR(Iden);

	enum class SymbolKind : u8
	{
		Struct,
		Union,
		ValEnum,
		ValEnumMember,
		AdtEnum,
		AdtEnumMember,
		MarkerInterface,
		WeakInterface,
		StrongInterface,
		Typealias,
		Typedef,

		Func,
		Method,
		Closure,

		Var,
		

		ImplType,

		GenType,
		GenVal,
	};
	
	
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_CLASS_SPTR(SymbolSubTable);
	FWDECL_CLASS_SPTR(ScopedSymbolTable);

	FWDECL_STRUCT_WPTR(ITrDef);
	
	struct Symbol
	{
		Symbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName);

		SymbolSPtr GetVariant(IdenSPtr iden);
		bool IsBaseVariant();

		void Log(u8 indent);
		
		QualNameSPtr qualName;


		SymbolSubTableSPtr children;

		// Interfaces that are implemented or types that implement the interface
		StdVector<SymbolSPtr> impls;
			
		SymbolWPtr self;
		SymbolWPtr baseVariant;
		StdVector<SymbolSPtr> variants;

		u16 funcDefaultStart;

		TypeHandle type;

		Context* pCtx;

		ITrDefWPtr associatedITr;
		
		SymbolKind kind;

		bool isImported : 1;
		
	};

	class SymbolSubTable
	{
	public:
		SymbolSubTable(Context* pCtx);

		bool Add(SymbolSPtr symbol, StdVector<IdenSPtr>& idens);
		bool Add(QualNameSPtr interfaceQualName, SymbolSPtr symbol, StdVector<IdenSPtr>& idens);
		bool AddChild(SymbolSPtr sym);

		SymbolSPtr Find(StdVector<IdenSPtr>& idens);
		SymbolSPtr Find(StdVector<IdenSPtr>& idens, const StdVector<StdString>& argNames);
		SymbolSPtr FindChild(QualNameSPtr implQualName, IdenSPtr iden);
		SymbolSPtr FindChild(QualNameSPtr implQualName, IdenSPtr iden, const StdVector<StdString>& argNames);

		void Log(u8 indent);

	private:

		ScopedSymbolTableSPtr m_SubTable;
		StdUnorderedMap<QualNameSPtr, ScopedSymbolTableSPtr> m_ImplSubtables;

		Context* m_pCtx;
	};


	class ModuleSymbolTable
	{
	public:
		ModuleSymbolTable(Context* pCtx);

		bool Add(SymbolSPtr sym);

		SymbolSPtr Find(QualNameSPtr scope, QualNameSPtr qualname);
		SymbolSPtr Find(TypeSPtr type);

		void Log();
		
	private:
		ScopedSymbolTableSPtr m_ScopedTable;
		StdUnorderedMap<TypeSPtr, SymbolSPtr> m_TypeSymbols;

		Context* m_pCtx;
	};

	class ScopedSymbolTable
	{
	public:
		ScopedSymbolTable(Context* pCtx);

		bool Add(SymbolSPtr sym, StdVector<IdenSPtr>& idens);

		SymbolSPtr Find(QualNameSPtr qualName);
		SymbolSPtr Find(QualNameSPtr qualName, const StdVector<StdString>& argNames);
		SymbolSPtr Find(StdVector<IdenSPtr>& idens);
		SymbolSPtr Find(StdVector<IdenSPtr>& idens, const StdVector<StdString>& argNames);

		void Log(u8 indent);

	private:
		
		StdUnorderedMap<StdString, ScopedSymbolTableSPtr> m_SubTables;
		StdUnorderedMap<StdString, SymbolSPtr> m_Symbols;
		StdUnorderedMap<StdString, StdUnorderedMap<StdString, SymbolSPtr>> m_Functions;

		Context* m_pCtx;
	};

	
	
}
