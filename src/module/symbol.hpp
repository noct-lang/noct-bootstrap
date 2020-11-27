 #pragma once
#include <functional>

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
		

		Impl,
		Type,

		GenType,
		AssocType,
		GenVal,
	};
	
	
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_CLASS_SPTR(SymbolSubTable);
	FWDECL_CLASS_SPTR(ScopedSymbolTable);

	FWDECL_STRUCT_SPTR(ITrDef);
	FWDECL_STRUCT_WPTR(ITrDef);
	
	struct Symbol
	{
		Symbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName);

		void SetSelf(SymbolWPtr self);
		SymbolSPtr GetVariant(IdenSPtr iden);
		bool IsBaseVariant();

		TypeHandle SelfType();
		
		void CalculateSizeAlignOffset();

		SymbolSPtr CreateVariant(QualNameSPtr qualName);
		
		void Log(u8 indent, bool includeImports);

		bool HasMarker(QualNameSPtr name);

		SymbolSPtr Copy();
	
		QualNameSPtr qualName;

		SymbolSubTableSPtr children;
		SymbolWPtr parent;
		StdVector<SymbolWPtr> orderedVarChildren;

		// Interfaces that are implemented or types that implement the interface
		// The boolean tells if the implemenation comes from an imported module (required for ImplType Symbols)
		StdVector<SymbolSPtr> impls;
		StdPairVector<QualNameSPtr, SymbolWPtr> interfaces;
		StdVector<SymbolWPtr> markers;
			
		SymbolWPtr self;
		SymbolWPtr baseVariant;
		StdVector<SymbolSPtr> variants;

		u16 funcDefaultStart;

		TypeHandle type;

		u64 size;
		u16 aligment;
		u64 offset;

		StdString mangledName;

		Context* pCtx;

		ITrDefWPtr associatedITr;
		
		SymbolKind kind;

		bool isImported : 1; 
		bool isDefaultImpl : 1;
		bool comptimeOnly : 1;
		bool valGenSolveNeeded : 1;
		bool dependsOnValueGenerics : 1;

		u8 defImplVer;
	};

	SymbolSPtr CreateSymbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName);
	SymbolSPtr CreateSymbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName, ITrDefWPtr node);

	class SymbolSubTable
	{
	public:
		SymbolSubTable(Context* pCtx);
		void SetParent(SymbolWPtr parent);

		bool Add(QualNameSPtr interfaceQualName, SymbolSPtr sym, usize idenIdx);
		bool AddChild(SymbolSPtr sym, QualNameSPtr interfaceQualName = nullptr);

		SymbolSPtr Find(QualNameSPtr qualName, usize idenIdx, QualNameSPtr interfaceName);
		SymbolSPtr FindChild(QualNameSPtr implQualName, IdenSPtr iden);

		void RemoveChild(SymbolSPtr sym);

		void Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda);

		bool Empty() const;

		void Log(u8 indent, bool includeImports);

	private:
		friend class ModuleSymbolTable;

		ScopedSymbolTableSPtr m_SubTable;
		StdUnorderedMap<QualNameSPtr, ScopedSymbolTableSPtr> m_ImplSubtables;

		Context* m_pCtx;
		SymbolWPtr m_Parent;
	};


	class ModuleSymbolTable
	{
	public:
		ModuleSymbolTable(Context* pCtx);

		bool Add(SymbolSPtr sym);

		SymbolSPtr Find(QualNameSPtr scope, QualNameSPtr qualname, QualNameSPtr interfaceQualName = nullptr);
		SymbolSPtr Find(QualNameSPtr qualName);
		SymbolSPtr Find(TypeHandle type);
		SymbolSPtr Find(TypeSPtr type);

		SymbolSPtr FindWithInterface(QualNameSPtr qualName, QualNameSPtr interfaceQualName);

		SymbolSPtr FindWithDisambiguation(QualNameSPtr qualName);

		void Merge(ModuleSymbolTable& src);
		void AddImport(QualNameSPtr qualName);

		void Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda);

		void Log(bool includeImports);
		
	private:
		ScopedSymbolTableSPtr m_ScopedTable;
		StdUnorderedMap<TypeSPtr, SymbolSPtr> m_TypeSymbols;
		StdUnorderedMap<IdenSPtr, SymbolSPtr> m_TypeNameSymbols;

		StdVector<QualNameSPtr> m_ImportedModuleNames;

		Context* m_pCtx;
	};

	class ScopedSymbolTable
	{
	public:
		ScopedSymbolTable(Context* pCtx);

		bool Add(SymbolSPtr sym, usize idenIdx);
		
		bool RemoveFromCur(SymbolSPtr sym);

		SymbolSPtr Find(QualNameSPtr qualName);
		SymbolSPtr Find(QualNameSPtr qualName, usize idenIdx);

		void Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda, QualNameSPtr iface);

		void Merge(ScopedSymbolTableSPtr src);

		bool Empty() const;

		void Log(u8 indent, bool includeImports);

	private:
		friend class ModuleSymbolTable;
		
		StdUnorderedMap<StdString, ScopedSymbolTableSPtr> m_SubTables;
		StdUnorderedMap<StdString, SymbolSPtr> m_Symbols;

		Context* m_pCtx;
	};

	inline bool ScopedSymbolTable::Empty() const
	{
		return m_Symbols.empty() && m_SubTables.empty();
	}
}
