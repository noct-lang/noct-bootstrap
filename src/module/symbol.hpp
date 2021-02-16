 #pragma once
#include <functional>

#include "attributes.hpp"
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

		AssocType,
		GenVal,
	};
	
	
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_CLASS_SPTR(SymbolSubTable);
	FWDECL_CLASS_SPTR(ScopedSymbolTable);

	FWDECL_STRUCT_SPTR(ITrDef);
	FWDECL_STRUCT_WPTR(ITrDef);

	FWDECL_STRUCT_SPTR(SymbolInst);
	FWDECL_STRUCT_WPTR(SymbolInst);
 	struct SymbolInst
 	{
		SymbolInst(SymbolSPtr sym, QualNameSPtr qualName);

		TypeHandle SelfType();
 		
		SymbolWPtr sym;
		QualNameSPtr qualName;
		TypeHandle type;

		SymbolInstSPtr parent;
		StdVector<SymbolInstWPtr> ifaces;

 		// Is the instantiation actually generic or just a reference to a symbol
		bool isGeneric : 1;
 	};
	
	struct Symbol
	{
		Symbol(Context* pCtx, SymbolKind kind, QualNameSPtr qualName);

		void SetSelf(SymbolWPtr self);

		TypeHandle SelfType();
		
		void CalculateSizeAlignOffset();

		SymbolInstSPtr GetInst(QualNameSPtr qualName);
		SymbolInstSPtr GetOrCreateInst(QualNameSPtr qualName);
		void AddInstForType(TypeHandle type, SymbolInstSPtr inst);
		const StdVector<SymbolInstWPtr>& GetInstForType(TypeHandle type);
		
		void Log(u8 indent, bool includeImports);

		bool HasMarker(QualNameSPtr name);

		SymbolSPtr Copy();

		bool IsInterface() const { return kind == SymbolKind::StrongInterface || kind == SymbolKind::WeakInterface || kind == SymbolKind::MarkerInterface; }
	
		QualNameSPtr qualName;

		SymbolSubTableSPtr children;
		SymbolWPtr parent;
		StdVector<SymbolWPtr> orderedVarChildren;

		// Interfaces that are implemented or types that implement the interface
		StdPairVector<SymbolWPtr, SymbolInstWPtr> impls;
		StdVector<SymbolInstWPtr> ifaces;
		StdVector<SymbolWPtr> markers;


		StdUnorderedMap<TypeSPtr, StdVector<SymbolInstWPtr>> instsForTypes;

		SymbolInstSPtr baseInst;
		StdVector<SymbolInstSPtr> instantiations;
			
		SymbolWPtr self;


		u16 funcDefaultStart;

		TypeHandle type;
		BoundsInfo boundsInfo;

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

		Attribute attribs;
		Visibility vis;

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
		void Foreach(const std::function<void(SymbolInstSPtr, QualNameSPtr)>& lambda);

		void Merge(SymbolSubTableSPtr from);
		
		bool Empty() const;

		void UpdateImplSubTableKey(QualNameSPtr oldKey, QualNameSPtr newKey);

		void Log(u8 indent, bool includeImports);

	private:
		friend class ModuleSymbolTable;

		// TODO: replace ScopedSymbolTableSPtr with StdUnorderedMap<StdString, SymbolSPtr> when GenVal kind is removed
		ScopedSymbolTableSPtr m_SubTable;
		StdUnorderedMap<QualNameSPtr, ScopedSymbolTableSPtr> m_ImplSubtables;

		Context* m_pCtx;
		SymbolWPtr m_Parent;
	};


	class ModuleSymbolTable
	{
	public:
		ModuleSymbolTable(Context* pCtx, QualNameSPtr scope);

		bool Add(SymbolSPtr sym);

		SymbolSPtr Find(QualNameSPtr scope, QualNameSPtr qualname, QualNameSPtr interfaceQualName = nullptr);
		SymbolSPtr Find(QualNameSPtr qualName);
		SymbolSPtr Find(TypeHandle type);
		SymbolSPtr Find(TypeSPtr type);

		SymbolSPtr FindWithInterface(QualNameSPtr qualName, QualNameSPtr interfaceQualName);

		SymbolSPtr FindWithDisambiguation(QualNameSPtr qualName);

		bool RemoveType(SymbolSPtr sym);
		void RemoveImpl();

		void Merge(ModuleSymbolTable& src);
		void AddImport(QualNameSPtr qualName);

		void Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda);
		void Foreach(const std::function<void(SymbolInstSPtr, QualNameSPtr)>& lambda);

		void Log(bool includeImports);
		
	private:
		ScopedSymbolTableSPtr m_ScopedTable;
		StdUnorderedMap<TypeSPtr, SymbolSPtr> m_TypeSymbols;
		StdUnorderedMap<IdenSPtr, SymbolSPtr> m_TypeNameSymbols;

		QualNameSPtr m_ModScope;
		StdVector<QualNameSPtr> m_ImportedModuleNames;

		Context* m_pCtx;
	};

	class ScopedSymbolTable
	{
	public:
		ScopedSymbolTable(Context* pCtx);

		bool Add(SymbolSPtr sym, usize idenIdx);
		
		bool RemoveFromCur(SymbolSPtr sym);
		void RemoveImpl();

		SymbolSPtr Find(QualNameSPtr qualName);
		SymbolSPtr Find(QualNameSPtr qualName, usize idenIdx);

		void Foreach(const std::function<void(SymbolSPtr, QualNameSPtr)>& lambda, QualNameSPtr iface);
		void Foreach(const std::function<void(SymbolInstSPtr, QualNameSPtr)>& lambda, QualNameSPtr iface);

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
