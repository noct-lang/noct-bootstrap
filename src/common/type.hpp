#pragma once

#include "defs.hpp"
#include <cassert>

namespace Noctis
{
	struct Module;
	struct Context;
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_SPTR(TypeDisambiguation);
	FWDECL_STRUCT_WPTR(Symbol);

	FWDECL_STRUCT_SPTR(AstExpr);
	FWDECL_STRUCT_SPTR(ITrExpr);

	enum class TypeKind : u8
	{
		Invalid,
		Builtin,
		Iden,
		Ptr,
		Ref,
		Slice,
		Array,
		Tuple,
		Opt,
		Compound,
		Func,
		Generic,
	};

	enum class BuiltinTypeKind : u8
	{
		Bool,
		Char,
		I8,
		I16,
		I32,
		I64,
		I128,
		ISize,
		U8,
		U16,
		U32,
		U64,
		U128,
		USize,
		F16,
		F32,
		F64,
		F128,


		Count,
	};

	bool IsBuiltinInteger(BuiltinTypeKind kind);
	bool IsBuiltinSigned(BuiltinTypeKind kind);
	bool IsBuiltinUnsigned(BuiltinTypeKind kind);
	bool IsBuiltinFloat(BuiltinTypeKind kind);
	u8 GetBuiltinBytes(BuiltinTypeKind kind);

	enum class TypeMod : u8
	{
		None,
		Mut,
		Count,
	};
	StdStringView TypeModToString(TypeMod mod);

	FWDECL_STRUCT_SPTR(Type);

	struct BaseType;
	struct BuiltinType;
	struct IdenType;
	struct PtrType;
	struct RefType;
	struct SliceType;
	struct ArrayType;
	struct TupleType;
	struct OptType;
	struct FuncType;
	struct GenericType;

	class TypeRegistry;

	struct THandle
	{
		THandle() {}
		THandle(TypeSPtr type)
			: type(type)
		{}

		TypeSPtr type;
	};

	struct TypeHandle
	{
		TypeHandle()
		{}
		explicit TypeHandle(StdSharedPtr<THandle> handle)
			: type(handle)
		{}

		TypeKind Kind() const;
		TypeSPtr Type();
		const TypeSPtr Type() const;

		usize Size();
		u16 Align();
		TypeMod Mod() const;
		bool HasFuzzyCompare() const;

		BuiltinType& AsBuiltin();
		IdenType& AsIden();
		const IdenType& AsIden() const;
		PtrType& AsPtr();
		RefType& AsRef();
		SliceType& AsSlice();
		ArrayType& AsArray();
		TupleType& AsTuple();
		OptType& AsOpt();
		FuncType& AsFunc();
		GenericType& AsGeneric();

		StdString ToString() const;

		bool IsValid() const { return !!type; }

		bool operator==(const TypeHandle& other) const;
		bool operator!=(const TypeHandle& other) const;

		StdSharedPtr<THandle> type;
	};
}

// Needs to be defined as early as possible (before first use)
namespace std
{
	template<> struct hash<Noctis::TypeHandle>
	{
		size_t operator()(const Noctis::TypeHandle& handle) const noexcept
		{
			std::hash<std::shared_ptr<Noctis::THandle>> hasher;
			return hasher(handle.type);
		}
	};
}

namespace Noctis
{

	struct Bounds
	{
		StdVector<TypeHandle> bounds;

		TypeHandle NarrowType(TypeHandle type) const;
	};
	
	struct BoundsInfo
	{
		StdUnorderedMap<TypeSPtr, Bounds> bounds;

		void Merge(BoundsInfo& other);

		const Bounds& GetBounds(TypeHandle type);
		const Bounds& GetBounds(TypeSPtr type);

		Bounds& GetOrAddBounds(TypeHandle type);
		Bounds& GetOrAddBounds(TypeSPtr type);

		void RemoveUnboundTypes();
	};

	// hasFuzzyCompare: does the type have a fuzzy comparison type (has value generics)
#define TYPE_BASE_DATA \
	TypeMod mod : 7; \
	bool hasFuzzyCompare : 1;\
	u16 align = 0; \
	u64 size = 0


	struct BaseType
	{
		BaseType()
			: mod(TypeMod::None)
			, hasFuzzyCompare(false)
		{}

		TYPE_BASE_DATA;
	};


	struct BuiltinType
	{
		BuiltinType(TypeMod mod, BuiltinTypeKind builtin)
			: mod(mod)
			, builtin(builtin)
			, hasFuzzyCompare(false)
		{}

		bool IsSigned() const;
		bool IsFp() const;

		TYPE_BASE_DATA;
		BuiltinTypeKind builtin;
	};

	struct IdenType
	{
		IdenType(TypeMod mod, QualNameSPtr qualName);

		TYPE_BASE_DATA;
		QualNameSPtr qualName;
		SymbolWPtr sym;
	};

	struct PtrType
	{
		PtrType(TypeMod mod, TypeHandle subType)
			: mod(mod)
			, subType(subType)
			, hasFuzzyCompare(subType.HasFuzzyCompare())
		{}

		TYPE_BASE_DATA;
		TypeHandle subType;
	};

	struct RefType
	{
		RefType(TypeMod mod, TypeHandle subType)
			: mod(mod)
			, subType(subType)
			, hasFuzzyCompare(subType.HasFuzzyCompare())
		{}

		TYPE_BASE_DATA;
		TypeHandle subType;
	};

	struct SliceType
	{
		SliceType(TypeMod mod, TypeHandle subType)
			: mod(mod)
			, subType(subType)
			, hasFuzzyCompare(subType.HasFuzzyCompare())
		{}

		TYPE_BASE_DATA;
		TypeHandle subType;
	};

	using ArrayExprType = StdVariant<AstExprSPtr, ITrExprSPtr>;
	struct ArrayType
	{
		ArrayType(TypeMod mod, TypeHandle subType, u64 size)
			: mod(mod)
			, subType(subType)
			, arrSize(size)
			, hasFuzzyCompare(true)
		{}

		ArrayType(TypeMod mod, TypeHandle subType, ArrayExprType expr)
			: mod(mod)
			, subType(subType)
			, arrSize(u64(-1))
			, expr(expr)
			, hasFuzzyCompare(subType.HasFuzzyCompare())
		{}

		TYPE_BASE_DATA;
		TypeHandle subType;
		u64 arrSize;
		ArrayExprType expr;
	};

	struct TupleType
	{
		TupleType(TypeMod mod, const StdVector<TypeHandle>& subTypes)
			: mod(mod)
			, subTypes(subTypes)
			, hasFuzzyCompare(false)
		{
			for (const TypeHandle& subType : subTypes)
			{
				if (subType.HasFuzzyCompare())
				{
					hasFuzzyCompare = true;
					break;
				}
			}
		}

		TYPE_BASE_DATA;
		StdVector<TypeHandle> subTypes;
		StdVector<u64> offsets;
	};

	struct OptType
	{
		OptType(TypeMod mod, TypeHandle subType)
			: mod(mod)
			, subType(subType)
			, hasFuzzyCompare(subType.HasFuzzyCompare())
		{}

		TYPE_BASE_DATA;
		TypeHandle subType;
	};

	struct FuncType
	{
		FuncType(TypeMod mod, const StdVector<TypeHandle>& paramTypes, TypeHandle retType)
			: mod(mod)
			, paramTypes(paramTypes)
			, retType(retType)
			, hasFuzzyCompare(false)
		{
			for (const TypeHandle& paramType : paramTypes)
			{
				if (paramType.HasFuzzyCompare())
				{
					hasFuzzyCompare = true;
					break;
				}
			}
			if (retType.IsValid())
				hasFuzzyCompare |= retType.HasFuzzyCompare();
		}

		TYPE_BASE_DATA;
		StdVector<TypeHandle> paramTypes;
		TypeHandle retType;
	};

	struct GenericType
	{
		GenericType(TypeMod mod, u16 id)
			: mod(mod)
			, id(id)
			, hasFuzzyCompare(false)
		{}

		TYPE_BASE_DATA;
		u16 id;
	};

#undef TYPE_BASE_DATA

	struct Type
	{
		Type();
		Type(BuiltinType builtin);
		Type(IdenType iden);
		Type(PtrType ptr);
		Type(RefType ref);
		Type(SliceType slice);
		Type(ArrayType arr);
		Type(TupleType tup);
		Type(OptType opt);
		Type(FuncType func);
		Type(GenericType generic);
		
		~Type();

		void CalculateSizeAlign();

		BuiltinType& AsBuiltin() { assert(typeKind == TypeKind::Builtin); return builtin; }
		IdenType& AsIden() { assert(typeKind == TypeKind::Iden); return iden; }
		PtrType& AsPtr() { assert(typeKind == TypeKind::Ptr); return ptr; }
		RefType& AsRef() { assert(typeKind == TypeKind::Ref); return ref; }
		SliceType& AsSlice() { assert(typeKind == TypeKind::Slice); return slice; }
		ArrayType& AsArray() { assert(typeKind == TypeKind::Array); return arr; }
		TupleType& AsTuple() { assert(typeKind == TypeKind::Tuple); return tup; }
		OptType& AsOpt() { assert(typeKind == TypeKind::Opt); return opt; }
		FuncType& AsFunc() { assert(typeKind == TypeKind::Func); return func; }
		GenericType& AsGeneric() { assert(typeKind == TypeKind::Generic); return generic; }

		TypeMod Mod() const { return base.mod; }
		u16 Align() const { return base.align; }
		u64 Size() const { return base.size; }
		bool HasFuzzyCompare() const { return base.hasFuzzyCompare; }

		TypeKind typeKind;

		union
		{
			BaseType base;
			BuiltinType builtin;
			IdenType iden;
			PtrType ptr;
			RefType ref;
			SliceType slice;
			ArrayType arr;
			TupleType tup;
			OptType opt;
			FuncType func;
			GenericType generic;
		};

	};
}



namespace Noctis
{
	class TypeRegistry
	{
	public:
		TypeRegistry();

		bool IsType(TypeHandle handle, TypeKind kind);
		StdString ToString(TypeHandle handle);
		StdString ToString(TypeSPtr type);

		bool CompareTypes(TypeHandle first, TypeHandle second);
		bool CompareTypesNonFuzzy(TypeHandle first, TypeHandle second);
		bool MatchTypes(TypeHandle toMatch, TypeHandle type, Module& mod);

		bool CanPassTo(TypeHandle param, TypeHandle arg);
		void SetIdenSym(QualNameSPtr qualName, SymbolWPtr sym);
		void SetAliasType(TypeHandle alias, TypeHandle type);

		TypeHandle ReplaceSubType(TypeHandle orig, TypeHandle toReplace, TypeHandle replacement);

		StdVector<TypeHandle> GetSubTypes(TypeHandle handle, TypeKind kind);

		void CalculateSizeAlign();

		i64 ScorePossibleVariant(TypeHandle type, TypeSPtr candidate, BoundsInfo& boundsInfo);
		StdVector<TypeSPtr> GetBestVariants(TypeHandle type, const StdVector<TypeSPtr>& candidates, BoundsInfo& boundsInfo);

		StdVector<TypeSPtr> ExtractGenerics(TypeSPtr type);

		TypeHandle GetGeneralizedType(TypeHandle handle);

		TypeHandle Builtin(TypeMod mod, BuiltinTypeKind builtin);
		TypeHandle Iden(TypeMod mod, QualNameSPtr qualName);
		TypeHandle Ptr(TypeMod mod, TypeHandle subType);
		TypeHandle Ref(TypeMod mod, TypeHandle subType);
		TypeHandle Slice(TypeMod mod, TypeHandle subType);
		TypeHandle Opt(TypeMod mod, TypeHandle subType);

		TypeHandle Array(TypeMod mod, TypeHandle subType, u64 size);
		TypeHandle Array(TypeMod mod, TypeHandle subType, ArrayExprType expr);

		TypeHandle Tuple(TypeMod mod, const StdVector<TypeHandle>& subTypes);

		TypeHandle Func(TypeMod mod, const StdVector<TypeHandle>& params, TypeHandle ret);

		TypeHandle Generic(TypeMod mod, u16 id);

		TypeHandle Mod(TypeMod mod, TypeHandle handle);

		TypeHandle Fuzzy(TypeHandle handle);
	private:
		TypeHandle CreateHandle(TypeSPtr type);
		TypeHandle CreateHandle(Type* pType);

		void ExtractGenerics(TypeSPtr type, StdVector<TypeSPtr>& gens);

		QualNameSPtr ReplaceSubType(QualNameSPtr qualName, TypeHandle toReplace, TypeHandle replacement);
		TypeDisambiguationSPtr ReplaceSubType(TypeDisambiguationSPtr disambig, TypeHandle toReplace, TypeHandle replacement);

		void GetSubTypes(TypeHandle handle, TypeKind kind, StdVector<TypeHandle>& foundTypes);
		void GetSubTypes(QualNameSPtr qualName, TypeKind kind, StdVector<TypeHandle>& foundTypes);
		void GetSubTypes(TypeDisambiguationSPtr disambig, TypeKind kind, StdVector<TypeHandle>& foundTypes);
		
		static constexpr u8 m_ModCount = u8(TypeMod::Count);

		StdArray<StdArray<TypeHandle, m_ModCount>, u8(BuiltinTypeKind::Count)> m_BuiltinMapping;
		StdUnorderedMap<QualNameSPtr, StdArray<TypeHandle, m_ModCount>> m_IdenMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_PtrMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_RefMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_SliceMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_OptMapping;

		StdUnorderedMap<TypeHandle, StdUnorderedMap<ArrayExprType, StdArray<TypeHandle, m_ModCount>>> m_ArrayMapping;
		StdUnorderedMap<TypeHandle, StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>> m_ArrayMappingKnownSize;

		TypeHandle m_EmptyTupleHandle;
		StdUnorderedMap<u64, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>> m_TupleMapping;
		StdUnorderedMap<u64, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>> m_FuncMapping;

		StdVector<StdArray<TypeHandle, m_ModCount>> m_GenericMapping;

		StdVector<TypeSPtr> m_Types;
	};
}
