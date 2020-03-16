#pragma once
#include "defs.hpp"

namespace Noctis
{
	FWDECL_CLASS_SPTR(QualName);
	
	enum class TypeKind : u8
	{
		Builtin,
		Iden,
		Ptr,
		Ref,
		Slice,
		Array,
		Tuple,
		Opt,
		Compound
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

	enum class TypeMod
	{
		None,
		Const,
		Count,
	};
	StdStringView TypeModToString(TypeMod mod);

	using TypeHandle = u64;

	struct BuiltinType;
	struct IdenType;
	struct PtrType;
	struct RefType;
	struct SliceType;
	struct ArrayType;
	struct TupleType;
	struct OptType;
	struct CompoundType;

	struct Type
	{
		Type(TypeKind kind, TypeMod mod);

		BuiltinType& AsBuiltin() { return *reinterpret_cast<BuiltinType*>(this); }
		IdenType& AsIden() { return *reinterpret_cast<IdenType*>(this); }
		PtrType& AsPtr() { return *reinterpret_cast<PtrType*>(this); }
		RefType& AsRef() { return *reinterpret_cast<RefType*>(this); }
		SliceType& AsSlice() { return *reinterpret_cast<SliceType*>(this); }
		ArrayType& AsArray() { return *reinterpret_cast<ArrayType*>(this); }
		TupleType& AsTuple() { return *reinterpret_cast<TupleType*>(this); }
		OptType& AsOpt() { return *reinterpret_cast<OptType*>(this); }
		CompoundType& AsCompound() { return *reinterpret_cast<CompoundType*>(this); }

		TypeKind typeKind;
		TypeMod mod;
	};
	using TypeSPtr = StdSharedPtr<Type>;

	struct BuiltinType : Type
	{
		BuiltinType(TypeMod mod, BuiltinTypeKind builtin);

		BuiltinTypeKind builtin;
	};
	
	struct IdenType : Type
	{
		IdenType(TypeMod mod, QualNameSPtr qualName);
		
		QualNameSPtr qualName;
	};

	struct PtrType : Type
	{
		PtrType(TypeMod mod, TypeHandle subType);

		TypeHandle subType;
	};

	struct RefType : Type
	{
		RefType(TypeMod mod, TypeHandle subType);

		TypeHandle subType;
	};

	struct SliceType : Type
	{
		SliceType(TypeMod mod, TypeHandle subType);

		TypeHandle subType;
	};

	struct ArrayType : Type
	{
		ArrayType(TypeMod mod, TypeHandle subType, u64 size);
		ArrayType(TypeMod mod, TypeHandle subType, StdSharedPtr<void> expr);

		bool sizeKnown;
		TypeHandle subType;
		u64 size;
		StdSharedPtr<void> expr;
	};

	struct TupleType : Type
	{
		TupleType(TypeMod mod, const StdVector<TypeHandle>& subTypes);

		StdVector<TypeHandle> subTypes;
	};

	struct OptType : Type
	{
		OptType(TypeMod mod, TypeHandle subType);

		TypeHandle subType;
	};

	struct CompoundType : Type
	{
		CompoundType(TypeMod mod, const StdVector<TypeHandle>& subTypes);

		StdVector<TypeHandle> subTypes;
	};

	class TypeRegistry
	{
	public:
		TypeRegistry();

		bool IsType(TypeHandle handle, TypeKind kind);
		TypeSPtr GetType(TypeHandle handle);
		StdString ToString(TypeHandle handle);


		TypeHandle Builtin(TypeMod mod, BuiltinTypeKind builtin);
		TypeHandle Iden(TypeMod mod, QualNameSPtr qualName);
		TypeHandle Ptr(TypeMod mod, TypeHandle subType);
		TypeHandle Ref(TypeMod mod, TypeHandle subType);
		TypeHandle Slice(TypeMod mod, TypeHandle subType);
		TypeHandle Opt(TypeMod mod, TypeHandle subType);

		TypeHandle Array(TypeMod mod, TypeHandle subType, u64 size);
		TypeHandle Array(TypeMod mod, TypeHandle subType, StdSharedPtr<void> expr);

		TypeHandle Tuple(TypeMod mod, const StdVector<TypeHandle>& subTypes);
		TypeHandle Compound(TypeMod mod, const StdVector<TypeHandle>& subTypes);


		TypeHandle Mod(TypeMod mod, TypeHandle handle);

	private:
		static constexpr u8 m_ModCount = u8(TypeMod::Count);
		
		StdArray<StdArray<TypeHandle, m_ModCount>, u8(BuiltinTypeKind::Count)> m_BuiltinMapping;
		StdUnorderedMap<QualNameSPtr, StdArray<TypeHandle, m_ModCount>> m_IdenMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_PtrMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_RefMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_SliceMapping;
		StdUnorderedMap<TypeHandle, StdArray<TypeHandle, m_ModCount>> m_OptMapping;
		
		StdUnorderedMap<TypeHandle, StdUnorderedMap<StdSharedPtr<void>, StdArray<TypeHandle, m_ModCount>>> m_ArrayMapping;
		StdUnorderedMap<TypeHandle, StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>> m_ArrayMappingKnownSize;

		TypeHandle m_EmptyTupleHandle;
		StdUnorderedMap<TypeHandle, StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>> m_TupleMapping;
		
		StdUnorderedMap<TypeHandle, StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>> m_CompoundMapping;

		StdVector<TypeSPtr> m_Types;
		
	};
	
}
