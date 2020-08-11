#pragma once
#include "defs.hpp"

namespace Noctis
{
	struct Context;
	FWDECL_CLASS_SPTR(QualName);
	FWDECL_CLASS_SPTR(Iden);
	FWDECL_STRUCT_WPTR(Symbol);
	
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

	enum class TypeMod : u8
	{
		None,
		Mut, 
		Count,
	};
	StdStringView TypeModToString(TypeMod mod);

	//using TypeHandle = u64;
	//constexpr TypeHandle InvalidTypeHandle = TypeHandle(-1);

	FWDECL_STRUCT_SPTR(Type);
	struct BuiltinType;
	struct IdenType;
	struct PtrType;
	struct RefType;
	struct SliceType;
	struct ArrayType;
	struct TupleType;
	struct OptType;
	struct CompoundType;
	struct FuncType;
	struct GenericType;
	class TypeRegistry;

	struct Type
	{
		Type(TypeKind kind, TypeMod mod);

		void CalculateSizeAlign(TypeRegistry& typeReg);

		StdVector<TypeSPtr> GetIdenSubtypes();

		BuiltinType& AsBuiltin() { return *reinterpret_cast<BuiltinType*>(this); }
		IdenType& AsIden() { return *reinterpret_cast<IdenType*>(this); }
		PtrType& AsPtr() { return *reinterpret_cast<PtrType*>(this); }
 		RefType& AsRef() { return *reinterpret_cast<RefType*>(this); }
		SliceType& AsSlice() { return *reinterpret_cast<SliceType*>(this); }
		ArrayType& AsArray() { return *reinterpret_cast<ArrayType*>(this); }
		TupleType& AsTuple() { return *reinterpret_cast<TupleType*>(this); }
		OptType& AsOpt() { return *reinterpret_cast<OptType*>(this); }
		CompoundType& AsCompound() { return *reinterpret_cast<CompoundType*>(this); }
		FuncType& AsFunc() { return *reinterpret_cast<FuncType*>(this); }
		GenericType& AsGeneric() { return *reinterpret_cast<GenericType*>(this); }

		TypeKind typeKind;
		TypeMod mod;

		u64 size;
		u16 alignment;
	};
	using TypeSPtr = StdSharedPtr<Type>;

	struct THandle
	{
		THandle(TypeSPtr type)
			: type(type)
		{
		}
		
		TypeSPtr type;
		
		BuiltinType& AsBuiltin() { return type->AsBuiltin(); }
		IdenType& AsIden() { return type->AsIden(); }
		PtrType& AsPtr() { return type->AsPtr(); }
		RefType& AsRef() { return type->AsRef(); }
		SliceType& AsSlice() { return type->AsSlice(); }
		ArrayType& AsArray() { return type->AsArray(); }
		TupleType& AsTuple() { return type->AsTuple(); }
		OptType& AsOpt() { return type->AsOpt(); }
		CompoundType& AsCompound() { return type->AsCompound(); }
		FuncType& AsFunc() { return type->AsFunc(); }
		GenericType& AsGeneric() { return type->AsGeneric(); }
	};
	using TypeHandle = StdSharedPtr<THandle>;
	
	

	struct BuiltinType : Type
	{
		BuiltinType(TypeMod mod, BuiltinTypeKind builtin);

		bool IsSigned() const;
		bool IsFp() const;
		
		BuiltinTypeKind builtin;
	};
	
	struct IdenType : Type
	{
		IdenType(TypeMod mod, QualNameSPtr qualName);
		
		QualNameSPtr qualName;
		SymbolWPtr sym;
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
		StdVector<u64> offsets;
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

	struct FuncType : Type
	{
		FuncType(TypeMod mod, const StdVector<TypeHandle>& paramTypes, TypeHandle retType);

		StdVector<TypeHandle> paramTypes;
		TypeHandle retType;
	};

	struct GenericType : Type
	{
		GenericType(TypeMod mod, IdenSPtr qualName, const StdVector<TypeHandle>& constraints);

		IdenSPtr iden;
		StdVector<TypeHandle> constraints;
	};

	class TypeRegistry
	{
	public:
		TypeRegistry(Context* pCtx);

		bool IsType(TypeHandle handle, TypeKind kind);
		StdString ToString(TypeHandle handle);
		StdString ToString(TypeSPtr type);

		bool AreTypesEqual(TypeHandle first, TypeHandle second);
		bool CanPassTo(TypeHandle param, TypeHandle arg);
		void SetIdenSym(QualNameSPtr qualName, SymbolWPtr sym);
		void SetAliasType(TypeHandle alias, TypeHandle type);

		TypeHandle ReplaceSubType(TypeHandle orig, TypeHandle toReplace, TypeHandle replacement);

		StdVector<TypeHandle> GetSubTypes(TypeHandle handle, TypeKind kind);

		void CalculateSizeAlign();

		i64 ScorePossibleVariant(TypeSPtr type, TypeSPtr candidate);
		StdVector<TypeSPtr> GetBestVariants(TypeHandle type, const StdVector<TypeSPtr>& candidates);

		StdVector<TypeSPtr> ExtractGenerics(TypeSPtr type);

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

		TypeHandle Func(TypeMod mod, const StdVector<TypeHandle>& params, TypeHandle ret);
		
		TypeHandle Generic(TypeMod mod, IdenSPtr qualName, const StdVector<TypeHandle>& constraints);

		TypeHandle Mod(TypeMod mod, TypeHandle handle);
	private:
		TypeHandle CreateHandle(TypeSPtr type);
		TypeHandle CreateHandle(Type* pType);
		
		void ExtractGenerics(TypeSPtr type, StdVector<TypeSPtr>& gens);
		
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
		StdUnorderedMap<u64, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>> m_TupleMapping;
		StdUnorderedMap<u64, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>> m_CompoundMapping;
		StdUnorderedMap<u64, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>> m_FuncMapping;

		StdUnorderedMap<IdenSPtr, StdVector<StdArray<TypeHandle, m_ModCount>>> m_GenericMapping;
		
		StdVector<TypeSPtr> m_Types;

		Context* m_pCtx;
	};

	bool AreTypesEqual(TypeHandle t0, TypeHandle t1);
	
}
