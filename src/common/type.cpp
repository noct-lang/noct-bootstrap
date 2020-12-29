#include "type.hpp"

#include <cassert>

#include "module/symbol.hpp"
#include "qualname.hpp"
#include "utils.hpp"
#include "context.hpp"
#include "module/module.hpp"


namespace Noctis
{
	IdenType::IdenType(TypeMod mod, QualNameSPtr qualName)
		: mod(mod)
		, qualName(qualName)
		, hasFuzzyCompare(false)
	{
		for (IdenSPtr iden : qualName->Idens())
		{
			if (!iden->Generics().empty())
				continue;

			for (IdenGeneric& idenGen : iden->Generics())
			{
				if (!idenGen.isType)
				{
					hasFuzzyCompare = true;
					break;
				}
			}
			if (hasFuzzyCompare)
				break;
		}

		if (!hasFuzzyCompare && qualName->Disambiguation())
		{
			hasFuzzyCompare = qualName->Disambiguation()->Type().AsBase().hasFuzzyCompare;
		}
	}

	bool IsBuiltinInteger(BuiltinTypeKind kind)
	{
		switch (kind)
		{
		case BuiltinTypeKind::I8:
		case BuiltinTypeKind::I16:
		case BuiltinTypeKind::I32:
		case BuiltinTypeKind::I64:
		case BuiltinTypeKind::I128:
		case BuiltinTypeKind::ISize:
		case BuiltinTypeKind::U8:
		case BuiltinTypeKind::U16:
		case BuiltinTypeKind::U32:
		case BuiltinTypeKind::U64:
		case BuiltinTypeKind::U128:
		case BuiltinTypeKind::USize:
			return true;
		default:
			return false;
		}
	}

	bool IsBuiltinSigned(BuiltinTypeKind kind)
	{
		switch (kind)
		{
		case BuiltinTypeKind::I8:
		case BuiltinTypeKind::I16:
		case BuiltinTypeKind::I32:
		case BuiltinTypeKind::I64:
		case BuiltinTypeKind::I128:
		case BuiltinTypeKind::ISize:
			return true;
		default:
			return false;
		}
	}

	bool IsBuiltinUnsigned(BuiltinTypeKind kind)
	{
		switch (kind)
		{
		case BuiltinTypeKind::U8:
		case BuiltinTypeKind::U16:
		case BuiltinTypeKind::U32:
		case BuiltinTypeKind::U64:
		case BuiltinTypeKind::U128:
		case BuiltinTypeKind::USize:
			return true;
		default:
			return false;
		}
	}

	bool IsBuiltinFloat(BuiltinTypeKind kind)
	{
		switch (kind)
		{
		case BuiltinTypeKind::F16:
		case BuiltinTypeKind::F32:
		case BuiltinTypeKind::F64:
		case BuiltinTypeKind::F128:
			return true;
		default:
			return false;
		}
	}

	u8 GetBuiltinBytes(BuiltinTypeKind kind)
	{
		switch (kind)
		{
		case BuiltinTypeKind::Bool:
		case BuiltinTypeKind::I8:
		case BuiltinTypeKind::U8:
			return 1;
		case BuiltinTypeKind::I16:
		case BuiltinTypeKind::U16:
		case BuiltinTypeKind::F16:
			return 2;
		case BuiltinTypeKind::Char:
		case BuiltinTypeKind::I32:
		case BuiltinTypeKind::U32:
		case BuiltinTypeKind::F32:
			return 4;
		case BuiltinTypeKind::I64:
		case BuiltinTypeKind::U64:
		case BuiltinTypeKind::F64:
			return 8;
		case BuiltinTypeKind::U128:
		case BuiltinTypeKind::I128:
		case BuiltinTypeKind::F128:
			return 16;
		case BuiltinTypeKind::USize:
		case BuiltinTypeKind::ISize:
			// TODO: arch specific size
			return 8;
		default:
			return 0;
		}
	}

	/*
		switch (kind)
		{
		case BuiltinTypeKind::Bool:
		case BuiltinTypeKind::Char:
		case BuiltinTypeKind::I8:
		case BuiltinTypeKind::I16:
		case BuiltinTypeKind::I32:
		case BuiltinTypeKind::I64:
		case BuiltinTypeKind::I128:
		case BuiltinTypeKind::ISize:
		case BuiltinTypeKind::U8:
		case BuiltinTypeKind::U16:
		case BuiltinTypeKind::U32:
		case BuiltinTypeKind::U64:
		case BuiltinTypeKind::U128:
		case BuiltinTypeKind::USize:
		case BuiltinTypeKind::F16:
		case BuiltinTypeKind::F32:
		case BuiltinTypeKind::F64:
		case BuiltinTypeKind::F128:
			return true;
		default:
			return false;
		}
	 */

	StdStringView TypeModToString(TypeMod mod)
	{
		switch (mod)
		{
		case TypeMod::Mut: return "mut ";
		default: return "";
		}
	}

	TypeKind TypeHandle::Kind() const
	{
		return type ? type->type->typeKind : TypeKind::Invalid;
	}

	TypeSPtr TypeHandle::Type()
	{
		return type ? type->type : nullptr;
	}

	const TypeSPtr TypeHandle::Type() const
	{
		return type ? type->type : nullptr;
	}

	BaseType& TypeHandle::AsBase()
	{
		assert(IsValid());
		return Type()->AsBase();
	}

	const BaseType& TypeHandle::AsBase() const
	{
		assert(IsValid());
		return Type()->AsBase();
	}

	BuiltinType& TypeHandle::AsBuiltin()
	{
		assert(IsValid());
		return Type()->AsBuiltin();
	}

	IdenType& TypeHandle::AsIden()
	{
		assert(IsValid());
		return Type()->AsIden();
	}

	const IdenType& TypeHandle::AsIden() const
	{
		assert(IsValid());
		return Type()->AsIden();
	}

	PtrType& TypeHandle::AsPtr()
	{
		assert(IsValid());
		return Type()->AsPtr();
	}

	RefType& TypeHandle::AsRef()
	{
		assert(IsValid());
		return Type()->AsRef();
	}

	SliceType& TypeHandle::AsSlice()
	{
		assert(IsValid());
		return Type()->AsSlice();
	}

	ArrayType& TypeHandle::AsArray()
	{
		assert(IsValid());
		return Type()->AsArray();
	}

	TupleType& TypeHandle::AsTuple()
	{
		assert(IsValid());
		return Type()->AsTuple();
	}

	OptType& TypeHandle::AsOpt()
	{
		assert(IsValid());
		return Type()->AsOpt();
	}

	CompoundType& TypeHandle::AsCompound()
	{
		assert(IsValid());
		return Type()->AsCompound();
	}

	FuncType& TypeHandle::AsFunc()
	{
		assert(IsValid());
		return Type()->AsFunc();
	}

	GenericType& TypeHandle::AsGeneric()
	{
		assert(IsValid());
		return Type()->AsGeneric();
	}

	StdString TypeHandle::ToString()
	{
		if (IsValid())
			return pReg->ToString(*this);
		return "__unknown__";
	}

	bool TypeHandle::operator==(const TypeHandle& other) const
	{
		if (!IsValid() || !other.IsValid())
			return false;
		return pReg->CompareTypes(*this, other);
	}

	bool TypeHandle::operator!=(const TypeHandle& other) const
	{
		return !(*this == other);
	}

	Type::Type()
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Invalid;
	}

	Type::Type(BuiltinType builtin)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Builtin;
		new (&this->builtin) BuiltinType(std::move(builtin));
	}

	Type::Type(IdenType iden)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Iden;
		new (&this->iden) IdenType(std::move(iden));
	}

	Type::Type(PtrType ptr)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Ptr;
		new (&this->ptr) PtrType(std::move(ptr));
	}

	Type::Type(RefType ref)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Ref;
		new (&this->ref) RefType(std::move(ref));
	}

	Type::Type(SliceType slice)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Slice;
		new (&this->slice) SliceType(std::move(slice));
	}

	Type::Type(ArrayType arr)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Array;
		new (&this->arr) ArrayType(std::move(arr));
	}

	Type::Type(TupleType tup)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Tuple;
		new (&this->tup) TupleType(std::move(tup));
	}

	Type::Type(OptType opt)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Opt;
		new (&this->opt) OptType(std::move(opt));
	}

	Type::Type(CompoundType compound)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Compound;
		new (&this->compound) CompoundType(std::move(compound));
	}

	Type::Type(FuncType func)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Func;
		new (&this->func) FuncType(std::move(func));
	}

	Type::Type(GenericType generic)
	{
		memset(this, 0, sizeof(Type));
		typeKind = TypeKind::Generic;
		new (&this->generic) GenericType(std::move(generic));
	}

	Type::~Type()
	{
		switch (typeKind)
		{
		case TypeKind::Invalid: break;
		case TypeKind::Builtin: builtin.~BuiltinType(); break;
		case TypeKind::Iden: iden.~IdenType(); break;
		case TypeKind::Ptr: ptr.~PtrType(); break;
		case TypeKind::Ref: ref.~RefType(); break;
		case TypeKind::Slice: slice.~SliceType(); break;
		case TypeKind::Array: arr.~ArrayType(); break;
		case TypeKind::Tuple: tup.~TupleType(); break;
		case TypeKind::Opt: opt.~OptType(); break;
		case TypeKind::Compound: compound.~CompoundType(); break;
		case TypeKind::Func: func.~FuncType(); break;
		case TypeKind::Generic: generic.~GenericType(); break;
		default: ;
		}
	}

	void Type::CalculateSizeAlign(TypeRegistry& typeReg)
	{
		switch (typeKind)
		{
		case TypeKind::Builtin:
		{
			BuiltinType& builtin = AsBuiltin();
			base.size = base.align = GetBuiltinBytes(builtin.builtin);
			break;
		}
		case TypeKind::Iden:
		{
			IdenType& idenType = AsIden();
			SymbolSPtr sym = idenType.sym.lock();
			if (!sym)
				break;
			
			sym->CalculateSizeAlignOffset();
			base.align = sym->aligment;
			base.size = sym->size;
			break;
		}
		case TypeKind::Ptr: base.size = base.align = 8; break; // TODO: arch specific size
		case TypeKind::Ref: base.size = base.align = 8; break; // TODO: arch specific size
		case TypeKind::Slice: base.size = base.align = 16; break; // TODO: arch specific size
		case TypeKind::Array:
		{
			TypeSPtr subType = AsArray().subType.Type();
			if (subType->base.size == 0)
				subType->CalculateSizeAlign(typeReg);

			base.align = subType->base.align;
			base.size = subType->base.size * AsArray().size;

			break;
		}
		case TypeKind::Tuple:
		{
			TupleType& tup = AsTuple();
			
			for (TypeHandle subHandle : AsTuple().subTypes)
			{
				TypeSPtr subType = subHandle.Type();
				if (subType->base.size == 0)
					subType->CalculateSizeAlign(typeReg);

				if (base.align < subType->base.align)
					base.align = subType->base.align;
				
				u64 alignOffset = (base.align + base.size) & (subType->base.align - 1);
				base.size += alignOffset == 0 ? 0 : subType->base.align - alignOffset;
				tup.offsets.push_back(base.size);
				base.size += subType->base.size;
			}
			break;
		}
		case TypeKind::Opt:
		{
			TypeSPtr subType = AsOpt().subType.Type();
			if (subType->base.size == 0)
				subType->CalculateSizeAlign(typeReg);

			base.align = subType->base.align;
			base.size = subType->base.size + 1; // TODO: is this correct?
			
			break;
		}
		case TypeKind::Func: base.size = base.align = 8; break; // TODO: arch specific size
		default: ;
		}
	}

	bool BuiltinType::IsSigned() const
	{
		return IsBuiltinSigned(builtin);
	}

	bool BuiltinType::IsFp() const
	{
		return IsBuiltinFloat(builtin);
	}

	TypeRegistry::TypeRegistry(Context* pCtx)
		: m_BuiltinMapping()
		, m_pCtx(pCtx)
	{
		for (auto& subArr : m_BuiltinMapping)
			subArr.fill(TypeHandle{});

		m_EmptyTupleHandle = CreateHandle(new Type{ TupleType{ TypeMod::None, {} } });
	}

	bool TypeRegistry::IsType(TypeHandle handle, TypeKind kind)
	{
		TypeSPtr type = handle.Type();
		return type->typeKind == kind;
	}

	StdString TypeRegistry::ToString(TypeHandle handle)
	{
		TypeSPtr type = handle.Type();
		return ToString(type);
	}

	StdString TypeRegistry::ToString(TypeSPtr type)
	{
		if (!type)
			return "()";

		StdString mod{ TypeModToString(type->base.mod) };
		switch (type->typeKind)
		{
		case TypeKind::Builtin:
		{
			BuiltinType& builtin = type->AsBuiltin();
			switch (builtin.builtin)
			{
			case BuiltinTypeKind::Bool: return mod + "bool";
			case BuiltinTypeKind::Char: return mod + "char";
			case BuiltinTypeKind::I8: return mod + "i8";
			case BuiltinTypeKind::I16: return mod + "i16";
			case BuiltinTypeKind::I32: return mod + "i32";
			case BuiltinTypeKind::I64: return mod + "i64";
			case BuiltinTypeKind::I128: return mod + "i128";
			case BuiltinTypeKind::ISize: return mod + "isize";
			case BuiltinTypeKind::U8: return mod + "u8";
			case BuiltinTypeKind::U16: return mod + "u16";
			case BuiltinTypeKind::U32: return mod + "u32";
			case BuiltinTypeKind::U64: return mod + "u64";
			case BuiltinTypeKind::U128: return mod + "u128";
			case BuiltinTypeKind::USize: return mod + "usize";
			case BuiltinTypeKind::F16: return mod + "f16";
			case BuiltinTypeKind::F32: return mod + "f32";
			case BuiltinTypeKind::F64: return mod + "f64";
			case BuiltinTypeKind::F128: return mod + "f128";
			default: return "__unknown__builtin__";
			}
		}
		case TypeKind::Iden:
		{
			IdenType& iden = type->AsIden();
			StdString idenName = iden.qualName->ToString();
			return mod + idenName;
		}
		case TypeKind::Ptr:
		{
			PtrType& ptr = type->AsPtr();
			return mod + '*' + ToString(ptr.subType);
		}
		case TypeKind::Ref:
		{
			RefType& ref = type->AsRef();
			return mod + '&' + ToString(ref.subType);
		}
		case TypeKind::Slice:
		{
			SliceType& slice = type->AsSlice();
			return mod + "[]" + ToString(slice.subType);
		}
		case TypeKind::Array:
		{
			ArrayType& arr = type->AsArray();
			StdString tmp;
			if (arr.arrSize != u64(-1))
			{
				tmp = Format("[%llu]", arr.size);
			}
			else if (std::holds_alternative<ITrExprSPtr>(arr.expr) && std::get<ITrExprSPtr>(arr.expr)->exprKind == ITrExprKind::QualName)
			{
				tmp = '[';
				ITrExprSPtr expr = std::get<ITrExprSPtr>(arr.expr);
				ITrQualNameExpr& qualName = reinterpret_cast<ITrQualNameExpr&>(*expr);
				tmp += qualName.qualName->LastIden()->ToString();
				tmp += ']';
			}
			else
			{	
				tmp = "[...]";
			}

			return mod + tmp + ToString(arr.subType);
		}
		case TypeKind::Tuple:
		{
			TupleType& tup = type->AsTuple();
			StdString tmp;
			for (TypeHandle subType : tup.subTypes)
			{
				if (!tmp.empty())
					tmp += ", ";
				tmp += ToString(subType);
			}
			return '(' + tmp + ')';
		}
		case TypeKind::Opt: 
		{
			OptType& opt = type->AsOpt();
			return mod + '?' + ToString(opt.subType);
		}
		case TypeKind::Compound:
		{
			TupleType& tup = type->AsTuple();
			StdString tmp;
			for (TypeHandle subType : tup.subTypes)
			{
				if (!tmp.empty())
					tmp += " + ";
				tmp += ToString(subType);
			}
			return tmp;
		}
		case TypeKind::Generic:
		{
			GenericType& gen = type->AsGeneric();
			StdString idenName = gen.iden->ToString();
			return mod + idenName;
		}
		case TypeKind::Func:
		{
			FuncType& funcType = type->AsFunc();
			StdString params;
			for (TypeHandle paramType : funcType.paramTypes)
			{
				if (!params.empty())
					params += ',';
				params += ToString(paramType);
			}

			StdString ret;
			if (funcType.retType.IsValid())
				ret = ToString(funcType.retType);
			
			return mod + '(' + params + ")->(" + ret + ')';
		}
		default: return "()";
		}
	}

	bool TypeRegistry::CompareTypes(TypeHandle first, TypeHandle second)
	{
		if (!first.IsValid() || !second.IsValid())
			return false;

		TypeHandle fuzzy0 = Fuzzy(first);
		TypeHandle fuzzy1 = Fuzzy(second);
		
		return fuzzy0.Type() == fuzzy1.Type();
	}

	bool TypeRegistry::CompareTypesNonFuzzy(TypeHandle first, TypeHandle second)
	{
		if (!first.IsValid() || !second.IsValid())
			return false;
		return first.Type() == second.Type();
	}

	bool TypeRegistry::MatchTypes(TypeHandle toMatch, TypeHandle type, Module& mod)
	{
		if (CompareTypes(toMatch, type))
			return true;

		SymbolSPtr sym = mod.symTable.Find(type);
		if (!sym)
			return false;

		for (StdPair<QualNameSPtr, SymbolWPtr> interface : sym->interfaces)
		{
			if (CompareTypes(interface.second.lock()->type, type))
				return true;
		}

		return false;
	}

	bool TypeRegistry::CanPassTo(TypeHandle param, TypeHandle arg)
	{
		if (param == arg)
			return true;

		TypeSPtr paramType = param.Type();
		TypeSPtr argType = arg.Type();

		if (paramType->base.mod == TypeMod::Mut &&
				argType->base.mod == TypeMod::None &&
				Mod(TypeMod::None, param) == arg)
			return true;

		if (paramType->typeKind == argType->typeKind)
		{
			switch (paramType->typeKind)
			{
			case TypeKind::Builtin:
			case TypeKind::Iden:
			{
				// Should never get here
				break;
			}
			case TypeKind::Ptr:
			{
				TypeHandle paramSubType = paramType->AsPtr().subType;
				TypeHandle argSubType = argType->AsPtr().subType;

				if (paramSubType.Type()->base.mod == TypeMod::Mut &&
					argSubType.Type()->base.mod == TypeMod::None &&
					Mod(TypeMod::None, paramSubType) == argSubType)
					return true;
			}
			case TypeKind::Ref:
			{
				TypeHandle paramSubType = paramType->AsRef().subType;
				TypeHandle argSubType = argType->AsRef().subType;;

				if (paramSubType.Type()->base.mod == TypeMod::Mut &&
					argSubType.Type()->base.mod == TypeMod::None &&
					Mod(TypeMod::None, paramSubType) == argSubType)
					return true;
			}
			case TypeKind::Slice:
			{
				TypeHandle paramSubTypeHandle = paramType->AsSlice().subType;
				TypeHandle argSubTypeHandle = argType->AsSlice().subType;

				TypeSPtr paramSubType = paramSubTypeHandle.Type();
				TypeSPtr argSubType = argSubTypeHandle.Type();

				if (paramSubType->base.mod == TypeMod::Mut &&
					argSubType->base.mod == TypeMod::None &&
					Mod(TypeMod::None, paramSubTypeHandle) == argSubTypeHandle)
					return true;
			}
			case TypeKind::Array:
				// TODO
				break;
			case TypeKind::Tuple: break;
			case TypeKind::Opt: break;
			case TypeKind::Compound: break;
			case TypeKind::Func: break;
			default:;
			}
		}
		else if (argType->typeKind == TypeKind::Ref)
		{
			TypeHandle subArgType = argType->AsRef().subType;
			return CanPassTo(param, subArgType);
		}
		
		return false;
	}

	void TypeRegistry::SetIdenSym(QualNameSPtr qualName, SymbolWPtr sym)
	{
		auto it = m_IdenMapping.find(qualName);
		if (it == m_IdenMapping.end())
			it = m_IdenMapping.try_emplace(qualName, StdArray<TypeHandle, m_ModCount>{}).first;
		for (u8 i = 0; i < u8(TypeMod::Count); ++i)
		{
			TypeHandle& handle = it->second[i];
			if (!handle.IsValid())
			{
				TypeSPtr type{ new Type { IdenType{ TypeMod(i), qualName } } };
				type->AsIden().sym = sym;
				handle = CreateHandle(type);
				m_Types.push_back(type);
			}
			else
			{
				TypeSPtr type = handle.Type();
				type->AsIden().sym = sym;
			}
		}
	}

	void TypeRegistry::SetAliasType(TypeHandle alias, TypeHandle type)
	{
		alias.Type() = type.Type();
	}

	TypeHandle TypeRegistry::ReplaceSubType(TypeHandle orig, TypeHandle toReplace, TypeHandle replacement)
	{
		if (!orig.IsValid())
			return TypeHandle{};

		if (orig == toReplace)
			return replacement;

		TypeSPtr type = orig.Type();
		if (type->base.mod != TypeMod::None)
		{
			TypeSPtr toReplaceType = toReplace.Type();
			if (toReplaceType->base.mod == TypeMod::None)
			{
				toReplace = Mod(type->base.mod, toReplace);
				if (orig == toReplace)
					return replacement;
			}
		}

		switch (type->typeKind)
		{
		case TypeKind::Ptr:
		{
			PtrType& ptrType = type->AsPtr();
			TypeHandle subType = ReplaceSubType(ptrType.subType, toReplace, replacement);
			return Ptr(type->base.mod, subType);
		}
		case TypeKind::Ref:
		{
			RefType& refType = type->AsRef();
			TypeHandle subType = ReplaceSubType(refType.subType, toReplace, replacement);
			return Ref(type->base.mod, subType);
		}
		case TypeKind::Slice: 
		{
			SliceType& sliceType = type->AsSlice();
			TypeHandle subType = ReplaceSubType(sliceType.subType, toReplace, replacement);
			return Slice(type->base.mod, subType);
		}
		case TypeKind::Array:
		{
			ArrayType& arrType = type->AsArray();
			TypeHandle subType = ReplaceSubType(arrType.subType, toReplace, replacement);
			if (arrType.arrSize != u64(-1))
				return Array(type->base.mod, subType, arrType.size);
			return Array(type->base.mod, subType, arrType.expr);
		}
		case TypeKind::Tuple:
		{
			TupleType& tupType = type->AsTuple();
			StdVector<TypeHandle> subTypes;
			for (TypeHandle subType : tupType.subTypes)
			{
				TypeHandle tmp = ReplaceSubType(subType, toReplace, replacement);
				subTypes.push_back(tmp);
			}
			return Tuple(type->base.mod, subTypes);
		}
		case TypeKind::Opt:
		{
			OptType& ptrType = type->AsOpt();
			TypeHandle subType = ReplaceSubType(ptrType.subType, toReplace, replacement);
			return Ptr(type->base.mod, subType);
		}
		case TypeKind::Compound:
		{
			CompoundType& compType = type->AsCompound();
			StdVector<TypeHandle> subTypes;
			for (TypeHandle subType : compType.subTypes)
			{
				TypeHandle tmp = ReplaceSubType(subType, toReplace, replacement);
				subTypes.push_back(tmp);
			}
			return Compound(type->base.mod, subTypes);
		}
		case TypeKind::Func:
		{
			FuncType& funcType = type->AsFunc();
			StdVector<TypeHandle> paramTypes;
			for (TypeHandle subType : funcType.paramTypes)
			{
				TypeHandle tmp = ReplaceSubType(subType, toReplace, replacement);
				paramTypes.push_back(tmp);
			}
			TypeHandle retType = ReplaceSubType(funcType.retType, toReplace, replacement);
			return Func(type->base.mod, paramTypes, retType);
		}
		case TypeKind::Generic:
		{
			if (toReplace.Type()->typeKind != TypeKind::Generic)
				return orig;
			
			GenericType& genOrig = orig.AsGeneric();
			GenericType& genFrom = toReplace.AsGeneric();

			if (genOrig.iden == genFrom.iden)
				return replacement;
		}
		/*case TypeKind::Iden:
		{
			if (toReplace->type->typeKind != TypeKind::Iden)
				return orig;
			
			IdenType& idenTypeOrig = orig->AsIden();
			IdenType& idenTypeFrom = toReplace->AsIden();

			QualNameSPtr origQualName = idenTypeOrig.qualName;
			QualNameSPtr fromQualName = idenTypeFrom.qualName;
			
			IdenSPtr origIden = origQualName->LastIden();
			IdenSPtr fromIden = fromQualName->LastIden();

			if (origQualName->Base() == fromQualName->Base() &&
				origIden->Name() == fromIden->Name())
			{
				
			}
		}*/
		default:
			return orig;
		}
	}

	StdVector<TypeHandle> TypeRegistry::GetSubTypes(TypeHandle handle, TypeKind kind)
	{
		StdVector<TypeHandle> res;
		TypeSPtr type = handle.Type();

		switch (type->typeKind)
		{
		case TypeKind::Ptr:
			res = GetSubTypes(type->AsPtr().subType, kind);
			break;
		case TypeKind::Ref:
			res = GetSubTypes(type->AsRef().subType, kind);
			break;
		case TypeKind::Slice:
			res = GetSubTypes(type->AsSlice().subType, kind);
			break;
		case TypeKind::Array:
			res = GetSubTypes(type->AsArray().subType, kind);
			break;
		case TypeKind::Tuple:
		{
			TupleType& tupType = type->AsTuple();
			for (TypeHandle subType : tupType.subTypes)
			{
				StdVector<TypeHandle> tmp = GetSubTypes(subType, kind);
				res.insert(res.end(), tmp.begin(), tmp.end());
			}
			break;
		}
		case TypeKind::Opt:
			res = GetSubTypes(type->AsOpt().subType, kind);
			break;
		case TypeKind::Compound:
		{
			CompoundType& compType = type->AsCompound();
			for (TypeHandle subType : compType.subTypes)
			{
				StdVector<TypeHandle> tmp = GetSubTypes(subType, kind);
				res.insert(res.end(), tmp.begin(), tmp.end());
			}
			break;
		}
		case TypeKind::Func:
		{
			FuncType& funcType = type->AsFunc();
			for (TypeHandle subType : funcType.paramTypes)
			{
				StdVector<TypeHandle> tmp = GetSubTypes(subType, kind);
				res.insert(res.end(), tmp.begin(), tmp.end());
			}
			if (funcType.retType.IsValid())
			{
				StdVector<TypeHandle> tmp = GetSubTypes(funcType.retType, kind);
				res.insert(res.end(), tmp.begin(), tmp.end());
			}
			break;
		}
		default: ;
		}
		
		if (type->typeKind == kind)
			res.push_back(handle);
		
		return res;
	}

	void TypeRegistry::CalculateSizeAlign()
	{
		for (TypeSPtr type : m_Types)
		{
			if (!type->base.size)
				type->CalculateSizeAlign(*this);
		}
	}

	i64 TypeRegistry::ScorePossibleVariant(TypeSPtr type, TypeSPtr candidate)
	{
		if (type->typeKind == candidate->typeKind)
		{
			switch (type->typeKind)
			{
			case TypeKind::Builtin:
			case TypeKind::Iden:
			{
				if (type == candidate)
					return 0;
				break;
			}
			case TypeKind::Ptr:
			{
				TypeSPtr typeSub = type->AsPtr().subType.Type();
				TypeSPtr candidateSub = candidate->AsPtr().subType.Type();
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->base.mod == TypeMod::None && candidateSub->base.mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Ref:
			{
				TypeSPtr typeSub = type->AsRef().subType.Type();
				TypeSPtr candidateSub = candidate->AsRef().subType.Type();
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->base.mod == TypeMod::None && candidateSub->base.mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Slice:
			{
				TypeSPtr typeSub = type->AsSlice().subType.Type();
				TypeSPtr candidateSub = candidate->AsSlice().subType.Type();
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->base.mod == TypeMod::None && candidateSub->base.mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Array:
			{
				ArrayType& typeArr = type->AsArray();
				ArrayType& candidateArr = type->AsArray();

				if (candidateArr.arrSize != u64(-1))
				{
					if (typeArr.size != candidateArr.size)
						return -1;
				}
				else
				{
					// TODO: Value Generics
				}
				
				TypeSPtr typeSub = type->AsArray().subType.Type();
				TypeSPtr candidateSub = candidate->AsArray().subType.Type();

				i64 tmp = ScorePossibleVariant(typeSub, candidate);
				if (tmp == -1)
					return -1;
				if (typeSub->base.mod == TypeMod::None && candidateSub->base.mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Tuple:
			{
				StdVector<TypeHandle>& typeSubs = type->AsTuple().subTypes;
				StdVector<TypeHandle>& candidateSubs = candidate->AsTuple().subTypes;

				if (typeSubs.size() != candidateSubs.size())
					return -1;

				i64 totalScore = 0;
				for (usize i = 0; i < typeSubs.size(); ++i)
				{
					TypeSPtr typeSub = typeSubs[i].Type();
					TypeSPtr candidateSub = candidateSubs[i].Type();

					i64 tmp = ScorePossibleVariant(typeSub, candidate);
					if (tmp == -1)
						return -1;
					if (typeSub->base.mod == TypeMod::None && candidateSub->base.mod == TypeMod::Mut)
						return -1;
					totalScore += tmp;
				}
				return totalScore;
			}
			case TypeKind::Opt:
			{
				TypeSPtr typeSub = type->AsOpt().subType.Type();
				TypeSPtr candidateSub = candidate->AsOpt().subType.Type();
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->base.mod == TypeMod::None && candidateSub->base.mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Compound: return -1;
			case TypeKind::Func: /* TODO */ break;
			case TypeKind::Generic: break;
			default:;
			}
		}

		if (candidate->typeKind != TypeKind::Generic)
			return -1;
		if (type->base.mod == TypeMod::None && candidate->base.mod == TypeMod::Mut)
			return -1;

		// Check bounds
		GenericType& genType = candidate->AsGeneric();
		if (!genType.constraints.empty())
		{
			SymbolSPtr sym;
			if (type->typeKind == TypeKind::Iden)
			{
				sym = type->AsIden().sym.lock();
			}
			else
			{
				sym = m_pCtx->activeModule->symTable.Find(type);
			}
			
			if (sym)
			{
				for (TypeHandle constraint : genType.constraints)
				{
					QualNameSPtr constraintQualName = constraint.AsIden().qualName;

					bool found = false;
					for (StdPair<QualNameSPtr, SymbolWPtr> pair : sym->interfaces)
					{
						if (pair.first == constraintQualName)
						{
							found = true;
							break;
						}
					}
					if (found)
						continue;

					if (!sym->HasMarker(constraintQualName))
						return -1;
				}
			}
			else if (type->typeKind == TypeKind::Generic)
			{
				GenericType& typeGenType = type->AsGeneric();
				for (TypeHandle constraint : genType.constraints)
				{
					bool found = false;
					for (TypeHandle typeConstraint : typeGenType.constraints)
					{
						if (typeConstraint == constraint)
						{
							found = true;
							break;
						}
					}
					if (found)
						continue;

					QualNameSPtr constraintQualName = constraint.AsIden().qualName;
					if (!sym->HasMarker(constraintQualName))
						return -1;
				}
			}
			else
			{	
				return -1;
			}
		}
		
		return 1;
	}

	StdVector<TypeSPtr> TypeRegistry::GetBestVariants(TypeHandle type, const StdVector<TypeSPtr>& candidates)
	{
		TypeSPtr wantedType = type.Type();

		StdVector<TypeSPtr> bestCandidates;
		i64 bestScore = -1;
		
		for (TypeSPtr candidate : candidates)
		{
			i64 score = ScorePossibleVariant(wantedType, candidate);
			if (score == -1)
				continue;

			if (bestScore == -1)
			{
				bestScore = score;
				bestCandidates.push_back(candidate);
			}
			else if (score == bestScore)
			{
				bestCandidates.push_back(candidate);
			}
			else if (score < bestScore)
			{
				bestScore = score;
				bestCandidates.clear();
				bestCandidates.push_back(candidate);
			}
		}
		return bestCandidates;
	}

	StdVector<TypeSPtr> TypeRegistry::ExtractGenerics(TypeSPtr type)
	{
		StdVector<TypeSPtr> gens;
		ExtractGenerics(type, gens);
		return gens;
	}

	TypeHandle TypeRegistry::Builtin(TypeMod mod, BuiltinTypeKind builtin)
	{
		TypeHandle& handle = m_BuiltinMapping[u8(builtin)][u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ BuiltinType{ mod, builtin } });
		return handle;
	}

	TypeHandle TypeRegistry::Iden(TypeMod mod, QualNameSPtr qualName)
	{
		auto it = m_IdenMapping.find(qualName);
		if (it == m_IdenMapping.end())
		{
			it = m_IdenMapping.insert(std::pair{ qualName, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle{});
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ IdenType{ mod, qualName } });

		if (handle.AsBase().hasFuzzyCompare)
		{
			QualNameSPtr fuzzyQualName = qualName->Fuzzy();
			if (fuzzyQualName != qualName)
			{
				auto fuzzyIt = m_IdenMapping.find(fuzzyQualName);
				if (fuzzyIt == m_IdenMapping.end())
				{
					fuzzyIt = m_IdenMapping.insert(std::pair{ qualName, StdArray<TypeHandle, m_ModCount>{} }).first;
					fuzzyIt->second.fill(TypeHandle{});
					fuzzyIt->second[u8(mod)] = CreateHandle(new Type{ IdenType{ mod, fuzzyQualName } });
				}
				else if (!fuzzyIt->second[u8(mod)].IsValid())
					fuzzyIt->second[u8(mod)] = CreateHandle(new Type{ IdenType{ mod, fuzzyQualName } });
			}
		}

		return handle;
	}

	TypeHandle TypeRegistry::Ptr(TypeMod mod, TypeHandle subType)
	{
		auto it = m_PtrMapping.find(subType);
		if (it == m_PtrMapping.end())
		{
			it = m_PtrMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle{});
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ PtrType{ mod, subType } });
		return handle;
	}

	TypeHandle TypeRegistry::Ref(TypeMod mod, TypeHandle subType)
	{
		auto it = m_RefMapping.find(subType);
		if (it == m_RefMapping.end())
		{
			it = m_RefMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle{});
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ RefType{ mod, subType } });
		return handle;
	}

	TypeHandle TypeRegistry::Slice(TypeMod mod, TypeHandle subType)
	{
		auto it = m_SliceMapping.find(subType);
		if (it == m_SliceMapping.end())
		{
			it = m_SliceMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle{});
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ SliceType{ mod, subType } });
		return handle;
	}

	TypeHandle TypeRegistry::Opt(TypeMod mod, TypeHandle subType)
	{
		auto it = m_OptMapping.find(subType);
		if (it == m_OptMapping.end())
		{
			it = m_OptMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle{});
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ OptType{ mod, subType } });
		return handle;
	}

	TypeHandle TypeRegistry::Array(TypeMod mod, TypeHandle subType, u64 size)
	{
		auto it = m_ArrayMappingKnownSize.find(subType);
		if (it == m_ArrayMappingKnownSize.end())
			it = m_ArrayMappingKnownSize.insert(std::pair{ subType, StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>{} }).first;

		auto subIt = it->second.find(size);
		if (subIt == it->second.end())
		{
			subIt = it->second.insert(std::pair{ size, StdArray<TypeHandle, m_ModCount>{} }).first;
			subIt->second.fill(TypeHandle{});
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		handle = CreateHandle(new Type{ ArrayType{ mod, subType, size } });
		return handle;
	}

	TypeHandle TypeRegistry::Array(TypeMod mod, TypeHandle subType, ArrayExprType expr)
	{
		auto it = m_ArrayMapping.find(subType);
		if (it == m_ArrayMapping.end())
			it = m_ArrayMapping.insert(std::pair{ subType, StdUnorderedMap<ArrayExprType, StdArray<TypeHandle, m_ModCount>>{} }).first;

		auto subIt = it->second.find(expr);
		if (subIt == it->second.end())
		{
			subIt = it->second.insert(std::pair{ expr, StdArray<TypeHandle, m_ModCount>{} }).first;
			subIt->second.fill(TypeHandle{});
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle.IsValid())
			return handle;

		ArrayExprType fuzzyExpr{ ITrExprSPtr{} };
		auto fuzzyIt = it->second.find(fuzzyExpr);
		if (fuzzyIt == it->second.end())
		{
			fuzzyIt = it->second.insert(std::pair{ fuzzyExpr, StdArray<TypeHandle, m_ModCount>{} }).first;
			fuzzyIt->second.fill(TypeHandle{});
		}
		fuzzyIt->second[u8(mod)] = CreateHandle(new Type{ ArrayType{ mod, subType, expr } });

		handle = CreateHandle(new Type{ ArrayType{ mod, subType, expr } });
		return handle;
	}

	TypeHandle TypeRegistry::Tuple(TypeMod mod, const StdVector<TypeHandle>& subTypes)
	{
		if (subTypes.size() == 0)
			return m_EmptyTupleHandle;

		u64 count = u64(subTypes.size());
		auto it = m_TupleMapping.find(count);
		if (it == m_TupleMapping.end())
			it = m_TupleMapping.try_emplace(count, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		auto subIt = it->second.find(subTypes[0]);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(subTypes[0], StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount>& arr : subIt->second)
		{
			TypeHandle handle;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle.IsValid())
					break;
			}
			
			TupleType& tupType = handle.AsTuple();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (subTypes[i] != tupType.subTypes[i])
				{
					found = false;
					break;
				}
			}

			if (found)
			{
				TypeHandle& retHandle = arr[u8(mod)];
				if (retHandle.IsValid())
					return retHandle;
				
				retHandle = CreateHandle(new Type{ TupleType{ mod, subTypes } });
				return retHandle;
			}
		}

		TypeHandle handle = CreateHandle(new Type{ TupleType{ mod, subTypes } });

		subIt->second.push_back({});
		subIt->second.back().fill(TypeHandle{});
		subIt->second.back()[u8(mod)] = handle;
		
		return handle;
	}

	TypeHandle TypeRegistry::Compound(TypeMod mod, const StdVector<TypeHandle>& subTypes)
	{
		u64 count = u64(subTypes.size());
		auto it = m_CompoundMapping.find(count);
		if (it == m_CompoundMapping.end())
			it = m_CompoundMapping.try_emplace(count, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		auto subIt = it->second.find(subTypes[0]);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(subTypes[0], StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount>& arr : subIt->second)
		{
			TypeHandle handle;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle.IsValid())
					break;
			}
			
			CompoundType& compType = handle.AsCompound();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (subTypes[i] != compType.subTypes[i])
				{
					found = false;
					break;
				}
			}

			if (found)
			{
				TypeHandle& retHandle = arr[u8(mod)];
				if (retHandle.IsValid())
					return retHandle;

				retHandle = CreateHandle(new Type{ CompoundType{ mod, subTypes } });
				return retHandle;
			}
		}

		TypeHandle handle = CreateHandle(new Type{ CompoundType{ mod, subTypes } });

		subIt->second.push_back({});
		subIt->second.back().fill(TypeHandle{});
		subIt->second.back()[u8(mod)] = handle;

		return handle;
	}

	TypeHandle TypeRegistry::Func(TypeMod mod, const StdVector<TypeHandle>& params, TypeHandle ret)
	{
		u64 count = u64(params.size());
		auto it = m_FuncMapping.find(count);
		if (it == m_FuncMapping.end())
			it = m_FuncMapping.try_emplace(count, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		TypeHandle checkType = !params.empty() ? params[0] : ret;
		auto subIt = it->second.find(checkType);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(checkType, StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount>& arr : subIt->second)
		{
			TypeHandle handle;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle.IsValid())
					break;
			}
			FuncType& funcType = handle.AsFunc();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (params[i] != funcType.paramTypes[i])
				{
					found = false;
					break;
				}
			}

			if (found && ret == funcType.retType)
			{
				TypeHandle& retHandle = arr[u8(mod)];
				if (retHandle.IsValid())
					return retHandle;

				retHandle = CreateHandle(new Type{ FuncType{ mod, params, ret } });
				return retHandle;
			}
		}

		TypeHandle handle = CreateHandle(new Type{ FuncType{ mod, params, ret } });

		subIt->second.push_back({});
		subIt->second.back().fill(TypeHandle{});
		subIt->second.back()[u8(mod)] = handle;

		return handle;
	}

	TypeHandle TypeRegistry::Generic(TypeMod mod, IdenSPtr iden, const StdVector<TypeHandle>& constraints)
	{
		auto it = m_GenericMapping.find(iden);
		if (it == m_GenericMapping.end())
		{
			it = m_GenericMapping.try_emplace(iden, StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;
		}

		for (std::array<TypeHandle, m_ModCount>& arr : it->second)
		{
			TypeHandle handle;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle.IsValid())
					break;
			}
			GenericType& genType = handle.AsGeneric();

			bool found = true;
			for (TypeHandle constraint : constraints)
			{
				bool foundConstraint = false;

				for (TypeHandle candidate : genType.constraints)
				{
					if (constraint == candidate)
					{
						foundConstraint = true;
						break;
					}
				}

				if (!foundConstraint)
				{
					found = false;
					break;
				}
			}
			
			if (found)
			{
				TypeHandle& retHandle = arr[u8(mod)];
				if (retHandle.IsValid())
					return retHandle;

				retHandle = CreateHandle(new Type{ GenericType{ mod, iden, constraints } });
				return retHandle;
			}
		}

		it->second.push_back(StdArray<TypeHandle, m_ModCount>{});
		StdArray<TypeHandle, m_ModCount>& arr = it->second.back();
		arr.fill(TypeHandle{});

		TypeHandle& handle = arr[u8(mod)];
		handle = CreateHandle(new Type{ GenericType{ mod, iden, constraints } });
		
		return handle;
	}

	TypeHandle TypeRegistry::Mod(TypeMod mod, TypeHandle handle)
	{
		TypeSPtr type = handle.Type();
		if (!type)
			return TypeHandle{};
		if (type->base.mod == mod)
			return handle;

		switch (type->typeKind)
		{
		case TypeKind::Builtin:
		{
			BuiltinType& builtin = type->AsBuiltin();
			return Builtin(mod, builtin.builtin);
		}
		case TypeKind::Iden:
		{
			IdenType& iden = type->AsIden();
			return Iden(mod, iden.qualName);
		}
		case TypeKind::Ptr:
		{
			PtrType& ptr = type->AsPtr();
			return Ptr(mod, ptr.subType);
		}
		case TypeKind::Ref:
		{
			RefType& ref = type->AsRef();
			return Ref(mod, ref.subType);
		}
		case TypeKind::Slice:
		{
			SliceType& slice = type->AsSlice();
			return Slice(mod, slice.subType);
		}
		case TypeKind::Array:
		{
			ArrayType& arr = type->AsArray();
			if (arr.arrSize != u64(-1))
				return Array(mod, arr.subType, arr.size);
			return Array(mod, arr.subType, arr.expr);
		}
		case TypeKind::Tuple:
		{
			TupleType& tup = type->AsTuple();
			return Tuple(mod, tup.subTypes);
		}
		case TypeKind::Opt:
		{
			OptType& opt = type->AsOpt();
			return Opt(mod, opt.subType);
		}
		case TypeKind::Compound:
		{
			CompoundType& compound = type->AsCompound();
			return Compound(mod, compound.subTypes);
		}
		case TypeKind::Func:
		{
			FuncType& fn = type->AsFunc();
			return Func(mod, fn.paramTypes, fn.retType);
		}
		case TypeKind::Generic:
		{
			GenericType& gen = type->AsGeneric();
			return Generic(mod, gen.iden, gen.constraints);
		}
		default:
			return TypeHandle{};
		}
	}

	TypeHandle TypeRegistry::Fuzzy(TypeHandle handle)
	{
		if (!handle.IsValid())
			return handle;
		
		if (!handle.AsBase().hasFuzzyCompare)
			return handle;
		
		switch (handle.Kind())
		{
		case TypeKind::Iden:
		{
			IdenType& orig = handle.AsIden();
			QualNameSPtr fuzzy = orig.qualName->Fuzzy();
			return Iden(orig.mod, fuzzy);
		}
		case TypeKind::Ptr:
		{
			PtrType& orig = handle.AsPtr();
			TypeHandle fuzzy = Fuzzy(orig.subType);
			return Ptr(orig.mod, fuzzy);
		}
		case TypeKind::Ref:
		{
			RefType& orig = handle.AsRef();
			TypeHandle fuzzy = Fuzzy(orig.subType);
			return Ref(orig.mod, fuzzy);
		}
		case TypeKind::Slice:
		{
			SliceType& orig = handle.AsSlice();
			TypeHandle fuzzy = Fuzzy(orig.subType);
			return Slice(orig.mod, fuzzy);
		}
		case TypeKind::Array:
		{
			ArrayType& orig = handle.AsArray();
			TypeHandle fuzzy = Fuzzy(orig.subType);
			ArrayExprType fuzzyExpr{ ITrExprSPtr{} };
			return Array(orig.mod, fuzzy, fuzzyExpr);
		}
		case TypeKind::Tuple:
		{
			TupleType& orig = handle.AsTuple();
			StdVector<TypeHandle> fuzzys;
			for (TypeHandle subType : orig.subTypes)
			{
				fuzzys.push_back(Fuzzy(subType));
			}
			return Tuple(orig.mod, fuzzys);
		}
		case TypeKind::Opt:
		{
			OptType& orig = handle.AsOpt();
			TypeHandle fuzzy = Fuzzy(orig.subType);
			return Ref(orig.mod, fuzzy);
		}
		case TypeKind::Compound:
		{
			// should not happen
			assert(0);
			return TypeHandle{};
		}
		case TypeKind::Func:
		{
			FuncType& orig = handle.AsFunc();
			StdVector<TypeHandle> fuzzyParams;
			for (TypeHandle subType : orig.paramTypes)
			{
				fuzzyParams.push_back(Fuzzy(subType));
			}
			TypeHandle fuzzyRet = Fuzzy(orig.retType);
			return Func(orig.mod, fuzzyParams, fuzzyRet);
		}
		default:
			return handle;
		}
	}

	TypeHandle TypeRegistry::CreateHandle(TypeSPtr type)
	{
		return TypeHandle{ this, StdSharedPtr<THandle>{ new THandle{ type } } };
	}

	TypeHandle TypeRegistry::CreateHandle(Type* pType)
	{
		return CreateHandle(TypeSPtr{ pType });
	}

	void TypeRegistry::ExtractGenerics(TypeSPtr type, StdVector<TypeSPtr>& gens)
	{
		switch (type->typeKind)
		{
		case TypeKind::Builtin: break;
		case TypeKind::Iden:
		{
			QualNameSPtr qualName = type->AsIden().qualName;

			if (qualName->Disambiguation())
			{
				ExtractGenerics(qualName->Disambiguation()->Type().Type(), gens);
			}
			
			for (IdenSPtr iden : qualName->Idens())
			{
				for (IdenGeneric& idenGen : iden->Generics())
				{
					if (!idenGen.isType)
						ExtractGenerics(idenGen.type.Type(), gens);
				}
			}
			break;
		}
		case TypeKind::Ptr:
		{
			TypeSPtr subType = type->AsPtr().subType.Type();
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Ref:
		{
			TypeSPtr subType = type->AsRef().subType.Type();
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Slice:
		{
			TypeSPtr subType = type->AsSlice().subType.Type();
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Array:
		{
			TypeSPtr subType = type->AsArray().subType.Type();
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Tuple:
		{
			StdVector<TypeHandle>& subTypes = type->AsTuple().subTypes;
			for (TypeHandle subType : subTypes)
			{
				ExtractGenerics(subType.Type(), gens);
			}
			break;
		}
		case TypeKind::Opt:
		{
			TypeSPtr subType = type->AsOpt().subType.Type();
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Compound: break;
		case TypeKind::Func:
		{
			FuncType& funcType = type->AsFunc();
			for (TypeHandle subType : funcType.paramTypes)
			{
				ExtractGenerics(subType.Type(), gens);
			}
			if (funcType.retType.IsValid())
				ExtractGenerics(funcType.retType.Type(), gens);
			break;
		}
		case TypeKind::Generic:
			gens.push_back(type);
			break;
		default: ;
		}
	}

	TypeInfo::TypeInfo()
	{
	}

	TypeInfo::TypeInfo(TypeHandle handle)
		: handle(handle)
	{
	}

	TypeInfo::TypeInfo(TypeHandle handle, GenTypeInfo genInfo)
		: handle(handle)
		, genInfo(genInfo)
	{
	}
}
