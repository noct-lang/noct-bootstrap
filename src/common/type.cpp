#include "type.hpp"

#include "qualname.hpp"
#include "utils.hpp"

namespace Noctis
{
	StdStringView TypeModToString(TypeMod mod)
	{
		switch (mod)
		{
		case TypeMod::Const: return "const ";
		default: return "";
		}
	}

	Type::Type(TypeKind kind, TypeMod mod)
		: typeKind(kind)
		, mod(mod)
	{
	}

	BuiltinType::BuiltinType(TypeMod mod, BuiltinTypeKind builtin)
		: Type(TypeKind::Builtin, mod)
		, builtin(builtin)
	{
	}

	IdenType::IdenType(TypeMod mod, QualNameSPtr qualName)
		: Type(TypeKind::Iden, mod)
		, qualName(qualName)
	{
	}

	PtrType::PtrType(TypeMod mod, TypeHandle subType)
		: Type(TypeKind::Ptr, mod)
		, subType(subType)
	{
	}

	RefType::RefType(TypeMod mod, TypeHandle subType)
		: Type(TypeKind::Ref, mod)
		, subType(subType)
	{
	}

	SliceType::SliceType(TypeMod mod, TypeHandle subType)
		: Type(TypeKind::Slice, mod)
		, subType(subType)
	{
	}

	ArrayType::ArrayType(TypeMod mod, TypeHandle subType, u64 size)
		: Type(TypeKind::Array, mod)
		, sizeKnown(true)
		, subType(subType)
		, size(size)
	{
	}

	ArrayType::ArrayType(TypeMod mod, TypeHandle subType, StdSharedPtr<void> expr)
		: Type(TypeKind::Array, mod)
		, sizeKnown(false)
		, subType(subType)
		, size(0)
		, expr(expr)
	{
	}

	TupleType::TupleType(TypeMod mod, const StdVector<TypeHandle>& subTypes)
		: Type(TypeKind::Tuple, mod)
		, subTypes(subTypes)
	{
	}

	OptType::OptType(TypeMod mod, TypeHandle subType)
		: Type(TypeKind::Opt, mod)
		, subType(subType)
	{
	}

	CompoundType::CompoundType(TypeMod mod, const StdVector<TypeHandle>& subTypes)
		: Type(TypeKind::Compound, mod)
		, subTypes(subTypes)
	{
	}

	FuncType::FuncType(TypeMod mod, const StdVector<TypeHandle>& paramTypes, TypeHandle retType)
		: Type(TypeKind::Func, mod)
		, paramTypes(paramTypes)
		, retType(retType)
	{
	}

	TypeRegistry::TypeRegistry()
		: m_BuiltinMapping()
		, m_EmptyTupleHandle(TypeHandle(-1))
	{
		for (auto& subArr : m_BuiltinMapping)
			subArr.fill(TypeHandle(-1));
	}

	bool TypeRegistry::IsType(TypeHandle handle, TypeKind kind)
	{
		TypeSPtr type = GetType(handle);
		return type->typeKind == kind;
	}

	TypeSPtr TypeRegistry::GetType(TypeHandle handle)
	{
		if (handle >= m_Types.size())
			return nullptr;
		return m_Types[handle];
	}

	StdString TypeRegistry::ToString(TypeHandle handle)
	{
		TypeSPtr type = GetType(handle);
		if (!type)
			return "";

		StdString mod{ TypeModToString(type->mod) };
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
			case BuiltinTypeKind::U8: return mod + "u8";
			case BuiltinTypeKind::U16: return mod + "u16";
			case BuiltinTypeKind::U32: return mod + "u32";
			case BuiltinTypeKind::U64: return mod + "u64";
			case BuiltinTypeKind::U128: return mod + "u128";
			case BuiltinTypeKind::F16: return mod + "f16";
			case BuiltinTypeKind::F32: return mod + "f32";
			case BuiltinTypeKind::F64: return mod + "f64";
			case BuiltinTypeKind::F128: return mod + "f128";
			default: return "";
			}
		}
		case TypeKind::Iden:
		{
			IdenType& iden = type->AsIden();
			return mod + iden.qualName->ToString();
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
			if (arr.sizeKnown)
				tmp = Format("[%ull]", arr.size);
			else
				tmp = "[...]";
			
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
		default: return "";
		}
	}

	bool TypeRegistry::AreTypesEqual(TypeHandle first, TypeHandle second)
	{
		return GetType(first) == GetType(second);
	}

	bool TypeRegistry::CanPassTo(TypeHandle param, TypeHandle arg)
	{
		if (!AreTypesEqual(param, arg))
		{
			TypeHandle paramNoMod = Mod(TypeMod::None, param);
			return AreTypesEqual(paramNoMod, arg);
		}
		return true;
	}

	bool TypeRegistry::CanPassToRec(TypeHandle rec, TypeHandle caller)
	{
		if (!AreTypesEqual(rec, caller))
		{
			TypeSPtr recType = GetType(rec);
			TypeHandle recNoRef = recType->typeKind == TypeKind::Ref ? recType->AsRef().subType : rec;

			if (!AreTypesEqual(recNoRef, caller))
			{
				TypeHandle recNoRefNoMod = Mod(TypeMod::None, recNoRef);
				if (!AreTypesEqual(recNoRef, caller))
					return false;
			}
		}
		return true;
	}

	void TypeRegistry::SetIdenSym(QualNameSPtr qualName, SymbolWPtr sym)
	{
		auto it = m_IdenMapping.find(qualName);
		if (it == m_IdenMapping.end())
			it = m_IdenMapping.try_emplace(qualName, StdArray<TypeHandle, m_ModCount>{}).first;
		for (u8 i = 0; i < u8(TypeMod::Count); ++i)
		{
			TypeHandle& handle = it->second[i];
			if (handle == TypeHandle(-1))
			{
				TypeSPtr type{ new IdenType{ TypeMod(i), qualName } };
				type->AsIden().sym = sym;
				handle = TypeHandle(m_Types.size());
				m_Types.push_back(type);
			}
			else
			{
				TypeSPtr type = m_Types[handle];
				type->AsIden().sym = sym;
			}
		}
	}

	void TypeRegistry::SetAliasType(TypeHandle alias, TypeHandle type)
	{
		m_Types[alias] = m_Types[type];
	}

	TypeHandle TypeRegistry::Builtin(TypeMod mod, BuiltinTypeKind builtin)
	{
		TypeHandle& handle = m_BuiltinMapping[u8(builtin)][u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new BuiltinType{ mod, builtin });

		return handle;
	}

	TypeHandle TypeRegistry::Iden(TypeMod mod, QualNameSPtr qualName)
	{
		auto it = m_IdenMapping.find(qualName);
		if (it == m_IdenMapping.end())
		{
			it = m_IdenMapping.insert(std::pair{ qualName, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new IdenType{ mod, qualName });

		return handle;
	}

	TypeHandle TypeRegistry::Ptr(TypeMod mod, TypeHandle subType)
	{
		auto it = m_PtrMapping.find(subType);
		if (it == m_PtrMapping.end())
		{
			it = m_PtrMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new PtrType{ mod, subType });

		return handle;
	}

	TypeHandle TypeRegistry::Ref(TypeMod mod, TypeHandle subType)
	{
		auto it = m_RefMapping.find(subType);
		if (it == m_RefMapping.end())
		{
			it = m_RefMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new RefType{ mod, subType });

		return handle;
	}

	TypeHandle TypeRegistry::Slice(TypeMod mod, TypeHandle subType)
	{
		auto it = m_SliceMapping.find(subType);
		if (it == m_SliceMapping.end())
		{
			it = m_SliceMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new SliceType{ mod, subType });

		return handle;
	}

	TypeHandle TypeRegistry::Opt(TypeMod mod, TypeHandle subType)
	{
		auto it = m_OptMapping.find(subType);
		if (it == m_OptMapping.end())
		{
			it = m_OptMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new OptType{ mod, subType });

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
			subIt->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new ArrayType{ mod, subType, size });

		return handle;
	}

	TypeHandle TypeRegistry::Array(TypeMod mod, TypeHandle subType, StdSharedPtr<void> expr)
	{
		auto it = m_ArrayMapping.find(subType);
		if (it == m_ArrayMapping.end())
			it = m_ArrayMapping.insert(std::pair{ subType, StdUnorderedMap<StdSharedPtr<void>, StdArray<TypeHandle, m_ModCount>>{} }).first;

		auto subIt = it->second.find(expr);
		if (subIt == it->second.end())
		{
			subIt = it->second.insert(std::pair{ expr, StdArray<TypeHandle, m_ModCount>{} }).first;
			subIt->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new ArrayType{ mod, subType, expr });

		return handle;
	}

	TypeHandle TypeRegistry::Tuple(TypeMod mod, const StdVector<TypeHandle>& subTypes)
	{
		if (subTypes.size() == 0)
		{
			if (m_EmptyTupleHandle != TypeHandle(-1))
				return m_EmptyTupleHandle;

			m_EmptyTupleHandle = TypeHandle(m_Types.size());
			m_Types.emplace_back(new TupleType{ TypeMod::None, {} });
			return m_EmptyTupleHandle;
		}

		u64 count = u64(subTypes.size());
		auto it = m_TupleMapping.find(count);
		if (it == m_TupleMapping.end())
			it = m_TupleMapping.try_emplace(count, StdUnorderedMap<u64, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		auto subIt = it->second.find(subTypes[0]);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(subTypes[0], StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount> array : subIt->second)
		{
			TypeHandle handle = array[u8(mod)];
			TypeSPtr type = GetType(handle);
			TupleType& tupType = type->AsTuple();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (!AreTypesEqual(subTypes[i], tupType.subTypes[i]))
				{
					found = false;
					break;
				}
			}

			if (found)
				return handle;
		}

		TypeHandle handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new TupleType{ mod, subTypes });

		subIt->second.push_back({});
		subIt->second.back().fill(TypeHandle(-1));
		subIt->second.back()[u8(mod)] = handle;
		
		return handle;
	}

	TypeHandle TypeRegistry::Compound(TypeMod mod, const StdVector<TypeHandle>& subTypes)
	{
		u64 count = u64(subTypes.size());
		auto it = m_CompoundMapping.find(count);
		if (it == m_CompoundMapping.end())
			it = m_CompoundMapping.try_emplace(count, StdUnorderedMap<u64, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		auto subIt = it->second.find(subTypes[0]);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(subTypes[0], StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount> array : subIt->second)
		{
			TypeHandle handle = array[u8(mod)];
			TypeSPtr type = GetType(handle);
			TupleType& tupType = type->AsTuple();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (!AreTypesEqual(subTypes[i], tupType.subTypes[i]))
				{
					found = false;
					break;
				}
			}

			if (found)
				return handle;
		}

		TypeHandle handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new TupleType{ mod, subTypes });

		subIt->second.push_back({});
		subIt->second.back().fill(TypeHandle(-1));
		subIt->second.back()[u8(mod)] = handle;

		return handle;
	}

	TypeHandle TypeRegistry::Func(TypeMod mod, const StdVector<TypeHandle>& params, TypeHandle ret)
	{
		u64 count = u64(params.size());
		auto it = m_CompoundMapping.find(count);
		if (it == m_CompoundMapping.end())
			it = m_CompoundMapping.try_emplace(count, StdUnorderedMap<u64, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		TypeHandle checkType = !params.empty() ? params[0] : ret;
		auto subIt = it->second.find(checkType);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(checkType, StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount> array : subIt->second)
		{
			TypeHandle handle = array[u8(mod)];
			TypeSPtr type = GetType(handle);
			FuncType& funcType = type->AsFunc();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (!AreTypesEqual(params[i], funcType.paramTypes[i]))
				{
					found = false;
					break;
				}
			}

			if (found && ret == funcType.retType)
				return handle;
		}

		TypeHandle handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new FuncType{ mod, params, ret });

		subIt->second.push_back({});
		subIt->second.back().fill(TypeHandle(-1));
		subIt->second.back()[u8(mod)] = handle;

		return handle;
	}

	TypeHandle TypeRegistry::Mod(TypeMod mod, TypeHandle handle)
	{
		TypeSPtr type = GetType(handle);
		if (!type)
			return TypeHandle(-1);
		if (type->mod == mod)
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
			RefType& ptr = type->AsRef();
			return Ref(mod, ptr.subType);
		}
		case TypeKind::Slice:
		{
			SliceType& ptr = type->AsSlice();
			return Slice(mod, ptr.subType);
		}
		case TypeKind::Array:
		{
			ArrayType& arr = type->AsArray();
			if (arr.sizeKnown)
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
			OptType& ptr = type->AsOpt();
			return Opt(mod, ptr.subType);
		}
		case TypeKind::Compound:
		{
			CompoundType& tup = type->AsCompound();
			return Compound(mod, tup.subTypes);
		}
		case TypeKind::Func:
		{
			FuncType& fn = type->AsFunc();
			return Func(mod, fn.paramTypes, fn.retType);
		}
		default:
			return TypeHandle(-1);
		}
	}
}
