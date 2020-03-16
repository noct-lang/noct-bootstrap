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

		auto it = m_TupleMapping.find(subTypes[0]);
		if (it == m_TupleMapping.end())
			it = m_TupleMapping.insert(std::pair{ subTypes[0], StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>{} }).first;

		auto subIt = it->second.find(u64(subTypes.size()));
		if (subIt == it->second.end())
		{
			subIt = it->second.insert(std::pair{ u64(subTypes.size()), StdArray<TypeHandle, m_ModCount>{} }).first;
			subIt->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new TupleType{ mod, subTypes });

		return handle;
	}

	TypeHandle TypeRegistry::Compound(TypeMod mod, const StdVector<TypeHandle>& subTypes)
	{
		auto it = m_CompoundMapping.find(subTypes[0]);
		if (it == m_CompoundMapping.end())
			it = m_CompoundMapping.insert(std::pair{ subTypes[0], StdUnorderedMap<u64, StdArray<TypeHandle, m_ModCount>>{} }).first;

		auto subIt = it->second.find(u64(subTypes.size()));
		if (subIt == it->second.end())
		{
			subIt = it->second.insert(std::pair{ u64(subTypes.size()), StdArray<TypeHandle, m_ModCount>{} }).first;
			subIt->second.fill(TypeHandle(-1));
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle != TypeHandle(-1))
			return handle;

		handle = TypeHandle(m_Types.size());
		m_Types.emplace_back(new CompoundType{ mod, subTypes });

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
		default:
			return TypeHandle(-1);
		}
	}
}
