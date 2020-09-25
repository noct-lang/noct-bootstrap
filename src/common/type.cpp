#include "type.hpp"


#include "module/symbol.hpp"
#include "qualname.hpp"
#include "utils.hpp"
#include "context.hpp"
#include "module/module.hpp"

namespace Noctis
{
	StdStringView TypeModToString(TypeMod mod)
	{
		switch (mod)
		{
		case TypeMod::Mut: return "mut ";
		default: return "";
		}
	}

	Type::Type(TypeKind kind, TypeMod mod)
		: typeKind(kind)
		, mod(mod)
		, size(0)
		, alignment(0)
	{
	}

	void Type::CalculateSizeAlign(TypeRegistry& typeReg)
	{
		switch (typeKind)
		{
		case TypeKind::Builtin:
		{
			BuiltinType& builtin = AsBuiltin();
			switch (builtin.builtin)
			{
			case BuiltinTypeKind::Bool:  size = alignment = 1;  break;
			case BuiltinTypeKind::Char:  size = alignment = 4;  break;
			case BuiltinTypeKind::I8:    size = alignment = 1;  break;
			case BuiltinTypeKind::I16:   size = alignment = 2;  break;
			case BuiltinTypeKind::I32:   size = alignment = 4;  break;
			case BuiltinTypeKind::I64:   size = alignment = 8;  break;
			case BuiltinTypeKind::I128:  size = alignment = 16; break;
			case BuiltinTypeKind::ISize: size = alignment = 8;  break; // TODO: arch specific size
			case BuiltinTypeKind::U8:    size = alignment = 1;  break;
			case BuiltinTypeKind::U16:   size = alignment = 2;  break;
			case BuiltinTypeKind::U32:   size = alignment = 3;  break;
			case BuiltinTypeKind::U64:   size = alignment = 4;  break;
			case BuiltinTypeKind::U128:  size = alignment = 16; break; 
			case BuiltinTypeKind::USize: size = alignment = 8;  break; // TODO: arch specific size
			case BuiltinTypeKind::F16:   size = alignment = 2;  break;
			case BuiltinTypeKind::F32:   size = alignment = 4;  break;
			case BuiltinTypeKind::F64:   size = alignment = 8;  break;
			case BuiltinTypeKind::F128:  size = alignment = 16; break;
			default: ;
			}
			
			break;
		}
		case TypeKind::Iden:
		{
			IdenType& idenType = AsIden();
			SymbolSPtr sym = idenType.sym.lock();
			if (!sym)
				break;
			
			sym->CalculateSizeAlignOffset();
			alignment = sym->aligment;
			size = sym->size;
			break;
		}
		case TypeKind::Ptr: size = alignment = 8; break; // TODO: arch specific size
		case TypeKind::Ref: size = alignment = 8; break; // TODO: arch specific size
		case TypeKind::Slice: size = alignment = 16; break; // TODO: arch specific size
		case TypeKind::Array:
		{
			TypeSPtr subType = AsArray().subType->type;
			if (subType->size == 0)
				subType->CalculateSizeAlign(typeReg);

			alignment = subType->alignment;
			size = subType->size * AsArray().size;

			break;
		}
		case TypeKind::Tuple:
		{
			TupleType& tup = AsTuple();
			
			for (TypeHandle subHandle : AsTuple().subTypes)
			{
				TypeSPtr subType = subHandle->type;
				if (subType->size == 0)
					subType->CalculateSizeAlign(typeReg);

				if (alignment < subType->alignment)
					alignment = subType->alignment;
				
				u64 alignOffset = (alignment + size) & (subType->alignment - 1);
				size += alignOffset == 0 ? 0 : subType->alignment - alignOffset;
				tup.offsets.push_back(size);
				size += subType->size;
			}
			break;
		}
		case TypeKind::Opt:
		{
			TypeSPtr subType = AsOpt().subType->type;
			if (subType->size == 0)
				subType->CalculateSizeAlign(typeReg);

			alignment = subType->alignment;
			size = subType->size + 1; // TODO: is this correct?
			
			break;
		}
		case TypeKind::Func: size = alignment = 8; break; // TODO: arch specific size
		default: ;
		}
	}

	BuiltinType::BuiltinType(TypeMod mod, BuiltinTypeKind builtin)
		: Type(TypeKind::Builtin, mod)
		, builtin(builtin)
	{
	}

	bool BuiltinType::IsSigned() const
	{
		switch (builtin)
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

	bool BuiltinType::IsFp() const
	{
		switch (builtin)
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

	ArrayType::ArrayType(TypeMod mod, TypeHandle subType, ArrayExprType expr)
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

	GenericType::GenericType(TypeMod mod, IdenSPtr qualName, const StdVector<TypeHandle>& constraints)
		: Type(TypeKind::Generic, mod)
		, iden(qualName)
		, constraints(constraints)
	{
	}

	TypeRegistry::TypeRegistry(Context* pCtx)
		: m_BuiltinMapping()
		, m_pCtx(pCtx)
	{
		for (auto& subArr : m_BuiltinMapping)
			subArr.fill(nullptr);
	}

	bool TypeRegistry::IsType(TypeHandle handle, TypeKind kind)
	{
		TypeSPtr type = handle->type;
		return type->typeKind == kind;
	}

	StdString TypeRegistry::ToString(TypeHandle handle)
	{
		TypeSPtr type = handle->type;
		return ToString(type);
	}

	StdString TypeRegistry::ToString(TypeSPtr type)
	{
		if (!type)
			return "()";

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
			idenName.erase(idenName.begin(), idenName.begin() + 2);
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
			if (arr.sizeKnown)
			{
				tmp = Format("[%llu]", arr.size);
			}
			else if (std::holds_alternative<ITrExprSPtr>(arr.expr) && std::get<ITrExprSPtr>(arr.expr)->exprKind == ITrExprKind::QualName)
			{
				tmp = '[';
				ITrExprSPtr expr = std::get<ITrExprSPtr>(arr.expr);
				ITrQualNameExpr& qualName = reinterpret_cast<ITrQualNameExpr&>(*expr);
				tmp += qualName.qualName->Iden()->ToString();
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
			return mod + "gen__" + idenName;
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
			if (funcType.retType)
				ret = ToString(funcType.retType);
			
			return mod + '(' + params + ")->(" + ret + ')';
		}
		default: return "()";
		}
	}

	bool TypeRegistry::AreTypesEqual(TypeHandle first, TypeHandle second)
	{
		return first->type == second->type;
	}

	bool TypeRegistry::CanPassTo(TypeHandle param, TypeHandle arg)
	{
		if (AreTypesEqual(param, arg))
			return true;

		TypeSPtr paramType = param->type;
		TypeSPtr argType = arg->type;

		if (paramType->mod == TypeMod::Mut &&
				argType->mod == TypeMod::None &&
				AreTypesEqual( Mod(TypeMod::None, param), arg))
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
				TypeHandle paramSubTypeHandle = paramType->AsPtr().subType;
				TypeHandle argSubTypeHandle = argType->AsPtr().subType;

				TypeSPtr paramSubType = paramSubTypeHandle->type;
				TypeSPtr argSubType = argSubTypeHandle->type;

				if (paramSubType->mod == TypeMod::Mut &&
					argSubType->mod == TypeMod::None &&
					AreTypesEqual( Mod(TypeMod::None, paramSubTypeHandle), argSubTypeHandle))
					return true;
			}
			case TypeKind::Ref:
			{
				TypeHandle paramSubTypeHandle = paramType->AsRef().subType;
				TypeHandle argSubTypeHandle = argType->AsRef().subType;

				TypeSPtr paramSubType = paramSubTypeHandle->type;
				TypeSPtr argSubType = argSubTypeHandle->type;

				if (paramSubType->mod == TypeMod::Mut &&
					argSubType->mod == TypeMod::None &&
					AreTypesEqual(Mod(TypeMod::None, paramSubTypeHandle), argSubTypeHandle))
					return true;
			}
			case TypeKind::Slice:
			{
				TypeHandle paramSubTypeHandle = paramType->AsSlice().subType;
				TypeHandle argSubTypeHandle = argType->AsSlice().subType;

				TypeSPtr paramSubType = paramSubTypeHandle->type;
				TypeSPtr argSubType = argSubTypeHandle->type;

				if (paramSubType->mod == TypeMod::Mut &&
					argSubType->mod == TypeMod::None &&
					AreTypesEqual(Mod(TypeMod::None, paramSubTypeHandle), argSubTypeHandle))
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
			if (!handle)
			{
				TypeSPtr type{ new IdenType{ TypeMod(i), qualName } };
				type->AsIden().sym = sym;
				handle = CreateHandle(type);
				m_Types.push_back(type);
			}
			else
			{
				TypeSPtr type = handle->type;
				type->AsIden().sym = sym;
			}
		}
	}

	void TypeRegistry::SetAliasType(TypeHandle alias, TypeHandle type)
	{
		alias->type = type->type;
	}

	TypeHandle TypeRegistry::ReplaceSubType(TypeHandle orig, TypeHandle toReplace, TypeHandle replacement)
	{
		if (!orig)
			return nullptr;

		if (orig == toReplace)
			return replacement;

		TypeSPtr type = orig->type;
		if (type->mod != TypeMod::None)
		{
			TypeSPtr toReplaceType = toReplace->type;
			if (toReplaceType->mod == TypeMod::None)
			{
				toReplace = Mod(type->mod, toReplace);
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
			return Ptr(type->mod, subType);
		}
		case TypeKind::Ref:
		{
			RefType& refType = type->AsRef();
			TypeHandle subType = ReplaceSubType(refType.subType, toReplace, replacement);
			return Ref(type->mod, subType);
		}
		case TypeKind::Slice: 
		{
			SliceType& sliceType = type->AsSlice();
			TypeHandle subType = ReplaceSubType(sliceType.subType, toReplace, replacement);
			return Slice(type->mod, subType);
		}
		case TypeKind::Array:
		{
			ArrayType& arrType = type->AsArray();
			TypeHandle subType = ReplaceSubType(arrType.subType, toReplace, replacement);
			if (arrType.sizeKnown)
				return Array(type->mod, subType, arrType.size);
			return Array(type->mod, subType, arrType.expr);
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
			return Tuple(type->mod, subTypes);
		}
		case TypeKind::Opt:
		{
			OptType& ptrType = type->AsOpt();
			TypeHandle subType = ReplaceSubType(ptrType.subType, toReplace, replacement);
			return Ptr(type->mod, subType);
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
			return Compound(type->mod, subTypes);
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
			return Func(type->mod, paramTypes, retType);
		}
		case TypeKind::Generic:
		{
			if (toReplace->type->typeKind != TypeKind::Generic)
				return orig;
			
			GenericType& genOrig = orig->AsGeneric();
			GenericType& genFrom = toReplace->AsGeneric();

			if (genOrig.iden == genFrom.iden)
				return replacement;
		}
		/*case TypeKind::Iden:
		{9
			if (toReplace->type->typeKind != TypeKind::Iden)
				return orig;
			
			IdenType& idenTypeOrig = orig->AsIden();
			IdenType& idenTypeFrom = toReplace->AsIden();

			QualNameSPtr origQualName = idenTypeOrig.qualName;
			QualNameSPtr fromQualName = idenTypeFrom.qualName;
			
			IdenSPtr origIden = origQualName->Iden();
			IdenSPtr fromIden = fromQualName->Iden();

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
		TypeSPtr type = handle->type;

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
			if (funcType.retType)
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
			if (!type->size)
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
				TypeSPtr typeSub = type->AsPtr().subType->type;
				TypeSPtr candidateSub = candidate->AsPtr().subType->type;
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->mod == TypeMod::None && candidateSub->mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Ref:
			{
				TypeSPtr typeSub = type->AsRef().subType->type;
				TypeSPtr candidateSub = candidate->AsRef().subType->type;
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->mod == TypeMod::None && candidateSub->mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Slice:
			{
				TypeSPtr typeSub = type->AsSlice().subType->type;
				TypeSPtr candidateSub = candidate->AsSlice().subType->type;
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->mod == TypeMod::None && candidateSub->mod == TypeMod::Mut)
					return -1;
				return tmp;
			}
			case TypeKind::Array:
			{
				ArrayType& typeArr = type->AsArray();
				ArrayType& candidateArr = type->AsArray();

				if (candidateArr.sizeKnown)
				{
					if (typeArr.size != candidateArr.size)
						return -1;
				}
				else
				{
					// TODO: Value Generics
				}
				
				TypeSPtr typeSub = type->AsArray().subType->type;
				TypeSPtr candidateSub = candidate->AsArray().subType->type;

				i64 tmp = ScorePossibleVariant(typeSub, candidate);
				if (tmp == -1)
					return -1;
				if (typeSub->mod == TypeMod::None && candidateSub->mod == TypeMod::Mut)
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
					TypeSPtr typeSub = typeSubs[i]->type;
					TypeSPtr candidateSub = candidateSubs[i]->type;

					i64 tmp = ScorePossibleVariant(typeSub, candidate);
					if (tmp == -1)
						return -1;
					if (typeSub->mod == TypeMod::None && candidateSub->mod == TypeMod::Mut)
						return -1;
					totalScore += tmp;
				}
				return totalScore;
			}
			case TypeKind::Opt:
			{
				TypeSPtr typeSub = type->AsOpt().subType->type;
				TypeSPtr candidateSub = candidate->AsOpt().subType->type;
				i64 tmp = ScorePossibleVariant(typeSub, candidateSub);
				if (tmp == -1)
					return -1;
				if (typeSub->mod == TypeMod::None && candidateSub->mod == TypeMod::Mut)
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
		if (type->mod == TypeMod::None && candidate->mod == TypeMod::Mut)
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
					QualNameSPtr constraintQualName = constraint->AsIden().qualName;

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

					QualNameSPtr constraintQualName = constraint->AsIden().qualName;
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
		TypeSPtr wantedType = type->type;

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
		if (handle)
			return handle;

		handle = TypeHandle(CreateHandle(new BuiltinType{ mod, builtin }));

		return handle;
	}

	TypeHandle TypeRegistry::Iden(TypeMod mod, QualNameSPtr qualName)
	{
		auto it = m_IdenMapping.find(qualName);
		if (it == m_IdenMapping.end())
		{
			it = m_IdenMapping.insert(std::pair{ qualName, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(nullptr);
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new IdenType{ mod, qualName });

		return handle;
	}

	TypeHandle TypeRegistry::Ptr(TypeMod mod, TypeHandle subType)
	{
		auto it = m_PtrMapping.find(subType);
		if (it == m_PtrMapping.end())
		{
			it = m_PtrMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(nullptr);
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new PtrType{ mod, subType });

		return handle;
	}

	TypeHandle TypeRegistry::Ref(TypeMod mod, TypeHandle subType)
	{
		auto it = m_RefMapping.find(subType);
		if (it == m_RefMapping.end())
		{
			it = m_RefMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(nullptr);
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new RefType{ mod, subType });

		return handle;
	}

	TypeHandle TypeRegistry::Slice(TypeMod mod, TypeHandle subType)
	{
		auto it = m_SliceMapping.find(subType);
		if (it == m_SliceMapping.end())
		{
			it = m_SliceMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(nullptr);
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new SliceType{ mod, subType });

		return handle;
	}

	TypeHandle TypeRegistry::Opt(TypeMod mod, TypeHandle subType)
	{
		auto it = m_OptMapping.find(subType);
		if (it == m_OptMapping.end())
		{
			it = m_OptMapping.insert(std::pair{ subType, StdArray<TypeHandle, m_ModCount>{} }).first;
			it->second.fill(nullptr);
		}

		TypeHandle& handle = it->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new OptType{ mod, subType });

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
			subIt->second.fill(nullptr);
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new ArrayType{ mod, subType, size });

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
			subIt->second.fill(nullptr);
		}

		TypeHandle& handle = subIt->second[u8(mod)];
		if (handle)
			return handle;

		handle = CreateHandle(new ArrayType{ mod, subType, expr });

		return handle;
	}

	TypeHandle TypeRegistry::Tuple(TypeMod mod, const StdVector<TypeHandle>& subTypes)
	{
		if (subTypes.size() == 0)
		{
			if (m_EmptyTupleHandle)
				return m_EmptyTupleHandle;

			m_EmptyTupleHandle = CreateHandle(new TupleType{ TypeMod::None, {} });
			return m_EmptyTupleHandle;
		}

		u64 count = u64(subTypes.size());
		auto it = m_TupleMapping.find(count);
		if (it == m_TupleMapping.end())
			it = m_TupleMapping.try_emplace(count, StdUnorderedMap<TypeHandle, StdVector<StdArray<TypeHandle, m_ModCount>>>{}).first;

		auto subIt = it->second.find(subTypes[0]);
		if (subIt == it->second.end())
			subIt = it->second.try_emplace(subTypes[0], StdVector<StdArray<TypeHandle, m_ModCount>>{}).first;

		for (StdArray<TypeHandle, m_ModCount> arr : subIt->second)
		{
			TypeHandle handle = nullptr;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle)
					break;
			}
			
			TupleType& tupType = handle->AsTuple();

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
			{
				if (arr[u8(mod)] == nullptr)
				{
					handle = CreateHandle(new TupleType{ mod, subTypes });
					arr[u8(mod)] = handle;
				}
				return handle;
			}
		}

		TypeHandle handle = CreateHandle(new TupleType{ mod, subTypes });

		subIt->second.push_back({});
		subIt->second.back().fill(nullptr);
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

		for (StdArray<TypeHandle, m_ModCount> arr : subIt->second)
		{
			TypeHandle handle = nullptr;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle)
					break;
			}
			
			CompoundType& compType = handle->AsCompound();

			bool found = true;
			for (usize i = 1; i < count; ++i)
			{
				if (!AreTypesEqual(subTypes[i], compType.subTypes[i]))
				{
					found = false;
					break;
				}
			}

			if (found)
			{
				if (arr[u8(mod)] == nullptr)
				{
					handle = CreateHandle(new CompoundType{ mod, subTypes });
					arr[u8(mod)] = handle;
				}
				return handle;
			}
		}

		TypeHandle handle = CreateHandle(new CompoundType{ mod, subTypes });

		subIt->second.push_back({});
		subIt->second.back().fill(nullptr);
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

		for (StdArray<TypeHandle, m_ModCount> arr : subIt->second)
		{
			TypeHandle handle = nullptr;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle)
					break;
			}
			FuncType& funcType = handle->AsFunc();

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
			{
				if (arr[u8(mod)] == nullptr)
				{
					handle = CreateHandle(new FuncType{ mod, params, ret });
					arr[u8(mod)] = handle;
				}
				return handle;
			}
		}

		TypeHandle handle = CreateHandle(new FuncType{ mod, params, ret });

		subIt->second.push_back({});
		subIt->second.back().fill(nullptr);
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

		for (std::array<TypeHandle, m_ModCount> arr : it->second)
		{
			TypeHandle handle = nullptr;
			for (usize i = 0; i < m_ModCount; ++i)
			{
				handle = arr[i];
				if (handle)
					break;
			}
			GenericType& genType = handle->AsGeneric();

			bool found = true;
			for (TypeHandle constraint : constraints)
			{
				bool foundConstraint = false;

				for (TypeHandle candidate : genType.constraints)
				{
					if (AreTypesEqual(constraint, candidate))
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
				if (arr[u8(mod)])
					return arr[u8(mod)];
			}
		}

		it->second.push_back(StdArray<TypeHandle, m_ModCount>{});
		StdArray<TypeHandle, m_ModCount>& arr = it->second.back();
		arr.fill(nullptr);

		TypeHandle& handle = arr[u8(mod)];
		handle = CreateHandle(new GenericType{ mod, iden, constraints });
		
		return handle;
	}

	TypeHandle TypeRegistry::Mod(TypeMod mod, TypeHandle handle)
	{
		TypeSPtr type = handle->type;
		if (!type)
			return nullptr;
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
		case TypeKind::Generic:
		{
			GenericType& gen = type->AsGeneric();
			return Generic(mod, gen.iden, gen.constraints);
		}
		default:
			return nullptr;
		}
	}

	TypeHandle TypeRegistry::CreateHandle(TypeSPtr type)
	{
		return TypeHandle{ new THandle{ type } };
	}

	TypeHandle TypeRegistry::CreateHandle(Type* pType)
	{
		return TypeHandle{ new THandle{ TypeSPtr{ pType } } };
	}

	void TypeRegistry::ExtractGenerics(TypeSPtr type, StdVector<TypeSPtr>& gens)
	{
		switch (type->typeKind)
		{
		case TypeKind::Builtin: break;
		case TypeKind::Iden: break;
		case TypeKind::Ptr:
		{
			TypeSPtr subType = type->AsPtr().subType->type;
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Ref:
		{
			TypeSPtr subType = type->AsRef().subType->type;
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Slice:
		{
			TypeSPtr subType = type->AsSlice().subType->type;
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Array:
		{
			TypeSPtr subType = type->AsArray().subType->type;
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Tuple:
		{
			StdVector<TypeHandle>& subTypes = type->AsTuple().subTypes;
			for (TypeHandle subType : subTypes)
			{
				ExtractGenerics(subType->type, gens);
			}
			break;
		}
		case TypeKind::Opt:
		{
			TypeSPtr subType = type->AsOpt().subType->type;
			ExtractGenerics(subType, gens);
			break;
		}
		case TypeKind::Compound: break;
		case TypeKind::Func:
		{
			FuncType& funcType = type->AsFunc();
			for (TypeHandle subType : funcType.paramTypes)
			{
				ExtractGenerics(subType->type, gens);
			}
			if (funcType.retType)
				ExtractGenerics(funcType.retType->type, gens);
			break;
		}
		case TypeKind::Generic:
			gens.push_back(type);
			break;
		default: ;
		}
	}

	bool AreTypesEqual(TypeHandle t0, TypeHandle t1)
	{
		if (!t0)
			return !t1;
		if (!t1)
			return false;
		return t0->type == t1->type;
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
