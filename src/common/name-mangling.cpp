#include "name-mangling.hpp"
#include "qualname.hpp"
#include "module/symbol.hpp"
#include "utils.hpp"
#include "context.hpp"
#include <charconv>

namespace Noctis::NameMangling
{
	StdString Mangle(SymbolSPtr sym)
	{
		BoundsInfo* pBoundsInfo = &sym->boundsInfo;
		
		switch (sym->kind)
		{
		case SymbolKind::Func:
		{
			StdString mangledName;
			if (sym->qualName->Base())
				mangledName += Mangle(sym->qualName->Base(), pBoundsInfo);
			
			mangledName += Mangle(sym->qualName->LastIden());
			mangledName += Mangle(sym->qualName->Generics(), pBoundsInfo);
			
			StdString mangledType = Mangle(sym->type);
			return "_NF" + mangledName + mangledType;
		}
		case SymbolKind::Method:
		{
			SymbolSPtr parent = sym->parent.lock();
			if (!parent ||
				parent->impls.empty() ||
				parent->kind == SymbolKind::StrongInterface || 
				parent->kind == SymbolKind::WeakInterface)
			{
				StdString mangledName = Mangle(sym->type);
				mangledName += Mangle(sym->qualName->LastIden());
				mangledName += Mangle(sym->qualName->Generics(), pBoundsInfo);

				StdString mangledType = Mangle(sym->type);
				return "_NM" + mangledName + mangledType;
			}
			else
			{
				QualNameSPtr parentQualName = sym->impls.front().first.lock()->parent.lock()->qualName;
				StdString parentMangled = Mangle(parentQualName, pBoundsInfo);
				
				StdString mangledName = Mangle(parent->type);
				mangledName += Mangle(sym->qualName->LastIden());
				mangledName += Mangle(sym->qualName->Generics(), pBoundsInfo);
				
				StdString mangledType = Mangle(sym->type);
				return "_NN"  + parentMangled + "Z" + mangledName + mangledType;
			}
		}
		default: return "";
		}
	}

	StdString Mangle(QualNameSPtr qualName, BoundsInfo* pBoundsInfo)
	{
		StdString mangled;
		for (const StdString& iden : qualName->Idens())
		{
			mangled += Mangle(iden);
		}
		mangled += Mangle(qualName->Generics(), pBoundsInfo);
		return mangled;
	}

	StdString Mangle(StdVector<IdenGeneric>& idenGens, BoundsInfo* pBoundsInfo)
	{
		if (idenGens.empty())
			return "";

		StdString mangled = "G";

		for (IdenGeneric& generic : idenGens)
		{
			if (generic.isType)
			{
				if (generic.isSpecialized)
				{
					mangled += "U";
					mangled += Mangle(generic.type);
					mangled += "Z";
				}
				else
				{
					mangled += Format("H%u", generic.type.AsGeneric().id);
					if (pBoundsInfo)
					{
						const Bounds& bounds = pBoundsInfo->GetBounds(generic.type);
						if (!bounds.bounds.empty())
						{
							mangled += "C";
							for (TypeHandle constraint : bounds.bounds)
							{
								mangled += Mangle(constraint);
							}
							mangled += "Z";
						}
					}

					mangled += "Z";
				}
			}
			else
			{
				if (generic.isSpecialized)
				{
					mangled += "W";
					// TODO
					mangled += "Z";
				}
				else
				{
					mangled += "V";
					mangled += Mangle(generic.iden);
					mangled += Mangle(generic.type);
					mangled += "Z";
				}
			}
		}
		return mangled;
	}

	StdString Mangle(const StdString& iden)
	{
		return Format("%u%s", iden.size(), iden.c_str());
	}

	StdString Mangle(TypeHandle type)
	{
		if (!type.IsValid())
			return "";
		
		return Mangle(type.Type());
	}

	StdString Mangle(TypeSPtr type)
	{
		StdString mod;
		if (type->Mod() == TypeMod::Mut)
			mod = "M";
		
		switch (type->typeKind)
		{
		case TypeKind::Builtin:
		{
			switch (type->AsBuiltin().builtin)
			{
			case BuiltinTypeKind::Bool:  return mod + "b";
			case BuiltinTypeKind::Char:  return mod + "c";
			case BuiltinTypeKind::I8:    return mod + "i";
			case BuiltinTypeKind::I16:   return mod + "j";
			case BuiltinTypeKind::I32:   return mod + "k";
			case BuiltinTypeKind::I64:   return mod + "l";
			case BuiltinTypeKind::I128:  return mod + "m";
			case BuiltinTypeKind::ISize: return mod + "n";
			case BuiltinTypeKind::U8:    return mod + "u";
			case BuiltinTypeKind::U16:   return mod + "v";
			case BuiltinTypeKind::U32:   return mod + "w";
			case BuiltinTypeKind::U64:   return mod + "x";
			case BuiltinTypeKind::U128:  return mod + "y";
			case BuiltinTypeKind::USize: return mod + "z";
			case BuiltinTypeKind::F16:   return mod + "e";
			case BuiltinTypeKind::F32:   return mod + "f";
			case BuiltinTypeKind::F64:   return mod + "g";
			case BuiltinTypeKind::F128:  return mod + "h";
			default: return "";
			}
		}
		case TypeKind::Iden:
		{
			return mod + Mangle(type->AsIden().qualName, nullptr);
		}
		case TypeKind::Ptr:
		{
			return mod + "P" + Mangle(type->AsPtr().subType);
		}
		case TypeKind::Ref:
		{
			return mod + "R" + Mangle(type->AsRef().subType);
		}
		case TypeKind::Slice:
		{
			return mod + "S" + Mangle(type->AsSlice().subType);
		}
		case TypeKind::Array:
		{
			ArrayType& arrType = type->AsArray();

			if (arrType.arrSize != u64(-1))
				return mod + Format("A%llu", arrType.size) + Mangle(arrType.subType);
			return mod + "A_" + Mangle(arrType.subType);
		}
		case TypeKind::Tuple:
		{
			TupleType& tupType = type->AsTuple();
			StdString mangled = "T";
			for (TypeHandle subType : tupType.subTypes)
			{
				mangled += Mangle(subType);
			}
			return mod + mangled + "Z";
		}
		case TypeKind::Opt:
		{
			return mod + "O" + Mangle(type->AsOpt().subType);
		}
		case TypeKind::Func:
		{
			FuncType& funcType = type->AsFunc();
			StdString mangled = "F";
			for (TypeHandle subType : funcType.paramTypes)
			{
				mangled += Mangle(subType);
			}
			mangled += mod + "Z";

			if (funcType.retType.IsValid())
				mangled += Mangle(funcType.retType);

			return mod + mangled + "Z";
		}
		case TypeKind::Generic:
		{
			// TODO: bounds
			return mod + Format("H%u", type->AsGeneric().id);
		}
		default: return "";
		}
	}

	QualNameSPtr DemangleQualName(StdStringView data, BoundsInfo* pBoundsInfo)
	{
		usize idx = 0;
		return DemangleQualName(data, idx, pBoundsInfo);
	}

	QualNameSPtr DemangleQualName(StdStringView data, usize& idx, BoundsInfo* pBoundsInfo)
	{
		QualNameSPtr qualName;
		while (idx < data.size() && isdigit(data[idx]))
		{
			if (!qualName)
				qualName = QualName::Create(DemangleLName(data, idx));
			else
				qualName = qualName->Append(DemangleLName(data, idx));
		}

		StdVector<IdenGeneric> generics = DemangledGenerics(data, idx, pBoundsInfo);
		if (!generics.empty())
			qualName = qualName->Base()->Append(qualName->LastIden(), generics);
		
		return qualName;
	}

	StdVector<IdenGeneric> DemangledGenerics(StdStringView data, usize& idx, BoundsInfo* pBoundsInfo)
	{
		StdVector<IdenGeneric> generics;
		if (idx < data.size() && data[idx] == 'G')
		{
			++idx;
			while (data[idx] != 'Z')
			{
				switch (data[idx])
				{
				case 'H':
				{
					++idx;
					u16 id = u16(DemangleUSize(data, idx));

					IdenGeneric idenGen;
					idenGen.isType = true;
					idenGen.type = g_TypeReg.Generic(TypeMod::None, id);
					generics.push_back(idenGen);

					// Constraints
					if (idx < data.length() &&
						data[idx] == 'C')
					{
						++idx;
						while (data[idx] != 'Z')
						{
							TypeHandle constraint = DemangleType(data, idx);

							if (pBoundsInfo)
							{
								Bounds& bound = pBoundsInfo->GetOrAddBounds(idenGen.type);
								bound.bounds.push_back(constraint);
							}
						}
						++idx;
					}
					break;
				}
				case 'V':
				{
					++idx;
					StdString genName = DemangleLName(data, idx);
					TypeHandle type = DemangleType(data, idx);

					IdenGeneric idenGen;
					idenGen.isType = false;
					idenGen.iden = genName;
					idenGen.type = type;

					generics.push_back(idenGen);
					break;
				}
				case 'U':
				{
					++idx;
					TypeHandle type = DemangleType(data, idx);

					IdenGeneric idenGen;
					idenGen.isType = true;
					idenGen.isSpecialized = true;
					idenGen.type = type;
					generics.push_back(idenGen);
					break;
				}
				case 'W':
				{
					// TODO
					break;
				}
				default:;
				}
			}
			++idx;
		}

		return generics;
	}

	TypeHandle DemangleType(StdStringView data)
	{
		usize tmp = 0;
		return DemangleType(data, tmp);
	}

	TypeHandle DemangleType(StdStringView data, usize& idx)
	{
		TypeMod mod = TypeMod::None;
		if (data[idx] == 'M')
		{
			++idx;
			mod = TypeMod::Mut;
		}
		
		switch (data[idx])
		{
		case 'b': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::Bool); }
		case 'c': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::Char); }
		case 'i': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::I8); }
		case 'j': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::I16); }
		case 'k': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::I32); }
		case 'l': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::I64); }
		case 'm': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::I128); }
		case 'n': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::ISize); }
		case 'u': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::U8); }
		case 'v': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::U16); }
		case 'w': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::U32); }
		case 'x': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::U64); }
		case 'y': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::U128); }
		case 'z': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::USize); }
		case 'e': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::F16); }
		case 'f': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::F32); }
		case 'g': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::F64); }
		case 'h': { ++idx; return g_TypeReg.Builtin(mod, BuiltinTypeKind::F128); }
		case 'P':
		{
			++idx;
			TypeHandle subType = DemangleType(data, idx);
			return g_TypeReg.Ptr(mod, subType);
		}
		case 'R':
		{
			++idx;
			TypeHandle subType = DemangleType(data, idx);
			return g_TypeReg.Ref(mod, subType);
		}
		case 'S':
		{
			++idx;
			TypeHandle subType = DemangleType(data, idx);
			return g_TypeReg.Slice(mod, subType);
		}
		case 'A':
		{
			++idx;
			u64 size;
			if (data[idx] == '_')
			{
				size = u64(-1);
				++idx;
			}
			else
			{
				usize end = data.find_first_not_of("0123456789", idx);
				std::from_chars(data.data() + idx, data.data() + end, size);
			}
			
			TypeHandle subType = DemangleType(data, idx);
			return g_TypeReg.Array(mod, subType, size);
		}
		case 'O':
		{
			++idx;
			TypeHandle subType = DemangleType(data, idx);
			return g_TypeReg.Opt(mod, subType);
		}
		case 'T':
		{
			++idx;
			StdVector<TypeHandle> subTypes;
			while (data[idx] != 'Z')
			{
				TypeHandle subType = DemangleType(data, idx);
				subTypes.push_back(subType);
			}
			++idx;
			return g_TypeReg.Tuple(mod, subTypes);
		}
		case 'F':
		{
			++idx;
			StdVector<TypeHandle> subTypes;
			while (data[idx] != 'Z')
			{
				TypeHandle subType = DemangleType(data, idx);
				subTypes.push_back(subType);
			}
			++idx;

			TypeHandle retType;
			if (data[idx] != 'Z')
			{
				retType = DemangleType(data, idx);
			}
			++idx;
			
			return g_TypeReg.Func(mod, subTypes, retType);
		}
		case 'H':
		{
			++idx;
			usize id = DemangleUSize(data, idx);
			// TODO: bounds
			return g_TypeReg.Generic(mod, u16(id));
		}
		default:
		{
			if (isdigit(data[idx]))
			{
				QualNameSPtr qualName = DemangleQualName(data, idx, nullptr);
				return g_TypeReg.Iden(mod, qualName);
			}

			return TypeHandle{};
		}
		}
	}

	usize DemangleUSize(StdStringView data, usize& idx)
	{
		usize end = data.find_first_not_of("0123456789", idx);
		if (end == StdString::npos)
			end = data.size();
		
		u64 size;
		std::from_chars(data.data() + idx, data.data() + end, size);
		idx = end;
		return size;
	}

	StdString DemangleLName(StdStringView data, usize& idx)
	{
		usize size = DemangleUSize(data, idx);
		StdString name = StdString{ data.substr(idx, size) };
		idx += size;
		return name;
	}
}
