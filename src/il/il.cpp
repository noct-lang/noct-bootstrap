#include "il.hpp"

namespace Noctis
{
	StdString GetCompIntrinName(ILCompIntrinKind intrin)
	{
		switch (intrin)
		{
		case ILCompIntrinKind::SizeOf: return "sizeof";
		case ILCompIntrinKind::AlignOf: return "alignof";
		case ILCompIntrinKind::AlignOfVal: return "alignofval";
		case ILCompIntrinKind::BytewiseCopy: return "bytewisecopy";
		case ILCompIntrinKind::FuzzyTypeComp: return "fuzzytypecomp";
		default: return "";
		}
	}

	bool HasCompIntrinReturn(ILCompIntrinKind intrin)
	{
		switch (intrin)
		{
		case ILCompIntrinKind::SizeOf:
		case ILCompIntrinKind::AlignOf:
		case ILCompIntrinKind::AlignOfVal:
		case ILCompIntrinKind::BytewiseCopy:
			return true;
		case ILCompIntrinKind::FuzzyTypeComp:
		default:
			return false;
		}
	}

	usize GetCompIntrinVarCount(ILCompIntrinKind intrin)
	{
		switch (intrin)
		{
		case ILCompIntrinKind::SizeOf: return 0;
		case ILCompIntrinKind::AlignOf: return 0;
		case ILCompIntrinKind::AlignOfVal: return 1;
		case ILCompIntrinKind::BytewiseCopy: return 1;
		case ILCompIntrinKind::FuzzyTypeComp: return 0;
		default: return 0;
		}
	}

	usize GetCompIntrinTypeCount(ILCompIntrinKind intrin)
	{
		switch (intrin)
		{
		case ILCompIntrinKind::SizeOf: return 1;
		case ILCompIntrinKind::AlignOf: return 1;
		case ILCompIntrinKind::AlignOfVal: return 0;
		case ILCompIntrinKind::BytewiseCopy: return 0;
		case ILCompIntrinKind::FuzzyTypeComp: return 2;
		default: return 0;
		}
	}

	ILVar::ILVar()
		: kind(ILVarKind::Copy)
		, id(0)
	{
	}

	ILVar::ILVar(ILVarKind kind, u32 id, TypeHandle type)
		: kind(kind)
		, id(id)
		, type(type)
	{
	}

	ILVar::ILVar(ILLitType lit, const StdVector<u8>& data)
		: kind(ILVarKind::Lit)
		, litType(lit)
		, boolBit(false)
		, litData(data)
	{
	}

	ILVar::ILVar(ILLitType lit, u64 val)
		: kind(ILVarKind::Lit)
		, litType(lit)
		, boolBit(false)
	{
		switch (lit)
		{
		case ILLitType::U8:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(u8));
			break;
		}
		case ILLitType::U16:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(u16));
			break;
		}
		case ILLitType::U32:
		case ILLitType::Char:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(u32));
			break;
		}
		case ILLitType::U64:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(u64));
			break;
		}
		case ILLitType::U128: break;
		default: ;
		}
	}

	ILVar::ILVar(ILLitType lit, i64 val)
		: kind(ILVarKind::Lit)
		, litType(lit)
		, boolBit(false)
	{
		switch (lit)
		{
		case ILLitType::I8:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(i8));
			break;
		}
		case ILLitType::I16:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(i16));
			break;
		}
		case ILLitType::I32:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(i32));
			break;
		}
		case ILLitType::I64:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(i64));
			break;
		}
		case ILLitType::I128: break;
		default:;
		}
	}

	ILVar::ILVar(ILLitType lit, f64 val)
		: kind(ILVarKind::Lit)
		, litType(lit)
		, boolBit(false)
	{
		switch (lit)
		{
		case ILLitType::F32:
		{
			f32 val32 = f32(val);
			u8* addr = reinterpret_cast<u8*>(&val32);
			litData.insert(litData.begin(), addr, addr + sizeof(i32));
			break;
		}
		case ILLitType::F64:
		{
			u8* addr = reinterpret_cast<u8*>(&val);
			litData.insert(litData.begin(), addr, addr + sizeof(i64));
			break;
		}
		default:;
		}
	}

	ILVar::ILVar(bool bval)
		: kind(ILVarKind::Lit)
		, litType(ILLitType::Bool)
		, boolBit(bval)
	{
	}

	ILElem::ILElem(ILKind kind)
		: kind(kind)
	{
	}

	ILTerminal::ILTerminal(ILKind kind)
		: ILElem(kind)
	{
	}

	ILBlock::ILBlock(u32 label)
		: ILElem(ILKind::Block)
		, label(label)
	{
	}

	ILIf::ILIf(ILVar cond, u32 trueLabel, u32 falseLabel)
		: ILTerminal(ILKind::If)
		, cond(std::move(cond))
		, trueLabel(trueLabel)
		, falseLabel(falseLabel)
	{
	}

	ILSwitch::ILSwitch(ILVar cond, const StdPairVector<ILVar, u32>& cases, u32 defCase)
		: ILTerminal(ILKind::Switch)
		, cond(cond)
		, cases(cases)
		, defCase(defCase)
	{
	}

	ILGoto::ILGoto(u32 label)
		: ILTerminal(ILKind::Goto)
		, label(label)
	{
	}

	ILReturn::ILReturn()
		: ILTerminal(ILKind::ReturnNoVal)
	{
	}

	ILReturn::ILReturn(ILVar var)
		: ILTerminal(ILKind::ReturnVal)
		, var(std::move(var))
	{
	}

	ILUnreachable::ILUnreachable()
		: ILTerminal(ILKind::Unreachable)
	{
	}

	ILAssign::ILAssign(ILVar dst, ILVar src)
		: ILElem(ILKind::Assign)
		, dst(std::move(dst))
		, src(std::move(src))
	{
	}

	ILPrimAssign::ILPrimAssign(OperatorKind op, ILVar dst, ILVar src)
		: ILElem(ILKind::PrimAssign)
		, op(op)
		, dst(std::move(dst))
		, src(std::move(src))
	{
	}

	ILPrimBinary::ILPrimBinary(OperatorKind op, ILVar dst, ILVar src0, ILVar src1)
		: ILElem(ILKind::PrimBinary)
		, op(op)
		, dst(std::move(dst))
		, src0(std::move(src0))
		, src1(std::move(src1))
	{
	}

	ILPrimUnary::ILPrimUnary(OperatorKind op, ILVar dst, ILVar src)
		: ILElem(ILKind::PrimUnary)
		, op(op)
		, dst(std::move(dst))
		, src(std::move(src))
	{
	}

	ILPrimCast::ILPrimCast(ILVar dst, ILVar src)
		: ILElem(ILKind::PrimCast)
		, dst(std::move(dst))
		, src(std::move(src))
	{
	}

	ILTernary::ILTernary(ILVar dst, ILVar cond, ILVar src0, ILVar src1)
		: ILElem(ILKind::Ternary)
		, dst(std::move(dst))
		, cond(std::move(cond))
		, src0(std::move(src0))
		, src1(std::move(src1))
	{
	}

	ILTransmute::ILTransmute(ILVar dst, ILVar src)
		: ILElem(ILKind::Transmute)
		, dst(std::move(dst))
		, src(std::move(src))
	{
	}

	ILIndex::ILIndex(ILVar dst, ILVar src, ILVar idx)
		: ILElem(ILKind::Index)
		, dst(dst)
		, src(src)
		, idx(idx)
	{
	}

	ILCompIntrin::ILCompIntrin(ILVar dst, ILCompIntrinKind kind, const StdVector<ILVar>& vars,
		const StdVector<TypeHandle>& types)
		: ILElem(ILKind::CompIntrin)
		, dst(dst)
		, intrin(kind)
		, vars(vars)
		, types(types)
	{
	}

	ILFuncCall::ILFuncCall(QualNameSPtr func, const StdVector<ILVar>& args)
		: ILElem(ILKind::FuncCallNoRet)
		, func(func)
		, args(args)
	{
	}

	ILFuncCall::ILFuncCall(ILVar dst, QualNameSPtr func, const StdVector<ILVar>& args)
		: ILElem(ILKind::FuncCallRet)
		, dst(std::move(dst))
		, func(func)
		, args(args)
	{
	}

	ILMethodCall::ILMethodCall(ILVar caller, const StdString& func, const StdVector<ILVar>& args)
		: ILElem(ILKind::MethodCallNoRet)
		, caller(caller)
		, func(func)
		, args(args)
	{
	}

	ILMethodCall::ILMethodCall(ILVar dst, ILVar caller, const StdString& func, const StdVector<ILVar>& args)
		: ILElem(ILKind::MethodCallRet)
		, dst(std::move(dst))
		, caller(caller)
		, func(func)
		, args(args)
	{
	}

	ILIndirectCall::ILIndirectCall(ILVar func, const StdVector<ILVar>& args)
		: ILElem(ILKind::IndirectCallNoRet)
		, func(func)
		, args(args)
	{
	}

	ILIndirectCall::ILIndirectCall(ILVar dst, ILVar func, const StdVector<ILVar>& args)
		: ILElem(ILKind::IndirectCallRet)
		, dst(dst)
		, func(func)
		, args(args)
	{
	}

	ILMemberAccess::ILMemberAccess(ILVar dst, ILVar src, const StdString& name)
		: ILElem(ILKind::MemberAccess)
		, dst(std::move(dst))
		, src(std::move(src))
		, name(name)
	{
	}

	ILTupleAccess::ILTupleAccess(ILVar dst, ILVar src, u16 index)
		: ILElem(ILKind::TupleAccess)
		, dst(std::move(dst))
		, src(std::move(src))
		, index(index)
	{
	}

	ILStructInit::ILStructInit(ILVar dst, const StdVector<ILVar>& args)
		: ILElem(ILKind::StructInit)
		, dst(std::move(dst))
		, args(args)
	{
	}

	ILUnionInit::ILUnionInit(ILVar dst, ILVar arg)
		: ILElem(ILKind::UnionInit)
		, dst(std::move(dst))
		, arg(std::move(arg))
	{
	}

	ILValEnumInit::ILValEnumInit(ILVar dst, const StdString& member)
		: ILElem(ILKind::ValEnumInit)
		, dst(std::move(dst))
		, member(member)
	{
	}

	ILAdtEnumInit::ILAdtEnumInit(ILVar dst, const StdString& member, const StdVector<ILVar>& args)
		: ILElem(ILKind::AdtEnumInit)
		, dst(std::move(dst))
		, member(member)
		, args(args)
	{
	}

	ILTupInit::ILTupInit(ILVar dst, const StdVector<ILVar>& args)
		: ILElem(ILKind::TupInit)
		, dst(std::move(dst))
		, args(args)
	{
	}

	ILArrInit::ILArrInit(ILVar dst, const StdVector<ILVar>& args)
		: ILElem(ILKind::ArrInit)
		, dst(std::move(dst))
		, args(args)
	{
	}

	ILFuncDef::ILFuncDef(Context* pCtx, QualNameSPtr qualName, StdVector<ILGeneric>&& generics)
		: ILElem(ILKind::FuncDef)
		, qualName(qualName)
		, generics(std::move(generics))
		, graph(pCtx)
	{
	}
}
