#include "il.hpp"

namespace Noctis
{
	ILVar::ILVar()
		: id(0)
		, type(TypeHandle(-1))
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
		, type(TypeHandle(-1))
		, litData(data)
	{
	}

	ILVar::ILVar(bool bval)
		: kind(ILVarKind::Lit)
		, litType(ILLitType::Bool)
		, boolBit(bval)
		, type(TypeHandle(-1))
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

	ILSwitch::ILSwitch()
		: ILTerminal(ILKind::Switch)
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

	ILFuncCall::ILFuncCall(const StdString& func, const StdVector<ILVar>& args)
		: ILElem(ILKind::FuncCallNoRet)
		, func(func)
		, args(args)
	{
	}

	ILFuncCall::ILFuncCall(ILVar dst, const StdString& func, const StdVector<ILVar>& args)
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
		: ILElem(ILKind::MethodCallNoRet)
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

	ILUnionInit::ILUnionInit(ILVar dst, const StdVector<ILVar>& args)
		: ILElem(ILKind::UnionInit)
		, dst(std::move(dst))
		, args(args)
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

	ILFuncDef::ILFuncDef(const StdString& mangleName)
		: ILElem(ILKind::FuncDef)
		, mangleName(mangleName)
	{
	}
}
