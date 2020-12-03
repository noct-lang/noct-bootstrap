#include "il-interp.hpp"
#include "common/context.hpp"
#include "module/module.hpp"

namespace Noctis
{
	// NOTE: interpreter depends on the machine being little endian
	
	void PrimBinaryOp(u8* dst, u8* src0, u8* src1, u16 size, const std::function<u64(u64, u64)>& op) 
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0) & mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1) & mask;

		u64 tmp = op(rSrc0, rSrc1);

		memcpy(dst, &tmp, size);
	}

	void PrimBinaryOpSigned(u8* dst, u8* src0, u8* src1, u16 size, const std::function<i64(i64, i64)>& op)
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0) & mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1) & mask;

		i64 tmp = op(i64(rSrc0), i64(rSrc1));

		memcpy(dst, &tmp, size);
	}

	void PrimBinaryOpFp(u8* dst, u8* src0, u8* src1, u16 size, const std::function<f64(f64, f64)>& op)
	{
		if (size == 4)
		{
			f32 rSrc0 = *reinterpret_cast<f32*>(dst);
			f32 rSrc1 = *reinterpret_cast<f32*>(dst);
			f32& rDst = *reinterpret_cast<f32*>(dst);
			rDst = f32(op(rSrc0, rSrc1));
		}
		else
		{
			f64 rSrc0 = *reinterpret_cast<f64*>(dst);
			f64 rSrc1 = *reinterpret_cast<f64*>(dst);
			f64& rDst = *reinterpret_cast<f64*>(dst);
			rDst = op(rSrc0, rSrc1);
		}
	}

	void PrimAShr(u8* dst, u8* src0, u8* src1, u16 size)
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0)& mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1)& mask;

		u64 sign = rSrc0 << (bitShift - 1);
		sign <<= bitShift;
		
		i64 tmp = rSrc0 >> rSrc1;
		tmp |= sign;
		memcpy(dst, &tmp, size);
	}

	void PrimRotl(u8* dst, u8* src0, u8* src1, u16 size)
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0)& mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1)& mask;

		rSrc1 &= size - 1;

		i64 tmp = rSrc0 << rSrc1 | rSrc0 >> (size - rSrc1);
		memcpy(dst, &tmp, size);
	}

	void PrimRotr(u8* dst, u8* src0, u8* src1, u16 size)
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0)& mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1)& mask;

		i64 tmp = rSrc0 >> rSrc1 | rSrc0 << (size - rSrc1);
		memcpy(dst, &tmp, size);
	}

	void PrimUnaryOp(u8* dst, u8* src0, u8* src1, u16 size, const std::function<u64(u64, u64)>& op)
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;
		u64 invMask = ~mask;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0)& mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1)& mask;

		u64 tmp = op(rSrc0, rSrc1);

		u64& rDst = *reinterpret_cast<u64*>(dst);
		rDst &= invMask;
		rDst |= tmp & mask;
	}

	void PrimUnaryOpSigned(u8* dst, u8* src0, u8* src1, u16 size, const std::function<i64(i64, i64)>& op)
	{
		u16 shift = 8 - size;
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;
		u64 invMask = ~mask;

		u64 rSrc0 = *reinterpret_cast<u64*>(src0)& mask;
		u64 rSrc1 = *reinterpret_cast<u64*>(src1)& mask;

		i64 tmp = op(i64(rSrc0), i64(rSrc1));

		u64& rDst = *reinterpret_cast<u64*>(dst);
		rDst &= invMask;
		rDst |= tmp & mask;
	}

	void PrimUnaryOpFp(u8* dst, u8* src0, u8* src1, u16 size, const std::function<f64(f64, f64)>& op)
	{
		if (size == 4)
		{
			f32 rSrc0 = *reinterpret_cast<f32*>(dst);
			f32 rSrc1 = *reinterpret_cast<f32*>(dst);
			f32& rDst = *reinterpret_cast<f32*>(dst);
			rDst = f32(op(rSrc0, rSrc1));
		}
		else
		{
			f64 rSrc0 = *reinterpret_cast<f64*>(dst);
			f64 rSrc1 = *reinterpret_cast<f64*>(dst);
			f64& rDst = *reinterpret_cast<f64*>(dst);
			rDst = op(rSrc0, rSrc1);
		}
	}
	
	ILInterpVar::ILInterpVar()
		: stackIdx(u64(-1))
		, size(0)
	{
	}

	ILInterpVar::ILInterpVar(ILVar var)
		: var(std::move(var))
		, stackIdx(u64(-1))
		, size(0)
	{
	}

	ILInterpStackFrame::ILInterpStackFrame(u64 retIdx, u64 initialStackPos)
		: stackIdx(initialStackPos)
		, retIdx(retIdx)
	{
	}

	ILInterpVar& ILInterpStackFrame::GetVar(Context* pCtx, ILVar& var)
	{
		u32 id = var.id;
		if (vars.size() <= id)
		{
			vars.resize(usize(id) + 1);
			vars[id] = ILInterpVar{ var };
			TypeSPtr type = var.type.Type();
			vars[id].size = type->Size();
			vars[id].tmpData.resize(type->Size());
		}
		return vars[id];
	}

	ILInterp::ILInterp(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	void ILInterp::Interp(QualNameSPtr funcName)
	{
		Interp(funcName, {});
	}

	void ILInterp::Interp(QualNameSPtr funcName, const StdVector<ILInterpVar>& args)
	{
		ILFuncDefSPtr funcDef = GetFuncDef(funcName);
		if (funcDef)
			Interp(*funcDef, args);
	}

	ILFuncDefSPtr ILInterp::GetFuncDef(QualNameSPtr funcName)
	{
		for (StdPair<const QualNameSPtr, ModuleSPtr>& pair : m_pCtx->modules)
		{
			for (ILFuncDefSPtr funcDef : pair.second->ilMod.funcs)
			{
				if (funcDef->qualName == funcName)
					return funcDef;
			}
		}
		return nullptr;
	}

	void ILInterp::SetupStackFrame(ILFuncDef& func, const StdVector<ILInterpVar>& args, u64 retIdx)
	{
		u64 initialStackPos;
		if (m_Frames.empty())
			initialStackPos = 0;
		else
			initialStackPos = m_Frames.top().stackIdx;
		
		m_Frames.push(ILInterpStackFrame{ retIdx, initialStackPos });
		ILInterpStackFrame& stackFrame = m_Frames.top();

		// Setup params and copy data to stack
		for (usize i = 0; i < func.params.size(); ++i)
		{
			ILVar& param = func.params[i];
			const ILInterpVar& arg = args[i];

			ILInterpVar var{ param };

			TypeSPtr type = var.var.type.Type();
			PadStack(type->Align());
			var.stackIdx = stackFrame.stackIdx;
			var.size = type->Size();

			switch (arg.var.kind)
			{
			case ILVarKind::Copy:
			case ILVarKind::Ref:
				if (var.stackIdx != u64(-1))
					StackCopy(arg.stackIdx, var.stackIdx, type->Size());
				else
					StackCopy(arg.tmpData, var.stackIdx);
				stackFrame.stackIdx += var.size;
				break;

				// No need for copying if arg was moved in
			case ILVarKind::Move:
				var.stackIdx = arg.stackIdx;
				break;
			case ILVarKind::Lit:
				StackCopy(arg.var.litData, var.stackIdx);
				stackFrame.stackIdx += var.size;
				break;
			default: ;
			}
			

			stackFrame.vars.push_back(var);
		}

		// Setup local vars on the stack
		for (usize i = 0; i < func.localVars.size(); ++i)
		{
			ILVar localVar = func.localVars[i];

			ILInterpVar var{ localVar };
			var.var = localVar;

			TypeSPtr type = var.var.type.Type();
			PadStack(type->Align());
			var.stackIdx = stackFrame.stackIdx;
			var.size = type->Size();
			ReserveStackSpace(var.size);
			stackFrame.stackIdx += type->Size();

			stackFrame.vars.push_back(var);
		}

		m_Frames.push(stackFrame);
	}

	void ILInterp::Interp(ILFuncDef& funcDef, const StdVector<ILInterpVar>& args, u64 retIdx)
	{
		SetupStackFrame(funcDef, args, retIdx);

		u32 id = 0;
		do
		{
			ILBlock& block = funcDef.blocks[id];
			id = Interp(block);
		}
		while (id != u32(-1));

		m_Frames.pop();
	}

	u32 ILInterp::Interp(ILBlock& block)
	{
		for (ILElemSPtr elem : block.elems)
		{
			Interp(*elem);
		}
		return Interp(*block.terminal);
	}

	u32 ILInterp::Interp(ILElem& elem)
	{
		switch (elem.kind)
		{
		case ILKind::Block: Interp(static_cast<ILBlock&>(elem)); break;
		case ILKind::If: Interp(static_cast<ILIf&>(elem)); break;
		case ILKind::Switch: break;
		case ILKind::Goto: break;
		case ILKind::ReturnNoVal: break;
		case ILKind::ReturnVal: Interp(static_cast<ILReturn&>(elem)); return true;
		case ILKind::Assign: Interp(static_cast<ILAssign&>(elem)); break;
		case ILKind::PrimAssign: Interp(static_cast<ILPrimAssign&>(elem)); break;
		case ILKind::PrimBinary: Interp(static_cast<ILPrimBinary&>(elem)); break;
		case ILKind::PrimUnary: break;
		case ILKind::PrimCast: Interp(static_cast<ILPrimCast&>(elem)); break;
		case ILKind::Ternary: Interp(static_cast<ILTernary&>(elem)); break;
		case ILKind::Transmute: break;
		case ILKind::CompIntrin: break;
		case ILKind::StaticCallNoRet: Interp(static_cast<ILStaticCall&>(elem)); break;
		case ILKind::StaticCallRet:  Interp(static_cast<ILStaticCall&>(elem)); break;
		case ILKind::DynamicCallNoRet: break;
		case ILKind::DynamicCallRet: break;
		case ILKind::MemberAccess: Interp(static_cast<ILMemberAccess&>(elem)); break;
		case ILKind::TupleAccess: Interp(static_cast<ILTupleAccess&>(elem)); break;
		case ILKind::StructInit: Interp(static_cast<ILStructInit&>(elem)); break;
		case ILKind::UnionInit: break;
		case ILKind::ValEnumInit: break;
		case ILKind::AdtEnumInit: break;
		case ILKind::TupInit: Interp(static_cast<ILTupInit&>(elem)); break;
		case ILKind::ArrInit: Interp(static_cast<ILArrInit&>(elem)); break;
		default: ;
		}

		return u32(-1);
	}

	u32 ILInterp::Interp(ILIf& ifElem)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		u8* condAddr = GetSrcAddr(stackFrame, ifElem.cond);
		return *condAddr ? ifElem.trueLabel : ifElem.falseLabel;
	}

	void ILInterp::Interp(ILReturn& ret)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar var = stackFrame.GetVar(m_pCtx, ret.var);
		u8* srcAddress = GetSrcAddr(stackFrame, ret.var);
		u8* retAdrress = m_Stack.data() + stackFrame.retIdx;

		memcpy(retAdrress, srcAddress, var.size);
	}

	void ILInterp::Interp(ILAssign& assign)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, assign.dst);
		if (assign.src.kind == ILVarKind::Lit)
		{
			StackCopy(assign.src.litData, dst.stackIdx);
		}
		else
		{
			ILInterpVar& src = stackFrame.GetVar(m_pCtx, assign.src);
			if (src.tmpData.empty())
			{
				StackCopy(src.stackIdx, dst.stackIdx, src.size);
			}
			else
			{
				StackCopy(src.tmpData, dst.stackIdx);
			}
		}
		
	}

	void ILInterp::Interp(ILPrimAssign& assign)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();
		
		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, assign.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);
		u8* srcAddr = GetSrcAddr(stackFrame, assign.src);

		BuiltinType& type = assign.dst.type.AsBuiltin();
		bool isSigned = type.IsSigned();
		bool isFp = type.IsFp();
		
		switch (assign.op)
		{
		case OperatorKind::AddAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first + last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first + last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first + last; });
			break;
		case OperatorKind::SubAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first - last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first - last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first - last; });
			break;
		case OperatorKind::MulAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first * last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first * last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first * last; });
			break;
		case OperatorKind::DivAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first / last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first / last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first / last; });
			break;
		case OperatorKind::RemAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first % last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](f64 first, f64 last) -> f64 { return fmod(first, last); });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first % last; });
			break;
		case OperatorKind::LShlAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> u64 { return first << last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::AShlAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> u64 { return first << last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::RotlAssign:
			PrimRotl(dstAddr, dstAddr, srcAddr, u16(dst.size));
			break;
		case OperatorKind::LShrAssign:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, dstAddr, srcAddr, u16(dst.size), [](i64 first, i64 last) -> u64 { return first << last; });
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::AShrAssign:
			if (isSigned)
				PrimAShr(dstAddr, dstAddr, srcAddr, u16(dst.size));
			else
				PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::RotrAssign:
			PrimRotl(dstAddr, dstAddr, srcAddr, u16(dst.size));
			break;
		case OperatorKind::BinOrAssign:
			PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first | last; });
			break;
		case OperatorKind::BinXorAssign:
			PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first ^ last; });
			break;
		case OperatorKind::BinAndAssign:
			PrimBinaryOp(dstAddr, dstAddr, srcAddr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first & last; });
			break;
			
		case OperatorKind::ConcatAssign: break;
		case OperatorKind::NullCoalesceAssign: break;
		default: ;
		}
	}

	void ILInterp::Interp(ILPrimBinary& binary)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, binary.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);
		u8* src0Addr = GetSrcAddr(stackFrame, binary.src0);
		u8* src1Addr = GetSrcAddr(stackFrame, binary.src1);

		BuiltinType& type = binary.dst.type.AsBuiltin();
		bool isSigned = type.IsSigned();
		bool isFp = type.IsFp();

		switch (binary.op)
		{
		case OperatorKind::Add:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first + last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first + last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first + last; });
			break;
		case OperatorKind::Sub:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first - last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first - last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first - last; });
			break;
		case OperatorKind::Mul:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first * last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first * last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first * last; });
			break;
		case OperatorKind::Div:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first / last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](f64 first, f64 last) -> f64 { return first / last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first / last; });
			break;
		case OperatorKind::Rem:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> i64 { return first % last; });
			else if (isFp)
				PrimBinaryOpFp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](f64 first, f64 last) -> f64 { return fmod(first, last); });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first % last; });
			break;
		case OperatorKind::LShl:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> u64 { return first << last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::AShl:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> u64 { return first << last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::Rotl:
			PrimRotl(dstAddr, src0Addr, src1Addr, u16(dst.size));
			break;
		case OperatorKind::LShr:
			if (isSigned)
				PrimBinaryOpSigned(dstAddr, src0Addr, src1Addr, u16(dst.size), [](i64 first, i64 last) -> u64 { return first << last; });
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::AShr:
			if (isSigned)
				PrimAShr(dstAddr, src0Addr, src1Addr, u16(dst.size));
			else
				PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first << last; });
			break;
		case OperatorKind::Rotr:
			PrimRotl(dstAddr, src0Addr, src1Addr, u16(dst.size));
			break;
		case OperatorKind::BinOr:
			PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first | last; });
			break;
		case OperatorKind::BinXor:
			PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first ^ last; });
			break;
		case OperatorKind::BinAnd:
			PrimBinaryOp(dstAddr, src0Addr, src1Addr, u16(dst.size), [](u64 first, u64 last) -> u64 { return first & last; });
			break;

		case OperatorKind::Concat: break;
		case OperatorKind::NullCoalesce: break;
		default:;
		}
	}

	void ILInterp::Interp(ILPrimUnary& unary)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, unary.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);

		u8* srcAdd = GetSrcAddr(stackFrame, unary.src);

		BuiltinType& type = unary.dst.type.AsBuiltin();
		bool isSigned = type.IsSigned();
		bool isFp = type.IsFp();

		// TODO

		switch (unary.op)
		{
		case OperatorKind::Pos: break;
		case OperatorKind::Neg: break;
		case OperatorKind::PreInc: break;
		case OperatorKind::PreDec: break;
		case OperatorKind::Not: break;
		case OperatorKind::BinNeg: break;
		case OperatorKind::Deref: break;
		case OperatorKind::RefOrAddrOf: break;
		case OperatorKind::BoolConv: break;
		case OperatorKind::PostInc: break;
		case OperatorKind::PostDec: break;
		case OperatorKind::NullPanic: break;
		default: ;
		}
	}

	void ILInterp::Interp(ILPrimCast& cast)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();
		
		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, cast.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);

		u8* srcAddr = GetSrcAddr(stackFrame, cast.src);

		u64 srcSize;
		TypeHandle srcType;
		GetVarType(cast.src, srcType, srcSize);

		if (srcType == cast.dst.type)
		{
			memcpy(dstAddr, srcAddr, dst.size);
			return;
		}
		
		BuiltinType& fromType = srcType.AsBuiltin();
		bool fromSigned = fromType.IsSigned();
		bool fromFp = fromType.IsFp();

		BuiltinType& toType = cast.dst.type.AsBuiltin();
		bool toSigned = toType.IsSigned();
		bool toFp = toType.IsFp();

		u16 shift = u16(8 - dst.size);
		u16 bitShift = shift << 3;
		u64 mask = 0xFFFF'FFFF'FFFF'FFFF >> bitShift;
		u64 signMask = 0x1000'0000'0000'0000 >> bitShift;
		u64 invMask = ~mask;

		// TODO: support for f16 and f128
		if (fromFp)
		{
			f64 val;
			if (srcSize == 4)
			{
				val = f64(*reinterpret_cast<f32*>(srcAddr));
			}
			else
			{
				val = *reinterpret_cast<f64*>(srcAddr);
			}
			
			if (toFp)
			{
				if (dst.size == 4)
				{
					f32 fval = f32(val);
					memcpy(dstAddr, &fval, sizeof(fval));
				}
				else
				{
					f64 fval = f64(val);
					memcpy(dstAddr, &fval, sizeof(fval));
				}
			}
			else if (toSigned)
			{
				i64 ival = i64(val);
				memcpy(dstAddr, &ival, dst.size);
			}
			else
			{
				u64 uval = u64(val);
				memcpy(dstAddr, &uval, dst.size);
			}
		}
		else if (fromSigned)
		{
			i64 val = *reinterpret_cast<u64*>(srcAddr);
			val &= mask;
			bool sign = val & signMask;
			u64 hiBits = sign ? invMask : 0;
			val |= hiBits;
			
			if (toFp)
			{
				if (dst.size == 4)
				{
					f32 fval = f32(val);
					memcpy(dstAddr, &fval, sizeof(fval));
				}
				else
				{
					f64 fval = f64(val);
					memcpy(dstAddr, &fval, sizeof(fval));
				}
			}
			else if (toSigned)
			{
				i64 val = *reinterpret_cast<u64*>(srcAddr);
				val &= mask;
				memcpy(dstAddr, &val, dst.size);
			}
			else
			{
				memcpy(dstAddr, &val, dst.size);
			}
		}
		else
		{
			u64 val = *reinterpret_cast<u64*>(srcAddr);
			val &= mask;
			
			if (toFp)
			{
				if (dst.size == 4)
				{
					f32 fval = f32(val);
					memcpy(dstAddr, &fval, sizeof(fval));
				}
				else
				{
					f64 fval = f64(val);
					memcpy(dstAddr, &fval, sizeof(fval));
				}
			}
			else
			{
				bool sign = val & signMask;
				u64 hiBits = toSigned && sign ? invMask : 0;
				val |= hiBits;
				memcpy(dstAddr, &val, dst.size);
			}
		}
	}

	void ILInterp::Interp(ILTernary& ternary)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, ternary.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);
		u8* condAddr = GetSrcAddr(stackFrame, ternary.cond);
		u8* src0Addr = GetSrcAddr(stackFrame, ternary.src0);
		u8* src1Addr = GetSrcAddr(stackFrame, ternary.src1);

		memcpy(dstAddr, *condAddr ? src0Addr : src1Addr, dst.size);
	}

	void ILInterp::Interp(ILMemberAccess& access)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, access.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);
		u8* srcAddr = GetSrcAddr(stackFrame, access.src);

		TypeSPtr type = access.src.type.Type();
		SymbolSPtr sym = type->AsIden().sym.lock();
		SymbolSPtr child = sym->children->FindChild(nullptr, Iden::Create(access.name));
		memcpy(dstAddr, srcAddr + child->offset, dst.size);
	}

	void ILInterp::Interp(ILTupleAccess& access)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, access.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);

		TupleType& tupType = access.src.type.AsTuple();

		u8* srcAddr = GetSrcAddr(stackFrame, access.src);
		u8* readAddr = srcAddr + tupType.offsets[access.index];

		memcpy(dstAddr, readAddr, dst.size);
	}

	void ILInterp::Interp(ILStructInit& init)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, init.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);

		TypeSPtr type = init.dst.type.Type();
		SymbolSPtr sym = type->AsIden().sym.lock();

		for (usize i = 0; i < init.args.size(); ++i)
		{
			ILVar src = init.args[i];
			SymbolSPtr child = sym->orderedVarChildren[i].lock();

			u8* srcAddr = GetSrcAddr(stackFrame, src);
			memcpy(dstAddr + child->offset, srcAddr, child->size);
		}
	}

	void ILInterp::Interp(ILTupInit& init)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, init.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);

		TupleType& tupType = init.dst.type.AsTuple();
		for (usize i = 0; i < init.args.size(); ++i)
		{
			ILVar& arg = init.args[i];
			u8* writeAddr = dstAddr + tupType.offsets[i];

			u64 srcSize;
			TypeHandle srcType;
			GetVarType(arg, srcType, srcSize);

			u8* srcAddr = GetSrcAddr(stackFrame, arg);
			memcpy(writeAddr, srcAddr, srcSize);
		}
	}

	void ILInterp::Interp(ILArrInit& init)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();

		ILInterpVar& dst = stackFrame.GetVar(m_pCtx, init.dst);
		u8* dstAddr = GetDstAddr(stackFrame, dst);

		ArrayType& arrType = init.dst.type.AsArray();
		TypeSPtr subType = arrType.subType.Type();

		u64 curOffset = 0;
		for (ILVar& arg : init.args)
		{
			u8* srcAddr = GetSrcAddr(stackFrame, arg);
			memcpy(dstAddr + curOffset, srcAddr, subType->Size());
			curOffset += subType->Size();
		}
	}

	void ILInterp::Interp(ILStaticCall& call)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();
		
		StdVector<ILInterpVar> args;
		for (ILVar& var : call.args)
		{
			args.push_back(stackFrame.GetVar(m_pCtx, var));
		}

		u64 stackRet = u64(-1);
		if (call.kind == ILKind::StaticCallRet)
		{
			ILInterpVar& var = stackFrame.GetVar(m_pCtx, call.dst);
			var.var = call.dst;
			TypeSPtr type = var.var.type.Type();
			var.stackIdx = stackFrame.stackIdx;
			stackFrame.stackIdx += type->Size();
			var.size = type->Size();
			stackRet = var.stackIdx;
		}

		ILFuncDefSPtr func = GetFuncDef(call.func);
		if (func)
			Interp(*func, args, stackRet);

		
	}

	u8* ILInterp::GetDstAddr(ILInterpStackFrame& stackFrame, ILInterpVar& dst)
	{
		if (dst.stackIdx != u64(-1))
			return m_Stack.data() + dst.stackIdx;
		return dst.tmpData.data();
	}

	u8* ILInterp::GetSrcAddr(ILInterpStackFrame& stackFrame, ILVar& var)
	{
		if (var.kind == ILVarKind::Lit)
		{
			if (var.litType == ILLitType::Bool)
				return reinterpret_cast<u8*>(&var.boolBit);
			
			return var.litData.data();
		}

		ILInterpVar& src = stackFrame.GetVar(m_pCtx, var);

		if (!src.tmpData.empty())
			return src.tmpData.data();

		return m_Stack.data() + src.stackIdx;
	}

	void ILInterp::GetVarType(ILVar& var, TypeHandle& type, u64& size)
	{
		if (var.kind == ILVarKind::Lit)
		{
			size = u64(var.litData.size());
			switch (var.litType)
			{
			case ILLitType::Bool: type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Bool); break;
			case ILLitType::I8:   type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I8); break;
			case ILLitType::I16:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I16); break;
			case ILLitType::I32:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I32); break;
			case ILLitType::I64:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I64); break;
			case ILLitType::I128: type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::I128); break;
			case ILLitType::U8:   type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U8); break;
			case ILLitType::U16:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U16); break;
			case ILLitType::U32:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U32); break;
			case ILLitType::U64:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U64); break;
			case ILLitType::U128: type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::U128); break;
			case ILLitType::F32:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F32); break;
			case ILLitType::F64:  type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::F64); break;
			case ILLitType::Char: type = m_pCtx->typeReg.Builtin(TypeMod::None, BuiltinTypeKind::Char); break;
			default: type = TypeHandle{};
			}
		}
		else
		{
			size = var.type.Type()->Size();
			type = var.type;
		}
	}

	void ILInterp::StackCopy(u64 from, u64 to, u64 size)
	{
		StackResizeMin(to + size);
		u8* fromIt = m_Stack.data() + from;
		u8* toIt = m_Stack.data() + to;
		memcpy(toIt, fromIt, size);
	}

	void ILInterp::StackCopy(const StdVector<u8>& data, u64 to)
	{
		usize size = data.size();
		StackResizeMin(to + size);
		u8* toIt = m_Stack.data() + to;
		memcpy(toIt, data.data(), size);
	}

	void ILInterp::StackResizeMin(u64 size)
	{
		// make sure to add 8 bytes for math interpreting
		size += 8;
		if (m_Stack.size() < size)
			m_Stack.resize(size);
	}

	void ILInterp::PadStack(u16 expectedPadding)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();
		u64 padMask = expectedPadding - 1;
		u16 offset = u16(stackFrame.stackIdx & padMask);
		u16 padSize = offset ? expectedPadding - offset : 0;
		ReserveStackSpace(padSize);
		stackFrame.stackIdx += padSize;
	}

	void ILInterp::ReserveStackSpace(u64 size)
	{
		ILInterpStackFrame& stackFrame = m_Frames.top();
		StackResizeMin(stackFrame.stackIdx + size);
	}
}
