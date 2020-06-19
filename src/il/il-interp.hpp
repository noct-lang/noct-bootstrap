#pragma once

#include "il.hpp"

namespace Noctis
{
	struct ILInterpVar
	{
		ILInterpVar();
		ILInterpVar(ILVar var);
		
		ILVar var;
		u64 stackIdx;
		u64 size;


		StdVector<u8> tmpData;
	};

	struct ILInterpStackFrame
	{
		ILInterpStackFrame(u64 retIdx, u64 initialStackPos);
		
		StdVector<ILInterpVar> vars;
		u64 stackIdx;
		u64 retIdx;

		ILInterpVar& GetVar(Context* pCtx, ILVar& var);
	};

	class ILInterp
	{
	public:

		ILInterp(Context* pCtx);


		void Interp(const StdString& funcName);
		void Interp(const StdString& funcName, const StdVector<ILInterpVar>& args);

		
	private:

		ILFuncDefSPtr GetFuncDef(const StdString& funcName);

		void SetupStackFrame(ILFuncDef& func, const StdVector<ILInterpVar>& args, u64 retIdx);


		void Interp(ILFuncDef& funcDef, const StdVector<ILInterpVar>& args, u64 retIdx = u64(-1));

		u32 Interp(ILBlock& block);
		
		u32 Interp(ILElem& elem);
		u32 Interp(ILIf& ifElem);

		
		void Interp(ILReturn& ret);


		void Interp(ILAssign& assign);
		
		void Interp(ILPrimAssign& assign);
		void Interp(ILPrimBinary& binary);
		void Interp(ILPrimUnary& unary);
		void Interp(ILPrimCast& assign);

		void Interp(ILTernary& ternary);

		void Interp(ILMemberAccess& access);
		void Interp(ILTupleAccess& access);

		void Interp(ILStructInit& init);
		void Interp(ILTupInit& init);
		void Interp(ILArrInit& init);
		
		void Interp(ILFuncCall& call);
		

		u8* GetDstAddr(ILInterpStackFrame& stackFrame, ILInterpVar& dst);
		u8* GetSrcAddr(ILInterpStackFrame& stackFrame, ILVar& var);
		void GetVarType(ILVar& var, TypeHandle& type, u64& size);

		void StackCopy(u64 from, u64 to, u64 size);
		void StackCopy(const StdVector<u8>& data, u64 to);
		void StackResizeMin(u64 size);
		void PadStack(u16 expectedPadding);
		void ReserveStackSpace(u64 size);

		StdStack<ILInterpStackFrame> m_Frames;
		StdVector<u8> m_Stack;

		

		Context* m_pCtx;
	};
}
