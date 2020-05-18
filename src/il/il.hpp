#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"
#include "module/operator.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(ILIfElse);

	enum class ILKind : u8
	{
		Block = 0x01,
		If = 0x02,
		Else = 0x03,
		Loop = 0x04,
		Switch = 0x05,
		Label = 0x08,
		Goto = 0x09,
		ReturnNoVal = 0x0A,
		ReturnVal = 0x0B,
		
		Assign = 0x40,
		PrimAssign = 0x41,
		PrimBinary = 0x42,
		PrimUnary = 0x43,
		PrimCast = 0x44,
		Ternary = 0x45,
		Transmute = 0x46,
		CompIntrin = 0x47,

		FuncCallNoRet = 0x50,
		FuncCallRet = 0x51,
		MethodCallNoRet = 0x52,
		MethodCallRet = 0x53,
		MemberAccess = 0x54,
		TupleAccess = 0x55,

		AggrInit = 0x60,
		ValEnumInit = 0x61,
		AdtEnumInit = 0x62,
		TupInit = 0x63,
		ArrInit = 0x64,


		FuncDef = 0xF0,
	};

	enum class ILVarKind : u8
	{
		Copy = 0b00,
		Ref = 0b01,
		Move = 0b10,
		Lit = 0b11,
	};

	enum class ILLitType : u8
	{
		Bool = 0,
		I8 = 1,
		I16 = 2,
		I32 = 3,
		I64 = 4,
		I128 = 5,
		U8 = 6,
		U16 = 7,
		U32 = 8,
		U64 = 9,
		U128 = 10,
		// F16 = 11,
		F32 = 12,
		F64 = 13,
		// F128 = 14,

		Char = 15,
		String = 16,
		Null = 17,
	};

	struct ILVar
	{
		ILVar();
		ILVar(ILVarKind kind, u32 id, TypeHandle type);
		ILVar(ILLitType lit, const StdVector<u8>& data);
		ILVar(bool bval);

		ILVarKind kind;

		union
		{
			u32 id;

			struct
			{
				ILLitType litType;
				bool boolBit;
			};
		};
		
		
		TypeHandle type;
		
		StdVector<u8> litData;
	};
	
	struct ILElem
	{
		ILElem(ILKind kind);
		
		ILKind kind;
	};
	using ILElemSPtr = StdSharedPtr<ILElem>;

	struct ILBlock : public ILElem
	{
		ILBlock(u32 label);

		u32 label;
		StdVector<ILElemSPtr> elems;
	};

	struct ILIfElse : public ILElem
	{
		ILIfElse();
		
		StdVector<ILElemSPtr> elems;
	};

	struct ILIf : public ILElem
	{
		ILIf(ILVar cond);

		ILVar cond;
		StdVector<ILElemSPtr> elems;
		ILIfElse elseElem;
	};

	struct ILLoop : public ILElem
	{
		ILLoop(u32 beginLabel,u32 endLabel);

		u32 beginLabel;
		u32 endLabel;
		StdVector<ILElemSPtr> elems;
	};

	struct ILSwitch : public ILElem
	{
		// TODO
		ILSwitch();
	};

	struct ILLabel : public ILElem
	{
		ILLabel(u32 label);
		
		u32 label;
	};

	struct ILGoto : public ILElem
	{
		ILGoto(u32 label);

		u32 label;
	};

	struct ILReturn : public ILElem
	{
		ILReturn();
		ILReturn(ILVar var);

		ILVar var;
	};

	struct ILAssign : public ILElem
	{
		ILAssign(ILVar dst, ILVar src);

		ILVar dst;
		ILVar src;
	};

	struct ILPrimAssign : public ILElem
	{
		ILPrimAssign(OperatorKind op, ILVar dst, ILVar src);

		OperatorKind op;
		ILVar dst;
		ILVar src;
	};

	struct ILPrimBinary : public ILElem
	{
		ILPrimBinary(OperatorKind op, ILVar dst, ILVar src0, ILVar src1);

		OperatorKind op;
		ILVar dst;
		ILVar src0;
		ILVar src1;
	};

	struct ILPrimUnary : public ILElem
	{
		ILPrimUnary(OperatorKind op, ILVar dst, ILVar src);

		OperatorKind op;
		ILVar dst;
		ILVar src;
	};

	struct ILPrimCast : public ILElem
	{
		ILPrimCast(ILVar dst, ILVar src);

		ILVar dst;
		ILVar src;
	};

	struct ILTernary : public ILElem
	{
		ILTernary(ILVar dst, ILVar cond, ILVar src0, ILVar src1);

		ILVar dst;
		ILVar cond;
		ILVar src0;
		ILVar src1;
	};

	struct ILTransmute : public ILElem
	{
		ILTransmute(ILVar dst, ILVar src);

		ILVar dst;
		ILVar src;
	};

	struct ILFuncCall : public ILElem
	{
		ILFuncCall(const StdString& func, const StdVector<ILVar>& params);
		ILFuncCall(ILVar dst, const StdString& func, const StdVector<ILVar>& params);

		ILVar dst;
		StdString func;
		StdVector<ILVar> args;
	};

	struct ILMethodCall : public ILElem
	{
		ILMethodCall(ILVar caller, const StdString& func, const StdVector<ILVar>& args);
		ILMethodCall(ILVar dst, ILVar caller, const StdString& func, const StdVector<ILVar>& args);

		ILVar dst;
		ILVar caller;
		StdString func;
		StdVector<ILVar> args;
	};

	struct ILMemberAccess : public ILElem
	{
		ILMemberAccess(ILVar dst, ILVar src, const StdString& name);

		ILVar dst;
		ILVar src;
		StdString name;
	};

	struct ILTupleAccess : public ILElem
	{
		ILTupleAccess(ILVar dst, ILVar src, u16 index);

		ILVar dst;
		ILVar src;
		u16 index;
	};

	struct ILAggrInit : public ILElem
	{
		ILAggrInit(ILVar dst, const StdVector<ILVar>& args);

		ILVar dst;
		StdVector<ILVar> args;
	};

	struct ILValEnumInit : public ILElem
	{
		ILValEnumInit(ILVar dst, const StdString& member);

		ILVar dst;
		StdString member;
	};

	struct ILAdtEnumInit : public ILElem
	{
		ILAdtEnumInit(ILVar dst, const StdString& member, const StdVector<ILVar>& args);

		ILVar dst;
		StdString member;
		StdVector<ILVar> args;
	};

	struct ILTupInit : public ILElem
	{
		ILTupInit(ILVar dst, const StdVector<ILVar>& args);

		ILVar dst;
		StdVector<ILVar> args;
	};

	struct ILArrInit : public ILElem
	{
		ILArrInit(ILVar dst, const StdVector<ILVar>& args);

		ILVar dst;
		StdVector<ILVar> args;
	};


	struct ILFuncDef : public ILElem
	{
		ILFuncDef(const StdString& mangleName);
		
		StdString mangleName;
		StdVector<ILVar> params;
		StdVector<ILVar> localVars;
		StdVector<ILVar> tmpVars;

		StdVector<ILElemSPtr> elems;

		TypeHandle retType;
	};
	using ILFuncDefSPtr = StdSharedPtr<ILFuncDef>;

	struct ILModule
	{
		StdVector<ILFuncDefSPtr> funcs;
		StdUnorderedSet<TypeSPtr> types;
		StdUnorderedSet<StdString> names;
	};
	
}
