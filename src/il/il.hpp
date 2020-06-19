#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"
#include "module/operator.hpp"

namespace Noctis
{
	enum class ILKind : u8
	{
		Block = 0x01,
		
		Assign = 0x40,
		PrimAssign = 0x41,
		PrimBinary = 0x42,
		PrimUnary = 0x43,
		PrimCast = 0x44,
		Ternary = 0x45,
		Transmute = 0x46,
		CompIntrin = 0x47, // TODO

		FuncCallNoRet = 0x50,
		FuncCallRet = 0x51,
		MethodCallNoRet = 0x52,
		MethodCallRet = 0x53,
		IndirectCallNoRet = 0x54, // TODO
		IndirectCallRet = 0x55, // TODO
		MemberAccess = 0x56,
		TupleAccess = 0x57,

		StructInit = 0x60,
		UnionInit = 0x61,
		ValEnumInit = 0x62,
		AdtEnumInit = 0x63,
		TupInit = 0x64,
		ArrInit = 0x65,

		If = 0x70,
		Switch = 0x71,
		Goto = 0x72,
		ReturnNoVal = 0x73,
		ReturnVal = 0x74,


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

	struct ILTerminal : ILElem
	{
		ILTerminal(ILKind kind);
	};
	using ILTerminalSPtr = StdSharedPtr<ILTerminal>;

	struct ILBlock : public ILElem
	{
		ILBlock(u32 label);

		u32 label;
		StdVector<ILElemSPtr> elems;
		ILTerminalSPtr terminal;
	};

	struct ILIf : public ILTerminal
	{
		ILIf(ILVar cond, u32 trueLabel, u32 falseLabel);

		ILVar cond;
		u32 trueLabel;
		u32 falseLabel;
	};

	struct ILSwitch : public ILTerminal
	{
		// TODO
		ILSwitch();
	};

	struct ILGoto : public ILTerminal
	{
		ILGoto(u32 label);

		u32 label;
	};

	struct ILReturn : public ILTerminal
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
		ILFuncCall(const StdString& func, const StdVector<ILVar>& args);
		ILFuncCall(ILVar dst, const StdString& func, const StdVector<ILVar>& args);

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

	struct ILIndirectCall : public ILElem
	{
		ILIndirectCall(ILVar func, const StdVector<ILVar>& args);
		ILIndirectCall(ILVar dst, ILVar func, const StdVector<ILVar>& args);

		ILVar dst;
		ILVar func;
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

	struct ILStructInit : public ILElem
	{
		ILStructInit(ILVar dst, const StdVector<ILVar>& args);

		ILVar dst;
		StdVector<ILVar> args;
	};

	struct ILUnionInit : public ILElem
	{
		ILUnionInit(ILVar dst, ILVar args);

		ILVar dst;
		ILVar arg;
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

		StdVector<ILBlock> blocks;

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
