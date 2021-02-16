#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"
#include "module/graph.hpp"
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
		Index = 0x47,

		GenVal = 0x4D,
		CompIntrin = 0x4E,

		StaticCallNoRet = 0x50,
		StaticCallRet = 0x51,
		DynamicCallNoRet = 0x52,
		DynamicCallRet = 0x53,
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
		Unreachable = 0x75,

		FuncDef = 0xF0,
	};

	enum class ILCompIntrinKind : u8
	{
		SizeOf         = 0x00,
		AlignOf        = 0x01,
		AlignOfVal     = 0x02,
		Log2AlignOf    = 0x03,
		Log2AlignOfVal = 0x04,

		BytewiseCopy   = 0x10,

		FuzzyTypeComp  = 0x80,
	};

	StdString GetCompIntrinName(ILCompIntrinKind intrin);
	bool HasCompIntrinReturn(ILCompIntrinKind intrin);
	usize GetCompIntrinVarCount(ILCompIntrinKind intrin);
	usize GetCompIntrinTypeCount(ILCompIntrinKind intrin);

	enum class ILVarKind : u8
	{
		Copy = 0b00,
		Ref = 0b01,
		Move = 0b10,
		Lit = 0b11,
	};

	enum class ILLitType : u8
	{
		False  = 0,
		True   = 1,
		I8     = 2,
		I16    = 3,
		I32    = 4,
		I64    = 5,
		I128   = 6,
		U8     = 7,
		U16    = 8,
		U32    = 9,
		U64    = 10,
		U128   = 11,
		// F16    = 12,
		F32    = 13,
		F64    = 14,
		// F128   = 15,
		Char   = 16,
		String = 17,
		Null   = 18,
	};

	struct ILVar
	{
		ILVar();
		ILVar(ILVarKind kind, u32 id, TypeHandle type);
		ILVar(ILLitType lit, const StdVector<u8>& data);
		ILVar(ILLitType lit, u64 val);
		ILVar(ILLitType lit, i64 val);
		ILVar(ILLitType lit, f64 val);
		ILVar(ILLitType lit);

		void SetLitData(u64 val);

		ILVarKind kind;

		union
		{
			u32 id;

			struct
			{
				ILLitType litType;
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
		ILSwitch(ILVar cond, const StdPairVector<ILVar, u32>& cases, u32 defCase);

		ILVar cond;
		StdPairVector<ILVar, u32> cases;
		u32 defCase;
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

	struct ILUnreachable : public ILTerminal
	{
		ILUnreachable();
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

	struct ILIndex : public ILElem
	{
		ILIndex(ILVar dst, ILVar src, ILVar idx);

		ILVar dst;
		ILVar src;
		ILVar idx;
	};

	struct ILGenVal : public ILElem
	{
		ILGenVal(ILVar dst, const StdString& genName);

		ILVar dst;
		StdString genName;
	};

	struct ILCompIntrin : public ILElem
	{
		ILCompIntrin(ILVar dst, ILCompIntrinKind kind, const StdVector<ILVar>& vars, const StdVector<TypeHandle>& types);

		ILVar dst;
		ILCompIntrinKind intrin;
		StdVector<ILVar> vars;
		StdVector<TypeHandle> types;
	};

	struct ILStaticCall : public ILElem
	{
		ILStaticCall(QualNameSPtr func, const StdVector<ILVar>& args);
		ILStaticCall(ILVar dst, QualNameSPtr func, const StdVector<ILVar>& args);

		ILVar dst;
		QualNameSPtr func;
		StdVector<ILVar> args;
	};

	struct ILDynamicCall : public ILElem
	{
		ILDynamicCall(ILVar caller, const StdString& func, const StdVector<ILVar>& args);
		ILDynamicCall(ILVar dst, ILVar caller, const StdString& func, const StdVector<ILVar>& args);

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
		ILUnionInit(ILVar dst, const StdString& member, ILVar args);

		ILVar dst;
		ILVar arg;
		StdString member;
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

	struct ILGeneric
	{
		ILGeneric(IdenSPtr iden)
			: iden(iden)
		{
		}
		
		ILGeneric(IdenSPtr iden, TypeHandle type)
			: iden(iden)
			, type(type)
		{
		}
		
		IdenSPtr iden;
		TypeHandle type;
	};

	struct ILFuncDef : public ILElem
	{
		ILFuncDef(Context* pCtx, QualNameSPtr qualName, StdVector<ILGeneric>&& generics);
		
		QualNameSPtr qualName;
		
		StdVector<ILGeneric> generics;
		StdVector<ILVar> params;
		StdVector<ILVar> localVars;
		StdVector<ILVar> tmpVars;

		StdVector<ILBlock> blocks;

		TypeHandle retType;

		SymbolWPtr sym;

		ILDependencyGraph graph;
	};
	using ILFuncDefSPtr = StdSharedPtr<ILFuncDef>;

	struct ILModule
	{
		StdVector<ILFuncDefSPtr> funcs;
		StdUnorderedSet<TypeSPtr> types;
		StdUnorderedSet<StdString> names;
	};
	
}
