#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"

namespace Noctis
{
	enum class TokenType : u8;
	struct Context;
	FWDECL_STRUCT_SPTR(Symbol);
	FWDECL_STRUCT_SPTR(SymbolInst);
	FWDECL_STRUCT_SPTR(ITrGenDecl);

	class ModuleSymbolTable;

	enum class OperatorKind : u8
	{
		// Unary prefix
		Pos,
		Neg,
		PreInc,
		PreDec,
		Not,
		BinNeg,
		Deref,
		MutDeref,
		RefOrAddrOf,

		// Unary postfix
		PostInc,
		PostDec,
		NullPanic,

		// Binary
		Add,
		Sub,
		Mul,
		Div,
		Rem,
		Concat,
		Or,
		And,
		LShl,
		AShl,
		Rotl,
		LShr,
		AShr,
		Rotr,
		BinOr,
		BinXor,
		BinAnd,
		
		Range,
		IncRange,
		NullCoalesce,
		Elvis,
		In,
		NotIn,

		// Comparison
		Eq,
		Ne,
		Lt,
		Le,
		Gt,
		Ge,

		// Assign
		Assign,
		AddAssign,
		SubAssign,
		MulAssign,
		DivAssign,
		RemAssign,
		ConcatAssign,
		LShlAssign,
		AShlAssign,
		RotlAssign,
		LShrAssign,
		AShrAssign,
		RotrAssign,
		BinOrAssign,
		BinXorAssign,
		BinAndAssign,
		NullCoalesceAssign,

		// Cast
		Cast,
		TryCast,

		// Index
		Index,
		MutIndex,

		// Special
		Count,
		Invalid = u8(-1)
	};

	bool IsPrefix(OperatorKind op);
	bool IsPostfix(OperatorKind op);
	bool IsBinary(OperatorKind op);
	bool IsAssign(OperatorKind op);

	OperatorKind TokenTypeToOperator(TokenType type, bool unary = false, bool postfix = false);
	StdStringView GetOpName(OperatorKind op);
	

	struct Operator
	{
		TypeHandle left;
		TypeHandle right;
		TypeHandle result;

		SymbolSPtr sym;
		bool isBuiltin = false;
		bool isInterfaceOp = false;
	};
	
	class OperatorTable
	{
	public:
		OperatorTable(Context* pCtx);

		void Collect(ModuleSymbolTable& table);

		Operator GetOperator(OperatorKind kind, TypeHandle expr, BoundsInfo& boundsInfo);
		Operator GetOperator(OperatorKind kind, TypeHandle left, TypeHandle right, BoundsInfo& boundsInfo);

		Operator GetConstriantOperator(OperatorKind kind, TypeHandle expr, ITrGenDeclSPtr genDecl, BoundsInfo& boundsInfo);
		Operator GetConstriantOperator(OperatorKind kind, TypeHandle left, TypeHandle right, ITrGenDeclSPtr genDecl, BoundsInfo& boundsInfo);
	private:
		
		void HandleBinaryOp(OperatorKind kind, SymbolSPtr impl, SymbolInstSPtr ifaceInst);
		void HandleUnaryOp(OperatorKind kind, SymbolSPtr impl, SymbolInstSPtr ifaceInst);
		void HandleConvOp(SymbolSPtr impl, SymbolInstSPtr ifaceInst);
		
		QualNameSPtr GetOpInterfaceQualName(OperatorKind kind);
		const StdString& GetOpFuncIden(OperatorKind kind);

		bool IsBuiltinOp(TypeHandle handle, BoundsInfo& boundsInfo);
		bool IsBuiltinOp(TypeHandle left, TypeHandle right, BoundsInfo& boundsInfo);

		StdArray<StdUnorderedMap<TypeSPtr, StdVector<Operator>>, u8(OperatorKind::Count)> m_OpSymbols;
		Context* m_pCtx;
	};
	
}
