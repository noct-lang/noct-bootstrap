#pragma once
#include "common/defs.hpp"

namespace Noctis
{

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
		AddrOf,
		BoolConv,

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
		Eq,
		Ne,
		Lt,
		Le,
		Gt,
		Ge,
		Range,
		IncRange,
		NullCoalesce,
		Elvis,
		In,
		NotIn,

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

		// Special

		Invalid = u8(-1)
	};

	bool IsPrefix(OperatorKind op);
	bool IsPostfix(OperatorKind op);
	bool IsBinary(OperatorKind op);
	bool IsAssign(OperatorKind op);

	StdStringView GetOpName(OperatorKind op);
	
}
