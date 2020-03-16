#include "operator.hpp"

namespace Noctis
{
	bool IsPrefix(OperatorKind op)
	{
		return u8(op) <= u8(OperatorKind::BoolConv);
	}

	bool IsPostfix(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::PostInc) &&
			u8(op) <= u8(OperatorKind::NullPanic);
	}

	bool IsBinary(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::Add) &&
			u8(op) <= u8(OperatorKind::NotIn);
	}

	bool IsAssign(OperatorKind op)
	{
		return u8(op) >= u8(OperatorKind::Assign) &&
			u8(op) <= u8(OperatorKind::NullCoalesceAssign);
	}

	StdStringView GetOpName(OperatorKind op)
	{
		switch (op)
		{
		case OperatorKind::Pos: return "+";
		case OperatorKind::Neg: return "-";
		case OperatorKind::PreInc: return "X++";
		case OperatorKind::PreDec: return "X--";
		case OperatorKind::Not: return "!";
		case OperatorKind::BinNeg: return "~";
		case OperatorKind::Deref: return "*";
		case OperatorKind::AddrOf: return "&";
		case OperatorKind::BoolConv: return "!!";
		case OperatorKind::PostInc: return "++X";
		case OperatorKind::PostDec: return "--X";
		case OperatorKind::NullPanic: return "!!";
		case OperatorKind::Add: return "+";
		case OperatorKind::Sub: return "-";
		case OperatorKind::Mul: return "*";
		case OperatorKind::Div: return "/";
		case OperatorKind::Rem: return "%";
		case OperatorKind::Concat: return "~";
		case OperatorKind::Or: return "||";
		case OperatorKind::And: return "&&";
		case OperatorKind::LShl: return "<<";
		case OperatorKind::AShl: return "<<<";
		case OperatorKind::Rotl: return "<<*";
		case OperatorKind::LShr: return ">>";
		case OperatorKind::AShr: return ">>>";
		case OperatorKind::Rotr: return ">>*";
		case OperatorKind::BinOr: return "|";
		case OperatorKind::BinXor: return "^";
		case OperatorKind::BinAnd: return "&";
		case OperatorKind::Eq: return "==";
		case OperatorKind::Ne: return "!=";
		case OperatorKind::Lt: return "<";
		case OperatorKind::Le: return "<=";
		case OperatorKind::Gt: return ">";
		case OperatorKind::Ge: return ">=";
		case OperatorKind::Range: return "..";
		case OperatorKind::IncRange: return "..=";
		case OperatorKind::NullCoalesce: return "??";
		case OperatorKind::Elvis: return "?:";
		case OperatorKind::In: return "in";
		case OperatorKind::NotIn: return "!in";
		case OperatorKind::Assign: return "=";
		case OperatorKind::AddAssign: return "+=";
		case OperatorKind::SubAssign: return "-=";
		case OperatorKind::MulAssign: return "*=";
		case OperatorKind::DivAssign: return "/=";
		case OperatorKind::RemAssign: return "%=";
		case OperatorKind::ConcatAssign: return "~=";
		case OperatorKind::LShlAssign: return "<<=";
		case OperatorKind::AShlAssign: return "<<<=";
		case OperatorKind::RotlAssign: return "<<*=";
		case OperatorKind::LShrAssign: return ">>=";
		case OperatorKind::AShrAssign: return ">>>=";
		case OperatorKind::RotrAssign: return ">>*=";
		case OperatorKind::BinOrAssign: return "|=";
		case OperatorKind::BinXorAssign: return "^=";
		case OperatorKind::BinAndAssign: return "&=";
		case OperatorKind::NullCoalesceAssign: return "??=";
		default: return "";
		}
	}
}
