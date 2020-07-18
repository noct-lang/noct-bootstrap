#pragma once
#include "common/defs.hpp"
#include "common/type.hpp"

namespace Noctis
{
	enum class TokenType : u8;
	struct Context;
	FWDECL_STRUCT_SPTR(Symbol);

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
		TypeHandle left = TypeHandle(-1);
		TypeHandle right = TypeHandle(-1);
		TypeHandle result = TypeHandle(-1);

		SymbolSPtr sym;
		bool isBuiltin = false;
	};
	
	class OperatorTable
	{
	public:
		OperatorTable(Context* pCtx);

		void Collect(ModuleSymbolTable& table);

		Operator& GetOperator(OperatorKind kind, TypeHandle expr);
		Operator& GetOperator(OperatorKind kind, TypeHandle left, TypeHandle right);
		

	private:

		void HandleBinaryOp(OperatorKind kind, SymbolSPtr impl, SymbolSPtr interfaceSym);
		void HandleUnaryOp(OperatorKind kind, SymbolSPtr impl, SymbolSPtr interfaceSym);
		void HandleFromOp(bool isTry, SymbolSPtr impl, SymbolSPtr interfaceSym);
		void HandleToOp(bool isTry, SymbolSPtr impl, SymbolSPtr interfaceSym);
		
		QualNameSPtr GetOpInterfaceQualName(OperatorKind kind);

		bool IsBuiltinOp(TypeHandle handle);
		bool IsBuiltinOp(TypeHandle left, TypeHandle right);

		StdArray<StdUnorderedMap<TypeSPtr, StdVector<Operator>>, u8(OperatorKind::Count)> m_OpSymbols;
		Context* m_pCtx;
	};
	
}
