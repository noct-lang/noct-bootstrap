#include "token.hpp"

namespace Noctis
{

#define TOKTYPENAMECASE(type) case TokenType::type: return StdStringView{ #type }
	
	StdStringView GetTokenTypeName(TokenType type)
	{
		switch (type)
		{
		case TokenType::Unknown: return StdStringView{ "__unknown__" };
			
		TOKTYPENAMECASE(Break);
		TOKTYPENAMECASE(Cast);
		TOKTYPENAMECASE(CConst);
		TOKTYPENAMECASE(Const);
		TOKTYPENAMECASE(Continue);
		TOKTYPENAMECASE(Defer);
		TOKTYPENAMECASE(Do);
		TOKTYPENAMECASE(Else);
		TOKTYPENAMECASE(Enum);
		TOKTYPENAMECASE(Fallthrough);
		TOKTYPENAMECASE(For);
		TOKTYPENAMECASE(Func);
		TOKTYPENAMECASE(Goto);
		TOKTYPENAMECASE(If);
		TOKTYPENAMECASE(Immutable);
		TOKTYPENAMECASE(Import);
		TOKTYPENAMECASE(Impl);
		TOKTYPENAMECASE(In);
		TOKTYPENAMECASE(Interface);
		TOKTYPENAMECASE(Lazy);
		TOKTYPENAMECASE(Loop);
		TOKTYPENAMECASE(Macro);
		TOKTYPENAMECASE(Module);
		TOKTYPENAMECASE(Move);
		TOKTYPENAMECASE(Public);
		TOKTYPENAMECASE(Return);
		TOKTYPENAMECASE(StackDefer);
		TOKTYPENAMECASE(Static);
		TOKTYPENAMECASE(Struct);
		TOKTYPENAMECASE(Switch);
		TOKTYPENAMECASE(Transmute);
		TOKTYPENAMECASE(Typealias);
		TOKTYPENAMECASE(Typedef);
		TOKTYPENAMECASE(Union);
		TOKTYPENAMECASE(Unsafe);
		TOKTYPENAMECASE(While);
			
		TOKTYPENAMECASE(Bool);
		TOKTYPENAMECASE(Char);
		TOKTYPENAMECASE(F16);
		TOKTYPENAMECASE(F32);
		TOKTYPENAMECASE(F64);
		TOKTYPENAMECASE(F128);
		TOKTYPENAMECASE(ISize);
		TOKTYPENAMECASE(I8);
		TOKTYPENAMECASE(I16);
		TOKTYPENAMECASE(I32);
		TOKTYPENAMECASE(I64);
		TOKTYPENAMECASE(I128);
		TOKTYPENAMECASE(USize);
		TOKTYPENAMECASE(U8);
		TOKTYPENAMECASE(U16);
		TOKTYPENAMECASE(U32);
		TOKTYPENAMECASE(U64);
		TOKTYPENAMECASE(U128);
		TOKTYPENAMECASE(Void);
			
		TOKTYPENAMECASE(False);
		TOKTYPENAMECASE(Null);
		TOKTYPENAMECASE(True);
			
		TOKTYPENAMECASE(As);
		TOKTYPENAMECASE(Dynlib);
		TOKTYPENAMECASE(Package);
		TOKTYPENAMECASE(IdenSelf);
		TOKTYPENAMECASE(Self);
		TOKTYPENAMECASE(Weak);
			
		TOKTYPENAMECASE(Eq);
		TOKTYPENAMECASE(EqEq);
		TOKTYPENAMECASE(DblArrow);
		TOKTYPENAMECASE(Plus);
		TOKTYPENAMECASE(PlusPlus);
		TOKTYPENAMECASE(PlusEq);
		TOKTYPENAMECASE(Minus);
		TOKTYPENAMECASE(MinusMinus);
		TOKTYPENAMECASE(MinusEq);
		TOKTYPENAMECASE(Arrow);
		TOKTYPENAMECASE(Asterisk);
		TOKTYPENAMECASE(AsteriskEq);
		TOKTYPENAMECASE(Slash);
		TOKTYPENAMECASE(SlashEq);
		TOKTYPENAMECASE(Percent);
		TOKTYPENAMECASE(PercentEq);
		TOKTYPENAMECASE(Tilde);
		TOKTYPENAMECASE(TildeEq);
		TOKTYPENAMECASE(And);
		TOKTYPENAMECASE(AndAnd);
		TOKTYPENAMECASE(AndEq);
		TOKTYPENAMECASE(Or);
		TOKTYPENAMECASE(OrOr);
		TOKTYPENAMECASE(OrEq);
		TOKTYPENAMECASE(Caret);
		TOKTYPENAMECASE(CaretEq);
		TOKTYPENAMECASE(Less);
		TOKTYPENAMECASE(LessLess);
		TOKTYPENAMECASE(LessLessLess);
		TOKTYPENAMECASE(LessLessAsterisk);
		TOKTYPENAMECASE(LessEq);
		TOKTYPENAMECASE(LessLessEq);
		TOKTYPENAMECASE(LessLessLessEq);
		TOKTYPENAMECASE(LessLessAsteriskEq);
		TOKTYPENAMECASE(Greater);
		TOKTYPENAMECASE(GreaterGreater);
		TOKTYPENAMECASE(GreaterGreaterGreater);
		TOKTYPENAMECASE(GreaterGreaterAsterisk);
		TOKTYPENAMECASE(GreaterEq);
		TOKTYPENAMECASE(GreaterGreaterEq);
		TOKTYPENAMECASE(GreaterGreaterGreaterEq);
		TOKTYPENAMECASE(GreaterGreaterAsteriskEq);
		TOKTYPENAMECASE(Exclaim);
		TOKTYPENAMECASE(ExclaimEq);
		TOKTYPENAMECASE(ExclaimLess);
		TOKTYPENAMECASE(ExclaimParen);
		TOKTYPENAMECASE(ExclaimBrace);
		TOKTYPENAMECASE(ExclaimBracket);
		TOKTYPENAMECASE(LParen);
		TOKTYPENAMECASE(RParen);
		TOKTYPENAMECASE(LBrace);
		TOKTYPENAMECASE(RBrace);
		TOKTYPENAMECASE(LBracket);
		TOKTYPENAMECASE(RBracket);
		TOKTYPENAMECASE(Comma);
		TOKTYPENAMECASE(Semicolon);
		TOKTYPENAMECASE(Colon);
		TOKTYPENAMECASE(ColonColon);
		TOKTYPENAMECASE(ColonEq);
		TOKTYPENAMECASE(Dot);
		TOKTYPENAMECASE(DotDot);
		TOKTYPENAMECASE(DotDotDot);
		TOKTYPENAMECASE(DotDotEq);
		TOKTYPENAMECASE(At);
		TOKTYPENAMECASE(AtColon);
		TOKTYPENAMECASE(Question);
		TOKTYPENAMECASE(QuestionQuestion);
		TOKTYPENAMECASE(QuestionColon);
		TOKTYPENAMECASE(QuestionDot);
		TOKTYPENAMECASE(QuestionBracket);
		TOKTYPENAMECASE(Hash);
		TOKTYPENAMECASE(Dollar);
			
		TOKTYPENAMECASE(CharLit);
		TOKTYPENAMECASE(F16Lit);
		TOKTYPENAMECASE(F32Lit);
		TOKTYPENAMECASE(F64Lit);
		TOKTYPENAMECASE(F128Lit);
		TOKTYPENAMECASE(I8Lit);
		TOKTYPENAMECASE(I16Lit);
		TOKTYPENAMECASE(I32Lit);
		TOKTYPENAMECASE(I64Lit);
		TOKTYPENAMECASE(I128Lit);
		TOKTYPENAMECASE(StringLit);
		TOKTYPENAMECASE(U8Lit);
		TOKTYPENAMECASE(U16Lit);
		TOKTYPENAMECASE(U32Lit);
		TOKTYPENAMECASE(U64Lit);
		TOKTYPENAMECASE(U128Lit);
			
		TOKTYPENAMECASE(Iden);
		TOKTYPENAMECASE(EoL);
	default: return StdStringView{ "__unknown__" };
		}
	}

#undef TOKTYPENAMECASE

	bool IsTokenTypeSignedLiteral(TokenType type)
	{
		return u8(type) >= u8(TokenType::I8Lit) && u8(type) <= u8(TokenType::I128Lit);
	}

	bool IsTokenTypeUnsignedLiteral(TokenType type)
	{
		return u8(type) >= u8(TokenType::U8Lit) && u8(type) <= u8(TokenType::U128Lit);
	}

	bool IsTokenTypeFpLiteral(TokenType type)
	{
		return u8(type) >= u8(TokenType::F16Lit) && u8(type) <= u8(TokenType::F128Lit);
	}

	Token::Token(TokenType type, StdString text, u64 tokenIdx)
		: m_Type(type)
		, m_Text(std::move(text))
		, m_TokenIdx(tokenIdx)
		, m_Signed(0)
		, m_Bool(type == TokenType::True)
	{
	}

	Token::Token(TokenType type, StdString text, i64 val, u64 tokenIdx)
		: m_Type(type)
		, m_Text(std::move(text))
		, m_TokenIdx(tokenIdx)
		, m_Signed(val)
	{
	}

	Token::Token(TokenType type, StdString text, u64 val, u64 tokenIdx)
		: m_Type(type)
		, m_Text(std::move(text))
		, m_TokenIdx(tokenIdx)
		, m_Unsigned(val)
	{
	}

	Token::Token(TokenType type, StdString text, f64 val, u64 tokenIdx)
		: m_Type(type)
		, m_Text(std::move(text))
		, m_TokenIdx(tokenIdx)
		, m_Fp(val)
	{
	}
}
