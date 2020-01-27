#pragma once
#include "common/defs.hpp"

namespace Noctis
{

	enum class TokenType
	{
		Unknown,
		
		// Keywords
		Break,
		Cast,
		CConst,
		Const,
		Continue,
		Defer,
		Do,
		Else,
		Enum,
		Fallthrough,
		For,
		Func,
		Goto,
		If,
		Is,
		Immutable,
		Import,
		Impl,
		In,
		NotIn,
		Interface,
		Lazy,
		Loop,
		Macro,
		Module,
		Move,
		Public,
		Return,
		StackDefer,
		Static,
		Struct,
		Switch,
		Transmute,
		Typealias,
		Typedef,
		Union,
		Unsafe,
		While,

		// Type keywords
		Bool,
		Char,
		
		F16,
		F32,
		F64,
		F128,
		ISize,
		I8,
		I16,
		I32,
		I64,
		I128,
		USize,
		U8,
		U16,
		U32,
		U64,
		U128,
		Void,

		// Constant keywords
		False,
		Null,
		True,

		// Context dependent keywords
		As,
		Dynlib,
		Package,
		IdenSelf,
		Self,
		Weak,
		Where,

		// Operators and Punctuation
		Eq,
		EqEq,
		DblArrow,
		Plus,
		PlusPlus,
		PlusEq,
		Minus,
		MinusMinus,
		MinusEq,
		Arrow,
		Asterisk,
		AsteriskEq,
		Slash,
		SlashEq,
		Percent,
		PercentEq,
		Tilde,
		TildeEq,
		And,
		AndAnd,
		AndEq,
		Or,
		OrOr,
		OrEq,
		Caret,
		CaretEq,
		Less,
		LessLess,
		LessLessLess,
		LessLessAsterisk,
		LessEq,
		LessLessEq,
		LessLessLessEq,
		LessLessAsteriskEq,
		Greater,
		GreaterGreater,
		GreaterGreaterGreater,
		GreaterGreaterAsterisk,
		GreaterEq,
		GreaterGreaterEq,
		GreaterGreaterGreaterEq,
		GreaterGreaterAsteriskEq,
		Exclaim,
		ExclaimEq,
		ExclaimLess,
		ExclaimParen,
		ExclaimBrace,
		ExclaimBracket,
		LParen,
		RParen,
		LBrace,
		RBrace,
		LBracket,
		RBracket,
		Comma,
		Semicolon,
		Colon,
		ColonColon,
		ColonEq,
		Dot,
		DotDot,
		DotDotDot,
		DotDotEq,
		At,
		AtColon,
		Question,
		QuestionQuestion,
		QuestionColon,
		QuestionDot,
		QuestionBracket,
		Hash,
		Dollar,
		DollarParen,
		DollarBrace,

		// Literals
		CharLit,
		F16Lit,
		F32Lit,
		F64Lit,
		F128Lit,
		I8Lit,
		I16Lit,
		I32Lit,
		I64Lit,
		I128Lit,
		StringLit,
		U8Lit,
		U16Lit,
		U32Lit,
		U64Lit,
		U128Lit,

		// Other
		Iden,
		EoL
		
	};

	StdStringView GetTokenTypeName(TokenType type);

	bool IsTokenTypeSignedLiteral(TokenType type);
	bool IsTokenTypeUnsignedLiteral(TokenType type);
	bool IsTokenTypeFpLiteral(TokenType type);
	
	class Token
	{
	public:

		Token(TokenType type, u64 tokenIdx);
		Token(TokenType type, StdString text, u64 tokenIdx);
		Token(TokenType type, i64 val, u64 tokenIdx);
		Token(TokenType type, u64 val, u64 tokenIdx);
		Token(TokenType type, f64 val, u64 tokenIdx);

		TokenType Type() const { return m_Type; }
		const StdString& Text() const { return m_Iden; }
		u64 Idx() const { return m_TokenIdx; }
		
		i64 Signed() const { return m_Signed; }
		i64 Unsigned() const { return m_Unsigned; }
		f64 Fp() const { return m_Fp; }
		bool Bool() const { return m_Bool; }
		
	private:

		TokenType m_Type;
		u64 m_TokenIdx;
		
		union 
		{
			i64 m_Signed;
			u64 m_Unsigned;
			f64 m_Fp;
			bool m_Bool;
		};
		StdString m_Iden;
	};
	
}
