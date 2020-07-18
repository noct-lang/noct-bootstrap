#pragma once
#include "common/defs.hpp"

namespace Noctis
{
	class SpanManager;

	enum class TokenType : u8
	{
		Unknown,
		
		// Keywords
		As,
		AsQuestion,
		AsExclaim,
		Break,
		Comptime,
		Const,
		Continue,
		Defer,
		Do,
		Else,
		Enum,
		ErrDefer,
		Fallthrough,
		For,
		Func,
		Goto,
		If,
		Immutable,
		Import,
		Impl,
		In,
		NotIn,
		Is,
		NotIs,
		Interface,
		Lazy,
		Loop,
		Macro,
		Module,
		Move,
		Mut,
		Public,
		Return,
		Static,
		Struct,
		Switch,
		Throw,
		Transmute,
		Try,
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

		// Constant keywords
		False,
		Null,
		True,

		// Reserved keywords
		Async,
		Await,
		Yield,

		// Special keywords
		SBenchmark,
		SConditional,
		SDebug,
		SErrorHandler,
		SFile,
		SFileFullPath,
		SFullModule,
		SFunc,
		SFuncName,
		SIf,
		SLine,
		SModule,
		SPackage,
		SPrettyFunc,
		SRun,
		SUnittest,
		
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
		ExclaimExclaim,
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
		QuestionQuestionEq,
		QuestionColon,
		QuestionDot,
		QuestionBracket,
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
		MacroIden,
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

		bool Token::operator==(const Token& other) const;
		bool Token::operator!=(const Token& other) const;
		
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

	struct TokenTree
	{
		TokenTree();
		TokenTree(Token& tok);
		TokenTree(StdVector<TokenTree>&& subToks);
		template<typename It>
		TokenTree(It&& begin, It&& end)
			: tok(TokenType::Unknown, u64(-1))
		{
			subToks.insert(subToks.end(), begin, end);
		}

		Token tok;
		StdVector<TokenTree> subToks;

		void Append(Token& tok);
		void Append(TokenTree& subTree);

		void ToToks(StdVector<Token>& toks);

		// Debug utils
		void LogTokens(SpanManager& spanManager, usize indent = 0) const;
	};
	
}
