#pragma once
#include "common/defs.hpp"

namespace Noctis
{
	class SpanManager;

	enum class TokenType : u8
	{
		Unknown,
		Skip,
		
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
		TryNullable,
		TryPanic,
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
		ExclaimEq,
		ExclaimLess,
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
		DollarBracket,
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
		EoF,
		
	};

	StdStringView GetTokenTypeName(TokenType type);

	bool IsTokenTypeSignedLiteral(TokenType type);
	bool IsTokenTypeUnsignedLiteral(TokenType type);
	bool IsTokenTypeFpLiteral(TokenType type);

	bool IsTreeStartTokenType(TokenType type);
	
	struct Token
	{
		Token(TokenType type, u64 spanIdx);
		Token(TokenType type, StdString text, u64 spanIdx);
		Token(TokenType type, i64 val, u64 spanIdx);
		Token(TokenType type, u64 val, u64 spanIdx);
		Token(TokenType type, f64 val, u64 spanIdx);

		bool Token::operator==(const Token& other) const;
		bool Token::operator!=(const Token& other) const;

		TokenType type;
		u64 spanId;
		
		union 
		{
			i64 sval;
			u64 uval;
			f64 fval;
			bool bval;
		};
		StdString iden;
	};

	class TokenTree
	{
	public:
		TokenTree();
		TokenTree(const Token& tok);
		TokenTree(StdVector<TokenTree>&& subToks);
		template<typename It>
		TokenTree(It&& begin, It&& end)
			: tok(TokenType::Unknown, u64(-1))
			, m_LocalIdx(0)
		{
			subToks.insert(subToks.end(), begin, end);
		}

		Token tok;
		StdVector<TokenTree> subToks;

		void Append(const Token& tok);
		void Append(const TokenTree& subTree);
		void InsertAtCur(const TokenTree& subTree);

		Token& Peek(usize offset = 0);
		Token& Eat(TokenType type = TokenType::Skip);
		bool TryEat(TokenType type);
		Token& EatIden(const StdString& iden);
		bool TryEatIden(const StdString& iden);

		TokenTree& GetSubTree(bool isBase = true);

		void ResetIdx();

		bool IsExhausted() const { return m_LocalIdx >= subToks.size(); }

		void ToToks(StdVector<Token>& toks);

		// Debug utils
		void LogTokens(usize indent = 0) const;

	private:
		Token& PeekInternal(usize& offset);

		usize m_LocalIdx;
	};
	
}
