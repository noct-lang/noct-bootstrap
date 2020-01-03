#pragma once

#include "common/defs.hpp"
#include "token.hpp"

namespace Noctis
{
	class SpanManager;
	struct Context;

	class Lexer
	{
	public:
		Lexer(Context* pContext);
		
		void Reset();
		void Lex(const StdStringView& content);
		
		const StdVector<Token>& Tokens() const { return m_Tokens; };

		// Debug utils
		void LogTokens();

	private:

		StdUnorderedMap<StdStringView, TokenType>& GetKeywordMap();

		void ParseLiteral(StdStringView content, SpanManager& spanManager, u64 tokIdx);
		TokenType ParseLiteralType(StdStringView content);

		void ParseChar(StdStringView content, SpanManager& spanManager, u64 tokIdx);
		void ParseString(StdStringView content, SpanManager& spanManager, u64 tokIdx);
		void ParseWysiwygString(StdStringView content, SpanManager& spanManager, u64 tokIdx);

		u32 ParseEscapeCode(StdStringView content, usize offset, usize& escapeSize);

		void ParseSingleLineComment(StdStringView content);
		void ParseBlockComment(StdStringView content);
		

		StdVector<Token> m_Tokens;
		u64 m_Index;
		u64 m_Line;
		u64 m_Column;
		Context* m_pCtx;
	};
	
}
