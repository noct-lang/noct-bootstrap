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
		Lexer();
		
		void Reset();
		void Lex(const StdString& filePath, const StdStringView& content);
		
		const StdVector<Token>& Tokens() const { return m_Tokens; };

		// Debug utils
		void LogTokens();

	private:

		StdUnorderedMap<StdStringView, TokenType>& GetKeywordMap();

		void ParseLiteral(SpanManager& spanManager, u64 tokIdx);
		TokenType ParseLiteralType();

		void ParseChar(SpanManager& spanManager, u64 tokIdx);
		void ParseString(SpanManager& spanManager, u64 tokIdx);
		void ParseWysiwygString(SpanManager& spanManager, u64 tokIdx);

		u32 ParseEscapeCode(usize offset, usize& escapeSize);

		void ParseSingleLineComment();
		void ParseBlockComment();
		

		StdVector<Token> m_Tokens;
		u64 m_Index;
		u64 m_Line;
		u64 m_Column;

		StdString m_FilePath;
		StdStringView m_Content;
	};
	
}
