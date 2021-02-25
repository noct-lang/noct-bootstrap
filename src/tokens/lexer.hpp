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

		void ParseLiteral();
		TokenType ParseLiteralType();

		void ParseChar();
		void ParseString();
		void ParseWysiwygString();

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
