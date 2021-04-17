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
		
		const TokenTree& TokTree() const { return m_TokTree; }
		
		// Debug utils
		void LogTokens();

	private:

		TokenTree LexTree();
		Token LexToken();

		StdUnorderedMap<StdStringView, TokenType>& GetKeywordMap();

		Token ParseLiteral();
		TokenType ParseLiteralType();

		Token ParseChar();
		Token ParseString();
		Token ParseWysiwygString();

		u32 ParseEscapeCode(usize offset, usize& escapeSize);

		void ParseSingleLineComment();
		void ParseBlockComment();


		TokenTree m_TokTree;

		u64 m_Index;
		u64 m_Line;
		u64 m_Column;

		StdString m_FilePath;
		StdStringView m_Content;
	};
	
}
