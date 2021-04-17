#include "lexer.hpp"
#include "token.hpp"
#include "common/context.hpp"
#include <charconv>
#include "common/errorsystem.hpp"
#include <algorithm>
#include "common/logger.hpp"
#include <sstream>

namespace Noctis
{
	Lexer::Lexer()
		: m_Index(0)
		, m_Line(1)
		, m_Column(1)
	{
	}

	void Lexer::Reset()
	{
		m_Index = 0;
		m_Line = 1;
		m_Column = 1;
	}

	void Lexer::Lex(const StdString& filePath, const StdStringView& content)
	{
		m_FilePath = filePath;
		m_Content = content;

		while (m_Index < m_Content.size())
		{
			TokenTree tmp = LexTree();
			if (tmp.tok.type == TokenType::EoF)
				break;
			m_TokTree.Append(tmp);
		}
	}

	void Lexer::LogTokens()
	{
		m_TokTree.LogTokens();
	}

	TokenTree Lexer::LexTree()
	{
		TokenTree tree;

		Token startTok = LexToken();
		while (startTok.type == TokenType::Skip)
		{
			startTok = LexToken();
		}
		
		tree.Append(startTok);

		TokenType endType = TokenType::Unknown;
		switch (startTok.type)
		{
		case TokenType::LParen:
		case TokenType::DollarParen:
			endType = TokenType::RParen;
			break;
		case TokenType::LBrace:
		case TokenType::DollarBrace:
			endType = TokenType::RBrace;
			break;
		case TokenType::LBracket:
		case TokenType::QuestionBracket:
		case TokenType::DollarBracket:
			endType = TokenType::RBracket;
			break;
		case TokenType::ExclaimLess:
			endType = TokenType::Greater;
			break;
		default: break;
		}

		if (endType == TokenType::Unknown || endType == TokenType::EoF)
			return TokenTree{ startTok };
		
		while (m_Index < m_Content.size())
		{
			TokenTree tokTree = LexTree();
			while (tokTree.tok.type == TokenType::Skip)
			{
				tokTree = LexTree();
			}
			
			if (!tokTree.subToks.empty())
			{
				tree.Append(tokTree);
			}
			else
			{
				Token& tok = tokTree.tok;
				if (tok.type == TokenType::Skip)
					continue;

				if (tok.type == TokenType::Unknown)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					g_ErrorSystem.Error(span, "Unknown token");
					continue;
				}

				if (tok.type != TokenType::EoF)
					tree.Append(tok);
				
				if (tok.type == endType || tok.type == TokenType::EoF)
					break;
			}
		}

		if (tree.subToks.back().tok.type != endType)
		{
			Span span = g_SpanManager.GetSpan(tree.subToks.front().tok.spanId);
			StdStringView closingChar = GetTokenTypeName(endType);
			g_ErrorSystem.Error(span, "Did not find closing character '%s'", closingChar.data());
		}

		return tree;
	}

	Token Lexer::LexToken()
	{
		u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, m_Index, m_Line, m_Column });
		++m_Index;
		++m_Column;
		
		switch (m_Content[m_Index - 1])
		{
		case '=':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == '=')
			{
				++m_Index; ++m_Column;
				g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
				return { TokenType::EqEq, spanId };
			}
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::EqEq, spanId };
				}
				if (m_Content[m_Index] == '>')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::DblArrow, spanId };
				}
			}
			return { TokenType::Eq, spanId };
		}
		case '+':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::PlusEq, spanId };
				}
				if (m_Content[m_Index] == '+')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::PlusPlus, spanId };
				}
			}
			return { TokenType::Plus, spanId };
		}
		case '-':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::MinusEq, spanId };
				}
				if (m_Content[m_Index] == '-')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::MinusMinus, spanId };
				}
				if (m_Content[m_Index] == '>')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::Arrow, spanId };
				}
				if (isdigit(m_Content[m_Index]))
				{
					--m_Index;
					return ParseLiteral();
				}
			}
			return { TokenType::Minus, spanId };
		}
		case '*':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == '=')
			{
				++m_Index; ++m_Column;
				g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
				return { TokenType::AsteriskEq, spanId };
			}
			return { TokenType::Asterisk, spanId };
		}
		case '/':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::SlashEq, spanId };
				}
				if (m_Content[m_Index] == '/')
				{
					--m_Index;
					ParseSingleLineComment();
					return LexToken();
				}
				if (m_Content[m_Index] == '*')
				{
					--m_Index;
					ParseBlockComment();
					return LexToken();
				}
			}
			return { TokenType::Slash, spanId };
		}
		case '%':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == '=')
			{
				++m_Index; ++m_Column;
				g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
				return { TokenType::PercentEq, spanId };
			}
			return { TokenType::Percent, spanId };
		}
		case '~':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == '=')
			{
				++m_Index; ++m_Column;
				g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
				return { TokenType::TildeEq, spanId };
			}
			return { TokenType::Tilde, spanId };
		}
		case '&':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::AndEq, spanId };
				}
				if (m_Content[m_Index] == '&')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::AndAnd, spanId };
				}
			}
			return { TokenType::And, spanId };
		}
		case '|':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::OrEq, spanId };
				}
				if (m_Content[m_Index] == '|')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::OrOr, spanId };
				}
			}
			return { TokenType::Or, spanId };
		}
		case '^':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == '=')
			{
				++m_Index; ++m_Column;
				g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
				return { TokenType::CaretEq, spanId };
			}
			return { TokenType::Caret, spanId };
		}
		case '<':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::LessEq, spanId };
				}
				if (m_Content[m_Index] == '<')
				{
					if (m_Index + 1 < m_Content.size())
					{
						if (m_Content[m_Index + 1] == '=')
						{
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::LessLessEq, spanId };
						}
						if (m_Content[m_Index + 1] == '<')
						{
							if (m_Index + 2 < m_Content.size() && m_Content[m_Index + 2] == '=')
							{
								m_Index += 3; ++m_Column;
								g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
								return { TokenType::LessLessLessEq, spanId };
							}
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::LessLessLess, spanId };
						}
						if (m_Content[m_Index + 1] == '*')
						{
							if (m_Index + 2 < m_Content.size() && m_Content[m_Index + 2] == '=')
							{
								m_Index += 3; ++m_Column;
								g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
								return { TokenType::LessLessAsteriskEq, spanId };
							}
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::LessLessAsterisk, spanId };
						}
					}
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::LessLess, spanId };
				}
			}
			return { TokenType::Less, spanId };
		}
		case '>':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::GreaterEq, spanId };
				}
				if (m_Content[m_Index] == '>')
				{
					if (m_Index + 1 < m_Content.size())
					{
						if (m_Content[m_Index + 1] == '=')
						{
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::GreaterGreaterEq, spanId };
						}
						if (m_Content[m_Index + 1] == '>')
						{
							if (m_Index + 2 < m_Content.size() && m_Content[m_Index + 2] == '=')
							{
								m_Index += 3; ++m_Column;
								g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
								return { TokenType::GreaterGreaterGreaterEq, spanId };
							}
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::GreaterGreaterGreater, spanId };
						}
						if (m_Content[m_Index + 1] == '*')
						{
							if (m_Index + 2 < m_Content.size() && m_Content[m_Index + 2] == '=')
							{
								m_Index += 3; ++m_Column;
								g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
								return { TokenType::GreaterGreaterAsteriskEq, spanId };
							}
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::GreaterGreaterAsterisk, spanId };
						}
					}
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::GreaterGreater, spanId };
				}
			}
			return { TokenType::Greater, spanId };
		}
		case '!':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::ExclaimEq, spanId };
				}
				if (m_Content[m_Index] == '<')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::ExclaimLess, spanId };
				}
			}
			return { TokenType::Exclaim, spanId };
		}
		case '(': return { TokenType::LParen, spanId };
		case ')': return { TokenType::RParen, spanId };
		case '{': return { TokenType::LBrace, spanId };
		case '}': return { TokenType::RBrace, spanId };
		case '[': return { TokenType::LBracket, spanId };
		case ']': return { TokenType::RBracket, spanId };
		case ',': return { TokenType::Comma, spanId };
		case ';': return { TokenType::Semicolon, spanId };
		case ':':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '=')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::ColonEq, spanId };
				}
				if (m_Content[m_Index] == ':')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::ColonColon, spanId };
				}
			}
			return { TokenType::Colon, spanId };
		}
		case '.':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '.')
				{
					if (m_Index + 1 < m_Content.size())
					{
						if (m_Content[m_Index + 1] == '=')
						{
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::DotDotEq, spanId };
						}
						if (m_Content[m_Index + 1] == '.')
						{
							m_Index += 2; ++m_Column;
							g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
							return { TokenType::DotDotDot, spanId };
						}
					}
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::DotDot, spanId };
				}
				if (isdigit(m_Content[m_Index]))
				{
					--m_Index;
					return ParseLiteral();
				}
			}
			return { TokenType::Dot, spanId };
		}
		case '@':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == ':')
			{
				++m_Index; ++m_Column;
				g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
				return { TokenType::AtColon, spanId };
			}
			return { TokenType::At, spanId };
		}
		case '?':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '?')
				{
					if (m_Index + 1 < m_Content.size() && m_Content[m_Index + 1] == '=')
					{
						m_Index += 2; ++m_Column;
						g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
						return { TokenType::QuestionQuestionEq, spanId };
					}
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::QuestionQuestion, spanId };
				}
				if (m_Content[m_Index] == ':')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::QuestionColon, spanId };
				}
				if (m_Content[m_Index] == '.')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::QuestionDot, spanId };
				}
				if (m_Content[m_Index] == '[')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::QuestionBracket, spanId };
				}
			}
			return { TokenType::Question, spanId };
		}
		case '$':
		{
			if (m_Index < m_Content.size())
			{
				if (m_Content[m_Index] == '(')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::DollarParen, spanId };
				}
				if (m_Content[m_Index] == '[')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::DollarBracket, spanId };
				}
				if (m_Content[m_Index] == '{')
				{
					++m_Index; ++m_Column;
					g_SpanManager.UpdateSpan(spanId, { m_Index - 2, m_Index, m_Line, m_Column - 2 });
					return { TokenType::DollarBrace, spanId };
				}
			}


			usize end = m_Content.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", m_Index);
			StdStringView iden = m_Content.substr(m_Index, end - m_Index);

			usize start = m_Index - 1;
			m_Index += iden.size();
			g_SpanManager.UpdateSpan(spanId, { start, m_Index, m_Line, m_Column - 2 });
			return { TokenType::MacroIden, StdString{ iden }, spanId };
		}
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			return ParseLiteral();
		case ' ':
		{
			return { TokenType::Skip, u64(-1) };
		}
		case '\t':
		{
			m_Index += g_Ctx.options.TabWidth() - 1;
			return { TokenType::Skip, u64(-1) };
		}
		case '\r':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] != '\n')
				return { TokenType::Skip, u64(-1) };
			++m_Index;
			[[fallthrough]];
		}
		case '\n':
		{
			++m_Line;
			m_Column = 1;
			return { TokenType::Skip, u64(-1) };
		}
		case '\'': return ParseChar();
		case '"': return ParseString();
		case '\0':
			--m_Index;
			return { TokenType::EoF, spanId };

		case 'r':
		{
			if (m_Index < m_Content.size() && m_Content[m_Index] == '"')
				return ParseWysiwygString();
			[[fallthrough]];
		}
		default:
		{
			--m_Index;
			usize searchStart = m_Index;
			if (searchStart + 1 < m_Content.size() && m_Content[searchStart] == '#')
				++searchStart;

			usize end = m_Content.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", searchStart);
			StdStringView iden = m_Content.substr(m_Index, end - m_Index);

			StdUnorderedMap<StdStringView, TokenType>& keywords = GetKeywordMap();
			auto it = keywords.find(iden);
			if (it == keywords.end())
			{
				Span span{ m_Index, end, m_Line, m_Column };
				g_SpanManager.UpdateSpan(spanId, span);
				m_Column += end - m_Index - 1;
				m_Index = end;
				return { TokenType::Iden, StdString{ iden }, spanId };
			}

			if (!m_TokTree.subToks.empty())
			{
				Token& prevTok = m_TokTree.subToks.back().tok;
				switch (it->second)
				{
				case TokenType::In:
				{
					if (prevTok.type == TokenType::Exclaim)
					{
						g_SpanManager.UpdateSpan(spanId, { m_Index - 1, end, m_Line, m_Column - 1 });
						prevTok = Token{ TokenType::NotIn, prevTok.spanId };
						m_Column += end - m_Index - 1;
						m_Index = end;
						return { TokenType::Skip, u64(-1) };
					}
					break;
				}
				case TokenType::Is:
				{
					if (prevTok.type == TokenType::Exclaim)
					{
						g_SpanManager.UpdateSpan(spanId, { m_Index - 1, end, m_Line, m_Column - 1 });
						prevTok = Token{ TokenType::NotIs, prevTok.spanId };
						m_Column += end - m_Index - 1;
						m_Index = end;
						return { TokenType::Skip, u64(-1) };
					}
					break;
				}
				case TokenType::Try:
				{
					if (end < m_Content.size())
					{
						if (m_Content[end] == '?')
						{
							Span span{ m_Index, end + 1, m_Line, m_Column };
							g_SpanManager.UpdateSpan(spanId, span);
							m_Column += end - m_Index - 1;
							m_Index = end + 1;
							return { TokenType::TryNullable, spanId };
						}
						if (m_Content[end] == '!')
						{
							Span span{ m_Index, end + 1, m_Line, m_Column };
							g_SpanManager.UpdateSpan(spanId, span);
							m_Column += end - m_Index - 1;
							m_Index = end + 1;
							return { TokenType::TryPanic, spanId };
						}
					}
				}
				default: break;
				}
			}

			Span span{ m_Index, end, m_Line, m_Column };
			g_SpanManager.UpdateSpan(spanId, span);
			m_Column += end - m_Index - 1;
			m_Index = end;
			return { it->second, StdString{ iden }, spanId };
		}
		}

	}

	StdUnorderedMap<StdStringView, TokenType>& Lexer::GetKeywordMap()
	{
		static StdUnorderedMap<StdStringView, TokenType> keywords =
		{
			{ "as", TokenType::As },
			{ "as?", TokenType::AsQuestion },
			{ "as!", TokenType::AsExclaim },
			{ "break", TokenType::Break },
			{ "comptime", TokenType::Comptime },
			{ "const", TokenType::Const },
			{ "continue", TokenType::Continue },
			{ "defer", TokenType::Defer },
			{ "do", TokenType::Do },
			{ "else", TokenType::Else },
			{ "enum", TokenType::Enum },
			{ "errdefer", TokenType::ErrDefer },
			{ "fallthrough", TokenType::Fallthrough },
			{ "for", TokenType::For },
			{ "func", TokenType::Func },
			{ "goto", TokenType::Goto },
			{ "if", TokenType::If },
			{ "immutable", TokenType::Immutable },
			{ "import", TokenType::Import },
			{ "impl", TokenType::Impl },
			{ "in", TokenType::In },
			{ "interface", TokenType::Interface },
			{ "is", TokenType::Is },
			{ "lazy", TokenType::Lazy },
			{ "loop", TokenType::Loop },
			{ "macro", TokenType::Macro },
			{ "module", TokenType::Module },
			{ "move", TokenType::Move },
			{ "mut", TokenType::Mut },
			{ "public", TokenType::Public },
			{ "return", TokenType::Return },
			{ "static", TokenType::Static },
			{ "struct", TokenType::Struct },
			{ "switch", TokenType::Switch },
			{ "throw", TokenType::Throw },
			{ "transmute", TokenType::Transmute },
			{ "try", TokenType::Try },
			{ "typealias", TokenType::Typealias },
			{ "typedef", TokenType::Typedef },
			{ "union", TokenType::Union },
			{ "unsafe", TokenType::Unsafe },
			{ "while", TokenType::While },
			
			{ "bool", TokenType::Bool },
			{ "char", TokenType::Char },
			{ "f16", TokenType::F16 },
			{ "f32", TokenType::F32 },
			{ "f64", TokenType::F64 },
			{ "f128", TokenType::F128 },
			{ "isize", TokenType::ISize },
			{ "i8", TokenType::I8 },
			{ "i16", TokenType::I16 },
			{ "i32", TokenType::I32 },
			{ "i64", TokenType::I64 },
			{ "i128", TokenType::I128 },
			{ "usize", TokenType::USize },
			{ "u8", TokenType::U8 },
			{ "u16", TokenType::U16 },
			{ "u32", TokenType::U32 },
			{ "u64", TokenType::U64 },
			{ "u128", TokenType::U128 },
			
			{ "false", TokenType::False },
			{ "null", TokenType::Null },
			{ "true", TokenType::True },
			
			{ "async", TokenType::Async },
			{ "await", TokenType::Await },
			{ "yield", TokenType::Yield },

			{ "#benchmark" , TokenType::SBenchmark },
			{ "#cond" , TokenType::SConditional },
			{ "#debug" , TokenType::SDebug },
			{ "#errorhandler" , TokenType::SErrorHandler },
			{ "#file" , TokenType::SFile },
			{ "#fileFullPath" , TokenType::SFileFullPath },
			{ "#fullModule" , TokenType::SFullModule },
			{ "#func" , TokenType::SFunc },
			{ "#funcName" , TokenType::SFuncName },
			{ "#if" , TokenType::SIf },
			{ "#line" , TokenType::SLine },
			{ "#module" , TokenType::SModule },
			{ "#package" , TokenType::SPackage },
			{ "#prettyFunc" , TokenType::SPrettyFunc },
			{ "#run" , TokenType::SRun },
			{ "#unittest" , TokenType::SUnittest },
		};

		return keywords;
	}

	Token Lexer::ParseLiteral()
	{
		usize size = m_Content.size();
		usize start = m_Index;
		if (m_Content[m_Index] == '0' && m_Index + 1 < size)
		{
			switch (m_Content[m_Index + 1])
			{
			case 'b':
			case 'B':
			{
				usize end = m_Content.find_first_not_of("01_", m_Index + 2);
				
				StdStringView digitsView = m_Content.substr(m_Index + 2, end - m_Index - 2);
				StdString digits;
				digits.reserve(digitsView.size());
				for (char c : digitsView)
				{
					if (c != '_')
						digits.push_back(c);
				}
				

				u64 value;
				std::from_chars(digits.data(), digits.data() + digits.size(), value, 2);

				m_Index = end;
				TokenType litType = ParseLiteralType();

				if (litType == TokenType::Unknown)
				{
					litType = (value > std::numeric_limits<u32>::max()) ? TokenType::U64Lit : TokenType::U32Lit;
				}

				StdString text = StdString(m_Content.substr(start, m_Index - start));
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
				return{ litType, value, spanId };
			}
			case 'o':
			case 'O':
			{
				usize end = m_Content.find_first_not_of("01234567_", m_Index + 2);

				StdStringView digitsView = m_Content.substr(m_Index + 2, end - m_Index - 2);
				StdString digits;
				digits.reserve(digitsView.size());
				for (char c : digitsView)
				{
					if (c != '_')
						digits.push_back(c);
				}

				u64 value;
				std::from_chars(digits.data(), digits.data() + digits.size(), value, 8);

				m_Index = end;
				TokenType litType = ParseLiteralType();

				if (litType == TokenType::Unknown)
				{
					litType = (value > std::numeric_limits<u32>::max()) ? TokenType::U64 : TokenType::U32Lit;
				}

				StdString text = StdString(m_Content.substr(start, m_Index - start));
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
				return{ litType, value, spanId };
			}
			case 'x':
			case 'X':
			{
				usize end = m_Content.find_first_not_of("0123456789aAbBcCdDeEfF_", m_Index + 2);

				StdStringView digitsView = m_Content.substr(m_Index + 2, end - m_Index - 2);
				StdString digits;
				digits.reserve(digitsView.size());
				for (char c : digitsView)
				{
					if (c != '_')
						digits.push_back(c);
				}

				u64 value;
				std::from_chars(digits.data(), digits.data() + digits.size(), value, 16);

				m_Index = end;
				TokenType litType = ParseLiteralType();

				if (litType == TokenType::Unknown)
				{
					litType = (value > std::numeric_limits<u32>::max()) ? TokenType::U64Lit : TokenType::U32Lit;
				}

				StdString text = StdString(m_Content.substr(start, m_Index - start));
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
				return{ litType, value, spanId };
			}

			default:
				break;
			}
		}

		usize offset = m_Index;
		bool neg = false;
		if (m_Content[offset] == '-')
		{
			++offset;
			neg = true;
		}

		usize end = m_Content.find_first_not_of("0123456789._", offset);
		StdStringView digitsView = m_Content.substr(offset, end - offset);
		
		// Make sure that the range operator is not confused as being part of the literal, i.e 1.. -> 1 ..
		usize doubleDotPos = digitsView.find("..");
		if (doubleDotPos != StdString::npos && offset + doubleDotPos < end)
		{
			digitsView = m_Content.substr(offset, doubleDotPos);
			end = offset + doubleDotPos;
		}

		StdString digits;
		digits.reserve(digitsView.size() + usize(neg));
		if (neg)
			digits.push_back('-');
		
		bool isFp = false;
		for (char c : digitsView)
		{
			if (c != '_')
				digits.push_back(c);
			if (c == '.')
				isFp = true;
		}
		

		if (isFp || m_Content[end] == 'e' || m_Content[end] == 'E')
		{
			f64 val;
			std::from_chars(digits.data(), digits.data() + digits.size(), val);
			
			if (m_Content[end] == 'e' || m_Content[end] == 'E')
			{
				offset = end + 1;
				bool expNeg = false;
				if (m_Content[offset] == '-')
				{
					expNeg = true;
					++offset;
				}
				
				end = m_Content.find_first_not_of("0123456789._", offset);
				StdStringView expDigitsView = m_Content.substr(offset, end - offset);
				StdString expDigits;
				expDigits.reserve(expDigitsView.size() + usize(expNeg));
				if (expNeg)
					expDigits.push_back('-');
				for (char c : expDigitsView)
				{
					if (c != '_')
						expDigits.push_back(c);
				}

				i16 exp;
				std::from_chars(expDigits.data(), expDigits.data() + expDigits.size(), exp);

				f64 multiplier = 10.0;
				if (exp < 0)
				{
					exp = -exp;
					multiplier = 0.1;
				}

				for (i16 i = 0; i < exp; ++i)
				{
					val *= multiplier;
				}
			}

			m_Index = end;
			TokenType litType = ParseLiteralType();
			if (litType == TokenType::Unknown)
			{
				if (neg)
					litType = val <= std::numeric_limits<f32>::lowest() ? TokenType::F64Lit : TokenType::F32Lit;
				else
					litType = val >= std::numeric_limits<f32>::max() ? TokenType::F64Lit : TokenType::F32Lit;
			}

			StdString text = StdString{ m_Content.substr(start, m_Index - start) };
			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
			return { litType, val, spanId };
		}
		else
		{
			m_Index = end;
			TokenType litType = ParseLiteralType();

			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
			if (IsTokenTypeUnsignedLiteral(litType))
			{
				u64 val;
				std::from_chars(digits.data(), digits.data() + digits.size(), val);

				return { litType, val, spanId };
			}
			else
			{
				i64 val;
				std::from_chars(digits.data(), digits.data() + digits.size(), val);

				if (litType == TokenType::Unknown)
				{
					if (neg)
						litType = val <= std::numeric_limits<i32>::lowest() ? TokenType::I64Lit : TokenType::I32Lit;
					else
						litType = val >= std::numeric_limits<i32>::max() ? TokenType::I64Lit : TokenType::I32Lit;
				}

				return { litType, val, spanId };
			}
		}
	}

	TokenType Lexer::ParseLiteralType()
	{
		usize size = m_Content.size();
		if (m_Index < size)
		{
			switch (m_Content[m_Index])
			{
			case 'i':
			{
				if (m_Index + 1 < size)
				{
					usize end = m_Content.find_first_not_of("0123456789", m_Index + 1);
					StdStringView value = m_Content.substr(m_Index + 1, end - m_Index - 1);

					if (value == "8")
					{
						m_Index += 2;
						return TokenType::I8Lit;
					}
					if (value == "16")
					{
						m_Index += 3;
						return TokenType::I16Lit;
					}
					if (value == "32")
					{
						m_Index += 3;
						return TokenType::I32Lit;
					}
					if (value == "64")
					{
						m_Index += 3;
						return TokenType::I64Lit;
					}
					if (value == "128")
					{
						m_Index += 4;
						return TokenType::I128Lit;
					}
				}

				g_ErrorSystem.Error(m_Line, m_Column, "Unexpected literal suffix");
				return TokenType::Unknown;
			}
			case 'u':
			{
				if (m_Index + 1 < size)
				{
					usize end = m_Content.find_first_not_of("0123456789", m_Index+ 1);
					StdStringView value = m_Content.substr(m_Index + 1, end - m_Index - 1);

					if (value == "8")
					{
						m_Index += 2;
						return TokenType::U8Lit;
					}
					if (value == "16")
					{
						m_Index += 3;
						return TokenType::U16Lit;
					}
					if (value == "32")
					{
						m_Index += 3;
						return TokenType::U32Lit;
					}
					if (value == "64")
					{
						m_Index += 3;
						return TokenType::U64Lit;
					}
					if (value == "128")
					{
						m_Index += 4;
						return TokenType::U128Lit;
					}
				}
				
				g_ErrorSystem.Error(m_Line, m_Column, "Unexpected literal suffix");
				return TokenType::Unknown;
			}
			case 'f':
			{
				if (m_Index + 1 < size)
				{
					usize end = m_Content.find_first_not_of("0123456789", m_Index + 1);
					StdStringView value = m_Content.substr(m_Index + 1, end - m_Index - 1);

					if (value == "16")
					{
						m_Index += 3;
						return TokenType::F16Lit;
					}
					if (value == "32")
					{
						m_Index += 3;
						return TokenType::F32Lit;
					}
					if (value == "64")
					{
						m_Index += 3;
						return TokenType::F64Lit;
					}
					if (value == "128")
					{
						m_Index += 4;
						return TokenType::F128Lit;
					}
				}

				g_ErrorSystem.Error(m_Line, m_Column, "Unexpected literal suffix");
				return TokenType::Unknown;
			}
			default:
				return TokenType::Unknown;
			}
		}

		return TokenType::Unknown;
	}

	Token Lexer::ParseChar()
	{
		usize size = m_Content.size();
		usize start = m_Index;

		if (m_Index + 1 >= size)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Character literal is not closed");
			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, m_Index + 1, m_Line, m_Column });
			return { TokenType::Unknown, spanId };
		}

		usize charSize = 1;
		u32 val;
		if (m_Content[m_Index + 1] == '\\')
		{
			val = ParseEscapeCode(m_Index + 1, charSize);
		}
		else
		{
			val = u32(m_Content[m_Index + 1]);
		}

		m_Index += charSize + 1;
		if (m_Index >= size || m_Content[m_Index] != '\'')
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Character literal is not closed");
			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index + 1, m_Line, m_Column });
			return { TokenType::Unknown, spanId };
		}
		++m_Index;
		
		StdString text = StdString{ m_Content.substr(start, m_Index - start) };
		u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
		return { TokenType::CharLit, u64(val), spanId };
	}

	Token Lexer::ParseString()
	{
		usize start = m_Index;
		m_Index = m_Content.find('"', m_Index + 1);
		while (m_Index != StdString::npos && m_Content[m_Index - 1] == '\\')
		{
			usize escapedBackslashes = m_Content.find_last_not_of('\\', m_Index - 1);
			usize numBackslashes = m_Index - escapedBackslashes - 1;
			if (!(numBackslashes & 1))
				break;

			m_Index = m_Content.find('"', m_Index + 1);
		}
		
		if (m_Index == StdString::npos)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "string literal isn't closed");
			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
			return { TokenType::Unknown, spanId };
		}

		++m_Index;
		StdStringView string = m_Content.substr(start, m_Index - start);
		usize nl = string.find('\n', m_Index);
		if (nl != StdString::npos && nl < m_Index)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "A string literal should be contained on 1 line!");
			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
			return { TokenType::Unknown, spanId };
		}

		StdString text = StdString{ string };
		u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
		return { TokenType::StringLit, text, spanId };
	}

	Token Lexer::ParseWysiwygString()
	{
		usize start = m_Index;
		m_Index = m_Content.find('"', m_Index + 2);
		while (m_Index != StdString::npos && m_Content[m_Index - 1] == '\\')
		{
			m_Index = m_Content.find('"', m_Index + 1);
		}

		if (m_Index == StdString::npos)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "wysiwyg string literal isn't closed");
			u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, m_Index + 1, m_Line, m_Column });
			return { TokenType::Unknown, spanId };
		}

		usize nl = m_Content.find('\n', start + 2);
		while (nl < m_Index)
		{
			++m_Line;
			nl = m_Content.find('\n', nl + 1);
		}

		++m_Index;
		StdString text = StdString{ m_Content.substr(start, m_Index - start) };
		u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
		return { TokenType::StringLit, text, spanId };
	}

	u32 Lexer::ParseEscapeCode(usize offset, usize& escapeSize)
	{	
		usize size = m_Content.size();
		usize column = m_Column + offset - m_Index;
		if (offset + 1 >= size)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Escape code at end of file");
			return 0;
		}

		escapeSize = 2;

		switch (m_Content[offset + 1])
		{
		case '0': return 0;
		case 'a': return u32('\a');
		case 'b': return u32('\b');
		case 'f': return u32('\f');
		case 'n': return u32('\n');
		case 'r': return u32('\r');
		case 't': return u32('\t');
		case 'v': return u32('\v');
		case '\\': return u32('\\');
		case 'x':
		{
			if (offset + 3 >= size)
			{
				g_ErrorSystem.Error(m_Line, column, "Invalid escape code");
				return 0;
			}

			StdStringView digits = m_Content.substr(offset + 2, 2);
			StdStringView validDigits = StdStringView{ "0123456789aAbBcCdDeEfF" };

			if (validDigits.find(digits[0]) == StdString::npos ||
				validDigits.find(digits[1]) == StdString::npos)
			{
				char value[3] = { digits[0], digits[1], '\0' };
				g_ErrorSystem.Error(m_Line, column, "Invalid hex value: '%s'", value);
				return 0;
			}

			u32 val;
			std::from_chars(digits.data(), digits.data() + digits.size(), val, 16);
			escapeSize = 4;
			return val;
		}
		case 'o':
		{
			if (offset + 4 >= size)
			{
				g_ErrorSystem.Error(m_Line, column, "Invalid escape code");
				return 0;
			}

			StdStringView digits = m_Content.substr(offset + 2, 3);
			StdStringView validDigits = StdStringView{ "01234567" };

			if (validDigits.find(digits[0]) == StdString::npos ||
				validDigits.find(digits[1]) == StdString::npos ||
				validDigits.find(digits[2]) == StdString::npos)
			{
				char value[4] = { digits[0], digits[1], digits[2], '\0' };
				g_ErrorSystem.Error(m_Line, column, "Invalid hex value: '%s'", value);
				return 0;
			}

			u32 val;
			std::from_chars(digits.data(), digits.data() + digits.size(), val, 8);
			escapeSize = 5;
			return val;
		}
		case 'u':
		{
			if (offset + 5 >= size)
			{
				g_ErrorSystem.Error(m_Line, column, "Invalid escape code");
				return 0;
			}

			StdStringView digits = m_Content.substr(offset + 2, 4);
			StdStringView validDigits = StdStringView{ "0123456789aAbBcCdDeEfF" };

			if (validDigits.find(digits[0]) == StdString::npos ||
				validDigits.find(digits[1]) == StdString::npos ||
				validDigits.find(digits[2]) == StdString::npos ||
				validDigits.find(digits[3]) == StdString::npos)
			{
				char value[5] = { digits[0], digits[1], digits[2], digits[3], '\0' };
				g_ErrorSystem.Error(m_Line, column, "Invalid hex value: '%s'", value);
				return 0;
			}

			u32 val;
			std::from_chars(digits.data(), digits.data() + digits.size(), val, 16);
			escapeSize = 6;
			return val;
		}
		case 'U':
		{
			if (offset + 9 >= size)
			{
				g_ErrorSystem.Error(m_Line, column, "Invalid escape code");
				return 0;
			}

			StdStringView digits = m_Content.substr(offset + 2, 8);
			StdStringView validDigits = StdStringView{ "0123456789aAbBcCdDeEfF" };

			if (validDigits.find(digits[0]) == StdString::npos ||
				validDigits.find(digits[1]) == StdString::npos ||
				validDigits.find(digits[2]) == StdString::npos ||
				validDigits.find(digits[3]) == StdString::npos ||
				validDigits.find(digits[4]) == StdString::npos ||
				validDigits.find(digits[5]) == StdString::npos ||
				validDigits.find(digits[6]) == StdString::npos ||
				validDigits.find(digits[7]) == StdString::npos)
			{
				char value[9] = { digits[0], digits[1], digits[2], digits[3], digits[4], digits[5], digits[6], digits[7], '\0' };
				g_ErrorSystem.Error(m_Line, column, "Invalid hex value: '%s'", value);
				return 0;
			}

			u32 val;
			std::from_chars(digits.data(), digits.data() + digits.size(), val, 16);
			escapeSize = 10;
			return val;
		}
		default:
			return 0;
		}
	}

	void Lexer::ParseSingleLineComment()
	{
		m_Index = m_Content.find('\n', m_Index);
		if (m_Index != StdString::npos)
			++m_Index;
		++m_Line;
		m_Column = 1;
	}

	void Lexer::ParseBlockComment()
	{
		m_Index += 2;
		m_Column += 2;
		usize begin = m_Content.find("/*", m_Index);
		usize end = m_Content.find("*/", m_Index);
		usize nl = m_Content.find('\n', m_Index);

		if (end == StdString::npos)
		{
			g_ErrorSystem.Error(m_Line, m_Column - 2, "Block comment isn't closed");
		}

		do
		{
			usize closest = begin < end ? begin : end;
			while (nl < closest)
			{
				m_Index = nl + 1;
				++m_Line;
				nl = m_Content.find('\n', m_Index);
			}

			m_Column = closest - m_Index;

			if (begin < end)
			{
				m_Index = begin;
				ParseBlockComment();
				begin = m_Content.find("/*", m_Index);
				end = m_Content.find("*/", m_Index);
				nl = m_Content.find('\n', m_Index);
			}
			
		}
		while(begin < end);

		while (nl < end)
		{
			m_Index = nl + 1;
			++m_Line;
			nl = m_Content.find('\n', m_Index);
		}

		end += 2;
		m_Column = end - m_Index;
		m_Index = end;
	}
}
