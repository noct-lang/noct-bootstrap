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
		m_Tokens.clear();
	}

	void Lexer::Lex(const StdString& filePath, const StdStringView& content)
	{
		m_FilePath = filePath;
		m_Content = content;
		
		usize size = m_Content.size();
		
		while (m_Index < size)
		{
			usize start = m_Index;
			switch (m_Content[m_Index])
			{
			case '=':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::EqEq, spanId });
						break;
					}
					if (m_Content[m_Index] == '>')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::DblArrow, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Eq, spanId });
				break;
			}
			case '+':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '+')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::PlusPlus, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::PlusEq, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Plus, spanId });
				break;
			}
			case '-':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '-')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::MinusMinus, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::MinusEq, spanId });
						break;
					}
					if (m_Content[m_Index] == '>')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::Arrow, spanId });
						break;
					}
					if (isdigit(m_Content[m_Index]))
					{
						--m_Index;
						ParseLiteral();
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Minus, spanId });
				break;
			}
			case '*':
			{
				++m_Index;
				if (m_Index < size && m_Content[m_Index] == '=')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::AsteriskEq, spanId });
					break;
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Asterisk, spanId });
				break;
			}
			case '/':
			{
				if (m_Index + 1 < size)
				{
					if (m_Content[m_Index + 1] == '=')
					{
						m_Index += 2;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::SlashEq, spanId });
						break;
					}
					if (m_Content[m_Index + 1] == '/')
					{
						ParseSingleLineComment();
						start = m_Index;
						break;
					}
					if (m_Content[m_Index + 1] == '*')
					{
						ParseBlockComment();
						start = m_Index;
						break;
					}
				}

				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Slash, spanId });
				break;
			}
			case '%':
			{
				++m_Index;
				if (m_Index < size && m_Content[m_Index] == '=')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::PercentEq, spanId });
					break;
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Percent, spanId });
				break;
			}
			case '~':
			{
				++m_Index;
				if (m_Index < size && m_Content[m_Index] == '=')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::TildeEq, spanId });
					break;
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Tilde, spanId });
				break;
			}
			case '&':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '&')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::AndAnd, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::AndEq, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::And, spanId });
				break;
			}
			case '|':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '|')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::OrOr, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::OrEq, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Or, spanId });
				break;
			}
			case '^':
			{
				++m_Index;
				if (m_Index + 1 < size && m_Content[m_Index] == '=')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::CaretEq, spanId });
					break;
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Caret, spanId });
				break;
			}
			case '<':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '<')
					{
						if (m_Index + 1 < size)
						{
							if (m_Content[m_Index + 1] == '<')
							{
								if (m_Index + 2 < size && m_Content[m_Index + 2] == '=')
								{
									m_Index += 3;
									u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
									m_Tokens.push_back({ TokenType::LessLessLessEq, spanId });
									break;
								}

								m_Index += 2;
								u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
								m_Tokens.push_back({ TokenType::LessLessLess, spanId });
								break;
							}
							if (m_Content[m_Index + 1] == '*')
							{
								if (m_Index + 2 < size && m_Content[m_Index + 2] == '=')
								{
									m_Index += 3;
									u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
									m_Tokens.push_back({ TokenType::LessLessAsteriskEq, spanId });
									break;
								}

								m_Index += 2;
								u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
								m_Tokens.push_back({ TokenType::LessLessAsterisk, spanId });
								break;
							}
							if (m_Content[m_Index + 1] == '=')
							{
								m_Index += 2;
								u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
								m_Tokens.push_back({ TokenType::LessLessEq, spanId });
								break;
							}
						}
						
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::LessLess, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::LessEq, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Less, spanId });
				break;
			}
			case '>':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '>')
					{
						if (m_Index + 1 < size)
						{
							if (m_Content[m_Index + 1] == '>')
							{
								if (m_Index + 2 < size && m_Content[m_Index + 2] == '=')
								{
									m_Index += 3;
									u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
									m_Tokens.push_back({ TokenType::GreaterGreaterGreaterEq, spanId });
									break;
								}

								m_Index += 2;
								u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
								m_Tokens.push_back({ TokenType::GreaterGreaterGreater, spanId });
								break;
							}
							if (m_Content[m_Index + 1] == '*')
							{
								if (m_Index + 2 < size && m_Content[m_Index + 2] == '=')
								{
									m_Index += 3;
									u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
									m_Tokens.push_back({ TokenType::GreaterGreaterAsteriskEq, spanId });
									break;
								}

								m_Index += 2;
								u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
								m_Tokens.push_back({ TokenType::GreaterGreaterAsterisk, spanId });
								break;
							}
							if (m_Content[m_Index + 1] == '=')
							{
								m_Index += 2;
								u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
								m_Tokens.push_back({ TokenType::GreaterGreaterEq, spanId });
								break;
							}
						}

						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::GreaterGreater, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::GreaterEq, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Greater, ">", spanId });
				break;
			}
			case '!':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::ExclaimEq, spanId });
						break;
					}
					if (m_Content[m_Index] == '<')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::ExclaimLess, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Exclaim, spanId });
				break;
			}
			case '(':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::LParen, spanId });
				break;
			}
			case ')':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::RParen, spanId });
				break;
			}
			case '{':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::LBrace, spanId });
				break;
			}
			case '}':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::RBrace, spanId });
				break;
			}
			case '[':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::LBracket, spanId });
				break;
			}
			case ']':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::RBracket, spanId });
				break;
			}
			case ',':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Comma, spanId });
				break;
			}
			case ';':
			{
				++m_Index;
				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Semicolon, spanId });
				break;
			}
			case ':':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == ':')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::ColonColon, spanId });
						break;
					}
					if (m_Content[m_Index] == '=')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::ColonEq, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Colon, ":", spanId });
				break;
			}
			case '.':
			{
				++m_Index;
				if (m_Index < size && m_Content[m_Index] == '.')
				{
					if (m_Index + 1 < size)
					{
						if (m_Content[m_Index + 1] == '.')
						{
							m_Index += 2;
							u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
							m_Tokens.push_back({ TokenType::DotDotDot, spanId });
							break;
						}
						if (m_Content[m_Index + 1] == '=')
						{
							m_Index += 2;
							u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
							m_Tokens.push_back({ TokenType::DotDotEq, spanId });
							break;
						}
					}

					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::DotDot, spanId });
					break;
				}
				if (isdigit(m_Content[m_Index + 1]))
				{
					--m_Index;
					ParseLiteral();
					break;
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Dot, spanId });
				break;
			}
			case '@':
			{
				++m_Index;
				if (m_Index < size && m_Content[m_Index] == ':')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::AtColon, spanId });
					break;
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::At, spanId });
				break;
			}
			case '?':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (m_Content[m_Index] == '?')
					{
						if (m_Index + 1 < size && m_Content[m_Index + 1] == '=')
						{
							m_Index += 2;
							u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
							m_Tokens.push_back({ TokenType::QuestionQuestionEq, spanId });
							break;
						}
						
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::QuestionQuestion, spanId });
						break;
					}
					if (m_Content[m_Index] == ':')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::QuestionColon, spanId });
						break;
					}
					if (m_Content[m_Index] == '.')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::QuestionDot, spanId });
						break;
					}
					if (m_Content[m_Index] == '[')
					{
						++m_Index;
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
						m_Tokens.push_back({ TokenType::QuestionBracket, spanId });
						break;
					}
				}

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::Question, spanId });
				break;
			}
			case '$':
			{
				++m_Index;

				if (m_Content[m_Index] == '(')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::DollarParen, spanId });
					break;
				}
				else if (m_Content[m_Index] == '[')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::DollarBracket, spanId });
					break;
				}
				else if (m_Content[m_Index] == '{')
				{
					++m_Index;
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { size, m_Index, m_Line, m_Column });
					m_Tokens.push_back({ TokenType::DollarBrace, spanId });
					break;
				}

				usize searchStart = m_Index + 1;
				usize end = m_Content.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", searchStart);
				StdStringView iden = m_Content.substr(m_Index, end - m_Index);

				u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, end, m_Line, m_Column });
				m_Tokens.push_back({ TokenType::MacroIden, StdString{ iden }, spanId });
				m_Index = end;
				break;
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
				ParseLiteral();
				break;

			case ' ':
			{
				++m_Index;
				break;
			}
			case '\t':
			{
				m_Index += g_Ctx.options.TabWidth();
			}
			case '\r':
			{
				++m_Index;
				if (m_Index < size && m_Content[m_Index] != '\n')
					break;
				
				// fallthrough
			}
			case '\n':
			{
				++m_Index;
				start = m_Index;
				++m_Line;
				m_Column = 1;
				break;
			}
			case '\'':
			{
				ParseChar();
				break;
			}
			case '"':
			{
				ParseString();
				break;
			}
			case '\0':
				return;

			case 'r':
			{
				if (m_Index + 1 < size && m_Content[m_Index + 1] == '"')
				{
					ParseWysiwygString();
					break;
				}
				
				// fallthrough
			}
			default:
			{
				usize searchStart = m_Index;
				if (searchStart + 1 < size && m_Content[searchStart] == '#')
					++searchStart;
				
				usize end = m_Content.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", searchStart);
				StdStringView iden = m_Content.substr(m_Index, end - m_Index);
				
				StdUnorderedMap<StdStringView, TokenType>& keywords = GetKeywordMap();
				auto it = keywords.find(iden);
				if (it != keywords.end())
				{
					if (it->second == TokenType::In &&
						!m_Tokens.empty() && m_Tokens.back().Type() == TokenType::Exclaim)
					{
						m_Tokens.back() = Token{ TokenType::NotIn, m_Tokens.back().Idx() };
					}
					else if (it->second == TokenType::Is &&
						!m_Tokens.empty() && m_Tokens.back().Type() == TokenType::Exclaim)
					{
						m_Tokens.back() = Token{ TokenType::NotIs, m_Tokens.back().Idx() };
					}
					else if (it->second == TokenType::Try && m_Content.size() > m_Index + 1)
					{
						if (m_Content[m_Index] == '?')
						{
							u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, end, m_Line, m_Column });
							m_Tokens.emplace_back(TokenType::TryNullable, spanId);
							++m_Index;
						}
						else if (m_Content[m_Index] == '!')
						{
							u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, end, m_Line, m_Column });
							m_Tokens.emplace_back(TokenType::TryPanic, spanId);
							++m_Index;
						}
						else
						{
							u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, end, m_Line, m_Column });
							m_Tokens.emplace_back(TokenType::Try, spanId);
						}
					}
					else
					{
						u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, end, m_Line, m_Column });
						m_Tokens.emplace_back(it->second, StdString{ iden }, spanId);
					}
				}
				else
				{
					u64 spanId = g_SpanManager.AddSpan(m_FilePath, { m_Index, end, m_Line, m_Column });
					m_Tokens.emplace_back(TokenType::Iden, StdString{ iden }, spanId);
				}

				m_Index = end;
			}
			}

			m_Column += (m_Index - start);
		}

	}

	void Lexer::LogTokens()
	{
		for (usize i = 0; i < m_Tokens.size(); ++i)
		{
			const Token& tok = m_Tokens[i];
			std::stringstream ss;

			Span span = g_SpanManager.GetSpan(tok.Idx());

			ss << '[' << span.line << ':' << span.column << ']';
			ss << ", ";
			ss << GetTokenTypeName(tok.Type());

			if (tok.Type() == TokenType::Iden)
			{
				ss << ", ";
				ss << tok.Text();
			}

			if (IsTokenTypeSignedLiteral(tok.Type()))
				ss << ", " << tok.Signed();
			if (IsTokenTypeUnsignedLiteral(tok.Type()))
				ss << ", " << tok.Unsigned();
			if (IsTokenTypeFpLiteral(tok.Type()))
				ss << ", " << tok.Fp();
			if (tok.Type() == TokenType::CharLit)
				ss << ", " << tok.Signed();
			if (tok.Type() == TokenType::True || tok.Type() == TokenType::False)
				ss << ", " << tok.Bool();

			ss << "\n";

			g_Logger.Log(ss.str());
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

	void Lexer::ParseLiteral()
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
				m_Tokens.push_back({ litType, value, spanId });

				return;
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
				m_Tokens.push_back({ litType, value, spanId });
				
				return;
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
				m_Tokens.push_back({ litType, value, spanId });

				return;
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
			m_Tokens.push_back({ litType, val, spanId });
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

				m_Tokens.push_back({ litType, val, spanId });
				StdString text = StdString{ m_Content.substr(start, m_Index - start) };
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

				StdString text = StdString{ m_Content.substr(start, m_Index - start) };
				m_Tokens.push_back({ litType, val, spanId });
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

	void Lexer::ParseChar()
	{
		usize size = m_Content.size();
		usize start = m_Index;

		if (m_Index + 1 >= size)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Character literal is not closed");
			return;
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
			return;
		}
		++m_Index;
		
		StdString text = StdString{ m_Content.substr(start, m_Index - start) };
		u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
		m_Tokens.push_back({ TokenType::CharLit, u64(val), spanId });
	}

	void Lexer::ParseString()
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
			return;
		}

		++m_Index;
		StdStringView string = m_Content.substr(start, m_Index - start);
		usize nl = string.find('\n', m_Index);
		if (nl != StdString::npos && nl < m_Index)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "A string literal should be contained on 1 line!");
			return;
		}

		StdString text = StdString{ string };
		u64 spanId = g_SpanManager.AddSpan(m_FilePath, { start, m_Index, m_Line, m_Column });
		m_Tokens.push_back({ TokenType::StringLit, text, spanId });
	}

	void Lexer::ParseWysiwygString()
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
			return;
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
		m_Tokens.push_back({ TokenType::StringLit, text, spanId });
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
