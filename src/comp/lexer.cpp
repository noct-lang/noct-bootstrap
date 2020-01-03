#include "lexer.hpp"
#include "token.hpp"
#include "common/context.hpp"
#include "comp/compcontext.hpp"
#include <charconv>
#include "common/errorsystem.hpp"
#include <algorithm>
#include "common/logger.hpp"
#include <sstream>

namespace Noctis
{
	Lexer::Lexer(Context* pContext)
		: m_Index(0)
		, m_Line(1)
		, m_Column(1)
		, m_pCtx(pContext)
	{
	}

	void Lexer::Reset()
	{
		m_Index = 0;
		m_Line = 1;
		m_Column = 1;
		m_Tokens.clear();
	}

	void Lexer::Lex(const StdStringView& content)
	{
		usize size = content.size();

		SpanManager& spanManager = m_pCtx->pCompContext->spanManager;
		
		while (m_Index < size)
		{
			usize start = m_Index;
			u64 tokIdx = u64(m_Tokens.size());
			switch (content[m_Index])
			{
			case '=':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::EqEq, "==", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '>')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::DblArrow, "=>", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Eq, "=", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '+':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '+')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::PlusPlus, "++", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::PlusEq, "+=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Plus, "+", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '-':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '-')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::MinusMinus, "--", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::MinusEq, "-=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '>')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::Arrow, "->", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (isdigit(content[m_Index]))
					{
						--m_Index;
						ParseLiteral(content, spanManager, tokIdx);
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Minus, "-", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '*':
			{
				++m_Index;
				if (m_Index < size && content[m_Index] == '=')
				{
					++m_Index;
					m_Tokens.push_back({ TokenType::AsteriskEq, "*=", tokIdx });
					spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
					break;
				}

				m_Tokens.push_back({ TokenType::Asterisk, "*", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '/':
			{
				if (m_Index + 1 < size)
				{
					if (content[m_Index + 1] == '=')
					{
						m_Index += 2;
						m_Tokens.push_back({ TokenType::SlashEq, "/=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index + 1] == '/')
					{
						ParseSingleLineComment(content);
						start = m_Index;
						break;
					}
					if (content[m_Index + 1] == '*')
					{
						ParseBlockComment(content);
						start = m_Index;
						break;
					}
				}

				++m_Index;
				m_Tokens.push_back({ TokenType::Slash, "/", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '%':
			{
				++m_Index;
				if (m_Index < size && content[m_Index] == '=')
				{
					++m_Index;
					m_Tokens.push_back({ TokenType::PercentEq, "%=", tokIdx });
					spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
					break;
				}

				m_Tokens.push_back({ TokenType::Percent, "%", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '~':
			{
				++m_Index;
				if (m_Index < size && content[m_Index] == '=')
				{
					++m_Index;
					m_Tokens.push_back({ TokenType::TildeEq, "~=", tokIdx });
					spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
					break;
				}

				m_Tokens.push_back({ TokenType::Tilde, "~", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '&':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '&')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::AndAnd, "&&", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::AndEq, "&=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::And, "&", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '|':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '|')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::OrOr, "||", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::OrEq, "|=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Or, "&", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '^':
			{
				++m_Index;
				if (m_Index + 1 < size && content[m_Index] == '=')
				{
					++m_Index;
					m_Tokens.push_back({ TokenType::CaretEq, "^=", tokIdx });
					spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
					break;
				}

				m_Tokens.push_back({ TokenType::Caret, "^", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '<':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '<')
					{
						if (m_Index + 1 < size)
						{
							if (content[m_Index + 1] == '<')
							{
								if (m_Index + 2 < size && content[m_Index + 2] == '=')
								{
									m_Index += 3;
									m_Tokens.push_back({ TokenType::LessLessLessEq, "<<<=", tokIdx });
									spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
									break;
								}

								m_Index += 2;
								m_Tokens.push_back({ TokenType::LessLessLess, "<<<", tokIdx });
								spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
								break;
							}
							if (content[m_Index + 1] == '*')
							{
								if (m_Index + 2 < size && content[m_Index + 2] == '=')
								{
									m_Index += 3;
									m_Tokens.push_back({ TokenType::LessLessAsteriskEq, "<<*=", tokIdx });
									spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
									break;
								}

								m_Index += 2;
								m_Tokens.push_back({ TokenType::LessLessAsterisk, "<<*", tokIdx });
								spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
								break;
							}
							if (content[m_Index + 1] == '=')
							{
								m_Index += 2;
								m_Tokens.push_back({ TokenType::LessLessEq, "<<=", tokIdx });
								spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
								break;
							}
						}
						
						++m_Index;
						m_Tokens.push_back({ TokenType::LessLess, "<<", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::LessEq, "<=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Less, "<", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '>':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '>')
					{
						if (m_Index + 1 < size)
						{
							if (content[m_Index + 1] == '>')
							{
								if (m_Index + 2 < size && content[m_Index + 2] == '=')
								{
									m_Index += 3;
									m_Tokens.push_back({ TokenType::GreaterGreaterGreaterEq, ">>>=", tokIdx });
									spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
									break;
								}

								m_Index += 2;
								m_Tokens.push_back({ TokenType::GreaterGreaterGreater, ">>>", tokIdx });
								spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
								break;
							}
							if (content[m_Index + 1] == '*')
							{
								if (m_Index + 2 < size && content[m_Index + 2] == '=')
								{
									m_Index += 3;
									m_Tokens.push_back({ TokenType::GreaterGreaterAsteriskEq, ">>*=", tokIdx });
									spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
									break;
								}

								m_Index += 2;
								m_Tokens.push_back({ TokenType::GreaterGreaterAsterisk, ">>*", tokIdx });
								spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
								break;
							}
							if (content[m_Index + 1] == '=')
							{
								m_Index += 2;
								m_Tokens.push_back({ TokenType::GreaterGreaterEq, ">>=", tokIdx });
								spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
								break;
							}
						}

						++m_Index;
						m_Tokens.push_back({ TokenType::GreaterGreater, ">>", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::GreaterEq, ">=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Greater, ">", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '!':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ExclaimEq, "!=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '<')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ExclaimLess, "!<", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '(')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ExclaimParen, "!(", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '{')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ExclaimBrace, "!{", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '[')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ExclaimBracket, "![", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Exclaim, "!", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '(':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::LParen, "(", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case ')':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::RParen, ")", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '{':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::LBrace, "{", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '}':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::RBrace, "}", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '[':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::LBracket, "[", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case ']':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::RBracket, "]", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case ',':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::Comma, ",", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case ';':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::Semicolon, ";", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case ':':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == ':')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ColonColon, "::", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '=')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::ColonEq, ":=", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Colon, ":", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '.':
			{
				++m_Index;
				if (m_Index < size && content[m_Index] == '.')
				{
					if (m_Index + 1 < size)
					{
						if (content[m_Index + 1] == '.')
						{
							m_Index += 2;
							m_Tokens.push_back({ TokenType::DotDotDot, "...", tokIdx });
							spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
							break;
						}
						if (content[m_Index + 1] == '=')
						{
							m_Index += 2;
							m_Tokens.push_back({ TokenType::DotDotEq, "..=", tokIdx });
							spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
							break;
						}
					}

					++m_Index;
					m_Tokens.push_back({ TokenType::DotDot, "..", tokIdx });
					spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
					break;
				}
				if (isdigit(content[m_Index + 1]))
				{
					--m_Index;
					ParseLiteral(content, spanManager, tokIdx);
					break;
				}

				m_Tokens.push_back({ TokenType::Dot, ".", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '@':
			{
				++m_Index;
				if (m_Index < size && content[m_Index] == ':')
				{
					++m_Index;
					m_Tokens.push_back({ TokenType::AtColon, "@:", tokIdx });
					spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
					break;
				}

				m_Tokens.push_back({ TokenType::At, "@", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '?':
			{
				++m_Index;
				if (m_Index < size)
				{
					if (content[m_Index] == '?')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::QuestionQuestion, "??", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == ':')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::QuestionColon, "?:", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '.')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::QuestionDot, "?.", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
					if (content[m_Index] == '[')
					{
						++m_Index;
						m_Tokens.push_back({ TokenType::QuestionBracket, "?[", tokIdx });
						spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
						break;
					}
				}

				m_Tokens.push_back({ TokenType::Question, "?", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '#':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::Hash, "#", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
				break;
			}
			case '$':
			{
				++m_Index;
				m_Tokens.push_back({ TokenType::Dollar, "$", tokIdx });
				spanManager.AddSpan({ size, m_Index, m_Line, m_Column });
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
				ParseLiteral(content, spanManager, tokIdx);
				break;

			case ' ':
			{
				++m_Index;
				break;
			}
			case '\t':
			{
				m_Index += m_pCtx->options.TabWidth();
			}
			case '\r':
			{
				++m_Index;
				if (m_Index < size && content[m_Index] != '\n')
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
				ParseChar(content, spanManager, tokIdx);
				break;
			}
			case '"':
			{
				ParseString(content, spanManager, tokIdx);
				break;
			}
			case '\0':
				return;

			case 'r':
			{
				if (m_Index + 1 < size && content[m_Index + 1] == '"')
				{
					ParseWysiwygString(content, spanManager, tokIdx);
					break;
				}
				
				// fallthrough
			}
			default:
			{
				usize end = content.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", m_Index);
				StdStringView iden = content.substr(m_Index, end - m_Index);
				
				StdUnorderedMap<StdStringView, TokenType>& keywords = GetKeywordMap();
				auto it = keywords.find(iden);
				if (it != keywords.end())
					m_Tokens.push_back({ it->second, StdString{ iden }, tokIdx });
				else
					m_Tokens.push_back({ TokenType::Iden, StdString{ iden }, tokIdx });

				spanManager.AddSpan({ m_Index, end, m_Line, m_Column });
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
			const Noctis::Token& tok = m_Tokens[i];
			std::stringstream ss;

			Noctis::Span span = m_pCtx->pCompContext->spanManager.GetSpan(i);

			ss << '[' << span.line << ':' << span.column << ']';
			ss << ", ";
			ss << Noctis::GetTokenTypeName(tok.Type());
			ss << ", ";
			ss << tok.Text();

			if (Noctis::IsTokenTypeSignedLiteral(tok.Type()))
				ss << ", " << tok.Signed();
			if (Noctis::IsTokenTypeUnsignedLiteral(tok.Type()))
				ss << ", " << tok.Unsigned();
			if (Noctis::IsTokenTypeFpLiteral(tok.Type()))
				ss << ", " << tok.Fp();
			if (tok.Type() == Noctis::TokenType::CharLit)
				ss << ", " << tok.Signed();
			if (tok.Type() == Noctis::TokenType::True || tok.Type() == Noctis::TokenType::False)
				ss << ", " << tok.Bool();

			ss << "\n";

			g_Logger.Log(ss.str());
		}
	}

	StdUnorderedMap<StdStringView, TokenType>& Lexer::GetKeywordMap()
	{
		static StdUnorderedMap<StdStringView, TokenType> keywords =
		{
			{ "break", TokenType::Break },
			{ "cast", TokenType::Cast },
			{ "cconst", TokenType::CConst },
			{ "const", TokenType::Const },
			{ "continue", TokenType::Continue },
			{ "defer", TokenType::Defer },
			{ "do", TokenType::Do },
			{ "else", TokenType::Else },
			{ "enum", TokenType::Enum },
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
			{ "lazy", TokenType::Lazy },
			{ "loop", TokenType::Loop },
			{ "macro", TokenType::Macro },
			{ "module", TokenType::Module },
			{ "move", TokenType::Move },
			{ "public", TokenType::Public },
			{ "return", TokenType::Return },
			{ "stack_defer", TokenType::StackDefer },
			{ "static", TokenType::Static },
			{ "struct", TokenType::Struct },
			{ "switch", TokenType::Switch },
			{ "transmute", TokenType::Transmute },
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
			{ "void", TokenType::Void },
			
			{ "false", TokenType::False },
			{ "null", TokenType::Null },
			{ "true", TokenType::True },
			
			{ "as", TokenType::As },
			{ "dynlib", TokenType::Dynlib },
			{ "package", TokenType::Package },
			{ "self", TokenType::IdenSelf },
			{ "Self", TokenType::Self },
			{ "weak", TokenType::Weak },
		};

		return keywords;
	}

	void Lexer::ParseLiteral(StdStringView content, SpanManager& spanManager, u64 tokIdx)
	{
		usize size = content.size();
		usize start = m_Index;
		if (content[m_Index] == '0' && m_Index + 1 < size)
		{
			switch (content[m_Index + 1])
			{
			case 'b':
			case 'B':
			{
				usize end = content.find_first_not_of("01_", m_Index + 2);
				
				StdStringView digitsView = content.substr(m_Index + 2, end - m_Index - 2);
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
				TokenType litType = ParseLiteralType(content);

				if (litType == TokenType::Unknown)
				{
					litType = (value > std::numeric_limits<u32>::max()) ? TokenType::U64Lit : TokenType::U32Lit;
				}

				StdString text = StdString(content.substr(start, m_Index - start));
				m_Tokens.push_back({ litType, text, value, tokIdx });
				spanManager.AddSpan({ start, m_Index, m_Line, m_Column });

				return;
			}
			case 'o':
			case 'O':
			{
				usize end = content.find_first_not_of("01234567_", m_Index + 2);

				StdStringView digitsView = content.substr(m_Index + 2, end - m_Index - 2);
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
				TokenType litType = ParseLiteralType(content);

				if (litType == TokenType::Unknown)
				{
					litType = (value > std::numeric_limits<u32>::max()) ? TokenType::U64 : TokenType::U32Lit;
				}

				StdString text = StdString(content.substr(start, m_Index - start));
				m_Tokens.push_back({ litType, text, value, tokIdx });
				spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
				
				return;
			}
			case 'x':
			case 'X':
			{
				usize end = content.find_first_not_of("0123456789aAbBcCdDeEfF_", m_Index + 2);

				StdStringView digitsView = content.substr(m_Index + 2, end - m_Index - 2);
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
				TokenType litType = ParseLiteralType(content);

				if (litType == TokenType::Unknown)
				{
					litType = (value > std::numeric_limits<u32>::max()) ? TokenType::U64Lit : TokenType::U32Lit;
				}

				StdString text = StdString(content.substr(start, m_Index - start));
				m_Tokens.push_back({ litType, text, value, tokIdx });
				spanManager.AddSpan({ start, m_Index, m_Line, m_Column });

				return;
			}

			default:
				break;
			}
		}

		usize offset = m_Index;
		bool neg = false;
		if (content[offset] == '-')
		{
			++offset;
			neg = true;
		}

		usize end = content.find_first_not_of("0123456789._", offset);
		StdStringView digitsView = content.substr(offset, end - offset);
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
		

		if (isFp || content[end] == 'e' || content[end] == 'E')
		{
			f64 val;
			std::from_chars(digits.data(), digits.data() + digits.size(), val);
			
			if (content[end] == 'e' || content[end] == 'E')
			{
				offset = end + 1;
				bool expNeg = false;
				if (content[offset] == '-')
				{
					expNeg = true;
					++offset;
				}
				
				end = content.find_first_not_of("0123456789._", offset);
				StdStringView expDigitsView = content.substr(offset, end - offset);
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
			TokenType litType = ParseLiteralType(content);
			if (litType == TokenType::Unknown)
			{
				if (neg)
					litType = val <= std::numeric_limits<f32>::lowest() ? TokenType::F64Lit : TokenType::F32Lit;
				else
					litType = val >= std::numeric_limits<f32>::max() ? TokenType::F64Lit : TokenType::F32Lit;
			}

			StdString text = StdString{ content.substr(start, m_Index - start) };
			m_Tokens.push_back({ litType, text, val, tokIdx });
			spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
		}
		else
		{
			m_Index = end;
			TokenType litType = ParseLiteralType(content);

			if (IsTokenTypeUnsignedLiteral(litType))
			{
				u64 val;
				std::from_chars(digits.data(), digits.data() + digits.size(), val);

				StdString text = StdString{ content.substr(start, m_Index - start) };
				m_Tokens.push_back({ litType, text, val, tokIdx });
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

				StdString text = StdString{ content.substr(start, m_Index - start) };
				m_Tokens.push_back({ litType, text, val, tokIdx });
			}

			spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
		}
	}

	TokenType Lexer::ParseLiteralType(StdStringView content)
	{
		usize size = content.size();
		if (m_Index < size)
		{
			switch (content[m_Index])
			{
			case 'i':
			{
				if (m_Index + 1 < size)
				{
					usize end = content.find_first_of(" \t\r\n", m_Index);
					StdStringView value = content.substr(m_Index + 1, end - m_Index - 1);

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
					usize end = content.find_first_of(" \t\r\n", m_Index);
					StdStringView value = content.substr(m_Index + 1, end - m_Index - 1);

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
					usize end = content.find_first_of(" \t\r\n", m_Index);
					StdStringView value = content.substr(m_Index + 1, end - m_Index - 1);

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

	void Lexer::ParseChar(StdStringView content, SpanManager& spanManager, u64 tokIdx)
	{
		usize size = content.size();
		usize start = m_Index;

		if (m_Index + 1 >= size)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Character literal is not closed");
			return;
		}

		usize charSize = 1;
		u32 val;
		if (content[m_Index + 1] == '\\')
		{
			val = ParseEscapeCode(content, m_Index + 1, charSize);
		}
		else
		{
			val = u32(content[m_Index]);
		}

		m_Index += charSize + 1;
		if (m_Index >= size || content[m_Index] != '\'')
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Character literal is not closed");
			return;
		}
		++m_Index;
		
		StdString text = StdString{ content.substr(start, m_Index - start) };
		m_Tokens.push_back({ TokenType::CharLit, text, u64(val), tokIdx });
		spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
	}

	void Lexer::ParseString(StdStringView content, SpanManager& spanManager, u64 tokIdx)
	{
		usize start = m_Index;
		m_Index = content.find('"', m_Index + 1);
		while (m_Index != StdString::npos && content[m_Index - 1] == '\\')
		{
			usize escapedBackslashes = content.find_last_not_of('\\', m_Index - 1);
			usize numBackslashes = m_Index - escapedBackslashes - 1;
			if (!(numBackslashes & 1))
				break;

			m_Index = content.find('"', m_Index + 1);
		}
		
		if (m_Index == StdString::npos)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "string literal isn't closed");
			return;
		}

		++m_Index;
		StdStringView string = content.substr(start, m_Index - start);
		usize nl = string.find('\n', m_Index);
		if (nl != StdString::npos && nl < m_Index)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "A string literal should be contained on 1 line!");
			return;
		}

		StdString text = StdString{ string };
		m_Tokens.push_back({ TokenType::StringLit, text, tokIdx });
		spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
	}

	void Lexer::ParseWysiwygString(StdStringView content, SpanManager& spanManager, u64 tokIdx)
	{
		usize start = m_Index;
		m_Index = content.find('"', m_Index + 2);
		while (m_Index != StdString::npos && content[m_Index - 1] == '\\')
		{
			m_Index = content.find('"', m_Index + 1);
		}

		if (m_Index == StdString::npos)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "wysiwyg string literal isn't closed");
			return;
		}

		usize nl = content.find('\n', start + 2);
		while (nl < m_Index)
		{
			++m_Line;
			nl = content.find('\n', nl + 1);
		}

		++m_Index;
		StdString text = StdString{ content.substr(start, m_Index - start) };
		m_Tokens.push_back({ TokenType::StringLit, text, tokIdx });
		spanManager.AddSpan({ start, m_Index, m_Line, m_Column });
	}

	u32 Lexer::ParseEscapeCode(StdStringView content, usize offset, usize& escapeSize)
	{	
		usize size = content.size();
		usize column = m_Column + offset - m_Index;
		if (offset + 1 >= size)
		{
			g_ErrorSystem.Error(m_Line, m_Column, "Escape code at end of file");
			return 0;
		}

		escapeSize = 2;

		switch (content[offset + 1])
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

			StdStringView digits = content.substr(offset + 2, 2);
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

			StdStringView digits = content.substr(offset + 2, 3);
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

			StdStringView digits = content.substr(offset + 2, 4);
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

			StdStringView digits = content.substr(offset + 2, 8);
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

	void Lexer::ParseSingleLineComment(StdStringView content)
	{
		m_Index = content.find('\n', m_Index);
		if (m_Index != StdString::npos)
			++m_Index;
		++m_Line;
		m_Column = 1;
	}

	void Lexer::ParseBlockComment(StdStringView content)
	{
		m_Index += 2;
		m_Column += 2;
		usize begin = content.find("/*", m_Index);
		usize end = content.find("*/", m_Index);
		usize nl = content.find('\n', m_Index);

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
				nl = content.find('\n', m_Index);
			}

			m_Column = closest - m_Index;

			if (begin < end)
			{
				m_Index = begin;
				ParseBlockComment(content);
				begin = content.find("/*", m_Index);
				end = content.find("*/", m_Index);
				nl = content.find('\n', m_Index);
			}
			
		}
		while(begin < end);

		while (nl < end)
		{
			m_Index = nl + 1;
			++m_Line;
			nl = content.find('\n', m_Index);
		}

		end += 2;
		m_Column = end - m_Index;
		m_Index = end;
	}
}
