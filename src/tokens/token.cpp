#include "token.hpp"
#include "span.hpp"
#include "common/logger.hpp"
#include <sstream>

#include "common/context.hpp"
#include "common/errorsystem.hpp"

namespace Noctis
{
	StdStringView GetTokenTypeName(TokenType type)
	{
		switch (type)
		{
		case TokenType::Unknown:                  return "__unknown__";
			
		case TokenType::As:                       return "as";
		case TokenType::Break:                    return "break";
		case TokenType::Comptime:                 return "comptime";
		case TokenType::Const:                    return "const";
		case TokenType::Continue:                 return "continue";
		case TokenType::Defer:                    return "defer";
		case TokenType::Do:                       return "do";
		case TokenType::Else:                     return "else";
		case TokenType::Enum:                     return "enum";
		case TokenType::ErrDefer:                 return "errdefer";
		case TokenType::Fallthrough:              return "fallthrough";
		case TokenType::For:                      return "for";
		case TokenType::Func:                     return "func";
		case TokenType::Goto:                     return "goto";
		case TokenType::If:                       return "if";
		case TokenType::Immutable:                return "immutable";
		case TokenType::Import:                   return "import";
		case TokenType::Impl:                     return "impl";
		case TokenType::In:                       return "in";
		case TokenType::NotIn:                    return "!in";
		case TokenType::Interface:                return "interface";
		case TokenType::Is:                       return "is";
		case TokenType::NotIs:                    return "!is";
		case TokenType::Lazy:                     return "lazy";
		case TokenType::Loop:                     return "loop";
		case TokenType::Macro:                    return "macro";
		case TokenType::Module:                   return "module";
		case TokenType::Move:                     return "move";
		case TokenType::Mut:                      return "mut";
		case TokenType::Public:                   return "public";
		case TokenType::Return:                   return "return";
		case TokenType::Static:                   return "static";
		case TokenType::Struct:                   return "struct";
		case TokenType::Switch:                   return "switch";
		case TokenType::Throw:                    return "throw";
		case TokenType::Transmute:                return "transmute";
		case TokenType::Try:                      return "try";
		case TokenType::Typealias:                return "typealias";
		case TokenType::Typedef:                  return "typedef";
		case TokenType::Union:                    return "union";
		case TokenType::Unsafe:                   return "unsafe";
		case TokenType::While:                    return "while";
			
		case TokenType::Bool:                     return "bool";
		case TokenType::Char:                     return "char";
		case TokenType::F16:                      return "f16";
		case TokenType::F32:                      return "f32";
		case TokenType::F64:                      return "f64";
		case TokenType::F128:                     return "f128";
		case TokenType::ISize:                    return "isize";
		case TokenType::I8:                       return "i8";
		case TokenType::I16:                      return "i16";
		case TokenType::I32:                      return "i32";
		case TokenType::I64:                      return "i64";
		case TokenType::I128:                     return "i128";
		case TokenType::USize:                    return "uSize";
		case TokenType::U8:                       return "u8";
		case TokenType::U16:                      return "u16";
		case TokenType::U32:                      return "u32";
		case TokenType::U64:                      return "u64";
		case TokenType::U128:                     return "u128";
			
		case TokenType::False:                    return "false";
		case TokenType::Null:                     return "null";
		case TokenType::True:                     return "true";
			
		case TokenType::Async:                    return "async";
		case TokenType::Await:                    return "await";
		case TokenType::Yield:                    return "yield";

		case TokenType::SBenchmark:               return "#benchmark";
		case TokenType::SConditional:             return "#conditional";
		case TokenType::SDebug:                   return "#debug";
		case TokenType::SErrorHandler:            return "#errorhandler";
		case TokenType::SFile:                    return "#file";
		case TokenType::SFileFullPath:            return "#fileFullPath";
		case TokenType::SFullModule:              return "#fullModule";
		case TokenType::SFunc:                    return "#func";
		case TokenType::SFuncName:                return "#funcName";
		case TokenType::SIf:                      return "#if";
		case TokenType::SLine:                    return "#line";
		case TokenType::SModule:                  return "#module";
		case TokenType::SPackage:                 return "#package";
		case TokenType::SPrettyFunc:              return "#prettyFunc";
		case TokenType::SRun:                     return "#run";
		case TokenType::SUnittest:                return "#unittest";
			
		case TokenType::Eq:                       return "=";
		case TokenType::EqEq:                     return "==";
		case TokenType::DblArrow:                 return "=>";
		case TokenType::Plus:                     return "+";
		case TokenType::PlusPlus:                 return "++";
		case TokenType::PlusEq:                   return "+=";
		case TokenType::Minus:                    return "-";
		case TokenType::MinusMinus:               return "--";
		case TokenType::MinusEq:                  return "-=";
		case TokenType::Arrow:                    return "->";
		case TokenType::Asterisk:                 return "*";
		case TokenType::AsteriskEq:               return "*=";
		case TokenType::Slash:                    return "/";
		case TokenType::SlashEq:                  return "/=";
		case TokenType::Percent:                  return "%";
		case TokenType::PercentEq:                return "%=";
		case TokenType::Tilde:                    return "~";
		case TokenType::TildeEq:                  return "~=";
		case TokenType::And:                      return "&";
		case TokenType::AndAnd:                   return "&&";
		case TokenType::AndEq:                    return "&=";
		case TokenType::Or:                       return "|";
		case TokenType::OrOr:                     return "||";
		case TokenType::OrEq:                     return "|=";
		case TokenType::Caret:                    return "^";
		case TokenType::CaretEq:                  return "^=";
		case TokenType::Less:                     return "<";
		case TokenType::LessLess:                 return "<<";
		case TokenType::LessLessLess:             return "<<<";
		case TokenType::LessLessAsterisk:         return "<<*";
		case TokenType::LessEq:                   return "<=";
		case TokenType::LessLessEq:               return "<<=";
		case TokenType::LessLessLessEq:           return "<<<=";
		case TokenType::LessLessAsteriskEq:       return "<<*=";
		case TokenType::Greater:                  return ">";
		case TokenType::GreaterGreater:           return ">>";
		case TokenType::GreaterGreaterGreater:    return ">>>";
		case TokenType::GreaterGreaterAsterisk:   return ">>*";
		case TokenType::GreaterEq:                return ">=";
		case TokenType::GreaterGreaterEq:         return ">>=";
		case TokenType::GreaterGreaterGreaterEq:  return ">>>=";
		case TokenType::GreaterGreaterAsteriskEq: return ">>*=";
		case TokenType::Exclaim:                  return "!";
		case TokenType::ExclaimEq:                return "!=";
		case TokenType::ExclaimLess:              return "!<";
		case TokenType::LParen:                   return "(";
		case TokenType::RParen:                   return ")";
		case TokenType::LBrace:                   return "{";
		case TokenType::RBrace:                   return "}";
		case TokenType::LBracket:                 return "[";
		case TokenType::RBracket:                 return "]";
		case TokenType::Comma:                    return ",";
		case TokenType::Semicolon:                return ";";
		case TokenType::Colon:                    return ":";
		case TokenType::ColonColon:               return "::";
		case TokenType::ColonEq:                  return ":=";
		case TokenType::Dot:                      return ".";
		case TokenType::DotDot:                   return "..";
		case TokenType::DotDotDot:                return "...";
		case TokenType::DotDotEq:                 return "..=";
		case TokenType::At:                       return "@";
		case TokenType::AtColon:                  return "@:";
		case TokenType::Question:                 return "?";
		case TokenType::QuestionQuestion:         return "??";
		case TokenType::QuestionQuestionEq:       return "??=";
		case TokenType::QuestionColon:            return "?:";
		case TokenType::QuestionDot:              return "?.";
		case TokenType::QuestionBracket:          return "?[";
		case TokenType::DollarParen:              return "$(";
		case TokenType::DollarBracket:            return "$[";
		case TokenType::DollarBrace:              return "${";
			
		case TokenType::CharLit:                  return "CharLit";
		case TokenType::F16Lit:                   return "F16Lit";
		case TokenType::F32Lit:                   return "F32Lit";
		case TokenType::F64Lit:                   return "F64Lit";
		case TokenType::F128Lit:                  return "F128Lit";
		case TokenType::I8Lit:                    return "I8Lit";
		case TokenType::I16Lit:                   return "I16Lit";
		case TokenType::I32Lit:                   return "I32Lit";
		case TokenType::I64Lit:                   return "I64Lit";
		case TokenType::I128Lit:                  return "I128Lit";
		case TokenType::StringLit:                return "StringLit";
		case TokenType::U8Lit:                    return "U8Lit";
		case TokenType::U16Lit:                   return "U16Lit";
		case TokenType::U32Lit:                   return "U32Lit";
		case TokenType::U64Lit:                   return "U64Lit";
		case TokenType::U128Lit:                  return "U128Lit";

		case TokenType::Iden:                     return "Iden";
		case TokenType::MacroIden:                return "MacroIden";
		case TokenType::EoF:                      return "EoF";
		default:                                  return "__unknown__";
		}
	}

	bool IsTokenTypeSignedLiteral(TokenType type)
	{
		return u8(type) >= u8(TokenType::I8Lit) && u8(type) <= u8(TokenType::I128Lit);
	}

	bool IsTokenTypeUnsignedLiteral(TokenType type)
	{
		return u8(type) >= u8(TokenType::U8Lit) && u8(type) <= u8(TokenType::U128Lit);
	}

	bool IsTokenTypeFpLiteral(TokenType type)
	{
		return u8(type) >= u8(TokenType::F16Lit) && u8(type) <= u8(TokenType::F128Lit);
	}

	bool IsTreeStartTokenType(TokenType type)
	{
		switch (type)
		{
		case TokenType::LBrace:
		case TokenType::DollarBrace:
		case TokenType::LBracket:
		case TokenType::DollarBracket:
		case TokenType::LParen:
		case TokenType::DollarParen:
		case TokenType::ExclaimLess:
			return true;
		default:
			return false;
		}
	}

	Token::Token(TokenType type, u64 spanIdx)
		: type(type)
		, spanId(spanIdx)
		, sval(0)
	{
	}

	Token::Token(TokenType type, StdString text, u64 spanIdx)
		: type(type)
		, iden(std::move(text))
		, spanId(spanIdx)
		, sval(0)
		, bval(type == TokenType::True)
	{
	}

	Token::Token(TokenType type, i64 val, u64 spanIdx)
		: type(type)
		, spanId(spanIdx)
		, sval(val)
	{
	}

	Token::Token(TokenType type, u64 val, u64 spanIdx)
		: type(type)
		, spanId(spanIdx)
		, uval(val)
	{
	}

	Token::Token(TokenType type, f64 val, u64 spanIdx)
		: type(type)
		, spanId(spanIdx)
		, fval(val)
	{
	}

	bool Token::operator==(const Token& other) const
	{
		if (type != other.type)
			return false;

		switch (type)
		{
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
			return fval == other.fval;
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			return sval == other.sval;
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::CharLit:
			return uval == other.uval;
		case TokenType::StringLit:
		case TokenType::Iden:
		case TokenType::MacroIden:
			return iden == other.iden;
		default:
			return true;
		}
	}

	bool Token::operator!=(const Token& other) const
	{
		return !(*this == other);
	}

	TokenTree::TokenTree()
		: tok(TokenType::Unknown, u64(-1))
		, m_LocalIdx(0)
	{
	}

	TokenTree::TokenTree(const Token& tok)
		: tok(tok)
		, m_LocalIdx(0)
	{
	}

	TokenTree::TokenTree(StdVector<TokenTree>&& subToks)
		: tok(TokenType::Unknown, u64(-1))
		, subToks(std::move(subToks))
		, m_LocalIdx(0)
	{
	}

	void TokenTree::Append(const Token& tok)
	{
		subToks.push_back(TokenTree{ tok });
	}

	void TokenTree::Append(const TokenTree& subTree)
	{
		subToks.push_back(subTree);
	}

	void TokenTree::InsertAtCur(const TokenTree& subTree)
	{
		if (m_LocalIdx == subToks.size())
		{
			if (IsTreeStartTokenType(subTree.subToks[0].tok.type))
				subToks.push_back(subTree);
			else
				subToks.insert(subToks.end(), subTree.subToks.begin(), subTree.subToks.end());
			return;
		}

		if (subToks[m_LocalIdx].subToks.empty() ||
			subToks[m_LocalIdx].m_LocalIdx == 0)
		{
			if (subToks[m_LocalIdx].tok.type == TokenType::MacroIden)
				subToks.erase(subToks.begin() + m_LocalIdx);

			if (subTree.subToks.empty())
			{
				subToks.emplace(subToks.begin() + m_LocalIdx, subTree);
			}
			else
			{
				if (IsTreeStartTokenType(subTree.subToks[0].tok.type))
					subToks.emplace(subToks.begin() + m_LocalIdx, subTree);
				else
					subToks.insert(subToks.begin() + m_LocalIdx, subTree.subToks.begin(), subTree.subToks.end());
			}
			return;
		}

		subToks[m_LocalIdx].InsertAtCur(subTree);
	}

	Token& TokenTree::Peek(usize offset)
	{
		return PeekInternal(offset);
	}

	Token& TokenTree::Eat(TokenType type)
	{
		static Token empty{ TokenType::Unknown, u64(-1) };

		// If we reached the end, notify the caller we left this tree (should only happen in the base tree)
		if (m_LocalIdx >= subToks.size())
			return empty;

		// If we are at a token, just eat it and check it
		if (subToks[m_LocalIdx].subToks.empty())
		{
			// special case for greater, i.e. closing of generics
			if (type == TokenType::Greater)
			{
				Token tmp = Peek();
				TokenType cutType;
				switch (tmp.type)
				{
				case TokenType::GreaterEq: cutType = TokenType::Eq; break;
				case TokenType::GreaterGreater: cutType = TokenType::Greater; break;
				case TokenType::GreaterGreaterEq: cutType = TokenType::GreaterEq; break;
				case TokenType::GreaterGreaterAsterisk: cutType = TokenType::Asterisk; break;
				case TokenType::GreaterGreaterAsteriskEq: cutType = TokenType::AsteriskEq; break;
				case TokenType::GreaterGreaterGreater: cutType = TokenType::GreaterGreater; break;
				case TokenType::GreaterGreaterGreaterEq: cutType = TokenType::GreaterGreaterEq; break;
				default:
				{
					Token& tok = Peek();
					if (tmp.type != type)
					{
						Span span = g_SpanManager.GetSpan(tok.spanId);
						StdStringView found = GetTokenTypeName(tok.type);
						StdStringView expected = GetTokenTypeName(type);
						g_ErrorSystem.Error(span, "Unexpected token: found '%s', expected '%5'", found.data(), expected.data());
						return empty;
					}
					++m_LocalIdx;
					return tok;
				}
				}

				++m_LocalIdx;
				Span tmpSpan = g_SpanManager.GetSpan(tmp.spanId);
				if (cutType == TokenType::Asterisk ||
					cutType == TokenType::AsteriskEq)
				{
					u64 spanId = g_SpanManager.AddSpan(StdString(tmpSpan.filePath), { tmpSpan.startChar + 1, tmpSpan.startChar + 2, tmpSpan.line, tmpSpan.column + 1 });
					subToks.emplace(subToks.begin() + m_LocalIdx, Token{ TokenType::Greater, spanId });

					spanId = g_SpanManager.AddSpan(StdString(tmpSpan.filePath), { tmpSpan.startChar + 2, tmpSpan.endChar, tmpSpan.line, tmpSpan.column + 1 });
					subToks.emplace(subToks.begin() + m_LocalIdx, Token{ cutType, spanId });
				}
				else
				{
					u64 spanId = g_SpanManager.AddSpan(StdString(tmpSpan.filePath), { tmpSpan.startChar + 2, tmpSpan.endChar, tmpSpan.line, tmpSpan.column + 1 });
					subToks.emplace(subToks.begin() + m_LocalIdx, Token{ cutType, spanId });
				}
				return subToks[m_LocalIdx - 1].tok;
			}
			else
			{
				Token& tok = subToks[m_LocalIdx].tok;
				++m_LocalIdx;

				if (type != TokenType::Skip && tok.type != type)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					StdStringView found = GetTokenTypeName(tok.type);
					StdStringView expected = GetTokenTypeName(type);
					g_ErrorSystem.Error(span, "Unexpected token: found '%s', expected '%5'", found.data(), expected.data());
				}

				// If we just ate the last token, notify the caller we now left the subtree
				return tok;
			}
		}

		// Otherwise, we need to eat a token in the subtree
		// If the subtree is at the been exited, we also need to make sure that the next EatInternal call will use the next token/subtree in this tree
		Token& ret = subToks[m_LocalIdx].Eat(type);
		if (subToks[m_LocalIdx].IsExhausted())
			++m_LocalIdx;
		return ret;
	}

	bool TokenTree::TryEat(TokenType type)
	{
		Token& tok = Peek();
		if (tok.type != type)
			return false;
		Eat(type);
		return true;
	}

	Token& TokenTree::EatIden(const StdString& iden)
	{
		Token& tok = Peek();
		if (tok.type == TokenType::Iden &&
			tok.iden == iden)
		{
			Eat(TokenType::Iden);
			return tok;
		}

		Span span = g_SpanManager.GetSpan(tok.spanId);
		StdString found = tok.type == TokenType::Iden ? tok.iden : StdString(GetTokenTypeName(tok.type));
		g_ErrorSystem.Error(span, "Unexpected token: found '%s', expected '%5'", found.c_str(), iden.c_str());

		static Token empty{ TokenType::Unknown, u64(-1) };
		return empty;
	}

	bool TokenTree::TryEatIden(const StdString& iden)
	{
		Token& tok = Peek();
		if (tok.type != TokenType::Iden ||
			tok.iden != iden)
			return false;
		Eat(TokenType::Iden);
		return true;
	}

	TokenTree& TokenTree::GetSubTree(bool isBase)
	{
		static TokenTree empty{ Token { TokenType::Unknown, u64(-1) } };
		if (m_LocalIdx >= subToks.size())
			return empty;

		if (!isBase && m_LocalIdx == 0)
			return *this;

		if (subToks[m_LocalIdx].subToks.empty())
			return subToks[m_LocalIdx++];
		
		return subToks[m_LocalIdx++].GetSubTree(false);
	}

	void TokenTree::ResetIdx()
	{
		m_LocalIdx = 0;
		for (TokenTree& subTree : subToks)
		{
			subTree.ResetIdx();
		}
	}

	void TokenTree::ToToks(StdVector<Token>& toks)
	{
		if (subToks.empty())
		{
			toks.push_back(tok);
			return;
		}

		for (TokenTree& subTok : subToks)
		{
			if (subTok.subToks.empty())
				toks.push_back(subTok.tok);
			else
				subTok.ToToks(toks);
		}
	}

	void TokenTree::LogTokens(usize indent) const
	{
		for (usize i = 0; i < subToks.size(); ++i)
		{
			const TokenTree& subTree = subToks[i];

			if (!subTree.subToks.empty())
			{
				subTree.LogTokens(indent + 1);
				continue;
			}

			for (usize j = 0; j < indent; ++j)
			{
				g_Logger.Log("|   ");
			}
			
			const Token& tok = subTree.tok;
			std::stringstream ss;

			Span span = g_SpanManager.GetSpan(tok.spanId);

			ss << '[' << span.line << ':' << span.column << ']';
			ss << ", ";
			ss << GetTokenTypeName(tok.type);

			if (tok.type == TokenType::Iden ||
				tok.type == TokenType::MacroIden)
			{
				ss << ", ";
				ss << tok.iden;
			}

			if (IsTokenTypeSignedLiteral(tok.type))
				ss << ", " << tok.sval;
			if (IsTokenTypeUnsignedLiteral(tok.type))
				ss << ", " << tok.uval;
			if (IsTokenTypeFpLiteral(tok.type))
				ss << ", " << tok.fval;
			if (tok.type == TokenType::CharLit)
				ss << ", " << tok.sval;
			if (tok.type == TokenType::True || tok.type == TokenType::False)
				ss << ", " << tok.bval;

			ss << "\n";

			g_Logger.Log(ss.str());
		}
	}

	Token& TokenTree::PeekInternal(usize& offset)
	{
		static Token empty{ TokenType::Unknown, u64(-1) };
		if (subToks.empty())
			return tok;
		
		if (m_LocalIdx >= subToks.size())
			return empty;

		usize idx = m_LocalIdx;
		while (idx < subToks.size())
		{
			Token& tmp = subToks[idx].PeekInternal(offset);
			if (offset == 0)
				return tmp;
			++idx;
			--offset;
		}
		return empty;
	}
}
