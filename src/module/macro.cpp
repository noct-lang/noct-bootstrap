#include "macro.hpp"
#include "ast/ast.hpp"
#include "ast/parser.hpp"
#include "common/errorsystem.hpp"
#include "common/context.hpp"
#include "tokens/token.hpp"

namespace Noctis
{
	MacroPatternElem::MacroPatternElem(Token& tok)
		: kind(MacroPatternKind::Tok)
		, tok(tok)
	{
	}

	MacroPatternElem::MacroPatternElem(const StdString& iden, MacroVarKind varKind)
		: kind(MacroPatternKind::Var)
		, var(iden, varKind)
		, tok(TokenType::Unknown, u64(-1))
	{
	}

	MacroPatternElem::MacroPatternElem(MacroFragment&& fragment)
		: kind(MacroPatternKind::Fragment)
		, fragment(fragment)
	{
	}

	MacroPatternElem::~MacroPatternElem()
	{
		switch (kind)
		{
		case MacroPatternKind::Var:
			var.first.~basic_string();
			break;
		case MacroPatternKind::Fragment:
			fragment.~MacroFragment();
			break;
		case MacroPatternKind::Tok:
		default: ;
		}
	}

	void MacroExtractedElem::Parse()
	{
		switch (elemKind)
		{
		case MacroPatternKind::Tok: break;
		case MacroPatternKind::Var:
		{
			Parser parser{ tokTree };
			switch (varKind)
			{
			case MacroVarKind::Stmt:
			{
				AstStmtSPtr stmt = parser.ParseStatement();
				node = stmt;
				break;
			}
			case MacroVarKind::Expr:
			{
				AstExprSPtr expr = parser.ParseExpression();
				node = expr;
				break;
			}
			case MacroVarKind::Type:
			{
				AstTypeSPtr type = parser.ParseType();
				node = type;
				break;
			}
			case MacroVarKind::Qual:
			{
				AstQualNameSPtr qualName = parser.ParseQualName(false);
				node = qualName;
				break;
			}
			case MacroVarKind::Iden:
			{
				if (!tokTree.subToks.empty())
				{
					Span span = g_SpanManager.GetSpan(tokTree.subToks[0].tok.spanId);
					g_ErrorSystem.Error(span, "expected a single identifier");
					break;
				}
				
				actIden = tokTree.tok.iden;
				break;
			}
			case MacroVarKind::Attr:
			{
				AstAttribsSPtr attribs = parser.ParseAttributes();
				node = attribs;
				break;
			}
			case MacroVarKind::Toks:
				break;
			case MacroVarKind::Patr:
			{
				AstPatternSPtr pattern = parser.ParsePattern();
				node = pattern;
				break;
			}
			default:;
			}

			if (varKind != MacroVarKind::Toks &&
				varKind != MacroVarKind::Iden &&
				!parser.HasParsedAllTokens())
			{
				// TODO: span
				g_ErrorSystem.Error("Failed to parse macro variable");
			}

			break;
		}
		case MacroPatternKind::Fragment:
		{
			for (MacroExtractedElem& subElem : subElems)
			{
				subElem.Parse();
			}
			break;
		}
		case MacroPatternKind::FragmentIt:
		{
			for (MacroExtractedElem& subElem : subElems)
			{
				subElem.Parse();
			}
			break;
		}
			
		default:;
		}
	}

	bool DeclMacro::MatchPatternAndExtract(TokenTree& toks, StdVector<MacroExtractedElem>& extracted)
	{
		for (usize i = 0; i < pattern.size(); ++i)
		{
			MacroPatternElem& elem = *pattern[i];
			if (toks.IsExhausted())
				return false;

			switch (elem.kind)
			{
			case MacroPatternKind::Tok:
			{
				if (elem.tok != toks.Peek())
					return false;
				toks.Eat();
				break;
			}
			case MacroPatternKind::Var:
			{
				if (i + 1 == pattern.size())
					ExtractVariable(elem.var.first, elem.var.second, toks, TokenType::Unknown, extracted);
				else
					ExtractVariable(elem.var.first, elem.var.second, toks, pattern[i + 1]->tok.type, extracted);
				break;
			}
			case MacroPatternKind::Fragment:
			{
				if (!MatchFragmentAndExtract(elem.fragment, toks, extracted))
					return false;
				break;
			}
			default:;
			}
		}

		return true;
	}

	bool DeclMacro::MatchFragmentAndExtract(const MacroFragment& fragment, TokenTree& toks,
		StdVector<MacroExtractedElem>& extracted)
	{
		StdVector<MacroExtractedElem> tmpExtracted;
		StdVector<MacroExtractedElem> fragmentItContent;
		bool res = true;
		bool validRep = false;
		bool firstPass = true;

		do
		{
			for (usize i = 0; i < fragment.elems.size(); ++i)
			{
				MacroPatternElem& elem = *fragment.elems[i];

				switch (elem.kind)
				{
				case MacroPatternKind::Tok:
				{
					if (elem.tok != toks.Peek())
						return false;
					toks.Eat();
					break;
				}
				case MacroPatternKind::Var:
				{
					if (i + 1 == fragment.elems.size())
						res = true;

					ExtractVariable(elem.var.first, elem.var.second, toks, fragment.repTok.type, fragmentItContent);
					break;
				}
				case MacroPatternKind::Fragment:
				default:;
				}

				if (!res)
					break;
			}

			if (!res)
			{
				if (fragment.rep == MacroFragmentRep::Opt && firstPass ||
					fragment.rep == MacroFragmentRep::OneOrMore && !firstPass ||
					fragment.rep == MacroFragmentRep::Any)
					res = true;
				break;
			}

			validRep = toks.TryEat(fragment.repTok.type);
			tmpExtracted.push_back(MacroExtractedElem{ fragmentItContent });
			fragmentItContent.clear();

			firstPass = false;
		}
		while (!toks.IsExhausted() && 
			   (validRep || fragment.repTok.type == TokenType::Unknown));

		if (res)
			extracted.push_back(MacroExtractedElem{ tmpExtracted, fragment.rep });

		return res || !fragmentItContent.empty();
	}

	void DeclMacro::ExtractVariable(const StdString& iden, MacroVarKind varKind, TokenTree& toks, TokenType separator, StdVector<MacroExtractedElem>& extracted)
	{
		TokenTree tokTree;
		while (!toks.IsExhausted() && 
			   (separator == TokenType::Unknown ||
			    toks.Peek().type != separator))
		{
			tokTree.Append(toks.GetSubTree());
		}

		if (tokTree.subToks.size() == 1)
			tokTree = tokTree.subToks[0];
		
		extracted.emplace_back(iden, tokTree, varKind);
	}

	bool MacroContext::AddMacro(QualNameSPtr scope, const StdString& iden, AstDeclMacroSPtr astMacro)
	{
		DeclMacro macro;
		macro.iden = iden;
		macro.pattern = GeneratePattern(astMacro->pattern);
		macro.body = astMacro->body;

		return AddMacro(scope, macro);
	}

	StdVector<DeclMacro> MacroContext::GetDeclMacros(QualNameSPtr curScope, QualNameSPtr qualName)
	{
		return GetDeclMacros(curScope, qualName, 0);
	}

	StdVector<MacroPatternElemSPtr> MacroContext::GeneratePattern(AstMacroPatternSPtr astPattern)
	{
		StdVector<MacroPatternElemSPtr> pattern;
		for (AstMacroPatternElemSPtr astElem : astPattern->elems)
		{
			switch (astElem->elemKind)
			{
			case AstMacroPatternElemKind::Variable:
			{
				AstMacroVar* astVar = static_cast<AstMacroVar*>(astElem.get());
				MacroVarKind kind;
				switch (astVar->kind)
				{
				case AstMacroVarKind::Expr: kind = MacroVarKind::Expr; break;
				case AstMacroVarKind::Type: kind = MacroVarKind::Type; break;
				case AstMacroVarKind::Qual: kind = MacroVarKind::Qual; break;
				case AstMacroVarKind::Iden: kind = MacroVarKind::Iden; break;
				case AstMacroVarKind::Attr: kind = MacroVarKind::Attr; break;
				case AstMacroVarKind::Toks: kind = MacroVarKind::Toks; break;
				case AstMacroVarKind::Patr: kind = MacroVarKind::Patr; break;
				case AstMacroVarKind::Stmt:
				default: kind = MacroVarKind::Stmt;
				}

				pattern.emplace_back(new MacroPatternElem{ astVar->iden, kind });
				break;
			}
			case AstMacroPatternElemKind::Separator:
			{
				AstMacroSeparator* pSep = static_cast<AstMacroSeparator*>(astElem.get());
				for (Token& tok : pSep->toks)
				{
					pattern.emplace_back(new MacroPatternElem{ tok });
				}
				break;
			}
			case AstMacroPatternElemKind::Fragment:
			{
				AstMacroFragment* pFrag = static_cast<AstMacroFragment*>(astElem.get());
				StdVector<MacroPatternElemSPtr> fragment = GeneratePattern(*pFrag);

				MacroFragmentRep rep;
				switch (pFrag->repType)
				{
				case TokenType::Question: rep = MacroFragmentRep::Opt; break;
				case TokenType::Plus: rep = MacroFragmentRep::OneOrMore; break;
				default: rep = MacroFragmentRep::Any; break;
				}
				
				pattern.emplace_back(new MacroPatternElem{ MacroFragment{ std::move(fragment), pFrag->repTok, rep } });
				break;
			}
			default: ;
			}
		}
		return pattern;
	}

	StdVector<MacroPatternElemSPtr> MacroContext::GeneratePattern(AstMacroFragment& astfragment)
	{
		StdVector<MacroPatternElemSPtr> pattern;
		for (AstMacroPatternElemSPtr astElem : astfragment.subPattern->elems)
		{
			switch (astElem->elemKind)
			{
			case AstMacroPatternElemKind::Variable:
			{
				AstMacroVar* astVar = static_cast<AstMacroVar*>(astElem.get());
				MacroVarKind kind;
				switch (astVar->kind)
				{
				case AstMacroVarKind::Expr: kind = MacroVarKind::Expr; break;
				case AstMacroVarKind::Type: kind = MacroVarKind::Type; break;
				case AstMacroVarKind::Qual: kind = MacroVarKind::Qual; break;
				case AstMacroVarKind::Iden: kind = MacroVarKind::Iden; break;
				case AstMacroVarKind::Attr: kind = MacroVarKind::Attr; break;
				case AstMacroVarKind::Toks: kind = MacroVarKind::Toks; break;
				case AstMacroVarKind::Patr: kind = MacroVarKind::Patr; break;
				case AstMacroVarKind::Stmt:
				default: kind = MacroVarKind::Stmt;
				}

				pattern.emplace_back(new MacroPatternElem{ astVar->iden, kind });
				break;
			}
			case AstMacroPatternElemKind::Separator:
			{
				AstMacroSeparator* pSep = static_cast<AstMacroSeparator*>(astElem.get());
				for (Token& tok : pSep->toks)
				{
					pattern.emplace_back(new MacroPatternElem{ tok });
				}
				break;
			}
			case AstMacroPatternElemKind::Fragment:
			default:;
			}
		}
		return pattern;
	}

	bool MacroContext::DoPatternsMatch(StdVector<MacroPatternElemSPtr>& m0, StdVector<MacroPatternElemSPtr>& m1)
	{
		if (m0.size() != m1.size())
			return false;

		for (usize i = 0; i < m0.size(); ++i)
		{
			MacroPatternElem& elem0 = *m0[i];
			MacroPatternElem& elem1 = *m1[i];

			if (elem0.kind != elem1.kind)
				return false;

			switch (elem0.kind)
			{
			case MacroPatternKind::Tok:
			{
				if (elem0.tok != elem1.tok)
					return false;
				break;
			}
			case MacroPatternKind::Var:
				break;
			case MacroPatternKind::Fragment:
				if (!DoPatternsMatch(m0[i]->fragment.elems, m1[i]->fragment.elems))
					return false;
				break;
			default: ;
			}
		}

		return true;
	}

	bool MacroContext::AddMacro(QualNameSPtr scope, DeclMacro& macro, usize startIdx)
	{
		if (startIdx == scope->Idens().size())
		{
			for (DeclMacro& declMacro : m_DeclMacros)
			{
				if (declMacro.iden == macro.iden &&
					DoPatternsMatch(macro.pattern, declMacro.pattern))
					return false;
			}

			m_DeclMacros.push_back(macro);
			return true;
		}
		
		auto it = m_SubCtxs.find(scope->Idens()[startIdx]);
		if (it == m_SubCtxs.end())
		{
			it = m_SubCtxs.insert(std::pair{ scope->Idens()[startIdx], MacroContext{} }).first;
		}
		return it->second.AddMacro(scope, macro, startIdx + 1);
	}

	StdVector<DeclMacro> MacroContext::GetDeclMacros(QualNameSPtr curScope, QualNameSPtr qualName, usize startIdx)
	{
		if (startIdx != curScope->Idens().size())
		{
			auto it = m_SubCtxs.find(curScope->Idens()[startIdx]);
			if (it != m_SubCtxs.end())
			{
				StdVector<DeclMacro> macros = it->second.GetDeclMacros(curScope, qualName, startIdx + 1);
				if (!macros.empty())
					return macros;
			}
		}

		MacroContext* pCtx = this;
		for (usize i = 0; i < qualName->Idens().size() - 1; ++i)
		{
			auto it = m_SubCtxs.find(qualName->Idens()[i]);
			if (it == m_SubCtxs.end())
				return StdVector<DeclMacro>{};
			pCtx = &it->second;
		}

		StdVector<DeclMacro> declMacros;
		for (DeclMacro& declMacro : pCtx->m_DeclMacros)
		{
			if (declMacro.iden == qualName->Idens().back())
				declMacros.push_back(declMacro);
		}
		return declMacros;
	}

	MacroVarSolver::MacroVarSolver(StdVector<MacroExtractedElem>&& extracted)
		: m_ExtractedElems(std::move(extracted))
		, m_EnteredLoop(false)
		, m_LoopIdx(0)
		, m_MaxLoopIdx(0)
	{
	}

	void MacroVarSolver::PreparseMacroVars()
	{
		for (MacroExtractedElem& elem : m_ExtractedElems)
		{
			elem.Parse();
		}
	}

	void MacroVarSolver::CollectMacroVarsForParsing()
	{
		for (MacroExtractedElem& elem : m_ExtractedElems)
		{
			if (elem.elemKind == MacroPatternKind::Fragment)
				CollectFragmentVars(elem);
			else
				m_CollectedVars.try_emplace(elem.varIden, StdVector<MacroExtractedElem>{ elem });
		}
	}

	void MacroVarSolver::EnterMacroLoop()
	{
		m_EnteredLoop = true;
	}

	bool MacroVarSolver::NextLoopIt()
	{
		++m_LoopIdx;
		return m_LoopIdx < m_MaxLoopIdx;
	}

	void MacroVarSolver::LeaveMacroLoop()
	{
		m_MaxLoopIdx = m_LoopIdx = 0;
	}

	TokenTree MacroVarSolver::GetTokenTree(const StdString& name)
	{
		MacroExtractedElem elem = GetElem(name);
		if (elem.varKind != MacroVarKind::Toks)
			return TokenTree{};

		return elem.tokTree;
	}

	MacroExtractedElem& MacroVarSolver::GetElem(const StdString& name)
	{
		static MacroExtractedElem invalid;
		
		auto it = m_CollectedVars.find(name);
		if (it == m_CollectedVars.end())
		{
			const char* pName = name.c_str();
			g_ErrorSystem.Error("No macro variable named '%s' exists", pName);
			return invalid;
		}

		if (it->second.size() > 1)
		{
			if (m_EnteredLoop)
			{
				m_EnteredLoop = false;
				m_MaxLoopIdx = u64(it->second.size());
				return it->second[0];
			}

			if (m_MaxLoopIdx != it->second.size())
			{
				const char* pName = name.c_str();
				u64 expected = m_MaxLoopIdx + 1;
				u64 found = u64(it->second.size());
				g_ErrorSystem.Error("'%s' only appears %u times, while it's expected to appear %u times", pName, expected, found);
				return invalid;
			}

			return it->second[m_LoopIdx];
		}

		return it->second[0];
	}

	bool MacroVarSolver::HasLoopedToks()
	{
		for (StdPair<StdString, StdVector<MacroExtractedElem>> macorVar : m_CollectedVars)
		{
			if (macorVar.second.size() > 1)
				return true;
		}
		return false;
	}

	void MacroVarSolver::CollectFragmentVars(MacroExtractedElem& fragment)
	{
		for (MacroExtractedElem& it  : fragment.subElems)
		{
			for (MacroExtractedElem& elem : it.subElems)
			{
				auto varsIt = m_CollectedVars.find(elem.varIden);
				if (varsIt == m_CollectedVars.end())
				{
					varsIt = m_CollectedVars.try_emplace(elem.varIden, StdVector<MacroExtractedElem>{}).first;
				}
				varsIt->second.push_back(elem);
			}
		}
	}

}
