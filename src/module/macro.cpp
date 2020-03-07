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

	void MacroExtractedElem::PreParse(Context* pCtx)
	{
		switch (elemKind)
		{
		case MacroPatternKind::Tok: break;
		case MacroPatternKind::Var:
		{
			StdVector<Token> toks;
			tokTree.ToToks(toks);
			Parser parser{ toks, pCtx };
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
				if (toks.size() != 1)
				{
					Span span = pCtx->spanManager.GetSpan(toks[0].Idx());
					g_ErrorSystem.Error(span, "expected a single identifier");
					break;
				}
				
				actIden = toks[0].Text();
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
				subElem.PreParse(pCtx);
			}
			break;
		}
		case MacroPatternKind::FragmentIt:
		{
			for (MacroExtractedElem& subElem : subElems)
			{
				subElem.PreParse(pCtx);
			}
			break;
		}
			
		default:;
		}
	}

	bool DeclMacro::MatchPatternAndExtract(TokenTree& toks, StdVector<MacroExtractedElem>& extracted)
	{
		usize tokIdx = 0;
		for (usize i = 0; i < pattern.size(); ++i)
		{
			MacroPatternElem& elem = *pattern[i];
			if (tokIdx >= toks.subToks.size())
				return false;

			switch (elem.kind)
			{
			case MacroPatternKind::Tok:
			{
				if (elem.tok != toks.subToks[tokIdx].tok)
					return false;
				++tokIdx;
				break;
			}
			case MacroPatternKind::Var:
			{
				if (i + 1 == pattern.size())
				{
					TokenTree tokTree{ toks.subToks.begin() + tokIdx, toks.subToks.end() };
					extracted.push_back(MacroExtractedElem{ elem.var.first, tokTree, elem.var.second });
					return true;
				}

				MacroPatternElem& next = *pattern[i + 1];
				usize startIdx = tokIdx;
				while (tokIdx < toks.subToks.size() && toks.subToks[tokIdx].tok != next.tok)
					++tokIdx;

				TokenTree tokTree{ toks.subToks.begin() + startIdx, toks.subToks.begin() + tokIdx };
				extracted.push_back(MacroExtractedElem{ elem.var.first, tokTree, elem.var.second });
				break;
			}
			case MacroPatternKind::Fragment:
			{
				if (!MatchFragmentAndExtract(elem.fragment, toks, tokIdx, extracted))
					return false;
				break;
			}
			default:;
			}
		}

		return true;
	}

	bool DeclMacro::MatchFragmentAndExtract(const MacroFragment& fragment, TokenTree& toks, usize& tokIdx,
		StdVector<MacroExtractedElem>& extracted)
	{
		StdVector<MacroExtractedElem> tmpExtracted;
		StdVector<MacroExtractedElem> fragmentItContent;
		bool res = true;
		bool firstPass = true;

		do
		{
			if (!firstPass && fragment.repTok.Type() != TokenType::Unknown)
				++tokIdx;

			usize startIdx = tokIdx;
			for (usize i = 0; i < fragment.elems.size(); ++i)
			{
				MacroPatternElem& elem = *fragment.elems[i];
				if (tokIdx >= toks.subToks.size())
				{
					res = false;
					break;
				}

				switch (elem.kind)
				{
				case MacroPatternKind::Tok:
				{
					if (elem.tok != toks.subToks[tokIdx].tok)
						res = false;

					++tokIdx;
					break;
				}
				case MacroPatternKind::Var:
				{
					if (i + 1 == fragment.elems.size())
					{
						fragmentItContent.push_back(MacroExtractedElem{ elem.var.first, toks.subToks[tokIdx], elem.var.second });
						res = true;
						++tokIdx;
						break;
					}

					MacroPatternElem& next = *fragment.elems[i + 1];
					while (tokIdx < toks.subToks.size() && toks.subToks[tokIdx].tok != next.tok)
						++tokIdx;

					TokenTree tokTree{ toks.subToks.begin() + startIdx, toks.subToks.begin() + tokIdx };
					fragmentItContent.push_back(MacroExtractedElem{ elem.var.first, tokTree, elem.var.second });
					break;
				}
				case MacroPatternKind::Fragment:
				default:;
				}

				if (!res)
				{
					tokIdx = startIdx;
					break;
				}
			}

			if (!res)
			{
				if (fragment.rep == MacroFragmentRep::Opt && firstPass ||
					fragment.rep == MacroFragmentRep::OneOrMore && !firstPass ||
					fragment.rep == MacroFragmentRep::Any)
					res = true;
				break;
			}

			tmpExtracted.push_back(MacroExtractedElem{ fragmentItContent });
			fragmentItContent.clear();

			firstPass = false;
		}
		while (tokIdx < toks.subToks.size() && 
			(toks.subToks[tokIdx].tok == fragment.repTok || fragment.repTok.Type() == TokenType::Unknown));

		if (res)
		{
			extracted.push_back(MacroExtractedElem{ tmpExtracted, fragment.rep });
		}

		return res || !fragmentItContent.empty();
	}

	bool MacroContext::AddMacro(QualNameSPtr scope, IdenSPtr iden, AstDeclMacroSPtr astMacro)
	{
		DeclMacro macro;
		macro.iden = iden;
		macro.pattern = GeneratePattern(astMacro->pattern);
		macro.body = astMacro->body;

		StdVector<IdenSPtr> scopeIdens = scope->AllIdens();
		return AddMacro(scopeIdens, macro);
	}

	StdVector<DeclMacro> MacroContext::GetDeclMacros(QualNameSPtr curScope, QualNameSPtr qualName)
	{
		StdVector<IdenSPtr> curScopeIdens = curScope->AllIdens();
		StdVector<IdenSPtr> qualNameIdens = qualName->AllIdens();
		return GetDeclMacros(curScopeIdens, qualNameIdens);
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

	bool MacroContext::AddMacro(const StdVector<IdenSPtr>& scopeIdens, DeclMacro& macro, usize startIdx)
	{
		if (startIdx == scopeIdens.size())
		{
			for (DeclMacro& declMacro : m_DeclMacros)
			{
				if (DoPatternsMatch(macro.pattern, declMacro.pattern))
					return false;
			}

			m_DeclMacros.push_back(macro);
			return true;
		}
		
		auto it = m_SubCtxs.find(scopeIdens[startIdx]);
		if (it == m_SubCtxs.end())
		{
			it = m_SubCtxs.insert(std::pair{ scopeIdens[startIdx], MacroContext{} }).first;
		}
		return it->second.AddMacro(scopeIdens, macro, startIdx + 1);
	}

	StdVector<DeclMacro> MacroContext::GetDeclMacros(const StdVector<IdenSPtr>& curScopeIdens, 
		const StdVector<IdenSPtr>& idens, usize startIdx)
	{
		if (startIdx != curScopeIdens.size())
		{
			auto it = m_SubCtxs.find(curScopeIdens[startIdx]);
			if (it != m_SubCtxs.end())
			{
				StdVector<DeclMacro> macros = it->second.GetDeclMacros(curScopeIdens, idens, startIdx + 1);
				if (!macros.empty())
					return macros;
			}
		}

		MacroContext* pCtx = this;
		for (usize i = 0; i < idens.size() - 1; ++i)
		{
			auto it = m_SubCtxs.find(idens[i]);
			if (it == m_SubCtxs.end())
				return StdVector<DeclMacro>{};
			pCtx = &it->second;
		}

		StdVector<DeclMacro> declMacros;
		for (DeclMacro& declMacro : pCtx->m_DeclMacros)
		{
			if (declMacro.iden == idens.back())
				declMacros.push_back(declMacro);
		}
		return declMacros;
	}

	MacroVarSolver::MacroVarSolver(StdVector<MacroExtractedElem>&& extracted, Context* pCtx)
		: m_ExtractedElems(std::move(extracted))
		, m_pCtx(pCtx)
		, m_EnteredLoop(false)
		, m_LoopIdx(0)
		, m_MaxLoopIdx(0)
	{
	}

	void MacroVarSolver::PreparseMacroVars()
	{
		for (MacroExtractedElem& elem : m_ExtractedElems)
		{
			elem.PreParse(m_pCtx);
		}
	}

	void MacroVarSolver::CollectMacroVarsForParsing()
	{
		for (MacroExtractedElem& elem : m_ExtractedElems)
		{
			if (elem.elemKind == MacroPatternKind::Fragment)
				CollectFragmentVars(elem);
			else
				m_CollectedVars.insert(std::pair{ elem.varIden, StdVector<MacroExtractedElem>{ elem } });
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
				u64 expected = m_MaxLoopIdx;
				u64 found = u64(it->second.size());
				g_ErrorSystem.Error("'%s' only appears %U times, while the other variables only loop %u times", pName);
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
			if (macorVar.second.size() > 1 && macorVar.second[0].varKind == MacroVarKind::Toks)
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
					varsIt = m_CollectedVars.insert(std::pair{ elem.varIden, StdVector<MacroExtractedElem>{} }).first;
				}
				varsIt->second.push_back(elem);
			}
		}
	}

}
