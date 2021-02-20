#pragma once
#include "common/defs.hpp"
#include "tokens/token.hpp"
#include "ast/ast.hpp"

namespace Noctis
{
	struct Context;

	FWDECL_CLASS_SPTR(QualName);
	FWDECL_STRUCT_SPTR(AstDeclMacro);

	enum class MacroPatternKind : u8
	{
		Tok,
		Var,
		Fragment,
		FragmentIt
	};

	enum class MacroVarKind : u8
	{
		Stmt,
		Expr,
		Type,
		Qual,
		Iden,
		Attr,
		Toks,
		Patr,
	};

	enum class MacroFragmentRep : u8
	{
		Opt,
		Any,
		OneOrMore
	};

	FWDECL_STRUCT_SPTR(MacroPatternElem);
	struct MacroFragment
	{
		MacroFragment()
			: repTok(TokenType::Unknown, u64(-1))
			, rep(MacroFragmentRep::Any)
		{}
		
		MacroFragment(StdVector<MacroPatternElemSPtr>&& elems, Token repTok, MacroFragmentRep rep)
			: elems(std::move(elems))
			, repTok(repTok)
			, rep(rep)
		{}
		
		StdVector<MacroPatternElemSPtr> elems;
		Token repTok;
		MacroFragmentRep rep;
	};

	struct MacroPatternElem
	{
		MacroPatternElem(Token& tok);
		MacroPatternElem(const StdString& iden, MacroVarKind varKind);
		MacroPatternElem(MacroFragment&& fragment);
		
		MacroPatternKind kind;
		union
		{
			Token tok;
			StdPair<StdString, MacroVarKind> var;
			MacroFragment fragment;
		};

		~MacroPatternElem();
		
	};

	struct MacroExtractedElem
	{
		MacroExtractedElem()
			: elemKind(MacroPatternKind::Tok)
			, repKind(MacroFragmentRep::Any)
		{}
		
		MacroExtractedElem(const StdVector<MacroExtractedElem>& subElems, MacroFragmentRep rep)
			: subElems(subElems)
			, elemKind(MacroPatternKind::Fragment)
			, repKind(rep)
		{}

		MacroExtractedElem(const StdVector<MacroExtractedElem>& subElems)
			: subElems(subElems)
			, elemKind(MacroPatternKind::FragmentIt)
			, repKind(MacroFragmentRep::Any)
		{}
		
		MacroExtractedElem(const StdString& varIden, TokenTree& toks, MacroVarKind varKind)
			: varIden(varIden)
			, tokTree(toks)
			, elemKind(MacroPatternKind::Var)
			, varKind(varKind)
		{
		}

		void Parse(Context* pCtx);

		// NOTE: When writing this in noct, make sure to copy nodes, instead of reparsing them to save time
		//       Currently not done to not over-complicate the ast node code
		AstQualNameSPtr GetQualName() { return *reinterpret_cast<AstQualNameSPtr*>(&node); }
		AstTypeSPtr GetType() { return *reinterpret_cast<AstTypeSPtr*>(&node); }
		AstPatternSPtr GetPattern() { return *reinterpret_cast<AstPatternSPtr*>(&node); }
		AstExprSPtr GetExpr() { return *reinterpret_cast<AstExprSPtr*>(&node); }
		
		TokenTree tokTree;
		StdVector<MacroExtractedElem> subElems;
		StdString varIden;

		MacroPatternKind elemKind;
		union
		{
			MacroVarKind varKind;
			MacroFragmentRep repKind;
		};
		StdSharedPtr<void> node;
		StdString actIden;
	};

	struct DeclMacro
	{
		StdString iden;
		StdVector<MacroPatternElemSPtr> pattern;
		TokenTree body;

		bool MatchPatternAndExtract(TokenTree& toks, StdVector<MacroExtractedElem>& extracted);
		bool MatchFragmentAndExtract(const MacroFragment& fragment, TokenTree& toks, usize& tokIdx,
			StdVector<MacroExtractedElem>& extracted);
	};

	class MacroContext
	{
	public:
		bool AddMacro(QualNameSPtr scope, const StdString& iden, AstDeclMacroSPtr astMacro);

		StdVector<DeclMacro> GetDeclMacros(QualNameSPtr curScope, QualNameSPtr qualName);

	private:
		StdVector<MacroPatternElemSPtr> GeneratePattern(AstMacroPatternSPtr astPattern);
		StdVector<MacroPatternElemSPtr> GeneratePattern(AstMacroFragment& astfragment);
		bool DoPatternsMatch(StdVector<MacroPatternElemSPtr>& m0, StdVector<MacroPatternElemSPtr>& m1);
		
		bool AddMacro(QualNameSPtr scope, DeclMacro& astMacro,
			usize startIdx = 0);

		StdVector<DeclMacro> GetDeclMacros(QualNameSPtr curScope, QualNameSPtr qualName, usize startIdx);

		StdUnorderedMap<StdString, MacroContext> m_SubCtxs;
		StdVector<DeclMacro> m_DeclMacros;
	};

	class MacroVarSolver
	{
	public:
		MacroVarSolver(StdVector<MacroExtractedElem>&& extracted, Context* pCtx);
		
		void PreparseMacroVars();
		void CollectMacroVarsForParsing();

		void EnterMacroLoop();
		bool NextLoopIt();
		void LeaveMacroLoop();

		TokenTree GetTokenTree(const StdString& name);
		MacroExtractedElem& GetElem(const StdString& name);

		bool HasLoopedToks();
	private:
		void CollectFragmentVars(MacroExtractedElem& fragment);

		
		Context* m_pCtx;
		StdVector<MacroExtractedElem> m_ExtractedElems;
		StdUnorderedMap<StdString, StdVector<MacroExtractedElem>> m_CollectedVars;

		bool m_EnteredLoop;
		u64 m_LoopIdx;
		u64 m_MaxLoopIdx;
	};
	
}
