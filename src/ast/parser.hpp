#pragma  once
#include "tokens/token.hpp"
#include "ast.hpp"

namespace Noctis
{
	class MacroVarSolver;
	struct Context;

	class Parser
	{
	private:

		enum class OpPrecedence : u8
		{
			MulDivRemCon,
			AddMin,
			ShiftRot,
			BinAnd,
			BinXor,
			BinOr,
			Range,
			Contains,
			Cmp,
			NullElvis,
			LogAnd,
			LogOr,
			None,
		};
		
	public:
		Parser(const StdVector<Token>& tokens, Context* pCtx);

		StdVector<AstStmtSPtr> Parse();

		AstDeclSPtr ParseModuleDecl();
		AstDeclSPtr ParseUnittestDecl();
		AstDeclSPtr ParseBenchmarkDecl();
	
		AstStmtSPtr ParseStatement(AstAttribsSPtr attribs = nullptr, bool funcAreMethods = false);

		AstDeclSPtr ParseStruct(AstAttribsSPtr attribs);
		AstDeclSPtr ParseUnion(AstAttribsSPtr attribs);
		AstDeclSPtr ParseEnum(AstAttribsSPtr attribs);
		AstDeclSPtr ParseInterface(AstAttribsSPtr attribs);
		AstDeclSPtr ParseWeakInterface(AstAttribsSPtr attribs);
		AstDeclSPtr ParseTypealias(AstAttribsSPtr attribs);
		AstDeclSPtr ParseTypedef(AstAttribsSPtr attribs);
		AstVarDeclSPtr ParseVarDecl(AstAttribsSPtr attribs);
		AstVarDeclSPtr ParseVarDecl(AstAttribsSPtr attribs, u64 startIdx,
			StdVector<StdString>&& idens);
		AstDeclSPtr ParseFuncDecl(AstAttribsSPtr attribs, bool asMethod);
		AstDeclSPtr ParseImplDecl(AstAttribsSPtr attribs);

		AstStmtSPtr ParseImport(AstAttribsSPtr attribs);
		AstStmtSPtr ParseBlockStmt();
		AstStmtSPtr ParseIfStmt();
		AstStmtSPtr ParseLoopStmt(AstLabelStmtSPtr label);
		AstStmtSPtr ParseWhileStmt(AstLabelStmtSPtr label);
		AstStmtSPtr ParseDoWhileStmt(AstLabelStmtSPtr label);
		AstStmtSPtr ParseForStmt(AstLabelStmtSPtr label);
		AstStmtSPtr ParseSwitch(AstLabelStmtSPtr label);
		AstLabelStmtSPtr ParseLabelStmt();
		AstStmtSPtr ParseBreakStmt();
		AstStmtSPtr ParseContinueStmt();
		AstStmtSPtr ParseFallthroughStmt();
		AstStmtSPtr ParseGotoStmt();
		AstStmtSPtr ParseReturnStmt();
		AstStmtSPtr ParseThrowStmt();
		AstStmtSPtr ParseExprOrMacroStmt();
		AstStmtSPtr ParseDeferStmt();
		AstStmtSPtr ParseErrDeferStmt();
		AstStmtSPtr ParseUnsafeStmt();
		AstStmtSPtr ParseErrorHandlerStmt();
		AstStmtSPtr ParseCompIfStmt();
		AstStmtSPtr ParseCompCondStmt();
		AstStmtSPtr ParseCompDebugStmt();
		AstStmtSPtr ParseMacroLoopStmt();

		AstExprSPtr ParseCommaExpression();

		AstExprSPtr ParseExpression(AstExprSPtr prev = nullptr, bool allowBlockExpr = false);
		AstExprSPtr ParseOperand(AstExprSPtr prev);
		AstExprSPtr ParseOperandImpl(AstExprSPtr prev);

		AstExprSPtr ParseAssignmentExpr(AstExprSPtr lExpr);
		AstExprSPtr ParseTernaryExpr(AstExprSPtr cond);
		AstExprSPtr ParseBinaryExpr(AstExprSPtr lExpr);
		AstExprSPtr ParsePostfixExpr(AstExprSPtr expr);
		AstExprSPtr ParsePrefixExpr();
		AstExprSPtr ParseQualNameExpr();
		AstExprSPtr ParseIndexSlicExpr(AstExprSPtr expr);
		AstExprSPtr ParseFuncCallExpr(AstExprSPtr expr);
		AstExprSPtr ParseMemberAccessExpr(AstExprSPtr expr);
		AstExprSPtr ParseMethodcallExpr(AstExprSPtr expr);
		AstExprSPtr ParseTupleAccessExpr(AstExprSPtr expr);
		AstExprSPtr ParseLiteralExpr();
		AstExprSPtr ParseAggrInitExpr(AstQualNameExpr* qualName);
		AstExprSPtr ParseArrayInitExpr();
		AstExprSPtr ParseCastExpr(AstExprSPtr expr);
		AstExprSPtr ParseTransmuteExpr(AstExprSPtr expr);
		AstExprSPtr ParseMoveExpr();
		AstExprSPtr ParseBracketExpr();
		AstExprSPtr ParseBlockExpr();
		AstExprSPtr ParseUnsafeExpr();
		AstExprSPtr ParseClosureExpr();
		AstExprSPtr ParseIsExpr(AstExprSPtr expr);
		AstExprSPtr ParseTryExpr();
		AstExprSPtr ParseSpecKwExpr();
		AstExprSPtr ParseCompRunExpr();
		AstExprSPtr ParseMacroVarExpr();
			
		AstTypeSPtr ParseType(bool structKwOptional = false);
		AstIdentifierTypeSPtr ParseIdentifierType(AstAttribsSPtr attribs = nullptr);
		AstTypeSPtr ParsePointerType(AstAttribsSPtr attribs);
		AstTypeSPtr ParseReferenceType(AstAttribsSPtr attribs);
		AstTypeSPtr ParseArraySliceType(AstAttribsSPtr attribs);
		AstTypeSPtr ParseTupleType(AstAttribsSPtr attribs);
		AstTypeSPtr ParseOptionalType(AstAttribsSPtr attribs);
		AstTypeSPtr ParseInlineStructType(bool structKwOptional = false);
		AstTypeSPtr ParseInlineEnumType();
		AstTypeSPtr ParseCompoundInterfaceType();
		AstTypeSPtr ParseCompoundInterfaceType(AstIdentifierTypeSPtr first);

		AstPatternSPtr ParsePattern();
		AstPatternSPtr ParseValueBindPattern(StdString&& iden);
		AstPatternSPtr ParseRangePattern(Token from);
		AstPatternSPtr ParseTuplePattern();
		AstPatternSPtr ParseEnumPattern(AstQualNameSPtr iden);
		AstPatternSPtr ParseAggrPattern(AstQualNameSPtr qualName);
		AstPatternSPtr ParseSlicePattern();
		AstPatternSPtr ParseEitherPattern(AstPatternSPtr pattern);
		AstPatternSPtr ParseTypePattern();
		
		AstAttribsSPtr ParseAttributes();
		AstCompAttribSPtr ParseCompAttribute();
		AstUserAttribSPtr ParseUserAttribute();
		AstVisibilityAttribSPtr ParseVisibilityAttribute();

		AstGenericDeclSPtr ParseGenericDecl();
		AstGenericParam ParseGenericParam();
		AstGenericTypeBoundSPtr ParseGenericTypeBound();
		AstGenericBoundTypeSPtr ParseGenericBoundType();
		AstGenericAssocTypeBound ParseAssocBounds();
		AstGenericWhereClauseSPtr ParseGenericWhereClause();

		AstMacroPatternElemSPtr ParseMacroVar();
		AstMacroPatternElemSPtr ParseMacroSeparator();
		AstMacroPatternElemSPtr ParseMacroFragment();
		AstMacroPatternSPtr ParseMacroPattern(bool inFragment = false);
		AstMacroRuleSPtr ParseMacroRule();
		AstDeclSPtr ParseDeclMacro();
		AstDeclSPtr ParseProcMacro();
		TokenTree ParseTokenTree(Token firstTok = Token{ TokenType::Unknown, u64(-1) });
		AstExprSPtr ParseMacroInstExpr(AstQualNameSPtr qualName);
		AstPatternSPtr ParseMacroInstPattern(AstQualNameSPtr qualName);

		AstQualNameSPtr ParseQualName(bool genericInstWithLess);
		AstQualIdenSPtr ParseQualIden(bool genericInstWithLess);
		StdVector<AstParamSPtr> ParseParams(bool allowNoType);
		AstParamSPtr ParseParam(bool allowNoType = false);
		StdVector<AstArgSPtr> ParseArgs();
		AstArgSPtr ParseArg();
		StdString ParseIden();
		StdString ParseIden(u64& tokIdx);

		bool HasParsedAllTokens() const { return m_TokIdx == m_Tokens.size(); }
		void SetMacroVarSolver(MacroVarSolver* pSolver) { m_pMacroSolver = pSolver; }

	private:

		StdVector<StdString> ParseIdenList(TokenType separator);
		StdVector<StdString> ParseIdenList(TokenType separator, u64& startIdx, u64& endIdx);

		OpPrecedence GetPrecedence(TokenType op);

		Token& EatToken();
		Token& EatToken(TokenType type);
		Token& EatIdenToken(StdStringView text);
		bool TryEatToken(TokenType type);
		bool TryEatIdenToken(StdStringView text);
		Token& PeekToken();
		Token& PeekToken(u64 offset);

		void InsertTreeIntoTokens(TokenTree& tree);

		StdVector<Token> m_Tokens;
		u64 m_TokIdx;
		Context* m_pCtx;
		MacroVarSolver* m_pMacroSolver;
		bool m_AllowAggrInit;
	};
	
}
