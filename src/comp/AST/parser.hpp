#pragma  once
#include "comp/token.hpp"
#include "ast.hpp"

namespace Noctis
{
	struct Context;

	class Parser
	{
	private:

		enum class OperatorPrecedence : u8
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
		AstDeclSPtr ParseFuncDecl(AstAttribsSPtr attribs);
		AstDeclSPtr ParseMethodDecl(AstAttribsSPtr attribs);
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
		AstStmtSPtr ParseExprStmt();
		AstStmtSPtr ParseDeferStmt();
		AstStmtSPtr ParseStackDeferStmt();
		AstStmtSPtr ParseUnsafeStmt();
		AstStmtSPtr ParseCompIfStmt();
		AstStmtSPtr ParseCompCondStmt();
		AstStmtSPtr ParseCompDebugStmt();
		AstStmtSPtr ParseMacroLoopStmt();

		AstExprSPtr ParseCommaExpression();
		AstExprSPtr ParseExpression(AstExprSPtr prev = nullptr, bool allowBlockExpr = false);

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
		AstExprSPtr ParseCastExpr();
		AstExprSPtr ParseTransmuteExpr();
		AstExprSPtr ParseMoveExpr();
		AstExprSPtr ParseBracketExpr();
		AstExprSPtr ParseBlockExpr();
		AstExprSPtr ParseUnsafeExpr();
		AstExprSPtr ParseClosureExpr();
		AstExprSPtr ParseIsExpr(AstExprSPtr expr);
		AstExprSPtr ParseCompRunExpr();
		AstExprSPtr ParseMacroVarExpr();
			
		AstTypeSPtr ParseType(bool structKwOptional = false);
		AstIdentifierTypeSPtr ParseIdentifierType();
		AstTypeSPtr ParsePointerType();
		AstTypeSPtr ParseReferenceType();
		AstTypeSPtr ParseArraySliceType();
		AstTypeSPtr ParseTupleType();
		AstTypeSPtr ParseOptionalType();
		AstTypeSPtr ParseInlineStructType(bool structKwOptional = false);
		AstTypeSPtr ParseInlineEnumType();
		AstTypeSPtr ParseCompoundInterfaceType(AstIdentifierTypeSPtr first);

		AstAttribsSPtr ParseAttributes();
		AstCompAttribSPtr ParseCompAttribute();
		AstUserAttribSPtr ParseUserAttribute();
		AstVisibilityAttribSPtr ParseVisibilityAttribute();

		AstGenericDeclSPtr ParseGenericDecl();
		AstGenericParam ParseGenericParam();
		AstGenericTypeBoundSPtr ParseGenericTypeBound();
		AstGenericWhereClauseSPtr ParseGenericWhereClause();

		AstMacroPatternElemSPtr ParseMacroVar();
		AstMacroPatternElemSPtr ParseMacroSeparator();
		AstMacroPatternElemSPtr ParseMacroFragment();
		AstMacroPatternSPtr ParseMacroPattern();
		AstMacroRuleSPtr ParseMacroRules();
		AstDeclSPtr ParseDeclMacro();
		AstDeclSPtr ParseProcMacro();
		AstExprSPtr ParseMacroInst();

		AstQualNameSPtr ParseQualName();
		StdVector<AstParamSPtr> ParseParams(bool allowNoType = false);
		AstParamSPtr ParseParam(bool allowNoType = false);
		StdVector<AstArgSPtr> ParseArgs();
		AstArgSPtr ParseArg();

	private:

		StdVector<StdString> ParseIdenList(TokenType separator);
		StdVector<StdString> ParseIdenList(TokenType separator, u64& startIdx, u64& endIdx);

		OperatorPrecedence GetPrecedence(TokenType op);
		
		Token& EatToken();
		Token& EatToken(TokenType type);
		Token& EatIdenToken(StdStringView text);
		bool TryEatToken(TokenType type);
		Token& PeekToken();
		Token& PeekToken(u64 offset);

		StdVector<Token> m_Tokens;
		u64 m_TokIdx;
		Context* m_pCtx;
	};
	
}
