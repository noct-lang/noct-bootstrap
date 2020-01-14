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

		StdVector<AstNodeSPtr> Parse();

		AstNodeSPtr ParseModuleDecl();

		AstNodeSPtr ParseStatement(AstNodeSPtr attribs = nullptr, bool funcAreMethods = false);

		AstNodeSPtr ParseStruct(AstNodeSPtr attribs);
		AstNodeSPtr ParseUnion(AstNodeSPtr attribs);
		AstNodeSPtr ParseEnum(AstNodeSPtr attribs);
		AstNodeSPtr ParseInterface(AstNodeSPtr attribs);
		AstNodeSPtr ParseWeakInterface(AstNodeSPtr attribs);
		AstNodeSPtr ParseTypealias(AstNodeSPtr attribs);
		AstNodeSPtr ParseTypedef(AstNodeSPtr attribs);
		AstNodeSPtr ParseVarDecl(AstNodeSPtr attribs);
		AstNodeSPtr ParseVarDecl(AstNodeSPtr attribs, u64 startIdx, StdVector<StdString>&& idens);
		AstNodeSPtr ParseFuncDecl(AstNodeSPtr attribs);
		AstNodeSPtr ParseMethodDecl(AstNodeSPtr attribs);
		AstNodeSPtr ParseImplDecl(AstNodeSPtr attribs);

		AstNodeSPtr ParseImport(AstNodeSPtr attribs);
		AstNodeSPtr ParseIfStmt();
		AstNodeSPtr ParseLoopStmt(AstNodeSPtr label);
		AstNodeSPtr ParseWhileStmt(AstNodeSPtr label);
		AstNodeSPtr ParseDoWhileStmt(AstNodeSPtr label);
		AstNodeSPtr ParseForStmt(AstNodeSPtr label);
		AstNodeSPtr ParseSwitch(AstNodeSPtr label);
		AstNodeSPtr ParseLabelStmt();
		AstNodeSPtr ParseBreakStmt();
		AstNodeSPtr ParseContinueStmt();
		AstNodeSPtr ParseFallthroughStmt();
		AstNodeSPtr ParseGotoStmt();
		AstNodeSPtr ParseReturnStmt();
		AstNodeSPtr ParseExprStmt();
		AstNodeSPtr ParseDeferStmt();
		AstNodeSPtr ParseStackDeferStmt();
		AstNodeSPtr ParseUnsafeStmt();
		AstNodeSPtr ParseCompIfStmt();
		AstNodeSPtr ParseCompCondStmt();
		AstNodeSPtr ParseCompDebugStmt();

		AstNodeSPtr ParseCommaExpression();
		AstNodeSPtr ParseExpression(AstNodeSPtr prev = nullptr, bool allowBlockExpr = false);

		AstNodeSPtr ParseAssignmentExpr(AstNodeSPtr lExpr);
		AstNodeSPtr ParseTernaryExpr(AstNodeSPtr cond);
		AstNodeSPtr ParseBinaryExpr(AstNodeSPtr lExpr);
		AstNodeSPtr ParsePostfixExpr(AstNodeSPtr expr);
		AstNodeSPtr ParsePrefixExpr();
		AstNodeSPtr ParseQualNameExpr();
		AstNodeSPtr ParseIndexSlicExpr(AstNodeSPtr expr);
		AstNodeSPtr ParseFuncCallExpr(AstNodeSPtr expr);
		AstNodeSPtr ParseMemberAccessExpr(AstNodeSPtr expr);
		AstNodeSPtr ParseMethodcallExpr(AstNodeSPtr expr);
		AstNodeSPtr ParseTupleAccessExpr(AstNodeSPtr expr);
		AstNodeSPtr ParseLiteralExpr();
		AstNodeSPtr ParseAggrInitExpr(AstQualNameExpr* qualName);
		AstNodeSPtr ParseArrayInitExpr();
		AstNodeSPtr ParseCastExpr();
		AstNodeSPtr ParseTransmuteExpr();
		AstNodeSPtr ParseMoveExpr();
		AstNodeSPtr ParseBracketExpr();
		AstNodeSPtr ParseBlockExpr();
		AstNodeSPtr ParseUnsafeExpr();
		AstNodeSPtr ParseClosureExpr();
		AstNodeSPtr ParseCompRunExpr();
			
		AstNodeSPtr ParseType();
		AstNodeSPtr ParseIdentifierType();
		AstNodeSPtr ParsePointerType();
		AstNodeSPtr ParseReferenceType();
		AstNodeSPtr ParseArraySliceType();
		AstNodeSPtr ParseTupleType();
		AstNodeSPtr ParseOptionalType();

		AstNodeSPtr ParseAttributes();
		AstNodeSPtr ParseCompAttribute();
		AstNodeSPtr ParseUserAttribute();
		AstNodeSPtr ParseVisibilityAttribute();

		AstNodeSPtr ParseGenericDecl();
		AstNodeSPtr ParseGenericParam();
		AstNodeSPtr ParseGenericWhereClause();
		AstNodeSPtr ParseGenericInst();

		AstNodeSPtr ParseMacroVar();
		AstNodeSPtr ParseMacroSeparator();
		AstNodeSPtr ParseMacroFragment();
		AstNodeSPtr ParseMacroPattern();
		AstNodeSPtr ParseMacroRules();
		AstNodeSPtr ParseDeclMacro();
		AstNodeSPtr ParseProcMacro();
		AstNodeSPtr ParseMacroInst();

		AstNodeSPtr ParseAstIden();
		StdVector<AstNodeSPtr> ParseParams(bool allowNoType = false);
		AstNodeSPtr ParseParam(bool allowNoType = false);
		StdVector<AstNodeSPtr> ParseArgs();
		AstNodeSPtr ParseArg();

	private:

		StdVector<StdString> ParseIdenList(TokenType separator);
		StdVector<StdString> ParseIdenList(TokenType separator, u64& startIdx, u64& endIdx);

		StdVector<AstNodeSPtr> ParseAstIdenList(TokenType separator);
		StdVector<AstNodeSPtr> ParseAstIdenList(TokenType separator, u64& startIdx, u64& endIdx);

		OperatorPrecedence GetPrecedence(TokenType op);
		
		Token& EatToken();
		Token& EatToken(TokenType type);
		Token& EatIdenToken(StdStringView text);
		Token& PeekToken();
		Token& PeekToken(u64 offset);

		StdVector<Token> m_Tokens;
		u64 m_TokIdx;
		Context* m_pCtx;
	};
	
}
