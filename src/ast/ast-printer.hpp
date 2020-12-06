#pragma once
#include "semantic/semantic-pass.hpp"
#include "ast.hpp"

namespace Noctis
{
	class AstPrinter : public AstVisitor
	{
	public:
		AstPrinter();
		
		void Visit(AstTree& tree) override;
		
		void Visit(AstIden& node) override;
		void Visit(AstTypeDisambiguation& node) override;
		void Visit(AstQualIdenSPtr node) override;
		void Visit(AstQualName& node) override;
		void Visit(AstParam& node) override;
		void Visit(AstArg& node) override;
		
		void Visit(AstModuleDecl& node) override;
		void Visit(AstUnittestDecl& node) override;
		void Visit(AstBenchmarkDecl& node) override;
		void Visit(AstStructDecl& node) override;
		void Visit(AstUnionDecl& node) override;
		void Visit(AstValueEnumDecl& node) override;
		void Visit(AstAdtEnumDecl& node) override;
		void Visit(AstMarkerInterfaceDecl& node) override;
		void Visit(AstWeakInterfaceDecl& node) override;
		void Visit(AstStrongInterfaceDecl& node) override;
		void Visit(AstTypeAliasDecl& node) override;
		void Visit(AstTypeDefDecl& node) override;
		void Visit(AstVarDecl& node) override;
		void Visit(AstFuncDecl& node) override;
		void Visit(AstMethodDecl& node) override;
		void Visit(AstImplDecl& node) override;
		
		void Visit(AstImportStmt& node) override;
		void Visit(AstBlockStmt& node) override;
		void Visit(AstIfStmt& node) override;
		void Visit(AstLoopStmt& node) override;
		void Visit(AstWhileStmt& node) override;
		void Visit(AstDoWhileStmt& node) override;
		void Visit(AstForStmt& node) override;
		void Visit(AstSwitchStmt& node) override;
		void Visit(AstLabelStmt& node) override;
		void Visit(AstBreakStmt& node) override;
		void Visit(AstContinueStmt& node) override;
		void Visit(AstFallthroughStmt& node) override;
		void Visit(AstGotoStmt& node) override;
		void Visit(AstReturnStmt& node) override;
		void Visit(AstThrowStmt& node) override;
		void Visit(AstExprStmt& node) override;
		void Visit(AstDeferStmt& node) override;
		void Visit(AstErrDeferStmt& node) override;
		void Visit(AstUnsafeStmt& node) override;
		void Visit(AstErrorHandlerStmt& node) override;
		void Visit(AstCompIfStmt& node) override;
		void Visit(AstCompCondStmt& node) override;
		void Visit(AstCompDebugStmt& node) override;
		void Visit(AstMacroLoopStmt& node) override;
		
		void Visit(AstAssignExpr& node) override;
		void Visit(AstTernaryExpr& node) override;
		void Visit(AstBinaryExpr& node) override;
		void Visit(AstPostfixExpr& node) override;
		void Visit(AstPrefixExpr& node) override;
		void Visit(AstQualNameExpr& node) override;
		void Visit(AstIndexSliceExpr& node) override;
		void Visit(AstSliceExpr& node) override;
		void Visit(AstFuncCallExpr& node) override;
		void Visit(AstMemberAccessExpr& node) override;
		void Visit(AstMethodCallExpr& node) override;
		void Visit(AstTupleAccessExpr& node) override;
		void Visit(AstLiteralExpr& node) override;
		void Visit(AstAggrInitExpr& node) override;
		void Visit(AstTupleInitExpr& node) override;
		void Visit(AstArrayInitExpr& node) override;
		void Visit(AstCastExpr& node) override;
		void Visit(AstTransmuteExpr& node) override;
		void Visit(AstMoveExpr& node) override;
		void Visit(AstBracketExpr& node) override;
		void Visit(AstBlockExpr& node) override;
		void Visit(AstUnsafeExpr& node) override;
		void Visit(AstCommaExpr& node) override;
		void Visit(AstClosureExpr& node) override;
		void Visit(AstIsExpr& node) override;
		void Visit(AstTryExpr& node) override;
		void Visit(AstSpecKwExpr& node) override;
		void Visit(AstCompRunExpr& node) override;
		void Visit(AstMacroVarExpr& node) override;
		
		void Visit(AstBuiltinType& node) override;
		void Visit(AstIdentifierType& node) override;
		void Visit(AstPointerType& node) override;
		void Visit(AstReferenceType& node) override;
		void Visit(AstArrayType& node) override;
		void Visit(AstSliceType& node) override;
		void Visit(AstTupleType& node) override;
		void Visit(AstOptionalType& node) override;
		void Visit(AstInlineStructType& node) override;
		void Visit(AstInlineEnumType& node) override;
		void Visit(AstCompoundInterfaceType& node) override;

		void Visit(AstPlaceholderPattern& node) override;
		void Visit(AstWildcardPattern& node) override;
		void Visit(AstValueBindPattern& node) override;
		void Visit(AstLiteralPattern& node) override;
		void Visit(AstRangePattern& node) override;
		void Visit(AstTuplePattern& node) override;
		void Visit(AstEnumPattern& node) override;
		void Visit(AstAggrPattern& node) override;
		void Visit(AstSlicePattern& node) override;
		void Visit(AstEitherPattern& node) override;
		void Visit(AstTypePattern& node) override;
		
		void Visit(AstAttribs& node) override;
		void Visit(AstCompAttrib& node) override;
		void Visit(AstUserAttrib& node) override;
		void Visit(AstVisibilityAttrib& node) override;
		void Visit(AstSimpleAttrib& node) override;
		
		void Visit(AstGenericDecl& node) override;
		void Visit(AstGenericTypeParam& node) override;
		void Visit(AstGenericValueParam& node) override;
		void Visit(AstGenericTypeBound& node) override;
		void Visit(AstGenericAssocTypeBound& node) override;
		void Visit(AstGenericBoundType& node) override;
		void Visit(AstGenericWhereClause& node) override;
		
		void Visit(AstMacroVar& node) override;
		void Visit(AstMacroSeparator& node) override;
		void Visit(AstMacroFragment& node) override;
		void Visit(AstMacroPattern& node) override;
		void Visit(AstMacroRule& node) override;
		void Visit(AstDeclMacro& node) override;
		void Visit(AstRulesDeclMacro& node) override;
		void Visit(AstProcMacro& node) override;
		void Visit(AstRulesProcMacro& node) override;
		
		void Visit(AstMacroInstStmt& node) override;
		void Visit(AstMacroInstExpr& node) override;
		void Visit(AstMacroInstPattern& node) override;

	private:
		void PrintTokTree(TokenTree& tokTree);
		void PrintIndent();
		void PrintContextAndClose(AstContextPtr& ctx);

	private:
		u32 m_Indent;
	};
	
}
