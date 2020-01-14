#include "astvisitor.hpp"

namespace Noctis
{	
	AstVisitor::AstVisitor()
	{
	}

	AstVisitor::~AstVisitor()
	{
	}

	bool AstVisitor::ShouldVisit(AstNodeKind type)
	{
		return true;
	}

	void AstVisitor::Visit(AstModuleDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstIdentifier& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstParam& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstArg& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstStructDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstUnionDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstValueEnumDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstAdtEnumStructMember& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstAdtEnumDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMarkerInterfaceDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstWeakInterfaceDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstStrongInterfaceDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTypeAliasDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTypeDefDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstVarDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstFuncDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMethodDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstEmptyMethodDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstImplDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstImportStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstBlockStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstIfStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstLoopStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstWhileStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstDoWhileStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstForStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstForRangeStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstSwitchStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstLabelStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstBreakStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstContinueStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstFallthroughStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstGotoStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstReturnStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstExprStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstDeferStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstStackDeferStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstUnsafeStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCompIfStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCompCondStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCompDebugStmt& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstAssignExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTernaryExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstBinaryExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstPostfixExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstPrefixExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstQualNameExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstIndexSliceExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstSliceExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstFuncCallExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMemberAccessExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMethodCallExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTupleAccessExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstLiteralExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstAggrInitExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTupleInitExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstArrayInitExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCastExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTransmuteExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMoveExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstBracketExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstBlockExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstUnsafeExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCommaExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstVoidExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstClosureExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCompRunExpr& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstBuiltinType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstIdentifierType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstPointerType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstReferenceType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstArrayType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstSliceType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstTupleType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstOptionalType& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstAttributes& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstCompAttribute& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstUserAttribute& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstVisibilityAttribute& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstSimpleAttribute& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstGenericDecl& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstGenericValueParam& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstGenericTypeParam& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstGenericWhereClause& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstGenericInst& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMacroVar& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMacroSeparator& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMacroFragment& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMacroPattern& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMacroRule& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstDeclMacro& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstRulesDeclMacro& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstProcMacro& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstRulesProcMacro& node, AstVisitLoc loc)
	{
	}

	void AstVisitor::Visit(AstMacroInst& node, AstVisitLoc loc)
	{
	}
}
