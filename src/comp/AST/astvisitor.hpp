#pragma once
#include "common/defs.hpp"
#include "ast.hpp"

namespace Noctis
{

	// AST node forward decls
	struct AstTree;
	
	struct AstModuleDecl;
	struct AstIdentifier;
	struct AstParam;
	struct AstArg;

	struct AstStructDecl;
	struct AstUnionDecl;
	struct AstValueEnumDecl;
	struct AstValueEnumStructMember;
	struct AstAdtEnumDecl;
	struct AstMarkerInterfaceDecl;
	struct AstWeakInterfaceDecl;
	struct AstStrongInterfaceDecl;
	struct AstTypeAliasDecl;
	struct AstTypeDefDecl;
	struct AstVarDecl;
	struct AstFuncDecl;
	struct AstMethodDecl;
	struct AstEmptyMethodDecl;
	struct AstImplDecl;

	struct AstImportStmt;
	struct AstBlockStmt;
	struct AstIfStmt;
	struct AstLoopStmt;
	struct AstWhileStmt;
	struct AstDoWhileStmt;
	struct AstForStmt;
	struct AstForRangeStmt;
	struct AstSwitchStmt;
	struct AstLabelStmt;
	struct AstBreakStmt;
	struct AstContinueStmt;
	struct AstFallthroughStmt;
	struct AstGotoStmt;
	struct AstReturnStmt;
	struct AstExprStmt;
	struct AstDeferStmt;
	struct AstStackDeferStmt;
	struct AstUnsafeStmt;
	struct AstCompIfStmt;
	struct AstCompCondStmt;
	struct AstCompDebugStmt;

	struct AstAssignExpr;
	struct AstTernaryExpr;
	struct AstBinaryExpr;
	struct AstPostfixExpr;
	struct AstPrefixExpr;
	struct AstQualNameExpr;
	struct AstIndexSliceExpr;
	struct AstSliceExpr;
	struct AstFuncCallExpr;
	struct AstMemberAccessExpr;
	struct AstMethodCallExpr;
	struct AstTupleAccessExpr;
	struct AstLiteralExpr;
	struct AstAggrInitExpr;
	struct AstTupleInitExpr;
	struct AstArrayInitExpr;
	struct AstCastExpr;
	struct AstTransmuteExpr;
	struct AstMoveExpr;
	struct AstBracketExpr;
	struct AstBlockExpr;
	struct AstUnsafeExpr;
	struct AstCommaExpr;
	struct AstVoidExpr;
	struct AstClosureExpr;
	struct AstIsExpr;
	struct AstCompRunExpr;

	struct AstBuiltinType;
	struct AstIdentifierType;
	struct AstPointerType;
	struct AstReferenceType;
	struct AstArrayType;
	struct AstSliceType;
	struct AstTupleType;
	struct AstOptionalType;

	struct AstAttributes;
	struct AstCompAttribute;
	struct AstUserAttribute;
	struct AstVisibilityAttribute;
	struct AstSimpleAttribute;

	struct AstGenericDecl;
	struct AstGenericTypeParam;
	struct AstGenericValueParam;
	struct AstGenericWhereClause;
	struct AstGenericInst;

	struct AstMacroVar;
	struct AstMacroSeparator;
	struct AstMacroFragment;
	struct AstMacroPattern;
	struct AstMacroRule;
	struct AstDeclMacro;
	struct AstRulesDeclMacro;
	struct AstProcMacro;
	struct AstRulesProcMacro;
	struct AstMacroInst;

	enum class AstNodeKind : u8;

	enum class AstVisitLoc
	{
		Begin,
		End
	};

	class AstVisitor
	{
	public:
		AstVisitor();
		virtual ~AstVisitor();

		virtual bool ShouldVisit(AstNodeKind type);
		
		virtual void Visit(AstTree& tree, AstVisitLoc loc);

		virtual void Visit(AstModuleDecl& node, AstVisitLoc loc);
		virtual void Visit(AstIdentifier& node, AstVisitLoc loc);
		virtual void Visit(AstParam& node, AstVisitLoc loc);
		virtual void Visit(AstArg& node, AstVisitLoc loc);

		virtual void Visit(AstStructDecl& node, AstVisitLoc loc);
		virtual void Visit(AstUnionDecl& node, AstVisitLoc loc);
		virtual void Visit(AstValueEnumDecl& node, AstVisitLoc loc);
		virtual void Visit(AstAdtEnumStructMember& node, AstVisitLoc loc);
		virtual void Visit(AstAdtEnumDecl& node, AstVisitLoc loc);
		virtual void Visit(AstMarkerInterfaceDecl& node, AstVisitLoc loc);
		virtual void Visit(AstWeakInterfaceDecl& node, AstVisitLoc loc);
		virtual void Visit(AstStrongInterfaceDecl& node, AstVisitLoc loc);
		virtual void Visit(AstTypeAliasDecl& node, AstVisitLoc loc);
		virtual void Visit(AstTypeDefDecl& node, AstVisitLoc loc);
		virtual void Visit(AstVarDecl& node, AstVisitLoc loc);
		virtual void Visit(AstFuncDecl& node, AstVisitLoc loc);
		virtual void Visit(AstMethodDecl& node, AstVisitLoc loc);
		virtual void Visit(AstEmptyMethodDecl& node, AstVisitLoc loc);
		virtual void Visit(AstImplDecl& node, AstVisitLoc loc);

		virtual void Visit(AstImportStmt& node, AstVisitLoc loc);
		virtual void Visit(AstBlockStmt& node, AstVisitLoc loc);
		virtual void Visit(AstIfStmt& node, AstVisitLoc loc);
		virtual void Visit(AstLoopStmt& node, AstVisitLoc loc);
		virtual void Visit(AstWhileStmt& node, AstVisitLoc loc);
		virtual void Visit(AstDoWhileStmt& node, AstVisitLoc loc);
		virtual void Visit(AstForStmt& node, AstVisitLoc loc);
		virtual void Visit(AstForRangeStmt& node, AstVisitLoc loc);
		virtual void Visit(AstSwitchStmt& node, AstVisitLoc loc);
		virtual void Visit(AstLabelStmt& node, AstVisitLoc loc);
		virtual void Visit(AstBreakStmt& node, AstVisitLoc loc);
		virtual void Visit(AstContinueStmt& node, AstVisitLoc loc);
		virtual void Visit(AstFallthroughStmt& node, AstVisitLoc loc);
		virtual void Visit(AstGotoStmt& node, AstVisitLoc loc);
		virtual void Visit(AstReturnStmt& node, AstVisitLoc loc);
		virtual void Visit(AstExprStmt& node, AstVisitLoc loc);
		virtual void Visit(AstDeferStmt& node, AstVisitLoc loc);
		virtual void Visit(AstStackDeferStmt& node, AstVisitLoc loc);
		virtual void Visit(AstUnsafeStmt& node, AstVisitLoc loc);
		virtual void Visit(AstCompIfStmt& node, AstVisitLoc loc);
		virtual void Visit(AstCompCondStmt& node, AstVisitLoc loc);
		virtual void Visit(AstCompDebugStmt& node, AstVisitLoc loc);

		virtual void Visit(AstAssignExpr& node, AstVisitLoc loc);
		virtual void Visit(AstTernaryExpr& node, AstVisitLoc loc);
		virtual void Visit(AstBinaryExpr& node, AstVisitLoc loc);
		virtual void Visit(AstPostfixExpr& node, AstVisitLoc loc);
		virtual void Visit(AstPrefixExpr& node, AstVisitLoc loc);
		virtual void Visit(AstQualNameExpr& node, AstVisitLoc loc);
		virtual void Visit(AstIndexSliceExpr& node, AstVisitLoc loc);
		virtual void Visit(AstSliceExpr& node, AstVisitLoc loc);
		virtual void Visit(AstFuncCallExpr& node, AstVisitLoc loc);
		virtual void Visit(AstMemberAccessExpr& node, AstVisitLoc loc);
		virtual void Visit(AstMethodCallExpr& node, AstVisitLoc loc);
		virtual void Visit(AstTupleAccessExpr& node, AstVisitLoc loc);
		virtual void Visit(AstLiteralExpr& node, AstVisitLoc loc);
		virtual void Visit(AstAggrInitExpr& node, AstVisitLoc loc);
		virtual void Visit(AstTupleInitExpr& node, AstVisitLoc loc);
		virtual void Visit(AstArrayInitExpr& node, AstVisitLoc loc);
		virtual void Visit(AstCastExpr& node, AstVisitLoc loc);
		virtual void Visit(AstTransmuteExpr& node, AstVisitLoc loc);
		virtual void Visit(AstMoveExpr& node, AstVisitLoc loc);
		virtual void Visit(AstBracketExpr& node, AstVisitLoc loc);
		virtual void Visit(AstBlockExpr& node, AstVisitLoc loc);
		virtual void Visit(AstUnsafeExpr& node, AstVisitLoc loc);
		virtual void Visit(AstCommaExpr& node, AstVisitLoc loc);
		virtual void Visit(AstVoidExpr& node, AstVisitLoc loc);
		virtual void Visit(AstClosureExpr& node, AstVisitLoc loc);
		virtual void Visit(AstIsExpr& node, AstVisitLoc loc);
		virtual void Visit(AstCompRunExpr& node, AstVisitLoc loc);

		virtual void Visit(AstBuiltinType& node, AstVisitLoc loc);
		virtual void Visit(AstIdentifierType& node, AstVisitLoc loc);
		virtual void Visit(AstPointerType& node, AstVisitLoc loc);
		virtual void Visit(AstReferenceType& node, AstVisitLoc loc);
		virtual void Visit(AstArrayType& node, AstVisitLoc loc);
		virtual void Visit(AstSliceType& node, AstVisitLoc loc);
		virtual void Visit(AstTupleType& node, AstVisitLoc loc);
		virtual void Visit(AstOptionalType& node, AstVisitLoc loc);
		
		virtual void Visit(AstAttributes& node, AstVisitLoc loc);
		virtual void Visit(AstCompAttribute& node, AstVisitLoc loc);
		virtual void Visit(AstUserAttribute& node, AstVisitLoc loc);
		virtual void Visit(AstVisibilityAttribute& node, AstVisitLoc loc);
		virtual void Visit(AstSimpleAttribute& node, AstVisitLoc loc);
		
		virtual void Visit(AstGenericDecl& node, AstVisitLoc loc);
		virtual void Visit(AstGenericValueParam& node, AstVisitLoc loc);
		virtual void Visit(AstGenericTypeParam& node, AstVisitLoc loc);
		virtual void Visit(AstGenericWhereClause& node, AstVisitLoc loc);
		virtual void Visit(AstGenericInst& node, AstVisitLoc loc);
		
		virtual void Visit(AstMacroVar& node, AstVisitLoc loc);
		virtual void Visit(AstMacroSeparator& node, AstVisitLoc loc);
		virtual void Visit(AstMacroFragment& node, AstVisitLoc loc);
		virtual void Visit(AstMacroPattern& node, AstVisitLoc loc);
		virtual void Visit(AstMacroRule& node, AstVisitLoc loc);
		virtual void Visit(AstDeclMacro& node, AstVisitLoc loc);
		virtual void Visit(AstRulesDeclMacro& node, AstVisitLoc loc);
		virtual void Visit(AstProcMacro& node, AstVisitLoc loc);
		virtual void Visit(AstRulesProcMacro& node, AstVisitLoc loc);
		virtual void Visit(AstMacroInst& node, AstVisitLoc loc);
		
	};
	
}
