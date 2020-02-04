#pragma once
#include "common/defs.hpp"

namespace Noctis
{

	// AST node forward decls
	struct AstTree;
	
	struct AstIden;
	struct AstTypeDisambiguation;
	struct AstQualName;
	struct AstParam;
	struct AstArg;

	struct AstModuleDecl;
	struct AstUnittestDecl;
	struct AstBenchmarkDecl;
	
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
	struct AstSwitchStmt;
	struct AstLabelStmt;
	struct AstBreakStmt;
	struct AstContinueStmt;
	struct AstFallthroughStmt;
	struct AstGotoStmt;
	struct AstReturnStmt;
	struct AstExprStmt;
	struct AstDeferStmt;
	struct AstErrDeferStmt;
	struct AstUnsafeStmt;
	struct AstErrorHandlerStmt;
	struct AstCompIfStmt;
	struct AstCompCondStmt;
	struct AstCompDebugStmt;
	struct AstMacroLoopStmt;

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
	struct AstClosureExpr;
	struct AstIsExpr;
	struct AstTryExpr;
	struct AstThrowExpr;
	struct AstSpecKwExpr;
	struct AstCompRunExpr;
	struct AstMacroVarExpr;

	struct AstBuiltinType;
	struct AstIdentifierType;
	struct AstPointerType;
	struct AstReferenceType;
	struct AstArrayType;
	struct AstSliceType;
	struct AstTupleType;
	struct AstOptionalType;
	struct AstInlineStructType;
	struct AstInlineEnumType;
	struct AstCompoundInterfaceType;

	struct AstPlaceholderPattern;
	struct AstWildcardPattern;
	struct AstValueBindPattern;
	struct AstLiteralPattern;
	struct AstRangePattern;
	struct AstTuplePattern;
	struct AstEnumPattern;
	struct AstAggrPattern;
	struct AstSlicePattern;
	struct AstEitherPattern;
	struct AstTypePattern;

	struct AstAttribs;
	struct AstCompAttrib;
	struct AstUserAttrib;
	struct AstVisibilityAttrib;
	struct AstSimpleAttrib;

	struct AstGenericDecl;
	struct AstGenericTypeParam;
	struct AstGenericValueParam;
	struct AstGenericTypeBound;
	struct AstGenericWhereClause;

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

#define  FWD_DECL_SPTR(type) \
	struct type;\
	using type##SPtr = StdSharedPtr<type>

	FWD_DECL_SPTR(AstStmt);
	FWD_DECL_SPTR(AstDecl);
	FWD_DECL_SPTR(AstExpr);
	FWD_DECL_SPTR(AstType);
	FWD_DECL_SPTR(AstQualIden);
	FWD_DECL_SPTR(AstPattern);

	class AstVisitor
	{
	public:
		AstVisitor();
		virtual ~AstVisitor();
		
		virtual void Visit(AstTree& tree);

		virtual void Visit(AstQualIdenSPtr node);
		virtual void Visit(AstIden& node);
		virtual void Visit(AstTypeDisambiguation& node);
		virtual void Visit(AstQualName& node);
		virtual void Visit(AstParam& node);
		virtual void Visit(AstArg& node);

		virtual void Visit(AstModuleDecl& node);
		virtual void Visit(AstUnittestDecl& node);
		virtual void Visit(AstBenchmarkDecl& node);
		
		virtual void Visit(AstStructDecl& node);
		virtual void Visit(AstUnionDecl& node);
		virtual void Visit(AstValueEnumDecl& node);
		virtual void Visit(AstAdtEnumDecl& node);
		virtual void Visit(AstMarkerInterfaceDecl& node);
		virtual void Visit(AstWeakInterfaceDecl& node);
		virtual void Visit(AstStrongInterfaceDecl& node);
		virtual void Visit(AstTypeAliasDecl& node);
		virtual void Visit(AstTypeDefDecl& node);
		virtual void Visit(AstVarDecl& node);
		virtual void Visit(AstFuncDecl& node);
		virtual void Visit(AstMethodDecl& node);
		virtual void Visit(AstEmptyMethodDecl& node);
		virtual void Visit(AstImplDecl& node);

		virtual void Visit(AstImportStmt& node);
		virtual void Visit(AstBlockStmt& node);
		virtual void Visit(AstIfStmt& node);
		virtual void Visit(AstLoopStmt& node);
		virtual void Visit(AstWhileStmt& node);
		virtual void Visit(AstDoWhileStmt& node);
		virtual void Visit(AstForStmt& node);
		virtual void Visit(AstSwitchStmt& node);
		virtual void Visit(AstLabelStmt& node);
		virtual void Visit(AstBreakStmt& node);
		virtual void Visit(AstContinueStmt& node);
		virtual void Visit(AstFallthroughStmt& node);
		virtual void Visit(AstGotoStmt& node);
		virtual void Visit(AstReturnStmt& node);
		virtual void Visit(AstExprStmt& node);
		virtual void Visit(AstDeferStmt& node);
		virtual void Visit(AstErrDeferStmt& node);
		virtual void Visit(AstUnsafeStmt& node);
		virtual void Visit(AstErrorHandlerStmt& node);
		virtual void Visit(AstCompIfStmt& node);
		virtual void Visit(AstCompCondStmt& node);
		virtual void Visit(AstCompDebugStmt& node);
		virtual void Visit(AstMacroLoopStmt& node);

		virtual void Visit(AstAssignExpr& node);
		virtual void Visit(AstTernaryExpr& node);
		virtual void Visit(AstBinaryExpr& node);
		virtual void Visit(AstPostfixExpr& node);
		virtual void Visit(AstPrefixExpr& node);
		virtual void Visit(AstQualNameExpr& node);
		virtual void Visit(AstIndexSliceExpr& node);
		virtual void Visit(AstSliceExpr& node);
		virtual void Visit(AstFuncCallExpr& node);
		virtual void Visit(AstMemberAccessExpr& node);
		virtual void Visit(AstMethodCallExpr& node);
		virtual void Visit(AstTupleAccessExpr& node);
		virtual void Visit(AstLiteralExpr& node);
		virtual void Visit(AstAggrInitExpr& node);
		virtual void Visit(AstTupleInitExpr& node);
		virtual void Visit(AstArrayInitExpr& node);
		virtual void Visit(AstCastExpr& node);
		virtual void Visit(AstTransmuteExpr& node);
		virtual void Visit(AstMoveExpr& node);
		virtual void Visit(AstBracketExpr& node);
		virtual void Visit(AstBlockExpr& node);
		virtual void Visit(AstUnsafeExpr& node);
		virtual void Visit(AstCommaExpr& node);
		virtual void Visit(AstClosureExpr& node);
		virtual void Visit(AstIsExpr& node);
		virtual void Visit(AstTryExpr& node);
		virtual void Visit(AstThrowExpr& node);
		virtual void Visit(AstSpecKwExpr& node);
		virtual void Visit(AstCompRunExpr& node);
		virtual void Visit(AstMacroVarExpr& node);

		virtual void Visit(AstBuiltinType& node);
		virtual void Visit(AstIdentifierType& node);
		virtual void Visit(AstPointerType& node);
		virtual void Visit(AstReferenceType& node);
		virtual void Visit(AstArrayType& node);
		virtual void Visit(AstSliceType& node);
		virtual void Visit(AstTupleType& node);
		virtual void Visit(AstOptionalType& node);
		virtual void Visit(AstInlineStructType& node);
		virtual void Visit(AstInlineEnumType& node);
		virtual void Visit(AstCompoundInterfaceType& node);
		
		virtual void Visit(AstPlaceholderPattern& node);
		virtual void Visit(AstWildcardPattern& node);
		virtual void Visit(AstValueBindPattern& node);
		virtual void Visit(AstLiteralPattern& node);
		virtual void Visit(AstRangePattern& node);
		virtual void Visit(AstTuplePattern& node);
		virtual void Visit(AstEnumPattern& node);
		virtual void Visit(AstAggrPattern& node);
		virtual void Visit(AstSlicePattern& node);
		virtual void Visit(AstEitherPattern& node);
		virtual void Visit(AstTypePattern& node);
		
		virtual void Visit(AstAttribs& node);
		virtual void Visit(AstCompAttrib& node);
		virtual void Visit(AstUserAttrib& node);
		virtual void Visit(AstVisibilityAttrib& node);
		virtual void Visit(AstSimpleAttrib& node);
		
		virtual void Visit(AstGenericDecl& node);
		virtual void Visit(AstGenericTypeParam& node);
		virtual void Visit(AstGenericValueParam& node);
		virtual void Visit(AstGenericTypeBound& node);
		virtual void Visit(AstGenericWhereClause& node);
		
		virtual void Visit(AstMacroVar& node);
		virtual void Visit(AstMacroSeparator& node);
		virtual void Visit(AstMacroFragment& node);
		virtual void Visit(AstMacroPattern& node);
		virtual void Visit(AstMacroRule& node);
		virtual void Visit(AstDeclMacro& node);
		virtual void Visit(AstRulesDeclMacro& node);
		virtual void Visit(AstProcMacro& node);
		virtual void Visit(AstRulesProcMacro& node);
		virtual void Visit(AstMacroInst& node);

	protected:
		void Visit(AstStmtSPtr node);
		void Visit(AstDeclSPtr node);
		void Visit(AstExprSPtr node);
		void Visit(AstTypeSPtr node);
		void Visit(AstPatternSPtr node);
		
		void Walk(AstTree& tree);

		void Walk(AstIden& node);
		void Walk(AstTypeDisambiguation& node);
		void Walk(AstQualName& node);
		void Walk(AstParam& node);
		void Walk(AstArg& node);

		void Walk(AstModuleDecl& node);
		void Walk(AstUnittestDecl& node);
		void Walk(AstBenchmarkDecl& node);

		void Walk(AstStructDecl& node);
		void Walk(AstUnionDecl& node);
		void Walk(AstValueEnumDecl& node);
		void Walk(AstAdtEnumDecl& node);
		void Walk(AstMarkerInterfaceDecl& node);
		void Walk(AstWeakInterfaceDecl& node);
		void Walk(AstStrongInterfaceDecl& node);
		void Walk(AstTypeAliasDecl& node);
		void Walk(AstTypeDefDecl& node);
		void Walk(AstVarDecl& node);
		void Walk(AstFuncDecl& node);
		void Walk(AstMethodDecl& node);
		void Walk(AstEmptyMethodDecl& node);
		void Walk(AstImplDecl& node);

		void Walk(AstImportStmt& node);
		void Walk(AstBlockStmt& node);
		void Walk(AstIfStmt& node);
		void Walk(AstLoopStmt& node);
		void Walk(AstWhileStmt& node);
		void Walk(AstDoWhileStmt& node);
		void Walk(AstForStmt& node);
		void Walk(AstSwitchStmt& node);
		void Walk(AstLabelStmt& node);
		void Walk(AstBreakStmt& node);
		void Walk(AstContinueStmt& node);
		void Walk(AstFallthroughStmt& node);
		void Walk(AstGotoStmt& node);
		void Walk(AstReturnStmt& node);
		void Walk(AstExprStmt& node);
		void Walk(AstDeferStmt& node);
		void Walk(AstErrDeferStmt& node);
		void Walk(AstUnsafeStmt& node);
		void Walk(AstErrorHandlerStmt& node);
		void Walk(AstCompIfStmt& node);
		void Walk(AstCompCondStmt& node);
		void Walk(AstCompDebugStmt& node);
		void Walk(AstMacroLoopStmt& node);

		void Walk(AstAssignExpr& node);
		void Walk(AstTernaryExpr& node);
		void Walk(AstBinaryExpr& node);
		void Walk(AstPostfixExpr& node);
		void Walk(AstPrefixExpr& node);
		void Walk(AstQualNameExpr& node);
		void Walk(AstIndexSliceExpr& node);
		void Walk(AstSliceExpr& node);
		void Walk(AstFuncCallExpr& node);
		void Walk(AstMemberAccessExpr& node);
		void Walk(AstMethodCallExpr& node);
		void Walk(AstTupleAccessExpr& node);
		void Walk(AstLiteralExpr& node);
		void Walk(AstAggrInitExpr& node);
		void Walk(AstTupleInitExpr& node);
		void Walk(AstArrayInitExpr& node);
		void Walk(AstCastExpr& node);
		void Walk(AstTransmuteExpr& node);
		void Walk(AstMoveExpr& node);
		void Walk(AstBracketExpr& node);
		void Walk(AstBlockExpr& node);
		void Walk(AstUnsafeExpr& node);
		void Walk(AstCommaExpr& node);
		void Walk(AstClosureExpr& node);
		void Walk(AstIsExpr& node);
		void Walk(AstTryExpr& node);
		void Walk(AstThrowExpr& node);
		void Walk(AstSpecKwExpr& node);
		void Walk(AstCompRunExpr& node);
		void Walk(AstMacroVarExpr& node);

		void Walk(AstBuiltinType& node);
		void Walk(AstIdentifierType& node);
		void Walk(AstPointerType& node);
		void Walk(AstReferenceType& node);
		void Walk(AstArrayType& node);
		void Walk(AstSliceType& node);
		void Walk(AstTupleType& node);
		void Walk(AstOptionalType& node);
		void Walk(AstInlineStructType& node);
		void Walk(AstInlineEnumType& node);
		void Walk(AstCompoundInterfaceType& node);

		void Walk(AstPlaceholderPattern& node);
		void Walk(AstWildcardPattern& node);
		void Walk(AstValueBindPattern& node);
		void Walk(AstLiteralPattern& node);
		void Walk(AstRangePattern& node);
		void Walk(AstTuplePattern& node);
		void Walk(AstEnumPattern& node);
		void Walk(AstAggrPattern& node);
		void Walk(AstSlicePattern& node);
		void Walk(AstEitherPattern& node);
		void Walk(AstTypePattern& node);

		void Walk(AstAttribs& node);
		void Walk(AstCompAttrib& node);
		void Walk(AstUserAttrib& node);
		void Walk(AstVisibilityAttrib& node);
		void Walk(AstSimpleAttrib& node);

		void Walk(AstGenericDecl& node);
		void Walk(AstGenericTypeParam& node);
		void Walk(AstGenericValueParam& node);
		void Walk(AstGenericTypeBound& node);
		void Walk(AstGenericWhereClause& node);

		void Walk(AstMacroVar& node);
		void Walk(AstMacroSeparator& node);
		void Walk(AstMacroFragment& node);
		void Walk(AstMacroPattern& node);
		void Walk(AstMacroRule& node);
		void Walk(AstDeclMacro& node);
		void Walk(AstRulesDeclMacro& node);
		void Walk(AstProcMacro& node);
		void Walk(AstRulesProcMacro& node);
		void Walk(AstMacroInst& node);
	};
	
}
