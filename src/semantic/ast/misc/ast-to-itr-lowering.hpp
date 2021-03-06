#pragma once
#include "common/type.hpp"
#include "itr/itr.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	enum class AstMethodReceiverKind : u8;
	
	FWDECL_STRUCT_SPTR(ITrIden);
	FWDECL_STRUCT_SPTR(ITrTypeDisambiguation);
	FWDECL_STRUCT_SPTR(ITrQualName);
	
	FWDECL_STRUCT_SPTR(ITrParam);
	FWDECL_STRUCT_SPTR(ITrArg);
	
	FWDECL_STRUCT_SPTR(ITrDef);
	FWDECL_STRUCT_SPTR(ITrStmt);
	FWDECL_STRUCT_SPTR(ITrExpr);
	FWDECL_STRUCT_SPTR(ITrType);
	FWDECL_STRUCT_SPTR(ITrAttribs);
	FWDECL_STRUCT_SPTR(ITrPattern);
	FWDECL_STRUCT_SPTR(ITrGenDecl);
	FWDECL_STRUCT_SPTR(ITrGenBoundType);
	
	FWDECL_STRUCT_SPTR(AstArg);
	FWDECL_STRUCT_SPTR(AstParam);
	FWDECL_STRUCT_SPTR(AstAttribs);
	FWDECL_STRUCT_SPTR(AstGenericDecl);
	FWDECL_STRUCT_SPTR(AstGenericBoundType);

	class AstToITrLowering : public AstSemanticPass
	{
	public:
		AstToITrLowering();

		void Visit(AstTypeDisambiguation& node) override;
		void Visit(AstIden& node) override;
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
		void Visit(AstErrHandler& node) override;
		
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
		void Visit(AstCompIfStmt& node) override;
		void Visit(AstCompCondStmt& node) override;
		void Visit(AstCompDebugStmt& node) override;
		
		void Visit(AstAssignExpr& node) override;
		void Visit(AstTernaryExpr& node) override;
		void Visit(AstBinaryExpr& node) override;
		void Visit(AstRangeExpr& node) override;
		void Visit(AstPostfixExpr& node) override;
		void Visit(AstPrefixExpr& node) override;
		void Visit(AstQualNameExpr& node) override;
		void Visit(AstIndexSliceExpr& node) override;
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

		void Visit(AstMacroLoopStmt& node) override;
		
		void Visit(AstGenericDecl& node) override;
		void Visit(AstGenericTypeParam& node) override;
		void Visit(AstGenericValueParam& node) override;
		void Visit(AstGenericTypeBound& node) override;
		void Visit(AstGenericBoundType& node) override;

		void Visit(AstDeclSPtr& node) override;

	private:
		StdVector<ITrParamSPtr> GetParams(StdVector<AstParamSPtr>& astParams);
		StdVector<ITrArgSPtr> GetArgs(StdVector<AstArgSPtr>& astArgs);

		void HandleGenerics(ITrGenDeclSPtr genDecl, QualNameSPtr qualName);

		void AddMethodReceiverToParams(AstMethodDecl& node, StdVector<ITrParamSPtr>& params);
		void GetNamedReturns(ITrTypeSPtr& retType, StdVector<ITrStmtSPtr> stmts, StdPairVector<StdVector<StdString>, AstTypeSPtr>& astNamedRets);

		void HandleWhereClause(AstGenericWhereClause& clause, ITrGenDeclSPtr genDecl);

		void PushDefFrame(ITrDefSPtr def);
		void PushDef(ITrDefSPtr def);
		StdVector<ITrDefSPtr> PopDefFrame();
		bool IsModDef();

		ITrStmtSPtr VisitAndGetStmt(AstStmtSPtr stmt);
		ITrBlockSPtr VisitAndGetBlock(AstStmtSPtr stmt, const StdString& scopeName);
		ITrExprSPtr VisitAndGetExpr(AstExprSPtr expr);
		ITrTypeSPtr VisitAndGetType(AstTypeSPtr type);
		ITrPatternSPtr VisitAndGetPattern(AstPatternSPtr pattern);
		ITrAttribsSPtr VisitAndGetAttribs(AstAttribsSPtr attribs);
		ITrGenDeclSPtr VisitAndGetGenDecl(AstGenericDeclSPtr genDecl);
		ITrGenBoundTypeSPtr VisitAndGetGenBoundType(AstGenericBoundTypeSPtr bound);
		
		StdStack<StdPair<ITrDefSPtr, StdVector<ITrDefSPtr>>> m_Defs;
		ITrStmtSPtr m_Stmt;
		ITrExprSPtr m_Expr;
		ITrTypeSPtr m_Type;
		ITrPatternSPtr m_Pattern;
		ITrAttribsSPtr m_Attribs;
		StdStack<ITrGenBoundTypeSPtr> m_BoundTypes;
		ITrGenDeclSPtr m_GenDecl;
		ITrExprSPtr m_NamedRet;
		TypeHandle m_ImplType;
		ITrTypeSPtr m_ITrImplType;
		bool m_InFunc;
		StdString m_TreeFilename;
		StdVector<ITrIdenSPtr> m_Idens;
		ITrTypeDisambiguationSPtr m_TypeDisambiguation;
		ITrQualNameSPtr m_QualName;
	};
}
