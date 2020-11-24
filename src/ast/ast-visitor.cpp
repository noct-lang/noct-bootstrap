#include "ast-visitor.hpp"
#include "ast.hpp"

namespace Noctis
{	
	AstVisitor::AstVisitor()
	{
	}

	AstVisitor::~AstVisitor()
	{
	}

	void AstVisitor::Visit(AstTree& tree)
	{
		Walk(tree);
	}

	void AstVisitor::Visit(AstQualIdenSPtr node)
	{
		switch (node->qualIdenKind)
		{
		case AstQualIdenKind::Identifier: Visit(*static_cast<AstIden*>(node.get())); break;
		case AstQualIdenKind::TypeDisambiguation: Visit(*static_cast<AstTypeDisambiguation*>(node.get())); break;
		default: ;
		}
	}

	void AstVisitor::Visit(AstIden& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTypeDisambiguation& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstQualName& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstParam& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstArg& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstModuleDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstUnittestDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBenchmarkDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstStructDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstUnionDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstValueEnumDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstAdtEnumDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMarkerInterfaceDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstWeakInterfaceDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstStrongInterfaceDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTypeAliasDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTypeDefDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstVarDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstFuncDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMethodDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstEmptyMethodDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstImplDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstImportStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBlockStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstIfStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstLoopStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstWhileStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstDoWhileStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstForStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstSwitchStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstLabelStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBreakStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstContinueStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstFallthroughStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGotoStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstReturnStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstThrowStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstExprStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstDeferStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstErrDeferStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstUnsafeStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstErrorHandlerStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCompIfStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCompCondStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCompDebugStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroLoopStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstAssignExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTernaryExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBinaryExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstPostfixExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstPrefixExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstQualNameExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstIndexSliceExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstSliceExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstFuncCallExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMemberAccessExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMethodCallExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTupleAccessExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstLiteralExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstAggrInitExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTupleInitExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstArrayInitExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCastExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTransmuteExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMoveExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBracketExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBlockExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstUnsafeExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCommaExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstClosureExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstIsExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTryExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstSpecKwExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCompRunExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroVarExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstBuiltinType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstIdentifierType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstPointerType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstReferenceType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstArrayType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstSliceType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTupleType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstOptionalType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstInlineStructType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstInlineEnumType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCompoundInterfaceType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstPlaceholderPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstWildcardPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstValueBindPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstLiteralPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstRangePattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTuplePattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstEnumPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstAggrPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstSlicePattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstEitherPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstTypePattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstAttribs& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstCompAttrib& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstUserAttrib& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstVisibilityAttrib& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstSimpleAttrib& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericDecl& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericTypeParam& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericValueParam& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericTypeBound& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericAssocTypeBound& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericBoundType& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstGenericWhereClause& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroVar& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroSeparator& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroFragment& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroRule& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstDeclMacro& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstRulesDeclMacro& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstProcMacro& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstRulesProcMacro& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroInstStmt& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroInstExpr& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstMacroInstPattern& node)
	{
		Walk(node);
	}

	void AstVisitor::Visit(AstStmtSPtr& node)
	{
		AstStmt* pStmt = node.get();
		switch (pStmt->stmtKind)
		{
		case AstStmtKind::Decl: Visit(*reinterpret_cast<AstDeclSPtr*>(&node)); break;
		case AstStmtKind::Import: Visit(*static_cast<AstImportStmt*>(node.get())); break;
		case AstStmtKind::Block: Visit(*static_cast<AstBlockStmt*>(node.get())); break;
		case AstStmtKind::If: Visit(*static_cast<AstIfStmt*>(node.get())); break;
		case AstStmtKind::Loop: Visit(*static_cast<AstLoopStmt*>(node.get())); break;
		case AstStmtKind::While: Visit(*static_cast<AstWhileStmt*>(node.get())); break;
		case AstStmtKind::DoWhile: Visit(*static_cast<AstDoWhileStmt*>(node.get())); break;
		case AstStmtKind::For: Visit(*static_cast<AstForStmt*>(node.get())); break;
		case AstStmtKind::Switch: Visit(*static_cast<AstSwitchStmt*>(node.get())); break;
		case AstStmtKind::Label: Visit(*static_cast<AstLabelStmt*>(node.get())); break;
		case AstStmtKind::Break: Visit(*static_cast<AstBreakStmt*>(node.get())); break;
		case AstStmtKind::Continue: Visit(*static_cast<AstContinueStmt*>(node.get())); break;
		case AstStmtKind::Fallthrough: Visit(*static_cast<AstFallthroughStmt*>(node.get())); break;
		case AstStmtKind::Goto: Visit(*static_cast<AstGotoStmt*>(node.get())); break;
		case AstStmtKind::Return: Visit(*static_cast<AstReturnStmt*>(node.get())); break;
		case AstStmtKind::Throw: Visit(*static_cast<AstThrowStmt*>(node.get())); break;
		case AstStmtKind::Expr: Visit(*static_cast<AstExprStmt*>(node.get())); break;
		case AstStmtKind::Defer: Visit(*static_cast<AstDeferStmt*>(node.get())); break;
		case AstStmtKind::ErrDefer: Visit(*static_cast<AstErrDeferStmt*>(node.get())); break;
		case AstStmtKind::Unsafe: Visit(*static_cast<AstUnsafeStmt*>(node.get())); break;
		case AstStmtKind::ErrorHandler: Visit(*static_cast<AstErrorHandlerStmt*>(node.get())); break;
		case AstStmtKind::CompIf: Visit(*static_cast<AstCompIfStmt*>(node.get())); break;
		case AstStmtKind::CompCond: Visit(*static_cast<AstCompCondStmt*>(node.get())); break;
		case AstStmtKind::CompDebug: Visit(*static_cast<AstCompDebugStmt*>(node.get())); break;
		case AstStmtKind::MacroLoop: Visit(*static_cast<AstMacroLoopStmt*>(node.get())); break;
		case AstStmtKind::MacroInst: Visit(*static_cast<AstMacroInstStmt*>(node.get())); break;
		default:
			break;
		}
	}

	void AstVisitor::Visit(AstDeclSPtr& node)
	{
		switch (node->declKind)
		{
		case AstDeclKind::Module: Visit(*static_cast<AstModuleDecl*>(node.get())); break;
		case AstDeclKind::UnitTest: Visit(*static_cast<AstUnittestDecl*>(node.get())); break;
		case AstDeclKind::Benchmark: Visit(*static_cast<AstBenchmarkDecl*>(node.get())); break;
		case AstDeclKind::Struct: Visit(*static_cast<AstStructDecl*>(node.get())); break;
		case AstDeclKind::Union: Visit(*static_cast<AstUnionDecl*>(node.get())); break;
		case AstDeclKind::ValueEnum: Visit(*static_cast<AstValueEnumDecl*>(node.get())); break;
		case AstDeclKind::AdtEnum: Visit(*static_cast<AstAdtEnumDecl*>(node.get())); break;
		case AstDeclKind::MarkerInterface: Visit(*static_cast<AstMarkerInterfaceDecl*>(node.get())); break;
		case AstDeclKind::WeakInterface: Visit(*static_cast<AstWeakInterfaceDecl*>(node.get())); break;
		case AstDeclKind::StrongInterface: Visit(*static_cast<AstStrongInterfaceDecl*>(node.get())); break;
		case AstDeclKind::Typealias: Visit(*static_cast<AstTypeAliasDecl*>(node.get())); break;
		case AstDeclKind::Typedef: Visit(*static_cast<AstTypeDefDecl*>(node.get())); break;
		case AstDeclKind::Var: Visit(*static_cast<AstVarDecl*>(node.get())); break;
		case AstDeclKind::Func: Visit(*static_cast<AstFuncDecl*>(node.get())); break;
		case AstDeclKind::Method: Visit(*static_cast<AstMethodDecl*>(node.get())); break;
		case AstDeclKind::EmptyMethod: Visit(*static_cast<AstEmptyMethodDecl*>(node.get())); break;
		case AstDeclKind::Impl: Visit(*static_cast<AstImplDecl*>(node.get())); break;
		case AstDeclKind::DeclMacro: Visit(*static_cast<AstDeclMacro*>(node.get())); break;
		case AstDeclKind::RulesDeclMacro: Visit(*static_cast<AstRulesDeclMacro*>(node.get())); break;
		case AstDeclKind::ProcMacro: Visit(*static_cast<AstProcMacro*>(node.get())); break;
		case AstDeclKind::RulesProcMacro: Visit(*static_cast<AstRulesProcMacro*>(node.get())); break;
		default:
			break;
		}
	}

	void AstVisitor::Visit(AstExprSPtr& node)
	{
		switch (node->exprKind)
		{
		case AstExprKind::Assign: Visit(*static_cast<AstAssignExpr*>(node.get())); break;
		case AstExprKind::Ternary: Visit(*static_cast<AstTernaryExpr*>(node.get())); break;
		case AstExprKind::Binary: Visit(*static_cast<AstBinaryExpr*>(node.get())); break;
		case AstExprKind::Postfix: Visit(*static_cast<AstPostfixExpr*>(node.get())); break;
		case AstExprKind::Prefix: Visit(*static_cast<AstPrefixExpr*>(node.get())); break;
		case AstExprKind::QualName: Visit(*static_cast<AstQualNameExpr*>(node.get())); break;
		case AstExprKind::IndexSlice: Visit(*static_cast<AstIndexSliceExpr*>(node.get())); break;
		case AstExprKind::Slice: Visit(*static_cast<AstSliceExpr*>(node.get())); break;
		case AstExprKind::FuncCall: Visit(*static_cast<AstFuncCallExpr*>(node.get())); break;
		case AstExprKind::MemberAccess: Visit(*static_cast<AstMemberAccessExpr*>(node.get())); break;
		case AstExprKind::MethodCall: Visit(*static_cast<AstMethodCallExpr*>(node.get())); break;
		case AstExprKind::TupleAccess: Visit(*static_cast<AstTupleAccessExpr*>(node.get())); break;
		case AstExprKind::Literal: Visit(*static_cast<AstLiteralExpr*>(node.get())); break;
		case AstExprKind::AggrInit: Visit(*static_cast<AstAggrInitExpr*>(node.get())); break;
		case AstExprKind::TupleInit: Visit(*static_cast<AstTupleInitExpr*>(node.get())); break;
		case AstExprKind::ArrayInit: Visit(*static_cast<AstArrayInitExpr*>(node.get())); break;
		case AstExprKind::Cast: Visit(*static_cast<AstCastExpr*>(node.get())); break;
		case AstExprKind::Transmute: Visit(*static_cast<AstTransmuteExpr*>(node.get())); break;
		case AstExprKind::Move: Visit(*static_cast<AstMoveExpr*>(node.get())); break;
		case AstExprKind::Bracket: Visit(*static_cast<AstBracketExpr*>(node.get())); break;
		case AstExprKind::Block: Visit(*static_cast<AstBlockExpr*>(node.get())); break;
		case AstExprKind::Unsafe: Visit(*static_cast<AstUnsafeExpr*>(node.get())); break;
		case AstExprKind::Comma: Visit(*static_cast<AstCommaExpr*>(node.get())); break;
		case AstExprKind::Closure: Visit(*static_cast<AstClosureExpr*>(node.get())); break;
		case AstExprKind::Is: Visit(*static_cast<AstIsExpr*>(node.get())); break;
		case AstExprKind::CompRun: Visit(*static_cast<AstCompRunExpr*>(node.get())); break;
		case AstExprKind::MacroVar: Visit(*static_cast<AstMacroVarExpr*>(node.get())); break;
		case AstExprKind::MacroInst: Visit(*static_cast<AstMacroInstExpr*>(node.get())); break;
		default: ;
		}
	}

	void AstVisitor::Visit(AstTypeSPtr& node)
	{
		switch (node->typeKind)
		{
		case AstTypeKind::Builtin: Visit(*static_cast<AstBuiltinType*>(node.get())); break;
		case AstTypeKind::Iden: Visit(*static_cast<AstIdentifierType*>(node.get())); break;
		case AstTypeKind::Ptr: Visit(*static_cast<AstPointerType*>(node.get())); break;
		case AstTypeKind::Ref: Visit(*static_cast<AstReferenceType*>(node.get())); break;
		case AstTypeKind::Arr: Visit(*static_cast<AstArrayType*>(node.get())); break;
		case AstTypeKind::Slice: Visit(*static_cast<AstSliceType*>(node.get())); break;
		case AstTypeKind::Tuple: Visit(*static_cast<AstTupleType*>(node.get())); break;
		case AstTypeKind::Optional: Visit(*static_cast<AstOptionalType*>(node.get())); break;
		case AstTypeKind::InlineStruct: Visit(*static_cast<AstInlineStructType*>(node.get())); break;
		case AstTypeKind::InlineEnum: Visit(*static_cast<AstInlineEnumType*>(node.get())); break;
		case AstTypeKind::CompoundInterface: Visit(*static_cast<AstCompoundInterfaceType*>(node.get())); break;
		default: ;
		}
	}

	void AstVisitor::Visit(AstPatternSPtr& node)
	{
		switch (node->patternKind)
		{
		case AstPatternKind::Placeholder: Visit(*static_cast<AstPlaceholderPattern*>(node.get())); break;
		case AstPatternKind::Wildcard: Visit(*static_cast<AstWildcardPattern*>(node.get())); break;
		case AstPatternKind::ValueBind: Visit(*static_cast<AstValueBindPattern*>(node.get())); break;
		case AstPatternKind::Literal: Visit(*static_cast<AstLiteralPattern*>(node.get())); break;
		case AstPatternKind::Range: Visit(*static_cast<AstRangePattern*>(node.get())); break;
		case AstPatternKind::Tuple: Visit(*static_cast<AstTuplePattern*>(node.get())); break;
		case AstPatternKind::Enum: Visit(*static_cast<AstEnumPattern*>(node.get())); break;
		case AstPatternKind::Aggr: Visit(*static_cast<AstAggrPattern*>(node.get())); break;
		case AstPatternKind::Slice: Visit(*static_cast<AstSlicePattern*>(node.get())); break;
		case AstPatternKind::Either: Visit(*static_cast<AstEitherPattern*>(node.get())); break;
		case AstPatternKind::Type: Visit(*static_cast<AstTypePattern*>(node.get())); break;
		case AstPatternKind::MacroInst: Visit(*static_cast<AstMacroInstPattern*>(node.get())); break;
		default:;
		}
	}

	void AstVisitor::Walk(AstTree& tree)
	{
		for (AstStmtSPtr node : tree.nodes)
		{
			Visit(node);
		}
	}

	void AstVisitor::Walk(AstIden& node)
	{
		for (AstGenericArg& arg : node.args)
		{
			if (arg.kind == GenericArgKind::Type)
				Visit(arg.type);
			else
				Visit(arg.expr);
		}
	}

	void AstVisitor::Walk(AstTypeDisambiguation& node)
	{
		Visit(node.type);
		Visit(*node.interface);
	}

	void AstVisitor::Walk(AstQualName& node)
	{
		for (AstQualIdenSPtr iden : node.idens)
		{
			Visit(iden);
		}
	}

	void AstVisitor::Walk(AstParam& node)
	{
		for (AstParamVarSPtr var : node.vars)
		{
			if (var->attribs)
				Visit(*var->attribs);
		}
		
		if (node.type)
			Visit(node.type);
	}

	void AstVisitor::Walk(AstArg& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstModuleDecl& node)
	{
	}

	void AstVisitor::Walk(AstUnittestDecl& node)
	{
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstBenchmarkDecl& node)
	{
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstStructDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstStmtSPtr member : node.members)
		{
			Visit(member);
		}
	}

	void AstVisitor::Walk(AstUnionDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstStmtSPtr member : node.members)
		{
			Visit(member);
		}
	}

	void AstVisitor::Walk(AstValueEnumDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.baseType)
			Visit(node.baseType);
		for (StdPair<StdString, AstExprSPtr> member: node.members)
		{
			if (member.second)
				Visit(member.second);
		}
	}

	void AstVisitor::Walk(AstAdtEnumDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (StdPair<StdString, AstTypeSPtr>& member : node.members)
		{
			if (member.second)
				Visit(member.second);
		}
	}

	void AstVisitor::Walk(AstMarkerInterfaceDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
	}

	void AstVisitor::Walk(AstWeakInterfaceDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		for (AstStmtSPtr member : node.members)
		{
			Visit(member);
		}
	}

	void AstVisitor::Walk(AstStrongInterfaceDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstStmtSPtr member : node.members)
		{
			Visit(member);
		}
	}

	void AstVisitor::Walk(AstTypeAliasDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		if (node.type)
			Visit(node.type);
	}

	void AstVisitor::Walk(AstTypeDefDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		Visit(node.type);
	}

	void AstVisitor::Walk(AstVarDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.type)
			Visit(node.type);
		if (node.expr)
			Visit(node.expr);
	}

	void AstVisitor::Walk(AstFuncDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstParamSPtr param : node.params)
		{
			Visit(*param);
		}
		if (node.errorType)
			Visit(node.errorType);
		if (node.retType)
			Visit(node.retType);
		for (StdPair<StdVector<StdString>, AstTypeSPtr>& namedRet : node.namedRet)
		{
			Visit(namedRet.second);
		}
		if (node.whereClause)
			Visit(*node.whereClause);
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstMethodDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstParamSPtr param : node.params)
		{
			Visit(*param);
		}
		if (node.errorType)
			Visit(node.errorType);
		if (node.retType)
			Visit(node.retType);
		for (StdPair<StdVector<StdString>, AstTypeSPtr>& namedRet : node.namedRet)
		{
			Visit(namedRet.second);
		}
		if (node.whereClause)
			Visit(*node.whereClause);
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstEmptyMethodDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstParamSPtr param : node.params)
		{
			Visit(*param);
		}
		if (node.retType)
			Visit(node.retType);
	}

	void AstVisitor::Walk(AstImplDecl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		Visit(node.type);
		if (node.interface)
			Visit(*node.interface);
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstImportStmt& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
	}

	void AstVisitor::Walk(AstBlockStmt& node)
	{
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstIfStmt& node)
	{
		if (node.decl)
			Visit(*node.decl);
		Visit(node.cond);
		Visit(node.body);
		if (node.elseBody)
			Visit(node.elseBody);
	}

	void AstVisitor::Walk(AstLoopStmt& node)
	{
		if (node.label)
			Visit(*node.label);
		Visit(node.body);
	}

	void AstVisitor::Walk(AstWhileStmt& node)
	{
		if (node.label)
			Visit(*node.label);
		Visit(node.cond);
		Visit(node.body);
	}

	void AstVisitor::Walk(AstDoWhileStmt& node)
	{
		if (node.label)
			Visit(*node.label);
		Visit(node.body);
		Visit(node.cond);
	}

	void AstVisitor::Walk(AstForStmt& node)
	{
		if (node.label)
			Visit(*node.label);
		Visit(node.range);
		Visit(node.body);
	}

	void AstVisitor::Walk(AstSwitchStmt& node)
	{
		if (node.label)
			Visit(*node.label);
		for (AstSwitchCase& case_ : node.cases)
		{
			Visit(case_.pattern);
			if (case_.expr)
				Visit(case_.expr);
			Visit(case_.body);
		}
		Visit(node.cond);
	}

	void AstVisitor::Walk(AstLabelStmt& node)
	{
	}

	void AstVisitor::Walk(AstBreakStmt& node)
	{
	}

	void AstVisitor::Walk(AstContinueStmt& node)
	{
	}

	void AstVisitor::Walk(AstFallthroughStmt& node)
	{
	}

	void AstVisitor::Walk(AstGotoStmt& node)
	{
	}

	void AstVisitor::Walk(AstReturnStmt& node)
	{
		if (node.expr)
			Visit(node.expr);
	}
	
	void AstVisitor::Walk(AstThrowStmt& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstExprStmt& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstDeferStmt& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstErrDeferStmt& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstUnsafeStmt& node)
	{
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstErrorHandlerStmt& node)
	{
		if (node.errType)
			Visit(node.errType);
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstCompIfStmt& node)
	{
		if (node.decl)
			Visit(*node.decl);
		Visit(node.cond);
		Visit(node.body);
		if (node.elseBody)
			Visit(node.elseBody);
	}

	void AstVisitor::Walk(AstCompCondStmt& node)
	{
		Visit(node.body);
		if (node.elseBody)
			Visit(node.elseBody);
	}

	void AstVisitor::Walk(AstCompDebugStmt& node)
	{
		Visit(node.body);
		if (node.elseBody)
			Visit(node.elseBody);
	}

	void AstVisitor::Walk(AstMacroLoopStmt& node)
	{
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstAssignExpr& node)
	{
		Visit(node.lExpr);
		Visit(node.rExpr);
	}

	void AstVisitor::Walk(AstTernaryExpr& node)
	{
		Visit(node.cond);
		Visit(node.trueExpr);
		Visit(node.falseExpr);
	}

	void AstVisitor::Walk(AstBinaryExpr& node)
	{
		Visit(node.lExpr);
		Visit(node.rExpr);
	}

	void AstVisitor::Walk(AstPostfixExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstPrefixExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstQualNameExpr& node)
	{
		Visit(*node.qualName);
	}

	void AstVisitor::Walk(AstIndexSliceExpr& node)
	{
		Visit(node.expr);
		Visit(node.index);
	}

	void AstVisitor::Walk(AstSliceExpr& node)
	{
		Visit(node.expr);
		if (node.begin)
			Visit(node.begin);
		if (node.end)
			Visit(node.end);
	}

	void AstVisitor::Walk(AstFuncCallExpr& node)
	{
		Visit(node.func);
		for (AstArgSPtr arg : node.args)
		{
			Visit(*arg);
		}
	}

	void AstVisitor::Walk(AstMemberAccessExpr& node)
	{
		Visit(node.caller);
	}

	void AstVisitor::Walk(AstMethodCallExpr& node)
	{
		Visit(node.caller);
		for (AstArgSPtr arg : node.args)
		{
			Visit(*arg);
		}
	}

	void AstVisitor::Walk(AstTupleAccessExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstLiteralExpr& node)
	{
	}

	void AstVisitor::Walk(AstAggrInitExpr& node)
	{
		Visit(node.type);
		for (AstArgSPtr arg : node.args)
		{
			Visit(*arg);
		}
		if (node.defExpr)
			Visit(node.defExpr);
	}

	void AstVisitor::Walk(AstTupleInitExpr& node)
	{
		for (AstExprSPtr expr : node.exprs)
		{
			Visit(expr);
		}
	}

	void AstVisitor::Walk(AstArrayInitExpr& node)
	{
		for (AstExprSPtr expr : node.exprs)
		{
			Visit(expr);
		}
	}

	void AstVisitor::Walk(AstCastExpr& node)
	{
		Visit(node.type);
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstTransmuteExpr& node)
	{
		Visit(node.type);
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstMoveExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstBracketExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstBlockExpr& node)
	{
		for (AstStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void AstVisitor::Walk(AstUnsafeExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstCommaExpr& node)
	{
		for (AstExprSPtr expr : node.exprs)
		{
			Visit(expr);
		}
	}

	void AstVisitor::Walk(AstClosureExpr& node)
	{
		for (AstParamSPtr param : node.params)
		{
			Visit(*param);
		}
		if (node.ret)
			Visit(node.ret);
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstIsExpr& node)
	{
		if (node.expr)
			Visit(node.expr);
		Visit(node.type);
	}

	void AstVisitor::Walk(AstTryExpr& node)
	{
		Visit(node.call);
	}

	void AstVisitor::Walk(AstSpecKwExpr& node)
	{
	}

	void AstVisitor::Walk(AstCompRunExpr& node)
	{
		Visit(node.expr);
	}

	void AstVisitor::Walk(AstMacroVarExpr& node)
	{
	}

	void AstVisitor::Walk(AstBuiltinType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
	}

	void AstVisitor::Walk(AstIdentifierType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		Visit(*node.qualName);
	}

	void AstVisitor::Walk(AstPointerType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		Visit(node.subType);
	}

	void AstVisitor::Walk(AstReferenceType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		Visit(node.subType);
	}

	void AstVisitor::Walk(AstArrayType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		Visit(node.arraySize);
		Visit(node.subType);
	}

	void AstVisitor::Walk(AstSliceType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		Visit(node.subType);
	}

	void AstVisitor::Walk(AstTupleType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		for (AstTypeSPtr subType : node.subTypes)
		{
			Visit(subType);
		}
	}

	void AstVisitor::Walk(AstOptionalType& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		Visit(node.subType);
	}

	void AstVisitor::Walk(AstInlineStructType& node)
	{
		for (StdPair<StdVector<StdString>, AstTypeSPtr>& member : node.members)
		{
			Visit(member.second);
		}
	}

	void AstVisitor::Walk(AstInlineEnumType& node)
	{
	}

	void AstVisitor::Walk(AstCompoundInterfaceType& node)
	{
		for (AstIdentifierTypeSPtr interface : node.interfaces)
		{
			Walk(*interface);
		}
	}

	void AstVisitor::Walk(AstPlaceholderPattern& node)
	{
	}

	void AstVisitor::Walk(AstWildcardPattern& node)
	{
	}

	void AstVisitor::Walk(AstValueBindPattern& node)
	{
		if (node.subPattern)
			Visit(node.subPattern);
	}

	void AstVisitor::Walk(AstLiteralPattern& node)
	{
	}

	void AstVisitor::Walk(AstRangePattern& node)
	{
	}

	void AstVisitor::Walk(AstTuplePattern& node)
	{
		for (AstPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void AstVisitor::Walk(AstEnumPattern& node)
	{
		Visit(*node.qualName);
		for (AstPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void AstVisitor::Walk(AstAggrPattern& node)
	{
		Visit(*node.qualName);
		for (StdPair<StdString, AstPatternSPtr>& member : node.subPatterns)
		{
			Visit(member.second);
		}
	}

	void AstVisitor::Walk(AstSlicePattern& node)
	{
		for (AstPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void AstVisitor::Walk(AstEitherPattern& node)
	{
		for (AstPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void AstVisitor::Walk(AstTypePattern& node)
	{
		Visit(node.type);
	}

	void AstVisitor::Walk(AstAttribs& node)
	{
		for (AstCompAttribSPtr compAttrib : node.compAttribs)
		{
			Visit(*compAttrib);
		}
		for (AstUserAttribSPtr userAttrib : node.userAttribs)
		{
			Visit(*userAttrib);
		}
		if (node.visibility)
			Visit(*node.visibility);
		for (AstSimpleAttribSPtr simpleAttrib : node.simpleAttribs)
		{
			Visit(*simpleAttrib);
		}
	}

	void AstVisitor::Walk(AstCompAttrib& node)
	{
		for (AstArgSPtr arg : node.args)
		{
			Visit(*arg);
		}
	}

	void AstVisitor::Walk(AstUserAttrib& node)
	{
		for (AstArgSPtr arg : node.args)
		{
			Visit(*arg);
		}
	}

	void AstVisitor::Walk(AstVisibilityAttrib& node)
	{
	}

	void AstVisitor::Walk(AstSimpleAttrib& node)
	{
	}

	void AstVisitor::Walk(AstGenericDecl& node)
	{
		for (AstGenericParam& param : node.params)
		{
			switch (param.kind)
			{
			case AstGenericParamKind::TypeParam: Visit(*param.typeParam); break;
			case AstGenericParamKind::ValueParam: Visit(*param.valueParam); break;
			case AstGenericParamKind::TypeSpec: Visit(param.typeSpec); break;
			case AstGenericParamKind::ValueSpec: Visit(param.valueSpec); break;
			default: ;
			}
		}
	}

	void AstVisitor::Walk(AstGenericTypeParam& node)
	{
		for (AstIdentifierTypeSPtr implType : node.implTypes)
		{
			Visit(*implType);
		}
		if (node.defType)
			Visit(node.defType);
	}

	void AstVisitor::Walk(AstGenericValueParam& node)
	{
		Visit(node.type);
		if (node.defExpr)
			Visit(node.defExpr);
	}

	void AstVisitor::Walk(AstGenericTypeBound& node)
	{
		Visit(node.type);
		Visit(*node.bound);
	}

	void AstVisitor::Walk(AstGenericAssocTypeBound& node)
	{
		Visit(*node.type);
	}

	void AstVisitor::Walk(AstGenericBoundType& node)
	{
		Visit(node.type);
		for (AstGenericAssocTypeBound& assocBound : node.assocBounds)
		{
			Visit(assocBound);
		}
	}

	void AstVisitor::Walk(AstGenericWhereClause& node)
	{
		for (AstGenericTypeBoundSPtr bound : node.bounds)
		{
			Visit(*bound);
		}
	}

	void AstVisitor::Walk(AstMacroVar& node)
	{
	}

	void AstVisitor::Walk(AstMacroSeparator& node)
	{
	}

	void AstVisitor::Walk(AstMacroFragment& node)
	{
		Visit(*node.subPattern);
	}

	void AstVisitor::Walk(AstMacroPattern& node)
	{
		for (AstMacroPatternElemSPtr elem : node.elems)
		{
			switch (elem->elemKind)
			{
			case AstMacroPatternElemKind::Variable: Visit(*static_cast<AstMacroVar*>(elem.get())); break;
			case AstMacroPatternElemKind::Separator: Visit(*static_cast<AstMacroSeparator*>(elem.get())); break;
			case AstMacroPatternElemKind::Fragment:  Visit(*static_cast<AstMacroFragment*>(elem.get())); break;
			default: ;
			}
		}
	}

	void AstVisitor::Walk(AstMacroRule& node)
	{
		Visit(*node.pattern);
	}

	void AstVisitor::Walk(AstDeclMacro& node)
	{
		Visit(*node.pattern);
	}

	void AstVisitor::Walk(AstRulesDeclMacro& node)
	{
		for (AstMacroRuleSPtr rule : node.rules)
		{
			Visit(*rule);
		}
	}

	void AstVisitor::Walk(AstProcMacro& node)
	{
		Visit(*node.pattern);
	}

	void AstVisitor::Walk(AstRulesProcMacro& node)
	{
		for (AstMacroRuleSPtr rule : node.rules)
		{
			Visit(*rule);
		}
	}

	void AstVisitor::Walk(AstMacroInstStmt& node)
	{
		Visit(*node.qualName);
		if (node.expandedStmt)
			Visit(node.expandedStmt);
	}

	void AstVisitor::Walk(AstMacroInstExpr& node)
	{
		Visit(*node.qualName);
		if (node.expandedExpr)
			Visit(node.expandedExpr);
	}

	void AstVisitor::Walk(AstMacroInstPattern& node)
	{
		Visit(*node.qualName);
		if (node.expandedPattern)
			Visit(node.expandedPattern);
	}
}
