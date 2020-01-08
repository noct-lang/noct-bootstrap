#pragma once
#include "common/defs.hpp"
#include <memory>
#include "comp/token.hpp"

namespace Noctis
{
	class AstVisitor;
	
	struct AstContext
	{
		
	};

	enum class AstNodeKind : u8
	{
		ModuleDecl,
		UnittestDecl, // TODO
		BenchmarkDecl, // TODO

		Param,
		Arg,

		DeclStartMarker,
		StructDecl = DeclStartMarker,
		UnionDecl,
		ValueEnumDecl,
		AdtEnumDecl,
		MarkerInterfaceDecl,
		WeakInterfaceDecl,
		StrongInterfaceDecl,
		TypeAliasDecl,
		TypeDefDecl,
		VarDecl,
		FuncDecl,
		MethodDecl,
		ImplDecl,
		DeclEndMarker = ImplDecl,

		StmtStartMarker,
		ImportStmt = StmtStartMarker,
		BlockStmt,
		IfStmt,
		LoopStmt,
		WhileStmt,
		DoWhileStmt,
		ForStmt,
		ForRangeStmt,
		SwitchStmt,
		LabelStmt,
		BreakStmt,
		ContinueStmt,
		FallthroughStmt,
		GotoStmt,
		ReturnStmt,
		ExprStmt,
		DeferStmt,
		StackDeferStmt,
		UnsafeStmt,
		CompIfStmt,
		CompForStmt,
		CompCondStmt,
		CompDebugStmt,
		StmtEndMarker,

		ExprStartMarker,
		AssignExpr = ExprStartMarker,
		TernaryExpr,
		BinaryExpr,
		PostFixExpr,
		PrefixExpr,
		QualNameExpr,
		IndexSliceExpr,
		SliceExpr,
		FuncCallExpr,
		MemberAccessExpr,
		MethodCallExpr,
		TupleAccessExpr,
		LiteralExpr,
		AggrInitExpr,
		TupleInitExpr,
		ArrayInitExpr,
		CastExpr,
		TransmuteExpr,
		MoveExpr,
		BracketExpr,
		BlockExpr,
		UnsafeExpr,
		CommaExpr,
		VoidExpr,
		ClosureExpr,
		CompRunExpr,
		MacroExpExpr,
		ExprEndMarker = CompRunExpr,

		TypeStartMarker,
		BuiltinType = TypeStartMarker,
		IdentifierType,
		PointerType,
		ReferenceType,
		ArrayType,
		SliceType,
		TupleType,
		OptionalType,
		TypeEndMarker = OptionalType,

		AttributeStartMarker,
		Attributes = AttributeStartMarker,
		CompilerAttribute,
		UserAttribute,
		VisibilityAttribute,
		SimpleAttribute,
		AttributeEndMaprker = SimpleAttribute,

		GenericsStartMarker,
		GenericsDecl,
		GenericsTypeParam,
		GenericTypeSpec,
		GenericValueSpec,
		GenericValueParam,
		GenericWhereClause,
		GenericInst,
		GenericsEndMarker,
		
		MacroStartMarker,
		MacroVar,
		MacroSeparator,
		MacroFragment,
		MacroPattern,
		MacroRule,
		DeclMacro,
		RulesDeclMacro,
		ProcMacro,
		RulesProcMacro,
		MacroInst,
		MacroEndMarker,
		
	};
	
	struct AstNode
	{
		AstNode(AstNodeKind nodeKind, u64 startTokIdx, u64 endTokIdx);
		virtual ~AstNode();
		
		virtual void Visit(AstVisitor& visitor);

		
		
		AstNodeKind nodeKind;
		u64 startTokIdx;
		u64 endTokIdx;

		std::unique_ptr<AstContext> pCtx;
	};

	using AstNodeSPtr = StdSharedPtr<AstNode>;

	struct AstModuleDecl : public AstNode
	{
		AstModuleDecl(u64 importTokIdx, StdVector<StdString>&& moduleIdens, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		
		StdVector<StdString> moduleIdens;
	};

	struct AstParam : public AstNode
	{
		AstParam(u64 startTokIdx, StdVector<StdString>&& idens, AstNodeSPtr type, bool isVariadic, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
		AstNodeSPtr type;
		bool isVariadic;
	};

	struct AstArg : public AstNode
	{
		AstArg(u64 startTokIdx, StdString&& iden, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		
		StdString iden;
		AstNodeSPtr expr;
	};

	struct AstStructDecl : public AstNode
	{
		AstStructDecl(AstNodeSPtr attribs, u64 structTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> members;
	};

	struct AstUnionDecl : public AstNode
	{
		AstUnionDecl(AstNodeSPtr attribs, u64 unionTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> members;
	};

	struct AstValueEnumDecl : public AstNode
	{
		AstValueEnumDecl(AstNodeSPtr attribs, u64 enumTokIdx, StdString&& iden, StdVector<std::pair<StdString, AstNodeSPtr>>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		StdVector<std::pair<StdString, AstNodeSPtr>> members;
	};
	
	struct AstAdtEnumDecl : public AstNode
	{
		AstAdtEnumDecl(AstNodeSPtr attribs, u64 enumTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<std::pair<StdString, AstNodeSPtr>>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<std::pair<StdString, AstNodeSPtr>> members;
	};

	struct AstMarkerInterfaceDecl : public AstNode
	{
		AstMarkerInterfaceDecl(AstNodeSPtr attribs, u64 interfaceTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
	};

	struct AstWeakInterfaceDecl : public AstNode
	{
		AstWeakInterfaceDecl(AstNodeSPtr attribs, u64 weakTokIddx, StdString&& iden, StdVector<AstNodeSPtr>&& members, u64 rBraceToken);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		StdVector<AstNodeSPtr> members;
	};
	
	struct AstStrongInterfaceDecl : public AstNode
	{
		AstStrongInterfaceDecl(AstNodeSPtr attribs, u64 interfaceTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> members;
	};

	struct AstTypeAliasDecl : public AstNode
	{
		AstTypeAliasDecl(AstNodeSPtr attribs, u64 typealiasTokIdx, StdString&& iden, AstNodeSPtr generics, AstNodeSPtr type, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		AstNodeSPtr type;
	};

	struct AstTypeDefDecl : public AstNode
	{
		AstTypeDefDecl(AstNodeSPtr attribs, u64 typedefTokIdx, StdString&& iden, AstNodeSPtr generics, AstNodeSPtr type, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		AstNodeSPtr type;
	};

	struct AstVarDecl : public AstNode
	{
		AstVarDecl(AstNodeSPtr attribs, u64 startTokIdx, StdVector<StdString>&& idens, AstNodeSPtr type, AstNodeSPtr expr, u64 termTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdVector<StdString> idens;
		AstNodeSPtr type;
		AstNodeSPtr expr;
	};

	struct AstFuncDecl : public AstNode
	{
		AstFuncDecl(AstNodeSPtr attribs, u64 funcTokIdx, StdString&& identifier, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& args, AstNodeSPtr ret, AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> args;
		AstNodeSPtr ret;
		AstNodeSPtr whereClause;
		StdVector<AstNodeSPtr> statements;
	};

	enum class AstMethodReceiverKind
	{
		None,
		Value,
		Ref,
		ConstRef
	};
	
	struct AstMethodDecl : public AstNode
	{
		AstMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec, StdString&& identifier, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx, bool hasBody);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		AstMethodReceiverKind rec;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> params;
		AstNodeSPtr ret;
		AstNodeSPtr whereClause;
		StdVector<AstNodeSPtr> statements;
		bool hasBody;
	};

	struct AstImplDecl : public AstNode
	{
		AstImplDecl(AstNodeSPtr attribs, u64 implTokIdx, AstNodeSPtr type, StdVector<AstNodeSPtr>&& interfaces, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr attribs;
		AstNodeSPtr type;
		StdVector<AstNodeSPtr> interfaces;
		StdVector<AstNodeSPtr> statements;
	};

	struct AstImportStmt : public AstNode
	{
		AstImportStmt(u64 moduleTokIds, StdVector<StdString>&& moduleIdens, StdVector<std::pair<StdVector<StdString>, StdString>>&& symbols, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> moduleIdens;
		StdVector<std::pair<StdVector<StdString>, StdString>> symbols;
	};

	struct AstBlockStmt : public AstNode
	{
		AstBlockStmt(u64 lBraceTokIds, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> statements;
	};

	struct AstIfStmt : public AstNode
	{
		AstIfStmt(u64 ifTokIdx, AstNodeSPtr decl, AstNodeSPtr cond, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr decl;
		AstNodeSPtr cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstLoopStmt : public AstNode
	{
		AstLoopStmt(u64 loopTokIdx, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr body;
	};

	struct AstWhileStmt : public AstNode
	{
		AstWhileStmt(u64 whileTokIdx, AstNodeSPtr cond, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr cond;
		AstNodeSPtr body;
	};

	struct AstDoWhileStmt : public AstNode
	{
		AstDoWhileStmt(u64 whileTokIdx, AstNodeSPtr body, AstNodeSPtr cond);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr body;
		AstNodeSPtr cond;
	};

	struct AstForStmt : public AstNode
	{
		AstForStmt(u64 forTokIdx, AstNodeSPtr init, AstNodeSPtr cond, AstNodeSPtr inc, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr init;
		AstNodeSPtr cond;
		AstNodeSPtr inc;
		AstNodeSPtr body;
	};

	struct AstForRangeStmt : public AstNode
	{
		AstForRangeStmt(u64 forTokIdx, StdVector<StdString>&& idens, AstNodeSPtr range, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
		AstNodeSPtr range;
		AstNodeSPtr body;
	};

	struct AStSwitchCase
	{
		AstNodeSPtr staticExpr;
		AstNodeSPtr dynamicExpr;
		AstNodeSPtr body;
	};

	struct AstSwitchStmt : public AstNode
	{
		AstSwitchStmt(u64 switchTokIdx, StdVector<AStSwitchCase>&& cases, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AStSwitchCase> cases;
	};

	struct AstLabelStmt : public AstNode
	{
		AstLabelStmt(u64 startTokIdx, StdString&& iden, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
	};

	struct AstBreakStmt : public AstNode
	{
		AstBreakStmt(u64 breakTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
	};

	struct AstContinueStmt : public AstNode
	{
		AstContinueStmt(u64 continueTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
	};

	struct AstFallthroughStmt : public AstNode
	{
		AstFallthroughStmt(u64 fallthroughTokIdx, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
	};

	struct AstGotoStmt : public AstNode
	{
		AstGotoStmt(u64 gotoTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
	};

	struct AstReturnStmt : public AstNode
	{
		AstReturnStmt(u64 returnTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> exprs;
	};

	struct AstExprStmt : public AstNode
	{
		AstExprStmt(AstNodeSPtr expr, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
	};

	struct AstDeferStmt : public AstNode
	{
		AstDeferStmt(u64 deferTokIdx, AstNodeSPtr stmt);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr stmt;
	};

	struct AstStackDeferStmt : public AstNode
	{
		AstStackDeferStmt(u64 deferTokIdx, AstNodeSPtr stmt);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr stmt;
	};

	struct AstUnsafeStmt : public AstNode
	{
		AstUnsafeStmt(u64 unsafeTokIdx, StdVector<AstNodeSPtr>&& stmts, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> stmts;
	};

	struct AstCompIfStmt : public AstNode
	{
		AstCompIfStmt(u64 hashTokIdx, AstNodeSPtr decl, AstNodeSPtr expr, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr decl;
		AstNodeSPtr expr;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstCompForStmt : public AstNode
	{
		AstCompForStmt(u64 hashTokIdx, AstNodeSPtr init, AstNodeSPtr cond, AstNodeSPtr inc, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr init;
		AstNodeSPtr cond;
		AstNodeSPtr inc;
		AstNodeSPtr body;
	};

	struct AstCompCondStmt : public AstNode
	{
		AstCompCondStmt(u64 hashTokIdx, AstNodeSPtr cond, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstCompDebugStmt : public AstNode
	{
		AstCompDebugStmt(u64 hashTokIdx, AstNodeSPtr cond, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstAssignExpr : public AstNode
	{
		AstAssignExpr(AstNodeSPtr lExpr, TokenType op, AstNodeSPtr rExpr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr lExpr;
		TokenType op;
		AstNodeSPtr rExpr;
	};

	struct AstTernaryExpr : public AstNode
	{
		AstTernaryExpr(AstNodeSPtr cond, AstNodeSPtr trueExpr, AstNodeSPtr falseExpr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr cond;
		AstNodeSPtr trueExpr;
		AstNodeSPtr falseExpr;
	};

	struct AstBinaryExpr : public AstNode
	{
		AstBinaryExpr(AstNodeSPtr lExpr, TokenType op, AstNodeSPtr rExpr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr lExpr;
		TokenType op;
		AstNodeSPtr rExpr;
	};

	struct AstPostfixExpr : public AstNode
	{
		AstPostfixExpr(AstNodeSPtr expr, TokenType op, u64 opTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
		TokenType op;
	};

	struct AstPrefixExpr : public AstNode
	{
		AstPrefixExpr(TokenType op, u64 opTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;

		TokenType op;
		AstNodeSPtr expr;
	};

	struct AstQualNameExpr : public AstNode
	{
		AstQualNameExpr(u64 startTokIdx, StdVector<StdString>&& idens, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
	};

	struct AstIndexSliceExpr : public AstNode
	{
		AstIndexSliceExpr(AstNodeSPtr expr, AstNodeSPtr index, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
		AstNodeSPtr index;
	};

	struct AstSliceExpr : public AstNode
	{
		AstSliceExpr(AstNodeSPtr expr, AstNodeSPtr begin, AstNodeSPtr end, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
		AstNodeSPtr begin;
		AstNodeSPtr end;
	};

	// can be func, ADT enum member
	struct AstFuncCallExpr : public AstNode
	{
		AstFuncCallExpr(AstNodeSPtr func, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr func;
		StdVector<AstNodeSPtr> args;
	};

	struct AstMemberAccessExpr : public AstNode
	{
		AstMemberAccessExpr(AstNodeSPtr caller, StdString&& iden, u64 idenTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr caller;
		StdString iden;
	};

	struct AstMethodCallExpr : public AstNode
	{
		AstMethodCallExpr(AstNodeSPtr caller, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr caller;
		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	struct AstTupleAccessExpr : public AstNode
	{
		AstTupleAccessExpr(AstNodeSPtr expr, u16 index, u64 indexTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
		u16 index;
	};

	struct AstLiteralExpr : public AstNode
	{
		AstLiteralExpr(Token literal);

		void Visit(AstVisitor& visitor) override;
		
		Token literal;
	};

	// Can be: struct, union, ADT enum member
	struct AstAggrInitExpr : public AstNode
	{
		AstAggrInitExpr(u64 startTokIdx, StdVector<StdString>&& idens, StdVector<AstNodeSPtr>&& args, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
		StdVector<AstNodeSPtr> args;
	};

	struct AstTupleInitExpr : public AstNode
	{
		AstTupleInitExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> exprs;
	};

	struct AstArrayInitExpr : public AstNode
	{
		AstArrayInitExpr(u64 lBracketTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> exprs;
	};

	struct AstCastExpr : public AstNode
	{
		AstCastExpr(u64 castTokIdx, AstNodeSPtr type, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr type;
		AstNodeSPtr expr;
	};

	struct AstTransmuteExpr : public AstNode
	{
		AstTransmuteExpr(u64 transmuteTokIdx, AstNodeSPtr type, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr type;
		AstNodeSPtr expr;
	};

	struct AstMoveExpr : public AstNode
	{
		AstMoveExpr(u64 moveTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
	};

	struct AstBracketExpr : public AstNode
	{
		AstBracketExpr(u64 lBracketTokIdx, AstNodeSPtr expr, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
	};

	struct AstBlockExpr : public AstNode
	{
		AstBlockExpr(u64 lBraceTokIdx, StdVector<AstNodeSPtr>&& stmts, u64 rBraceExpr);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> stmts;
	};

	struct AstUnsafeExpr : public AstNode
	{
		AstUnsafeExpr(u64 unsafeTokIdx, AstNodeSPtr expr, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
	};

	struct AstVoidExpr : public AstNode
	{
		AstVoidExpr(u64 voidTokIdx);

		void Visit(AstVisitor& visitor) override;
	};

	struct AstClosureCapture
	{
		TokenType captureType;
		StdString iden;
	};

	struct AstClosureExpr : public AstNode
	{
		AstClosureExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& params, StdVector<AstClosureCapture>&& captures, StdVector<AstNodeSPtr> stmts, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> params;
		StdVector<AstClosureCapture> captures;
		StdVector<AstNodeSPtr> stmts;
	};

	struct AstCompRunExpr : public AstNode
	{
		AstCompRunExpr(u64 hashTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
	};

	struct AstBuiltinType : public AstNode
	{
		AstBuiltinType(const Token& tok);

		void Visit(AstVisitor& visitor) override;
		
		TokenType type;
	};

	struct AstIdentifierType : public AstNode
	{
		AstIdentifierType(u64 startTokIdx, StdVector<StdString>&& idens, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
	};

	struct AstPointerType : public AstNode
	{
		AstPointerType(u64 asteriskTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr subType;
	};

	struct AstReferenceType : public AstNode
	{
		AstReferenceType(u64 andTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr subType;
	};

	struct AstArrayType : public AstNode
	{
		AstArrayType(u64 lBracketTokIdx, AstNodeSPtr arraySize, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr arraySize;
		AstNodeSPtr subType;
	};

	struct AstSliceType : public AstNode
	{
		AstSliceType(u64 lBraceTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr subType;
	};

	struct AstTupleType : public AstNode
	{
		AstTupleType(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& subTypes, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> subTypes;
	};

	struct AstOptionalType : public AstNode
	{
		AstOptionalType(u64 lBraceTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr subType;
	};

	struct AstAttributes : public AstNode
	{
		AstAttributes(u64 startTokIdx, StdVector<AstNodeSPtr>&& compAttribs, StdVector<AstNodeSPtr>& userAttribs, AstNodeSPtr visibility, StdVector<AstNodeSPtr>&& simpleAttribs, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> compAttribs;
		StdVector<AstNodeSPtr> userAttribs;
		AstNodeSPtr visibility;
		StdVector<AstNodeSPtr> simpleAttribs;
	};

	struct AstCompAttribute : public AstNode
	{
		AstCompAttribute(u64 atColonTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx);
		
		void Visit(AstVisitor& visitor) override;

		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	struct AstUserAttribute : public AstNode
	{
		AstUserAttribute(u64 atColonTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	struct AstVisibilityAttribute : public AstNode
	{
		AstVisibilityAttribute(u64 publicTokIdx, TokenType subType, u64 endTokId);

		void Visit(AstVisitor& visitor) override;

		TokenType subType;
	};

	struct AstSimpleAttribute : public AstNode
	{
		AstSimpleAttribute(u64 tokIdx, TokenType attrib);

		void Visit(AstVisitor& visitor) override;

		TokenType attrib;
	};

	struct AstGenericDecl : public AstNode
	{
		AstGenericDecl(u64 startTokIdx, StdVector<AstNodeSPtr>&& params, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> params;
	};

	struct AstGenericTypeParam : public AstNode
	{
		AstGenericTypeParam(u64 idenTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& implTypes, AstNodeSPtr defType);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		StdVector<AstNodeSPtr> implTypes;
		AstNodeSPtr defType;
	};

	struct AstGenericValueParam : public AstNode
	{
		AstGenericValueParam(u64 idenTokIdx, StdString&& iden, AstNodeSPtr type, AstNodeSPtr defExpr);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		AstNodeSPtr type;
		AstNodeSPtr defExpr;
	};
	
	struct AstGenericWhereClause : public AstNode
	{
		AstGenericWhereClause(u64 whereTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr expr;
	};

	struct AstGenericInst : public AstNode
	{
		AstGenericInst(u64 startTokIdx, StdVector<StdString>&& idens, StdVector<AstNodeSPtr>&& args, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
		StdVector<AstNodeSPtr> args;
	};

	enum class AstMacroVarKind : u8
	{
		Stmt,
		Expr,
		Type,
		Qual,
		Attr,
		Toks
	};
	
	struct AstMacroVar : public AstNode
	{
		AstMacroVar(u64 dollarTokIdx, StdString&& iden, AstMacroVarKind kind, u64 kindTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		AstMacroVarKind kind;
	};

	struct AstMacroSeparator : public AstNode
	{
		AstMacroSeparator(StdVector<Token>&& toks);

		void Visit(AstVisitor& visitor) override;

		StdVector<Token> toks;
	};

	struct AstMacroFragment : public AstNode
	{
		AstMacroFragment(StdVector<AstNodeSPtr>&& subPatterns, AstNodeSPtr sep, TokenType repType, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<AstNodeSPtr> subPatterns;
		AstNodeSPtr sep;
		TokenType repType;
	};
	
	struct AstMacroPattern : public AstNode
	{
		AstMacroPattern(u64 startTokIdx, StdVector<AstNodeSPtr>&& elems, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		
		StdVector<AstNodeSPtr> elems;
	};

	struct AstMacroRule : public AstNode
	{
		AstMacroRule(u64 startTokIdx, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		AstNodeSPtr pattern;
		StdVector<AstNodeSPtr> body;
	};
	
	struct AstDeclMacro : public AstNode
	{
		AstDeclMacro(u64 macroTokIdx, StdString&& iden, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		AstNodeSPtr pattern;
		StdVector<AstNodeSPtr> body;
	};

	struct AstRulesDeclMacro : public AstNode
	{
		AstRulesDeclMacro(u64 macroTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& rules, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		StdVector<AstNodeSPtr> rules;
	};

	struct AstProcMacro : public AstNode
	{
		AstProcMacro(u64 macroTokIdx, StdString&& iden, StdString&& tokStreamIden, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		StdString tokStreamIden;
		AstNodeSPtr pattern;
		StdVector<AstNodeSPtr> body;
	};

	struct AstRulesProcMacro : public AstNode
	{
		AstRulesProcMacro(u64 macroTokIdx, StdString&& iden, StdString&& tokStreamIden, StdVector<AstNodeSPtr>&& rules, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdString iden;
		StdString tokStreamIden;
		StdVector<AstNodeSPtr> rules;
	};

	struct AstMacroInst : public AstNode
	{
		AstMacroInst(u64 startTokIdx, StdVector<StdString>&& idens, StdVector<Token>& toks, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;

		StdVector<StdString> idens;
		StdVector<Token> toks;
		StdVector<AstNodeSPtr> args;
	};
	
	struct AstTree
	{
		StdString filepath;
		StdVector<AstNodeSPtr> nodes;
	};

	
}
