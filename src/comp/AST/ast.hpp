#pragma once
#include "common/defs.hpp"
#include "comp/token.hpp"
#include "comp/qualname.hpp"

namespace Noctis
{
	class AstVisitor;
	
	struct AstContext
	{
		QualNameSPtr scope;
		IdenSPtr iden;
	};

	enum class AstNodeKind : u8
	{
		Unknown,
		
		ModuleDecl,
		UnittestDecl, // TODO
		BenchmarkDecl, // TODO

		Identifier,
		Param,
		Arg,

		DeclStartMarker,
		StructDecl = DeclStartMarker,
		UnionDecl,
		ValueEnumDecl,
		AdtEnumStructMember,
		AdtEnumDecl,
		MarkerInterfaceDecl,
		WeakInterfaceDecl,
		StrongInterfaceDecl,
		TypeAliasDecl,
		TypeDefDecl,
		VarDecl,
		FuncDecl,
		MethodDecl,
		EmptyMethodDecl,
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
		IsExpr,
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
		AttributeEndMarker = SimpleAttribute,

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
		MacroEndMarker = MacroInst,
		
	};

	bool IsNodeKindDeclaration(AstNodeKind kind);
	bool IsNodeKindStatement(AstNodeKind kind);
	bool IsNodeKindExpression(AstNodeKind kind);
	bool IsNodeKindType(AstNodeKind kind);
	bool IsNodeKindAttribute(AstNodeKind kind);
	bool IsNodeKindGeneric(AstNodeKind kind);
	bool IsNodeKindMacro(AstNodeKind kind);
	
	struct AstNode
	{
		AstNode(AstNodeKind nodeKind, u64 startTokIdx, u64 endTokIdx);
		virtual ~AstNode();
		
		virtual void Visit(AstVisitor& visitor);
		virtual void Log(u32 indent);
		
		AstNodeKind nodeKind;
		u64 startTokIdx;
		u64 endTokIdx;

		std::unique_ptr<AstContext> ctx;

	protected:
		void LogIndent(u32 indent);
	};
	using AstNodeSPtr = StdSharedPtr<AstNode>;

	struct AstTree
	{
		StdString filepath;
		StdVector<AstNodeSPtr> nodes;

		void Visit(AstVisitor& visitor);
		void Log();
	};

	struct AstModuleDecl : public AstNode
	{
		AstModuleDecl(u64 importTokIdx, StdVector<StdString>&& moduleIdens, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		StdVector<StdString> moduleIdens;
	};

	struct AstIdentifier : public AstNode
	{
		AstIdentifier(u64 idenIdx, StdString&& iden);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
	};

	struct AstParam : public AstNode
	{
		AstParam(u64 startTokIdx, StdVector<std::pair<AstNodeSPtr, StdString>>&& idens, AstNodeSPtr type, bool isVariadic, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<std::pair<AstNodeSPtr, StdString>> idens;
		AstNodeSPtr type;
		bool isVariadic;
	};

	struct AstArg : public AstNode
	{
		AstArg(u64 startTokIdx, StdString&& iden, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		StdString iden;
		AstNodeSPtr expr;
	};

	struct AstStructDecl : public AstNode
	{
		AstStructDecl(AstNodeSPtr attribs, u64 structTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> members;
	};

	struct AstUnionDecl : public AstNode
	{
		AstUnionDecl(AstNodeSPtr attribs, u64 unionTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> members;
	};

	struct AstValueEnumDecl : public AstNode
	{
		AstValueEnumDecl(AstNodeSPtr attribs, u64 enumTokIdx, StdString&& iden, StdVector<std::pair<StdString, AstNodeSPtr>>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr type;
		StdVector<std::pair<StdString, AstNodeSPtr>> members;
	};

	struct AstAdtEnumStructMember : public AstNode
	{
		AstAdtEnumStructMember(u64 lBraceTokIdx, StdVector<std::pair<StdVector<StdString>, AstNodeSPtr>>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		StdVector<std::pair<StdVector<StdString>, AstNodeSPtr>> members;
	};
	
	struct AstAdtEnumDecl : public AstNode
	{
		AstAdtEnumDecl(AstNodeSPtr attribs, u64 enumTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<std::pair<StdString, AstNodeSPtr>>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<std::pair<StdString, AstNodeSPtr>> members;
	};

	struct AstMarkerInterfaceDecl : public AstNode
	{
		AstMarkerInterfaceDecl(AstNodeSPtr attribs, u64 interfaceTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
	};

	struct AstWeakInterfaceDecl : public AstNode
	{
		AstWeakInterfaceDecl(AstNodeSPtr attribs, u64 weakTokIddx, StdString&& iden, StdVector<AstNodeSPtr>&& members, u64 rBraceToken);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		StdVector<AstNodeSPtr> members;
	};
	
	struct AstStrongInterfaceDecl : public AstNode
	{
		AstStrongInterfaceDecl(AstNodeSPtr attribs, u64 interfaceTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> members;
	};

	struct AstTypeAliasDecl : public AstNode
	{
		AstTypeAliasDecl(AstNodeSPtr attribs, u64 typealiasTokIdx, StdString&& iden, AstNodeSPtr generics, AstNodeSPtr type, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		AstNodeSPtr type;
	};

	struct AstTypeDefDecl : public AstNode
	{
		AstTypeDefDecl(AstNodeSPtr attribs, u64 typedefTokIdx, StdString&& iden, AstNodeSPtr generics, AstNodeSPtr type, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		AstNodeSPtr type;
	};

	struct AstVarDecl : public AstNode
	{
		AstVarDecl(AstNodeSPtr attribs, u64 startTokIdx, StdVector<StdString>&& idens, AstNodeSPtr type, AstNodeSPtr expr, u64 termTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdVector<StdString> idens;
		AstNodeSPtr type;
		AstNodeSPtr expr;
	};

	struct AstFuncDecl : public AstNode
	{
		AstFuncDecl(AstNodeSPtr attribs, u64 funcTokIdx, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> params;
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
		AstMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		AstMethodReceiverKind rec;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> params;
		AstNodeSPtr ret;
		AstNodeSPtr whereClause;
		StdVector<AstNodeSPtr> statements;
	};

	struct AstEmptyMethodDecl : public AstNode
	{
		AstEmptyMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec, StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		AstMethodReceiverKind rec;
		StdString iden;
		AstNodeSPtr generics;
		StdVector<AstNodeSPtr> params;
		AstNodeSPtr ret;
		AstNodeSPtr whereClause;
		StdVector<AstNodeSPtr> statements;
	};

	struct AstImplDecl : public AstNode
	{
		AstImplDecl(AstNodeSPtr attribs, u64 implTokIdx, AstNodeSPtr generics, AstNodeSPtr type, StdVector<AstNodeSPtr>&& interfaces, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		AstNodeSPtr generics;
		AstNodeSPtr type;
		StdVector<AstNodeSPtr> interfaces;
		StdVector<AstNodeSPtr> statements;
	};

	struct AstImportStmt : public AstNode
	{
		AstImportStmt(AstNodeSPtr attribs, u64 importTokIdx, StdVector<StdString>&& moduleIdens, StdVector<std::pair<StdVector<StdString>, StdString>>&& symbols, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr attribs;
		StdVector<StdString> moduleIdens;
		StdVector<std::pair<StdVector<StdString>, StdString>> symbols;
	};

	struct AstBlockStmt : public AstNode
	{
		AstBlockStmt(u64 lBraceTokIds, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> statements;
	};

	struct AstIfStmt : public AstNode
	{
		AstIfStmt(u64 ifTokIdx, AstNodeSPtr decl, AstNodeSPtr cond, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr decl;
		AstNodeSPtr cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstLoopStmt : public AstNode
	{
		AstLoopStmt(AstNodeSPtr label, u64 loopTokIdx, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr label;
		AstNodeSPtr body;
	};

	struct AstWhileStmt : public AstNode
	{
		AstWhileStmt(AstNodeSPtr label, u64 whileTokIdx, AstNodeSPtr cond, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr label;
		AstNodeSPtr cond;
		AstNodeSPtr body;
	};

	struct AstDoWhileStmt : public AstNode
	{
		AstDoWhileStmt(AstNodeSPtr label, u64 doTokIdx, AstNodeSPtr body, AstNodeSPtr cond, u64 endIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr label;
		AstNodeSPtr body;
		AstNodeSPtr cond;
	};

	struct AstForStmt : public AstNode
	{
		AstForStmt(AstNodeSPtr label, u64 forTokIdx, AstNodeSPtr init, AstNodeSPtr cond, AstNodeSPtr inc, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr label;
		AstNodeSPtr init;
		AstNodeSPtr cond;
		AstNodeSPtr inc;
		AstNodeSPtr body;
	};

	struct AstForRangeStmt : public AstNode
	{
		AstForRangeStmt(AstNodeSPtr label, u64 forTokIdx, StdVector<StdString>&& idens, AstNodeSPtr range, AstNodeSPtr body);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr label;
		StdVector<StdString> idens;
		AstNodeSPtr range;
		AstNodeSPtr body;
	};

	struct AstSwitchCase
	{
		AstNodeSPtr staticExpr;
		AstNodeSPtr dynamicExpr;
		AstNodeSPtr body;
	};

	struct AstSwitchStmt : public AstNode
	{
		AstSwitchStmt(AstNodeSPtr label, u64 switchTokIdx, StdVector<AstSwitchCase>&& cases, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr label;
		StdVector<AstSwitchCase> cases;
	};

	struct AstLabelStmt : public AstNode
	{
		AstLabelStmt(u64 startTokIdx, StdString&& iden, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
	};

	struct AstBreakStmt : public AstNode
	{
		AstBreakStmt(u64 breakTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
	};

	struct AstContinueStmt : public AstNode
	{
		AstContinueStmt(u64 continueTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
	};

	struct AstFallthroughStmt : public AstNode
	{
		AstFallthroughStmt(u64 fallthroughTokIdx, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
	};

	struct AstGotoStmt : public AstNode
	{
		AstGotoStmt(u64 gotoTokIdx, StdString&& iden, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
	};

	struct AstReturnStmt : public AstNode
	{
		AstReturnStmt(u64 returnTokIdx, AstNodeSPtr expr, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstExprStmt : public AstNode
	{
		AstExprStmt(AstNodeSPtr expr, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstDeferStmt : public AstNode
	{
		AstDeferStmt(u64 deferTokIdx, AstNodeSPtr stmt, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstStackDeferStmt : public AstNode
	{
		AstStackDeferStmt(u64 deferTokIdx, AstNodeSPtr stmt, u64 semicolonTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstUnsafeStmt : public AstNode
	{
		AstUnsafeStmt(u64 unsafeTokIdx, StdVector<AstNodeSPtr>&& stmts, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> stmts;
	};

	struct AstCompIfStmt : public AstNode
	{
		AstCompIfStmt(u64 hashTokIdx, AstNodeSPtr decl, AstNodeSPtr expr, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr decl;
		AstNodeSPtr cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstCompCondStmt : public AstNode
	{
		AstCompCondStmt(u64 hashTokIdx, Token cond, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		Token cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstCompDebugStmt : public AstNode
	{
		AstCompDebugStmt(u64 hashTokIdx, Token cond, AstNodeSPtr body, AstNodeSPtr elseBody);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		Token cond;
		AstNodeSPtr body;
		AstNodeSPtr elseBody;
	};

	struct AstAssignExpr : public AstNode
	{
		AstAssignExpr(AstNodeSPtr lExpr, TokenType op, AstNodeSPtr rExpr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr lExpr;
		TokenType op;
		AstNodeSPtr rExpr;
	};

	struct AstTernaryExpr : public AstNode
	{
		AstTernaryExpr(AstNodeSPtr cond, AstNodeSPtr trueExpr, AstNodeSPtr falseExpr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr cond;
		AstNodeSPtr trueExpr;
		AstNodeSPtr falseExpr;
	};

	struct AstBinaryExpr : public AstNode
	{
		AstBinaryExpr(AstNodeSPtr lExpr, TokenType op, AstNodeSPtr rExpr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr lExpr;
		TokenType op;
		AstNodeSPtr rExpr;
	};

	struct AstPostfixExpr : public AstNode
	{
		AstPostfixExpr(AstNodeSPtr expr, TokenType op, u64 opTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
		TokenType op;
	};

	struct AstPrefixExpr : public AstNode
	{
		AstPrefixExpr(TokenType op, u64 opTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		TokenType op;
		AstNodeSPtr expr;
	};

	struct AstQualNameExpr : public AstNode
	{
		AstQualNameExpr(u64 startTokIdx, StdVector<AstNodeSPtr>&& idens, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> idens;
	};

	struct AstIndexSliceExpr : public AstNode
	{
		AstIndexSliceExpr(AstNodeSPtr expr, bool nullCoalesce, AstNodeSPtr index, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		bool nullCoalesce;
		AstNodeSPtr expr;
		AstNodeSPtr index;
	};

	struct AstSliceExpr : public AstNode
	{
		AstSliceExpr(AstNodeSPtr expr, bool nullCoalesce, AstNodeSPtr begin, AstNodeSPtr end, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		bool nullCoalesce;
		AstNodeSPtr expr;
		AstNodeSPtr begin;
		AstNodeSPtr end;
	};

	// can be func, ADT enum member
	struct AstFuncCallExpr : public AstNode
	{
		AstFuncCallExpr(AstNodeSPtr func, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr func;
		StdVector<AstNodeSPtr> args;
	};

	struct AstMemberAccessExpr : public AstNode
	{
		AstMemberAccessExpr(AstNodeSPtr caller, bool nullCoalesce, StdString&& iden, u64 idenTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr caller;
		bool nullCoalesce;
		StdString iden;
	};

	struct AstMethodCallExpr : public AstNode
	{
		AstMethodCallExpr(AstNodeSPtr caller, bool nullCoalesce, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr caller;
		bool nullCoalesce;
		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	struct AstTupleAccessExpr : public AstNode
	{
		AstTupleAccessExpr(AstNodeSPtr expr, bool nullCoalesce, u16 index, u64 indexTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
		bool nullCoalesce;
		u16 index;
	};

	struct AstLiteralExpr : public AstNode
	{
		AstLiteralExpr(Token literal);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		Token literal;
	};

	// Can be: struct, union, ADT enum member
	struct AstAggrInitExpr : public AstNode
	{
		AstAggrInitExpr(u64 startTokIdx, AstNodeSPtr type, StdVector<AstNodeSPtr>&& args, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		AstNodeSPtr type;
		StdVector<AstNodeSPtr> args;
	};

	struct AstTupleInitExpr : public AstNode
	{
		AstTupleInitExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> exprs;
	};

	struct AstArrayInitExpr : public AstNode
	{
		AstArrayInitExpr(u64 lBracketTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> exprs;
	};

	struct AstCastExpr : public AstNode
	{
		AstCastExpr(u64 castTokIdx, AstNodeSPtr type, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr type;
		AstNodeSPtr expr;
	};

	struct AstTransmuteExpr : public AstNode
	{
		AstTransmuteExpr(u64 transmuteTokIdx, AstNodeSPtr type, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr type;
		AstNodeSPtr expr;
	};

	struct AstMoveExpr : public AstNode
	{
		AstMoveExpr(u64 moveTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstBracketExpr : public AstNode
	{
		AstBracketExpr(u64 lBracketTokIdx, AstNodeSPtr expr, u64 rBracketTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstBlockExpr : public AstNode
	{
		AstBlockExpr(u64 lBraceTokIdx, StdVector<AstNodeSPtr>&& stmts, u64 rBraceExpr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> stmts;
	};

	struct AstUnsafeExpr : public AstNode
	{
		AstUnsafeExpr(u64 unsafeTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstCommaExpr : public AstNode
	{
		AstCommaExpr(StdVector<AstNodeSPtr>&& exprs);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> exprs;
	};
	struct AstClosureExpr : public AstNode
	{
		AstClosureExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, StdVector<AstNodeSPtr> stmts, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> params;
		AstNodeSPtr ret;
		StdVector<AstNodeSPtr> stmts;
	};

	struct AstIsExpr : public AstNode
	{
		AstIsExpr(AstNodeSPtr expr, u64 isTokIdx, AstNodeSPtr type);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
		AstNodeSPtr type;
	};

	struct AstCompRunExpr : public AstNode
	{
		AstCompRunExpr(u64 hashTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstBuiltinType : public AstNode
	{
		AstBuiltinType(const Token& tok);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		TokenType type;
	};

	struct AstIdentifierType : public AstNode
	{
		AstIdentifierType(u64 startTokIdx, StdVector<AstNodeSPtr>&& idens, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> idens;
	};

	struct AstPointerType : public AstNode
	{
		AstPointerType(u64 asteriskTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr subType;
	};

	struct AstReferenceType : public AstNode
	{
		AstReferenceType(u64 andTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr subType;
	};

	struct AstArrayType : public AstNode
	{
		AstArrayType(u64 lBracketTokIdx, AstNodeSPtr arraySize, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr arraySize;
		AstNodeSPtr subType;
	};

	struct AstSliceType : public AstNode
	{
		AstSliceType(u64 lBraceTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr subType;
	};

	struct AstTupleType : public AstNode
	{
		AstTupleType(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& subTypes, u64 rParenTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> subTypes;
	};

	struct AstOptionalType : public AstNode
	{
		AstOptionalType(u64 lBraceTokIdx, AstNodeSPtr subType);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr subType;
	};

	struct AstAttributes : public AstNode
	{
		AstAttributes(u64 startTokIdx, StdVector<AstNodeSPtr>&& compAttribs, StdVector<AstNodeSPtr>&& userAttribs, AstNodeSPtr visibility, StdVector<AstNodeSPtr>&& simpleAttribs, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> compAttribs;
		StdVector<AstNodeSPtr> userAttribs;
		AstNodeSPtr visibility;
		StdVector<AstNodeSPtr> simpleAttribs;
	};

	struct AstCompAttribute : public AstNode
	{
		AstCompAttribute(u64 atColonTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 endTokIdx);
		
		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	struct AstUserAttribute : public AstNode
	{
		AstUserAttribute(u64 atColonTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	struct AstVisibilityAttribute : public AstNode
	{
		AstVisibilityAttribute(u64 publicTokIdx, StdString&& kind, u64 endTokId);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString kind;
	};

	struct AstSimpleAttribute : public AstNode
	{
		AstSimpleAttribute(u64 tokIdx, TokenType attrib);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		TokenType attrib;
	};

	struct AstGenericDecl : public AstNode
	{
		AstGenericDecl(u64 startTokIdx, StdVector<AstNodeSPtr>&& params, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<AstNodeSPtr> params;
	};

	struct AstGenericTypeParam : public AstNode
	{
		AstGenericTypeParam(u64 idenTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& implTypes, AstNodeSPtr defType);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdVector<AstNodeSPtr> implTypes;
		AstNodeSPtr defType;
	};

	struct AstGenericValueParam : public AstNode
	{
		AstGenericValueParam(u64 idenTokIdx, StdString&& iden, AstNodeSPtr type, AstNodeSPtr defExpr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		AstNodeSPtr type;
		AstNodeSPtr defExpr;
	};
	
	struct AstGenericWhereClause : public AstNode
	{
		AstGenericWhereClause(u64 whereTokIdx, AstNodeSPtr expr);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr expr;
	};

	struct AstGenericInst : public AstNode
	{
		AstGenericInst(u64 startTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdVector<AstNodeSPtr> args;
	};

	enum class AstMacroVarKind : u8
	{
		Unknown,
		Stmt,
		Expr,
		Type,
		Qual,
		Iden,
		Attr,
		Toks
	};
	
	struct AstMacroVar : public AstNode
	{
		AstMacroVar(u64 dollarTokIdx, StdString&& iden, AstMacroVarKind kind, u64 kindTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		StdString iden;
		AstMacroVarKind kind;
	};

	struct AstMacroSeparator : public AstNode
	{
		AstMacroSeparator(StdVector<Token>&& toks);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<Token> toks;
	};

	struct AstMacroFragment : public AstNode
	{
		AstMacroFragment(u64 startIdx, AstNodeSPtr subPattern, TokenType repType, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr subPattern;
		TokenType repType;
	};
	
	struct AstMacroPattern : public AstNode
	{
		AstMacroPattern(u64 startTokIdx, StdVector<AstNodeSPtr>&& elems, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;
		
		StdVector<AstNodeSPtr> elems;
	};

	struct AstMacroRule : public AstNode
	{
		AstMacroRule(u64 startTokIdx, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		AstNodeSPtr pattern;
		StdVector<AstNodeSPtr> body;
	};
	
	struct AstDeclMacro : public AstNode
	{
		AstDeclMacro(u64 macroTokIdx, StdString&& iden, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		AstNodeSPtr pattern;
		StdVector<AstNodeSPtr> body;
	};

	struct AstRulesDeclMacro : public AstNode
	{
		AstRulesDeclMacro(u64 macroTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& rules, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdVector<AstNodeSPtr> rules;
	};

	struct AstProcMacro : public AstNode
	{
		AstProcMacro(u64 macroTokIdx, StdString&& iden, StdString&& tokStreamIden, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdString tokStreamIden;
		AstNodeSPtr pattern;
		StdVector<AstNodeSPtr> body;
	};

	struct AstRulesProcMacro : public AstNode
	{
		AstRulesProcMacro(u64 macroTokIdx, StdString&& iden, StdString&& tokStreamIden, StdVector<AstNodeSPtr>&& rules, u64 rBraceTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdString iden;
		StdString tokStreamIden;
		StdVector<AstNodeSPtr> rules;
	};

	struct AstMacroInst : public AstNode
	{
		AstMacroInst(u64 startTokIdx, StdVector<StdString>&& idens, StdVector<Token>& toks, u64 endTokIdx);

		void Visit(AstVisitor& visitor) override;
		void Log(u32 indent) override;

		StdVector<StdString> idens;
		StdVector<Token> toks;
	};

	
}
