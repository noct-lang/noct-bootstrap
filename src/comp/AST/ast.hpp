#pragma once
#include "common/defs.hpp"
#include "comp/token.hpp"
#include "comp/qualname.hpp"
#include "ast.hpp"

namespace Noctis
{
#define  FWD_DECL_SPTR(type) \
	struct type;\
	using type##SPtr = StdSharedPtr<type>
	
	struct AstContext
	{
		u64 startIdx, endIdx;
		QualNameSPtr scope;
		IdenSPtr iden;
	};
	using AstContextPtr = StdUniquePtr<AstContext>;

	enum class AstStmtKind : u8
	{
		Decl,
		Import,
		Block,
		If,
		Loop,
		While,
		DoWhile,
		For,
		Switch,
		Label,
		Break,
		Continue,
		Fallthrough,
		Goto,
		Return,
		Expr,
		Defer,
		StackDefer,
		Unsafe,
		ErrorHandler,
		CompIf,
		CompCond,
		CompDebug,
		MacroLoop,
	};

	enum class AstDeclKind : u8
	{
		Module,
		UnitTest,
		Benchmark,
		Struct,
		Union,
		ValueEnum,
		AdtEnum,
		MarkerInterface,
		WeakInterface,
		StrongInterface,
		Typealias,
		Typedef,
		Var,
		Func,
		Method,
		EmptyMethod,
		Impl,
		DeclMacro,
		RulesDeclMacro,
		ProcMacro,
		RulesProcMacro
	};

	enum class AstExprKind : u8
	{
		Assign,
		Ternary,
		Binary,
		Postfix,
		Prefix,
		QualName,
		IndexSlice,
		Slice,
		FuncCall,
		MemberAccess,
		MethodCall,
		TupleAccess,
		Literal,
		AggrInit,
		TupleInit,
		ArrayInit,
		Cast,
		Transmute,
		Move,
		Bracket,
		Block,
		Unsafe,
		Comma,
		Closure,
		Is,
		Try,
		Throw,
		SpecKw,
		CompRun,
		MacroVar,
		MacroInst,
	};

	enum class AstTypeKind : u8
	{
		Builtin,
		Iden,
		Ptr,
		Ref,
		Arr,
		Slice,
		Tuple,
		Optional,
		InlineStruct,
		InlineEnum,
		CompoundInterface,
	};

	enum class GenericArgKind : u8
	{
		Type,
		Expr,
	};

	enum class AstQualIdenKind : u8
	{
		Identifier,
		TypeDisambiguation,
	};

	enum class AstMacroPatternElemKind : u8
	{
		Variable,
		Separator,
		Fragment
	};
	
	struct AstStmt
	{
		AstStmt(AstStmtKind kind, u64 startIdx, u64 endIdx);
		~AstStmt();
		AstStmtKind stmtKind;
		AstContextPtr ctx;
	};
	using AstStmtSPtr = StdSharedPtr<AstStmt>;

	struct AstDecl : public AstStmt
	{
		AstDecl(AstDeclKind kind, u64 startIdx, u64 endIdx);

		AstDeclKind declKind;
	};
	using AstDeclSPtr = StdSharedPtr<AstDecl>;

	struct AstExpr
	{
		AstExpr(AstExprKind kind, u64 startIdx, u64 endIdx);

		AstExprKind exprKind;
		AstContextPtr ctx;
	};
	using AstExprSPtr = StdSharedPtr<AstExpr>;

	struct AstType
	{
		AstType(AstTypeKind nodeKkindind, u64 startIdx, u64 endIdx);

		AstTypeKind typeKind;
		AstContextPtr ctx;
	};
	using AstTypeSPtr = StdSharedPtr<AstType>;

	FWD_DECL_SPTR(AstLabelStmt);
	FWD_DECL_SPTR(AstIdentifierType);
	FWD_DECL_SPTR(AstGenericDecl);
	FWD_DECL_SPTR(AstGenericWhereClause);
	FWD_DECL_SPTR(AstAttribs);
	FWD_DECL_SPTR(AstCompAttrib);
	FWD_DECL_SPTR(AstUserAttrib);
	FWD_DECL_SPTR(AstVisibilityAttrib);
	FWD_DECL_SPTR(AstSimpleAttrib);
	FWD_DECL_SPTR(AstGenericTypeParam);
	FWD_DECL_SPTR(AstGenericValueParam);
	FWD_DECL_SPTR(AstMacroPattern);

	struct AstTree
	{
		StdString filepath;
		StdVector<AstStmtSPtr> nodes;
	};

	struct AstParam
	{
		AstParam(u64 startIdx, StdPairVector<AstAttribsSPtr, StdString>&& idens,
			AstTypeSPtr type, bool isVariadic, u64 endIdx);

		StdPairVector<AstAttribsSPtr, StdString> idens;
		AstTypeSPtr type;
		bool isVariadic;
		AstContextPtr ctx;
	};
	using AstParamSPtr = StdSharedPtr<AstParam>;

	struct AstArg
	{
		AstArg(u64 startTokIdx, StdString&& iden, AstExprSPtr expr);

		StdString iden;
		AstExprSPtr expr;
		AstContextPtr ctx;
	};
	using AstArgSPtr = StdSharedPtr<AstArg>;

	struct AstGenericArg
	{
		AstGenericArg(AstTypeSPtr type);
		AstGenericArg(AstExprSPtr expr);
		AstGenericArg(AstGenericArg&& arg) noexcept;
		~AstGenericArg();

		GenericArgKind kind;
		union
		{
			AstTypeSPtr type;
			AstExprSPtr expr;
		};
	};

	struct AstQualIden
	{
		AstQualIden(AstQualIdenKind kind, u64 startIdx, u64 endIdx);
		
		AstQualIdenKind qualIdenKind;
		AstContextPtr ctx;
	};
	using AstQualIdenSPtr = StdSharedPtr<AstQualIden>;
	
	struct AstIden : public AstQualIden
	{
		AstIden(u64 startIdx, StdString&& iden, StdVector<AstGenericArg>&& args, u64 endIdx);

		StdString iden;
		StdVector<AstGenericArg> args;
	};

	struct AstTypeDisambiguation : public AstQualIden
	{
		AstTypeDisambiguation(u64 startIdx, AstTypeSPtr type, AstIdentifierTypeSPtr interface, u64 endIdx);

		AstTypeSPtr type;
		AstIdentifierTypeSPtr interface;
	};

	struct AstQualName
	{
		AstQualName(u64 startIdx, bool global, StdVector<AstQualIdenSPtr>&& idens);

		bool global;
		StdVector<AstQualIdenSPtr> idens;
		AstContextPtr ctx;
	};
	using AstQualNameSPtr = StdSharedPtr<AstQualName>;
	
	struct AstModuleDecl : public AstDecl
	{
		AstModuleDecl(u64 startIdx, StdVector<StdString>&& moduleIdens, u64 endIdx);
		
		StdVector<StdString> moduleIdens;
	};

	struct AstUnittestDecl : public AstDecl
	{
		AstUnittestDecl(u64 startIdx, StdString&& name, StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		StdString name;
		StdVector<AstStmtSPtr> stmts;
	};

	struct AstBenchmarkDecl : public AstDecl
	{
		AstBenchmarkDecl(u64 hashTokIdx, StdString&& name, StdString&& stateIden,
			StdVector<AstStmtSPtr>&& stmts, u64 rBraceTokIdx);

		StdString name;
		StdString stateIden;
		StdVector<AstStmtSPtr> stmts;
	};

	struct AstStructDecl : public AstDecl
	{
		AstStructDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, StdVector<AstStmtSPtr>&& members, u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdVector<AstStmtSPtr> members;
	};

	struct AstUnionDecl : public AstDecl
	{
		AstUnionDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, StdVector<AstStmtSPtr>&& members, u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdVector<AstStmtSPtr> members;
	};

	struct AstValueEnumDecl : public AstDecl
	{
		AstValueEnumDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstTypeSPtr baseType, StdPairVector<StdString, AstExprSPtr>&& members,
			u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstTypeSPtr baseType;
		StdPairVector<StdString, AstExprSPtr> members;
	};

	
	struct AstAdtEnumDecl : public AstDecl
	{
		AstAdtEnumDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, StdPairVector<StdString, AstTypeSPtr>&& members,
			u64 rBraceTokIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdPairVector<StdString, AstTypeSPtr> members;
	};

	struct AstMarkerInterfaceDecl : public AstDecl
	{
		AstMarkerInterfaceDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
	};

	struct AstWeakInterfaceDecl : public AstDecl
	{
		AstWeakInterfaceDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			StdVector<AstStmtSPtr>&& members, u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		StdVector<AstStmtSPtr> members;
	};
	
	struct AstStrongInterfaceDecl : public AstDecl
	{
		AstStrongInterfaceDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, StdVector<AstStmtSPtr>&& members, u64 rBraceTokIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdVector<AstStmtSPtr> members;
	};

	struct AstTypeAliasDecl : public AstDecl
	{
		AstTypeAliasDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, AstTypeSPtr type, u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		AstTypeSPtr type;
	};

	struct AstTypeDefDecl : public AstDecl
	{
		AstTypeDefDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, AstTypeSPtr type, u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		AstTypeSPtr type;
	};

	struct AstVarDecl : public AstDecl
	{
		AstVarDecl(AstAttribsSPtr attribs, u64 startIdx, StdVector<StdString>&& idens,
			AstTypeSPtr type, AstExprSPtr expr, u64 endIdx);

		AstAttribsSPtr attribs;
		StdVector<StdString> idens;
		AstTypeSPtr type;
		AstExprSPtr expr;
	};
	using AstVarDeclSPtr = StdSharedPtr<AstVarDecl>;

	struct AstFuncDecl : public AstDecl
	{
		AstFuncDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
			AstGenericDeclSPtr generics, StdVector<AstParamSPtr>&& params, bool thorws,
			AstTypeSPtr errorType, AstTypeSPtr retType,
			StdPairVector<StdString, AstTypeSPtr>&& namedRet, AstGenericWhereClauseSPtr whereClause,
			StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		AstAttribsSPtr attribs;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdVector<AstParamSPtr> params;
		bool throws;
		AstTypeSPtr errorType;
		AstTypeSPtr retType;
		StdPairVector<StdString, AstTypeSPtr> namedRet;
		AstGenericWhereClauseSPtr whereClause;
		StdVector<AstStmtSPtr> stmts;
	};

	enum class AstMethodReceiverKind
	{
		None,
		Value,
		Ref,
		ConstRef
	};
	
	struct AstMethodDecl : public AstDecl
	{
		AstMethodDecl(AstAttribsSPtr attribs, u64 startIdx, AstMethodReceiverKind rec,
			StdString&& iden, AstGenericDeclSPtr generics, StdVector<AstParamSPtr>&& params, 
			bool throws, AstTypeSPtr errorType, AstTypeSPtr retType, 
			StdPairVector<StdString, AstTypeSPtr>&& namedRet, AstGenericWhereClauseSPtr whereClause,
			StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		AstAttribsSPtr attribs;
		AstMethodReceiverKind rec;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdVector<AstParamSPtr> params;
		bool throws;
		AstTypeSPtr errorType;
		AstTypeSPtr retType;
		StdPairVector<StdString, AstTypeSPtr> namedRet;
		AstGenericWhereClauseSPtr whereClause;
		StdVector<AstStmtSPtr> stmts;
	};

	struct AstEmptyMethodDecl : public AstDecl
	{
		AstEmptyMethodDecl(AstAttribsSPtr attribs, u64 startIdx, AstMethodReceiverKind rec,
			StdString&& iden, AstGenericDeclSPtr generics, StdVector<AstParamSPtr>&& params,
			AstTypeSPtr retType, u64 endIdx);

		AstAttribsSPtr attribs;
		AstMethodReceiverKind rec;
		StdString iden;
		AstGenericDeclSPtr generics;
		StdVector<AstParamSPtr> params;
		AstTypeSPtr retType;
	};

	struct AstImplDecl : public AstDecl
	{
		AstImplDecl(AstAttribsSPtr attribs, u64 startIdx, AstGenericDeclSPtr generics,
			AstTypeSPtr type, StdVector<AstIdentifierTypeSPtr>&& interfaces, 
			StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		AstAttribsSPtr attribs;
		AstGenericDeclSPtr generics;
		AstTypeSPtr type;
		StdVector<AstIdentifierTypeSPtr> interfaces;
		StdVector<AstStmtSPtr> stmts;
	};

	struct AstImportStmt : public AstStmt
	{
		AstImportStmt(AstAttribsSPtr attribs, u64 startIdx, StdVector<StdString>&& moduleIdens, StdPairVector<StdVector<StdString>, StdString>&& symbols, u64 endIdx);

		AstAttribsSPtr attribs;
		StdVector<StdString> moduleIdens;
		StdPairVector<StdVector<StdString>, StdString> symbols;
	};

	struct AstBlockStmt : public AstStmt
	{
		AstBlockStmt(u64 startIdx, StdVector<AstStmtSPtr>&& statements, u64 endIdx);

		StdVector<AstStmtSPtr> stmts;
	};

	struct AstIfStmt : public AstStmt
	{
		AstIfStmt(u64 startIdx, AstVarDeclSPtr decl, AstExprSPtr cond, AstStmtSPtr body, AstStmtSPtr elseBody);

		AstVarDeclSPtr decl;
		AstExprSPtr cond;
		AstStmtSPtr body;
		AstStmtSPtr elseBody;
	};

	struct AstLoopStmt : public AstStmt
	{
		AstLoopStmt(AstLabelStmtSPtr label, u64 startIdx, AstStmtSPtr body);

		AstLabelStmtSPtr label;
		AstStmtSPtr body;
	};

	struct AstWhileStmt : public AstStmt
	{
		AstWhileStmt(AstLabelStmtSPtr label, u64 startIdx, AstExprSPtr cond, AstStmtSPtr body);

		AstLabelStmtSPtr label;
		AstExprSPtr cond;
		AstStmtSPtr body;
	};

	struct AstDoWhileStmt : public AstStmt
	{
		AstDoWhileStmt(AstLabelStmtSPtr label, u64 startIdx, AstStmtSPtr body, AstExprSPtr cond, u64 endIdx);

		AstLabelStmtSPtr label;
		AstStmtSPtr body;
		AstExprSPtr cond;
	};

	struct AstForStmt : public AstStmt
	{
		AstForStmt(AstLabelStmtSPtr label, u64 startIdx, StdVector<StdString>&& idens, 
			AstExprSPtr range, AstStmtSPtr body);

		AstLabelStmtSPtr label;
		StdVector<StdString> idens;
		AstExprSPtr range;
		AstStmtSPtr body;
	};

	struct AstSwitchCase
	{
		AstExprSPtr staticExpr;
		AstExprSPtr dynamicExpr;
		AstStmtSPtr body;
	};

	struct AstSwitchStmt : public AstStmt
	{
		AstSwitchStmt(AstLabelStmtSPtr label, u64 startIdx, AstExprSPtr cond, StdVector<AstSwitchCase>&& cases, u64 endIdx);

		AstLabelStmtSPtr label;
		AstExprSPtr cond;
		StdVector<AstSwitchCase> cases;
	};

	struct AstLabelStmt : public AstStmt
	{
		AstLabelStmt(u64 startIdx, StdString&& iden, u64 endIdx);

		StdString iden;
	};

	struct AstBreakStmt : public AstStmt
	{
		AstBreakStmt(u64 startIdx, StdString&& iden, u64 endIdx);

		StdString iden;
	};

	struct AstContinueStmt : public AstStmt
	{
		AstContinueStmt(u64 startIdx, StdString&& iden, u64 endIdx);

		StdString iden;
	};

	struct AstFallthroughStmt : public AstStmt
	{
		AstFallthroughStmt(u64 startIdx, u64 endIdx);
	};

	struct AstGotoStmt : public AstStmt
	{
		AstGotoStmt(u64 startIdx, StdString&& iden, u64 endIdx);

		StdString iden;
	};

	struct AstReturnStmt : public AstStmt
	{
		AstReturnStmt(u64 startIdx, AstExprSPtr expr, u64 endIdx);

		AstExprSPtr expr;
	};

	struct AstExprStmt : public AstStmt
	{
		AstExprStmt(AstExprSPtr expr, u64 endIdx);

		AstExprSPtr expr;
	};

	struct AstDeferStmt : public AstStmt
	{
		AstDeferStmt(u64 startIdx, AstExprSPtr stmt, u64 endIdx);

		AstExprSPtr expr;
	};

	struct AstStackDeferStmt : public AstStmt
	{
		AstStackDeferStmt(u64 startIdx, AstExprSPtr stmt, u64 endIdx);

		AstExprSPtr expr;
	};

	struct AstUnsafeStmt : public AstStmt
	{
		AstUnsafeStmt(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		StdVector<AstStmtSPtr> stmts;
	};

	struct AstErrorHandlerStmt : public AstStmt
	{
		AstErrorHandlerStmt(u64 startIdx, StdString&& errIden, AstTypeSPtr errType, StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		StdString errIden;
		AstTypeSPtr errType;
		StdVector<AstStmtSPtr> stmts;
	};

	struct AstCompIfStmt : public AstStmt
	{
		AstCompIfStmt(u64 startIdx, AstVarDeclSPtr decl, AstExprSPtr expr, AstStmtSPtr body,
			AstStmtSPtr elseBody);

		AstVarDeclSPtr decl;
		AstExprSPtr cond;
		AstStmtSPtr body;
		AstStmtSPtr elseBody;
	};

	struct AstCompCondStmt : public AstStmt
	{
		AstCompCondStmt(u64 startIdx, Token cond, AstStmtSPtr body, AstStmtSPtr elseBody);

		Token cond;
		AstStmtSPtr body;
		AstStmtSPtr elseBody;
	};

	struct AstCompDebugStmt : public AstStmt
	{
		AstCompDebugStmt(u64 startIdx, Token cond, AstStmtSPtr body, AstStmtSPtr elseBody);

		Token cond;
		AstStmtSPtr body;
		AstStmtSPtr elseBody;
	};

	struct AstMacroLoopStmt : public AstStmt
	{
		AstMacroLoopStmt(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		StdVector<AstStmtSPtr> stmts;
	};

	struct AstAssignExpr : public AstExpr
	{
		AstAssignExpr(AstExprSPtr lExpr, TokenType op, AstExprSPtr rExpr);

		AstExprSPtr lExpr;
		TokenType op;
		AstExprSPtr rExpr;
	};

	struct AstTernaryExpr : public AstExpr
	{
		AstTernaryExpr(AstExprSPtr cond, AstExprSPtr trueExpr, AstExprSPtr falseExpr);

		AstExprSPtr cond;
		AstExprSPtr trueExpr;
		AstExprSPtr falseExpr;
	};

	struct AstBinaryExpr : public AstExpr
	{
		AstBinaryExpr(AstExprSPtr lExpr, TokenType op, AstExprSPtr rExpr);

		AstExprSPtr lExpr;
		TokenType op;
		AstExprSPtr rExpr;
	};

	struct AstPostfixExpr : public AstExpr
	{
		AstPostfixExpr(AstExprSPtr expr, Token op);

		AstExprSPtr expr;
		TokenType op;
	};

	struct AstPrefixExpr : public AstExpr
	{
		AstPrefixExpr(Token op, AstExprSPtr expr);

		TokenType op;
		AstExprSPtr expr;
	};

	struct AstQualNameExpr : public AstExpr
	{
		AstQualNameExpr(AstQualNameSPtr qualName);

		AstQualNameSPtr qualName;
	};

	struct AstIndexSliceExpr : public AstExpr
	{
		AstIndexSliceExpr(AstExprSPtr expr, bool nullCoalesce, AstExprSPtr index, u64 endIdx);

		bool nullCoalesce;
		AstExprSPtr expr;
		AstExprSPtr index;
	};

	struct AstSliceExpr : public AstExpr
	{
		AstSliceExpr(AstExprSPtr expr, bool nullCoalesce, AstExprSPtr begin, AstExprSPtr end, 
			u64 endIdx);
		bool nullCoalesce;
		AstExprSPtr expr;
		AstExprSPtr begin;
		AstExprSPtr end;
	};

	// can be func, ADT enum member
	struct AstFuncCallExpr : public AstExpr
	{
		AstFuncCallExpr(AstExprSPtr func, StdVector<AstArgSPtr>&& args, u64 endIdx);

		AstExprSPtr func;
		StdVector<AstArgSPtr> args;
	};

	struct AstMemberAccessExpr : public AstExpr
	{
		AstMemberAccessExpr(AstExprSPtr caller, bool nullCoalesce, StdString&& iden, u64 endIdx);

		AstExprSPtr caller;
		bool nullCoalesce;
		StdString iden;
	};

	struct AstMethodCallExpr : public AstExpr
	{
		AstMethodCallExpr(AstExprSPtr caller, bool nullCoalesce, StdString&& iden,
			StdVector<AstArgSPtr>&& args, u64 endIdx);

		AstExprSPtr caller;
		bool nullCoalesce;
		StdString iden;
		StdVector<AstArgSPtr> args;
	};

	struct AstTupleAccessExpr : public AstExpr
	{
		AstTupleAccessExpr(AstExprSPtr expr, bool nullCoalesce, u16 index, u64 endIdx);

		AstExprSPtr expr;
		bool nullCoalesce;
		u16 index;
	};

	struct AstLiteralExpr : public AstExpr
	{
		AstLiteralExpr(Token literal);

		Token literal;
	};

	// Can be: struct, union, ADT enum member
	struct AstAggrInitExpr : public AstExpr
	{
		AstAggrInitExpr(u64 startIdx, AstTypeSPtr type, StdVector<AstArgSPtr>&& args, u64 endIdx);

		AstTypeSPtr type;
		StdVector<AstArgSPtr> args;
	};

	struct AstTupleInitExpr : public AstExpr
	{
		AstTupleInitExpr(u64 startIdx, StdVector<AstExprSPtr>&& exprs, u64 endIdx);

		StdVector<AstExprSPtr> exprs;
	};

	struct AstArrayInitExpr : public AstExpr
	{
		AstArrayInitExpr(u64 startIdx, StdVector<AstExprSPtr>&& exprs, u64 endIdx);

		StdVector<AstExprSPtr> exprs;
	};

	struct AstCastExpr : public AstExpr
	{
		AstCastExpr(u64 startIdx, AstTypeSPtr type, AstExprSPtr expr);

		AstTypeSPtr type;
		AstExprSPtr expr;
	};

	struct AstTransmuteExpr : public AstExpr
	{
		AstTransmuteExpr(u64 startIdx, AstTypeSPtr type, AstExprSPtr expr);

		AstTypeSPtr type;
		AstExprSPtr expr;
	};

	struct AstMoveExpr : public AstExpr
	{
		AstMoveExpr(u64 startIdx, AstExprSPtr expr);

		AstExprSPtr expr;
	};

	struct AstBracketExpr : public AstExpr
	{
		AstBracketExpr(u64 startIdx, AstExprSPtr expr, u64 endIdx);

		AstExprSPtr expr;
	};

	struct AstBlockExpr : public AstExpr
	{
		AstBlockExpr(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx);

		StdVector<AstStmtSPtr> stmts;
	};

	struct AstUnsafeExpr : public AstExpr
	{
		AstUnsafeExpr(u64 startIdx, AstExprSPtr expr);

		AstExprSPtr expr;
	};

	struct AstCommaExpr : public AstExpr
	{
		AstCommaExpr(StdVector<AstExprSPtr>&& exprs);

		StdVector<AstExprSPtr> exprs;
	};
	
	struct AstClosureExpr : public AstExpr
	{
		AstClosureExpr(u64 startIdx, StdVector<AstParamSPtr>&& params, AstTypeSPtr ret, AstExprSPtr expr);

		StdVector<AstParamSPtr> params;
		AstTypeSPtr ret;
		AstExprSPtr expr;
	};

	struct AstIsExpr : public AstExpr
	{
		AstIsExpr(AstExprSPtr expr, u64 isIdx, AstTypeSPtr type);

		AstExprSPtr expr;
		AstTypeSPtr type;
	};

	struct AstTryExpr : public AstExpr
	{
		AstTryExpr(Token& tryTok, AstExprSPtr call);

		TokenType tryType;
		AstExprSPtr call;
	};

	struct AstThrowExpr : public AstExpr
	{
		AstThrowExpr(u64 startIdx, AstExprSPtr expr);

		AstExprSPtr expr;
	};

	struct AstSpecKwExpr : public AstExpr
	{
		AstSpecKwExpr(Token& tok);

		TokenType specKw;
	};

	struct AstCompRunExpr : public AstExpr
	{
		AstCompRunExpr(u64 startIdx, AstExprSPtr expr);

		AstExprSPtr expr;
	};

	struct AstMacroVarExpr : public AstExpr
	{
		AstMacroVarExpr(u64 startIdx, StdString&& iden, u64 endIdx);

		StdString iden;
	};

	struct AstBuiltinType : public AstType
	{
		AstBuiltinType(const Token& tok);

		TokenType type;
	};

	struct AstIdentifierType : public AstType
	{
		AstIdentifierType(AstQualNameSPtr qualName);

		AstQualNameSPtr qualName;
	};

	struct AstPointerType : public AstType
	{
		AstPointerType(u64 asteriskTokIdx, AstTypeSPtr subType);

		AstTypeSPtr subType;
	};

	struct AstReferenceType : public AstType
	{
		AstReferenceType(u64 andTokIdx, AstTypeSPtr subType);

		AstTypeSPtr subType;
	};

	struct AstArrayType : public AstType
	{
		AstArrayType(u64 lBracketTokIdx, AstExprSPtr arraySize, AstTypeSPtr subType);

		AstExprSPtr arraySize;
		AstTypeSPtr subType;
	};

	struct AstSliceType : public AstType
	{
		AstSliceType(u64 lBraceTokIdx, AstTypeSPtr subType);

		AstTypeSPtr subType;
	};

	struct AstTupleType : public AstType
	{
		AstTupleType(u64 lParenTokIdx, StdVector<AstTypeSPtr>&& subTypes, u64 rParenTokIdx);

		StdVector<AstTypeSPtr> subTypes;
	};

	struct AstOptionalType : public AstType
	{
		AstOptionalType(u64 lBraceTokIdx, AstTypeSPtr subType);

		AstTypeSPtr subType;
	};

	struct AstInlineStructType : public AstType
	{
		AstInlineStructType(u64 startIdx, StdPairVector<StdVector<StdString>, AstTypeSPtr>&& members,
			u64 endIdx);

		StdPairVector<StdVector<StdString>, AstTypeSPtr> members;
	};

	struct AstInlineEnumType : public AstType
	{
		AstInlineEnumType(u64 startIdx, StdVector<StdString>&& members,
			u64 endIdx);

		StdVector<StdString> members;
	};

	struct AstCompoundInterfaceType : public AstType
	{
		AstCompoundInterfaceType(StdVector<AstIdentifierTypeSPtr>&& interfaces);

		StdVector<AstIdentifierTypeSPtr> interfaces;
	};

	struct AstAttribs
	{
		AstAttribs(u64 startIdx, StdVector<AstCompAttribSPtr>&& compAttribs,
			StdVector<AstUserAttribSPtr>&& userAttribs, AstVisibilityAttribSPtr visibility,
			StdVector<AstSimpleAttribSPtr>&& simpleAttribs, u64 endIdx);

		StdVector<AstCompAttribSPtr> compAttribs;
		StdVector<AstUserAttribSPtr> userAttribs;
		AstVisibilityAttribSPtr visibility;
		StdVector<AstSimpleAttribSPtr> simpleAttribs;
		AstContextPtr ctx;
	};

	struct AstCompAttrib
	{
		AstCompAttrib(u64 startIdx, StdString&& iden, StdVector<AstArgSPtr>&& args, u64 endTokIdx);
		
		StdString iden;
		StdVector<AstArgSPtr> args;
		AstContextPtr ctx;
	};

	struct AstUserAttrib
	{
		AstUserAttrib(u64 startIdx, StdString&& iden, StdVector<AstArgSPtr>&& args, u64 endTokIdx);

		StdString iden;
		StdVector<AstArgSPtr> args;
		AstContextPtr ctx;
	};

	struct AstVisibilityAttrib
	{
		AstVisibilityAttrib(u64 startIdx, StdString&& kind, u64 endTokId);

		StdString kind;
		AstContextPtr ctx;
	};

	struct AstSimpleAttrib
	{
		AstSimpleAttrib(Token attrib);

		TokenType attrib;
		AstContextPtr ctx;
	};

	enum class AstGenericParamKind : u8
	{
		Invalid,
		TypeParam,
		ValueParam,
		TypeSpec,
		ValueSpec,
	};
	
	struct AstGenericParam
	{
		AstGenericParam(AstGenericTypeParamSPtr typeParam);
		AstGenericParam(AstGenericValueParamSPtr valueParam);
		AstGenericParam(AstTypeSPtr typeSpec);
		AstGenericParam(AstExprSPtr valueSpec);
		AstGenericParam(AstGenericParam&& param) noexcept;
		~AstGenericParam();
		
		AstGenericParamKind kind;
		union
		{
			AstGenericTypeParamSPtr typeParam;
			AstGenericValueParamSPtr valueParam;
			AstTypeSPtr typeSpec;
			AstExprSPtr valueSpec;
		};
	};

	struct AstGenericDecl
	{
		AstGenericDecl(u64 startIdx, StdVector<AstGenericParam>&& params, u64 endIdx);

		StdVector<AstGenericParam> params;
		AstContextPtr ctx;
	};

	struct AstGenericTypeParam
	{
		AstGenericTypeParam(u64 startIdx, StdString&& iden,
			StdVector<AstIdentifierTypeSPtr>&& implTypes, AstTypeSPtr defType);

		StdString iden;
		StdVector<AstIdentifierTypeSPtr> implTypes;
		AstTypeSPtr defType;
		AstContextPtr ctx;
	};

	struct AstGenericValueParam
	{
		AstGenericValueParam(u64 startIdx, StdString&& iden, AstTypeSPtr type, AstExprSPtr defExpr);

		StdString iden;
		AstTypeSPtr type;
		AstExprSPtr defExpr;
		AstContextPtr ctx;
	};

	struct AstGenericTypeBound
	{
		AstGenericTypeBound(AstTypeSPtr type, AstTypeSPtr bound);

		AstTypeSPtr type;
		AstTypeSPtr bound;
		AstContextPtr ctx;
	};
	using AstGenericTypeBoundSPtr = StdSharedPtr<AstGenericTypeBound>;
	
	struct AstGenericWhereClause
	{
		AstGenericWhereClause(u64 startIdx, StdVector<AstGenericTypeBoundSPtr>&& bounds);

		StdVector<AstGenericTypeBoundSPtr> bounds;
		AstContextPtr ctx;
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

	struct AstMacroPatternElem
	{
		AstMacroPatternElem(AstMacroPatternElemKind kind, u64 startIdx, u64 endIdx);
		
		AstMacroPatternElemKind elemKind;
		AstContextPtr ctx;
	};
	using AstMacroPatternElemSPtr = StdSharedPtr<AstMacroPatternElem>;
	
	struct AstMacroVar : public AstMacroPatternElem
	{
		AstMacroVar(u64 startIdx, StdString&& iden, AstMacroVarKind kind, u64 endIdx);

		StdString iden;
		AstMacroVarKind kind;
	};

	struct AstMacroSeparator : public AstMacroPatternElem
	{
		AstMacroSeparator(StdVector<Token>&& toks);

		StdVector<Token> toks;
	};

	struct AstMacroFragment : public AstMacroPatternElem
	{
		AstMacroFragment(u64 startIdx, AstMacroPatternSPtr subPattern, TokenType repType, 
			u64 endTokIdx);

		AstMacroPatternSPtr subPattern;
		TokenType repType;
	};
	
	struct AstMacroPattern
	{
		AstMacroPattern(u64 startTokIdx, StdVector<AstMacroPatternElemSPtr>&& elems, u64 endTokIdx);

		StdVector<AstMacroPatternElemSPtr> elems;
		AstContextPtr ctx;
	};
	using AstMacroPatternSPtr = StdSharedPtr<AstMacroPattern>;

	struct AstMacroRule
	{
		AstMacroRule(u64 startIdx, AstMacroPatternSPtr pattern, StdVector<AstStmtSPtr>&& body,
			u64 endIdx);

		AstMacroPatternSPtr pattern;
		StdVector<AstStmtSPtr> body;
		AstContextPtr ctx;
	};
	using AstMacroRuleSPtr = StdSharedPtr<AstMacroRule>;
	
	struct AstDeclMacro : public AstDecl
	{
		AstDeclMacro(u64 startIdx, StdString&& iden, AstMacroPatternSPtr pattern,
			StdVector<AstStmtSPtr>&& body, u64 endIdx);

		StdString iden;
		AstMacroPatternSPtr pattern;
		StdVector<AstStmtSPtr> body;
	};

	struct AstRulesDeclMacro : public AstDecl
	{
		AstRulesDeclMacro(u64 startIdx, StdString&& iden, StdVector<AstMacroRuleSPtr>&& rules,
			u64 endIdx);

		StdString iden;
		StdVector<AstMacroRuleSPtr> rules;
	};

	struct AstProcMacro : public AstDecl
	{
		AstProcMacro(u64 startIdx, StdString&& iden, StdString&& tokStreamIden,
			AstMacroPatternSPtr pattern, StdVector<AstStmtSPtr>&& body, u64 endIdx);

		StdString iden;
		StdString tokStreamIden;
		AstMacroPatternSPtr pattern;
		StdVector<AstStmtSPtr> body;
	};

	struct AstRulesProcMacro : public AstDecl
	{
		AstRulesProcMacro(u64 startIdx, StdString&& iden, StdString&& tokStreamIden, StdVector<AstMacroRuleSPtr>&& rules, u64 endIdx);

		StdString iden;
		StdString tokStreamIden;
		StdVector<AstMacroRuleSPtr> rules;
	};

	struct AstMacroInst : public AstExpr
	{
		AstMacroInst(AstQualNameSPtr qualName, StdVector<Token>& toks, u64 endIdx);

		AstQualNameSPtr qualName;
		StdVector<Token> toks;
	};

	
}
