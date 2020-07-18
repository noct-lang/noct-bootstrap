#include "ast.hpp"
#include "common/logger.hpp"
#include "tokens/token.hpp"

namespace Noctis
{
	AstStmt::AstStmt(AstStmtKind kind, u64 startIdx, u64 endIdx)
		: stmtKind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstStmt::~AstStmt()
	{
	}

	AstDecl::AstDecl(AstDeclKind kind, u64 startIdx, u64 endIdx)
		: AstStmt(AstStmtKind::Decl, startIdx, endIdx)
		, declKind(kind)
	{
	}

	AstExpr::AstExpr(AstExprKind kind, u64 startIdx, u64 endIdx)
		: exprKind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstType::AstType(AstAttribsSPtr attribs, AstTypeKind kind, u64 startIdx, u64 endIdx)
		: attribs(attribs)
		, typeKind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstQualIden::AstQualIden(AstQualIdenKind kind, u64 startIdx, u64 endIdx)
		: qualIdenKind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstIden::AstIden(u64 startIdx, StdString&& iden, StdVector<AstGenericArg>&& args, u64 endIdx)
		: AstQualIden(AstQualIdenKind::Identifier, startIdx, endIdx)
		, iden(std::move(iden))
		, args(std::move(args))
	{
	}

	AstTypeDisambiguation::AstTypeDisambiguation(u64 startIdx, AstTypeSPtr type, AstIdentifierTypeSPtr interface,
		u64 endIdx)
		: AstQualIden(AstQualIdenKind::TypeDisambiguation, startIdx, endIdx)
		, type(type)
		, interface(interface)
	{
	}

	AstQualName::AstQualName(u64 startIdx, bool global, StdVector<AstQualIdenSPtr>&& idens)
		: global(global)
		, idens(std::move(idens))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = this->idens.back()->ctx->endIdx;
	}

	AstParamVar::AstParamVar(AstAttribsSPtr attribs, u64 startIdx, StdString&& label, StdString&& iden, u64 endIdx)
		: attribs(attribs)
		, label(std::move(label))
		, iden(std::move(iden))
		, ctx(new AstContext{})
	{
		ctx->startIdx = attribs ? attribs->ctx->startIdx : startIdx;
		ctx->endIdx = endIdx;
	}

	AstParam::AstParam(u64 startIdx, StdVector<AstParamVarSPtr>&& vars, AstTypeSPtr type,
	                   bool isVariadic, u64 endIdx)
		: vars(std::move(vars))
		, type(type)
		, isVariadic(isVariadic)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstArg::AstArg(u64 startIdx, StdString&& iden, AstExprSPtr expr)
		: iden(std::move(iden))
		, expr(expr)
		, ctx(AstContextPtr{ new AstContext{} })
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = expr->ctx->endIdx;
	}

	AstGenericArg::AstGenericArg(AstTypeSPtr type)
		: kind(GenericArgKind::Type)
		, type(type)
	{
	}

	AstGenericArg::AstGenericArg(AstExprSPtr expr)
		: kind(GenericArgKind::Expr)
		, expr(expr)
	{
	}

	AstGenericArg::AstGenericArg(AstGenericArg&& arg) noexcept
		: kind(arg.kind)
	{
		if (kind == GenericArgKind::Type)
			new (&type) AstTypeSPtr{ std::move(arg.type) };
		else
			new (&expr) AstExprSPtr{ std::move(arg.expr) };
	}

	AstGenericArg::~AstGenericArg()
	{
		if (kind == GenericArgKind::Type)
			type.~shared_ptr();
		else
			expr.~shared_ptr();
	}

	AstModuleDecl::AstModuleDecl(u64 startIdx, StdVector<StdString>&& moduleIdens, u64 endIdx)
		: AstDecl(AstDeclKind::Module, startIdx, endIdx)
		, moduleIdens(std::move(moduleIdens))
	{
	}

	AstUnittestDecl::AstUnittestDecl(u64 startIdx, StdString&& name, StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstDecl(AstDeclKind::UnitTest, startIdx, endIdx)
		, name(std::move(name))
		, stmts(std::move(stmts))
	{
	}

	AstBenchmarkDecl::AstBenchmarkDecl(u64 startIdx, StdString&& name, StdString&& stateIden, StdVector<AstStmtSPtr>&& stmts,
		u64 endIdx)
		: AstDecl(AstDeclKind::Benchmark, startIdx, endIdx)
		, name(std::move(name))
		, stateIden(std::move(stateIden))
		, stmts(std::move(stmts))
	{
	}

	AstStructDecl::AstStructDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstGenericDeclSPtr generics,
		StdVector<AstStmtSPtr>&& members, u64 endIdx)
		: AstDecl(AstDeclKind::Struct, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	AstUnionDecl::AstUnionDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstGenericDeclSPtr generics,
		StdVector<AstStmtSPtr>&& members, u64 endIdx)
		: AstDecl(AstDeclKind::Union, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	AstValueEnumDecl::AstValueEnumDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstTypeSPtr baseType,
	    StdPairVector<StdString, AstExprSPtr>&& members, u64 endIdx)
		: AstDecl(AstDeclKind::ValueEnum, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, members(std::move(members))
	{
	}

	AstAdtEnumDecl::AstAdtEnumDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstGenericDeclSPtr generics,
		StdPairVector<StdString, AstTypeSPtr>&& members, u64 endIdx)
		: AstDecl(AstDeclKind::AdtEnum, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	AstMarkerInterfaceDecl::AstMarkerInterfaceDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
		u64 endIdx)
		: AstDecl(AstDeclKind::MarkerInterface, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		
	{
	}

	AstWeakInterfaceDecl::AstWeakInterfaceDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
		StdVector<AstStmtSPtr>&& members, u64 endIdx)
		: AstDecl(AstDeclKind::WeakInterface, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, members(std::move(members))
	{
	}

	AstStrongInterfaceDecl::AstStrongInterfaceDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden,
		AstGenericDeclSPtr generics, StdVector<AstIdentifierTypeSPtr>&& implInterfaces, StdVector<AstStmtSPtr>&& members, u64 endIdx)
		: AstDecl(AstDeclKind::StrongInterface, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, implInterfaces(std::move(implInterfaces))
		, members(std::move(members))
	{
	}

	AstTypeAliasDecl::AstTypeAliasDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstGenericDeclSPtr generics,
		AstTypeSPtr type, u64 endIdx)
		: AstDecl(AstDeclKind::Typealias, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, type(type)
	{
	}

	AstTypeDefDecl::AstTypeDefDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstGenericDeclSPtr generics,
		AstTypeSPtr type, u64 endIdx)
		: AstDecl(AstDeclKind::Typedef, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, type(type)
	{
	}

	AstVarDecl::AstVarDecl(AstAttribsSPtr attribs, u64 startTokIdx, StdVector<StdString>&& idens, AstTypeSPtr type,
		AstExprSPtr expr, u64 termTokIdx)
		: AstDecl(AstDeclKind::Var  , startTokIdx, termTokIdx)
		, attribs(attribs)
		, idens(std::move(idens))
		, type(type)
		, expr(expr)
	{
	}

	AstFuncDecl::AstFuncDecl(AstAttribsSPtr attribs, u64 startIdx, StdString&& iden, AstGenericDeclSPtr generics,
		StdVector<AstParamSPtr>&& params, bool throws, AstTypeSPtr errorType, AstTypeSPtr retType,
		StdPairVector<StdVector<StdString>, AstTypeSPtr>&& namedRet, AstGenericWhereClauseSPtr whereClause,
		StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstDecl(AstDeclKind::Func, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, params(std::move(params))
		, throws(throws)
		, errorType(errorType)
		, retType(retType)
		, namedRet(std::move(namedRet))
		, whereClause(whereClause)
		, stmts(std::move(stmts))
	{
	}

	AstMethodDecl::AstMethodDecl(AstAttribsSPtr attribs, u64 startIdx, AstMethodReceiverKind rec, StdString&& iden,
		AstGenericDeclSPtr generics, StdVector<AstParamSPtr>&& params, bool throws, AstTypeSPtr errorType, AstTypeSPtr retType,
		StdPairVector<StdVector<StdString>, AstTypeSPtr>&& namedRet, AstGenericWhereClauseSPtr whereClause,
	    StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstDecl(AstDeclKind::Method, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, rec(rec)
		, iden(std::move(iden))
		, generics(generics)
		, params(std::move(params))
		, throws(throws)
		, errorType(errorType)
		, retType(retType)
		, namedRet(std::move(namedRet))
		, whereClause(whereClause)
		, stmts(std::move(stmts))
	{
	}

	AstEmptyMethodDecl::AstEmptyMethodDecl(AstAttribsSPtr attribs, u64 startIdx, AstMethodReceiverKind rec,
	    StdString&& iden, AstGenericDeclSPtr generics, StdVector<AstParamSPtr>&& params,
		AstTypeSPtr retType, u64 endIdx)
		: AstDecl(AstDeclKind::EmptyMethod, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, rec(rec)
		, iden(std::move(iden))
		, generics(generics)
		, params(std::move(params))
		, retType(retType)
	{
	}

	AstImplDecl::AstImplDecl(AstAttribsSPtr attribs, u64 startIdx, AstGenericDeclSPtr generics, AstTypeSPtr type,
		StdVector<AstIdentifierTypeSPtr>&& interfaces, AstGenericWhereClauseSPtr whereClause, StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstDecl(AstDeclKind::Impl, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, generics(generics)
		, type(type)
		, interfaces(std::move(interfaces))
		, whereClause(whereClause)
		, stmts(std::move(stmts))
	{
	}

	AstImportStmt::AstImportStmt(AstAttribsSPtr attribs, u64 startIdx, StdVector<StdString>&& moduleIdens,
	                             StdPairVector<StdVector<StdString>, StdString>&& symbols, u64 endIdx)
		: AstStmt(AstStmtKind::Import, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, attribs(attribs)
		, moduleIdens(std::move(moduleIdens))
		, symbols(std::move(symbols))
	{
	}

	AstBlockStmt::AstBlockStmt(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstStmt(AstStmtKind::Block, startIdx, endIdx)
		, stmts(std::move(stmts))
	{
	}

	AstIfStmt::AstIfStmt(u64 startIdx, AstVarDeclSPtr decl, AstExprSPtr cond, AstStmtSPtr body, AstStmtSPtr elseBody)
		: AstStmt(AstStmtKind::If, startIdx, elseBody ? elseBody->ctx->endIdx : body->ctx->endIdx)
		, decl(decl)
		, cond(cond)
		, body(body)
		, elseBody(elseBody)
	{
	}

	AstLoopStmt::AstLoopStmt(AstLabelStmtSPtr label, u64 startIdx, AstStmtSPtr body)
		: AstStmt(AstStmtKind::Loop, label ? label->ctx->startIdx : startIdx, body->ctx->endIdx)
		, label(label)
		, body(body)
	{
	}

	AstWhileStmt::AstWhileStmt(AstLabelStmtSPtr label, u64 startIdx, AstExprSPtr cond, AstStmtSPtr body)
		: AstStmt(AstStmtKind::While, label ? label->ctx->startIdx : startIdx, body->ctx->endIdx)
		, label(label)
		, cond(cond)
		, body(body)
	{
	}

	AstDoWhileStmt::AstDoWhileStmt(AstLabelStmtSPtr label, u64 startIdx, AstStmtSPtr body, AstExprSPtr cond, u64 endIdx)
		: AstStmt(AstStmtKind::DoWhile, label ? label->ctx->startIdx : startIdx, endIdx)
		, label(label)
		, body(body)
		, cond(cond)
	{
	}

	AstForStmt::AstForStmt(AstLabelStmtSPtr label, u64 startIdx, StdVector<StdString>&& idens, AstExprSPtr range, 
		AstStmtSPtr body)
		: AstStmt(AstStmtKind::For, label ? label->ctx->startIdx : startIdx, body->ctx->endIdx)
		, label(label)
		, idens(std::move(idens))
		, range(range)
		, body(body)
	{
	}

	AstSwitchStmt::AstSwitchStmt(AstLabelStmtSPtr label, u64 startIdx, AstExprSPtr cond, StdVector<AstSwitchCase>&& cases, u64 endIdx)
		: AstStmt(AstStmtKind::Switch, label ? label->ctx->startIdx : startIdx, endIdx)
		, label(label)
		, cond(cond)
		, cases(std::move(cases))
	{
	}

	AstLabelStmt::AstLabelStmt(u64 startIdx, StdString&& iden, u64 endIdx)
		: AstStmt(AstStmtKind::Label, startIdx, endIdx)
		, iden(std::move(iden))
	{
	}
	
	AstBreakStmt::AstBreakStmt(u64 startIdx, StdString&& iden, u64 endIdx)
		: AstStmt(AstStmtKind::Break, startIdx, endIdx)
		, iden(std::move(iden))
	{
	}

	AstContinueStmt::AstContinueStmt(u64 startIdx, StdString&& iden, u64 endIdx)
		: AstStmt(AstStmtKind::Continue, startIdx, endIdx)
		, iden(std::move(iden))
	{
	}

	AstFallthroughStmt::AstFallthroughStmt(u64 startIdx, u64 endIdx)
		: AstStmt(AstStmtKind::Fallthrough, startIdx, endIdx)
	{
	}

	AstGotoStmt::AstGotoStmt(u64 startIdx, StdString&& iden, u64 endIdx)
		: AstStmt(AstStmtKind::Goto, startIdx, endIdx)
		, iden(std::move(iden))
	{
	}

	AstReturnStmt::AstReturnStmt(u64 startIdx, AstExprSPtr expr, u64 endIdx)
		: AstStmt(AstStmtKind::Return, startIdx, endIdx)
		, expr(expr)
	{
	}


	AstThrowStmt::AstThrowStmt(u64 startIdx, AstExprSPtr expr)
		: AstStmt(AstStmtKind::Throw, startIdx, expr->ctx->endIdx)
		, expr(expr)
	{
	}


	AstExprStmt::AstExprStmt(AstExprSPtr expr, u64 endIdx)
		: AstStmt(AstStmtKind::Expr, expr->ctx->startIdx, endIdx)
		, expr(expr)
	{
	}

	AstDeferStmt::AstDeferStmt(u64 startIdx, AstExprSPtr expr, u64 endIdx)
		: AstStmt(AstStmtKind::Defer, startIdx, endIdx)
		, expr(expr)
	{
	}

	AstErrDeferStmt::AstErrDeferStmt(u64 startIdx, AstExprSPtr expr, u64 endIdx)
		: AstStmt(AstStmtKind::ErrDefer, startIdx, endIdx)
		, expr(expr)
	{
	}

	AstUnsafeStmt::AstUnsafeStmt(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstStmt(AstStmtKind::Unsafe, startIdx, endIdx)
		, stmts(std::move(stmts))
	{
	}

	AstErrorHandlerStmt::AstErrorHandlerStmt(u64 startIdx, StdString&& errIden, AstTypeSPtr errType,
		StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstStmt(AstStmtKind::ErrorHandler, startIdx, endIdx)
		, errIden(std::move(errIden))
		, errType(std::move(errType))
		, stmts(std::move(stmts))
	{
	}

	AstCompIfStmt::AstCompIfStmt(u64 startIdx, AstVarDeclSPtr decl, AstExprSPtr expr, AstStmtSPtr body,
	                             AstStmtSPtr elseBody)
		: AstStmt(AstStmtKind::CompIf, startIdx, elseBody ? elseBody->ctx->endIdx : body->ctx->endIdx)
		, decl(decl)
		, cond(expr)
		, body(body)
		, elseBody(elseBody)
	{
	}

	AstCompCondStmt::AstCompCondStmt(u64 startIdx, Token cond, Token cmp, Token val, AstStmtSPtr body, AstStmtSPtr elseBody)
		: AstStmt(AstStmtKind::CompCond, startIdx, elseBody ? elseBody->ctx->endIdx : body->ctx->endIdx)
		, cond(cond)
		, cmp(cmp)
		, val(val)
		, body(body)
		, elseBody(elseBody)
	{
	}

	AstCompDebugStmt::AstCompDebugStmt(u64 startIdx, Token cond, Token cmp, Token val, AstStmtSPtr body, AstStmtSPtr elseBody)
		: AstStmt(AstStmtKind::CompDebug, startIdx, elseBody ? elseBody->ctx->endIdx : body->ctx->endIdx)
		, cond(cond)
		, cmp(cmp)
		, val(val)
		, body(body)
		, elseBody(elseBody)
	{
	}
	
	AstMacroLoopStmt::AstMacroLoopStmt(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstStmt(AstStmtKind::MacroLoop, startIdx, endIdx)
		, stmts(std::move(stmts))
	{
	}

	AstAssignExpr::AstAssignExpr(AstExprSPtr lExpr, TokenType op, AstExprSPtr rExpr)
		: AstExpr(AstExprKind::Assign, lExpr->ctx->startIdx, rExpr->ctx->endIdx)
		, lExpr(lExpr)
		, op(op)
		, rExpr(rExpr)
	{
	}

	AstTernaryExpr::AstTernaryExpr(AstExprSPtr cond, AstExprSPtr trueExpr, AstExprSPtr falseExpr)
		: AstExpr(AstExprKind::Ternary, cond->ctx->startIdx, falseExpr->ctx->endIdx)
		, cond(cond)
		, trueExpr(trueExpr)
		, falseExpr(falseExpr)
	{
	}

	AstBinaryExpr::AstBinaryExpr(AstExprSPtr lExpr, TokenType op, AstExprSPtr rExpr)
		: AstExpr(AstExprKind::Binary, lExpr->ctx->startIdx, rExpr->ctx->endIdx)
		, lExpr(lExpr)
		, op(op)
		, rExpr(rExpr)
	{
	}

	AstPostfixExpr::AstPostfixExpr(AstExprSPtr expr, Token op)
		: AstExpr(AstExprKind::Postfix, expr->ctx->startIdx, op.Idx())
		, expr(expr)
		, op(op.Type())
	{
	}

	AstPrefixExpr::AstPrefixExpr(Token op, AstExprSPtr expr)
		: AstExpr(AstExprKind::Prefix, op.Idx(), expr->ctx->endIdx)
		, op(op.Type())
		, expr(expr)
	{
	}

	AstQualNameExpr::AstQualNameExpr(AstQualNameSPtr qualName)
		: AstExpr(AstExprKind::QualName, qualName->ctx->startIdx, qualName->ctx->endIdx)
		, qualName(qualName)
	{
	}

	AstIndexSliceExpr::AstIndexSliceExpr(AstExprSPtr expr, bool nullCoalesce, AstExprSPtr index, u64 endIdx)
		: AstExpr(AstExprKind::IndexSlice, expr->ctx->startIdx, endIdx)
		, expr(expr)
		, nullCoalesce(nullCoalesce)
		, index(index)
	{
	}

	AstSliceExpr::AstSliceExpr(AstExprSPtr expr, bool nullCoalesce, AstExprSPtr begin, AstExprSPtr end, u64 endIdx)
		: AstExpr(AstExprKind::Slice, expr->ctx->startIdx, endIdx)
		, expr(expr)
		, nullCoalesce(nullCoalesce)
		, begin(begin)
		, end(end)
	{
	}

	AstFuncCallExpr::AstFuncCallExpr(AstExprSPtr func, StdVector<AstArgSPtr>&& args, u64 endIdx)
		: AstExpr(AstExprKind::FuncCall, func->ctx->startIdx, endIdx)
		, func(func)
		, args(std::move(args))
	{
	}

	AstMemberAccessExpr::AstMemberAccessExpr(AstExprSPtr caller, bool nullCoalesce, StdString&& iden, u64 endIdx)
		: AstExpr(AstExprKind::MemberAccess, caller->ctx->startIdx, endIdx)
		, caller(caller)
		, nullCoalesce(nullCoalesce)
		, iden(std::move(iden))
	{
	}

	AstMethodCallExpr::AstMethodCallExpr(AstExprSPtr caller, bool nullCoalesce, StdString&& iden, 
		StdVector<AstArgSPtr>&& args, u64 endIdx)
		: AstExpr(AstExprKind::MethodCall, caller->ctx->startIdx, endIdx)
		, caller(caller)
		, nullCoalesce(nullCoalesce)
		, iden(std::move(iden))
		, args(std::move(args))
	{
	}

	AstTupleAccessExpr::AstTupleAccessExpr(AstExprSPtr expr, bool nullCoalesce, u16 index, u64 endIdx)
		: AstExpr(AstExprKind::TupleAccess, expr->ctx->startIdx, endIdx)
		, expr(expr)
		, nullCoalesce(nullCoalesce)
		, index(index)
	{
	}
	AstLiteralExpr::AstLiteralExpr(Token literal)
		: AstExpr(AstExprKind::Literal, literal.Idx(), literal.Idx())
		, literal(literal)
	{
	}

	AstAggrInitExpr::AstAggrInitExpr(u64 startIdx, AstTypeSPtr type, StdVector<AstArgSPtr>&& args, bool hasDefInit,
		AstExprSPtr defExpr, u64 endIdx)
		: AstExpr(AstExprKind::AggrInit, startIdx, endIdx)
		, type(type)
		, args(std::move(args))
		, defExpr(defExpr)
		, hasDefInit(hasDefInit)
	{
	}

	AstTupleInitExpr::AstTupleInitExpr(u64 startIdx, StdVector<AstExprSPtr>&& exprs, u64 endIdx)
		: AstExpr(AstExprKind::TupleInit, startIdx, endIdx)
		, exprs(std::move(exprs))
	{
	}

	AstArrayInitExpr::AstArrayInitExpr(u64 startIdx, StdVector<AstExprSPtr>&& exprs, u64 endIdx)
		: AstExpr(AstExprKind::ArrayInit, startIdx, endIdx)
		, exprs(std::move(exprs))
	{
	}

	AstCastExpr::AstCastExpr(AstExprSPtr expr, TokenType castType, AstTypeSPtr type)
		: AstExpr(AstExprKind::Cast, expr->ctx->startIdx, type->ctx->endIdx)
		, expr(expr)
		, castType(castType)
		, type(type)
	{
	}

	AstTransmuteExpr::AstTransmuteExpr(AstExprSPtr expr, AstTypeSPtr type)
		: AstExpr(AstExprKind::Transmute, expr->ctx->startIdx, type->ctx->endIdx)
		, expr(expr)
		, type(type)
	{
	}

	AstMoveExpr::AstMoveExpr(u64 startIdx, AstExprSPtr expr)
		: AstExpr(AstExprKind::Move, startIdx, expr->ctx->endIdx)
		, expr(expr)
	{
	}

	AstBracketExpr::AstBracketExpr(u64 startIdx, AstExprSPtr expr, u64 endIdx)
		: AstExpr(AstExprKind::Bracket, startIdx, endIdx)
		, expr(expr)
	{
	}

	AstBlockExpr::AstBlockExpr(u64 startIdx, StdVector<AstStmtSPtr>&& stmts, u64 endIdx)
		: AstExpr(AstExprKind::Block, startIdx, endIdx)
		, stmts(std::move(stmts))
	{
	}

	AstUnsafeExpr::AstUnsafeExpr(u64 startIdx, AstExprSPtr expr)
		: AstExpr(AstExprKind::Unsafe, startIdx, expr->ctx->endIdx)
		, expr(expr)
	{
	}

	AstCommaExpr::AstCommaExpr(StdVector<AstExprSPtr>&& exprs)
		: AstExpr(AstExprKind::Comma, exprs.front()->ctx->startIdx, exprs.back()->ctx->endIdx)
		, exprs(std::move(exprs))
	{
	}

	AstClosureExpr::AstClosureExpr(u64 startIdx, StdVector<AstParamSPtr>&& params, AstTypeSPtr ret, AstExprSPtr expr)
		: AstExpr(AstExprKind::Closure, startIdx, expr->ctx->endIdx)
		, params(std::move(params))
		, ret(ret)
		, expr(expr)
	{
	}

	AstIsExpr::AstIsExpr(AstExprSPtr expr, u64 isIdx, AstTypeSPtr type)
		: AstExpr(AstExprKind::Is, expr ? expr->ctx->startIdx : isIdx, type->ctx->endIdx)
		, expr(expr)
		, type(type)
	{
	}

	AstTryExpr::AstTryExpr(u64 startIdx, AstExprSPtr call)
		: AstExpr(AstExprKind::Try, startIdx, call->ctx->endIdx)
		, call(call)
	{
	}

	AstSpecKwExpr::AstSpecKwExpr(Token& tok)
		: AstExpr(AstExprKind::SpecKw, tok.Idx(), tok.Idx())
		, specKw(tok.Type())
	{
	}

	AstCompRunExpr::AstCompRunExpr(u64 startIdx, AstExprSPtr expr)
		: AstExpr(AstExprKind::CompRun, startIdx, expr->ctx->endIdx)
		, expr(expr)
	{
	}

	AstMacroVarExpr::AstMacroVarExpr(u64 startIdx, StdString&& iden, u64 endIdx)
		: AstExpr(AstExprKind::MacroVar, startIdx, endIdx)
		, iden(std::move(iden))
	{
	}

	AstBuiltinType::AstBuiltinType(AstAttribsSPtr attribs, const Token& tok)
		: AstType(attribs, AstTypeKind::Builtin, tok.Idx(), tok.Idx())
		, type(tok.Type())
	{
	}

	AstIdentifierType::AstIdentifierType(AstAttribsSPtr attribs, AstQualNameSPtr qualName)
		: AstType(attribs, AstTypeKind::Iden, attribs ? attribs->ctx->startIdx : qualName->ctx->startIdx, 
			qualName->ctx->endIdx)
		, qualName(qualName)
	{
	}
	
	AstPointerType::AstPointerType(AstAttribsSPtr attribs, u64 startIdx, AstTypeSPtr subType)
		: AstType(attribs, AstTypeKind::Ptr, attribs ? attribs->ctx->startIdx : startIdx, subType->ctx->endIdx)
		, subType(subType)
	{
	}

	AstReferenceType::AstReferenceType(AstAttribsSPtr attribs, u64 startIdx, AstTypeSPtr subType)
		: AstType(attribs, AstTypeKind::Ref, attribs ? attribs->ctx->startIdx : startIdx, subType->ctx->endIdx)
		, subType(subType)
	{
	}

	AstArrayType::AstArrayType(AstAttribsSPtr attribs, u64 startIdx, AstExprSPtr arraySize, AstTypeSPtr subType)
		: AstType(attribs, AstTypeKind::Arr, attribs ? attribs->ctx->startIdx : startIdx, subType->ctx->endIdx)
		, arraySize(arraySize)
		, subType(subType)
	{
	}

	AstSliceType::AstSliceType(AstAttribsSPtr attribs, u64 startIdx, AstTypeSPtr subType)
		: AstType(attribs, AstTypeKind::Slice, attribs ? attribs->ctx->startIdx : startIdx, subType->ctx->endIdx)
		, subType(subType)
	{
	}

	AstTupleType::AstTupleType(AstAttribsSPtr attribs, u64 startIdx, StdVector<AstTypeSPtr>&& subTypes, u64 endIdx)
		: AstType(attribs, AstTypeKind::Tuple, attribs ? attribs->ctx->startIdx : startIdx, endIdx)
		, subTypes(std::move(subTypes))
	{
	}

	AstOptionalType::AstOptionalType(AstAttribsSPtr attribs, u64 startIdx, AstTypeSPtr subType)
		: AstType(attribs, AstTypeKind::Optional, attribs ? attribs->ctx->startIdx : startIdx, subType->ctx->endIdx)
		, subType(subType)
	{
	}

	AstInlineStructType::AstInlineStructType(u64 startIdx, StdPairVector<StdVector<StdString>, AstTypeSPtr>&& members,
		u64 endIdx)
		: AstType(nullptr, AstTypeKind::InlineStruct, startIdx, endIdx)
		, members(std::move(members))
	{
	}

	AstInlineEnumType::AstInlineEnumType(u64 startIdx, StdPairVector<StdString, AstExprSPtr>&& members, u64 endIdx)
		: AstType(nullptr, AstTypeKind::InlineStruct, startIdx, endIdx)
		, members(std::move(members))
	{
	}

	AstCompoundInterfaceType::AstCompoundInterfaceType(StdVector<AstIdentifierTypeSPtr>&& interfaces)
		: AstType(nullptr, AstTypeKind::CompoundInterface, interfaces.front()->ctx->startIdx, interfaces.back()->ctx->endIdx)
		, interfaces(std::move(interfaces))
	{
	}

	AstPattern::AstPattern(AstPatternKind kind, u64 startIdx, u64 endIdx)
		: patternKind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstPlaceholderPattern::AstPlaceholderPattern(u64 tokIdx)
		: AstPattern(AstPatternKind::Placeholder, tokIdx, tokIdx)
	{
	}

	AstWildcardPattern::AstWildcardPattern(u64 tokIdx)
		: AstPattern(AstPatternKind::Wildcard, tokIdx, tokIdx)
	{
	}

	AstValueBindPattern::AstValueBindPattern(u64 startIdx, StdString&& iden, AstPatternSPtr subPattern, u64 endIdx)
		: AstPattern(AstPatternKind::ValueBind, startIdx, endIdx)
		, iden(std::move(iden))
		, subPattern(subPattern)
	{
	}

	AstLiteralPattern::AstLiteralPattern(Token& tok)
		: AstPattern(AstPatternKind::Literal, tok.Idx(), tok.Idx())
		, literal(tok)
	{
	}

	AstRangePattern::AstRangePattern(AstPatternSPtr from, bool inclusive, AstPatternSPtr to)
		: AstPattern(AstPatternKind::Range, from->ctx->startIdx, from->ctx->endIdx)
		, from(from)
		, inclusive(inclusive)
		, to(to)
	{
	}

	AstTuplePattern::AstTuplePattern(u64 startIdx, StdVector<AstPatternSPtr>&& subPatterns, u64 endIdx)
		: AstPattern(AstPatternKind::Tuple, startIdx, endIdx)
		, subPatterns(std::move(subPatterns))
	{
	}

	AstEnumPattern::AstEnumPattern(u64 startIdx, AstQualNameSPtr iden, StdVector<AstPatternSPtr>&& subPatterns, u64 endIdx)
		: AstPattern(AstPatternKind::Enum, startIdx, endIdx)
		, iden(iden)
		, subPatterns(std::move(subPatterns))
	{
	}

	AstAggrPattern::AstAggrPattern(AstQualNameSPtr qualName, StdPairVector<StdString, AstPatternSPtr>&& subPatterns, 
		u64 endIdx)
		: AstPattern(AstPatternKind::Aggr, qualName->ctx->startIdx, endIdx)
		, qualName(qualName)
		, subPatterns(std::move(subPatterns))
	{
	}

	AstSlicePattern::AstSlicePattern(u64 startIdx, StdVector<AstPatternSPtr>&& subPatterns, u64 endIdx)
		: AstPattern(AstPatternKind::Slice, startIdx, endIdx)
		, subPatterns(std::move(subPatterns))
	{
	}

	AstEitherPattern::AstEitherPattern(StdVector<AstPatternSPtr>&& subPatterns)
		: AstPattern(AstPatternKind::Either, subPatterns.front()->ctx->startIdx, subPatterns.back()->ctx->endIdx)
		, subPatterns(std::move(subPatterns))
	{
	}

	AstTypePattern::AstTypePattern(u64 startIdx, AstTypeSPtr type)
		: AstPattern(AstPatternKind::Type, startIdx, type->ctx->endIdx)
		, type(type)
	{
	}

	AstAttribs::AstAttribs(u64 startIdx, StdVector<AstCompAttribSPtr>&& compAttribs,
	                       StdVector<AstUserAttribSPtr>&& userAttribs, AstVisibilityAttribSPtr visibility,
	                       StdVector<AstSimpleAttribSPtr>&& simpleAttribs, u64 endIdx)
		: compAttribs(std::move(compAttribs))
		, userAttribs(std::move(userAttribs))
		, visibility(visibility)
		, simpleAttribs(std::move(simpleAttribs))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstCompAttrib::AstCompAttrib(u64 startIdx, StdString&& iden, StdVector<AstArgSPtr>&& args, u64 endIdx)
		: iden(std::move(iden))
		, args(std::move(args))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstUserAttrib::AstUserAttrib(u64 startIdx, StdString&& iden, StdVector<AstArgSPtr>&& args, u64 endIdx)
		: iden(std::move(iden))
		, args(std::move(args))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstVisibilityAttrib::AstVisibilityAttrib(u64 startIdx, StdString&& kind, u64 endIdx)
		: kind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstSimpleAttrib::AstSimpleAttrib(Token attrib)
		: attrib(attrib.Type())
		, ctx(new AstContext{})
	{
		ctx->startIdx = ctx->endIdx = attrib.Idx();
	}

	AstGenericParam::AstGenericParam(AstGenericTypeParamSPtr typeParam)
		: kind(AstGenericParamKind::TypeParam)
		, typeParam(typeParam)
	{
	}

	AstGenericParam::AstGenericParam(AstGenericValueParamSPtr valueParam)
		: kind(AstGenericParamKind::ValueParam)
		, valueParam(valueParam)
	{
	}

	AstGenericParam::AstGenericParam(AstTypeSPtr typeSpec)
		: kind(AstGenericParamKind::TypeSpec)
		, typeSpec(typeSpec)
	{
	}

	AstGenericParam::AstGenericParam(AstExprSPtr valueSpec)
		: kind(AstGenericParamKind::ValueSpec)
		, valueSpec(valueSpec)
	{
	}

	AstGenericParam::AstGenericParam(AstGenericParam&& param) noexcept
		: kind(param.kind)
	{
		switch (kind)
		{
		case AstGenericParamKind::Invalid: break;
		case AstGenericParamKind::TypeParam:
			new(&typeParam) AstGenericTypeParamSPtr{ std::move(param.typeParam) };
			break;
		case AstGenericParamKind::ValueParam:
			new(&valueParam) AstGenericValueParamSPtr{ std::move(param.valueParam) };
			break;
		case AstGenericParamKind::TypeSpec:
			new(&typeSpec) AstTypeSPtr{ std::move(param.typeSpec) };
			break;
		case AstGenericParamKind::ValueSpec:
			new(&valueSpec) AstExprSPtr{ std::move(param.valueSpec) };
			break;
		default: ;
		}
	}

	AstGenericParam::~AstGenericParam()
	{
		switch (kind)
		{
		case AstGenericParamKind::TypeParam: typeParam.~shared_ptr(); break;
		case AstGenericParamKind::ValueParam: valueParam.~shared_ptr(); break;
		case AstGenericParamKind::TypeSpec: typeSpec.~shared_ptr(); break;
		case AstGenericParamKind::ValueSpec: valueSpec.~shared_ptr(); break;
		default: ;
		}
	}

	AstGenericDecl::AstGenericDecl(u64 startIdx, StdVector<AstGenericParam>&& params, u64 endIdx)
		: params(std::move(params))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstGenericTypeParam::AstGenericTypeParam(u64 startIdx, StdString&& iden, 
		StdVector<AstIdentifierTypeSPtr>&& implTypes, AstTypeSPtr defType)
		: iden(std::move(iden))
		, implTypes(std::move(implTypes))
		, defType(defType)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = defType ? defType->ctx->endIdx : !implTypes.empty() ? implTypes.back()->ctx->endIdx : startIdx;
	}

	AstGenericValueParam::AstGenericValueParam(u64 startIdx, StdString&& iden, AstTypeSPtr type, AstExprSPtr defExpr)
		: iden(std::move(iden))
		, type(type)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = defExpr ? defExpr->ctx->endIdx : type->ctx->endIdx;
	}

	AstGenericTypeBound::AstGenericTypeBound(AstTypeSPtr type, AstTypeSPtr bound)
		: type(type)
		, bound(bound)
		, ctx(new AstContext{})
	{
		ctx->startIdx = type->ctx->startIdx;
		ctx->endIdx = type->ctx->endIdx;
	}

	AstGenericWhereClause::AstGenericWhereClause(u64 startIdx, StdVector<AstGenericTypeBoundSPtr>&& bounds)
		: bounds(bounds)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = bounds.back()->ctx->endIdx;
	}

	AstMacroPatternElem::AstMacroPatternElem(AstMacroPatternElemKind kind, u64 startIdx, u64 endIdx)
		: elemKind(kind)
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstMacroVar::AstMacroVar(u64 startIdx, StdString&& iden, AstMacroVarKind kind, u64 endIdx)
		: AstMacroPatternElem(AstMacroPatternElemKind::Variable, startIdx, endIdx)
		, iden(std::move(iden))
		, kind(kind)
	{
	}

	AstMacroSeparator::AstMacroSeparator(StdVector<Token>&& toks)
		: AstMacroPatternElem(AstMacroPatternElemKind::Separator, toks.front().Idx(), toks.back().Idx())
		, toks(std::move(toks))
	{
	}

	AstMacroFragment::AstMacroFragment(u64 startIdx, AstMacroPatternSPtr subPattern, Token repTok, TokenType repType, u64 endIdx)
		: AstMacroPatternElem(AstMacroPatternElemKind::Fragment, startIdx, endIdx)
		, subPattern(subPattern)
		, repTok(repTok)
		, repType(repType)
	{
	}

	AstMacroPattern::AstMacroPattern(u64 startIdx, StdVector<AstMacroPatternElemSPtr>&& elems, u64 endIdx)
		: elems(std::move(elems))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstMacroRule::AstMacroRule(u64 startIdx, AstMacroPatternSPtr pattern, TokenTree&& body, u64 endIdx)
		: pattern(pattern)
		, body(std::move(body))
		, ctx(new AstContext{})
	{
		ctx->startIdx = startIdx;
		ctx->endIdx = endIdx;
	}

	AstDeclMacro::AstDeclMacro(u64 startIdx, StdString&& iden, AstMacroPatternSPtr pattern, TokenTree&& body, u64 endIdx)
		: AstDecl(AstDeclKind::DeclMacro, startIdx, endIdx)
		, iden(std::move(iden))
		, pattern(pattern)
		, body(std::move(body))
	{
	}

	AstRulesDeclMacro::AstRulesDeclMacro(u64 startIdx, StdString&& iden, StdVector<AstMacroRuleSPtr>&& rules, u64 endIdx)
		: AstDecl(AstDeclKind::RulesDeclMacro, startIdx, endIdx)
		, iden(std::move(iden))
		, rules(std::move(rules))
	{
	}

	AstProcMacro::AstProcMacro(u64 startIdx, StdString&& iden, StdString&& tokStreamIden, AstMacroPatternSPtr pattern,
		TokenTree&& body, u64 endIdx)
		: AstDecl(AstDeclKind::ProcMacro, startIdx, endIdx)
		, iden(std::move(iden))
		, tokStreamIden(std::move(tokStreamIden))
		, pattern(pattern)
		, body(std::move(body))
	{
	}

	AstRulesProcMacro::AstRulesProcMacro(u64 startIdx, StdString&& iden, StdString&& tokStreamIden,
	    StdVector<AstMacroRuleSPtr>&& rules, u64 endIdx)
		: AstDecl(AstDeclKind::RulesProcMacro, startIdx, endIdx)
		, iden(std::move(iden))
		, tokStreamIden(std::move(tokStreamIden))
		, rules(std::move(rules))
	{
	}

	AstMacroInstStmt::AstMacroInstStmt(AstQualNameSPtr qualName, TokenTree&& toks, u64 endIdx)
		: AstStmt(AstStmtKind::MacroInst, qualName->ctx->startIdx, endIdx)
		, qualName(qualName)
		, toks(std::move(toks))
	{
	}

	AstMacroInstExpr::AstMacroInstExpr(AstQualNameSPtr qualName, TokenTree&& toks, u64 endIdx)
		: AstExpr(AstExprKind::MacroInst, qualName->ctx->startIdx, endIdx)
		, qualName(qualName)
		, toks(std::move(toks))
	{
	}

	AstMacroInstPattern::AstMacroInstPattern(AstQualNameSPtr qualName, TokenTree&& toks, u64 endIdx)
		: AstPattern(AstPatternKind::MacroInst, qualName->ctx->startIdx, endIdx)
		, qualName(qualName)
		, toks(std::move(toks))
	{
	}
}
