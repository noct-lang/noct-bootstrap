#include "itr.hpp"

#include "common/qualname.hpp"
#include "module/function.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	ITrIden::ITrIden(const StdString& iden, const StdVector<IdenGeneric>& generics, StdPairVector<ITrTypeSPtr, ITrExprSPtr>&& assocArgs, u64 startIdx, u64 endIdx)
		: iden(iden)
		, generics(std::move(generics))
		, assocArgs(std::move(assocArgs))
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrTypeDisambiguation::ITrTypeDisambiguation(TypeDisambiguationSPtr disambiguation, ITrTypeSPtr assocType, ITrQualNameSPtr assocQualName, u64 startIdx, u64 endIdx)
		: disambiguation(disambiguation)
		, assocType(assocType)
		, assocQualName(assocQualName)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrQualName::ITrQualName(QualNameSPtr qualName, ITrTypeDisambiguationSPtr assocDisambiguation, StdVector<ITrIdenSPtr>&& assocIdens, bool hasColonColon)
		: qualName(qualName)
		, assocDisambiguation(assocDisambiguation)
		, assocIdens(std::move(assocIdens))
		, hasColonColon(hasColonColon)
	{
		startIdx = assocDisambiguation ? assocDisambiguation->startIdx : this->assocIdens.front()->startIdx;
		endIdx = this->assocIdens.back()->endIdx;
	}

	ITrParam::ITrParam(ITrAttribsSPtr attribs, const StdString& iden, ITrTypeSPtr type, u64 startIdx, u64 endIdx)
		: attribs(attribs)
		, iden(iden)
		, type(type)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrArg::ITrArg(const StdString& iden, ITrExprSPtr expr, u64 startIdx)
		: iden(iden)
		, expr(expr)
		, startIdx(startIdx)
		, endIdx(expr->endIdx)
	{
	}

	ITrDef::ITrDef(ITrDefKind kind, ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx)
		: kind(kind)
		, attribs(attribs)
		, qualName(qualName)
		, genDecl(genDecl)
		, bodyIdx(u64(-1))
		, isModDef(isModDef)
		, isDummyDef(false)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrDef::~ITrDef()
	{
	}

	ITrStruct::ITrStruct(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Struct, attribs, genDecl, qualName, isModDef, startIdx, endIdx)
	{
	}

	ITrUnion::ITrUnion(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Union, attribs, genDecl, qualName, isModDef, startIdx, endIdx)
	{
	}

	ITrValEnum::ITrValEnum(ITrAttribsSPtr attribs, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::ValEnum, attribs, nullptr, qualName, isModDef, startIdx, endIdx)
	{
	}

	ITrValEnumMember::ITrValEnumMember(QualNameSPtr parent, const StdString& iden, ITrExprSPtr val, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::ValEnumMember, nullptr, nullptr, parent->Append(iden), false, startIdx, endIdx)
		, val(val)
	{
	}

	ITrAdtEnum::ITrAdtEnum(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::AdtEnum, attribs, genDecl, qualName, isModDef, startIdx, endIdx)
	{
	}

	ITrAdtEnumMember::ITrAdtEnumMember(QualNameSPtr parent, const StdString& iden, ITrTypeSPtr type, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::AdtEnumMember, nullptr, nullptr, parent->Append(iden), false, startIdx, endIdx)
		, type(type)
	{
	}

	ITrMarkerInterface::ITrMarkerInterface(ITrAttribsSPtr attribs, QualNameSPtr qualName, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::MarkerInterface, attribs, nullptr, qualName, true, startIdx, endIdx)
	{
	}

	ITrStrongInterface::ITrStrongInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdPairVector<QualNameSPtr, SpanId>&& implInterfaces, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::StrongInterface, attribs, genDecl, qualName, true, startIdx, endIdx)
		, implInterfaces(std::move(implInterfaces))
	{
	}

	ITrWeakInterface::ITrWeakInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::WeakInterface, attribs, genDecl, qualName, true, startIdx, endIdx)
	{
	}

	ITrTypealias::ITrTypealias(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Typealias, attribs, genDecl, qualName, isModDef, startIdx, endIdx)
		, type(type)
	{
	}

	ITrTypedef::ITrTypedef(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Typedef, attribs, genDecl, qualName, isModDef, startIdx, endIdx)
		, type(type)
	{
	}

	ITrVar::ITrVar(ITrAttribsSPtr attribs, QualNameSPtr qualName, ITrTypeSPtr type, ITrExprSPtr init, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Var, attribs, nullptr, qualName, isModDef, startIdx, endIdx)
		, type(type)
		, init(init)
	{
	}

	ITrFunc::ITrFunc(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdVector<ITrParamSPtr>&& params, ITrTypeSPtr errorType, ITrTypeSPtr retType, ITrFuncKind funcKind, bool isUnsafe, bool isModDef, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Func, attribs, genDecl, qualName, isModDef, startIdx, endIdx)
		, funcKind(funcKind)
		, params(std::move(params))
		, errorType(errorType)
		, retType(retType)
		, ctx(new FuncContext{})
		, isUnsafe(isUnsafe)
	{
	}

	ITrImpl::ITrImpl(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr scope, ITrTypeSPtr type, StdPair<QualNameSPtr, SpanId> interface, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::Impl, attribs, genDecl, scope, true, startIdx, endIdx)
		, type(type)
		, interface(interface)
	{
	}

	ITrErrHandler::ITrErrHandler(QualNameSPtr qualName, StdString errIden, ITrTypeSPtr errType, u64 startIdx, u64 endIdx)
		: ITrDef(ITrDefKind::ErrHandler, nullptr, nullptr, qualName, false, startIdx, endIdx)
		, errIden(errIden)
		, errType(errType)
	{
	}

	ITrStmt::ITrStmt(ITrStmtKind kind, u64 startIdx, u64 endIdx)
		: stmtKind(kind)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrStmt::~ITrStmt()
	{
	}

	ITrBlock::ITrBlock(const StdString& scopeName, StdVector<ITrStmtSPtr>&& stmts, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Block, startIdx, endIdx)
		, stmts(std::move(stmts))
		, scopeName(scopeName)
	{
	}

	ITrIf::ITrIf(bool isComptime, ITrLocalVarSPtr decl, ITrExprSPtr cond, ITrBlockSPtr tBlock, ITrBlockSPtr fBlock, u64 startIdx)
		: ITrStmt(ITrStmtKind::If, startIdx, fBlock ? fBlock->endIdx : tBlock->endIdx)
		, isComptime(isComptime)
		, decl(decl)
		, cond(cond)
		, tBlock(tBlock)
		, fBlock(fBlock)
	{
	}

	ITrLoop::ITrLoop(const StdString& label, ITrBlockSPtr block, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Loop, startIdx, endIdx)
		, label(label)
		, block(block)
	{
	}

	ITrForRange::ITrForRange(const StdString& scopeName, const StdString& label, const StdVector<StdString>& idens,
		ITrExprSPtr range, ITrBlockSPtr body, u64 startIdx)
		: ITrStmt(ITrStmtKind::ForRange, startIdx, body->endIdx)
		, label(label)
		, idens(idens)
		, range(range)
		, body(body)
		, scopeName(scopeName)
	{
	}

	ITrSwitchCase::ITrSwitchCase()
	{
	}

	ITrSwitchCase::ITrSwitchCase(ITrPatternSPtr pattern, ITrExprSPtr expr, ITrBlockSPtr block)
		: pattern(pattern)
		, expr(expr)
		, block(block)
	{
	}

	ITrSwitch::ITrSwitch(const StdString& scopeName, const StdString& label, ITrExprSPtr expr, StdVector<ITrSwitchCase>&& cases, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Switch, startIdx, endIdx)
		, label(label)
		, expr(expr)
		, cases(std::move(cases))
		, scopeName(scopeName)
		, baseGroup(ITrSwitchGroupKind::Base, 0, usize(-1))
	{
	}

	ITrLabel::ITrLabel(const StdString& label, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Label, startIdx, endIdx)
		, label(label)
	{
	}

	ITrBreak::ITrBreak(const StdString& label, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Break, startIdx, endIdx)
		, label(label)
	{
	}

	ITrContinue::ITrContinue(const StdString& label, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Continue, startIdx, endIdx)
		, label(label)
	{
	}

	ITrFallthrough::ITrFallthrough(u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Fallthrough, startIdx, endIdx)
	{
	}

	ITrGoto::ITrGoto(const StdString& label, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Goto, startIdx, endIdx)
		, label(label)
	{
	}

	ITrReturn::ITrReturn(ITrExprSPtr expr, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Return, startIdx, endIdx)
		, expr(expr)
	{
	}

	ITrThrow::ITrThrow(ITrExprSPtr expr, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Throw, startIdx, endIdx)
		, expr(expr)
	{
	}

	ITrDefer::ITrDefer(bool isErr, ITrExprSPtr block, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Defer, startIdx, endIdx)
		, isErr(isErr)
		, block(block)
	{
	}

	ITrUnsafe::ITrUnsafe(ITrBlockSPtr block, u64 startIdx)
		: ITrStmt(ITrStmtKind::Unsafe, startIdx, block->endIdx)
		, block(block)
	{
	}

	ITrCompCond::ITrCompCond(bool isDebug, const StdString& iden, OperatorKind op, u64 cmpVal, ITrBlockSPtr tBlock,
		ITrBlockSPtr fBlock, u64 startIdx)
		: ITrStmt(ITrStmtKind::CompCond, startIdx, fBlock ? fBlock->endIdx : tBlock->endIdx)
		, isDebug(isDebug)
		, op(op)
		, iden(iden)
		, cmpVal(cmpVal)
		, tBlock(tBlock)
		, fBlock(fBlock)
	{
	}

	ITrLocalVar::ITrLocalVar(ITrAttribsSPtr attribs, const StdVector<StdString>& idens, ITrTypeSPtr type, ITrExprSPtr init, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::LocalDecl, startIdx, endIdx)
		, attribs(attribs)
		, idens(idens)
		, type(type)
		, init(init)
	{
	}

	ITrExpr::ITrExpr(ITrExprKind kind, u64 startIdx, u64 endIdx)
		: ITrStmt(ITrStmtKind::Expr, startIdx, endIdx)
		, exprKind(kind)
	{
	}

	ITrExpr::~ITrExpr()
	{
	}

	ITrAssign::ITrAssign(OperatorKind op, ITrExprSPtr lExpr, ITrExprSPtr rExpr)
		: ITrExpr(ITrExprKind::Assign, lExpr->startIdx, rExpr->endIdx)
		, op(op)
		, lExpr(lExpr)
		, rExpr(rExpr)
	{
	}

	ITrTernary::ITrTernary(ITrExprSPtr cond, ITrExprSPtr tExpr, ITrExprSPtr fExpr)
		: ITrExpr(ITrExprKind::Ternary, cond->startIdx, fExpr->endIdx)
		, cond(cond)
		, tExpr(tExpr)
		, fExpr(fExpr)
	{
	}

	ITrBinary::ITrBinary(OperatorKind op, ITrExprSPtr lExpr, ITrExprSPtr rExpr)
		: ITrExpr(ITrExprKind::Binary, lExpr->startIdx, rExpr->endIdx)
		, op(op)
		, lExpr(lExpr)
		, rExpr(rExpr)
	{
	}

	ITrUnary::ITrUnary(OperatorKind op, ITrExprSPtr expr, u64 startIdx, u64 endIdx)
		: ITrExpr(ITrExprKind::Unary, startIdx, endIdx)
		, op(op)
		, expr(expr)
	{
	}

	ITrQualNameExpr::ITrQualNameExpr(ITrQualNameSPtr qualName)
		: ITrExpr(ITrExprKind::QualName, qualName->startIdx, qualName->endIdx)
		, qualName(qualName->qualName)
		, itrQualName(qualName)
	{
	}

	ITrIndexSlice::ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr index, u64 endIdx)
		: ITrExpr(ITrExprKind::IndexSlice, expr->startIdx, endIdx)
		, explicitSlice(false)
		, expr(expr)
		, index(index)
	{
	}

	ITrIndexSlice::ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr from, ITrExprSPtr to, u64 endIdx)
		: ITrExpr(ITrExprKind::IndexSlice, expr->startIdx, endIdx)
		, explicitSlice(true)
		, expr(expr)
		, index(from)
		, to(to)
	{
	}

	ITrAmbiguousCall::ITrAmbiguousCall(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args, u64 endIdx)
		: ITrExpr(ITrExprKind::AmbiguousCall, expr->startIdx, endIdx)
		, expr(expr)
		, args(std::move(args))
	{
	}

	ITrAdtTupleEnumInit::ITrAdtTupleEnumInit(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args, u64 endIdx)
		: ITrExpr(ITrExprKind::AdtTupleEnumInit, expr->startIdx, endIdx)
		, expr(expr)
		, args(std::move(args))
	{
	}

	ITrFuncCall::ITrFuncCall(ITrExprSPtr func, StdVector<ITrArgSPtr>&& args, u64 endIdx)
		: ITrExpr(ITrExprKind::FuncOrMethodCall, func->startIdx, endIdx)
		, isMethod(false)
		, nullCoalesce(false)
		, callerOrFunc(func)
		, args(std::move(args))
	{
	}

	ITrFuncCall::ITrFuncCall(ITrExprSPtr caller, bool nullCoalesce, const StdString& iden, const StdVector<IdenGeneric>& generics, StdVector<ITrArgSPtr>&& args, u64 endIdx)
		: ITrExpr(ITrExprKind::FuncOrMethodCall, caller->startIdx, endIdx)
		, isMethod(true)
		, nullCoalesce(nullCoalesce)
		, callerOrFunc(caller)
		, iden(iden)
		, generics(std::move(generics))
		, args(std::move(args))
	{
	}

	ITrMemberAccess::ITrMemberAccess(bool nullCoalesce, ITrExprSPtr expr, const StdString& iden, u64 endIdx)
		: ITrExpr(ITrExprKind::MemberAccess, expr->startIdx, endIdx)
		, nullCoalesce(nullCoalesce)
		, expr(expr)
		, iden(iden)
	{
	}

	ITrTupleAccess::ITrTupleAccess(ITrExprSPtr expr, bool nullCoalesce, u16 index, u64 endIdx)
		: ITrExpr(ITrExprKind::TupleAccess, expr->startIdx, endIdx)
		, nullCoalesce(nullCoalesce)
		, index(index)
		, expr(expr)
	{
	}

	ITrLiteral::ITrLiteral(Token lit)
		: ITrExpr(ITrExprKind::Literal, lit.Idx(), lit.Idx())
		, lit(lit)
	{
	}

	ITrAmbiguousAggrInit::ITrAmbiguousAggrInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, bool hasDefInit, ITrExprSPtr defExpr, u64 endIdx)
		: ITrExpr(ITrExprKind::AmbiguousAggrInit, type->startIdx, endIdx)
		, type(type)
		, args(std::move(args))
		, defExpr(defExpr)
		, hasDefInit(hasDefInit)
	{
	}

	ITrStructInit::ITrStructInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, bool hasDefInit, ITrExprSPtr defExpr, u64 endIdx)
		: ITrExpr(ITrExprKind::StructInit, type->startIdx, endIdx)
		, type(type)
		, args(std::move(args))
		, defExpr(defExpr)
		, hasDefInit(hasDefInit)
	{
	}

	ITrUnionInit::ITrUnionInit(ITrTypeSPtr type, ITrArgSPtr arg, u64 endIdx)
		: ITrExpr(ITrExprKind::UnionInit, type->startIdx, endIdx)
		, type(type)
		, arg(arg)
	{
	}

	ITrAdtAggrEnumInit::ITrAdtAggrEnumInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args, u64 endIdx)
		: ITrExpr(ITrExprKind::AdtAggrEnumInit, type->startIdx, endIdx)
		, type(type)
		, args(std::move(args))
		, hasDefInit(false)
	{
	}

	ITrTupleInit::ITrTupleInit(StdVector<ITrExprSPtr>&& exprs, u64 startIdx, u64 endIdx)
		: ITrExpr(ITrExprKind::TupleInit, startIdx, endIdx)
		, exprs(std::move(exprs))
	{
	}

	ITrArrayInit::ITrArrayInit(StdVector<ITrExprSPtr>&& exprs, u64 startIdx, u64 endIdx)
		: ITrExpr(ITrExprKind::ArrayInit, startIdx, endIdx)
		, exprs(std::move(exprs))
	{
	}

	ITrCast::ITrCast(ITrCastKind castKind, ITrExprSPtr expr, ITrTypeSPtr type)
		: ITrExpr(ITrExprKind::CastOrTransmute, expr->startIdx, type->endIdx)
		, castKind(castKind)
		, expr(expr)
		, type(type)
		, castToTryCast(false)
	{
	}

	ITrBlockExpr::ITrBlockExpr(const StdString& scopeName, StdVector<ITrStmtSPtr> stmts, u64 startIdx, u64 endIdx)
		: ITrExpr(ITrExprKind::Block, startIdx, endIdx)
		, stmts(std::move(stmts))
		, scopeName(scopeName)
	{
	}

	ITrUnsafeExpr::ITrUnsafeExpr(ITrExprSPtr expr, u64 startIdx)
		: ITrExpr(ITrExprKind::Unsafe, startIdx, expr->endIdx)
		, expr(expr)
	{
	}

	ITrMove::ITrMove(ITrExprSPtr expr, u64 startIdx)
		: ITrExpr(ITrExprKind::Move, startIdx, expr->endIdx)
		, expr(expr)
	{
	}

	ITrComma::ITrComma(StdVector<ITrExprSPtr>&& exprs, u64 startIdx, u64 endIdx)
		: ITrExpr(ITrExprKind::Comma, startIdx, endIdx)
		, exprs(std::move(exprs))
	{
	}

	ITrClosure::ITrClosure(ITrDefSPtr def, u64 startIdx, u64 endIdx)
		: ITrExpr(ITrExprKind::Closure, startIdx, endIdx)
		, def(def)
	{
	}

	ITrIs::ITrIs(ITrExprSPtr expr, ITrTypeSPtr type)
		: ITrExpr(ITrExprKind::Is, expr->startIdx, type->endIdx)
		, expr(expr)
		, type(type)
	{
	}

	ITrTry::ITrTry(ITrTryKind kind, ITrExprSPtr expr, u64 startIdx)
		: ITrExpr(ITrExprKind::Try, startIdx, expr->endIdx)
		, kind(kind)
		, expr(expr)
	{
	}

	ITrSpecKw::ITrSpecKw(TokenType kw, u64 tokIdx)
		: ITrExpr(ITrExprKind::SpecKw, tokIdx, tokIdx)
		, kw(kw)
	{
	}

	ITrCompRun::ITrCompRun(ITrExprSPtr expr, u64 startIdx)
		: ITrExpr(ITrExprKind::CompRun, startIdx, expr->endIdx)
		, expr(expr)
	{
	}

	ITrType::ITrType(ITrAttribsSPtr attribs, TypeHandle handle, StdVector<ITrTypeSPtr>&& subTypes, ITrExprSPtr expr, u64 startIdx, u64 endIdx)
		: attribs(attribs)
		, subTypes(std::move(subTypes))
		, expr(expr)
		, handle(handle)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrPattern::ITrPattern(ITrPatternKind kind, u64 startIdx, u64 endIdx)
		: patternKind(kind)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrPlaceholderPattern::ITrPlaceholderPattern(bool isWildcard, u64 tokIdx)
		: ITrPattern(isWildcard ? ITrPatternKind::Wildcard : ITrPatternKind::Placeholder, tokIdx, tokIdx)
	{
	}

	ITrValueBindPattern::ITrValueBindPattern(const StdString& iden, ITrPatternSPtr subPattern, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::ValueBind, startIdx, endIdx)
		, iden(iden)
		, subPattern(subPattern)
	{
	}

	ITrLiteralPattern::ITrLiteralPattern(Token lit)
		: ITrPattern(ITrPatternKind::Literal, lit.Idx(), lit.Idx())
		, lit(lit)
	{
	}

	ITrRangePattern::ITrRangePattern(bool isInclusive, Token from, Token to)
		: ITrPattern(ITrPatternKind::Range, from.Idx(), to.Idx())
		, isInclusive(isInclusive)
		, from(std::move(from))
		, to(std::move(to))
	{
	}

	ITrTuplePattern::ITrTuplePattern(StdVector<ITrPatternSPtr>&& subPatterns, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::Tuple, startIdx, endIdx)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrValueEnumPattern::ITrValueEnumPattern(QualNameSPtr qualName, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::ValueEnum, startIdx, endIdx)
		, qualName(qualName)
	{
	}

	ITrAdtTupleEnumPattern::ITrAdtTupleEnumPattern(QualNameSPtr qualName, StdVector<ITrPatternSPtr>&& subPatterns, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::AdtTupleEnum, startIdx, endIdx)
		, qualName(qualName)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrAmbiguousAggrPattern::ITrAmbiguousAggrPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::AmbiguousAggr, startIdx, endIdx)
		, qualName(qualName)
		, args(std::move(args))
	{
	}

	ITrAggrPattern::ITrAggrPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::Aggr, startIdx, endIdx)
		, qualName(qualName)
		, args(std::move(args))
	{
	}

	ITrAdtAggrEnumPattern::ITrAdtAggrEnumPattern(QualNameSPtr qualName, StdPairVector<StdString, ITrPatternSPtr>&& args, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::AdtAggrEnum, startIdx, endIdx)
		, qualName(qualName)
		, args(std::move(args))
	{
	}

	ITrSlicePattern::ITrSlicePattern(StdVector<ITrPatternSPtr>&& subPatterns, u64 startIdx, u64 endIdx)
		: ITrPattern(ITrPatternKind::Slice, startIdx, endIdx)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrEitherPattern::ITrEitherPattern(StdVector<ITrPatternSPtr>&& subPatterns)
		: ITrPattern(ITrPatternKind::Either, 0, 0)
		, subPatterns(std::move(subPatterns))
	{
		startIdx = subPatterns.front()->startIdx;
		endIdx = subPatterns.back()->endIdx;
	}

	ITrTypePattern::ITrTypePattern(ITrTypeSPtr type, u64 startIdx)
		: ITrPattern(ITrPatternKind::Type, startIdx, type->endIdx)
		, type(type)
	{
	}

	ITrAttribs::ITrAttribs(Visibility vis, Attribute attribs, StdVector<ITrAtAttribSPtr>&& atAttribs, u64 startIdx, u64 endIdx)
		: vis(vis)
		, attribs(attribs)
		, atAttribs(std::move(atAttribs))
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrAtAttrib::ITrAtAttrib(bool isCompAttrib, const StdString& iden, StdVector<ITrArgSPtr>&& args, u64 startIdx, u64 endIdx)
		: isCompAttrib(isCompAttrib)
		, iden(iden)
		, args(std::move(args))
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrGenParam::ITrGenParam(bool isType, u64 startIdx, u64 endIdx)
		: isType(isType)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrGenTypeParam::ITrGenTypeParam(const StdString& name, ITrTypeSPtr defType, u64 startIdx, u64 endIdx)
		: ITrGenParam(true, startIdx, endIdx)
		, iden(name)
		, defType(defType)
	{
	}

	ITrGenValParam::ITrGenValParam(const StdString& iden, ITrTypeSPtr type, ITrExprSPtr defExpr, u64 startIdx, u64 endIdx)
		: ITrGenParam(false, startIdx, endIdx)
		, iden(iden)
		, type(type)
		, defExpr(defExpr)
	{
	}

	ITrGenAssocBound::ITrGenAssocBound(const StdString& iden, ITrGenBoundTypeSPtr type, u64 startIdx, u64 endIdx)
		: iden(iden)
		, type(type)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrGenBoundType::ITrGenBoundType(ITrTypeSPtr type, StdVector<ITrGenAssocBound>&& assocBounds, u64 startIdx, u64 endIdx)
		: type(type)
		, assocBounds(std::move(assocBounds))
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrGenTypeBound::ITrGenTypeBound(ITrTypeSPtr type, ITrGenBoundTypeSPtr bound, u64 startIdx, u64 endIdx)
		: type(type)
		, bound(bound)
		, startIdx(startIdx)
		, endIdx(endIdx)
	{
	}

	ITrBody::ITrBody(StdVector<ITrDefSPtr>&& defs, StdVector<ITrStmtSPtr>&& stmts)
		: defs(std::move(defs))
		, stmts(std::move(stmts))
	{
	}

	void ITrModule::AddDefinition(ITrDefSPtr def)
	{
		defMapping[u8(def->kind)].push_back(def);
	}

	void ITrModule::AddDefinition(ITrDefSPtr def, ITrBodySPtr body)
	{
		defMapping[u8(def->kind)].push_back(def);
		def->bodyIdx = u64(bodies.size());
		bodies.push_back(body);
	}

	u64 ITrModule::AddBody(ITrBodySPtr body)
	{
		u64 tmp = u64(bodies.size());
		bodies.push_back(body);
		return tmp;
	}

	ITrBodySPtr ITrModule::GetBody(ITrDef& def)
	{
		return GetBody(def.bodyIdx);
	}

	ITrBodySPtr ITrModule::GetBody(u64 idx)
	{
		if (idx >= bodies.size())
			return nullptr;
		return bodies[idx];
	}
}
