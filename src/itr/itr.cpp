#include "itr.hpp"

#include "common/qualname.hpp"

namespace Noctis
{
	ITrArg::ITrArg(IdenSPtr iden, ITrExprSPtr expr)
		: iden(iden)
		, expr(expr)
	{
	}

	ITrIden::ITrIden(IdenSPtr iden, StdPairVector<ITrTypeSPtr, ITrExprSPtr>&& assocArgs)
		: iden(iden)
		, assocArgs(std::move(assocArgs))
	{
	}

	ITrTypeDisambiguation::ITrTypeDisambiguation(TypeDisambiguationSPtr disambiguation, ITrTypeSPtr assocType, ITrQualNameSPtr assocQualName)
		: disambiguation(disambiguation)
		, assocType(assocType)
		, assocQualName(assocQualName)
	{
	}

	ITrQualName::ITrQualName(QualNameSPtr qualName, ITrTypeDisambiguationSPtr assocDisambiguation, StdVector<ITrIdenSPtr>&& assocIdens)
		: qualName(qualName)
		, assocDisambiguation(assocDisambiguation)
		, assocIdens(std::move(assocIdens))
	{
	}

	ITrParam::ITrParam(ITrAttribsSPtr attribs, IdenSPtr iden, ITrTypeSPtr type)
		: attribs(attribs)
		, iden(iden)
		, type(type)
	{
	}

	ITrDef::ITrDef(ITrDefKind kind, ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef)
		: kind(kind)
		, attribs(attribs)
		, qualName(qualName)
		, genDecl(genDecl)
		, bodyIdx(u64(-1))
		, isModDef(isModDef)
	{
	}

	ITrDef::~ITrDef()
	{
	}

	ITrStruct::ITrStruct(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef)
		: ITrDef(ITrDefKind::Struct, attribs, genDecl, qualName, isModDef)
	{
	}

	ITrUnion::ITrUnion(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef)
		: ITrDef(ITrDefKind::Union, attribs, genDecl, qualName, isModDef)
	{
	}

	ITrValEnum::ITrValEnum(ITrAttribsSPtr attribs, QualNameSPtr qualName, bool isModDef)
		: ITrDef(ITrDefKind::ValEnum, attribs, nullptr, qualName, isModDef)
	{
	}

	ITrValEnumMember::ITrValEnumMember(QualNameSPtr parent, IdenSPtr iden, ITrExprSPtr val)
		: ITrDef(ITrDefKind::ValEnumMember, nullptr, nullptr, QualName::Create(parent, iden), false)
		, val(val)
	{
	}

	ITrAdtEnum::ITrAdtEnum(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, bool isModDef)
		: ITrDef(ITrDefKind::AdtEnum, attribs, genDecl, qualName, isModDef)
	{
	}

	ITrAdtEnumMember::ITrAdtEnumMember(QualNameSPtr parent, IdenSPtr iden, ITrTypeSPtr type)
		: ITrDef(ITrDefKind::AdtEnumMember, nullptr, nullptr, QualName::Create(parent, iden), false)
		, type(type)
	{
	}

	ITrMarkerInterface::ITrMarkerInterface(ITrAttribsSPtr attribs, QualNameSPtr qualName)
		: ITrDef(ITrDefKind::MarkerInterface, attribs, nullptr, qualName, true)
	{
	}

	ITrStrongInterface::ITrStrongInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName)
		: ITrDef(ITrDefKind::StrongInterface, attribs, genDecl, qualName, true)
	{
	}

	ITrWeakInterface::ITrWeakInterface(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName)
		: ITrDef(ITrDefKind::WeakInterface, attribs, genDecl, qualName, true)
	{
	}

	ITrTypealias::ITrTypealias(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef)
		: ITrDef(ITrDefKind::Typealias, attribs, genDecl, qualName, isModDef)
		, type(type)
	{
	}

	ITrTypedef::ITrTypedef(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef)
		: ITrDef(ITrDefKind::Typedef, attribs, genDecl, qualName, isModDef)
		, type(type)
	{
	}

	ITrVar::ITrVar(ITrAttribsSPtr attribs, QualNameSPtr qualName, ITrTypeSPtr type, bool isModDef)
		: ITrDef(ITrDefKind::Var, attribs, nullptr, qualName, isModDef)
		, type(type)
	{
	}

	ITrFunc::ITrFunc(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr qualName, StdVector<ITrParamSPtr>&& params, ITrTypeSPtr retType, ITrFuncKind funcKind, bool isModDef)
		: ITrDef(ITrDefKind::Func, attribs, genDecl, qualName, isModDef)
		, funcKind(funcKind)
		, params(std::move(params))
		, retType(retType)
	{
	}

	ITrImpl::ITrImpl(ITrAttribsSPtr attribs, ITrGenDeclSPtr genDecl, QualNameSPtr scope, ITrTypeSPtr type, StdPairVector<QualNameSPtr, SpanId>&& interfaces)
		: ITrDef(ITrDefKind::Impl, attribs, genDecl, scope, true)
		, type(type)
		, interfaces(std::move(interfaces))
	{
	}

	ITrStmt::ITrStmt(ITrStmtKind kind)
		: stmtKind(kind)
	{
	}

	ITrStmt::~ITrStmt()
	{
	}

	ITrBlock::ITrBlock(const StdString& scopeName, StdVector<ITrStmtSPtr>&& stmts)
		: ITrStmt(ITrStmtKind::Block)
		, stmts(std::move(stmts))
		, scopeName(scopeName)
	{
	}

	ITrIf::ITrIf(bool isComptime, ITrLocalVarSPtr decl, ITrExprSPtr cond, ITrBlockSPtr tBlock, ITrBlockSPtr fBlock)
		: ITrStmt(ITrStmtKind::If)
		, isComptime(isComptime)
		, decl(decl)
		, cond(cond)
		, tBlock(tBlock)
		, fBlock(fBlock)
	{
	}

	ITrLoop::ITrLoop(const StdString& scopeName, IdenSPtr label, StdVector<ITrStmtSPtr>&& stmts)
		: ITrStmt(ITrStmtKind::Loop)
		, label(label)
		, stmts(stmts)
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

	ITrSwitch::ITrSwitch(const StdString& scopeName, IdenSPtr label, ITrExprSPtr expr, StdVector<ITrSwitchCase>&& cases)
		: ITrStmt(ITrStmtKind::Switch)
		, label(label)
		, expr(expr)
		, cases(std::move(cases))
		, scopeName(scopeName)
	{
	}

	ITrLabel::ITrLabel(IdenSPtr label)
		: ITrStmt(ITrStmtKind::Label)
		, label(label)
	{
	}

	ITrBreak::ITrBreak(IdenSPtr label)
		: ITrStmt(ITrStmtKind::Break)
		, label(label)
	{
	}

	ITrContinue::ITrContinue(IdenSPtr label)
		: ITrStmt(ITrStmtKind::Continue)
		, label(label)
	{
	}

	ITrFallthrough::ITrFallthrough()
		: ITrStmt(ITrStmtKind::Fallthrough)
	{
	}

	ITrGoto::ITrGoto(IdenSPtr label)
		: ITrStmt(ITrStmtKind::Goto)
		, label(label)
	{
	}

	ITrReturn::ITrReturn(ITrExprSPtr expr)
		: ITrStmt(ITrStmtKind::Return)
		, expr(expr)
	{
	}

	ITrThrow::ITrThrow(ITrExprSPtr expr)
		: ITrStmt(ITrStmtKind::Throw)
		, expr(expr)
	{
	}

	ITrDefer::ITrDefer(bool isErr, ITrExprSPtr block)
		: ITrStmt(ITrStmtKind::Defer)
		, isErr(isErr)
		, block(block)
	{
	}

	ITrUnsafe::ITrUnsafe(ITrBlockSPtr block)
		: ITrStmt(ITrStmtKind::Unsafe)
		, block(block)
	{
	}

	ITrErrHandler::ITrErrHandler(StdVector<ITrStmtSPtr>&& stmts)
		: ITrStmt(ITrStmtKind::ErrorHandler)
		, stmts(std::move(stmts))
	{
	}

	ITrCompCond::ITrCompCond(bool isDebug, IdenSPtr iden, OperatorKind op, u64 cmpVal, ITrBlockSPtr tBlock,
		ITrBlockSPtr fBlock)
		: ITrStmt(ITrStmtKind::CompCond)
		, isDebug(isDebug)
		, op(op)
		, iden(iden)
		, cmpVal(cmpVal)
		, tBlock(tBlock)
		, fBlock(fBlock)
	{
	}

	ITrLocalVar::ITrLocalVar(ITrAttribsSPtr attribs, StdVector<IdenSPtr>&& idens, ITrTypeSPtr type, ITrExprSPtr init)
		: ITrStmt(ITrStmtKind::LocalDecl)
		, attribs(attribs)
		, idens(std::move(idens))
		, type(type)
		, init(init)
	{
	}

	ITrExpr::ITrExpr(ITrExprKind kind)
		: ITrStmt(ITrStmtKind::Expr) 
		, exprKind(kind)
		, typeHandle(TypeHandle(-1))
	{
	}

	ITrExpr::~ITrExpr()
	{
	}

	ITrAssign::ITrAssign(OperatorKind op, ITrExprSPtr lExpr, ITrExprSPtr rExpr)
		: ITrExpr(ITrExprKind::Assign)
		, op(op)
		, lExpr(lExpr)
		, rExpr(rExpr)
	{
	}

	ITrTernary::ITrTernary(ITrExprSPtr cond, ITrExprSPtr tExpr, ITrExprSPtr fExpr)
		: ITrExpr(ITrExprKind::Ternary)
		, cond(cond)
		, tExpr(tExpr)
		, fExpr(fExpr)
	{
	}

	ITrBinary::ITrBinary(OperatorKind op, ITrExprSPtr lExpr, ITrExprSPtr rExpr)
		: ITrExpr(ITrExprKind::Binary)
		, op(op)
		, lExpr(lExpr)
		, rExpr(rExpr)
	{
	}

	ITrUnary::ITrUnary(OperatorKind op, ITrExprSPtr expr)
		: ITrExpr(ITrExprKind::Unary)
		, op(op)
		, expr(expr)
	{
	}

	ITrQualNameExpr::ITrQualNameExpr(ITrQualNameSPtr qualName)
		: ITrExpr(ITrExprKind::QualName)
		, qualName(qualName->qualName)
		, itrQualName(qualName)
	{
	}

	ITrIndexSlice::ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr index)
		: ITrExpr(ITrExprKind::IndexSlice)
		, explicitSlice(false)
		, expr(expr)
		, index(index)
	{
	}

	ITrIndexSlice::ITrIndexSlice(ITrExprSPtr expr, ITrExprSPtr from, ITrExprSPtr to)
		: ITrExpr(ITrExprKind::IndexSlice)
		, explicitSlice(true)
		, expr(expr)
		, index(from)
		, to(to)
	{
	}

	ITrAmbiguousCall::ITrAmbiguousCall(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::AmbiguousCall)
		, expr(expr)
		, args(std::move(args))
	{
	}

	ITrAdtTupleEnumInit::ITrAdtTupleEnumInit(ITrExprSPtr expr, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::AdtTupleEnumInit)
		, expr(expr)
		, args(std::move(args))
	{
	}

	ITrMemberAccess::ITrMemberAccess(bool nullCoalesce, ITrExprSPtr expr, IdenSPtr iden)
		: ITrExpr(ITrExprKind::MemberAccess)
		, nullCoalesce(nullCoalesce)
		, expr(expr)
		, iden(iden)
	{
	}

	ITrFuncCall::ITrFuncCall(ITrExprSPtr func, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::FuncOrMethodCall)
		, isMethod(false)
		, nullCoalesce(false)
		, callerOrFunc(func)
		, args(std::move(args))
	{
	}

	ITrFuncCall::ITrFuncCall(ITrExprSPtr caller, bool nullCoalesce, IdenSPtr iden, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::FuncOrMethodCall)
		, isMethod(true)
		, nullCoalesce(true)
		, callerOrFunc(caller)
		, iden(iden)
		, args(std::move(args))
	{
	}

	ITrTupleAccess::ITrTupleAccess(ITrExprSPtr expr, bool nullCoalesce, u16 index)
		: ITrExpr(ITrExprKind::TupleAccess)
		, nullCoalesce(nullCoalesce)
		, index(index)
		, expr(expr)
	{
	}

	ITrLiteral::ITrLiteral(Token lit)
		: ITrExpr(ITrExprKind::Literal)
		, lit(lit)
	{
	}

	ITrAmbiguousAggrInit::ITrAmbiguousAggrInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::AmbiguousAggrInit)
		, type(type)
		, args(std::move(args))
	{
	}

	ITrAggrInit::ITrAggrInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::AggrInit)
		, type(type)
		, args(std::move(args))
	{
	}

	ITrAdtAggrEnumInit::ITrAdtAggrEnumInit(ITrTypeSPtr type, StdVector<ITrArgSPtr>&& args)
		: ITrExpr(ITrExprKind::AdtAggrEnumInit)
		, type(type)
		, args(std::move(args))
	{
	}

	ITrTupleInit::ITrTupleInit(StdVector<ITrExprSPtr>&& exprs)
		: ITrExpr(ITrExprKind::TupleInit)
		, exprs(std::move(exprs))
	{
	}

	ITrArrayInit::ITrArrayInit(StdVector<ITrExprSPtr>&& exprs)
		: ITrExpr(ITrExprKind::ArrayInit)
		, exprs(std::move(exprs))
	{
	}

	ITrCast::ITrCast(bool isTransmute, ITrTypeSPtr type, ITrExprSPtr expr)
		: ITrExpr(ITrExprKind::CastOrTransmute)
		, isTransmute(isTransmute)
		, type(type)
		, expr(expr)
	{
	}

	ITrBlockExpr::ITrBlockExpr(const StdString& scopeName, StdVector<ITrStmtSPtr> stmts)
		: ITrExpr(ITrExprKind::Block)
		, stmts(std::move(stmts))
		, scopeName(scopeName)
	{
	}

	ITrUnsafeExpr::ITrUnsafeExpr(ITrExprSPtr expr)
		: ITrExpr(ITrExprKind::Unsafe)
		, expr(expr)
	{
	}

	ITrMove::ITrMove(ITrExprSPtr expr)
		: ITrExpr(ITrExprKind::Move)
		, expr(expr)
	{
	}

	ITrComma::ITrComma(StdVector<ITrExprSPtr>&& exprs)
		: ITrExpr(ITrExprKind::Comma)
		, exprs(std::move(exprs))
	{
	}

	ITrClosure::ITrClosure(ITrDefSPtr def)
		: ITrExpr(ITrExprKind::Closure)
		, def(def)
	{
	}

	ITrIs::ITrIs(ITrExprSPtr expr, ITrTypeSPtr type)
		: ITrExpr(ITrExprKind::Is)
		, expr(expr)
		, type(type)
	{
	}

	ITrTry::ITrTry(ITrExprSPtr expr)
		: ITrExpr(ITrExprKind::Try)
		, expr(expr)
	{
	}

	ITrSpecKw::ITrSpecKw(TokenType kw)
		: ITrExpr(ITrExprKind::SpecKw)
		, kw(kw)
	{
	}

	ITrCompRun::ITrCompRun(ITrExprSPtr expr)
		: ITrExpr(ITrExprKind::CompRun)
		, expr(expr)
	{
	}

	ITrType::ITrType(ITrAttribsSPtr attribs, TypeHandle handle, StdVector<ITrTypeSPtr>&& subTypes, ITrExprSPtr expr)
		: attribs(attribs)
		, subTypes(std::move(subTypes))
		, expr(expr)
		, handle(handle)
	{
	}

	ITrAttribs::ITrAttribs(Visibility vis, Attribute attribs, StdVector<ITrAtAttribSPtr>&& atAttribs)
		: vis(vis)
		, attribs(attribs)
		, atAttribs(std::move(atAttribs))
	{
	}

	ITrAtAttrib::ITrAtAttrib(bool isCompAttrib, IdenSPtr iden, StdVector<ITrArgSPtr>&& args)
		: isCompAttrib(isCompAttrib)
		, iden(iden)
		, args(std::move(args))
	{
	}

	ITrGenParam::ITrGenParam(bool isVar)
		: isVar(isVar)
	{
	}

	ITrGenTypeParam::ITrGenTypeParam(IdenSPtr name, ITrTypeSPtr defType)
		: ITrGenParam(false)
		, iden(name)
		, defType(defType)
	{
	}

	ITrGenValParam::ITrGenValParam(IdenSPtr iden, ITrTypeSPtr type, ITrExprSPtr defExpr)
		: ITrGenParam(true)
		, iden(iden)
		, type(type)
		, defExpr(defExpr)
	{
	}

	ITrGenBound::ITrGenBound(ITrTypeSPtr type, ITrTypeSPtr bound)
		: type(type)
		, bound(bound)
	{
	}

	ITrPattern::ITrPattern(ITrPatternKind kind)
		: patternKind(kind)
	{
	}

	ITrPlaceholderPattern::ITrPlaceholderPattern(bool isWildcard)
		: ITrPattern(ITrPatternKind::Placeholder)
		, isWildcard(isWildcard)
	{
	}

	ITrAmbiguousIdenPattern::ITrAmbiguousIdenPattern(IdenSPtr iden)
		: ITrPattern(ITrPatternKind::AmbiguousIden)
		, iden(iden)
	{
	}

	ITrValueBindPattern::ITrValueBindPattern(IdenSPtr iden, ITrPatternSPtr subPattern)
		: ITrPattern(ITrPatternKind::ValueBind)
		, iden(iden)
		, subPattern(subPattern)
	{
	}

	ITrLiteralPattern::ITrLiteralPattern(Token lit)
		: ITrPattern(ITrPatternKind::Literal)
		, lit(lit)
	{
	}

	ITrRangePattern::ITrRangePattern(bool isInclusive, ITrPatternSPtr from, ITrPatternSPtr to)
		: ITrPattern(ITrPatternKind::Range)
		, isInclusive(isInclusive)
		, from(from)
		, to(to)
	{
	}

	ITrTuplePattern::ITrTuplePattern(StdVector<ITrPatternSPtr>&& subPatterns)
		: ITrPattern(ITrPatternKind::Tuple)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrValueEnumPattern::ITrValueEnumPattern(QualNameSPtr qualName)
		: ITrPattern(ITrPatternKind::ValueEnum)
		, qualName(qualName)
	{
	}

	ITrAdtTupleEnumPattern::ITrAdtTupleEnumPattern(QualNameSPtr qualName, StdVector<ITrPatternSPtr>&& subPatterns)
		: ITrPattern(ITrPatternKind::AdtTupleEnum)
		, qualName(qualName)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrAmbiguousAggrPattern::ITrAmbiguousAggrPattern(QualNameSPtr qualName,	StdPairVector<IdenSPtr, ITrPatternSPtr>&& args)
		: ITrPattern(ITrPatternKind::AmbiguousAggr)
		, qualName(qualName)
		, args(std::move(args))
	{
	}

	ITrAggrPattern::ITrAggrPattern(QualNameSPtr qualName, StdPairVector<IdenSPtr, ITrPatternSPtr>&& args)
		: ITrPattern(ITrPatternKind::Aggr)
		, qualName(qualName)
		, args(std::move(args))
	{
	}

	ITrAdtAggrEnumPattern::ITrAdtAggrEnumPattern(QualNameSPtr qualName, StdPairVector<IdenSPtr, ITrPatternSPtr>&& args)
		: ITrPattern(ITrPatternKind::AdtAggrEnum)
		, qualName(qualName)
		, args(std::move(args))
	{
	}

	ITrSlicePattern::ITrSlicePattern(StdVector<ITrPatternSPtr>&& subPatterns)
		: ITrPattern(ITrPatternKind::Slice)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrEitherPattern::ITrEitherPattern(StdVector<ITrPatternSPtr>&& subPatterns)
		: ITrPattern(ITrPatternKind::Either)
		, subPatterns(std::move(subPatterns))
	{
	}

	ITrTypePattern::ITrTypePattern(ITrTypeSPtr type)
		: ITrPattern(ITrPatternKind::Type)
		, type(type)
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

	ITrBodySPtr ITrModule::GetBody(ITrDef& def)
	{
		u64 idx = def.bodyIdx;
		if (idx >= bodies.size())
			return nullptr;
		return bodies[idx];
	}
}
