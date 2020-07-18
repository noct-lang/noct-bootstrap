#include "itr-visitor.hpp"

#include "itr.hpp"

namespace Noctis
{
	ITrVisitor::ITrVisitor(bool walkDefs)
		: m_VisitDefs(walkDefs)
		, m_pMod(nullptr)
	{
	}

	ITrVisitor::~ITrVisitor()
	{
	}

	
#define FOREACH_IMPL(Type) \
	void ITrVisitor::Foreach(ITrVisitorDefKind kind, const std::function<void(ITr##Type&)>& func) \
	{ \
		for (ITrDefSPtr def : m_pMod->defMapping[u8(ITrDefKind::Type)]) \
		{ \
			switch (kind) \
			{ \
			case ITrVisitorDefKind::Module: \
			{ \
				if (def->isModDef) \
					func(*reinterpret_cast<ITr##Type*>(def.get())); \
				break; \
			} \
			case ITrVisitorDefKind::Local: \
			{ \
				if (!def->isModDef) \
					func(*reinterpret_cast<ITr##Type*>(def.get())); \
				break; \
			} \
			default: \
			{ \
				func(*reinterpret_cast<ITr##Type*>(def.get())); \
				break; \
			} \
			} \
		} \
	}

	FOREACH_IMPL(Struct)
	FOREACH_IMPL(Union)
	FOREACH_IMPL(ValEnum)
	FOREACH_IMPL(ValEnumMember)
	FOREACH_IMPL(AdtEnum)
	FOREACH_IMPL(AdtEnumMember)
	FOREACH_IMPL(MarkerInterface)
	FOREACH_IMPL(WeakInterface)
	FOREACH_IMPL(StrongInterface)
	FOREACH_IMPL(Typealias)
	FOREACH_IMPL(Typedef)
	FOREACH_IMPL(Var)
	FOREACH_IMPL(Func)
	FOREACH_IMPL(Impl)
	
	void ITrVisitor::Visit(ITrStruct& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrUnion& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrValEnum& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrValEnumMember& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAdtEnum& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAdtEnumMember& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrMarkerInterface& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrStrongInterface& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrWeakInterface& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTypealias& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTypedef& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrVar& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrFunc& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrImpl& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrBlock& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrIf& node)
	{	
		Walk(node);
	}

	void ITrVisitor::Visit(ITrLoop& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrSwitch& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrLabel& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrBreak& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrContinue& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrFallthrough& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrGoto& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrReturn& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrThrow& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrDefer& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrUnsafe& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrErrHandler& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrCompCond& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrLocalVar& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAssign& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTernary& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrBinary& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrUnary& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrQualNameExpr& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrIndexSlice& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrExprSPtr& ptr, ITrAmbiguousCall node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrFuncCall& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAdtTupleEnumInit& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrMemberAccess& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTupleAccess& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrLiteral& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrStructInit& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrUnionInit& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAdtAggrEnumInit& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTupleInit& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrArrayInit& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrCast& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrBlockExpr& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrUnsafeExpr& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrComma& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrClosure& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrMove& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrIs& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTry& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrSpecKw& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrCompRun& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrType& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrPlaceholderPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrGenDecl& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrGenTypeParam& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrGenValParam& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrGenTypeBound& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrPatternSPtr& ptr, ITrAmbiguousIdenPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrValueBindPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrLiteralPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrRangePattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTuplePattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrValueEnumPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAdtTupleEnumPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAggrPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAdtAggrEnumPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrSlicePattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrEitherPattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrTypePattern& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAttribs& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrAtAttrib& node)
	{
		Walk(node);
	}

	void ITrVisitor::Visit(ITrDefSPtr& def)
	{
		if (!m_VisitDefs)
			return;
		
		switch (def->kind)
		{
		case ITrDefKind::Struct: Visit(*reinterpret_cast<ITrStruct*>(def.get())); break;
		case ITrDefKind::Union: Visit(*reinterpret_cast<ITrUnion*>(def.get())); break;
		case ITrDefKind::ValEnum: Visit(*reinterpret_cast<ITrValEnum*>(def.get())); break;
		case ITrDefKind::ValEnumMember: Visit(*reinterpret_cast<ITrValEnumMember*>(def.get())); break;
		case ITrDefKind::AdtEnum: Visit(*reinterpret_cast<ITrAdtEnum*>(def.get())); break;
		case ITrDefKind::AdtEnumMember: Visit(*reinterpret_cast<ITrAdtEnumMember*>(def.get())); break;
		case ITrDefKind::MarkerInterface: Visit(*reinterpret_cast<ITrMarkerInterface*>(def.get())); break;
		case ITrDefKind::WeakInterface: Visit(*reinterpret_cast<ITrWeakInterface*>(def.get())); break;
		case ITrDefKind::StrongInterface: Visit(*reinterpret_cast<ITrStrongInterface*>(def.get())); break;
		case ITrDefKind::Typealias: Visit(*reinterpret_cast<ITrTypealias*>(def.get())); break;
		case ITrDefKind::Typedef: Visit(*reinterpret_cast<ITrTypedef*>(def.get())); break;
		case ITrDefKind::Var: Visit(*reinterpret_cast<ITrVar*>(def.get())); break;
		case ITrDefKind::Func: Visit(*reinterpret_cast<ITrFunc*>(def.get())); break;
		case ITrDefKind::Impl: Visit(*reinterpret_cast<ITrImpl*>(def.get())); break;
		default: ;
		}
	}

	void ITrVisitor::Visit(ITrStmtSPtr& stmt)
	{
		switch (stmt->stmtKind)
		{
		case ITrStmtKind::Block: Visit(*reinterpret_cast<ITrBlock*>(stmt.get())); break;
		case ITrStmtKind::If: Visit(*reinterpret_cast<ITrIf*>(stmt.get())); break;
		case ITrStmtKind::Loop: Visit(*reinterpret_cast<ITrLoop*>(stmt.get())); break;
		case ITrStmtKind::Switch: Visit(*reinterpret_cast<ITrSwitch*>(stmt.get())); break;
		case ITrStmtKind::Label: Visit(*reinterpret_cast<ITrLabel*>(stmt.get())); break;
		case ITrStmtKind::Break: Visit(*reinterpret_cast<ITrBreak*>(stmt.get())); break;
		case ITrStmtKind::Continue: Visit(*reinterpret_cast<ITrContinue*>(stmt.get())); break;
		case ITrStmtKind::Fallthrough: Visit(*reinterpret_cast<ITrFallthrough*>(stmt.get())); break;
		case ITrStmtKind::Goto: Visit(*reinterpret_cast<ITrGoto*>(stmt.get())); break;
		case ITrStmtKind::Return: Visit(*reinterpret_cast<ITrReturn*>(stmt.get())); break;
		case ITrStmtKind::Throw: Visit(*reinterpret_cast<ITrThrow*>(stmt.get())); break;
		case ITrStmtKind::Expr: Visit(*reinterpret_cast<ITrExprSPtr*>(&stmt)); break;
		case ITrStmtKind::Defer: Visit(*reinterpret_cast<ITrDefer*>(stmt.get())); break;
		case ITrStmtKind::Unsafe: Visit(*reinterpret_cast<ITrUnsafe*>(stmt.get())); break;
		case ITrStmtKind::ErrorHandler: Visit(*reinterpret_cast<ITrErrHandler*>(stmt.get())); break;
		case ITrStmtKind::CompCond: Visit(*reinterpret_cast<ITrCompCond*>(stmt.get())); break;
		case ITrStmtKind::LocalDecl: Visit(*reinterpret_cast<ITrLocalVar*>(stmt.get())); break;
		default: ;
		}
	}

	void ITrVisitor::Visit(ITrExprSPtr& expr)
	{
		switch (expr->exprKind)
		{
		case ITrExprKind::Assign: Visit(*reinterpret_cast<ITrAssign*>(expr.get())); break;
		case ITrExprKind::Ternary: Visit(*reinterpret_cast<ITrTernary*>(expr.get())); break;
		case ITrExprKind::Binary: Visit(*reinterpret_cast<ITrBinary*>(expr.get())); break;
		case ITrExprKind::Unary: Visit(*reinterpret_cast<ITrUnary*>(expr.get())); break;
		case ITrExprKind::QualName: Visit(*reinterpret_cast<ITrQualNameExpr*>(expr.get())); break;
		case ITrExprKind::IndexSlice: Visit(*reinterpret_cast<ITrIndexSlice*>(expr.get())); break;
		case ITrExprKind::AmbiguousCall: Visit(expr, *reinterpret_cast<ITrAmbiguousCall*>(expr.get())); break;
		case ITrExprKind::FuncOrMethodCall: Visit(*reinterpret_cast<ITrFuncCall*>(expr.get())); break;
		case ITrExprKind::AdtTupleEnumInit: Visit(*reinterpret_cast<ITrAdtTupleEnumInit*>(expr.get())); break;
		case ITrExprKind::MemberAccess: Visit(*reinterpret_cast<ITrMemberAccess*>(expr.get())); break;
		case ITrExprKind::TupleAccess: Visit(*reinterpret_cast<ITrTupleAccess*>(expr.get())); break;
		case ITrExprKind::Literal: Visit(*reinterpret_cast<ITrLiteral*>(expr.get())); break;
		case ITrExprKind::AmbiguousAggrInit: Visit(expr, *reinterpret_cast<ITrAmbiguousAggrInit*>(expr.get())); break;
		case ITrExprKind::StructInit: Visit(*reinterpret_cast<ITrStructInit*>(expr.get())); break;
		case ITrExprKind::UnionInit: Visit(*reinterpret_cast<ITrUnionInit*>(expr.get())); break;
		case ITrExprKind::TupleInit: Visit(*reinterpret_cast<ITrTupleInit*>(expr.get())); break;
		case ITrExprKind::ArrayInit: Visit(*reinterpret_cast<ITrArrayInit*>(expr.get())); break;
		case ITrExprKind::AdtAggrEnumInit: Visit(*reinterpret_cast<ITrAdtAggrEnumInit*>(expr.get())); break;
		case ITrExprKind::CastOrTransmute: Visit(*reinterpret_cast<ITrCast*>(expr.get())); break;
		case ITrExprKind::Move: Visit(*reinterpret_cast<ITrMove*>(expr.get())); break;
		case ITrExprKind::Block: Visit(*reinterpret_cast<ITrBlockExpr*>(expr.get())); break;
		case ITrExprKind::Unsafe: Visit(*reinterpret_cast<ITrUnsafeExpr*>(expr.get())); break;
		case ITrExprKind::Comma: Visit(*reinterpret_cast<ITrComma*>(expr.get())); break;
		case ITrExprKind::Closure: Visit(*reinterpret_cast<ITrClosure*>(expr.get())); break;
		case ITrExprKind::Is: Visit(*reinterpret_cast<ITrIs*>(expr.get())); break;
		case ITrExprKind::Try: Visit(*reinterpret_cast<ITrTry*>(expr.get())); break;
		case ITrExprKind::SpecKw: Visit(*reinterpret_cast<ITrSpecKw*>(expr.get())); break;
		case ITrExprKind::CompRun: Visit(*reinterpret_cast<ITrCompRun*>(expr.get())); break;
		default: ;
		}
	}

	void ITrVisitor::Visit(ITrPatternSPtr& pattern)
	{
		switch (pattern->patternKind)
		{
		case ITrPatternKind::Placeholder: Visit(*reinterpret_cast<ITrPlaceholderPattern*>(pattern.get())); break;
		case ITrPatternKind::ValueBind: Visit(*reinterpret_cast<ITrValueBindPattern*>(pattern.get())); break;
		case ITrPatternKind::Literal: Visit(*reinterpret_cast<ITrLiteralPattern*>(pattern.get())); break;
		case ITrPatternKind::Range: Visit(*reinterpret_cast<ITrRangePattern*>(pattern.get())); break;
		case ITrPatternKind::Tuple: Visit(*reinterpret_cast<ITrTuplePattern*>(pattern.get())); break;
		case ITrPatternKind::AmbiguousIden: Visit(pattern, *reinterpret_cast<ITrAmbiguousIdenPattern*>(pattern.get())); break;
		case ITrPatternKind::ValueEnum: Visit(*reinterpret_cast<ITrValueEnumPattern*>(pattern.get())); break;
		case ITrPatternKind::AdtTupleEnum: Visit(*reinterpret_cast<ITrAdtTupleEnumPattern*>(pattern.get())); break;
		case ITrPatternKind::AmbiguousAggr: Visit(pattern, *reinterpret_cast<ITrAmbiguousAggrPattern*>(pattern.get())); break;
		case ITrPatternKind::AdtAggrEnum: Visit(*reinterpret_cast<ITrAdtAggrEnumPattern*>(pattern.get())); break;
		case ITrPatternKind::Aggr: Visit(*reinterpret_cast<ITrAggrPattern*>(pattern.get())); break;
		case ITrPatternKind::Slice: Visit(*reinterpret_cast<ITrSlicePattern*>(pattern.get())); break;
		case ITrPatternKind::Either: Visit(*reinterpret_cast<ITrEitherPattern*>(pattern.get())); break;
		case ITrPatternKind::Type: Visit(*reinterpret_cast<ITrTypePattern*>(pattern.get())); break;
		default: ;
		}
	}

	void ITrVisitor::Visit(ITrBodySPtr& body)
	{
		for (ITrDefSPtr def : body->defs)
		{
			Visit(def);
		}
		for (ITrStmtSPtr stmt : body->stmts)
		{
			Visit(stmt);
		}
	}

	void ITrVisitor::Walk(ITrStruct& node)
	{
		if (node.attribs)
			Walk(*node.attribs);
		if (node.genDecl)
			Walk(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrUnion& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrValEnum& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrValEnumMember& node)
	{
		if (node.val)
			Visit(node.val);
	}

	void ITrVisitor::Walk(ITrAdtEnum& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrAdtEnumMember& node)
	{
		if (node.type)
			Visit(*node.type);
	}

	void ITrVisitor::Walk(ITrMarkerInterface& node)
	{
	}

	void ITrVisitor::Walk(ITrStrongInterface& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrWeakInterface& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrTypealias& node)
	{
		if (node.genDecl)
			Visit(*node.genDecl);
		if (node.type)
			Visit(*node.type);
	}

	void ITrVisitor::Walk(ITrTypedef& node)
	{
		if (node.genDecl)
			Visit(*node.genDecl);
		Visit(*node.type);
	}

	void ITrVisitor::Walk(ITrVar& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.type)
			Visit(*node.type);
	}

	void ITrVisitor::Walk(ITrFunc& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);
		for (ITrParamSPtr param : node.params)
		{
			if (param->attribs)
				Visit(*param->attribs);
			if (param->type)
				Visit(*param->type);
		}
		if (node.retType)
			Visit(*node.retType);

		if (node.funcKind != ITrFuncKind::EmptyMethod)
		{
			ITrBodySPtr body = m_pMod->GetBody(node);
			Visit(body);
		}
	}

	void ITrVisitor::Walk(ITrImpl& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.genDecl)
			Visit(*node.genDecl);

		ITrBodySPtr body = m_pMod->GetBody(node);
		Visit(body);
	}

	void ITrVisitor::Walk(ITrBlock& node)
	{
		for (ITrStmtSPtr stmt : node.stmts)
			Visit(stmt);
	}

	void ITrVisitor::Walk(ITrIf& node)
	{
		Visit(node.cond);
		Visit(*node.tBlock);
		if (node.fBlock)
			Visit(*node.fBlock);
	}

	void ITrVisitor::Walk(ITrLoop& node)
	{
		for (ITrStmtSPtr stmt : node.stmts)
		{
			Visit(stmt);
		}
	}

	void ITrVisitor::Walk(ITrSwitch& node)
	{
		Visit(node.expr);
		for (ITrSwitchCase case_ : node.cases)
		{
			Visit(case_.pattern);
			if (case_.expr)
				Visit(case_.expr);
			Visit(*case_.block);
		}
	}

	void ITrVisitor::Walk(ITrLabel& node)
	{
	}

	void ITrVisitor::Walk(ITrBreak& node)
	{
	}

	void ITrVisitor::Walk(ITrContinue& node)
	{
	}

	void ITrVisitor::Walk(ITrFallthrough& node)
	{
	}

	void ITrVisitor::Walk(ITrGoto& node)
	{
	}

	void ITrVisitor::Walk(ITrReturn& node)
	{
		if (node.expr)
			Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrThrow& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrDefer& node)
	{
		Visit(node.block);
	}

	void ITrVisitor::Walk(ITrUnsafe& node)
	{
		Visit(*node.block);
	}

	void ITrVisitor::Walk(ITrErrHandler& node)
	{
		for (ITrStmtSPtr stmt : node.stmts)
			Visit(stmt);
	}

	void ITrVisitor::Walk(ITrCompCond& node)
	{
		Visit(*node.tBlock);
		if (node.fBlock)
			Visit(*node.fBlock);
	}

	void ITrVisitor::Walk(ITrLocalVar& node)
	{
		if (node.attribs)
			Visit(*node.attribs);
		if (node.type)
			Visit(*node.type);
		if (node.init)
			Visit(node.init);
	}

	void ITrVisitor::Walk(ITrAssign& node)
	{
		Visit(node.lExpr);
		Visit(node.rExpr);
	}

	void ITrVisitor::Walk(ITrTernary& node)
	{
		Visit(node.cond);
		Visit(node.tExpr);
		Visit(node.fExpr);
	}

	void ITrVisitor::Walk(ITrBinary& node)
	{
		Visit(node.lExpr);
		Visit(node.rExpr);
	}

	void ITrVisitor::Walk(ITrUnary& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrQualNameExpr& node)
	{
	}

	void ITrVisitor::Walk(ITrIndexSlice& node)
	{
		Visit(node.expr);
		Visit(node.index);
		if (node.to)
			Visit(node.to);
	}

	void ITrVisitor::Walk(ITrAmbiguousCall& node)
	{
		Visit(node.expr);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
	}

	void ITrVisitor::Walk(ITrAdtTupleEnumInit& node)
	{
		Visit(node.expr);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
	}

	void ITrVisitor::Walk(ITrFuncCall& node)
	{
		Visit(node.callerOrFunc);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
	}

	void ITrVisitor::Walk(ITrMemberAccess& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrTupleAccess& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrLiteral& node)
	{
	}

	void ITrVisitor::Walk(ITrAmbiguousAggrInit& node)
	{
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
	}

	void ITrVisitor::Walk(ITrStructInit& node)
	{
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
		if (node.defExpr)
			Visit(node.defExpr);
	}

	void ITrVisitor::Walk(ITrUnionInit& node)
	{
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
	}

	void ITrVisitor::Walk(ITrAdtAggrEnumInit& node)
	{
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			Visit(arg->expr);
		}
	}

	void ITrVisitor::Walk(ITrTupleInit& node)
	{
		for (ITrExprSPtr expr : node.exprs)
		{
			Visit(expr);
		}
	}

	void ITrVisitor::Walk(ITrArrayInit& node)
	{
		for (ITrExprSPtr expr : node.exprs)
		{
			Visit(expr);
		}
	}

	void ITrVisitor::Walk(ITrCast& node)
	{
		Visit(*node.type);
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrBlockExpr& node)
	{
		for (ITrStmtSPtr stmt : node.stmts)
			Visit(stmt);
	}

	void ITrVisitor::Walk(ITrUnsafeExpr& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrComma& node)
	{
		for (ITrExprSPtr expr : node.exprs)
			Visit(expr);
	}

	void ITrVisitor::Walk(ITrClosure& node)
	{
		Visit(node.def);
	}

	void ITrVisitor::Walk(ITrMove& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrIs& node)
	{
		Visit(node.expr);
		Visit(*node.type);
	}

	void ITrVisitor::Walk(ITrTry& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrSpecKw& node)
	{
	}

	void ITrVisitor::Walk(ITrCompRun& node)
	{
		Visit(node.expr);
	}

	void ITrVisitor::Walk(ITrType& node)
	{
		for (ITrTypeSPtr subType : node.subTypes)
			Visit(*subType);
	}

	void ITrVisitor::Walk(ITrPlaceholderPattern& node)
	{
	}

	void ITrVisitor::Walk(ITrAmbiguousIdenPattern& node)
	{
	}

	void ITrVisitor::Walk(ITrValueBindPattern& node)
	{
		Visit(node.subPattern);
	}

	void ITrVisitor::Walk(ITrLiteralPattern& node)
	{
	}

	void ITrVisitor::Walk(ITrRangePattern& node)
	{
		Visit(node.from);
		Visit(node.to);
	}

	void ITrVisitor::Walk(ITrTuplePattern& node)
	{
		for (ITrPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void ITrVisitor::Walk(ITrValueEnumPattern& node)
	{
	}

	void ITrVisitor::Walk(ITrAdtTupleEnumPattern& node)
	{
		for (ITrPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void ITrVisitor::Walk(ITrAmbiguousAggrPattern& node)
	{
		for (StdPair<IdenSPtr, ITrPatternSPtr>& arg : node.args)
		{
			Visit(arg.second);
		}
	}

	void ITrVisitor::Walk(ITrAggrPattern& node)
	{
		for (StdPair<IdenSPtr, ITrPatternSPtr>& arg : node.args)
		{
			Visit(arg.second);
		}
	}

	void ITrVisitor::Walk(ITrAdtAggrEnumPattern& node)
	{
		for (StdPair<IdenSPtr, ITrPatternSPtr>& arg : node.args)
		{
			Visit(arg.second);
		}
	}

	void ITrVisitor::Walk(ITrSlicePattern& node)
	{
		for (ITrPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void ITrVisitor::Walk(ITrEitherPattern& node)
	{
		for (ITrPatternSPtr subPattern : node.subPatterns)
		{
			Visit(subPattern);
		}
	}

	void ITrVisitor::Walk(ITrTypePattern& node)
	{
		Visit(*node.type);
	}

	void ITrVisitor::Walk(ITrAttribs& node)
	{
		for (ITrAtAttribSPtr atAttrib : node.atAttribs)
			Visit(*atAttrib);
	}

	void ITrVisitor::Walk(ITrAtAttrib& node)
	{
		for (ITrArgSPtr arg : node.args)
			Visit(arg->expr);
	}

	void ITrVisitor::Walk(ITrGenDecl& node)
	{
		for (ITrGenParamSPtr param : node.params)
		{
			if (param->isVar)
				Visit(*reinterpret_cast<ITrGenValParam*>(param.get()));
			else
				Visit(*reinterpret_cast<ITrGenTypeParam*>(param.get()));
		}
		for (ITrGenTypeBoundSPtr bound : node.bounds)
		{
			Visit(*bound);
		}
	}

	void ITrVisitor::Walk(ITrGenTypeParam& node)
	{
		if (node.defType)
			Visit(*node.defType);
	}

	void ITrVisitor::Walk(ITrGenValParam& node)
	{
		Visit(*node.type);
		if (node.defExpr)
			Visit(node.defExpr);
	}

	void ITrVisitor::Walk(ITrGenTypeBound& node)
	{
		Visit(*node.bound);
	}
}
