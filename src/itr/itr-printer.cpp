#include "itr-printer.hpp"



#include "common/context.hpp"
#include "common/logger.hpp"
#include "module/module.hpp"

namespace Noctis
{
	ITrPrinter::ITrPrinter(Context* pCtx)
		: ITrVisitor(true)
		, m_pCtx(pCtx)
		, m_Indent(1)
	{
	}

	void ITrPrinter::Print(ITrModule& pMod)
	{
		SetModule(pMod);

		g_Logger.Log("(itr-tree)\n");
		m_Indent = 1;
		for (StdVector<ITrDefSPtr>& defs : pMod.defMapping)
		{
			for (ITrDefSPtr def : defs)
			{
				if (def->isModDef)
					ITrVisitor::Visit(def);
			}
		}
		g_Logger.Log("(itr-tree end)\n");
	}

	void ITrPrinter::Visit(ITrStruct& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(struct %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrUnion& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(union %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrValEnum& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(value-enum %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrValEnumMember& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(value-enum member %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAdtEnum& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(adt-enum %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAdtEnumMember& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(adt-enum member %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrMarkerInterface& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(marker-interface %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrStrongInterface& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(strong-interface %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrWeakInterface& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(weak-interface %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTypealias& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(typealias %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTypedef& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(typedef %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrVar& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(var %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrFunc& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		StdStringView funcKind;
		switch (node.funcKind)
		{
		case ITrFuncKind::Func: funcKind = "func"; break;
		case ITrFuncKind::Method: funcKind = "method"; break;
		case ITrFuncKind::EmptyMethod: funcKind = "empty-method"; break;
		case ITrFuncKind::Closure: funcKind = "closure"; break;
		default: funcKind = "";
		}
		g_Logger.Log("(%s %s)\n", funcKind.data(), name.c_str());

		++m_Indent;
		for (ITrParamSPtr param : node.params)
		{
			PrintIndent();
			StdString name = param->iden;
			g_Logger.Log("(param %s)\n", name.c_str());
			++m_Indent;
			if (param->attribs)
				Visit(*param->attribs);
			Visit(*param->type);
			--m_Indent;
		}

		if (node.attribs)
			Visit(*node.attribs);
		if (node.retType)
			Visit(*node.retType);

		if (node.funcKind != ITrFuncKind::EmptyMethod)
		{
			ITrBodySPtr body = m_pMod->GetBody(node);
			Visit(body);
		}
		
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrImpl& node)
	{
		PrintIndent();
		g_Logger.Log("(impl)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrBlock& node)
	{
		PrintIndent();
		g_Logger.Log("(block)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrIf& node)
	{
		PrintIndent();
		g_Logger.Log("(if)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrLoop& node)
	{
		PrintIndent();
		g_Logger.Log("(loop%s)\n", !node.label.empty() ? Format(" label: %s", node.label) : "");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrForRange& node)
	{
		PrintIndent();
		g_Logger.Log("(for-range%s)", !node.label.empty() ? Format(" label: %s", node.label) : "");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrSwitch& node)
	{
		PrintIndent();
		g_Logger.Log("(switch)\n");
		++m_Indent;
		ITrVisitor::Visit(node.expr);
		for (ITrSwitchCase case_ : node.cases)
		{
			g_Logger.Log("(switch-case)\n");
			++m_Indent;
			ITrVisitor::Visit(case_.pattern);
			if (case_.expr)
				ITrVisitor::Visit(case_.expr);
			Visit(*case_.block);
			--m_Indent;
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrLabel& node)
	{
		PrintIndent();
		g_Logger.Log("(label %s)\n", node.label.c_str());
	}

	void ITrPrinter::Visit(ITrBreak& node)
	{
		PrintIndent();
		g_Logger.Log("(break");
		if (!node.label.empty())
			g_Logger.Log("%s", node.label.c_str());
		g_Logger.Log(")\n");
	}

	void ITrPrinter::Visit(ITrContinue& node)
	{
		PrintIndent();
		g_Logger.Log("(continue");
		if (!node.label.empty())
			g_Logger.Log("%s", node.label.c_str());
		g_Logger.Log(")\n");
	}

	void ITrPrinter::Visit(ITrFallthrough& node)
	{
		PrintIndent();
		g_Logger.Log("(fallthrough)\n");
	}

	void ITrPrinter::Visit(ITrGoto& node)
	{
		PrintIndent();
		g_Logger.Log("(goto %s)\n", node.label.c_str());
	}

	void ITrPrinter::Visit(ITrReturn& node)
	{
		PrintIndent();
		g_Logger.Log("(return)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrThrow& node)
	{
		PrintIndent();
		g_Logger.Log("(throw)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrDefer& node)
	{
		PrintIndent();
		g_Logger.Log("(defer)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrUnsafe& node)
	{
		PrintIndent();
		g_Logger.Log("(unsafe)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrErrHandler& node)
	{
		PrintIndent();
		g_Logger.Log("(err-handler)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrCompCond& node)
	{
		PrintIndent();
		g_Logger.Log("(comp-cond)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrLocalVar& node)
	{
		PrintIndent();
		g_Logger.Log("(local-var)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAssign& node)
	{
		PrintIndent();
		StdStringView opName = GetOpName(node.op);
		g_Logger.Log("(assign %s)\n", opName.data());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTernary& node)
	{
		PrintIndent();
		g_Logger.Log("(ternary)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrBinary& node)
	{
		PrintIndent();
		StdStringView opName = GetOpName(node.op);
		g_Logger.Log("(binary %s)\n", opName.data());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrUnary& node)
	{
		PrintIndent();
		StdStringView opName = GetOpName(node.op);
		g_Logger.Log("(unary %s)\n", opName.data());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrQualNameExpr& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(qual-name %s)\n", name.c_str());
	}

	void ITrPrinter::Visit(ITrIndexSlice& node)
	{
		PrintIndent();
		g_Logger.Log("(index-slice)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrExprSPtr& ptr, ITrAmbiguousCall node)
	{
		PrintIndent();
		g_Logger.Log("(ambiguous-call)\n");
		++m_Indent;
		ITrVisitor::Visit(node.expr);
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log(" %s", arg->iden.c_str());
			g_Logger.Log(")\n");
			ITrVisitor::Visit(arg->expr);
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrFuncCall& node)
	{
		PrintIndent();
		g_Logger.Log("(func-call");
		if (node.isMethod)
		{
			g_Logger.Log(" %s", node.iden.c_str());

			if (node.nullCoalesce)
				g_Logger.Log(" ?.");
		}
		g_Logger.Log(")\n");
		
		++m_Indent;
		ITrVisitor::Visit(node.callerOrFunc);
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log(" %s", arg->iden.c_str());
			g_Logger.Log(")\n");
			ITrVisitor::Visit(arg->expr);
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAdtTupleEnumInit& node)
	{
		PrintIndent();
		g_Logger.Log("(adt-tuple-enum-init)\n");
		++m_Indent;
		ITrVisitor::Visit(node.expr);
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log(" %s", arg->iden.c_str());
			g_Logger.Log(")\n");
			ITrVisitor::Visit(arg->expr);
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrMemberAccess& node)
	{
		PrintIndent();
		g_Logger.Log("(member-access %s", node.iden.c_str());
		if (node.nullCoalesce)
			g_Logger.Log(" ?.");
		g_Logger.Log(" )\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTupleAccess& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-access %u", node.index);
		if (node.nullCoalesce)
			g_Logger.Log(" ?.");
		g_Logger.Log(" )\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrLiteral& node)
	{
		PrintIndent();
		Token& literal = node.lit;
		switch (literal.Type())
		{
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			g_Logger.Log("(literal %i %s)\n", literal.Signed(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::CharLit:
			g_Logger.Log("(literal %u %s)\n", literal.Unsigned(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
			g_Logger.Log("(literal %f %s)\n", literal.Fp(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::True:
			g_Logger.Log("(literal true)\n");
			break;
		case TokenType::False:
			g_Logger.Log("(literal false)\n");
			break;
		case TokenType::StringLit:
			g_Logger.Log("(literal %s StringLit)\n", literal.Text().c_str());
			break;
		case TokenType::Null:
			g_Logger.Log("(literal null)\n");
			break;
		default:
			g_Logger.Log("(literal unknown)\n");
			break;
		}
	}

	void ITrPrinter::Visit(ITrExprSPtr& ptr, ITrAmbiguousAggrInit node)
	{
		PrintIndent();
		g_Logger.Log("(ambiguous-aggr-init)\n");
		++m_Indent;
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log(" %s", arg->iden.c_str());
			g_Logger.Log(")\n");
			ITrVisitor::Visit(arg->expr);
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrStructInit& node)
	{
		PrintIndent();
		g_Logger.Log("(aggr-init)\n");
		++m_Indent;
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log(" %s", arg->iden.c_str());
			g_Logger.Log(")\n");
			ITrVisitor::Visit(arg->expr);
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAdtAggrEnumInit& node)
	{
		PrintIndent();
		g_Logger.Log("(adt-aggr-enum-init)\n");
		++m_Indent;
		Visit(*node.type);
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log(" %s", arg->iden.c_str());
			g_Logger.Log(")\n");
			ITrVisitor::Visit(arg->expr);
		}
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTupleInit& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-init)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrArrayInit& node)
	{
		PrintIndent();
		g_Logger.Log("(array-init)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrCast& node)
	{
		PrintIndent();
		g_Logger.Log("(cast)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrBlockExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(block-expr)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrUnsafeExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(unsafe-expr)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrComma& node)
	{
		PrintIndent();
		g_Logger.Log("(comma)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrClosure& node)
	{
		PrintIndent();
		g_Logger.Log("(closure)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrMove& node)
	{
		PrintIndent();
		g_Logger.Log("(move)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrIs& node)
	{
		PrintIndent();
		g_Logger.Log("(is)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTry& node)
	{
		PrintIndent();
		g_Logger.Log("(try)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrSpecKw& node)
	{
		PrintIndent();
		StdStringView kwName = GetTokenTypeName(node.kw);
		g_Logger.Log("(spec-kw %s)\n", kwName.data());
	}

	void ITrPrinter::Visit(ITrCompRun& node)
	{
		PrintIndent();
		g_Logger.Log("(comp-run)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrType& node)
	{
		PrintIndent();
		StdString type = m_pCtx->typeReg.ToString(node.handle);
		g_Logger.Log("(type %s)\n", type.c_str());
	}

	void ITrPrinter::Visit(ITrPlaceholderPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(placeholder-pattern %s)\n", node.patternKind == ITrPatternKind::Wildcard ? "wildcard" : "");
	}

	void ITrPrinter::Visit(ITrValueBindPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(value-bind-pattern %s)\n", node.iden.c_str());

		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrLiteralPattern& node)
	{
		PrintIndent();
		Token& literal = node.lit;
		switch (literal.Type())
		{
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			g_Logger.Log("(literal-pattern %i %s)\n", literal.Signed(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::CharLit:
			g_Logger.Log("(literal-pattern %u %s)\n", literal.Unsigned(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
			g_Logger.Log("(literal-pattern %f %s)\n", literal.Fp(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::True:
			g_Logger.Log("(literal-pattern true)\n");
			break;
		case TokenType::False:
			g_Logger.Log("(literal-pattern false)\n");
			break;
		case TokenType::StringLit:
			g_Logger.Log("(literal-pattern %s StringLit)\n", literal.Text().c_str());
			break;
		case TokenType::Null:
			g_Logger.Log("(literal-pattern null)\n");
			break;
		default:
			g_Logger.Log("(literal-pattern unknown)\n");
			break;
		}
	}

	void ITrPrinter::Visit(ITrRangePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(range-pattern)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTuplePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-pattern)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrValueEnumPattern& node)
	{
		PrintIndent();
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(value-enum-pattern %s)\n", name.c_str());
	}

	void ITrPrinter::Visit(ITrAdtTupleEnumPattern& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(tuple-enum-pattern %s)\n", name.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(ambiguous-aggr-pattern %s)\n", name.c_str());
		
		++m_Indent;
		for (StdPair<StdString, ITrPatternSPtr> arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg %s)\n", arg.first.c_str());
			++m_Indent;
			ITrVisitor::Visit(arg.second);
			--m_Indent;
		};
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAggrPattern& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(aggr-pattern %s)\n", name.c_str());

		++m_Indent;
		for (StdPair<StdString, ITrPatternSPtr> arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg %s)\n", arg.first.c_str());
			++m_Indent;
			ITrVisitor::Visit(arg.second);
			--m_Indent;
		};
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAdtAggrEnumPattern& node)
	{
		PrintIndent();
		StdString name = node.qualName->ToString();
		g_Logger.Log("(adt-aggr-enum-pattern %s)\n", name.c_str());

		++m_Indent;
		for (StdPair<StdString, ITrPatternSPtr> arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg %s)\n", arg.first.c_str());
			++m_Indent;
			ITrVisitor::Visit(arg.second);
			--m_Indent;
		};
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrSlicePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(slice-pattern)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrEitherPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(either-pattern)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrTypePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(type-pattern)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAttribs& node)
	{
		PrintIndent();
		g_Logger.Log("(attribs: ");

		if (node.attribs != Attribute::None)
		{
			if (ENUM_IS_SET(node.attribs, Attribute::Const))
				g_Logger.Log("const ");
			if (ENUM_IS_SET(node.attribs, Attribute::Mut))
				g_Logger.Log("mut ");
			if (ENUM_IS_SET(node.attribs, Attribute::Static))
				g_Logger.Log("static ");
			if (ENUM_IS_SET(node.attribs, Attribute::Comptime))
				g_Logger.Log("comptime ");
			if (ENUM_IS_SET(node.attribs, Attribute::Lazy))
				g_Logger.Log("lazy ");
			if (ENUM_IS_SET(node.attribs, Attribute::Move))
				g_Logger.Log("move ");

			g_Logger.Log(", ");
		}

		switch (node.vis)
		{
		case Visibility::Private:
			g_Logger.Log("vis=private");
			break;
		case Visibility::Module:
			g_Logger.Log("vis=module");
			break;
		case Visibility::Package:
			g_Logger.Log("vis=package");
			break;
		case Visibility::Public:
			g_Logger.Log("vis=public");
			break;
		case Visibility::Dynlib:
			g_Logger.Log("vis=dynlib");
			break;
		default: ;
		}

		g_Logger.Log(")\n");

		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrAtAttrib& node)
	{
		PrintIndent();
		g_Logger.Log("(at-attrib %s%s)\n", node.iden.c_str(), node.isCompAttrib ? " comp-attrib" : "");
		++m_Indent;
		for (ITrArgSPtr arg : node.args)
		{
			PrintIndent();
			g_Logger.Log("(arg");
			if (!arg->iden.empty())
				g_Logger.Log("(arg %s)\n", arg->iden.c_str());
			else
				g_Logger.Log("(arg)\n");
			
			++m_Indent;
			ITrVisitor::Visit(arg->expr);
			--m_Indent;
		};
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrGenDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(gen-decl)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrGenTypeParam& node)
	{
		PrintIndent();
		g_Logger.Log("(gen-type-param %s)\n", node.iden.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrGenValParam& node)
	{
		PrintIndent();
		g_Logger.Log("(gen-val-param %s)\n", node.iden.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrGenTypeBound& node)
	{
		PrintIndent();
		g_Logger.Log("(gen-bound)\n");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void ITrPrinter::Visit(ITrBodySPtr& body)
	{
		PrintIndent();
		g_Logger.Log("(body)\n");
		++m_Indent;
		ITrVisitor::Visit(body);
		--m_Indent;
	}

	void ITrPrinter::PrintIndent()
	{
		for (usize i = 0; i < m_Indent - 1; ++i)
			g_Logger.Log(" |");
		if (m_Indent > 0)
			g_Logger.Log(" +");
	}
}
