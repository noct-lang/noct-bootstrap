#include "ast-printer.hpp"
#include "common/logger.hpp"

namespace Noctis
{
	AstPrinter::AstPrinter()
		: m_Indent(0)
	{
	}

	void AstPrinter::Visit(AstTree& tree)
	{
		g_Logger.Log("(ast-tree source='%s')\n", tree.filepath.c_str());
		++m_Indent;
		Walk(tree);
		--m_Indent;
		g_Logger.Log("(ast-tree end)\n");
	}

	void AstPrinter::Visit(AstIden& node)
	{
		PrintIndent();
		g_Logger.Log("(identifier '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTypeDisambiguation& node)
	{
		PrintIndent();
		g_Logger.Log("(type-disambiguation");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstQualIdenSPtr node)
	{
		switch (node->qualIdenKind)
		{
		case AstQualIdenKind::Identifier: Visit(*static_cast<AstIden*>(node.get())); break;
		case AstQualIdenKind::TypeDisambiguation: Visit(*static_cast<AstTypeDisambiguation*>(node.get())); break;
		default: ;
		}
	}

	void AstPrinter::Visit(AstQualName& node)
	{
		PrintIndent();
		g_Logger.Log("(qual-name");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstParam& node)
	{
		PrintIndent();
		g_Logger.Log("(params");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		PrintIndent();
		g_Logger.Log("(identifiers)\n");
		++m_Indent;
		for (AstParamVarSPtr var : node.vars)
		{
			PrintIndent();
			g_Logger.Log("(iden '%s')\n", var->iden.c_str());
			if (var->attribs)
			{
				++m_Indent;
				Walk(*var->attribs);
				--m_Indent;
			}
		}
		--m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstArg& node)
	{
		PrintIndent();
		if (node.iden.empty())
			g_Logger.Log("(arg");
		else
			g_Logger.Log("(arg '%s", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstModuleDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(module '");
		for (usize i = 0; i < node.moduleIdens.size(); ++i)
		{
			if (i != 0)
				g_Logger.Log(".");
			g_Logger.Log(node.moduleIdens[i]);
		}
		g_Logger.Log("'");
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstUnittestDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(unittest-decl");
		if (!node.name.empty())
			g_Logger.Log(" %s", node.name.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstBenchmarkDecl& node)
	{
		PrintIndent();
		if (node.name.empty())
			g_Logger.Log("(benchmark-decl state='%s'", node.stateIden.c_str());
		else
			g_Logger.Log("(benchmark-decl %s state='%s'", node.name.c_str(), node.stateIden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstStructDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(struct-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstUnionDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(union-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstValueEnumDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(value-enum-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		
		if (node.attribs)
			Visit(*node.attribs);

		for (StdPair<StdString, AstExprSPtr>& member : node.members)
		{
			PrintIndent();
			g_Logger.Log("(value-enum-member '%s')\n", member.first.c_str());
			if (member.second)
			{
				++m_Indent;
				AstVisitor::Visit(member.second);
				--m_Indent;
			}
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstAdtEnumDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(adt-enum-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;

		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (StdPair<StdString, AstTypeSPtr>& member : node.members)
		{
			PrintIndent();
			g_Logger.Log("(adt-enum-member '%s')\n", member.first.c_str());
			if (member.second)
			{
				++m_Indent;
				AstVisitor::Visit(member.second);
				--m_Indent;
			}
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstMarkerInterfaceDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(marker-interface-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstWeakInterfaceDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(weak-interface-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstStrongInterfaceDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(strong-interface-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTypeAliasDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(typealias-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTypeDefDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(typedef-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstVarDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(var-decl idens=(");
		for (u32 i = 0; i < node.idens.size(); ++i)
		{
			if (i != 0)
				g_Logger.Log(", ");
			g_Logger.Log(node.idens[i]);
		}
		g_Logger.Log(")");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstFuncDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(func-decl '%s'", node.iden.c_str());
		if (node.throws)
			g_Logger.Log(" throws");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstParamSPtr param : node.params)
		{
			Visit(*param);
		}
		if (node.errorType)
		{
			PrintIndent();
			g_Logger.Log("(error-type)\n");
			++m_Indent;
			AstVisitor::Visit(node.errorType);
			--m_Indent;
		}
		if (node.retType || !node.namedRet.empty())
		{
			PrintIndent();
			g_Logger.Log("(return)\n");
			++m_Indent;
			if (node.retType)
				AstVisitor::Visit(node.retType);
			for (StdPair<StdVector<StdString>, AstTypeSPtr>& namedRet : node.namedRet)
			{
				PrintIndent();
				g_Logger.Log("(named-return)\n");
				++m_Indent;
				for (StdString& name : namedRet.first)
				{
					g_Logger.Log("(name '%s')\n", name.c_str());
				}
				AstVisitor::Visit(namedRet.second);
				--m_Indent;
			}
			--m_Indent;
		}
		if (node.whereClause)
			Visit(*node.whereClause);

		PrintIndent();
		g_Logger.Log("(body)\n");
		++m_Indent;
		for (AstStmtSPtr stmt : node.stmts)
		{
			AstVisitor::Visit(stmt);
		}
		m_Indent -= 2;
	}

	void AstPrinter::Visit(AstMethodDecl& node)
	{
		PrintIndent();
		const char* rec;
		switch (node.rec)
		{
		case AstMethodReceiverKind::None: rec = "none"; break;
		case AstMethodReceiverKind::Value: rec = "self"; break;
		case AstMethodReceiverKind::Ref: rec = "&self"; break;
		case AstMethodReceiverKind::MutRef: rec = "&mut self"; break;
		default: rec = "";
		}
		
		g_Logger.Log("(method-decl '%s' rec='%s'", node.iden.c_str(), rec);
		if (node.throws)
			g_Logger.Log(" throws");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		for (AstParamSPtr param : node.params)
		{
			Visit(*param);
		}
		if (node.errorType)
		{
			PrintIndent();
			g_Logger.Log("(error-type)\n");
			++m_Indent;
			AstVisitor::Visit(node.errorType);
			--m_Indent;
		}
		if (node.retType || !node.namedRet.empty())
		{
			PrintIndent();
			g_Logger.Log("(return)\n");
			++m_Indent;
			if (node.retType)
				AstVisitor::Visit(node.retType);
			for (StdPair<StdVector<StdString>, AstTypeSPtr>& namedRet : node.namedRet)
			{
				PrintIndent();
				g_Logger.Log("(named-return)\n");
				++m_Indent;
				for (StdString& name : namedRet.first)
				{
					g_Logger.Log("(name '%s')\n", name.c_str());
				}
				AstVisitor::Visit(namedRet.second);
				--m_Indent;
			}
			--m_Indent;
		}
		if (node.whereClause)
			Visit(*node.whereClause);

		PrintIndent();
		g_Logger.Log("(body)\n");
		++m_Indent;
		for (AstStmtSPtr stmt : node.stmts)
		{
			AstVisitor::Visit(stmt);
		}
		m_Indent -= 2;
	}

	void AstPrinter::Visit(AstEmptyMethodDecl& node)
	{
		PrintIndent();
		const char* rec;
		switch (node.rec)
		{
		case AstMethodReceiverKind::None: rec = "none"; break;
		case AstMethodReceiverKind::Value: rec = "self"; break;
		case AstMethodReceiverKind::Ref: rec = "&self"; break;
		case AstMethodReceiverKind::MutRef: rec = "&mut self"; break;
		default: rec = "";
		}

		g_Logger.Log("(empty-method-decl '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstImplDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(impl-decl");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		if (node.attribs)
			Visit(*node.attribs);
		if (node.generics)
			Visit(*node.generics);
		AstVisitor::Visit(node.type);
		if (node.interface)
		{
			PrintIndent();
			g_Logger.Log("(impl-interfaces)\n");
			++m_Indent;
			Visit(*node.interface);
			--m_Indent;
		}
		for (AstStmtSPtr stmt : node.stmts)
		{
			AstVisitor::Visit(stmt);
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstImportStmt& node)
	{
		PrintIndent();
		StdString iden;
		for (StdString& moduleIden : node.moduleIdens)
		{
			if (!iden.empty())
				iden += '.';
			iden += moduleIden;
		}
		g_Logger.Log("(import '%s'", iden.c_str());
		PrintContextAndClose(node.ctx);

		++m_Indent;
		for (std::pair<StdVector<StdString>, StdString>& pair : node.symbols)
		{
			StdString aliasStr;
			if (!pair.second.empty())
				aliasStr = Format(" alias='%s'", pair.second.c_str());
			PrintIndent();
			StdString symbolIden;
			for (StdString& symIden : pair.first)
			{
				if (!symbolIden.empty())
					symbolIden += "::";
				symbolIden += symIden;
			}
			g_Logger.Log("(alias '%s'%s)\n", symbolIden.c_str(), aliasStr.c_str());
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstBlockStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(block-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstIfStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(if-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstLoopStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(loop-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstWhileStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(while-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstDoWhileStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(do-while-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstForStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(for-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		PrintIndent();
		g_Logger.Log("(identifiers)\n");
		++m_Indent;
		for (StdString& iden : node.idens)
		{
			PrintIndent();
			g_Logger.Log("(iden '%s')\n", iden.c_str());
		}
		--m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstSwitchStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(switch-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		if (node.label)
			Visit(*node.label);
		AstVisitor::Visit(node.cond);
		for (AstSwitchCase& case_ : node.cases)
		{
			PrintIndent();
			g_Logger.Log("(case)\n");
			++m_Indent;
			AstVisitor::Visit(case_.pattern);
			if (case_.expr)
				AstVisitor::Visit(case_.expr);
			AstVisitor::Visit(case_.body);
			--m_Indent;
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstLabelStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(label-stmt '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstBreakStmt& node)
	{
		PrintIndent();
		if (node.iden.empty())
			g_Logger.Log("(break-stmt");
		else
			g_Logger.Log("(break-stmt '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstContinueStmt& node)
	{
		PrintIndent();
		if (node.iden.empty())
			g_Logger.Log("(continue-stmt");
		else
			g_Logger.Log("(continue-stmt '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstFallthroughStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(fallthrough-stmt");
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstGotoStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(goto-stmt '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstReturnStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(return-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}


	void AstPrinter::Visit(AstThrowStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(throw-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}


	void AstPrinter::Visit(AstExprStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(expr-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstDeferStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(defer-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstErrDeferStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(stack-defer-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstUnsafeStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(unsafe-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstErrorHandlerStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(error-handler-stmt '%s'", node.errIden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCompIfStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(comp-if-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCompCondStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(comp-cond-stmt '%s'", node.cond.Text().c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCompDebugStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(comp-debug-stmt '%s'", node.cond.Text().c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroLoopStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-loop-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstAssignExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(assign-expr '%s'", GetTokenTypeName(node.op).data());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTernaryExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(ternary-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstBinaryExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(binary-expr '%s'", GetTokenTypeName(node.op).data());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstPostfixExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(postfix-expr '%s'", GetTokenTypeName(node.op).data());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstPrefixExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(prefix-expr '%s'", GetTokenTypeName(node.op).data());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstQualNameExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(qual-name-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstIndexSliceExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(index-slice-expr");
		if (node.nullCoalesce)
			g_Logger.Log(" null-coalescing");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstSliceExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(slice-expr");
		if (node.nullCoalesce)
			g_Logger.Log(" null-coalescing");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstFuncCallExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(func-call-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMemberAccessExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(member-access-expr '%s'", node.iden.c_str());
		if (node.nullCoalesce)
			g_Logger.Log(" null-coalescing");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMethodCallExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(method-access-expr '%s'", node.iden.c_str());
		if (node.nullCoalesce)
			g_Logger.Log(" null-coalescing");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTupleAccessExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-access-expr '%u'", node.index);
		if (node.nullCoalesce)
			g_Logger.Log(" null-coalescing");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstLiteralExpr& node)
	{
		PrintIndent();
		Token& literal = node.literal;
		switch (literal.Type())
		{
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			g_Logger.Log("(literal-expr %i %s", literal.Signed(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::CharLit:
			g_Logger.Log("(literal-expr %u %s", literal.Unsigned(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
			g_Logger.Log("(literal-expr %f %s", literal.Fp(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::True:
			g_Logger.Log("(literal-expr true");
			break;
		case TokenType::False:
			g_Logger.Log("(literal-expr false");
			break;
		case TokenType::StringLit:
			g_Logger.Log("(literal-expr %s StringLit", literal.Text().c_str());
			break;
		case TokenType::Null:
			g_Logger.Log("(literal-expr null");
			break;
		default:
			g_Logger.Log("(literal-expr unknown");
			break;
		}
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstAggrInitExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(aggr-init-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTupleInitExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-init-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstArrayInitExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(array-init-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCastExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(cast-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTransmuteExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(transmute-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMoveExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(move-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstBracketExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(bracket-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstBlockExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(block-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstUnsafeExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(unsafe-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCommaExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(comma-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstClosureExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(closure-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstIsExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(is-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTryExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(try-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstSpecKwExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(spec-kw-expr '%s'", GetTokenTypeName(node.specKw).data());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstCompRunExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(comp-run-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroVarExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-var-expr 's'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstBuiltinType& node)
	{
		PrintIndent();
		g_Logger.Log("(builtin-type '%s'", GetTokenTypeName(node.type).data());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstIdentifierType& node)
	{
		PrintIndent();
		g_Logger.Log("(identifier-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstPointerType& node)
	{
		PrintIndent();
		g_Logger.Log("(pointer-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstReferenceType& node)
	{
		PrintIndent();
		g_Logger.Log("(reference-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstArrayType& node)
	{
		PrintIndent();
		g_Logger.Log("(array-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstSliceType& node)
	{
		PrintIndent();
		g_Logger.Log("(slice-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTupleType& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstOptionalType& node)
	{
		PrintIndent();
		g_Logger.Log("(optional-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstInlineStructType& node)
	{
		PrintIndent();
		g_Logger.Log("(inline-struct-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstInlineEnumType& node)
	{
		PrintIndent();
		g_Logger.Log("(inline-enum-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCompoundInterfaceType& node)
	{
		PrintIndent();
		g_Logger.Log("(compound-interface-type");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstPlaceholderPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(placeholder-pattern");
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstWildcardPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(wildcard-pattern");
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstValueBindPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(value-bind-pattern '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstLiteralPattern& node)
	{
		PrintIndent();
		Token& literal = node.literal;
		switch (literal.Type())
		{
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			g_Logger.Log("(literal-pattern %i %s", literal.Signed(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::CharLit:
			g_Logger.Log("(literal-pattern %u %s", literal.Unsigned(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
			g_Logger.Log("(literal-pattern %f %s", literal.Fp(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::True:
			g_Logger.Log("(literal-pattern true");
			break;
		case TokenType::False:
			g_Logger.Log("(literal-pattern false");
			break;
		case TokenType::StringLit:
			g_Logger.Log("(literal-pattern %s StringLit", literal.Text().c_str());
			break;
		case TokenType::Null:
			g_Logger.Log("(literal-pattern null");
			break;
		default:
			g_Logger.Log("(literal-pattern unknown");
			break;
		}
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstRangePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(range-pattern");
		if (node.inclusive)
			g_Logger.Log(" inclusive");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTuplePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(tuple-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstEnumPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(enum-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstAggrPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(aggr-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		for (StdPair<StdString, AstPatternSPtr>& pair : node.subPatterns)
		{
			g_Logger.Log("(iden: %s)\n", pair.first.c_str());
			++m_Indent;
			AstVisitor::Visit(pair.second);
			--m_Indent;
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstSlicePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(slice-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstEitherPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(either-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstTypePattern& node)
	{
		PrintIndent();
		g_Logger.Log("(type-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstAttribs& node)
	{
		PrintIndent();
		g_Logger.Log("(attribs");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstCompAttrib& node)
	{
		PrintIndent();
		g_Logger.Log("(compiler-attrib '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstUserAttrib& node)
	{
		PrintIndent();
		g_Logger.Log("(user-attrib '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstVisibilityAttrib& node)
	{
		PrintIndent();
		StdString vis = node.kind.empty() ? "public" : node.kind;
		g_Logger.Log("(visibility-attrib '%s'", vis.c_str());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstSimpleAttrib& node)
	{
		PrintIndent();
		g_Logger.Log("(simple-attrib '%s'", GetTokenTypeName(node.attrib).data());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstGenericDecl& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-decl");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstGenericTypeParam& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-value-param '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		if (!node.implTypes.empty())
		{
			PrintIndent();
			g_Logger.Log("(impl-types)\n");
			++m_Indent;
			for (AstIdentifierTypeSPtr implType : node.implTypes)
			{
				Visit(*implType);
			};
			--m_Indent;
		}
		--m_Indent;
	}

	void AstPrinter::Visit(AstGenericValueParam& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-value-param '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstGenericTypeBound& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-type-bound");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstGenericAssocTypeBound& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-assoc-type-bound %s)\n", node.iden.c_str());
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstGenericBoundType& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-bound-type)");
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstGenericWhereClause& node)
	{
		PrintIndent();
		g_Logger.Log("(generic-where-clause");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroVar& node)
	{
		PrintIndent();
		StdStringView kindIden;
		switch (node.kind)
		{
		case AstMacroVarKind::Stmt: kindIden = "stmt"; break;
		case AstMacroVarKind::Expr: kindIden = "expr"; break;
		case AstMacroVarKind::Type: kindIden = "type"; break;
		case AstMacroVarKind::Qual: kindIden = "qual"; break;
		case AstMacroVarKind::Iden: kindIden = "iden"; break;
		case AstMacroVarKind::Attr: kindIden = "attr"; break;
		case AstMacroVarKind::Toks: kindIden = "toks"; break;
		default:  kindIden = "unknown"; break;;
		}
		g_Logger.Log("(macro-var '%s' kind='%s'", node.iden.c_str(), kindIden.data());
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstMacroSeparator& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-separator '");
		for (Token& tok : node.toks)
		{
			if (!tok.Text().empty())
				g_Logger.Log(tok.Text());
			else
				g_Logger.Log(GetTokenTypeName(tok.Type()));
		}
		g_Logger.Log("'");
		PrintContextAndClose(node.ctx);
	}

	void AstPrinter::Visit(AstMacroFragment& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-fragment rep='%s'", GetTokenTypeName(node.repType).data());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroRule& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-rule");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		PrintTokTree(node.body);
		--m_Indent;
	}

	void AstPrinter::Visit(AstDeclMacro& node)
	{
		PrintIndent();
		g_Logger.Log("(decl-macro '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		PrintTokTree(node.body);
		--m_Indent;
	}

	void AstPrinter::Visit(AstRulesDeclMacro& node)
	{
		PrintIndent();
		g_Logger.Log("(rules-decl-macro '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstProcMacro& node)
	{
		PrintIndent();
		g_Logger.Log("(proc-macro '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		PrintTokTree(node.body);
		--m_Indent;
	}

	void AstPrinter::Visit(AstRulesProcMacro& node)
	{
		PrintIndent();
		g_Logger.Log("(rules-proc-macro '%s'", node.iden.c_str());
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Walk(node);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroInstStmt& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-inst-stmt");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Visit(*node.qualName);
		if (node.expandedStmt)
			AstVisitor::Visit(node.expandedStmt);
		else
			PrintTokTree(node.toks);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroInstExpr& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-inst-expr");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Visit(*node.qualName);
		if (node.expandedExpr)
			AstVisitor::Visit(node.expandedExpr);
		else
			PrintTokTree(node.toks);
		--m_Indent;
	}

	void AstPrinter::Visit(AstMacroInstPattern& node)
	{
		PrintIndent();
		g_Logger.Log("(macro-inst-pattern");
		PrintContextAndClose(node.ctx);
		++m_Indent;
		Visit(*node.qualName);
		if (node.expandedPattern)
			AstVisitor::Visit(node.expandedPattern);
		else
			PrintTokTree(node.toks);
		--m_Indent;
	}

	void AstPrinter::PrintTokTree(TokenTree& tokTree)
	{
		PrintIndent();
		g_Logger.Log("(token-tree)\n");
		++m_Indent;
		for (TokenTree& subTok : tokTree.subToks)
		{
			if (!subTok.subToks.empty())
			{
				PrintTokTree(subTok);
			}
			else
			{
				PrintIndent();
				g_Logger.Log("(token '");
				
				Token& tok = subTok.tok;
				if (!tok.Text().empty())
					g_Logger.Log(tok.Text());
				else
					g_Logger.Log(GetTokenTypeName(tok.Type()));

				g_Logger.Log("')\n");
			}
		}
		--m_Indent;
	}

	void AstPrinter::PrintIndent()
	{
		for (u32 i = 0; i < m_Indent - 1; ++i)
			g_Logger.Log(" |");
		if (m_Indent > 0)
			g_Logger.Log(" +");
	}

	void AstPrinter::PrintContextAndClose(AstContextPtr& ctx)
	{
		if (ctx->scope)
		{
			StdString name = ctx->scope->ToString();
			g_Logger.Log(" scope='%s'", name.c_str());
		}
		
		g_Logger.Log(")\n");
	}
}
