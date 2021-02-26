#include "ast-to-itr-lowering.hpp"
#include "ast/ast.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/type.hpp"
#include "itr/itr.hpp"
#include "module/attributes.hpp"
#include "module/module.hpp"

namespace Noctis
{
	AstToITrLowering::AstToITrLowering()
		: AstSemanticPass("ast to itr lowering")
		, m_InFunc(false)
	{
	}

	void AstToITrLowering::Visit(AstTypeDisambiguation& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);
		Visit(*node.interface->qualName);
		TypeDisambiguationSPtr disambiguation = TypeDisambiguation::Create(type->handle, node.interface->qualName->ctx->qualName);
		m_TypeDisambiguation.reset(new ITrTypeDisambiguation{disambiguation , type, m_QualName, node.ctx->startIdx, node.ctx->endIdx });
	}

	void AstToITrLowering::Visit(AstIden& node)
	{
		StdVector<IdenGeneric> generics;
		StdPairVector<ITrTypeSPtr, ITrExprSPtr> assocArgs;
		generics.reserve(node.args.size());
		assocArgs.reserve(node.args.size());
		for (AstGenericArg& arg : node.args)
		{
			if (arg.kind == GenericArgKind::Type)
			{
				ITrTypeSPtr type = VisitAndGetType(arg.type);
				IdenGeneric genArg;
				genArg.isType = true;
				genArg.isSpecialized = true;
				genArg.type = type->handle;
				generics.push_back(genArg);
				assocArgs.emplace_back(type, nullptr);
			}
			else
			{
				IdenGeneric genArg;
				genArg.isType = false;
				genArg.isSpecialized = true;
				genArg.itrExpr = VisitAndGetExpr(arg.expr);
				generics.push_back(genArg);
				assocArgs.emplace_back(nullptr, nullptr);
			}
		}

		ITrIdenSPtr itrIden{ new ITrIden{ node.iden, generics, std::move(assocArgs), node.ctx->startIdx, node.ctx->endIdx } };
		m_Idens.push_back(itrIden);
	}

	void AstToITrLowering::Visit(AstQualName& node)
	{
		Walk(node);

		QualNameSPtr qualName;
		if (m_TypeDisambiguation)
		{
			qualName = QualName::Create(m_TypeDisambiguation->disambiguation);
			qualName = qualName->Append(m_Idens[0]->iden, m_Idens[0]->generics);
		}
		else
		{
			qualName = QualName::Create(m_Idens[0]->iden, m_Idens[0]->generics);
		}
		
		for (usize i = 1; i < m_Idens.size(); ++i)
		{
			qualName = qualName->Append(m_Idens[i]->iden, m_Idens[i]->generics);
		}

		m_QualName.reset(new ITrQualName{ qualName, m_TypeDisambiguation, std::move(m_Idens), node.hasColonColon });
		new (&m_Idens) StdVector<ITrIdenSPtr>{};
		
		node.ctx->qualName = qualName;
		m_TypeDisambiguation = nullptr;
	}

	void AstToITrLowering::Visit(AstParam& node)
	{
		Walk(node);
		// Done separately in 'GetParams()'
	}

	void AstToITrLowering::Visit(AstArg& node)
	{
		Walk(node);
		// Done separately in 'GetArgs()'
	}

	void AstToITrLowering::Visit(AstModuleDecl& node)
	{
		// We can ignore this
	}

	void AstToITrLowering::Visit(AstUnittestDecl& node)
	{
		// TODO
	}

	void AstToITrLowering::Visit(AstBenchmarkDecl& node)
	{
		// TODO
	}

	void AstToITrLowering::Visit(AstStructDecl& node)
	{
		
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);
		
		ITrDefSPtr def{ new ITrStruct{ attribs, genDecl, node.ctx->qualName, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		for (AstStmtSPtr member : node.members)
		{
			AstVisitor::Visit(member);
		}
		ITrBodySPtr body{ new ITrBody{ PopDefFrame(), {} } };

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstUnionDecl& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);

		ITrDefSPtr def{ new ITrUnion{ attribs, genDecl, node.ctx->qualName, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		for (AstStmtSPtr member : node.members)
		{
			AstVisitor::Visit(member);
		}
		ITrBodySPtr body{ new ITrBody{ PopDefFrame(), {} } };
		
		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstValueEnumDecl& node)
	{
		QualNameSPtr enumQualName = node.ctx->qualName;
		ITrModule& mod = g_Ctx.activeModule->itrModule;
		
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdString, AstExprSPtr> astMember : node.members)
		{
			ITrExprSPtr expr;
			if (astMember.second)
				expr = VisitAndGetExpr(astMember.second);

			ITrDefSPtr member{ new ITrValEnumMember{ enumQualName, astMember.first, expr, node.ctx->startIdx, node.ctx->endIdx } };
			member->ptr = member;
			member->fileName = m_TreeFilename;
			mod.AddDefinition(member);
			members.push_back(member);
		}

		ITrBodySPtr body{ new ITrBody{ std::move(members) , {} } };

		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrDefSPtr def{ new ITrValEnum{ attribs, enumQualName, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstAdtEnumDecl& node)
	{
		QualNameSPtr enumQualName = node.ctx->qualName;
		ITrModule& mod = g_Ctx.activeModule->itrModule;

		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);
		ITrDefSPtr def{ new ITrAdtEnum{ attribs, genDecl, enumQualName, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdString, AstTypeSPtr> astMember : node.members)
		{
			ITrTypeSPtr type = VisitAndGetType(astMember.second);
			ITrDefSPtr member{ new ITrAdtEnumMember{ enumQualName, astMember.first, type, node.ctx->startIdx, node.ctx->endIdx } };
			member->ptr = member;
			member->fileName = m_TreeFilename;
			mod.AddDefinition(member);
			members.push_back(member);
		}

		StdVector<ITrDefSPtr> tmp = PopDefFrame();
		members.insert(members.end(), tmp.begin(), tmp.end());

		ITrBodySPtr body{ new ITrBody{ std::move(members) , {} } };

		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstMarkerInterfaceDecl& node)
	{
		m_ImplType = g_TypeReg.Iden(TypeMod::None, node.ctx->qualName);
		m_ImplType = TypeHandle{};
		
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrDefSPtr def{ new ITrMarkerInterface{ attribs, node.ctx->qualName, node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstWeakInterfaceDecl& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl;// = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);

		ITrDefSPtr def{ new ITrWeakInterface{ attribs, genDecl, node.ctx->qualName, node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		m_ImplType = g_TypeReg.Iden(TypeMod::None, node.ctx->qualName);
		for (AstStmtSPtr member : node.members)
		{
			AstVisitor::Visit(member);
		}
		m_ImplType = TypeHandle{};
		ITrBodySPtr body{ new ITrBody{ PopDefFrame(), {} } };
		
		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstStrongInterfaceDecl& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);

		StdPairVector<QualNameSPtr, SpanId> interfaces;
		usize size = node.implInterfaces.size();
		interfaces.resize(size);
		for (usize i = 0; i< size; ++i)
		{
			ITrTypeSPtr interface = VisitAndGetType(node.implInterfaces[i]);
			TypeSPtr interfaceType = interface->handle.Type();
			interfaces[i].first = interfaceType->AsIden().qualName;
			interfaces[i].second = interface->startIdx;
		}

		ITrDefSPtr def{ new ITrStrongInterface{ attribs, genDecl, node.ctx->qualName, std::move(interfaces), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		m_ImplType = g_TypeReg.Iden(TypeMod::None, node.ctx->qualName);
		for (AstStmtSPtr member : node.members)
		{
			AstVisitor::Visit(member);
		}
		m_ImplType = TypeHandle{};
		ITrBodySPtr body{ new ITrBody{ PopDefFrame(), {} } };
		
		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstTypeAliasDecl& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);

		ITrTypeSPtr type = VisitAndGetType(node.type);
		ITrDefSPtr def{ new ITrTypealias{ attribs, genDecl, node.ctx->qualName, type, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstTypeDefDecl& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
			HandleGenerics(genDecl, node.ctx->qualName);

		ITrTypeSPtr type = VisitAndGetType(node.type);
		ITrDefSPtr def{ new ITrTypedef{ attribs, genDecl, node.ctx->qualName, type, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstVarDecl& node)
	{
		if (m_InFunc)
		{
			ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
			ITrTypeSPtr type = VisitAndGetType(node.type);
			ITrExprSPtr expr = VisitAndGetExpr(node.expr);
			ITrStmtSPtr stmt{ new ITrLocalVar{ attribs, node.idens, type, expr, node.ctx->startIdx, node.ctx->endIdx } };
			m_Stmt = stmt;
		}
		else
		{
			ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
			ITrTypeSPtr type = VisitAndGetType(node.type);
			ITrExprSPtr expr = VisitAndGetExpr(node.expr);

			ITrModule& mod = g_Ctx.activeModule->itrModule;
			bool initVars = true;
			if (expr && expr->exprKind == ITrExprKind::TupleInit)
			{
				ITrTupleInit& tupInit = static_cast<ITrTupleInit&>(*expr);
				if (tupInit.exprs.size() > 1 && tupInit.exprs.size() != node.idens.size())
				{
					Span span = g_SpanManager.GetSpan(node.ctx->startIdx);
					g_ErrorSystem.Error(span, "Number of initializers does not match match number of values, has %u values, but found %u initializers", u32(node.idens.size()), u32(tupInit.exprs.size()));
					initVars = false;
				}
			}
			
			for (usize i = 0; i < node.idens.size(); ++i)
			{
				StdString& iden = node.idens[i];
				ITrExprSPtr initExpr;
				if (expr)
				{
					if (node.idens.size() == 1)
					{
						initExpr = expr;
					}
					else if (initVars && expr->exprKind == ITrExprKind::TupleInit)
					{
						ITrTupleInit& tupInit = static_cast<ITrTupleInit&>(*expr);
						initExpr = tupInit.exprs[i];
					}
				}

				QualNameSPtr qualName = node.ctx->scope->Append(node.idens[i]);
				ITrDefSPtr def{ new ITrVar{ attribs, qualName, type, initExpr, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
				def->ptr = def;
				def->fileName = m_TreeFilename;
				def->ptr = def;
				mod.AddDefinition(def);
				PushDef(def);
			}
		}
	}

	void AstToITrLowering::Visit(AstFuncDecl& node)
	{
		// TODO: throws

		bool prevInFunc = m_InFunc;
		m_InFunc = true;
		
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
		{
			HandleGenerics(genDecl, node.ctx->qualName);
			if (node.whereClause)
				HandleWhereClause(*node.whereClause, genDecl);
		}
		
		StdVector<ITrParamSPtr> params = GetParams(node.params);
		ITrTypeSPtr errorType = VisitAndGetType(node.errorType);

		StdVector<ITrStmtSPtr> stmts;
		ITrTypeSPtr retType;
		if (!node.namedRet.empty())
		{
			GetNamedReturns(retType, stmts, node.namedRet);
		}
		else if (node.retType)
		{
			retType = VisitAndGetType(node.retType);
		}
		
		ITrDefSPtr def{ new ITrFunc{ attribs, genDecl, node.ctx->qualName, std::move(params), errorType, retType, ITrFuncKind::Func, node.isUnsafe, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		for (AstStmtSPtr astStmt : node.stmts)
		{
			stmts.push_back(VisitAndGetStmt(astStmt));
		}
		ITrBodySPtr body{ new ITrBody{ PopDefFrame(), std::move(stmts) } };

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);

		m_InFunc = prevInFunc;
	}

	void AstToITrLowering::Visit(AstMethodDecl& node)
	{
		// TODO: throws
		
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
		{
			HandleGenerics(genDecl, node.ctx->qualName);
			if (node.whereClause)
				HandleWhereClause(*node.whereClause, genDecl);
		}
		
		StdVector<ITrParamSPtr> params = GetParams(node.params);
		AddMethodReceiverToParams(node, params);
		ITrTypeSPtr errorType = VisitAndGetType(node.errorType);

		StdVector<ITrStmtSPtr> stmts;
		ITrTypeSPtr retType;
		if (!node.namedRet.empty())
		{
			GetNamedReturns(retType, stmts, node.namedRet);
		}
		else if (node.retType)
		{
			retType = VisitAndGetType(node.retType);
		}

		ITrDefSPtr def{ new ITrFunc{ attribs, genDecl, node.ctx->qualName, std::move(params), errorType, retType, node.empty ? ITrFuncKind::EmptyMethod : ITrFuncKind::Method, node.isUnsafe, IsModDef(), node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		for (AstStmtSPtr astStmt : node.stmts)
		{
			stmts.push_back(VisitAndGetStmt(astStmt));
		}
		ITrBodySPtr body{ new ITrBody{ PopDefFrame(), std::move(stmts) } };

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstImplDecl& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);

		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		ITrGenDeclSPtr genDecl = VisitAndGetGenDecl(node.generics);
		if (genDecl)
		{
			HandleGenerics(genDecl, node.ctx->qualName);
			if (node.whereClause)
				HandleWhereClause(*node.whereClause, genDecl);
		}

		StdPair<QualNameSPtr, SpanId> interface;
		if (node.interface)
		{
			ITrTypeSPtr interfaceType = VisitAndGetType(node.interface);
			interface.first = interfaceType->handle.AsIden().qualName;
			interface.second = interfaceType->startIdx;
		}

		ITrDefSPtr def{ new ITrImpl{ attribs, genDecl, node.ctx->qualName, type, interface, node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		m_ITrImplType = type;
		m_ImplType = type->handle;
		for (AstStmtSPtr stmt : node.stmts)
		{
			AstVisitor::Visit(stmt);
		}
		m_ImplType = TypeHandle{};
		m_ITrImplType = nullptr;
		StdVector<ITrDefSPtr> defs = PopDefFrame();
		
		ITrBodySPtr body{ new ITrBody{ std::move(defs), {} } };

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}


	void AstToITrLowering::Visit(AstErrHandler& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.errType);
		ITrBlockSPtr block = VisitAndGetBlock(node.block, node.ctx->qualName->LastIden());

		QualNameSPtr qualName = QualName::Create(block->scopeName);
		ITrDefSPtr def{ new ITrErrHandler{ qualName, node.errIden, type, node.ctx->startIdx, node.ctx->endIdx } };

		PushDefFrame(def);
		AstBlockStmt& astBlock = static_cast<AstBlockStmt&>(*node.block);
		for (AstStmtSPtr stmt : astBlock.stmts)
		{
			AstVisitor::Visit(stmt);
		}
		StdVector<ITrDefSPtr> defs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(defs), std::move(block->stmts) } };

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		PushDef(def);
	}


	void AstToITrLowering::Visit(AstImportStmt& node)
	{
		// We can ignore this
	}

	void AstToITrLowering::Visit(AstBlockStmt& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			if (astStmt->stmtKind == AstStmtKind::Decl)
				AstVisitor::Visit(astStmt);
			else
				stmts.push_back(VisitAndGetStmt(astStmt));
		}

		if (stmts.size() == 1)
		{
			ITrStmtSPtr stmt = stmts[0];
			if (stmt->stmtKind == ITrStmtKind::Block ||
				stmt->stmtKind == ITrStmtKind::Unsafe)
			{
				m_Stmt = stmt;
				return;
			}
		}

		QualNameSPtr blockName = node.ctx->qualName;
		if (!blockName)
			blockName = node.ctx->scope;
		ITrStmtSPtr stmt{ new ITrBlock{ blockName->LastIden(), std::move(stmts), node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstIfStmt& node)
	{
		const StdString& baseScopeName = node.ctx->qualName->LastIden();
		ITrBlockSPtr tBlock = VisitAndGetBlock(node.body, baseScopeName + "__tblock");
		ITrBlockSPtr fBlock = VisitAndGetBlock(node.elseBody, baseScopeName + "__fblock");
		ITrExprSPtr cond = VisitAndGetExpr(node.cond);

		ITrLocalVarSPtr decl;
		if (node.decl)
		{
			ITrStmtSPtr declStmt = VisitAndGetStmt(node.decl);
			decl = *reinterpret_cast<ITrLocalVarSPtr*>(&declStmt);
		}

		ITrStmtSPtr stmt{ new ITrIf{ false, decl, cond, tBlock, fBlock, node.ctx->startIdx } };
		stmt.reset(new ITrBlock{ node.ctx->qualName->LastIden(), { stmt }, node.ctx->startIdx, node.ctx->endIdx });
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstLoopStmt& node)
	{
		ITrBlockSPtr body = VisitAndGetBlock(node.body, node.ctx->qualName->LastIden());
		body->scopeName = node.ctx->qualName->LastIden();
		StdString label = node.label ? node.label->iden : "";
		ITrStmtSPtr stmt{ new ITrLoop{ label, body, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstWhileStmt& node)
	{
		ITrBlockSPtr body = VisitAndGetBlock(node.body, node.ctx->qualName->LastIden());
		body->scopeName = node.ctx->qualName->LastIden();

		// Condition
		ITrExprSPtr cond = VisitAndGetExpr(node.cond);
		cond = ITrExprSPtr{ new ITrUnary{ OperatorKind::Not, cond, cond->startIdx, cond->endIdx } };
		ITrStmtSPtr breakStmt{ new ITrBreak{ "", cond->startIdx, cond->endIdx } };
		ITrBlockSPtr breakBlock{ new ITrBlock{ body->scopeName + "__loop_break", { breakStmt }, cond->startIdx, cond->endIdx } };
		body->stmts.insert(body->stmts.begin(), ITrStmtSPtr{ new ITrIf{ false, nullptr, cond, breakBlock, nullptr, node.ctx->startIdx } });

		StdString label = node.label ? node.label->iden : "";
		ITrStmtSPtr stmt{ new ITrLoop{ label, body, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstDoWhileStmt& node)
	{
		ITrBlockSPtr body = VisitAndGetBlock(node.body, node.ctx->qualName->LastIden());
		body->scopeName = node.ctx->qualName->LastIden();

		StdVector<ITrStmtSPtr> stmts;
		if (body->stmtKind == ITrStmtKind::Block)
			stmts = static_cast<ITrBlock*>(body.get())->stmts;
		else
			stmts.push_back(body);

		// Condition
		ITrExprSPtr cond = VisitAndGetExpr(node.cond);
		cond = ITrExprSPtr{ new ITrUnary{ OperatorKind::Not, cond, cond->startIdx, cond->endIdx } };
		ITrStmtSPtr breakStmt{ new ITrBreak{ "", cond->startIdx, cond->endIdx } };
		ITrBlockSPtr breakBlock{ new ITrBlock{ body->scopeName + "__loop_break", { breakStmt }, cond->startIdx, cond->endIdx } };
		stmts.push_back(ITrStmtSPtr{ new ITrIf{ false, nullptr, cond, breakBlock, nullptr, cond->startIdx } });

		StdString label = node.label ? node.label->iden : "";
		ITrStmtSPtr stmt{ new ITrLoop{ label, body, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstForStmt& node)
	{
		ITrExprSPtr range = VisitAndGetExpr(node.range);
		ITrBlockSPtr body = VisitAndGetBlock(node.body, node.ctx->qualName->LastIden());
		body->scopeName = node.ctx->qualName->LastIden();

		StdString label = node.label ? node.label->iden : "";
		ITrStmtSPtr stmt{ new ITrForRange{ body->scopeName + "__loop_begin", label, node.idens, range, *reinterpret_cast<ITrBlockSPtr*>(&body), node.ctx->startIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstSwitchStmt& node)
	{
		StdVector<ITrSwitchCase> cases;
		usize size = node.cases.size();
		cases.reserve(size);
		for (usize i = 0; i < node.cases.size(); ++i)
		{
			AstSwitchCase& case_ = node.cases[i];
			ITrPatternSPtr pattern = VisitAndGetPattern(case_.pattern);
			ITrExprSPtr expr = VisitAndGetExpr(case_.expr);
			ITrBlockSPtr block = VisitAndGetBlock(case_.body, case_.body->ctx->qualName->LastIden());
			cases.emplace_back(pattern, expr, block);
		}

		ITrExprSPtr expr = VisitAndGetExpr(node.cond);
		StdString label = node.label ? node.label->iden : "";
		ITrStmtSPtr stmt{ new ITrSwitch{ node.ctx->qualName->LastIden(), label, expr, std::move(cases), node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstLabelStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrLabel{ node.iden, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstBreakStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrBreak{ node.iden, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstContinueStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrContinue{ node.iden, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstFallthroughStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrFallthrough{ node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstGotoStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrGoto{ node.iden, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstReturnStmt& node)
	{
		Walk(node);
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);

		ITrStmtSPtr stmt{ new ITrReturn{ expr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstThrowStmt& node)
	{
		// TODO: incorrect
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrStmtSPtr stmt{ new ITrReturn{ expr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstExprStmt& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrStmtSPtr stmt = *reinterpret_cast<ITrStmtSPtr*>(&expr);
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstDeferStmt& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrStmtSPtr stmt{ new ITrDefer{ false, expr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstErrDeferStmt& node)
	{
		// TODO: incorrect
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrStmtSPtr stmt{ new ITrDefer{ true, expr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstUnsafeStmt& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			if (astStmt->stmtKind == AstStmtKind::Decl)
				AstVisitor::Visit(astStmt);
			else
				stmts.push_back(VisitAndGetStmt(astStmt));
		}

		if (stmts.size() == 1)
		{
			ITrStmtSPtr stmt = stmts[0];
			if (stmt->stmtKind == ITrStmtKind::Block ||
				stmt->stmtKind == ITrStmtKind::Unsafe)
			{
				m_Stmt = stmt;
				return;
			}
		}

		ITrBlockSPtr block{ new ITrBlock{ node.ctx->qualName->LastIden(), std::move(stmts), node.ctx->startIdx, node.ctx->endIdx } };
		ITrStmtSPtr stmt{ new ITrUnsafe{ block, node.ctx->startIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstCompIfStmt& node)
	{
		const StdString& baseScopeName = node.ctx->qualName->LastIden();
		ITrBlockSPtr tBlock = VisitAndGetBlock(node.body, baseScopeName + "__tblock");
		ITrBlockSPtr fBlock = VisitAndGetBlock(node.elseBody, baseScopeName + "__fblock");
		ITrExprSPtr cond = VisitAndGetExpr(node.cond);

		ITrLocalVarSPtr decl;
		if (node.decl)
		{
			ITrStmtSPtr declStmt = VisitAndGetStmt(node.decl);
			decl = *reinterpret_cast<ITrLocalVarSPtr*>(&declStmt);
		}

		ITrStmtSPtr stmt{ new ITrIf{ true, decl, cond, tBlock, fBlock, node.ctx->startIdx } };
		stmt.reset(new ITrBlock{ node.ctx->qualName->LastIden(), { stmt }, stmt->startIdx, stmt->endIdx });
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstCompCondStmt& node)
	{
		const StdString& baseScopeName = node.ctx->qualName->LastIden();
		ITrBlockSPtr tBlock = VisitAndGetBlock(node.body, baseScopeName + "__tblock");
		ITrBlockSPtr fBlock = VisitAndGetBlock(node.elseBody, baseScopeName + "__fblock");
		
		OperatorKind op = TokenTypeToOperator(node.cmp.Type());
		ITrStmtSPtr stmt{ new ITrCompCond{ false, node.cond.Text(), op, node.val.Unsigned(), tBlock, fBlock, node.ctx->startIdx } };
	}

	void AstToITrLowering::Visit(AstCompDebugStmt& node)
	{
		const StdString& baseScopeName = node.ctx->qualName->LastIden();
		ITrBlockSPtr tBlock = VisitAndGetBlock(node.body, baseScopeName + "__tblock");
		ITrBlockSPtr fBlock = VisitAndGetBlock(node.elseBody, baseScopeName + "__fblock");

		OperatorKind op = TokenTypeToOperator(node.cmp.Type());
		ITrStmtSPtr stmt{ new ITrCompCond{ true, node.cond.Text(), op, node.val.Unsigned(), tBlock, fBlock, node.ctx->startIdx } };
	}

	void AstToITrLowering::Visit(AstAssignExpr& node)
	{
		OperatorKind op = TokenTypeToOperator(node.op);
		ITrExprSPtr lExpr = VisitAndGetExpr(node.lExpr);
		ITrExprSPtr rExpr = VisitAndGetExpr(node.rExpr);
		ITrExprSPtr expr{ new ITrAssign{ op, lExpr, rExpr } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstTernaryExpr& node)
	{
		ITrExprSPtr cond = VisitAndGetExpr(node.cond);
		ITrExprSPtr tExpr = VisitAndGetExpr(node.trueExpr);
		ITrExprSPtr fExpr = VisitAndGetExpr(node.falseExpr);
		ITrExprSPtr expr{ new ITrTernary{ cond, tExpr, fExpr } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstBinaryExpr& node)
	{
		OperatorKind op = TokenTypeToOperator(node.op);
		ITrExprSPtr lExpr = VisitAndGetExpr(node.lExpr);
		ITrExprSPtr rExpr = VisitAndGetExpr(node.rExpr);
		ITrExprSPtr expr{ new ITrBinary{ op, lExpr, rExpr } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstRangeExpr& node)
	{
		ITrExprSPtr lExpr = VisitAndGetExpr(node.lExpr);
		ITrExprSPtr rExpr = VisitAndGetExpr(node.rExpr);
		ITrExprSPtr expr{ new ITrRange { node.ctx->startIdx, lExpr, node.inclusive, rExpr, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstPostfixExpr& node)
	{
		OperatorKind op = TokenTypeToOperator(node.op, true, true);
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		expr = ITrExprSPtr{ new ITrUnary{ op, expr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstPrefixExpr& node)
	{
		OperatorKind op = TokenTypeToOperator(node.op, true, false);
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		expr = ITrExprSPtr{ new ITrUnary{ op, expr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstQualNameExpr& node)
	{
		Visit(*node.qualName);
		ITrExprSPtr expr{ new ITrQualNameExpr{ m_QualName } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstIndexSliceExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrExprSPtr index = VisitAndGetExpr(node.index);
		expr = ITrExprSPtr{ new ITrIndexSlice{ expr, index, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstFuncCallExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.func);
		StdVector<ITrArgSPtr> args = GetArgs(node.args);
		expr = ITrExprSPtr{ new ITrAmbiguousCall{ expr, std::move(args), node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstMemberAccessExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.caller);
		expr = ITrExprSPtr{ new ITrMemberAccess{ node.nullCoalesce, expr, node.iden, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstMethodCallExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.caller);
		Visit(*node.iden);
		ITrIdenSPtr iden = m_Idens.back();
		m_Idens.pop_back();
		StdVector<ITrArgSPtr> args = GetArgs(node.args);
		expr = ITrExprSPtr{ new ITrFuncCall{ expr, node.nullCoalesce, iden->iden, iden->generics, std::move(args), node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstTupleAccessExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		expr = ITrExprSPtr{ new ITrTupleAccess{ expr, node.nullCoalesce, node.index, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstLiteralExpr& node)
	{
		ITrExprSPtr expr{ new ITrLiteral{ node.literal } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstAggrInitExpr& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);
		StdVector<ITrArgSPtr> args = GetArgs(node.args);
		ITrExprSPtr defExpr = VisitAndGetExpr(node.defExpr);
		ITrExprSPtr expr{ new ITrAmbiguousAggrInit{ type, std::move(args), node.hasDefInit, defExpr, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstTupleInitExpr& node)
	{
		StdVector<ITrExprSPtr> exprs;
		usize size = node.exprs.size();
		exprs.reserve(size);
		for (AstExprSPtr expr : node.exprs)
		{
			exprs.push_back(VisitAndGetExpr(expr));
		}

		ITrExprSPtr expr{ new ITrTupleInit{ std::move(exprs), node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstArrayInitExpr& node)
	{
		StdVector<ITrExprSPtr> exprs;
		usize size = node.exprs.size();
		exprs.reserve(size);
		for (AstExprSPtr expr : node.exprs)
		{
			exprs.push_back(VisitAndGetExpr(expr));
		}

		ITrExprSPtr expr{ new ITrArrayInit{ std::move(exprs), node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstCastExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrTypeSPtr type = VisitAndGetType(node.type);

		ITrCastKind castKind;
		switch (node.castType)
		{
		case TokenType::AsQuestion: castKind = ITrCastKind::SafeCast; break;
		case TokenType::AsExclaim: castKind = ITrCastKind::NullPanicCast; break;
		default: castKind = ITrCastKind::Cast; break;
		}

		expr = ITrExprSPtr{ new ITrCast{ castKind, expr, type } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstTransmuteExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrTypeSPtr type = VisitAndGetType(node.type);
		expr = ITrExprSPtr{ new ITrCast{ ITrCastKind::Transmute, expr, type } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstMoveExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		expr = ITrExprSPtr{ new ITrMove{ expr, node.ctx->startIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstBracketExpr& node)
	{
		AstVisitor::Visit(node.expr);
	}

	void AstToITrLowering::Visit(AstBlockExpr& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			if (astStmt->stmtKind == AstStmtKind::Decl)
				AstVisitor::Visit(astStmt);
			else
				stmts.push_back(VisitAndGetStmt(astStmt));
		}

		ITrExprSPtr expr{ new ITrBlockExpr{ node.ctx->qualName->LastIden(), std::move(stmts), node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstUnsafeExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		expr = ITrExprSPtr{ new ITrUnsafeExpr{ expr, node.ctx->startIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstCommaExpr& node)
	{
		StdVector<ITrExprSPtr> exprs;
		usize size = node.exprs.size();
		exprs.reserve(size);
		for (AstExprSPtr expr : node.exprs)
		{
			exprs.push_back(VisitAndGetExpr(expr));
		}

		ITrExprSPtr expr{ new ITrComma{ std::move(exprs), node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstClosureExpr& node)
	{
		StdVector<ITrParamSPtr> params = GetParams(node.params);
		ITrTypeSPtr retType = VisitAndGetType(node.ret);

		ITrDefSPtr def{ new ITrFunc{ nullptr, nullptr, node.ctx->qualName, std::move(params), nullptr, retType, ITrFuncKind::Closure, false, false, node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;

		PushDefFrame(def);
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		StdVector<ITrDefSPtr> defs = PopDefFrame();
		
		StdVector<ITrStmtSPtr> stmts;
		if (retType)
			stmts.emplace_back(new ITrReturn{ expr, node.ctx->startIdx, node.ctx->endIdx });
		else
			stmts.push_back(expr);
		
		ITrBodySPtr body{ new ITrBody{ std::move(defs), std::move(stmts) } };

		ITrModule& mod = g_Ctx.activeModule->itrModule;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);

		ITrExprSPtr closure{ new ITrClosure{ def, node.ctx->startIdx, node.ctx->endIdx } };
		m_Expr = closure;
	}

	void AstToITrLowering::Visit(AstIsExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		ITrTypeSPtr type = VisitAndGetType(node.type);
		expr = ITrExprSPtr{ new ITrIs{ expr, type } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstTryExpr& node)
	{
		ITrTryKind kind = node.tryKind == TokenType::Try ? ITrTryKind::Propagating :
			(node.tryKind == TokenType::TryNullable ? ITrTryKind::Nullable : ITrTryKind::Panic);
		ITrExprSPtr expr = VisitAndGetExpr(node.call);
		expr = ITrExprSPtr{ new ITrTry{ kind, expr, node.ctx->startIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstSpecKwExpr& node)
	{
		ITrExprSPtr expr{ new ITrSpecKw{ node.specKw, node.ctx->startIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstCompRunExpr& node)
	{
		ITrExprSPtr expr = VisitAndGetExpr(node.expr);
		expr = ITrExprSPtr{ new ITrCompRun{ expr, node.ctx->startIdx } };
		m_Expr = expr;
	}

	void AstToITrLowering::Visit(AstBuiltinType& node)
	{
		BuiltinTypeKind builtin;
		switch (node.type)
		{
		case TokenType::Bool:  builtin = BuiltinTypeKind::Bool;  break;
		case TokenType::Char:  builtin = BuiltinTypeKind::Char;  break;
		case TokenType::I8:    builtin = BuiltinTypeKind::I8;    break;
		case TokenType::I16:   builtin = BuiltinTypeKind::I16;   break;
		case TokenType::I32:   builtin = BuiltinTypeKind::I32;   break;
		case TokenType::I64:   builtin = BuiltinTypeKind::I64;   break;
		case TokenType::I128:  builtin = BuiltinTypeKind::I128;  break;
		case TokenType::ISize: builtin = BuiltinTypeKind::ISize; break;
		case TokenType::U8:    builtin = BuiltinTypeKind::U8;    break;
		case TokenType::U16:   builtin = BuiltinTypeKind::U16;   break;
		case TokenType::U32:   builtin = BuiltinTypeKind::U32;   break;
		case TokenType::U64:   builtin = BuiltinTypeKind::U64;   break;
		case TokenType::U128:  builtin = BuiltinTypeKind::U128;  break;
		case TokenType::USize: builtin = BuiltinTypeKind::USize; break;
		case TokenType::F16:   builtin = BuiltinTypeKind::F16;   break;
		case TokenType::F32:   builtin = BuiltinTypeKind::F32;   break;
		case TokenType::F64:   builtin = BuiltinTypeKind::F64;   break;
		case TokenType::F128:  builtin = BuiltinTypeKind::F128;  break;
		default:               builtin = BuiltinTypeKind::Count; break;
		}

		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		TypeHandle handle = g_TypeReg.Builtin(mod, builtin);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {}, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstIdentifierType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;

		Visit(*node.qualName);
		QualNameSPtr qualName = node.qualName->ctx->qualName;

		TypeHandle handle = g_TypeReg.Iden(mod, qualName);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {}, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstPointerType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		ITrTypeSPtr subType = VisitAndGetType(node.subType);
		TypeHandle handle = g_TypeReg.Ptr(mod, subType->handle);

		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType }, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstReferenceType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		ITrTypeSPtr subType = VisitAndGetType(node.subType);
		TypeHandle handle = g_TypeReg.Ref(mod, subType->handle);

		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType }, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstArrayType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		
		ITrTypeSPtr subType = VisitAndGetType(node.subType);
		ITrExprSPtr expr = VisitAndGetExpr(node.arraySize);
		TypeHandle handle = g_TypeReg.Array(mod, subType->handle, expr);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType }, expr, node.ctx->startIdx, node.ctx->endIdx }};
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstSliceType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		ITrTypeSPtr subType = VisitAndGetType(node.subType);
		TypeHandle handle = g_TypeReg.Slice(mod, subType->handle);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType }, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstTupleType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		StdVector<TypeHandle> subTypesHandles;
		StdVector<ITrTypeSPtr> subTypes;
		usize size = node.subTypes.size();
		subTypesHandles.reserve(size);
		subTypes.reserve(size);
		for (AstTypeSPtr astType : node.subTypes)
		{
			ITrTypeSPtr subType = VisitAndGetType(astType);
			subTypes.push_back(subType);
			subTypesHandles.push_back(subType->handle);
		}

		TypeHandle handle = g_TypeReg.Tuple(mod, subTypesHandles);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, std::move(subTypes), nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstOptionalType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod mod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			mod = TypeMod::Mut;
		
		ITrTypeSPtr subType = VisitAndGetType(node.subType);
		TypeHandle handle = g_TypeReg.Opt(mod, subType->handle);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType }, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstInlineStructType& node)
	{
		ITrModule& mod = g_Ctx.activeModule->itrModule;
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdVector<StdString>, AstTypeSPtr>& astMember : node.members)
		{
			ITrTypeSPtr type = VisitAndGetType(astMember.second);

			for (StdString& iden : astMember.first)
			{
				QualNameSPtr qualName = node.ctx->qualName->Append(iden);
				ITrDefSPtr member { new ITrVar{ nullptr, qualName, type, nullptr, false, node.ctx->startIdx, node.ctx->endIdx } };
				member->ptr = member;
				members.push_back(member);
				member->fileName = m_TreeFilename;
				mod.AddDefinition(member);
			}
		}

		ITrBodySPtr body{ new ITrBody{ std::move(members), {} } };
		ITrDefSPtr def{ new ITrStruct{ nullptr, nullptr, node.ctx->qualName, false, node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);
		
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod tmod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			tmod = TypeMod::Mut;

		TypeHandle handle = g_TypeReg.Iden(tmod, node.ctx->qualName);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {}, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstInlineEnumType& node)
	{
		QualNameSPtr qualName = node.ctx->qualName;
		ITrModule& mod = g_Ctx.activeModule->itrModule;
		
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdString, AstExprSPtr>& astMember : node.members)
		{
			ITrExprSPtr expr = VisitAndGetExpr(astMember.second);
			ITrDefSPtr member{ new ITrValEnumMember{ qualName, astMember.first, expr, node.ctx->startIdx, node.ctx->endIdx } };
			member->ptr = member;
			members.push_back(member);
			member->fileName = m_TreeFilename;
			mod.AddDefinition(member);
		}

		ITrBodySPtr body{ new ITrBody{ std::move(members), {} } };
		ITrDefSPtr def{ new ITrStruct{ nullptr, nullptr, qualName, false, node.ctx->startIdx, node.ctx->endIdx } };
		def->ptr = def;
		def->fileName = m_TreeFilename;
		mod.AddDefinition(def, body);

		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		TypeMod tmod = TypeMod::None;
		if (attribs && ENUM_IS_SET(attribs->attribs, Attribute::Mut))
			tmod = TypeMod::Mut;

		TypeHandle handle = g_TypeReg.Iden(tmod, qualName);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {}, nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstCompoundInterfaceType& node)
	{
		ITrAttribsSPtr attribs = VisitAndGetAttribs(node.attribs);
		
		StdVector<TypeHandle> subTypesHandles;
		StdVector<ITrTypeSPtr> subTypes;
		usize size = node.interfaces.size();
		subTypesHandles.reserve(size);
		subTypes.reserve(size);
		for (AstTypeSPtr astType : node.interfaces)
		{
			ITrTypeSPtr subType = VisitAndGetType(astType);
			subTypes.push_back(subType);
			subTypesHandles.push_back(subType->handle);
		}

		TypeHandle handle;
		if (subTypesHandles.size() == 1)
			handle = subTypesHandles[0];
		else
			handle = g_TypeReg.Compound(TypeMod::None, subTypesHandles);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, std::move(subTypes), nullptr, node.ctx->startIdx, node.ctx->endIdx } };
		m_Type = type;
	}

	void AstToITrLowering::Visit(AstPlaceholderPattern& node)
	{
		ITrPatternSPtr pattern{ new ITrPlaceholderPattern{ false, node.ctx->startIdx } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstWildcardPattern& node)
	{
		ITrPatternSPtr pattern{ new ITrPlaceholderPattern{ true, node.ctx->startIdx } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstValueBindPattern& node)
	{
		ITrPatternSPtr subPattern = VisitAndGetPattern(node.subPattern);
		ITrPatternSPtr pattern{ new ITrValueBindPattern{ node.iden, std::move(subPattern), node.ctx->startIdx, node.ctx->endIdx } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstLiteralPattern& node)
	{
		ITrPatternSPtr pattern{ new ITrLiteralPattern{ node.literal } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstRangePattern& node)
	{
		ITrPatternSPtr pattern{ new ITrRangePattern{ node.inclusive, node.from, node.to } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstTuplePattern& node)
	{
		StdVector<ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.reserve(size);
		for (AstPatternSPtr astPattern : node.subPatterns)
		{
			subPatterns.push_back(VisitAndGetPattern(astPattern));
		}

		ITrPatternSPtr pattern{ new ITrTuplePattern{ std::move(subPatterns), node.ctx->startIdx, node.ctx->endIdx } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstEnumPattern& node)
	{
		QualNameSPtr qualName = node.qualName->ctx->qualName;
		if (node.subPatterns.empty())
		{
			ITrPatternSPtr pattern{ new ITrValueEnumPattern{ qualName, node.ctx->startIdx, node.ctx->endIdx } };
			m_Pattern = pattern;
		}
		else
		{
			StdVector<ITrPatternSPtr> subPatterns;
			usize size = node.subPatterns.size();
			subPatterns.reserve(size);
			for (AstPatternSPtr astPattern : node.subPatterns)
			{
				subPatterns.push_back(VisitAndGetPattern(astPattern));
			}

			ITrPatternSPtr pattern{ new ITrAdtTupleEnumPattern{ qualName, std::move(subPatterns), node.ctx->startIdx, node.ctx->endIdx } };
			m_Pattern = pattern;
		}
	}

	void AstToITrLowering::Visit(AstAggrPattern& node)
	{
		QualNameSPtr qualName = node.qualName->ctx->qualName;
		
		StdPairVector<StdString, ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.reserve(size);
		for (StdPair<StdString, AstPatternSPtr>& astPattern : node.subPatterns)
		{
			subPatterns.emplace_back(astPattern.first, VisitAndGetPattern(astPattern.second));
		}

		if (node.qualName->hasColonColon && node.qualName->idens.size() == 1)
		{
			ITrPatternSPtr pattern{ new ITrAdtAggrEnumPattern{ qualName, std::move(subPatterns), node.ctx->startIdx, node.ctx->endIdx } };
			m_Pattern = pattern;
		}
		else
		{
			ITrPatternSPtr pattern{ new ITrAmbiguousAggrPattern{ qualName, std::move(subPatterns), node.ctx->startIdx, node.ctx->endIdx } };
			m_Pattern = pattern;
		}
	}

	void AstToITrLowering::Visit(AstSlicePattern& node)
	{
		StdVector<ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.reserve(size);
		for (AstPatternSPtr astPattern : node.subPatterns)
		{
			subPatterns.emplace_back(VisitAndGetPattern(astPattern));
		}

		ITrPatternSPtr pattern{ new ITrSlicePattern{ std::move(subPatterns), node.ctx->startIdx, node.ctx->endIdx } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstEitherPattern& node)
	{
		StdVector<ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.reserve(size);
		for (AstPatternSPtr astPattern : node.subPatterns)
		{
			subPatterns.emplace_back(VisitAndGetPattern(astPattern));
		}

		ITrPatternSPtr pattern{ new ITrEitherPattern{ std::move(subPatterns) } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstTypePattern& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);
		ITrPatternSPtr pattern{ new ITrTypePattern{ type, node.ctx->startIdx } };
		m_Pattern = pattern;
	}

	void AstToITrLowering::Visit(AstAttribs& node)
	{
		Visibility vis = Visibility::Private;
		if (node.visibility)
		{
			StdString& kind = node.visibility->kind;
			if (kind == "module")
				vis = Visibility::Module;
			else if (kind == "package")
				vis = Visibility::Package;
			else if (kind == "dynlib")
				vis = Visibility::Dynlib;
			else
				vis = Visibility::Public;
		}

		Attribute attribs = Attribute::None;
		for (AstSimpleAttribSPtr simpleAttrib : node.simpleAttribs)
		{
			Attribute tmp;
			switch (simpleAttrib->attrib)
			{
			case TokenType::Const: tmp = Attribute::Const; break;
			case TokenType::Mut: tmp = Attribute::Mut; break;
			case TokenType::Static: tmp = Attribute::Static; break;
			case TokenType::Comptime: tmp = Attribute::Comptime; break;
			case TokenType::Lazy: tmp = Attribute::Lazy; break;
			case TokenType::Move: tmp = Attribute::Move; break;
			default: tmp = Attribute::None;
			}

			if (ENUM_IS_SET(attribs, tmp))
			{
				Span span = g_SpanManager.GetSpan(node.ctx->startIdx);
				StdString attribName = ToString(tmp);
				const char* pAttribName = attribName.c_str();
				g_ErrorSystem.Error(span, "Duplicate occurence of '%s'", pAttribName);
			}

			attribs |= tmp;
		}

		StdVector<ITrAtAttribSPtr> atAttribs;
		for (AstCompAttribSPtr compAttrib : node.compAttribs)
		{
			StdVector<ITrArgSPtr> args = GetArgs(compAttrib->args);
			ITrAtAttribSPtr attr{ new ITrAtAttrib { true, compAttrib->iden, std::move(args), compAttrib->ctx->startIdx, compAttrib->ctx->endIdx } };
			atAttribs.push_back(attr);
		}

		for (AstUserAttribSPtr userAttrib : node.userAttribs)
		{
			StdVector<ITrArgSPtr> args = GetArgs(userAttrib->args);
			ITrAtAttribSPtr attr{ new ITrAtAttrib { false, userAttrib->iden, std::move(args), userAttrib->ctx->startIdx, userAttrib->ctx->endIdx } };
			atAttribs.push_back(attr);
		}

		m_Attribs.reset(new ITrAttribs{ vis, attribs, std::move(atAttribs), node.ctx->startIdx, node.ctx->endIdx });
	}

	void AstToITrLowering::Visit(AstMacroLoopStmt& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			if (astStmt->stmtKind == AstStmtKind::Decl)
				AstVisitor::Visit(astStmt);
			else
				stmts.push_back(VisitAndGetStmt(astStmt));
		}

		ITrStmtSPtr stmt{ new ITrBlock{ "", std::move(stmts), node.ctx->startIdx, node.ctx->endIdx } };
		m_Stmt = stmt;
	}

	void AstToITrLowering::Visit(AstGenericDecl& node)
	{
		m_GenDecl = ITrGenDeclSPtr{ new ITrGenDecl{} };
		Walk(node);
	}

	void AstToITrLowering::Visit(AstGenericTypeParam& node)
	{
		ITrTypeSPtr def = VisitAndGetType(node.defType);

		ITrGenParamSPtr param{ new ITrGenTypeParam{ node.iden, def, node.ctx->startIdx, node.ctx->endIdx } };
		m_GenDecl->params.push_back(param);

		if (!node.implTypes.empty())
		{
			TypeHandle idenType = g_TypeReg.Iden(TypeMod::None, QualName::Create(node.iden));
			ITrTypeSPtr toBindType{ new ITrType{ nullptr, idenType, {}, nullptr, u64(-1), u64(-1) } };

			for (AstIdentifierTypeSPtr implType : node.implTypes)
			{
				ITrTypeSPtr type = VisitAndGetType(implType);
				
				StdVector<ITrGenAssocBound> assocBounds;
				ITrGenBoundTypeSPtr boundType{ new ITrGenBoundType{ type, std::move(assocBounds), u64(-1), u64(-1) } };

				ITrGenTypeBoundSPtr bound{ new ITrGenTypeBound{ toBindType, boundType, u64(-1), u64(-1) } };
				m_GenDecl->bounds.push_back(bound);
			}
		}
		
	}

	void AstToITrLowering::Visit(AstGenericValueParam& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);
		ITrExprSPtr def = VisitAndGetExpr(node.defExpr);
		ITrGenParamSPtr param{ new ITrGenValParam{ node.iden, type, def, u64(-1), u64(-1) } };
		m_GenDecl->params.push_back(param);
	}

	void AstToITrLowering::Visit(AstGenericTypeBound& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);
		ITrGenBoundTypeSPtr boundType = VisitAndGetGenBoundType(node.bound);
		ITrGenTypeBoundSPtr bound{ new ITrGenTypeBound{ type, boundType, u64(-1), u64(-1) } };
		m_GenDecl->bounds.push_back(bound);
	}

	void AstToITrLowering::Visit(AstGenericBoundType& node)
	{
		ITrTypeSPtr type = VisitAndGetType(node.type);

		StdVector<ITrGenAssocBound> assocBounds;
		for (AstGenericAssocTypeBound& assocBound : node.assocBounds)
		{
			ITrGenBoundTypeSPtr boundType = VisitAndGetGenBoundType(assocBound.type);
			ITrGenAssocBound bound{ assocBound.iden, boundType, u64(-1), u64(-1) };
			assocBounds.push_back(bound);
		}

		ITrGenBoundTypeSPtr boundType = ITrGenBoundTypeSPtr{ new ITrGenBoundType{ type, std::move(assocBounds), u64(-1), u64(-1) } };
		m_BoundTypes.push(boundType);
	}

	void AstToITrLowering::Visit(AstDeclSPtr& node)
	{
		AstVisitor::Visit(node);
	}

	StdVector<ITrParamSPtr> AstToITrLowering::GetParams(StdVector<AstParamSPtr>& astParams)
	{
		StdVector<ITrParamSPtr> params;
		usize size = astParams.size();
		params.reserve(size);
		for (AstParamSPtr astParam : astParams)
		{
			ITrTypeSPtr type = VisitAndGetType(astParam->type);
			
			for (AstParamVarSPtr paramVar : astParam->vars)
			{
				ITrAttribsSPtr attribs = VisitAndGetAttribs(paramVar->attribs);
				ITrParamSPtr param{ new ITrParam{ attribs, paramVar->iden, type, astParam->ctx->startIdx, astParam->ctx->endIdx } };
				params.push_back(param);
			}
		}
		return params;
	}

	StdVector<ITrArgSPtr> AstToITrLowering::GetArgs(StdVector<AstArgSPtr>& astArgs)
	{
		StdVector<ITrArgSPtr> args;
		usize size = astArgs.size();
		args.reserve(size);
		for (AstArgSPtr astArg : astArgs)
		{
			ITrExprSPtr expr = VisitAndGetExpr(astArg->expr);
			const StdString& iden = astArg->iden;
			ITrArgSPtr arg{ new ITrArg{ iden, expr, astArg->ctx->startIdx } };
			args.push_back(arg);
		}
		std::reverse(args.begin(), args.end());
		return args;
	}

	void AstToITrLowering::HandleGenerics(ITrGenDeclSPtr genDecl, QualNameSPtr qualName)
	{
		// Update generic type argument names
		for (usize i = 0; i < genDecl->params.size(); ++i)
		{
			ITrGenParamSPtr param = genDecl->params[i];
			IdenGeneric& idenGen = qualName->Generics()[i];
			if (param->isType)
			{
				ITrGenTypeParam& genParam = *reinterpret_cast<ITrGenTypeParam*>(param.get());
				idenGen.iden = genParam.iden;
				idenGen.type = g_TypeReg.Generic(TypeMod::None, u16(i));
			}
			else
			{
				ITrGenValParam& genParam = *reinterpret_cast<ITrGenValParam*>(param.get());
				idenGen.iden = genParam.iden;
				idenGen.type = genParam.type->handle;
			}
		}
	}

	void AstToITrLowering::AddMethodReceiverToParams(AstMethodDecl& node, StdVector<ITrParamSPtr>& params)
	{
		ITrTypeSPtr recType;
		if (m_ITrImplType)
			recType = m_ITrImplType;
		else
			recType.reset(new ITrType { nullptr, m_ImplType, {}, nullptr, u64(-1), u64(-1) });
		switch (node.rec)
		{
		case AstMethodReceiverKind::Move:
		{
			ITrAttribsSPtr attribs{ new ITrAttribs{ Visibility::Private, Attribute::Move, {}, u64(-1), u64(-1) } };
			ITrParamSPtr recParam{ new ITrParam{ attribs, "self", recType, u64(-1), u64(-1) } };
			params.insert(params.begin(), recParam);
			break;
		}
		case AstMethodReceiverKind::MutRef:
		{
			TypeHandle tmp = g_TypeReg.Mod(TypeMod::Mut, recType->handle);
			if (m_ITrImplType)
			{
				recType = ITrTypeSPtr{ new ITrType { *m_ITrImplType } };
				recType->handle = tmp;
			}
			else
			{
				recType = ITrTypeSPtr{ new ITrType { nullptr, tmp, {}, nullptr, u64(-1), u64(-1) } };
			}
			// fallthrough
		}
		case AstMethodReceiverKind::Ref:
		{
			TypeHandle tmp = g_TypeReg.Ref(TypeMod::None, recType->handle);
			recType = ITrTypeSPtr{ new ITrType { nullptr, tmp, { recType }, nullptr, u64(-1), u64(-1) } };
			// fallthrough
		}
		case AstMethodReceiverKind::Value:
		{
			ITrParamSPtr recParam{ new ITrParam{ nullptr, "self", recType, u64(-1), u64(-1) } };
			params.insert(params.begin(), recParam);
			break;
		}
		default:;
		}
	}

	void AstToITrLowering::GetNamedReturns(ITrTypeSPtr& retType, StdVector<ITrStmtSPtr> stmts, StdPairVector<StdVector<StdString>, AstTypeSPtr>& astNamedRets)
	{
		StdVector<TypeHandle> subTypesHandles;
		StdVector<ITrTypeSPtr> subTypes;
		StdVector<ITrExprSPtr> retSubExprs;
		for (StdPair<StdVector<StdString>, AstTypeSPtr>& namedRet : astNamedRets)
		{
			ITrTypeSPtr tmp = VisitAndGetType(namedRet.second);

			for (StdString& name : namedRet.first)
			{
				subTypesHandles.push_back(tmp->handle);
				subTypes.push_back(tmp);

				stmts.emplace_back(new ITrLocalVar{ nullptr,  { name }, tmp, nullptr, u64(-1), u64(-1) });
				ITrQualNameSPtr itrQualName{ new ITrQualName{ QualName::Create(name), nullptr, StdVector<ITrIdenSPtr>{}, false } };
				retSubExprs.emplace_back(new ITrQualNameExpr{ itrQualName });
			}
		}

		m_NamedRet = ITrExprSPtr{ new ITrTupleInit{ std::move(retSubExprs), u64(-1), u64(-1) } };

		TypeHandle handle = g_TypeReg.Tuple(TypeMod::None, subTypesHandles);
		retType = ITrTypeSPtr{ new ITrType{ nullptr, handle, std::move(subTypes), nullptr, u64(-1), u64(-1) } };
	}

	void AstToITrLowering::HandleWhereClause(AstGenericWhereClause& clause, ITrGenDeclSPtr genDecl)
	{
		m_GenDecl = genDecl;
		for (AstGenericTypeBoundSPtr astBound : clause.bounds)
		{
			Visit(*astBound);
		}
		m_GenDecl = nullptr;
	}

	void AstToITrLowering::PushDefFrame(ITrDefSPtr def)
	{
		m_Defs.emplace(def, StdVector<ITrDefSPtr>{});
	}

	void AstToITrLowering::PushDef(ITrDefSPtr def)
	{
		if (!m_Defs.empty())
		{
			def->impl = m_Defs.top().first;
			m_Defs.top().second.push_back(def);
		}
	}

	StdVector<ITrDefSPtr> AstToITrLowering::PopDefFrame()
	{
		StdVector<ITrDefSPtr> tmp = m_Defs.top().second;
		m_Defs.pop();
		return tmp;
	}

	bool AstToITrLowering::IsModDef()
	{
		return m_Defs.empty();
	}

	ITrStmtSPtr AstToITrLowering::VisitAndGetStmt(AstStmtSPtr stmt)
	{
		if (!stmt)
			return nullptr;
		AstVisitor::Visit(stmt);
		return m_Stmt;
	}

	ITrBlockSPtr AstToITrLowering::VisitAndGetBlock(AstStmtSPtr stmt, const StdString& scopeName)
	{
		if (!stmt)
			return nullptr;
		AstVisitor::Visit(stmt);

		if (m_Stmt->stmtKind != ITrStmtKind::Block)
			return ITrBlockSPtr{ new ITrBlock { "", { m_Stmt }, m_Stmt->startIdx, m_Stmt->endIdx } };
		else
			return *reinterpret_cast<ITrBlockSPtr*>(&m_Stmt);
	}

	ITrExprSPtr AstToITrLowering::VisitAndGetExpr(AstExprSPtr expr)
	{
		if (!expr)
			return nullptr;
		AstVisitor::Visit(expr);
		return m_Expr;
	}

	ITrTypeSPtr AstToITrLowering::VisitAndGetType(AstTypeSPtr type)
	{
		if (!type)
			return nullptr;
		AstVisitor::Visit(type);
		return m_Type;
	}

	ITrPatternSPtr AstToITrLowering::VisitAndGetPattern(AstPatternSPtr pattern)
	{
		if (!pattern)
			return nullptr;
		AstVisitor::Visit(pattern);
		return m_Pattern;
	}

	ITrAttribsSPtr AstToITrLowering::VisitAndGetAttribs(AstAttribsSPtr attribs)
	{
		if (!attribs)
			return nullptr;
		Visit(*attribs);
		return m_Attribs;
	}

	ITrGenDeclSPtr AstToITrLowering::VisitAndGetGenDecl(AstGenericDeclSPtr genDecl)
	{
		if (!genDecl)
			return nullptr;
		Visit(*genDecl);
		return m_GenDecl;
	}

	ITrGenBoundTypeSPtr AstToITrLowering::VisitAndGetGenBoundType(AstGenericBoundTypeSPtr bound)
	{
		if (!bound)
			return nullptr;
		Visit(*bound);
		ITrGenBoundTypeSPtr tmp = m_BoundTypes.top();
		m_BoundTypes.pop();
		return tmp;
	}
}
