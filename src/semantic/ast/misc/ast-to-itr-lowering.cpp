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
	AstToITrLowering::AstToITrLowering(Context* pCtx)
		: AstSemanticPass("ast to itr lowering", pCtx)
		, m_ImplType(TypeHandle(-1))
		, m_InFunc(false)
	{
	}

	void AstToITrLowering::Visit(AstQualName& node)
	{
		StdVector<IdenSPtr> tmpIdens;
		QualNameSPtr tmpQual;
		
		for (AstQualIdenSPtr astIden : node.idens)
		{
			if (astIden->qualIdenKind == AstQualIdenKind::Identifier)
			{
				tmpIdens.push_back(astIden->ctx->iden);
			}
			else
			{
				if (!tmpIdens.empty())
					tmpQual = QualName::Create(tmpQual, tmpIdens);
				
				AstVisitor::Visit(astIden);
				ITrTypeSPtr type = PopType();
				
				QualNameSPtr qualName = QualName::Create(tmpQual, tmpIdens);
				TypeDisambiguationSPtr disambiguation = TypeDisambiguation::Create(qualName, type->handle);
				tmpQual = QualName::Create(disambiguation);
			}
		}

		if (!tmpIdens.empty())
			tmpQual = QualName::Create(tmpQual, tmpIdens);

		node.ctx->qualName = tmpQual;
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
	}

	void AstToITrLowering::Visit(AstBenchmarkDecl& node)
	{
	}

	void AstToITrLowering::Visit(AstStructDecl& node)
	{
		PushDefFrame();
		Walk(node);
		StdVector<ITrDefSPtr> subDefs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(subDefs), {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		
		ITrDefSPtr def{ new ITrStruct{ attribs, genDecl, node.ctx->qualName, IsModDef() } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstUnionDecl& node)
	{
		PushDefFrame();
		Walk(node);
		StdVector<ITrDefSPtr> subDefs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(subDefs), {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		ITrDefSPtr def{ new ITrUnion{ attribs, genDecl, node.ctx->qualName, IsModDef() } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstValueEnumDecl& node)
	{
		QualNameSPtr enumQualName = node.ctx->qualName;
		ITrModule& mod = m_pCtx->activeModule->itrModule;
		
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdString, AstExprSPtr> astMember : node.members)
		{
			ITrExprSPtr expr;
			if (astMember.second)
			{
				AstVisitor::Visit(astMember.second);
				expr = PopExpr();
				expr->astNode = astMember.second;
			}

			ITrDefSPtr member{ new ITrValEnumMember{ enumQualName, Iden::Create(astMember.first), expr } };
			member->fileName = m_TreeFilename;
			mod.AddDefinition(member);
			members.push_back(member);
		}

		ITrBodySPtr body{ new ITrBody{ std::move(members) , {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrDefSPtr def{ new ITrValEnum{ attribs, enumQualName, IsModDef() } };
		node.defItr = def;

		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstAdtEnumDecl& node)
	{
		QualNameSPtr enumQualName = node.ctx->qualName;
		ITrModule& mod = m_pCtx->activeModule->itrModule;

		PushDefFrame();
		
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdString, AstTypeSPtr> astMember : node.members)
		{
			ITrTypeSPtr type;
			if (astMember.second)
			{
				AstVisitor::Visit(astMember.second);
				type = PopType();
				type->astNode = astMember.second;
			}
			
			ITrDefSPtr member{ new ITrAdtEnumMember{ enumQualName, Iden::Create(astMember.first), type } };
			member->fileName = m_TreeFilename;
			mod.AddDefinition(member);
			members.push_back(member);
		}

		StdVector<ITrDefSPtr> tmp = PopDefFrame();
		members.insert(members.end(), tmp.begin(), tmp.end());

		ITrBodySPtr body{ new ITrBody{ std::move(members) , {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		ITrDefSPtr def{ new ITrAdtEnum{ attribs, genDecl, enumQualName, IsModDef() } };
		node.defItr = def;

		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstMarkerInterfaceDecl& node)
	{
		m_ImplType = m_pCtx->typeReg.Iden(TypeMod::None, node.ctx->qualName);
		Walk(node);
		m_ImplType = TypeHandle(-1);
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrDefSPtr def{ new ITrMarkerInterface{ attribs, node.ctx->qualName } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstWeakInterfaceDecl& node)
	{
		PushDefFrame();
		m_ImplType = m_pCtx->typeReg.Iden(TypeMod::None, node.ctx->qualName);
		Walk(node);
		m_ImplType = TypeHandle(-1);
		StdVector<ITrDefSPtr> subDefs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(subDefs), {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		//if (node.generics)
		//{
		//	genDecl = PopGenDecl();
		//	genDecl->astNode = node.generics;
		//}
		ITrDefSPtr def{ new ITrWeakInterface{ attribs, genDecl, node.ctx->qualName } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstStrongInterfaceDecl& node)
	{
		PushDefFrame();
		m_ImplType = m_pCtx->typeReg.Iden(TypeMod::None, node.ctx->qualName);
		Walk(node);
		m_ImplType = TypeHandle(-1);
		StdVector<ITrDefSPtr> subDefs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(subDefs), {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		ITrDefSPtr def{ new ITrStrongInterface{ attribs, genDecl, node.ctx->qualName } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstTypeAliasDecl& node)
	{
		PushDefFrame();
		Walk(node);
		StdVector<ITrDefSPtr> subDefs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(subDefs), {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}

		ITrTypeSPtr type = node.type ? PopType() : nullptr;
		ITrDefSPtr def{ new ITrTypealias{ attribs, genDecl, node.ctx->qualName, type, IsModDef() } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstTypeDefDecl& node)
	{
		PushDefFrame();
		Walk(node);
		StdVector<ITrDefSPtr> subDefs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(subDefs), {} } };

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}

		ITrTypeSPtr type = PopType();
		ITrDefSPtr def{ new ITrTypedef{ attribs, genDecl, node.ctx->qualName, type, IsModDef() } };
		node.defItr = def;

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstVarDecl& node)
	{
		Walk(node);
		if (m_InFunc)
		{
			StdVector<IdenSPtr> idens;
			for (const StdString& iden : node.idens)
			{
				idens.push_back(Iden::Create(iden));
			}

			ITrTypeSPtr type;
			if (node.type)
			{
				type = PopType();
				type->astNode = node.type;
			}

			ITrExprSPtr expr;
			if (node.expr)
			{
				expr = PopExpr();
				expr->astNode = node.expr;
			}

			ITrAttribsSPtr attribs;
			if (node.attribs)
			{
				attribs = PopAttribs();
				attribs->astNode = node.attribs;
			}
			ITrStmtSPtr stmt{ new ITrLocalVar{ attribs, std::move(idens), type, expr } };
			node.itr = stmt;
			m_Stmts.push(stmt);
		}
		else
		{
			ITrAttribsSPtr attribs;
			if (node.attribs)
			{
				attribs = PopAttribs();
				attribs->astNode = node.attribs;
			}
			
			ITrTypeSPtr type = PopType();
			type->astNode = node.type;
			
			ITrModule& mod = m_pCtx->activeModule->itrModule;
			for (StdString& iden : node.idens)
			{
				QualNameSPtr qualName = QualName::Create(node.ctx->scope, iden);
				ITrDefSPtr def{ new ITrVar{ attribs, qualName, type, IsModDef() } };
				def->fileName = m_TreeFilename;
				def->astNode = m_DeclNode;
				mod.AddDefinition(def);
				PushDef(def);
			}
		}
	}

	void AstToITrLowering::Visit(AstFuncDecl& node)
	{
		// TODO: throws
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}

		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			Visit(*node.generics);
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		
		StdVector<ITrParamSPtr> params = GetParams(node.params);

		StdVector<ITrStmtSPtr> stmts;
		ITrTypeSPtr retType;
		if (!node.namedRet.empty())
		{
			GetNamedReturns(retType, stmts, node.namedRet);
		}
		else if (node.retType)
		{
			AstVisitor::Visit(node.retType);
			retType = PopType();
			retType->astNode = node.retType;
		}
		
		ITrDefSPtr def{ new ITrFunc{ attribs, genDecl, node.ctx->qualName, std::move(params), retType, ITrFuncKind::Func, IsModDef() } };

		PushDefFrame();
		for (AstStmtSPtr astStmt : node.stmts)
		{
			AstVisitor::Visit(astStmt);
			ITrStmtSPtr stmt = PopStmt();
			stmt->astNode = astStmt;
			stmts.push_back(stmt);
		}
		StdVector<ITrDefSPtr> defs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(defs), std::move(stmts) } };

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstMethodDecl& node)
	{
		// TODO: throws
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}

		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			Visit(*node.generics);
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		
		StdVector<ITrParamSPtr> params = GetParams(node.params);
		AddMethodReceiverToParams(node.rec, params);

		StdVector<ITrStmtSPtr> stmts;
		ITrTypeSPtr retType;
		if (!node.namedRet.empty())
		{
			GetNamedReturns(retType, stmts, node.namedRet);
		}
		else if (node.retType)
		{
			AstVisitor::Visit(node.retType);
			retType = PopType();
			retType->astNode = node.retType;
		}

		ITrDefSPtr def{ new ITrFunc{ attribs, genDecl, node.ctx->qualName, std::move(params), retType, ITrFuncKind::Method, IsModDef() } };

		PushDefFrame();
		for (AstStmtSPtr astStmt : node.stmts)
		{
			AstVisitor::Visit(astStmt);
			ITrStmtSPtr stmt = PopStmt();
			stmt->astNode = astStmt;
			stmts.push_back(stmt);
		}
		StdVector<ITrDefSPtr> defs = PopDefFrame();

		ITrBodySPtr body{ new ITrBody{ std::move(defs), std::move(stmts) } };

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstEmptyMethodDecl& node)
	{
		// TODO: throws
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}

		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			Visit(*node.generics);
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}
		
		StdVector<ITrParamSPtr> params = GetParams(node.params);
		AddMethodReceiverToParams(node.rec, params);

		ITrTypeSPtr retType;
		if (node.retType)
		{
			AstVisitor::Visit(node.retType);
			retType = PopType();
			retType->astNode = node.retType;
		}

		ITrDefSPtr def{ new ITrFunc{ attribs, genDecl, node.ctx->qualName, std::move(params), retType, ITrFuncKind::EmptyMethod, false } };

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def);
		PushDef(def);
	}

	void AstToITrLowering::Visit(AstImplDecl& node)
	{
		Visit(node.type);
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;

		PushDefFrame();
		m_ImplType = type->handle;
		Walk(node);
		m_ImplType = TypeHandle(-1);
		StdVector<ITrDefSPtr> defs = PopDefFrame();
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();
			attribs->astNode = node.attribs;
		}
		ITrGenDeclSPtr genDecl;
		if (node.generics)
		{
			genDecl = PopGenDecl();
			genDecl->astNode = node.generics;
		}

		StdPairVector<QualNameSPtr, SpanId> interfaces;
		usize size = node.interfaces.size();
		interfaces.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrTypeSPtr interface = PopType();
			interface->astNode = node.interfaces[i];
			TypeSPtr interfaceType = m_pCtx->typeReg.GetType(interface->handle);
			interfaces[i].first = interfaceType->AsIden().qualName;
			interfaces[i].second = interface->astNode->ctx->startIdx;
		}
		
		ITrDefSPtr def{ new ITrImpl{ attribs, genDecl, node.ctx->qualName, type, std::move(interfaces) } };
		node.defItr = def;
		
		ITrBodySPtr body{ new ITrBody{ std::move(defs), {} } };

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
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
			AstVisitor::Visit(astStmt);
			if (astStmt->stmtKind != AstStmtKind::Decl)
			{
				ITrStmtSPtr stmt = PopStmt();
				stmt->astNode = astStmt;
				stmts.push_back(stmt);
			}
		}

		if (stmts.size() == 1)
		{
			ITrStmtSPtr stmt = stmts[0];
			if (stmt->stmtKind == ITrStmtKind::Block ||
				stmt->stmtKind == ITrStmtKind::Unsafe)
			{
				node.itr = stmt;
				m_Stmts.push(stmt);
				return;
			}
		}

		ITrStmtSPtr stmt{ new ITrBlock{ std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstIfStmt& node)
	{
		Walk(node);
		ITrBlockSPtr fBlock;
		if (node.elseBody)
		{
			ITrStmtSPtr fBlockStmt = PopStmt();
			if (fBlockStmt->stmtKind == ITrStmtKind::Block)
				fBlock = *reinterpret_cast<ITrBlockSPtr*>(&fBlockStmt);
			else
				fBlock = ITrBlockSPtr{ new ITrBlock{ { fBlockStmt } } };
			fBlock->astNode = node.elseBody;
		}
		ITrStmtSPtr tBlockStmt = PopStmt();
		ITrBlockSPtr tBlock;
		if (tBlockStmt->stmtKind == ITrStmtKind::Block)
			tBlock = *reinterpret_cast<ITrBlockSPtr*>(&tBlockStmt);
		else
			tBlock = ITrBlockSPtr{ new ITrBlock{ { tBlockStmt } } };
		tBlock->astNode = node.body;

		ITrExprSPtr cond = PopExpr();

		ITrLocalVarSPtr decl;
		if (node.decl)
		{
			ITrStmtSPtr declStmt = PopStmt();
			declStmt->astNode = node.decl;
			decl = *reinterpret_cast<ITrLocalVarSPtr*>(&declStmt);
		}

		ITrStmtSPtr stmt{ new ITrIf{ false, decl, cond, tBlock, fBlock } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstLoopStmt& node)
	{
		Walk(node);
		ITrStmtSPtr body = PopStmt();
		body->astNode = node.body;

		StdVector<ITrStmtSPtr> stmts;
		if (body->stmtKind == ITrStmtKind::Block)
			stmts = static_cast<ITrBlock*>(body.get())->stmts;
		else
			stmts.push_back(body);

		IdenSPtr label = node.label ? Iden::Create(node.label->iden) : nullptr;

		ITrStmtSPtr stmt{ new ITrLoop{ label, std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstWhileStmt& node)
	{
		Walk(node);
		ITrStmtSPtr body = PopStmt();
		body->astNode = node.body;

		StdVector<ITrStmtSPtr> stmts;
		if (body->stmtKind == ITrStmtKind::Block)
			stmts = static_cast<ITrBlock*>(body.get())->stmts;
		else
			stmts.push_back(body);

		// Condition
		ITrExprSPtr cond = PopExpr();
		cond = ITrExprSPtr{ new ITrUnary{ OperatorKind::Not, cond } };
		ITrStmtSPtr breakStmt{ new ITrBreak{} };
		ITrBlockSPtr breakBlock{ new ITrBlock{ { breakStmt } } };
		stmts.insert(stmts.begin(), ITrStmtSPtr{ new ITrIf{ false, nullptr, cond, breakBlock, nullptr } });

		IdenSPtr label = node.label ? Iden::Create(node.label->iden) : nullptr;

		ITrStmtSPtr stmt{ new ITrLoop{ label, std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstDoWhileStmt& node)
	{
		Walk(node);
		ITrStmtSPtr body = PopStmt();
		body->astNode = node.body;

		StdVector<ITrStmtSPtr> stmts;
		if (body->stmtKind == ITrStmtKind::Block)
			stmts = static_cast<ITrBlock*>(body.get())->stmts;
		else
			stmts.push_back(body);

		// Condition
		ITrExprSPtr cond = PopExpr();
		cond = ITrExprSPtr{ new ITrUnary{ OperatorKind::Not, cond } };
		ITrStmtSPtr breakStmt{ new ITrBreak{} };
		ITrBlockSPtr breakBlock{ new ITrBlock{ { breakStmt } } };
		stmts.push_back(ITrStmtSPtr{ new ITrIf{ false, nullptr, cond, breakBlock, nullptr } });

		IdenSPtr label = node.label ? Iden::Create(node.label->iden) : nullptr;
		
		ITrStmtSPtr stmt{ new ITrLoop{ label, std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstForStmt& node)
	{
		// TODO: Need to figure out iterator API first
		
		//StdVector<ITrStmtSPtr> stmts;
		//
		//IdenSPtr itIden = Iden::Create(Format("__it_%u", node.ctx->startIdx));
		//ITrStmtSPtr decl{ new ITrLocalVar{ nullptr, { itIden }, nullptr, } };
	}

	void AstToITrLowering::Visit(AstSwitchStmt& node)
	{
		Walk(node);

		StdVector<ITrSwitchCase> cases;
		usize size = node.cases.size();
		cases.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrPatternSPtr pattern = PopPattern();
			ITrExprSPtr expr;
			if (node.cases[i].expr)
			{
				expr = PopExpr();
				expr->astNode = node.cases[i].expr;
			}

			ITrStmtSPtr tmp = PopStmt();
			tmp->astNode = node.cases[i].body;
			ITrBlockSPtr block;
			if (tmp->stmtKind == ITrStmtKind::Block)
				block = *reinterpret_cast<ITrBlockSPtr*>(&tmp);
			else
				block = ITrBlockSPtr{ new ITrBlock{ { tmp } } };

			cases.emplace_back(pattern, expr, block);
		}

		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.cond;

		IdenSPtr label = node.label ? Iden::Create(node.label->iden) : nullptr;
		
		ITrStmtSPtr stmt{ new ITrSwitch{ label, expr, std::move(cases) } };
	}

	void AstToITrLowering::Visit(AstLabelStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrLabel{ Iden::Create(node.iden) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstBreakStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrBreak{ Iden::Create(node.iden) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstContinueStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrContinue{ Iden::Create(node.iden) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstFallthroughStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrFallthrough{} };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstGotoStmt& node)
	{
		ITrStmtSPtr stmt{ new ITrGoto{ Iden::Create(node.iden) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstReturnStmt& node)
	{
		Walk(node);
		ITrExprSPtr expr;
		if (node.expr)
		{
			expr = PopExpr();
			expr->astNode = node.expr;
		}

		ITrStmtSPtr stmt{ new ITrReturn{ expr } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstThrowStmt& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		ITrStmtSPtr stmt{ new ITrReturn{ expr } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstExprStmt& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		ITrStmtSPtr stmt = *reinterpret_cast<ITrStmtSPtr*>(expr.get());
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstDeferStmt& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		ITrStmtSPtr stmt{ new ITrDefer{ false, expr } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstErrDeferStmt& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		ITrStmtSPtr stmt{ new ITrDefer{ true, expr } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstUnsafeStmt& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			AstVisitor::Visit(astStmt);
			if (astStmt->stmtKind != AstStmtKind::Decl)
			{
				ITrStmtSPtr stmt = PopStmt();
				stmt->astNode = astStmt;
				stmts.push_back(stmt);
			}
		}

		if (stmts.size() == 1)
		{
			ITrStmtSPtr stmt = stmts[0];
			if (stmt->stmtKind == ITrStmtKind::Block ||
				stmt->stmtKind == ITrStmtKind::Unsafe)
			{
				node.itr = stmt;
				m_Stmts.push(stmt);
				return;
			}
		}

		ITrStmtSPtr stmt{ new ITrUnsafe{ std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstErrorHandlerStmt& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			AstVisitor::Visit(astStmt);
			if (astStmt->stmtKind != AstStmtKind::Decl)
			{
				ITrStmtSPtr stmt = PopStmt();
				stmt->astNode = astStmt;
				stmts.push_back(stmt);
			}
		}

		ITrStmtSPtr stmt{ new ITrErrHandler{ std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstCompIfStmt& node)
	{
		Walk(node);
		ITrBlockSPtr fBlock;
		if (node.elseBody)
		{
			ITrStmtSPtr fBlockStmt = PopStmt();
			if (fBlockStmt->stmtKind == ITrStmtKind::Block)
				fBlock = *reinterpret_cast<ITrBlockSPtr*>(&fBlockStmt);
			else
				fBlock = ITrBlockSPtr{ new ITrBlock{ { fBlockStmt } } };
			fBlock->astNode = node.elseBody;
		}
		ITrStmtSPtr tBlockStmt = PopStmt();
		ITrBlockSPtr tBlock;
		if (tBlockStmt->stmtKind == ITrStmtKind::Block)
			tBlock = *reinterpret_cast<ITrBlockSPtr*>(&tBlockStmt);
		else
			tBlock = ITrBlockSPtr{ new ITrBlock{ { tBlockStmt } } };
		tBlock->astNode = node.body;

		ITrExprSPtr cond = PopExpr();

		ITrLocalVarSPtr decl;
		if (node.decl)
		{
			ITrStmtSPtr declStmt = PopStmt();
			declStmt->astNode = node.decl;
			decl = *reinterpret_cast<ITrLocalVarSPtr*>(&declStmt);
		}

		ITrStmtSPtr stmt{ new ITrIf{ true, decl, cond, tBlock, fBlock } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstCompCondStmt& node)
	{
		Walk(node);
		ITrBlockSPtr fBlock;
		if (node.elseBody)
		{
			ITrStmtSPtr fBlockStmt = PopStmt();
			if (fBlockStmt->stmtKind == ITrStmtKind::Block)
				fBlock = *reinterpret_cast<ITrBlockSPtr*>(&fBlockStmt);
			else
				fBlock = ITrBlockSPtr{ new ITrBlock{ { fBlockStmt } } };
			fBlock->astNode = node.elseBody;
		}
		ITrStmtSPtr tBlockStmt = PopStmt();
		ITrBlockSPtr tBlock;
		if (tBlockStmt->stmtKind == ITrStmtKind::Block)
			tBlock = *reinterpret_cast<ITrBlockSPtr*>(&tBlockStmt);
		else
			tBlock = ITrBlockSPtr{ new ITrBlock{ { tBlockStmt } } };
		tBlock->astNode = node.body;
		
		IdenSPtr cond = Iden::Create(node.cond.Text());
		OperatorKind op;
		switch (node.cmp.Type())
		{
		case TokenType::EqEq: op = OperatorKind::Eq; break;
		case TokenType::ExclaimEq: op = OperatorKind::Ne; break;
		case TokenType::Less: op = OperatorKind::Lt; break;
		case TokenType::LessEq: op = OperatorKind::Le; break;
		case TokenType::Greater: op = OperatorKind::Gt; break;
		case TokenType::GreaterEq: op = OperatorKind::Ge; break;
		default: op = OperatorKind::Invalid; break;
		}
		u64 val = node.val.Unsigned();

		ITrStmtSPtr stmt{ new ITrCompCond{ false, cond, op, val, tBlock, fBlock } };
	}

	void AstToITrLowering::Visit(AstCompDebugStmt& node)
	{
		Walk(node);
		ITrBlockSPtr fBlock;
		if (node.elseBody)
		{
			ITrStmtSPtr fBlockStmt = PopStmt();
			if (fBlockStmt->stmtKind == ITrStmtKind::Block)
				fBlock = *reinterpret_cast<ITrBlockSPtr*>(&fBlockStmt);
			else
				fBlock = ITrBlockSPtr{ new ITrBlock{ { fBlockStmt } } };
			fBlock->astNode = node.elseBody;
		}
		ITrStmtSPtr tBlockStmt = PopStmt();
		ITrBlockSPtr tBlock;
		if (tBlockStmt->stmtKind == ITrStmtKind::Block)
			tBlock = *reinterpret_cast<ITrBlockSPtr*>(&tBlockStmt);
		else
			tBlock = ITrBlockSPtr{ new ITrBlock{ { tBlockStmt } } };
		tBlock->astNode = node.body;

		IdenSPtr cond = Iden::Create(node.cond.Text());
		OperatorKind op;
		switch (node.cmp.Type())
		{
		case TokenType::EqEq: op = OperatorKind::Eq; break;
		case TokenType::ExclaimEq: op = OperatorKind::Ne; break;
		case TokenType::Less: op = OperatorKind::Lt; break;
		case TokenType::LessEq: op = OperatorKind::Le; break;
		case TokenType::Greater: op = OperatorKind::Gt; break;
		case TokenType::GreaterEq: op = OperatorKind::Ge; break;
		default: op = OperatorKind::Invalid; break;
		}
		u64 val = node.val.Unsigned();

		ITrStmtSPtr stmt{ new ITrCompCond{ true, cond, op, val, tBlock, fBlock } };
	}

	void AstToITrLowering::Visit(AstAssignExpr& node)
	{
		OperatorKind op;
		switch (node.op)
		{
		case TokenType::Eq: op = OperatorKind::Assign; break;
		case TokenType::PlusEq: op = OperatorKind::AddAssign; break;
		case TokenType::MinusEq: op = OperatorKind::SubAssign; break;
		case TokenType::AsteriskEq: op = OperatorKind::MulAssign; break;
		case TokenType::SlashEq: op = OperatorKind::DivAssign; break;
		case TokenType::PercentEq: op = OperatorKind::RemAssign; break;
		case TokenType::TildeEq: op = OperatorKind::ConcatAssign; break;
		case TokenType::LessLessEq: op = OperatorKind::LShlAssign; break;
		case TokenType::LessLessLessEq: op = OperatorKind::AShlAssign; break;
		case TokenType::LessLessAsteriskEq: op = OperatorKind::RotlAssign; break;
		case TokenType::GreaterGreaterEq: op = OperatorKind::LShrAssign; break;
		case TokenType::GreaterGreaterGreaterEq: op = OperatorKind::AShrAssign; break;
		case TokenType::GreaterGreaterAsteriskEq: op = OperatorKind::RotrAssign; break;
		case TokenType::OrEq: op = OperatorKind::BinOrAssign; break;
		case TokenType::CaretEq: op = OperatorKind::BinXorAssign; break;
		case TokenType::AndEq: op = OperatorKind::BinAndAssign; break;
		case TokenType::QuestionQuestionEq: op = OperatorKind::NullCoalesceAssign; break;
		default: return;
		}

		Walk(node);
		ITrExprSPtr rExpr = PopExpr();
		rExpr->astNode = node.rExpr;
		ITrExprSPtr lExpr = PopExpr();
		lExpr->astNode = node.lExpr;

		ITrExprSPtr expr{ new ITrAssign{ op, lExpr, rExpr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstTernaryExpr& node)
	{
		Walk(node);
		
		ITrExprSPtr fExpr = PopExpr();
		fExpr->astNode = node.falseExpr;
		ITrExprSPtr tExpr = PopExpr();
		tExpr->astNode = node.trueExpr;
		ITrExprSPtr cond = PopExpr();
		cond->astNode = node.cond;

		ITrExprSPtr expr{ new ITrTernary{ cond, tExpr, fExpr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstBinaryExpr& node)
	{
		Walk(node);
		
		OperatorKind op;
		switch (node.op)
		{
		case TokenType::Plus: op = OperatorKind::Add; break;
		case TokenType::Minus: op = OperatorKind::Sub; break;
		case TokenType::Asterisk: op = OperatorKind::Mul; break;
		case TokenType::Slash: op = OperatorKind::Div; break;
		case TokenType::Percent: op = OperatorKind::Rem; break;
		case TokenType::Tilde: op = OperatorKind::Concat; break;
		case TokenType::OrOr: op = OperatorKind::Or; break;
		case TokenType::AndAnd: op = OperatorKind::And; break;
		case TokenType::LessLess: op = OperatorKind::LShl; break;
		case TokenType::LessLessLess: op = OperatorKind::AShl; break;
		case TokenType::LessLessAsterisk: op = OperatorKind::Rotl; break;
		case TokenType::GreaterGreater: op = OperatorKind::LShr; break;
		case TokenType::GreaterGreaterGreater: op = OperatorKind::AShr; break;
		case TokenType::GreaterGreaterAsterisk: op = OperatorKind::Rotr; break;
		case TokenType::Or: op = OperatorKind::BinOr; break;
		case TokenType::Caret: op = OperatorKind::BinXor; break;
		case TokenType::And: op = OperatorKind::BinAnd; break;
		case TokenType::EqEq: op = OperatorKind::Eq; break;
		case TokenType::ExclaimEq: op = OperatorKind::Ne; break;
		case TokenType::Less: op = OperatorKind::Lt; break;
		case TokenType::LessEq: op = OperatorKind::Le; break;
		case TokenType::Greater: op = OperatorKind::Gt; break;
		case TokenType::GreaterEq: op = OperatorKind::Ge; break;
		case TokenType::DotDot: op = OperatorKind::Range; break;
		case TokenType::DotDotEq: op = OperatorKind::IncRange; break;
		case TokenType::QuestionQuestion: op = OperatorKind::NullCoalesce; break;
		case TokenType::QuestionColon: op = OperatorKind::Elvis; break;
		case TokenType::In: op = OperatorKind::In; break;
		case TokenType::NotIn: op = OperatorKind::NotIn; break;
		default: return;
		}

		ITrExprSPtr rExpr = PopExpr();
		rExpr->astNode = node.rExpr;
		ITrExprSPtr lExpr = PopExpr();
		lExpr->astNode = node.lExpr;

		ITrExprSPtr expr{ new ITrBinary{ op, lExpr, rExpr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstPostfixExpr& node)
	{
		OperatorKind op;
		switch (node.op)
		{
		case TokenType::PlusPlus: op = OperatorKind::PostInc; break;
		case TokenType::MinusMinus: op = OperatorKind::PostDec; break;
		case TokenType::ExclaimExclaim: op = OperatorKind::NullPanic; break;
		default: return;
		}

		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrUnary{ op, PopExpr() } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstPrefixExpr& node)
	{
		OperatorKind op;
		switch (node.op)
		{
		case TokenType::Plus: op = OperatorKind::Pos; break;
		case TokenType::PlusPlus: op = OperatorKind::PreInc; break;
		case TokenType::Minus: op = OperatorKind::Neg; break;
		case TokenType::MinusMinus: op = OperatorKind::PreDec; break;
		case TokenType::Exclaim: op = OperatorKind::Not; break;
		case TokenType::Tilde: op = OperatorKind::BinNeg; break;
		case TokenType::Asterisk: op = OperatorKind::Deref; break;
		case TokenType::And: op = OperatorKind::AddrOf; break;
		case TokenType::ExclaimExclaim: op = OperatorKind::BoolConv; break;
		default: return;
		}

		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrUnary{ op, PopExpr() } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstQualNameExpr& node)
	{
		ITrExprSPtr expr{ new ITrQualName{ node.ctx->qualName } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstIndexSliceExpr& node)
	{
		Walk(node);
		ITrExprSPtr index = PopExpr();
		index->astNode = node.expr;
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrIndexSlice{ expr, index } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstSliceExpr& node)
	{
		Walk(node);

		ITrExprSPtr from, to;

		if (node.end)
		{
			to = PopExpr();
			to->astNode = node.end;
		}
		if (node.begin)
		{
			from = PopExpr();
			from->astNode = node.begin;
		}
		
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrIndexSlice{ expr, from, to } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstFuncCallExpr& node)
	{
		Walk(node);

		StdVector<ITrArgSPtr> args = GetArgs(node.args);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.func;

		expr = ITrExprSPtr{ new ITrAmbiguousCall{ expr, std::move(args) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstMemberAccessExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.caller;

		expr = ITrExprSPtr{ new ITrMemberAccess{ node.nullCoalesce, expr, Iden::Create(node.iden) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstMethodCallExpr& node)
	{
		Walk(node);

		StdVector<ITrArgSPtr> args = GetArgs(node.args);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.caller;
		IdenSPtr iden = Iden::Create(node.iden);
		
		expr = ITrExprSPtr{ new ITrFuncCall{ expr, node.nullCoalesce, iden, std::move(args) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstTupleAccessExpr& node)
	{
		Walk(node);

		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;
			
		expr = ITrExprSPtr{ new ITrTupleAccess{ expr, node.nullCoalesce, node.index } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstLiteralExpr& node)
	{
		ITrExprSPtr expr{ new ITrLiteral{ node.literal } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstAggrInitExpr& node)
	{
		Walk(node);

		StdVector<ITrArgSPtr> args = GetArgs(node.args);
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;
		
		ITrExprSPtr expr{ new ITrAmbiguousAggrInit{ type, std::move(args) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstTupleInitExpr& node)
	{
		Walk(node);
		
		StdVector<ITrExprSPtr> exprs;
		usize size = node.exprs.size();
		exprs.reserve(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrExprSPtr expr = PopExpr();
			expr->astNode = node.exprs[i];
			exprs[i] = expr;
		}

		ITrExprSPtr expr{ new ITrTupleInit{ std::move(exprs) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstArrayInitExpr& node)
	{
		Walk(node);

		StdVector<ITrExprSPtr> exprs;
		usize size = node.exprs.size();
		exprs.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrExprSPtr expr = PopExpr();
			expr->astNode = node.exprs[i];
			exprs[i] = expr;
		}

		ITrExprSPtr expr{ new ITrArrayInit{ std::move(exprs) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstCastExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;

		expr = ITrExprSPtr{ new ITrCast{ false, type, expr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstTransmuteExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;
		
		expr = ITrExprSPtr{ new ITrCast{ true, type, expr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstMoveExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrMove{ expr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstBracketExpr& node)
	{
		AstVisitor::Visit(node.expr);
		node.itr = m_Exprs.top();
	}

	void AstToITrLowering::Visit(AstBlockExpr& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			AstVisitor::Visit(astStmt);
			if (astStmt->stmtKind != AstStmtKind::Decl)
			{
				ITrStmtSPtr stmt = PopStmt();
				stmt->astNode = astStmt;
				stmts.push_back(stmt);
			}
		}

		ITrExprSPtr expr{ new ITrBlockExpr{ std::move(stmts) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstUnsafeExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrUnsafeExpr{ expr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstCommaExpr& node)
	{
		Walk(node);

		StdVector<ITrExprSPtr> exprs;
		usize size = node.exprs.size();
		exprs.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrExprSPtr expr = PopExpr();
			expr->astNode = node.exprs[i];
			exprs[i] = expr;
		}

		ITrExprSPtr expr{ new ITrComma{ std::move(exprs) } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstClosureExpr& node)
	{
		StdVector<ITrParamSPtr> params = GetParams(node.params);
		ITrTypeSPtr retType;
		if (node.ret)
		{
			AstVisitor::Visit(node.ret);
			retType = PopType();
			retType->astNode = node.ret;
		}

		ITrDefSPtr def{ new ITrFunc{ nullptr, nullptr, node.ctx->qualName, std::move(params), retType, ITrFuncKind::Closure, false } };

		AstVisitor::Visit(node.expr);
		PushDefFrame();
		ITrExprSPtr expr = PopExpr();
		StdVector<ITrDefSPtr> defs = PopDefFrame();
		
		StdVector<ITrStmtSPtr> stmts;
		if (retType)
			stmts.emplace_back(new ITrReturn{ expr });
		else
			stmts.push_back(expr);
		
		ITrBodySPtr body{ new ITrBody{ std::move(defs), std::move(stmts) } };

		ITrModule& mod = m_pCtx->activeModule->itrModule;
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);

		ITrExprSPtr closure{ new ITrClosure{ def } };
		node.itr = closure;
		m_Exprs.push(closure);
	}

	void AstToITrLowering::Visit(AstIsExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;

		expr = ITrExprSPtr{ new ITrIs{ expr, type } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstTryExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.call;

		expr = ITrExprSPtr{ new ITrTry{ expr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstSpecKwExpr& node)
	{
		ITrExprSPtr expr{ new ITrSpecKw{ node.specKw } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstCompRunExpr& node)
	{
		Walk(node);
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.expr;

		expr = ITrExprSPtr{ new ITrCompRun{ expr } };
		node.itr = expr;
		m_Exprs.push(expr);
	}

	void AstToITrLowering::Visit(AstTypeSPtr& node)
	{
		if (node->itr.lock())
		{
			m_Types.push(node->itr.lock());
			return;
		}
		
		AstVisitor::Visit(node);
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

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();
			
		}
		
		TypeHandle handle = m_pCtx->typeReg.Builtin(TypeMod::None, builtin);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {} } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstIdentifierType& node)
	{
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();

		}
		
		QualNameSPtr qualName = node.qualName->ctx->qualName;
		TypeHandle handle = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {} } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstPointerType& node)
	{
		Walk(node);
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		ITrTypeSPtr subType = PopType();
		subType->astNode = node.subType;
		TypeHandle handle = m_pCtx->typeReg.Ptr(TypeMod::None, subType->handle);

		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType } } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstReferenceType& node)
	{
		Walk(node);
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		ITrTypeSPtr subType = PopType();
		subType->astNode = node.subType;
		TypeHandle handle = m_pCtx->typeReg.Ref(TypeMod::None, subType->handle);

		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType } } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstArrayType& node)
	{
		Walk(node);

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		ITrExprSPtr expr = PopExpr();
		expr->astNode = node.arraySize;
		
		ITrTypeSPtr subType = PopType();
		subType->astNode = node.subType;
		TypeHandle handle = m_pCtx->typeReg.Array(TypeMod::None, subType->handle, expr);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType } } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstSliceType& node)
	{
		Walk(node);

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		ITrTypeSPtr subType = PopType();
		subType->astNode = node.subType;
		TypeHandle handle = m_pCtx->typeReg.Slice(TypeMod::None, subType->handle);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType } } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstTupleType& node)
	{
		Walk(node);
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		ITrTypeSPtr type;
		if (node.subTypes.empty())
		{
			 TypeHandle handle = m_pCtx->typeReg.Tuple(TypeMod::None, {});
			 ITrTypeSPtr type{ new ITrType{ attribs, handle, {} } };
		}
		else if (node.subTypes.size() == 1)
		{
			type = PopType();
			type->astNode = node.subTypes[0];
			type = ITrTypeSPtr{ new ITrType{ attribs, type->handle, { type } } };
		}
		else
		{
			StdVector<TypeHandle> subTypesHandles;
			StdVector<ITrTypeSPtr> subTypes;
			usize size = node.subTypes.size();
			subTypesHandles.resize(size);
			subTypes.resize(size);
			for (usize i = size; i > 0;)
			{
				--i;
				ITrTypeSPtr subType = PopType();
				subType->astNode = node.subTypes[i];
				subTypesHandles[i] = subType->handle;
				subTypes[i] = subType;
			}

			TypeHandle handle = m_pCtx->typeReg.Tuple(TypeMod::None, subTypesHandles);
			type = ITrTypeSPtr{ new ITrType{ attribs, handle, std::move(subTypes) } };
		}

		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstOptionalType& node)
	{
		Walk(node);
		
		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		ITrTypeSPtr subType = PopType();
		subType->astNode = node.subType;
		TypeHandle handle = m_pCtx->typeReg.Opt(TypeMod::None, subType->handle);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, { subType } } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstInlineStructType& node)
	{
		// TODO: def in type
		ITrModule& mod = m_pCtx->activeModule->itrModule;
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdVector<StdString>, AstTypeSPtr> astMember : node.members)
		{
			AstVisitor::Visit(astMember.second);
			ITrTypeSPtr type = PopType();
			type->astNode = astMember.second;

			for (StdString& iden : astMember.first)
			{
				QualNameSPtr qualName = QualName::Create(node.ctx->qualName, Iden::Create(iden));
				ITrDefSPtr member { new ITrVar{ nullptr, qualName, type, false } };
				members.push_back(member);
				member->fileName = m_TreeFilename;
				mod.AddDefinition(member);
			}
		}

		ITrBodySPtr body{ new ITrBody{ std::move(members), {} } };
		ITrDefSPtr def{ new ITrStruct{ nullptr, nullptr, node.ctx->qualName, false } };
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();

		}

		TypeHandle handle = m_pCtx->typeReg.Iden(TypeMod::None, node.ctx->qualName);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {} } };
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstInlineEnumType& node)
	{
		QualNameSPtr qualName = node.ctx->qualName;
		ITrModule& mod = m_pCtx->activeModule->itrModule;
		
		StdVector<ITrDefSPtr> members;
		for (StdPair<StdString, AstExprSPtr>& astMember : node.members)
		{
			IdenSPtr iden = Iden::Create(astMember.first);
			ITrExprSPtr expr;
			if (astMember.second)
			{
				AstVisitor::Visit(astMember.second);
				expr = PopExpr();
				expr->astNode = astMember.second;
			}

			ITrDefSPtr member{ new ITrValEnumMember{ qualName, iden, expr } };
			members.push_back(member);
			member->fileName = m_TreeFilename;
			mod.AddDefinition(member);
		}

		ITrBodySPtr body{ new ITrBody{ std::move(members), {} } };
		ITrDefSPtr def{ new ITrStruct{ nullptr, nullptr, qualName, false } };
		def->fileName = m_TreeFilename;
		def->astNode = m_DeclNode;
		mod.AddDefinition(def, body);

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			Visit(*node.attribs);
			attribs = PopAttribs();

		}

		TypeHandle handle = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, {} } };
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstCompoundInterfaceType& node)
	{
		Walk(node);

		ITrAttribsSPtr attribs;
		if (node.attribs)
		{
			attribs = PopAttribs();

		}
		
		StdVector<TypeHandle> subTypesHandles;
		StdVector<ITrTypeSPtr> subTypes;
		usize size = node.interfaces.size();
		subTypesHandles.reserve(size);
		subTypes.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrTypeSPtr type = PopType();
			type->astNode = node.interfaces[i];
			subTypesHandles[i] = type->handle;
			subTypes[i] = type;
		}

		TypeHandle handle = m_pCtx->typeReg.Compound(TypeMod::None, subTypesHandles);
		ITrTypeSPtr type{ new ITrType{ attribs, handle, std::move(subTypes) } };
		node.itr = type;
		m_Types.push(type);
	}

	void AstToITrLowering::Visit(AstPlaceholderPattern& node)
	{
		Walk(node);
		ITrPatternSPtr pattern{ new ITrPlaceholderPattern{ false } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstWildcardPattern& node)
	{
		Walk(node);
		ITrPatternSPtr pattern{ new ITrPlaceholderPattern{ true } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstValueBindPattern& node)
	{
		Walk(node);
		IdenSPtr iden = Iden::Create(node.iden);
		ITrPatternSPtr subPattern;
		if (node.subPattern)
		{
			subPattern = PopPattern();
			subPattern->astNode = node.subPattern;
		}

		ITrPatternSPtr pattern{ new ITrValueBindPattern{ iden, std::move(subPattern) } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstLiteralPattern& node)
	{
		Walk(node);
		ITrPatternSPtr pattern{ new ITrLiteralPattern{ node.literal } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstRangePattern& node)
	{
		Walk(node);
		ITrPatternSPtr to = PopPattern();
		to->astNode = node.to;
		ITrPatternSPtr from = PopPattern();
		from->astNode = node.from;

		ITrPatternSPtr pattern{ new ITrRangePattern{ node.inclusive, from, to } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstTuplePattern& node)
	{
		Walk(node);
		StdVector<ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrPatternSPtr tmp = PopPattern();
			tmp->astNode = node.subPatterns[i];
			subPatterns[i] = tmp;
		}

		ITrPatternSPtr pattern{ new ITrTuplePattern{ std::move(subPatterns) } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstEnumPattern& node)
	{
		Walk(node);
		QualNameSPtr qualName = node.iden->ctx->qualName;
		if (node.subPatterns.empty())
		{
			ITrPatternSPtr pattern{ new ITrValueEnumPattern{ qualName } };
			node.itr = pattern;
			m_Patterns.push(pattern);
		}
		else
		{
			StdVector<ITrPatternSPtr> subPatterns;
			usize size = node.subPatterns.size();
			subPatterns.resize(size);
			for (usize i = size; i > 0;)
			{
				--i;
				ITrPatternSPtr tmp = PopPattern();
				tmp->astNode = node.subPatterns[i];
				subPatterns[i] = tmp;
			}

			ITrPatternSPtr pattern{ new ITrAdtTupleEnumPattern{ qualName, std::move(subPatterns) } };
			node.itr = pattern;
			m_Patterns.push(pattern);
		}
	}

	void AstToITrLowering::Visit(AstAggrPattern& node)
	{
		Walk(node);
		QualNameSPtr qualName = node.qualName->ctx->qualName;
		
		StdPairVector<IdenSPtr, ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrPatternSPtr tmp = PopPattern();
			tmp->astNode = node.subPatterns[i].second;
			StdString name = node.subPatterns[i].first;
			IdenSPtr iden = name.empty() ? nullptr : Iden::Create(name);
			subPatterns[i] = std::pair{ iden, tmp };
		}

		ITrPatternSPtr pattern{ new ITrAmbiguousAggrPattern{ qualName, std::move(subPatterns) } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstSlicePattern& node)
	{
		Walk(node);
		StdVector<ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrPatternSPtr tmp = PopPattern();
			tmp->astNode = node.subPatterns[i];
			subPatterns[i] = tmp;
		}

		ITrPatternSPtr pattern{ new ITrSlicePattern{ std::move(subPatterns) } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstEitherPattern& node)
	{
		Walk(node);
		StdVector<ITrPatternSPtr> subPatterns;
		usize size = node.subPatterns.size();
		subPatterns.resize(size);
		for (usize i = size; i > 0;)
		{
			--i;
			ITrPatternSPtr tmp = PopPattern();
			tmp->astNode = node.subPatterns[i];
			subPatterns[i] = tmp;
		}

		ITrPatternSPtr pattern{ new ITrEitherPattern{ std::move(subPatterns) } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstTypePattern& node)
	{
		Walk(node);
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;
		ITrPatternSPtr pattern{ new ITrTypePattern{ type } };
		node.itr = pattern;
		m_Patterns.push(pattern);
	}

	void AstToITrLowering::Visit(AstAttribs& node)
	{
		Walk(node);
		
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
			case TokenType::Immutable: tmp = Attribute::Immutable; break;
			case TokenType::Static: tmp = Attribute::Static; break;
			case TokenType::Comptime: tmp = Attribute::Comptime; break;
			case TokenType::Lazy: tmp = Attribute::Lazy; break;
			case TokenType::Move: tmp = Attribute::Move; break;
			default: tmp = Attribute::None;
			}

			if (ENUM_IS_SET(attribs, tmp))
			{
				Span span = m_pCtx->spanManager.GetSpan(node.ctx->startIdx);
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
			IdenSPtr iden = Iden::Create(compAttrib->iden);
			ITrAtAttribSPtr attr{ new ITrAtAttrib { true, iden, std::move(args) } };
			atAttribs.push_back(attr);
		}

		for (AstUserAttribSPtr userAttrib : node.userAttribs)
		{
			StdVector<ITrArgSPtr> args = GetArgs(userAttrib->args);
			IdenSPtr iden = Iden::Create(userAttrib->iden);
			ITrAtAttribSPtr attr{ new ITrAtAttrib { false, iden, std::move(args) } };
			atAttribs.push_back(attr);
		}

		m_Attribs.push(ITrAttribsSPtr{ new ITrAttribs{ vis, attribs, std::move(atAttribs) } });
	}

	void AstToITrLowering::Visit(AstMacroLoopStmt& node)
	{
		StdVector<ITrStmtSPtr> stmts;
		for (AstStmtSPtr astStmt : node.stmts)
		{
			AstVisitor::Visit(astStmt);
			if (astStmt->stmtKind != AstStmtKind::Decl)
			{
				ITrStmtSPtr stmt = PopStmt();
				stmt->astNode = astStmt;
				stmts.push_back(stmt);
			}
		}

		ITrStmtSPtr stmt{ new ITrBlock{ std::move(stmts) } };
		node.itr = stmt;
		m_Stmts.push(stmt);
	}

	void AstToITrLowering::Visit(AstGenericDecl& node)
	{
		m_GenDecl = ITrGenDeclSPtr{ new ITrGenDecl{} };
		Walk(node);
		m_GenDecls.push(m_GenDecl);
		m_GenDecl = nullptr;
	}

	void AstToITrLowering::Visit(AstGenericTypeParam& node)
	{
		Walk(node);
		IdenSPtr iden = Iden::Create(node.iden);

		ITrTypeSPtr def;
		if (node.defType)
		{
			def = PopType();
			def->astNode = node.defType;
		}

		ITrGenParamSPtr param{ new ITrGenTypeParam{ iden, def } };
		node.itr = param;
		m_GenDecl->params.push_back(param);

		if (!node.implTypes.empty())
		{
			StdVector<TypeHandle> subTypesHandles;
			StdVector<ITrTypeSPtr> subTypes;
			usize size = node.implTypes.size();
			subTypesHandles.reserve(size);
			subTypes.resize(size);
			for (usize i = size; i > 0;)
			{
				--i;
				ITrTypeSPtr type = PopType();
				type->astNode = node.implTypes[i];
				subTypesHandles[i] = type->handle;
				subTypes[i] = type;
			}

			TypeHandle handle = m_pCtx->typeReg.Compound(TypeMod::None, subTypesHandles);
			ITrTypeSPtr interfaces{ new ITrType{ nullptr, handle, std::move(subTypes) } };

			QualNameSPtr qualName = QualName::Create(node.ctx->qualName, iden);
			handle = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
			ITrTypeSPtr type{ new ITrType{ nullptr, handle, {} } };

			ITrGenBoundSPtr bound{ new ITrGenBound{ type, std::move(interfaces) } };
			node.itrBound = bound;
			m_GenDecl->bounds.push_back(bound);
		}
		
	}

	void AstToITrLowering::Visit(AstGenericValueParam& node)
	{
		Walk(node);
		IdenSPtr iden = Iden::Create(node.iden);

		ITrExprSPtr def;
		if (node.defExpr)
		{
			def = PopExpr();
			def->astNode = node.defExpr;
		}

		ITrTypeSPtr type = PopType();
		type->astNode = node.type;

		ITrGenParamSPtr param{ new ITrGenValParam{ iden, type, def } };
		node.itr = param;
		m_GenDecl->params.push_back(param);
	}

	void AstToITrLowering::Visit(AstGenericTypeBound& node)
	{
		Walk(node);
		ITrTypeSPtr boundType = PopType();
		boundType->astNode = node.bound;
		ITrTypeSPtr type = PopType();
		type->astNode = node.type;

		ITrGenBoundSPtr bound{ new ITrGenBound{ type, boundType } };
		node.itr = bound;
		m_GenDecl->bounds.push_back(bound);
	}

	void AstToITrLowering::Visit(AstDeclSPtr& node)
	{
		m_DeclNode = node;
		AstVisitor::Visit(node);
	}

	StdVector<ITrParamSPtr> AstToITrLowering::GetParams(StdVector<AstParamSPtr>& astParams)
	{
		StdVector<ITrParamSPtr> params;
		usize size = astParams.size();
		params.reserve(size);
		for (AstParamSPtr astParam : astParams)
		{
			AstVisitor::Visit(astParam->type);
			ITrTypeSPtr type = PopType();
			type->astNode = astParam->type;
			
			for (AstParamVarSPtr paramVar : astParam->vars)
			{
				ITrAttribsSPtr attribs;
				if (paramVar->attribs)
				{
					Walk(*paramVar->attribs);
					attribs = PopAttribs();
					attribs->astNode = paramVar->attribs;
				}

				IdenSPtr iden = Iden::Create(paramVar->iden);
				
				ITrParamSPtr param{ new ITrParam{ attribs, iden, type } };
				param->astNode = astParam;
				param->astVarNode = paramVar;
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
			Walk(*astArg);
			ITrExprSPtr expr = PopExpr();
			expr->astNode = astArg->expr;

			const StdString& idenName = astArg->iden;
			IdenSPtr iden = idenName.empty() ? nullptr : Iden::Create(idenName);

			ITrArgSPtr arg{ new ITrArg{ iden, expr } };
			arg->astNode = astArg;
			args.push_back(arg);
		}
		return args;
	}

	void AstToITrLowering::AddMethodReceiverToParams(AstMethodReceiverKind recKind, StdVector<ITrParamSPtr>& params)
	{
		ITrTypeSPtr recType{ new ITrType { nullptr, m_ImplType, {} } };
		switch (recKind)
		{
		case AstMethodReceiverKind::ConstRef:
		{
			TypeHandle tmp = m_pCtx->typeReg.Mod(TypeMod::Const, recType->handle);
			recType = ITrTypeSPtr{ new ITrType { nullptr, tmp, {} } };
			// fallthrough
		}
		case AstMethodReceiverKind::Ref:
		{
			TypeHandle tmp = m_pCtx->typeReg.Ref(TypeMod::None, recType->handle);
			recType = ITrTypeSPtr{ new ITrType { nullptr, tmp, { recType } } };
			// fallthrough
		}
		case AstMethodReceiverKind::Value:
		{
			ITrParamSPtr recParam{ new ITrParam{ nullptr, Iden::Create("self"), recType } };
			params.insert(params.begin(), recParam);
			break;
		}
		default:;
		}
	}

	void AstToITrLowering::GetNamedReturns(ITrTypeSPtr& retType, StdVector<ITrStmtSPtr> stmts, StdPairVector<StdString, AstTypeSPtr>& astNamedRets)
	{
		StdVector<TypeHandle> subTypesHandles;
		StdVector<ITrTypeSPtr> subTypes;
		StdVector<ITrExprSPtr> retSubExprs;
		for (StdPair<StdString, AstTypeSPtr> namedRet : astNamedRets)
		{
			AstVisitor::Visit(namedRet.second);
			ITrTypeSPtr tmp = PopType();
			tmp->astNode = namedRet.second;

			subTypesHandles.push_back(tmp->handle);
			subTypes.push_back(tmp);

			IdenSPtr iden = Iden::Create(namedRet.first);
			stmts.emplace_back(new ITrLocalVar{ nullptr,  { iden }, tmp, nullptr });
			retSubExprs.emplace_back(new ITrQualName{ QualName::Create(iden) });
		}

		m_NamedRet = ITrExprSPtr{ new ITrTupleInit{ std::move(retSubExprs) } };

		TypeHandle handle = m_pCtx->typeReg.Tuple(TypeMod::None, subTypesHandles);
		retType = ITrTypeSPtr{ new ITrType{ nullptr, handle, std::move(subTypes) } };
	}

	void AstToITrLowering::PushDefFrame()
	{
		m_Defs.push(StdVector<ITrDefSPtr>{});
	}

	void AstToITrLowering::PushDef(ITrDefSPtr def)
	{
		if (!m_Defs.empty())
			m_Defs.top().push_back(def);
	}

	StdVector<ITrDefSPtr> AstToITrLowering::PopDefFrame()
	{
		StdVector<ITrDefSPtr> tmp = m_Defs.top();
		m_Defs.pop();
		return tmp;
	}

	bool AstToITrLowering::IsModDef()
	{
		return m_Defs.empty();
	}

	ITrStmtSPtr AstToITrLowering::PopStmt()
	{
		ITrStmtSPtr tmp = m_Stmts.top();
		m_Stmts.pop();
		return tmp;
	}

	ITrExprSPtr AstToITrLowering::PopExpr()
	{
		ITrExprSPtr tmp = m_Exprs.top();
		m_Exprs.pop();
		return tmp;
	}

	ITrTypeSPtr AstToITrLowering::PopType()
	{
		ITrTypeSPtr tmp = m_Types.top();
		m_Types.pop();
		return tmp;
	}

	ITrPatternSPtr AstToITrLowering::PopPattern()
	{
		ITrPatternSPtr tmp = m_Patterns.top();
		m_Patterns.pop();
		return tmp;
	}

	ITrAttribsSPtr AstToITrLowering::PopAttribs()
	{
		ITrAttribsSPtr tmp = m_Attribs.top();
		m_Attribs.pop();
		return tmp;
	}

	ITrGenDeclSPtr AstToITrLowering::PopGenDecl()
	{
		ITrGenDeclSPtr tmp = m_GenDecls.top();
		m_GenDecls.pop();
		return tmp;
	} 
}
