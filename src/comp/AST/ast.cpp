#include "ast.hpp"
#include "astvisitor.hpp"

namespace Noctis
{
	AstNode::AstNode(AstNodeKind nodeKind, u64 startTokIdx, u64 endTokIdx)
		: nodeKind(nodeKind)
		, startTokIdx(startTokIdx)
		, endTokIdx(endTokIdx)
		, pCtx(new AstContext{})
	{
	}

	AstNode::~AstNode()
	{
	}

	bool AstNode::IsDeclaration() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::DeclStartMarker) && u64(nodeKind) <= u64(AstNodeKind::DeclEndMarker);
	}

	bool AstNode::IsStatement() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::StmtStartMarker) && u64(nodeKind) <= u64(AstNodeKind::StmtEndMarker);
	}

	bool AstNode::IsExpression() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::ExprStartMarker) && u64(nodeKind) <= u64(AstNodeKind::ExprEndMarker);
	}

	bool AstNode::IsType() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::TypeStartMarker) && u64(nodeKind) <= u64(AstNodeKind::TypeEndMarker);
	}

	bool AstNode::IsAttribute() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::AttributeStartMarker) && u64(nodeKind) <= u64(AstNodeKind::AttributeEndMarker);
	}

	bool AstNode::IsGeneric() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::GenericsStartMarker) && u64(nodeKind) <= u64(AstNodeKind::GenericsEndMarker);
	}

	bool AstNode::IsMacro() const
	{
		return u64(nodeKind) >= u64(AstNodeKind::MacroStartMarker) && u64(nodeKind) <= u64(AstNodeKind::MacroEndMarker);
	}

	void AstNode::Visit(AstVisitor& visitor)
	{
	}

	AstModuleDecl::AstModuleDecl(u64 moduleTokIdx, StdVector<StdString>&& moduleIdens, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::ModuleDecl, moduleTokIdx, semicolonTokIdx)
		, moduleIdens(std::move(moduleIdens))
	{
	}

	void AstModuleDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;
		
		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstIdentifier::AstIdentifier(u64 idenIdx, StdString&& iden)
		: AstNode(AstNodeKind::IdentifierType, idenIdx, idenIdx)
		, iden(std::move(iden))
	{
	}

	void AstIdentifier::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstParam::AstParam(u64 startTokIdx, StdVector<std::pair<AstNodeSPtr, StdString>>&& idens, AstNodeSPtr type, bool isVariadic, u64 endTokIdx)
		: AstNode(AstNodeKind::Param, startTokIdx, endTokIdx)
		, idens(std::move(idens))
		, type(type)
		, isVariadic(isVariadic)
	{
	}

	void AstParam::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (type)
			type->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstArg::AstArg(u64 startTokIdx, StdString&& iden, AstNodeSPtr expr)
		: AstNode(AstNodeKind::Arg, startTokIdx, expr->endTokIdx)
		, iden(std::move(iden))
		, expr(expr)
	{
	}

	void AstArg::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstStructDecl::AstStructDecl(AstNodeSPtr attribs, u64 structTokIdx, StdString&& iden, AstNodeSPtr generics,
	                             StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::StructDecl, attribs ? attribs->startTokIdx : structTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	void AstStructDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);

		for (AstNodeSPtr member : members)
		{
			member->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstUnionDecl::AstUnionDecl(AstNodeSPtr attribs, u64 unionTokIdx, StdString&& iden,
		AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::UnionDecl, attribs ? attribs->startTokIdx : unionTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	void AstUnionDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);
		
		for (AstNodeSPtr member : members)
		{
			member->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstValueEnumDecl::AstValueEnumDecl(AstNodeSPtr attribs, u64 enumTokIdx, StdString&& iden,
		StdVector<std::pair<StdString, AstNodeSPtr>>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::ValueEnumDecl, attribs ? attribs->startTokIdx : enumTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, members(std::move(members))
	{
	}

	void AstValueEnumDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);

		for (std::pair<StdString, AstNodeSPtr> member : members)
		{
			if (member.second)
				member.second->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstAdtEnumStructMember::AstAdtEnumStructMember(u64 lBraceTokIdx,
		StdVector<std::pair<StdVector<StdString>, AstNodeSPtr>>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::AdtEnumStructMember, lBraceTokIdx, rBraceTokIdx)
		, members(std::move(members))
	{
	}

	void AstAdtEnumStructMember::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstAdtEnumDecl::AstAdtEnumDecl(AstNodeSPtr attribs, u64 enumTokIdx, StdString&& iden,
	                               AstNodeSPtr generics, StdVector<std::pair<StdString, AstNodeSPtr>>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::AdtEnumDecl, attribs ? attribs->startTokIdx : enumTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	void AstAdtEnumDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);

		for (std::pair<StdString, AstNodeSPtr> member : members)
		{
			if (member.second)
				member.second->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMarkerInterfaceDecl::AstMarkerInterfaceDecl(AstNodeSPtr attribs, u64 interfaceTokIdx, StdString&& iden,
	                                               u64 semicolonTokIdx)
		: AstNode(AstNodeKind::MarkerInterfaceDecl, attribs ? attribs->startTokIdx : interfaceTokIdx, semicolonTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		
	{
	}

	void AstMarkerInterfaceDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstWeakInterfaceDecl::AstWeakInterfaceDecl(AstNodeSPtr attribs, u64 weakTokIdx, StdString&& iden,
		StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::StrongInterfaceDecl, attribs ? attribs->startTokIdx : weakTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, members(std::move(members))
	{
	}

	void AstWeakInterfaceDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);

		for (AstNodeSPtr member : members)
		{
			member->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstStrongInterfaceDecl::AstStrongInterfaceDecl(AstNodeSPtr attribs, u64 interfaceTokIdx, StdString&& iden,
		AstNodeSPtr generics, StdVector<AstNodeSPtr>&& members, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::WeakInterfaceDecl, attribs ? attribs->startTokIdx : interfaceTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, members(std::move(members))
	{
	}

	void AstStrongInterfaceDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);

		for (AstNodeSPtr member : members)
		{
			member->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTypeAliasDecl::AstTypeAliasDecl(AstNodeSPtr attribs, u64 typealiasTokIdx, StdString&& iden, AstNodeSPtr generics,
		AstNodeSPtr type, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::TypeAliasDecl, attribs ? attribs->startTokIdx : typealiasTokIdx, semicolonTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, type(type)
	{
	}

	void AstTypeAliasDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);
		type->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTypeDefDecl::AstTypeDefDecl(AstNodeSPtr attribs, u64 typedefTokIdx, StdString&& iden, AstNodeSPtr generics,
		AstNodeSPtr type, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::TypeDefDecl, attribs ? attribs->startTokIdx : typedefTokIdx, semicolonTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, type(type)
	{
	}

	void AstTypeDefDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);
		type->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstVarDecl::AstVarDecl(AstNodeSPtr attribs, u64 startTokIdx, StdVector<StdString>&& idens, AstNodeSPtr type, AstNodeSPtr expr, u64 termTokIdx)
		: AstNode(AstNodeKind::VarDecl, startTokIdx, termTokIdx)
		, attribs(attribs)
		, idens(std::move(idens))
		, type(type)
		, expr(expr)
	{
	}

	void AstVarDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (type)
			type->Visit(visitor);
		if (expr)
			expr->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstFuncDecl::AstFuncDecl(AstNodeSPtr attribs, u64 funcTokIdx, StdString&& identifier, AstNodeSPtr generics,
		StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements,
		u64 rBraceTokIdx)
		: AstNode(AstNodeKind::FuncDecl, attribs ? attribs->startTokIdx : funcTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, iden(std::move(iden))
		, generics(generics)
		, params(std::move(params))
		, ret(ret)
		, whereClause(whereClause)
		, statements(std::move(statements))
	{
	}

	void AstFuncDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);

		for (AstNodeSPtr arg : params)
		{
			arg->Visit(visitor);
		}

		if (ret)
			ret->Visit(visitor);
		if (whereClause)
			whereClause->Visit(visitor);

		for (AstNodeSPtr statement : statements)
		{
			statement->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMethodDecl::AstMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec, StdString&& identifier,
		AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, AstNodeSPtr whereClause,
		StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::MethodDecl, attribs ? attribs->startTokIdx : funcTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, rec(rec)
		, iden(std::move(iden))
		, generics(generics)
		, params(std::move(params))
		, ret(ret)
		, whereClause(whereClause)
		, statements(std::move(statements))
	{
	}

	void AstMethodDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);

		for (AstNodeSPtr arg : params)
		{
			arg->Visit(visitor);
		}

		if (ret)
			ret->Visit(visitor);
		if (whereClause)
			whereClause->Visit(visitor);

		for (AstNodeSPtr statement : statements)
		{
			statement->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstEmptyMethodDecl::AstEmptyMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec,
		StdString&& identifier, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret,
		AstNodeSPtr whereClause, StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::MethodDecl, attribs ? attribs->startTokIdx : funcTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, rec(rec)
		, iden(std::move(iden))
		, generics(generics)
		, params(std::move(params))
		, ret(ret)
		, whereClause(whereClause)
		, statements(std::move(statements))
	{
	}

	void AstEmptyMethodDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);

		for (AstNodeSPtr arg : params)
		{
			arg->Visit(visitor);
		}

		if (ret)
			ret->Visit(visitor);
		if (whereClause)
			whereClause->Visit(visitor);

		for (AstNodeSPtr statement : statements)
		{
			statement->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstImplDecl::AstImplDecl(AstNodeSPtr attribs, u64 implTokIdx, AstNodeSPtr generics, AstNodeSPtr type, StdVector<AstNodeSPtr>&& interfaces,
	                         StdVector<AstNodeSPtr>&& statements, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::ImplDecl, attribs ? attribs->startTokIdx : implTokIdx, rBraceTokIdx)
		, attribs(attribs)
		, generics(generics)
		, type(type)
		, interfaces(std::move(interfaces))
		, statements(std::move(statements))
	{
	}

	void AstImplDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		if (generics)
			generics->Visit(visitor);
		if (type)
			type->Visit(visitor);

		for (AstNodeSPtr interface : interfaces)
		{
			interface->Visit(visitor);
		}

		for (AstNodeSPtr statement : statements)
		{
			statement->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstImportStmt::AstImportStmt(AstNodeSPtr attribs, u64 importTokIdx, StdVector<StdString>&& moduleIdens,
		StdVector<std::pair<StdVector<StdString>, StdString>>&& symbols, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::ImportStmt, attribs ? attribs->startTokIdx : importTokIdx, semicolonTokIdx)
		, attribs(attribs)
		, moduleIdens(std::move(moduleIdens))
		, symbols(std::move(symbols))
	{
	}

	void AstImportStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (attribs)
			attribs->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstBlockStmt::AstBlockStmt(u64 lBraceTokIds, StdVector<StdSharedPtr<AstNode>>&& statements, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::BlockStmt, lBraceTokIds, rBraceTokIdx)
		, statements(std::move(statements))
	{
	}

	void AstBlockStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (std::shared_ptr<AstNode> pNode : statements)
		{
			pNode->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstIfStmt::AstIfStmt(u64 ifTokIdx, AstNodeSPtr decl, AstNodeSPtr cond, AstNodeSPtr body, AstNodeSPtr elseBody)
		: AstNode(AstNodeKind::IfStmt, ifTokIdx, elseBody ? elseBody->endTokIdx : body->endTokIdx)
		, decl(decl)
		, cond(cond)
		, body(body)
		, elseBody(elseBody)
	{
	}

	void AstIfStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (decl)
			decl->Visit(visitor);
		cond->Visit(visitor);
		body->Visit(visitor);
		if (elseBody)
			elseBody->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstLoopStmt::AstLoopStmt(AstNodeSPtr label, u64 loopTokIdx, AstNodeSPtr body)
		: AstNode(AstNodeKind::LoopStmt, label ? label->startTokIdx : loopTokIdx, body->endTokIdx)
		, label(label)
		, body(body)
	{
	}

	void AstLoopStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (label)
			label->Visit(visitor);
		body->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstWhileStmt::AstWhileStmt(AstNodeSPtr label, u64 whileTokIdx, AstNodeSPtr cond, AstNodeSPtr body)
		: AstNode(AstNodeKind::WhileStmt, label ? label->startTokIdx : whileTokIdx, body->endTokIdx)
		, label(label)
		, cond(cond)
		, body(body)
	{
	}

	void AstWhileStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (label)
			label->Visit(visitor);
		cond->Visit(visitor);
		body->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstDoWhileStmt::AstDoWhileStmt(AstNodeSPtr label, u64 doTokIdx, AstNodeSPtr body, AstNodeSPtr cond, u64 endIdx)
		: AstNode(AstNodeKind::WhileStmt, label ? label->startTokIdx : doTokIdx, endIdx)
		, label(label)
		, body(body)
		, cond(cond)
	{
	}

	void AstDoWhileStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (label)
			label->Visit(visitor);
		body->Visit(visitor);
		cond->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstForStmt::AstForStmt(AstNodeSPtr label, u64 forTokIdx, AstNodeSPtr init, AstNodeSPtr cond, AstNodeSPtr inc, AstNodeSPtr body)
		: AstNode(AstNodeKind::ForStmt, label ? label->startTokIdx : forTokIdx, body->endTokIdx)
		, label(label)
		, init(init)
		, cond(cond)
		, inc(inc)
		, body(body)
	{
	}

	void AstForStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (label)
			label->Visit(visitor);
		if (init)
			init->Visit(visitor);
		cond->Visit(visitor);
		if (inc)
			inc->Visit(visitor);
		body->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstForRangeStmt::AstForRangeStmt(AstNodeSPtr label, u64 forTokIdx, StdVector<StdString>&& idens, AstNodeSPtr range, AstNodeSPtr body)
		: AstNode(AstNodeKind::ForRangeStmt, label ? label->startTokIdx : forTokIdx, body->endTokIdx)
		, label(label)
		, idens(std::move(idens))
		, range(range)
		, body(body)
	{
	}

	void AstForRangeStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (label)
			label->Visit(visitor);
		range->Visit(visitor);
		body->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstSwitchStmt::AstSwitchStmt(AstNodeSPtr label, u64 switchTokIdx, StdVector<AstSwitchCase>&& cases, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::SwitchStmt, label ? label->startTokIdx : switchTokIdx, rBraceTokIdx)
		, label(label)
		, cases(std::move(cases))
	{
	}

	void AstSwitchStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (label)
			label->Visit(visitor);
		for (AstSwitchCase& case_ : cases)
		{
			case_.staticExpr->Visit(visitor);
			case_.dynamicExpr->Visit(visitor);
			case_.body->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstLabelStmt::AstLabelStmt(u64 startTokIdx, StdString&& iden, u64 endTokIdx)
		: AstNode(AstNodeKind::LabelStmt, startTokIdx, endTokIdx)
		, iden(std::move(iden))
	{
	}

	void AstLabelStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstBreakStmt::AstBreakStmt(u64 breakTokIdx, StdString&& iden, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::BreakStmt, breakTokIdx, semicolonTokIdx)
		, iden(std::move(iden))
	{
	}

	void AstBreakStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstContinueStmt::AstContinueStmt(u64 continueTokIdx, StdString&& iden, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::ContinueStmt, continueTokIdx, semicolonTokIdx)
		, iden(std::move(iden))
	{
	}

	void AstContinueStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstFallthroughStmt::AstFallthroughStmt(u64 fallthroughTokIdx, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::FallthroughStmt, fallthroughTokIdx, semicolonTokIdx)
	{
	}

	void AstFallthroughStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstGotoStmt::AstGotoStmt(u64 gotoTokIdx, StdString&& iden, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::GotoStmt, gotoTokIdx, semicolonTokIdx)
		, iden(std::move(iden))
	{
	}

	void AstGotoStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstReturnStmt::AstReturnStmt(u64 returnTokIdx, AstNodeSPtr expr, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::ReturnStmt, returnTokIdx, semicolonTokIdx)
		, expr(expr)
	{
	}

	void AstReturnStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstExprStmt::AstExprStmt(AstNodeSPtr expr, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::ExprStmt, expr ? expr->startTokIdx : semicolonTokIdx, semicolonTokIdx)
		, expr(expr)
	{
	}

	void AstExprStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (expr)
			expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstDeferStmt::AstDeferStmt(u64 deferTokIdx, AstNodeSPtr expr, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::DeferStmt, deferTokIdx, semicolonTokIdx)
		, expr(expr)
	{
	}

	void AstDeferStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstStackDeferStmt::AstStackDeferStmt(u64 deferTokIdx, AstNodeSPtr expr, u64 semicolonTokIdx)
		: AstNode(AstNodeKind::StackDeferStmt, deferTokIdx, semicolonTokIdx)
		, expr(expr)
	{
	}

	void AstStackDeferStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstUnsafeStmt::AstUnsafeStmt(u64 unsafeTokIdx, StdVector<AstNodeSPtr>&& stmts, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::DeferStmt, unsafeTokIdx, rBraceTokIdx)
		, stmts(std::move(stmts))
	{
	}

	void AstUnsafeStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr stmt : stmts)
		{
			stmt->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCompIfStmt::AstCompIfStmt(u64 hashTokIdx, AstNodeSPtr decl, AstNodeSPtr expr, AstNodeSPtr body,
	                             AstNodeSPtr elseBody)
		: AstNode(AstNodeKind::CompIfStmt, hashTokIdx, elseBody ? elseBody->endTokIdx : body->endTokIdx)
		, decl(decl)
		, expr(expr)
		, body(body)
		, elseBody(elseBody)
	{
	}

	void AstCompIfStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (decl)
			decl->Visit(visitor);
		expr->Visit(visitor);
		body->Visit(visitor);
		if (elseBody)
			elseBody->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCompCondStmt::AstCompCondStmt(u64 hashTokIdx, Token cond, AstNodeSPtr body, AstNodeSPtr elseBody)
		: AstNode(AstNodeKind::CompForStmt, hashTokIdx, elseBody ? elseBody->endTokIdx : body->endTokIdx)
		, cond(cond)
		, body(body)
		, elseBody(elseBody)
	{
	}

	void AstCompCondStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		body->Visit(visitor);
		if (elseBody)
			elseBody->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCompDebugStmt::AstCompDebugStmt(u64 hashTokIdx, Token cond, AstNodeSPtr body, AstNodeSPtr elseBody)
		: AstNode(AstNodeKind::CompForStmt, hashTokIdx, elseBody ? elseBody->endTokIdx : body->endTokIdx)
		, cond(cond)
		, body(body)
		, elseBody(elseBody)
	{
	}

	void AstCompDebugStmt::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		body->Visit(visitor);
		if (elseBody)
			elseBody->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstAssignExpr::AstAssignExpr(AstNodeSPtr lExpr, TokenType op, AstNodeSPtr rExpr)
		: AstNode(AstNodeKind::AssignExpr, lExpr->startTokIdx, rExpr->endTokIdx)
		, lExpr(lExpr)
		, op(op)
		, rExpr(rExpr)
	{
	}

	void AstAssignExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		lExpr->Visit(visitor);
		rExpr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTernaryExpr::AstTernaryExpr(AstNodeSPtr cond, AstNodeSPtr trueExpr, AstNodeSPtr falseExpr)
		: AstNode(AstNodeKind::TernaryExpr, cond->startTokIdx, falseExpr->endTokIdx)
		, cond(cond)
		, trueExpr(trueExpr)
		, falseExpr(falseExpr)
	{
	}

	void AstTernaryExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		cond->Visit(visitor);
		trueExpr->Visit(visitor);
		falseExpr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstBinaryExpr::AstBinaryExpr(AstNodeSPtr lExpr, TokenType op, AstNodeSPtr rExpr)
		: AstNode(AstNodeKind::BinaryExpr, lExpr->startTokIdx, rExpr->endTokIdx)
		, lExpr(lExpr)
		, op(op)
		, rExpr(rExpr)
	{
	}

	void AstBinaryExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		lExpr->Visit(visitor);
		rExpr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstPostfixExpr::AstPostfixExpr(AstNodeSPtr expr, TokenType op, u64 opTokIdx)
		: AstNode(AstNodeKind::PostFixExpr, expr->startTokIdx, opTokIdx)
		, expr(expr)
		, op(op)
	{
	}

	void AstPostfixExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstPrefixExpr::AstPrefixExpr(TokenType op, u64 opTokIdx, AstNodeSPtr expr)
		: AstNode(AstNodeKind::PrefixExpr, opTokIdx, expr->endTokIdx)
		, op(op)
		, expr(expr)
	{
	}

	void AstPrefixExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstQualNameExpr::AstQualNameExpr(u64 startTokIdx, StdVector<AstNodeSPtr>&& idens, u64 endTokIdx)
		: AstNode(AstNodeKind::QualNameExpr, startTokIdx, endTokIdx)
		, idens(std::move(idens))
	{
	}

	void AstQualNameExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstIndexSliceExpr::AstIndexSliceExpr(AstNodeSPtr expr, bool nullCoalesce, AstNodeSPtr index, u64 rBracketTokIdx)
		: AstNode(AstNodeKind::IndexSliceExpr, expr->startTokIdx, rBracketTokIdx)
		, expr(expr)
		, nullCoalesce(nullCoalesce)
		, index(index)
	{
	}

	void AstIndexSliceExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		index->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstSliceExpr::AstSliceExpr(AstNodeSPtr expr, bool nullCoalesce, AstNodeSPtr begin, AstNodeSPtr end, u64 rBracketTokIdx)
		: AstNode(AstNodeKind::SliceExpr, expr->startTokIdx, rBracketTokIdx)
		, expr(expr)
		, nullCoalesce(nullCoalesce)
		, begin(begin)
		, end(end)
	{
	}

	void AstSliceExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		begin->Visit(visitor);
		end->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstFuncCallExpr::AstFuncCallExpr(AstNodeSPtr func, StdVector<AstNodeSPtr>&& args, u64 rParenTokIdx)
		: AstNode(AstNodeKind::FuncCallExpr, func->startTokIdx, rParenTokIdx)
		, func(func)
		, args(std::move(args))
	{
	}

	void AstFuncCallExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		func->Visit(visitor);
		for (AstNodeSPtr arg : args)
		{
			arg->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMemberAccessExpr::AstMemberAccessExpr(AstNodeSPtr caller, bool nullCoalesce, StdString&& iden, u64 idenTokIdx)
		: AstNode(AstNodeKind::MemberAccessExpr, caller->startTokIdx, idenTokIdx)
		, caller(caller)
		, nullCoalesc(nullCoalesc)
		, iden(std::move(iden))
	{
	}

	void AstMemberAccessExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		caller->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMethodCallExpr::AstMethodCallExpr(AstNodeSPtr caller, bool nullCoalesce, StdString&& iden, StdVector<AstNodeSPtr>&& args,
	                                     u64 rParenTokIdx)
		: AstNode(AstNodeKind::MethodCallExpr, caller->startTokIdx, rParenTokIdx)
		, caller(caller)
		, nullCoalesc(nullCoalesc)
		, iden(std::move(iden))
		, args(std::move(args))
	{
	}

	void AstMethodCallExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		caller->Visit(visitor);
		for (AstNodeSPtr arg : args)
		{
			arg->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTupleAccessExpr::AstTupleAccessExpr(AstNodeSPtr expr, bool nullCoalesce, u16 index, u64 indexTokIdx)
		: AstNode(AstNodeKind::TupleAccessExpr, expr->startTokIdx, indexTokIdx)
		, expr(expr)
		, nullCoalesc(nullCoalesc)
		, index(index)
	{
	}

	void AstTupleAccessExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstLiteralExpr::AstLiteralExpr(Token literal)
		: AstNode(AstNodeKind::LiteralExpr, literal.Idx(), literal.Idx())
		, literal(literal)
	{
	}

	void AstLiteralExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstAggrInitExpr::AstAggrInitExpr(u64 startTokIdx, StdVector<AstNodeSPtr>&& idens, StdVector<AstNodeSPtr>&& args,
		u64 rBraceTokIdx)
		: AstNode(AstNodeKind::AggrInitExpr, startTokIdx, rBraceTokIdx)
		, idens(std::move(idens))
		, args(std::move(args))
	{
	}

	void AstAggrInitExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr arg : args)
		{
			arg->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTupleInitExpr::AstTupleInitExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 rParenTokIdx)
		: AstNode(AstNodeKind::TupleInitExpr, lParenTokIdx, rParenTokIdx)
		, exprs(std::move(exprs))
	{
	}

	void AstTupleInitExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr expr : exprs)
		{
			expr->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstArrayInitExpr::AstArrayInitExpr(u64 lBracketTokIdx, StdVector<AstNodeSPtr>&& exprs, u64 rBracketTokIdx)
		: AstNode(AstNodeKind::ArrayInitExpr, lBracketTokIdx, rBracketTokIdx)
		, exprs(std::move(exprs))
	{
	}

	void AstArrayInitExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr expr : exprs)
		{
			expr->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCastExpr::AstCastExpr(u64 castTokIdx, AstNodeSPtr type, AstNodeSPtr expr)
		: AstNode(AstNodeKind::CastExpr, castTokIdx, expr->endTokIdx)
		, type(type)
		, expr(expr)
	{
	}

	void AstCastExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		type->Visit(visitor);
		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTransmuteExpr::AstTransmuteExpr(u64 transmuteTokIdx, AstNodeSPtr type, AstNodeSPtr expr)
		: AstNode(AstNodeKind::TransmuteExpr, transmuteTokIdx, expr->endTokIdx)
		, type(type)
		, expr(expr)
	{
	}

	void AstTransmuteExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		type->Visit(visitor);
		expr->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMoveExpr::AstMoveExpr(u64 moveTokIdx, AstNodeSPtr expr)
		: AstNode(AstNodeKind::MoveExpr, moveTokIdx, expr->endTokIdx)
		, expr(expr)
	{
	}

	void AstMoveExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstBracketExpr::AstBracketExpr(u64 lBracketTokIdx, AstNodeSPtr expr, u64 rBracketTokIdx)
		: AstNode(AstNodeKind::BracketExpr, lBracketTokIdx, rBracketTokIdx)
		, expr(expr)
	{
	}

	void AstBracketExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstBlockExpr::AstBlockExpr(u64 lBraceTokIdx, StdVector<AstNodeSPtr>&& stmts, u64 rBraceExpr)
		: AstNode(AstNodeKind::BlockExpr, lBraceTokIdx, rBraceExpr)
		, stmts(std::move(stmts))
	{
	}

	void AstBlockExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr stmt : stmts)
		{
			stmt->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstUnsafeExpr::AstUnsafeExpr(u64 unsafeTokIdx, AstNodeSPtr expr, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::UnsafeExpr, unsafeTokIdx, rBraceTokIdx)
		, expr(expr)
	{
	}

	void AstUnsafeExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCommaExpr::AstCommaExpr(StdVector<AstNodeSPtr>&& exprs)
		: AstNode(AstNodeKind::CommaExpr, exprs.front()->startTokIdx, exprs.back()->endTokIdx)
		, exprs(std::move(exprs))
	{
	}

	void AstCommaExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr expr : exprs)
		{
			expr->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstClosureExpr::AstClosureExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& params,
		StdVector<AstClosureCapture>&& captures, StdVector<AstNodeSPtr> stmts, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::ClosureExpr, lParenTokIdx, rBraceTokIdx)
		, params(std::move(params))
		, captures(std::move(captures))
		, stmts(std::move(stmts))
	{
	}

	void AstClosureExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (std::pair<StdString, AstNodeSPtr>& param : params)
		{
			if (param.second)
				param.second->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCompRunExpr::AstCompRunExpr(u64 hashTokIdx, AstNodeSPtr expr)
		: AstNode(AstNodeKind::CompRunExpr, hashTokIdx, expr->endTokIdx)
		, expr(expr)
	{
	}

	void AstCompRunExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstBuiltinType::AstBuiltinType(const Token& tok)
		: AstNode(AstNodeKind::BuiltinType, tok.Idx(), tok.Idx())
		, type(tok.Type())
	{
	}

	void AstBuiltinType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstIdentifierType::AstIdentifierType(u64 startTokIdx, StdVector<AstNodeSPtr>&& idens, u64 endTokIdx)
		: AstNode(AstNodeKind::IdentifierType, startTokIdx, endTokIdx)
		, idens(std::move(idens))
	{
	}

	void AstIdentifierType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstPointerType::AstPointerType(u64 asteriskTokIdx, StdSharedPtr<AstNode> subType)
		: AstNode(AstNodeKind::PointerType, asteriskTokIdx, subType->endTokIdx)
		, subType(subType)
	{
	}

	void AstPointerType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstReferenceType::AstReferenceType(u64 andTokIdx, StdSharedPtr<AstNode> subType)
		: AstNode(AstNodeKind::ReferenceType, andTokIdx, subType->endTokIdx)
		, subType(subType)
	{
	}

	void AstReferenceType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		subType->Visit(visitor);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstArrayType::AstArrayType(u64 lBracketTokIdx, StdSharedPtr<AstNode> arraySize, StdSharedPtr<AstNode> subType)
		: AstNode(AstNodeKind::ArrayType, lBracketTokIdx, subType->endTokIdx)
		, arraySize(arraySize)
		, subType(subType)
	{
	}

	void AstArrayType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		arraySize->Visit(visitor);
		subType->Visit(visitor);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstSliceType::AstSliceType(u64 lBraceTokIdx, StdSharedPtr<AstNode> subType)
		: AstNode(AstNodeKind::SliceType, lBraceTokIdx, subType->endTokIdx)
		, subType(subType)
	{
	}

	void AstSliceType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		subType->Visit(visitor);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstTupleType::AstTupleType(u64 lParenTokIdx, StdVector<StdSharedPtr<AstNode>>&& subTypes, u64 rParenTokIdx)
		: AstNode(AstNodeKind::TupleType, lParenTokIdx, rParenTokIdx)
		, subTypes(std::move(subTypes))
	{
	}

	void AstTupleType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (StdSharedPtr<AstNode> subType : subTypes)
		{
			subType->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstOptionalType::AstOptionalType(u64 lBraceTokIdx, StdSharedPtr<AstNode> subType)
		: AstNode(AstNodeKind::OptionalType, lBraceTokIdx, subType->endTokIdx)
		, subType(subType)
	{
	}

	void AstOptionalType::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		subType->Visit(visitor);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstAttributes::AstAttributes(u64 startTokIdx, StdVector<AstNodeSPtr>&& compAttribs, StdVector<AstNodeSPtr>&& userAttribs,
		AstNodeSPtr visibility, StdVector<AstNodeSPtr>&& simpleAttribs, u64 endTokIdx)
		: AstNode(AstNodeKind::Attributes, startTokIdx, endTokIdx)
		, compAttribs(std::move(compAttribs))
		, userAttribs(std::move(userAttribs))
		, visibility(visibility)
		, simpleAttribs(std::move(simpleAttribs))
	{
	}

	void AstAttributes::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstCompAttribute::AstCompAttribute(u64 atColonTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args,
	                                   u64 endTokIdx)
		: AstNode(AstNodeKind::CompilerAttribute, atColonTokIdx, endTokIdx)
		, iden(std::move(iden))
		, args(std::move(args))
	{
	}

	void AstCompAttribute::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr arg : args)
		{
			arg->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstUserAttribute::AstUserAttribute(u64 atColonTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args,
		u64 endTokIdx)
		: AstNode(AstNodeKind::UserAttribute, atColonTokIdx, endTokIdx)
		, iden(std::move(iden))
		, args(std::move(args))
	{
	}

	void AstUserAttribute::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstVisibilityAttribute::AstVisibilityAttribute(u64 publicTokIdx, StdString&& kind, u64 endTokId)
		: AstNode(AstNodeKind::VisibilityAttribute, publicTokIdx, endTokId)
		, kind(kind)
	{
	}

	void AstVisibilityAttribute::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstSimpleAttribute::AstSimpleAttribute(u64 tokIdx, TokenType attrib)
		: AstNode(AstNodeKind::SimpleAttribute, tokIdx, tokIdx)
		, attrib(attrib)
	{
	}

	void AstSimpleAttribute::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstGenericDecl::AstGenericDecl(u64 startTokIdx, StdVector<AstNodeSPtr>&& params, u64 endTokIdx)
		: AstNode(AstNodeKind::GenericsDecl, startTokIdx, endTokIdx)
		, params(std::move(params))
	{
	}

	void AstGenericDecl::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr param : params)
		{
			param->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstGenericTypeParam::AstGenericTypeParam(u64 idenTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& implTypes,
		AstNodeSPtr defType)
		: AstNode(AstNodeKind::GenericsTypeParam, idenTokIdx, defType ? defType->endTokIdx : implTypes.size() ? implTypes.back()->endTokIdx : idenTokIdx)
		, iden(std::move(iden))
		, implTypes(std::move(implTypes))
		, defType(defType)
	{
	}

	void AstGenericTypeParam::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		
		for (AstNodeSPtr implType : implTypes)
		{
			implType->Visit(visitor);
		}

		if (defType)
			defType->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstGenericValueParam::AstGenericValueParam(u64 idenTokIdx, StdString&& iden, AstNodeSPtr type, AstNodeSPtr defExpr)
		: AstNode(AstNodeKind::GenericValueParam, idenTokIdx, defExpr ? defExpr->endTokIdx : type->endTokIdx)
		, iden(std::move(iden))
		, type(type)
		, defExpr(defExpr)
	{
	}

	void AstGenericValueParam::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		type->Visit(visitor);
		if (defExpr)
			defExpr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstGenericWhereClause::AstGenericWhereClause(u64 whereTokIdx, AstNodeSPtr expr)
		: AstNode(AstNodeKind::GenericWhereClause, whereTokIdx, expr->endTokIdx)
		, expr(expr)
	{
	}

	void AstGenericWhereClause::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstGenericInst::AstGenericInst(u64 startTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& args,
		u64 endTokIdx)
		: AstNode(AstNodeKind::GenericInst, startTokIdx, endTokIdx)
		, iden(std::move(iden))
		, args(std::move(args))
	{
	}

	void AstGenericInst::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr arg : args)
		{
			arg->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMacroVar::AstMacroVar(u64 dollarTokIdx, StdString&& iden, AstMacroVarKind kind, u64 kindTokIdx)
		: AstNode(AstNodeKind::MacroVar, dollarTokIdx, kindTokIdx)
		, iden(std::move(iden))
		, kind(kind)
	{
	}

	void AstMacroVar::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMacroSeparator::AstMacroSeparator(StdVector<Token>&& toks)
		: AstNode(AstNodeKind::MacroSeparator, toks.front().Idx(), toks.back().Idx())
		, toks(std::move(toks))
	{
	}

	void AstMacroSeparator::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMacroFragment::AstMacroFragment(u64 startIdx, AstNodeSPtr subPattern, TokenType repType,
		u64 endTokIdx)
		: AstNode(AstNodeKind::MacroFragment, startIdx, endTokIdx)
		, subPattern(subPattern)
		, repType(repType)
	{
	}

	void AstMacroFragment::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		subPattern->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMacroPattern::AstMacroPattern(u64 startTokIdx, StdVector<AstNodeSPtr>&& elems, u64 endTokIdx)
		: AstNode(AstNodeKind::MacroPattern, startTokIdx, endTokIdx)
		, elems(std::move(elems))
	{
	}

	void AstMacroPattern::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr elem : elems)
		{
			elem->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMacroRule::AstMacroRule(u64 startTokIdx, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body, u64 endTokIdx)
		: AstNode(AstNodeKind::MacroRule, startTokIdx, endTokIdx)
		, pattern(pattern)
		, body(std::move(body))
	{
	}

	void AstMacroRule::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		pattern->Visit(visitor);
		for (AstNodeSPtr elem : body)
		{
			elem->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstDeclMacro::AstDeclMacro(u64 macroTokIdx, StdString&& iden, AstNodeSPtr pattern, StdVector<AstNodeSPtr>&& body,
		u64 rBraceTokIdx)
		: AstNode(AstNodeKind::DeclMacro, macroTokIdx, rBraceTokIdx)
		, iden(std::move(iden))
		, pattern(pattern)
		, body(std::move(body))
	{
	}

	void AstDeclMacro::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		pattern->Visit(visitor);
		for (AstNodeSPtr elem : body)
		{
			elem->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstRulesDeclMacro::AstRulesDeclMacro(u64 macroTokIdx, StdString&& iden, StdVector<AstNodeSPtr>&& rules,
		u64 rBraceTokIdx)
		: AstNode(AstNodeKind::RulesDeclMacro, macroTokIdx, rBraceTokIdx)
		, iden(std::move(iden))
		, rules(std::move(rules))
	{
	}

	void AstRulesDeclMacro::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr rule : rules)
		{
			rule->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstProcMacro::AstProcMacro(u64 macroTokIdx, StdString&& iden, StdString&& tokStreamIden, AstNodeSPtr pattern,
		StdVector<AstNodeSPtr>&& body, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::DeclMacro, macroTokIdx, rBraceTokIdx)
		, iden(std::move(iden))
		, tokStreamIden(std::move(tokStreamIden))
		, pattern(pattern)
		, body(std::move(body))
	{
	}

	void AstProcMacro::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		pattern->Visit(visitor);
		for (AstNodeSPtr elem : body)
		{
			elem->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstRulesProcMacro::AstRulesProcMacro(u64 macroTokIdx, StdString&& iden, StdString&& tokStreamIden,
		StdVector<AstNodeSPtr>&& rules, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::RulesDeclMacro, macroTokIdx, rBraceTokIdx)
		, iden(std::move(iden))
		, tokStreamIden(std::move(tokStreamIden))
		, rules(std::move(rules))
	{
	}

	void AstRulesProcMacro::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr rule : rules)
		{
			rule->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	AstMacroInst::AstMacroInst(u64 startTokIdx, StdVector<StdString>&& idens, StdVector<Token>& toks, u64 endTokIdx)
		: AstNode(AstNodeKind::MacroInst, startTokIdx, endTokIdx)
		, idens(std::move(idens))
		, toks(std::move(toks))
	{
	}

	void AstMacroInst::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);
		visitor.Visit(*this, AstVisitLoc::End);
	}
}
