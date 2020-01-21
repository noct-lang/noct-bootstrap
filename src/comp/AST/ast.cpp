#include "ast.hpp"
#include "astvisitor.hpp"
#include "common/logger.hpp"

namespace Noctis
{
	bool IsNodeKindDeclaration(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::DeclStartMarker) && u64(kind) <= u64(AstNodeKind::DeclEndMarker);
	}

	bool IsNodeKindStatement(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::StmtStartMarker) && u64(kind) <= u64(AstNodeKind::StmtEndMarker);
	}

	bool IsNodeKindExpression(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::ExprStartMarker) && u64(kind) <= u64(AstNodeKind::ExprEndMarker);
	}

	bool IsNodeKindType(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::TypeStartMarker) && u64(kind) <= u64(AstNodeKind::TypeEndMarker);
	}

	bool IsNodeKindAttribute(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::AttributeStartMarker) && u64(kind) <= u64(AstNodeKind::AttributeEndMarker);
	}

	bool IsNodeKindGeneric(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::GenericsStartMarker) && u64(kind) <= u64(AstNodeKind::GenericsEndMarker);
	}

	bool IsNodeKindMacro(AstNodeKind kind)
	{
		return u64(kind) >= u64(AstNodeKind::MacroStartMarker) && u64(kind) <= u64(AstNodeKind::MacroEndMarker);
	}

	AstNode::AstNode(AstNodeKind nodeKind, u64 startTokIdx, u64 endTokIdx)
		: nodeKind(nodeKind)
		, startTokIdx(startTokIdx)
		, endTokIdx(endTokIdx)
		, ctx(new AstContext{})
	{
	}

	AstNode::~AstNode()
	{
	}

	void AstNode::Visit(AstVisitor& visitor)
	{
	}

	void AstNode::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(unknown-node)\n");
	}

	void AstNode::LogIndent(u32 indent)
	{
		for (u32 i = 0; i < indent; ++i)
		{
			g_Logger.Log("|   ");
		}
	}

	void AstTree::Visit(AstVisitor& visitor)
	{
		visitor.Visit(*this, AstVisitLoc::Begin);
		for (AstNodeSPtr& node : nodes)
		{
			node->Visit(visitor);
		}
		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstTree::Log()
	{
		g_Logger.Log("(ast-tree source='%s')\n", filepath.c_str());
		for (AstNodeSPtr node : nodes)
		{
			node->Log(1);
		}
		g_Logger.Log("(ast-tree end)\n");
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

	void AstModuleDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(module '");
		for (usize i = 0; i < moduleIdens.size(); ++i)
		{
			if (i != 0)
				g_Logger.Log(".");
			g_Logger.Log(moduleIdens[i]);
		}
		g_Logger.Log("')\n");
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

	void AstIdentifier::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(identifier '%s')\n", iden.c_str());
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

	void AstParam::Log(u32 indent)
	{
		LogIndent(indent);
		if (idens.size() == 1)
		{
			g_Logger.Log("(param '%s'%s)\n", idens[0].second.c_str(), isVariadic ? " variadic" : "");
			if (idens[0].first)
				idens[0].first->Log(indent + 1);
		}
		else
		{
			g_Logger.Log("(params)\n");
			for (std::pair<AstNodeSPtr, StdString>& pair : idens)
			{
				LogIndent(indent + 1);
				g_Logger.Log("(param '%s')\n", pair.second.c_str());
				if (pair.first)
					pair.first->Log(indent + 2);
			}
		}

		if (type)
			type->Log(indent + 1);
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

	void AstArg::Log(u32 indent)
	{
		LogIndent(indent);
		if (iden.empty())
			g_Logger.Log("(arg)\n");
		else
			g_Logger.Log("(arg '%s')\n", iden.c_str());
		expr->Log(indent + 1);
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

	void AstStructDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(struct-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		if (generics)
			generics->Log(indent + 1);
		for (AstNodeSPtr member : members)
		{
			member->Log(indent + 1);
		}
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

	void AstUnionDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(union-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		if (generics)
			generics->Log(indent + 1);
		for (AstNodeSPtr member : members)
		{
			member->Log(indent + 1);
		}
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

	void AstValueEnumDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(value-enum-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		for (std::pair<StdString, AstNodeSPtr>& member : members)
		{
			LogIndent(indent + 1);
			g_Logger.Log("(value-enum-member '%s')\n", member.first.c_str());
			if (member.second)
				member.second->Log(indent + 2);
		}
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

	void AstAdtEnumStructMember::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(adt-enum-member-struct)\n");
		for (std::pair<StdVector<StdString>, AstNodeSPtr>& member : members)
		{
			for (StdString& iden : member.first)
			{
				LogIndent(indent + 1);
				g_Logger.Log("(elem '%s')\n", iden.c_str());
			}
			member.second->Log(indent + 2);
		}
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

	void AstAdtEnumDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(adt-enum-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		if (generics)
			generics->Log(indent + 1);
		for (std::pair<StdString, AstNodeSPtr>& member : members)
		{
			LogIndent(indent + 1);
			g_Logger.Log("(adt-enum-member '%s')\n", member.first.c_str());
			if (member.second)
				member.second->Log(indent + 2);
		}
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

	void AstMarkerInterfaceDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(marker-interface-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
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

	void AstWeakInterfaceDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(weak-interface-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		for (AstNodeSPtr member : members)
		{
			member->Log(indent + 1);
		}
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

	void AstStrongInterfaceDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(strong-interface-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		if (generics)
			generics->Log(indent + 1);
		for (AstNodeSPtr member : members)
		{
			member->Log(indent + 1);
		}
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

	void AstTypeAliasDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(typealias-decl '%s')\n", iden.c_str());
		if (type)
			type->Log(indent + 1);
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

	void AstTypeDefDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(typedef-decl '%s')\n", iden.c_str());
		type->Log(indent + 1);
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

	void AstVarDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(var-decl)\n");
		if (attribs)
			attribs->Log(indent + 1);
		for (StdString& iden : idens)
		{
			LogIndent(indent + 1);
			g_Logger.Log("(var '%s')\n", iden.c_str());
		}
		if (type)
			type->Log(indent + 1);
		if (expr)
			expr->Log(indent + 1);
	}

	AstFuncDecl::AstFuncDecl(AstNodeSPtr attribs, u64 funcTokIdx, StdString&& iden, AstNodeSPtr generics,
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

	void AstFuncDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(func-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		if (generics)
			generics->Log(indent + 1);
		for (AstNodeSPtr& param : params)
		{
			param->Log(indent + 1);
		}
		if (ret)
			ret->Log(indent + 1);
		if (whereClause)
			whereClause->Log(indent + 1);

		LogIndent(indent + 1);
		g_Logger.Log("(body)\n");
		for (AstNodeSPtr stmt : statements)
		{
			stmt->Log(indent + 2);
		}
	}

	AstMethodDecl::AstMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec, StdString&& iden,
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

	void AstMethodDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(method-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		switch (rec)
		{
		case AstMethodReceiverKind::None:
			LogIndent(indent + 1);
			g_Logger.Log("(rec none)\n");
			break;
		case AstMethodReceiverKind::Value:
			LogIndent(indent + 1);
			g_Logger.Log("(rec value)\n");
			break;
		case AstMethodReceiverKind::Ref:
			LogIndent(indent + 1);
			g_Logger.Log("(rec ref)\n");
			break;
		case AstMethodReceiverKind::ConstRef:
			LogIndent(indent + 1);
			g_Logger.Log("(rec const-ref)\n");
			break;
		default: ;
		}
		for (AstNodeSPtr& param : params)
		{
			param->Log(indent + 1);
		}
		if (ret)
			ret->Log(indent + 1);
		if (whereClause)
			whereClause->Log(indent + 1);

		LogIndent(indent + 1);
		g_Logger.Log("(body)\n");
		for (AstNodeSPtr stmt : statements)
		{
			stmt->Log(indent + 2);
		}
	}

	AstEmptyMethodDecl::AstEmptyMethodDecl(AstNodeSPtr attribs, u64 funcTokIdx, AstMethodReceiverKind rec,
	                                       StdString&& iden, AstNodeSPtr generics, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret,
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

	void AstEmptyMethodDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(empty-method-decl '%s')\n", iden.c_str());
		if (attribs)
			attribs->Log(indent + 1);
		switch (rec)
		{
		case AstMethodReceiverKind::None:
			LogIndent(indent + 1);
			g_Logger.Log("(rec none)\n");
			break;
		case AstMethodReceiverKind::Value:
			LogIndent(indent + 1);
			g_Logger.Log("(rec value)\n");
			break;
		case AstMethodReceiverKind::Ref:
			LogIndent(indent + 1);
			g_Logger.Log("(rec ref)\n");
			break;
		case AstMethodReceiverKind::ConstRef:
			LogIndent(indent + 1);
			g_Logger.Log("(rec const-ref)\n");
			break;
		default:;
		}
		for (AstNodeSPtr& param : params)
		{
			param->Log(indent + 1);
		}
		if (ret)
			ret->Log(indent + 1);
		if (whereClause)
			whereClause->Log(indent + 1);
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

	void AstImplDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(impl-decl)");
		if (attribs)
			attribs->Log(indent + 1);
		if (generics)
			generics->Log(indent + 1);
		type->Log(indent + 1);
		if (!interfaces.empty())
		{
			LogIndent(indent + 1);
			g_Logger.Log("(interfaces)");
			for (AstNodeSPtr interface : interfaces)
			{
				interface->Log(indent + 2);
			}
		}
		for (AstNodeSPtr stmt : statements)
		{
			stmt->Log(indent + 1);
		}
		
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

	void AstImportStmt::Log(u32 indent)
	{
		LogIndent(indent);
		StdString iden;
		for (StdString& moduleIden : moduleIdens)
		{
			if (!iden.empty())
				iden += '.';
			iden += moduleIden;
		}
		g_Logger.Log("(import '%s')\n", iden.c_str());
		for (std::pair<StdVector<StdString>, StdString>& pair : symbols)
		{
			StdString aliasStr;
			if (!pair.second.empty())
				aliasStr = Format(" alias='%s'", pair.second.c_str());
			LogIndent(indent + 1);
			StdString symbolIden;
			for (StdString& symIden : pair.first)
			{
				if (!symbolIden.empty())
					symbolIden += "::";
				symbolIden += symIden;
			}
			g_Logger.Log("(alias '%s'%s)\n", symbolIden.c_str(), aliasStr.c_str());
		}
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

	void AstBlockStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(block-stmt)\n");
		for (AstNodeSPtr stmt : statements)
		{
			stmt->Log(indent + 1);
		}
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

	void AstIfStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(if-statement)\n");
		if (decl)
			decl->Log(indent + 1);
		cond->Log(indent + 1);
		body->Log(indent + 1);
		if (elseBody)
			elseBody->Log(indent + 1);
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

	void AstLoopStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(loop-stmt)\n");
		if (label)
			label->Log(indent + 1);
		body->Log(indent + 1);
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

	void AstWhileStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(while-stmt)\n");
		if (label)
			label->Log(indent + 1);
		cond->Log(indent + 1);
		body->Log(indent + 1);
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

	void AstDoWhileStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(do-while-stmt)\n");
		if (label)
			label->Log(indent + 1);
		body->Log(indent + 1);
		cond->Log(indent + 1);
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

	void AstForStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(for-stmt)\n");
		if (label)
			label->Log(indent + 1);
		if (init)
			init->Log(indent + 1);
		cond->Log(indent + 1);
		if (inc)
			inc->Log(indent + 1);
		body->Log(indent + 1);
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

	void AstForRangeStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(for-range-stmt)\n");
		if (label)
			label->Log(indent + 1);
		for (StdString& iden : idens)
		{
			LogIndent(indent + 1);
			g_Logger.Log("(iden '%s')\n", iden.c_str());
		}
		range->Log(indent + 1);
		body->Log(indent + 1);
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
			if (case_.dynamicExpr)
				case_.dynamicExpr->Visit(visitor);
			case_.body->Visit(visitor);
		}

		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstSwitchStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(switch-stmt)\n");
		for (AstSwitchCase& case_ : cases)
		{
			LogIndent(indent + 1);
			g_Logger.Log("(case)\n");
			case_.staticExpr->Log(indent + 2);
			if (case_.dynamicExpr)
				case_.dynamicExpr->Log(indent + 2);
			case_.body->Log(indent + 2);
		}
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

	void AstLabelStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(label-stmt '%s')\n", iden.c_str());
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

	void AstBreakStmt::Log(u32 indent)
	{
		LogIndent(indent);
		if (iden.empty())
			g_Logger.Log("(break-stmt)\n");
		else
			g_Logger.Log("(break-stmt '%s')\n", iden.c_str());
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

	void AstContinueStmt::Log(u32 indent)
	{
		LogIndent(indent);
		if (iden.empty())
			g_Logger.Log("(continue-stmt)\n");
		else
			g_Logger.Log("(continue-stmt '%s')\n", iden.c_str());
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

	void AstFallthroughStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(fallthrough-stmt)\n");
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

	void AstGotoStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(goto-stmt '%s')\n", iden.c_str());
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

	void AstReturnStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(return-stmt)\n");
		expr->Log(indent + 1);
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

	void AstExprStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(expr-stmt)\n");
		expr->Log(indent + 1);
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

	void AstDeferStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(defer-stmt)\n");
		expr->Log(indent + 1);
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

	void AstStackDeferStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(stack-defer-stmt)\n");
		expr->Log(indent + 1);
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

	void AstUnsafeStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(unsafe-stmt)\n");
		for (AstNodeSPtr stmt : stmts)
		{
			stmt->Log(indent + 1);
		}
	}

	AstCompIfStmt::AstCompIfStmt(u64 hashTokIdx, AstNodeSPtr decl, AstNodeSPtr expr, AstNodeSPtr body,
	                             AstNodeSPtr elseBody)
		: AstNode(AstNodeKind::CompIfStmt, hashTokIdx, elseBody ? elseBody->endTokIdx : body->endTokIdx)
		, decl(decl)
		, cond(expr)
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
		cond->Visit(visitor);
		body->Visit(visitor);
		if (elseBody)
			elseBody->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstCompIfStmt::Log(u32 indent)
	{

		LogIndent(indent);
		g_Logger.Log("(comp-if-statement)\n");
		if (decl)
			decl->Log(indent + 1);
		cond->Log(indent + 1);
		body->Log(indent + 1);
		if (elseBody)
			elseBody->Log(indent + 1);
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

	void AstCompCondStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(comp-conditional-stmt '%s')\n", cond.Text().c_str());
		body->Log(indent + 1);
		if (elseBody)
			elseBody->Log(indent + 1);
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

	void AstCompDebugStmt::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(comp-debug-stmt '%s')\n", cond.Text().c_str());
		body->Log(indent + 1);
		if (elseBody)
			elseBody->Log(indent + 1);
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

	void AstAssignExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(assign-expr %s)\n", GetTokenTypeName(op).data());
		lExpr->Log(indent + 1);
		rExpr->Log(indent + 1);
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

	void AstTernaryExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(ternary-expr %s)\n");
		cond->Log(indent + 1);
		trueExpr->Log(indent + 1);
		falseExpr->Log(indent + 1);
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

	void AstBinaryExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(binary-expr %s)\n", GetTokenTypeName(op).data());
		lExpr->Log(indent + 1);
		rExpr->Log(indent + 1);
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

	void AstPostfixExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(postfix-expr %s)\n", GetTokenTypeName(op).data());
		expr->Log(indent + 1);
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

	void AstPrefixExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(prefix-expr %s)\n", GetTokenTypeName(op).data());
		expr->Log(indent + 1);
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

	void AstQualNameExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(qual-name-expr)\n");
		for (AstNodeSPtr iden : idens)
		{
			iden->Log(indent + 1);
		}
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

	void AstIndexSliceExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(index-slice-expr%s)\n", nullCoalesce ? " nullcoalescing" : "");
		expr->Log(indent + 1);
		index->Log(indent + 1);
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
		if (begin)
			begin->Visit(visitor);
		if (end)
			end->Visit(visitor);
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstSliceExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(slice-expr%s)\n", nullCoalesce ? " nullcoalescing" : "");
		expr->Log(indent + 1);
		if (begin)
			begin->Log(indent + 1);
		if (end)
			end->Log(indent + 1);
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

	void AstFuncCallExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(func-call-expr)\n");
		func->Log(indent + 1);
		for (AstNodeSPtr arg : args)
		{
			arg->Log(indent);
		}
	}

	AstMemberAccessExpr::AstMemberAccessExpr(AstNodeSPtr caller, bool nullCoalesce, StdString&& iden, u64 idenTokIdx)
		: AstNode(AstNodeKind::MemberAccessExpr, caller->startTokIdx, idenTokIdx)
		, caller(caller)
		, nullCoalesce(nullCoalesce)
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

	void AstMemberAccessExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(member-access-expr '%s'%s)\n", iden.c_str(), nullCoalesce ? " nullcoalescing" : "");
		caller->Log(indent + 1);
	}

	AstMethodCallExpr::AstMethodCallExpr(AstNodeSPtr caller, bool nullCoalesce, StdString&& iden, StdVector<AstNodeSPtr>&& args,
	                                     u64 rParenTokIdx)
		: AstNode(AstNodeKind::MethodCallExpr, caller->startTokIdx, rParenTokIdx)
		, caller(caller)
		, nullCoalesce(nullCoalesce)
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

	void AstMethodCallExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(method-call-expr '%s'%s)\n", iden.c_str(), nullCoalesce ? " nullcoalescing" : "");
		caller->Log(indent + 1);
		for (AstNodeSPtr arg : args)
		{
			arg->Log(indent);
		}
	}

	AstTupleAccessExpr::AstTupleAccessExpr(AstNodeSPtr expr, bool nullCoalesce, u16 index, u64 indexTokIdx)
		: AstNode(AstNodeKind::TupleAccessExpr, expr->startTokIdx, indexTokIdx)
		, expr(expr)
		, nullCoalesce(nullCoalesce)
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

	void AstTupleAccessExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(tuple-access-expr %u%s)\n", index, nullCoalesce ? " nullcoalescing" : "");
		expr->Log(indent + 1);
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

	void AstLiteralExpr::Log(u32 indent)
	{
		LogIndent(indent);
		switch (literal.Type())
		{
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
			g_Logger.Log("(literal-expr %i %s)\n", literal.Signed(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::CharLit:
			g_Logger.Log("(literal-expr %u %s)\n", literal.Unsigned(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
			g_Logger.Log("(literal-expr %f %s)\n", literal.Fp(), GetTokenTypeName(literal.Type()).data());
			break;
		case TokenType::True:
			g_Logger.Log("(literal-expr true)\n");
			break;
		case TokenType::False:
			g_Logger.Log("(literal-expr false)\n");
			break;
		case TokenType::StringLit:
			g_Logger.Log("(literal-expr %s StringLit)\n", literal.Text());
			break;
		case TokenType::Null:
			g_Logger.Log("(literal-expr null)\n");
			break;
		case TokenType::Void:
			g_Logger.Log("(literal-expr void)\n");
			break;
		default:
			g_Logger.Log("(literal-expr unknown)\n");
			break;
			break;
		}
	}

	AstAggrInitExpr::AstAggrInitExpr(u64 startTokIdx, AstNodeSPtr type, StdVector<AstNodeSPtr>&& args,
	                                 u64 rBraceTokIdx)
		: AstNode(AstNodeKind::AggrInitExpr, startTokIdx, rBraceTokIdx)
		, type(type)
		, args(std::move(args))
	{
	}

	void AstAggrInitExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		if (type)
			type->Visit(visitor);
		for (AstNodeSPtr arg : args)
		{
			arg->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstAggrInitExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(aggr-init)\n");
		if (type)
			type->Log(indent + 1);
		for (AstNodeSPtr arg : args)
		{
			arg->Log(indent + 1);
		}
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

	void AstTupleInitExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(tuple-init)\n");
		for (AstNodeSPtr expr : exprs)
		{
			expr->Log(indent + 1);
		}
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

	void AstArrayInitExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(array-init)\n");
		for (AstNodeSPtr expr : exprs)
		{
			expr->Log(indent + 1);
		}
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

	void AstCastExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(cast-expr)\n");
		type->Log(indent + 1);
		expr->Log(indent + 1);
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

	void AstTransmuteExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(transmute-expr)\n");
		type->Log(indent + 1);
		expr->Log(indent + 1);
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

	void AstMoveExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(move-expr)\n");
		expr->Log(indent + 1);
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

	void AstBracketExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(bracket-expr)\n");
		expr->Log(indent + 1);
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

	void AstBlockExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(block-expr)\n");
		for (AstNodeSPtr stmt : stmts)
		{
			stmt->Log(indent + 1);
		}
	}

	AstUnsafeExpr::AstUnsafeExpr(u64 unsafeTokIdx, AstNodeSPtr expr)
		: AstNode(AstNodeKind::UnsafeExpr, unsafeTokIdx, expr->endTokIdx)
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

	void AstUnsafeExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(unsafe-expr)\n");
		expr->Log(indent + 1);
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

	void AstCommaExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(comma-expr)\n");
		for (AstNodeSPtr expr : exprs)
		{
			expr->Log(indent + 1);
		}
	}

	AstClosureExpr::AstClosureExpr(u64 lParenTokIdx, StdVector<AstNodeSPtr>&& params, AstNodeSPtr ret, StdVector<AstNodeSPtr> stmts, u64 rBraceTokIdx)
		: AstNode(AstNodeKind::ClosureExpr, lParenTokIdx, rBraceTokIdx)
		, params(std::move(params))
		, ret(ret)
		, stmts(std::move(stmts))
	{
	}

	void AstClosureExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		for (AstNodeSPtr& param : params)
		{
			param->Visit(visitor);
		}

		if (ret)
			ret->Visit(visitor);

		for (AstNodeSPtr stmt : stmts)
		{
			stmt->Visit(visitor);
		}
		
		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstClosureExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(closure-expr)\n");

		if (!params.empty())
		{
			LogIndent(indent + 1);
			g_Logger.Log("(params)\n");
			for (AstNodeSPtr param : params)
			{
				param->Log(indent + 2);
			}
		}
		if (ret)
			ret->Log(indent + 1);
		LogIndent(indent + 1);
		g_Logger.Log("(body)\n");
		for (AstNodeSPtr stmt : stmts)
		{
			stmt->Log(indent + 2);
		}
		
	}

	AstIsExpr::AstIsExpr(AstNodeSPtr expr, u64 isTokIdx, AstNodeSPtr type)
		: AstNode(AstNodeKind::IsExpr, expr ? expr->startTokIdx : isTokIdx, type->endTokIdx)
		, expr(expr)
		, type(type)
	{
	}

	void AstIsExpr::Visit(AstVisitor& visitor)
	{
		if (!visitor.ShouldVisit(nodeKind))
			return;

		visitor.Visit(*this, AstVisitLoc::Begin);

		expr->Visit(visitor);
		type->Visit(visitor);

		visitor.Visit(*this, AstVisitLoc::End);
	}

	void AstIsExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(is-expr)\n");
		if (expr)
			expr->Log(indent + 1);
		type->Log(indent + 1);
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

	void AstCompRunExpr::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(comp-run-expr)\n");
		expr->Log(indent + 1);
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

	void AstBuiltinType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(builtin-type %s)\n", GetTokenTypeName(type).data());
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

	void AstIdentifierType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(identifier-type)\n");
		for (AstNodeSPtr iden : idens)
		{
			iden->Log(indent + 1);
		}
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

	void AstPointerType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(pointer-type)\n");
		subType->Log(indent + 1);
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

	void AstReferenceType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(reference-type)\n");
		subType->Log(indent + 1);
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

	void AstArrayType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(array-type)\n");
		subType->Log(indent + 1);
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

	void AstSliceType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(slice-type)\n");
		subType->Log(indent + 1);
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

	void AstTupleType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(optional-type)\n");
		for (AstNodeSPtr subType : subTypes)
		{
			subType->Log(indent + 1);
		}
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

	void AstOptionalType::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(optional-type)\n");
		subType->Log(indent + 1);
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

	void AstAttributes::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(attributes)\n");
		for (AstNodeSPtr compAttrib : compAttribs)
		{
			compAttrib->Log(indent + 1);
		}
		for (AstNodeSPtr userAttrib : userAttribs)
		{
			userAttrib->Log(indent + 1);
		}
		if (visibility)
			visibility->Log(indent + 1);
		for (AstNodeSPtr simpleAttrib : simpleAttribs)
		{
			simpleAttrib->Log(indent + 1);
		}
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

	void AstCompAttribute::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(compiler-attrib '%s')\n", iden.c_str());
		for (AstNodeSPtr arg : args)
		{
			arg->Log(indent + 1);
		}
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

	void AstUserAttribute::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(user-attrib '%s')\n", iden.c_str());
		for (AstNodeSPtr arg : args)
		{
			arg->Log(indent + 1);
		}
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

	void AstVisibilityAttribute::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(visibility-attrib '%s')\n", kind.c_str());
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

	void AstSimpleAttribute::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(simple-attrib '%s')\n", GetTokenTypeName(attrib).data());
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

	void AstGenericDecl::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(generic-decl)\n");
		for (AstNodeSPtr param : params)
		{
			param->Log(indent + 1);
		}
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

	void AstGenericTypeParam::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(generic-type-param '%s')\n", iden.c_str());
		if (!implTypes.empty())
		{
			LogIndent(indent + 1);
			g_Logger.Log("(generic-type-interfaces)\n");
			for (AstNodeSPtr implType : implTypes)
			{
				implType->Log(indent + 2);
			}
		}
		if (defType)
			defType->Log(indent + 1);
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

	void AstGenericValueParam::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(generic-value-param '%s')\n", iden.c_str());
		type->Log(indent + 1);
		if (defExpr)
			defExpr->Log(indent + 1);
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

	void AstGenericWhereClause::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(generic-where-clause)\n");
		expr->Log(indent + 1);
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

	void AstGenericInst::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(generic-instance '%s')\n", iden.c_str());
		for (AstNodeSPtr arg : args)
		{
			arg->Log(indent + 1);
		}
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

	void AstMacroVar::Log(u32 indent)
	{
		LogIndent(indent);
		StdStringView kindIden;
		switch (kind)
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
		g_Logger.Log("(macro-var '%s' kind='%s')\n", iden.c_str(), kindIden.data());
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

	void AstMacroSeparator::Log(u32 indent)
	{
		LogIndent(indent);
		StdString sepIden;
		for (Token& tok : toks)
		{
			sepIden += tok.Text();
		}
		
		g_Logger.Log("(macro-separator '%s')\n", sepIden.c_str());
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

	void AstMacroFragment::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(macro-fragment sep='%s')\n", GetTokenTypeName(repType).data());
		subPattern->Log(indent);
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

	void AstMacroPattern::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(macro-pattern)\n");
		for (AstNodeSPtr elem : elems)
		{
			elem->Log(indent + 1);
		}
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

	void AstMacroRule::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(macro-rule)\n");
		pattern->Log(indent + 1);
		LogIndent(indent + 1);
		g_Logger.Log("(body)\n");
		for (AstNodeSPtr stmt : body)
		{
			stmt->Log(indent + 2);
		}
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

	void AstDeclMacro::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(decl-macro '%s')\n", iden.c_str());
		pattern->Log(indent + 1);
		LogIndent(indent + 1);
		g_Logger.Log("(body)\n");
		for (AstNodeSPtr stmt : body)
		{
			stmt->Log(indent + 2);
		}
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

	void AstRulesDeclMacro::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(rules-decl-macro '%s')\n", iden.c_str());
		for (AstNodeSPtr rule : rules)
		{
			rule->Log(indent + 1);
		}
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

	void AstProcMacro::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(proc-macro '%s' tok-stream='%s')\n", iden.c_str(), tokStreamIden.c_str());
		pattern->Log(indent + 1);
		LogIndent(indent + 1);
		g_Logger.Log("(body)\n");
		for (AstNodeSPtr stmt : body)
		{
			stmt->Log(indent + 2);
		}
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

	void AstRulesProcMacro::Log(u32 indent)
	{
		LogIndent(indent);
		g_Logger.Log("(rules-proc-macro '%s' tok-stream='%s')\n", iden.c_str(), tokStreamIden.c_str());
		for (AstNodeSPtr rule : rules)
		{
			rule->Log(indent + 1);
		}
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

	void AstMacroInst::Log(u32 indent)
	{
		LogIndent(indent);
		StdString macroIden;
		for (StdString& iden : idens)
		{
			if (!macroIden.empty())
				macroIden += "::";
			macroIden += iden;
		}
		g_Logger.Log("(macro-instance '%s')\n", macroIden.c_str());
		LogIndent(indent + 1);
		g_Logger.Log("(tokens)\n");
		for (Token& tok : toks)
		{
			LogIndent(indent + 2);
			g_Logger.Log("(token '%s')\n", tok.Text().c_str());
		}
	}
}
