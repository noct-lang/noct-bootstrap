#include "parser.hpp"
#include "common/errorsystem.hpp"
#include "common/context.hpp"
#include "module/macro.hpp"
#include "tokens/span.hpp"
#include "tokens/token.hpp"

namespace Noctis
{
	Parser::Parser(const TokenTree& tokTree)
		: m_TokTree(tokTree)
		, m_pMacroSolver(nullptr)
		, m_AllowAggrInit(true)
		, m_IsSwitchStmt(false)
	{
	}

	StdVector<AstStmtSPtr> Parser::Parse()
	{
		StdVector<AstStmtSPtr> nodes;
		while (!m_TokTree.IsExhausted())
		{
			Token& tok = m_TokTree.Peek();
			if (tok.type == TokenType::Module)
				nodes.push_back(ParseModuleDecl());
			else
				nodes.push_back(ParseStatement());
		}
		return nodes;
	}

	AstDeclSPtr Parser::ParseModuleDecl()
	{
		u64 startIdx = m_TokTree.Eat().spanId;
		StdVector<StdString> idens = ParseIdenList(TokenType::Dot);
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstDeclSPtr{ new AstModuleDecl{ startIdx, std::move(idens), endIdx } };
	}

	AstDeclSPtr Parser::ParseUnittestDecl()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SUnittest).spanId;

		StdString name;
		if (m_TokTree.Peek().type == TokenType::StringLit)
			name = m_TokTree.Eat().iden;

		m_TokTree.Eat(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (m_TokTree.Peek().type != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstDeclSPtr{ new AstUnittestDecl{ startIdx, std::move(name), std::move(stmts), endIdx } };
	}

	AstDeclSPtr Parser::ParseBenchmarkDecl()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SBenchmark).spanId;

		StdString name;
		if (m_TokTree.Peek().type == TokenType::StringLit)
			name = m_TokTree.Eat().iden;

		m_TokTree.Eat(TokenType::LParen);
		StdString stateIden = m_TokTree.Eat(TokenType::Iden).iden;
		m_TokTree.Eat(TokenType::RParen);

		m_TokTree.Eat(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (m_TokTree.Peek().type != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstDeclSPtr{ new AstBenchmarkDecl{ startIdx, std::move(name), std::move(stateIden), std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseStatement(AstAttribsSPtr attribs, bool funcAreMethods)
	{
		if (!attribs)
			attribs = ParseAttributes();

		AstLabelStmtSPtr label;
		if (m_TokTree.Peek().type == TokenType::Colon)
		{
			label = ParseLabelStmt();
			switch (m_TokTree.Peek().type)
			{
			case TokenType::Loop: return ParseLoopStmt(label);
			case TokenType::While: return ParseWhileStmt(label);
			case TokenType::Do: return ParseDoWhileStmt(label);
			case TokenType::For: return ParseForStmt(label);
			case TokenType::Switch: return ParseSwitch(label);
			default: return label;
			}
		}
		
		switch (m_TokTree.Peek().type)
		{
		case TokenType::Struct: return ParseStruct(attribs);
		case TokenType::Union: return ParseUnion(attribs);
		case TokenType::Enum: return ParseEnum(attribs);
		case TokenType::Interface: return ParseInterface(attribs);
		case TokenType::Typealias: return ParseTypealias(attribs);
		case TokenType::Typedef: return ParseTypedef(attribs);
		case TokenType::Func: return ParseFuncDecl(attribs, funcAreMethods);
		case TokenType::Impl: return ParseImplDecl(attribs);
		case TokenType::Import: return ParseImport(attribs);
		case TokenType::If: return ParseIfStmt();
		case TokenType::Loop: return ParseLoopStmt(nullptr);
		case TokenType::While: return ParseWhileStmt(nullptr);
		case TokenType::Do: return ParseDoWhileStmt(nullptr);
		case TokenType::For: return ParseForStmt(nullptr);
		case TokenType::Switch: return ParseSwitch(nullptr);
		case TokenType::Break: return ParseBreakStmt();
		case TokenType::Continue: return ParseContinueStmt();
		case TokenType::Fallthrough: return ParseFallthroughStmt();
		case TokenType::Goto: return ParseGotoStmt();
		case TokenType::Return: return ParseReturnStmt();
		case TokenType::Throw: return ParseThrowStmt();
		case TokenType::Defer: return ParseDeferStmt();
		case TokenType::ErrDefer: return ParseErrDeferStmt();
		case TokenType::LBrace: return ParseBlockStmt();
		case TokenType::DollarBrace: return ParseMacroLoopStmt();
		case TokenType::Unsafe:
		{
			if (m_TokTree.Peek(1).type == TokenType::Func)
				return ParseFuncDecl(attribs, funcAreMethods);
			return ParseUnsafeStmt();
		}
		case TokenType::Macro:
		{
			if (m_TokTree.Peek(1).type == TokenType::Func)
				return ParseProcMacro();
			return ParseDeclMacro();
		}
		case TokenType::SConditional: return ParseCompCondStmt();
		case TokenType::SDebug: return ParseCompDebugStmt();
		case TokenType::SIf: return ParseCompIfStmt();
		case TokenType::SUnittest: return ParseUnittestDecl();
		case TokenType::SBenchmark: return ParseBenchmarkDecl();
		case TokenType::SErrorHandler: return ParseErrorHandlerStmt();
		case TokenType::Iden:
		{
			if (m_TokTree.Peek().iden == "weak" && m_TokTree.Peek(1).type == TokenType::Interface)
				return ParseWeakInterface(attribs);

			Token& nextTok = m_TokTree.Peek(1);
			if (nextTok.type == TokenType::Comma || nextTok.type == TokenType::Colon || nextTok.type == TokenType::ColonEq)
				return ParseVarDecl(attribs);
			return ParseExprOrMacroStmt();
		}
		default:
			return ParseExprOrMacroStmt();
		}
	}

	AstDeclSPtr Parser::ParseStruct(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Struct).spanId;
		StdString iden = ParseIden();

		AstGenericDeclSPtr generics = nullptr;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();

		AstStmtSPtr blockStmt = ParseBlockStmt();
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> members = std::move(block.stmts);
		return AstDeclSPtr{ new AstStructDecl{ attribs, startIdx, std::move(iden), generics, std::move(members), block.ctx->endIdx } };
	}

	AstDeclSPtr Parser::ParseUnion(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Union).spanId;
		StdString iden = ParseIden();

		AstGenericDeclSPtr generics = nullptr;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();

		AstStmtSPtr blockStmt = ParseBlockStmt();
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> members = std::move(block.stmts);
		return AstDeclSPtr{ new AstUnionDecl{ attribs, startIdx, std::move(iden), generics, std::move(members), block.ctx->endIdx } };
	}

	AstDeclSPtr Parser::ParseEnum(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Enum).spanId;
		StdString iden = ParseIden();
		
		AstGenericDeclSPtr generics;
		AstTypeSPtr baseType;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();

		if (m_TokTree.TryEat(TokenType::Colon))
			baseType = ParseType();

		bool isAdt = !!generics;
		bool isValue = !!baseType;

		StdPairVector<StdString, AstExprSPtr> valueMembers;
		StdPairVector<StdString, AstTypeSPtr> adtMembers;
		m_TokTree.Eat(TokenType::LBrace);
		if (m_TokTree.Peek().type != TokenType::RBrace)
		{
			do
			{
				StdString memberIden = m_TokTree.Eat(TokenType::Iden).iden;

				if (m_TokTree.TryEat(TokenType::Eq))
				{
					AstExprSPtr expr = ParseExpression();
					isValue = true;
					if (isAdt)
					{
						Span span = g_SpanManager.GetSpan(expr->ctx->startIdx - 1);
						g_ErrorSystem.Error(span, "Cannot assign a value to a member of an ADT enum");
						expr = nullptr;
						isValue = false;
					}
					valueMembers.push_back(std::pair{ memberIden, expr });
				}
				else if (m_TokTree.Peek().type == TokenType::LParen ||
					m_TokTree.Peek().type == TokenType::LBrace)
				{
					AstTypeSPtr type = ParseType(true);
					isAdt = true;
					if (isValue)
					{
						Span span = g_SpanManager.GetSpan(type->ctx->startIdx - 1);
						g_ErrorSystem.Error(span, "Cannot add a type to a member of a value enum");
						isAdt = false;
					}
					adtMembers.push_back(std::pair{ memberIden, type });
				}
				else
				{
					valueMembers.push_back(std::pair{ memberIden, nullptr });
					adtMembers.push_back(std::pair{ memberIden, nullptr });
				}
			}
			while (m_TokTree.TryEat(TokenType::Comma) && m_TokTree.Peek().type != TokenType::RBrace);
		}
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;

		if (isAdt)
			return AstDeclSPtr{ new AstAdtEnumDecl{ attribs, startIdx, std::move(iden), generics,
				std::move(adtMembers), endIdx } };
		return AstDeclSPtr{ new AstValueEnumDecl{ attribs, startIdx, std::move(iden), baseType,
			std::move(valueMembers), endIdx } };
	}

	AstDeclSPtr Parser::ParseInterface(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Interface).spanId;
		StdString iden = ParseIden();

		if (m_TokTree.Peek().type == TokenType::Semicolon)
		{
			u64 endIdx = m_TokTree.Eat().spanId;
			return AstDeclSPtr{ new AstMarkerInterfaceDecl{ attribs, startIdx, std::move(iden), endIdx } };
		}

		AstGenericDeclSPtr generics;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();

		StdVector<AstIdentifierTypeSPtr> implInterfaces;
		if (m_TokTree.TryEat(TokenType::Colon))
		{
			do
			{
				AstIdentifierTypeSPtr tmp = ParseIdentifierType();
				implInterfaces.push_back(tmp);
			}
			while (m_TokTree.TryEat(TokenType::Plus));
		}

		AstStmtSPtr blockStmt = ParseBlockStmt(true);
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> members = std::move(block.stmts);
		return AstDeclSPtr{ new AstStrongInterfaceDecl{ attribs, startIdx, std::move(iden), generics, std::move(implInterfaces), std::move(members), block.ctx->endIdx } };
	}

	AstDeclSPtr Parser::ParseWeakInterface(AstAttribsSPtr attribs)
	{
		u64 weakIdx = m_TokTree.EatIden("weak").spanId;
		m_TokTree.Eat(TokenType::Interface);
		StdString iden = ParseIden();

		AstStmtSPtr blockStmt = ParseBlockStmt(true);
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> members = std::move(block.stmts);
		return AstDeclSPtr{ new AstWeakInterfaceDecl{ attribs, weakIdx, std::move(iden), std::move(members), block.ctx->endIdx } };
	}

	AstDeclSPtr Parser::ParseTypealias(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Typealias).spanId;
		StdString iden = ParseIden();

		AstGenericDeclSPtr generics;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();
		
		AstTypeSPtr type;
		if (m_TokTree.TryEat(TokenType::Eq))
			type = ParseType();
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;

		return AstDeclSPtr{ new AstTypeAliasDecl{ attribs, startIdx, std::move(iden), generics, type, endIdx } };
	}

	AstDeclSPtr Parser::ParseTypedef(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Typedef).spanId;
		StdString iden = ParseIden();

		AstGenericDeclSPtr generics;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();

		m_TokTree.Eat(TokenType::Eq);
		AstTypeSPtr type = ParseType();
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;

		return AstDeclSPtr{ new AstTypeDefDecl{ attribs, startIdx, std::move(iden), generics, type, endIdx } };
	}

	AstVarDeclSPtr Parser::ParseVarDecl(AstAttribsSPtr attribs)
	{
		Token& tok = m_TokTree.Peek();
		u64 startIdx = tok.spanId;
		StdVector<StdString> idens = ParseIdenList(TokenType::Comma);
		return ParseVarDecl(attribs, startIdx, std::move(idens));
	}

	AstVarDeclSPtr Parser::ParseVarDecl(AstAttribsSPtr attribs, u64 startIdx, StdVector<StdString>&& idens)
	{
		AstTypeSPtr type;
		AstExprSPtr expr;
		if (m_TokTree.TryEat(TokenType::Colon))
		{
			type = ParseType();
			if (m_TokTree.TryEat(TokenType::Eq))
				expr = ParseCommaExpression();
		}
		else
		{
			m_TokTree.Eat(TokenType::ColonEq);
			expr = ParseCommaExpression();
		}
		
		u64 endIdx = 0;
		if (m_TokTree.Peek().type == TokenType::Comma)
			endIdx = m_TokTree.Eat(TokenType::Comma).spanId;
		else if (m_TokTree.Peek().type == TokenType::Semicolon)
			endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstVarDeclSPtr{ new AstVarDecl{ attribs, startIdx, std::move(idens), type, expr, endIdx } };
	}

	AstDeclSPtr Parser::ParseFuncDecl(AstAttribsSPtr attribs, bool asMethod)
	{
		u64 startIdx;
		bool isUnsafe = m_TokTree.Peek().type == TokenType::Unsafe;
		if (isUnsafe)
		{
			Token& tok = m_TokTree.Eat();
			startIdx = tok.spanId;
			attribs->simpleAttribs.emplace_back(new AstSimpleAttrib{ tok });
			m_TokTree.Eat(TokenType::Func);
		}
		else
		{
			startIdx = m_TokTree.Eat(TokenType::Func).spanId;
		}
		
		AstMethodReceiverKind recKind = AstMethodReceiverKind::None;
		if (asMethod && m_TokTree.TryEat(TokenType::LParen))
		{
			if (m_TokTree.TryEat(TokenType::Move))
			{
				recKind = AstMethodReceiverKind::Move;
			}
			else if (m_TokTree.TryEat(TokenType::And))
			{
				if (m_TokTree.TryEat(TokenType::Mut))
					recKind = AstMethodReceiverKind::MutRef;
				else
					recKind = AstMethodReceiverKind::Ref;
			}
			else
			{
				recKind = AstMethodReceiverKind::Value;
			}
			m_TokTree.EatIden("self");
			m_TokTree.Eat(TokenType::RParen);
		}
		
		StdString iden = ParseIden();

		AstGenericDeclSPtr generics;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();

		m_TokTree.Eat(TokenType::LParen);
		StdVector<AstParamSPtr> params;
		if (m_TokTree.Peek().type != TokenType::RParen)
			params = ParseParams(false);
		m_TokTree.Eat(TokenType::RParen);

		AstTypeSPtr errorType;
		if (m_TokTree.TryEatIden("throws"))
		{
			m_TokTree.Eat(TokenType::LParen);
			errorType = ParseType();
			m_TokTree.Eat(TokenType::RParen);
		}
		
		AstTypeSPtr retType;
		StdPairVector<StdVector<StdString>, AstTypeSPtr> namedRets;
		if (m_TokTree.TryEat(TokenType::Arrow))
		{
			if (m_TokTree.TryEat(TokenType::LBrace))
			{
				do
				{
					StdVector<StdString> retNames = ParseIdenList(TokenType::Comma);
					m_TokTree.Eat(TokenType::Colon);
					AstTypeSPtr type = ParseType();
					namedRets.push_back(std::pair{ std::move(retNames), type });
				}
				while (m_TokTree.TryEat(TokenType::Comma));
				m_TokTree.Eat(TokenType::RBrace);
			}
			else
			{	
				retType = ParseType();
			}
		}

		AstGenericWhereClauseSPtr whereClause;
		if (m_TokTree.Peek().iden == "where")
			whereClause = ParseGenericWhereClause();

		if (m_TokTree.Peek().type == TokenType::Semicolon)
		{
			if (whereClause)
			{
				Span span = g_SpanManager.GetSpan(whereClause->ctx->startIdx);
				g_ErrorSystem.Error(span, "An empty method declaration cannot have a where clause");
			}

			u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
			if (asMethod)
				return AstDeclSPtr{ new AstMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params), errorType, retType, {}, nullptr, {}, isUnsafe, true, endIdx } };
			else
				return AstDeclSPtr{ new AstFuncDecl{ attribs, startIdx, std::move(iden), generics, std::move(params), errorType,
					retType, std::move(namedRets), whereClause, {}, isUnsafe, endIdx } };
		}

		AstStmtSPtr blockStmt = ParseBlockStmt();
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> stmts = std::move(block.stmts);

		if (asMethod)
			return AstDeclSPtr{ new AstMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params),
				errorType, retType, std::move(namedRets), whereClause, std::move(stmts), isUnsafe, false, block.ctx->endIdx } };
		
		return AstDeclSPtr{ new AstFuncDecl { attribs, startIdx, std::move(iden), generics, std::move(params),
			errorType, retType, std::move(namedRets), whereClause, std::move(stmts), isUnsafe, block.ctx->endIdx } };
	}

	AstDeclSPtr Parser::ParseImplDecl(AstAttribsSPtr attribs)
	{
		u64 implIdx = m_TokTree.Eat(TokenType::Impl).spanId;
		AstGenericDeclSPtr generics;
		if (m_TokTree.Peek().type == TokenType::Less)
			generics = ParseGenericDecl();
		
		AstTypeSPtr type = ParseType();

		AstIdentifierTypeSPtr interface;
		if (m_TokTree.TryEat(TokenType::Colon))
			interface = ParseIdentifierType();

		AstGenericWhereClauseSPtr whereClause;
		if (m_TokTree.Peek().iden == "where")
			whereClause = ParseGenericWhereClause();
		
		AstStmtSPtr blockStmt = ParseBlockStmt(true);
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> stmts = std::move(block.stmts);
		return AstDeclSPtr{ new AstImplDecl{ attribs, implIdx, generics, type, interface, whereClause, std::move(stmts), block.ctx->endIdx } };
	}

	AstStmtSPtr Parser::ParseImport(AstAttribsSPtr attribs)
	{
		u64 importIdx = m_TokTree.Eat(TokenType::Import).spanId;
		StdVector<StdString> modIdens = ParseIdenList(TokenType::Dot);

		StdPairVector<StdVector<StdString>, StdString> symbols;
		if (m_TokTree.TryEat(TokenType::Colon))
		{
			do	
			{
				StdVector<StdString> symIdens = ParseIdenList(TokenType::ColonColon);
				StdString symAlias;
				if (m_TokTree.TryEat(TokenType::As))
					symAlias = m_TokTree.Eat(TokenType::Iden).iden;
				symbols.push_back(std::pair{ std::move(symIdens), std::move(symAlias) });
			}
			while (m_TokTree.TryEat(TokenType::Comma));
		}

		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstStmtSPtr{ new AstImportStmt{ attribs, importIdx, std::move(modIdens), std::move(symbols), endIdx } };
	}

	AstStmtSPtr Parser::ParseBlockStmt(bool funcsAreMethods)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LBrace).spanId;
		StdVector<AstStmtSPtr> stmts;
		while (m_TokTree.Peek().type != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, funcsAreMethods));
		}
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstStmtSPtr{ new AstBlockStmt{ startIdx, std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseIfStmt()
	{
		u64 ifIdx = m_TokTree.Eat(TokenType::If).spanId;

		AstVarDeclSPtr varDecl;
		Token& nextTok = m_TokTree.Peek(1);
		if (nextTok.type == TokenType::Comma || nextTok.type == TokenType::Colon || nextTok.type == TokenType::ColonEq)
			varDecl = ParseVarDecl(nullptr);

		AstExprSPtr cond;
		{
			SaveRestore allowAggrInit{ m_AllowAggrInit, false };
			cond = ParseExpression(nullptr, true);
		}

		AstStmtSPtr body;
		if (m_TokTree.TryEat(TokenType::Do))
			body = ParseStatement(nullptr, false);
		else
			body = ParseBlockStmt();
		
		AstStmtSPtr elseBody;
		if (m_TokTree.TryEat(TokenType::Else))
		{
			if (m_TokTree.TryEat(TokenType::Do))
				body = ParseStatement(nullptr, false);
			else if (m_TokTree.Peek().type == TokenType::If)
				elseBody = ParseIfStmt();	
			else
				elseBody = ParseBlockStmt();
		}
		return AstStmtSPtr{ new AstIfStmt{ ifIdx, varDecl, cond, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseLoopStmt(AstLabelStmtSPtr label)
	{
		u64 loopIdx = m_TokTree.Eat(TokenType::Loop).spanId;
		AstStmtSPtr body = ParseStatement();
		return AstStmtSPtr{ new AstLoopStmt{ label, loopIdx, body } };
	}

	AstStmtSPtr Parser::ParseWhileStmt(AstLabelStmtSPtr label)
	{
		u64 whileIdx = m_TokTree.Eat(TokenType::While).spanId;
		AstExprSPtr cond;
		{
			SaveRestore allowAggrInit{ m_AllowAggrInit, false };
			cond = ParseExpression(nullptr, true);
		}
		AstStmtSPtr body = ParseBlockStmt();
		return AstStmtSPtr{ new AstWhileStmt{ label, whileIdx, cond, body } };
	}

	AstStmtSPtr Parser::ParseDoWhileStmt(AstLabelStmtSPtr label)
	{
		u64 doIdx = m_TokTree.Eat(TokenType::Do).spanId;
		AstStmtSPtr body = ParseBlockStmt();
		m_TokTree.Eat(TokenType::While);
		AstExprSPtr cond;
		{
			SaveRestore allowAggrInit{ m_AllowAggrInit, false };
			cond = ParseExpression(nullptr, true);
		}
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstStmtSPtr{ new AstDoWhileStmt{ label, doIdx, body, cond, endIdx } };
	}

	AstStmtSPtr Parser::ParseForStmt(AstLabelStmtSPtr label)
	{
		u64 forTokIdx = m_TokTree.Eat(TokenType::For).spanId;

		StdVector<StdString> idens = ParseIdenList(TokenType::Comma);
		m_TokTree.Eat(TokenType::In);
		AstExprSPtr range;
		{
			SaveRestore allowAggrInit{ m_AllowAggrInit, false };
			range = ParseExpression(nullptr, true);
		}
		AstStmtSPtr body = ParseBlockStmt();
		return AstStmtSPtr{ new AstForStmt{ label, forTokIdx, std::move(idens), range, body } };
	}

	AstStmtSPtr Parser::ParseSwitch(AstLabelStmtSPtr label)
	{
		u64 switchIdx = m_TokTree.Eat(TokenType::Switch).spanId;

		AstExprSPtr cond;
		{
			SaveRestore allowAggrInit{ m_AllowAggrInit, false };
			cond = ParseExpression(nullptr, true);
		}

		m_TokTree.Eat(TokenType::LBrace);
		StdVector<AstSwitchCase> cases;
		do
		{
			if (m_TokTree.Peek().type == TokenType::RBrace)
				break;
			
			AstPatternSPtr pattern = ParsePattern();
			AstExprSPtr expr;
			if (m_TokTree.TryEatIden("where"))
				expr = ParseExpression();
			m_TokTree.Eat(TokenType::DblArrow);
			m_IsSwitchStmt = true;
			AstStmtSPtr body = ParseStatement();
			m_IsSwitchStmt = false;
			
			if (body->stmtKind != AstStmtKind::Block)
				body.reset(new AstBlockStmt{ body->ctx->startIdx, { body }, body->ctx->endIdx });

			cases.push_back(AstSwitchCase{ pattern, expr, body });
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;

		return AstStmtSPtr{ new AstSwitchStmt{ label, switchIdx, cond, std::move(cases) , endIdx } };
	}

	AstLabelStmtSPtr Parser::ParseLabelStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Colon).spanId;
		StdString label = m_TokTree.Eat(TokenType::Iden).iden;
		u64 endIdx = m_TokTree.Eat(TokenType::Colon).spanId;
		return AstLabelStmtSPtr{ new AstLabelStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseBreakStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Break).spanId;
		StdString label;
		if (m_TokTree.Peek().type == TokenType::Iden)
			label = m_TokTree.Eat().iden;
		
		u64 endIdx = EatStmtEndIdx();
		return AstStmtSPtr{ new AstBreakStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseContinueStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Continue).spanId;
		StdString label;
		if (m_TokTree.Peek().type == TokenType::Iden)
			label = m_TokTree.Eat().iden;
		
		u64 endIdx = EatStmtEndIdx();
		return AstStmtSPtr{ new AstContinueStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseFallthroughStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Fallthrough).spanId;
		u64 endIdx = EatStmtEndIdx();
		return AstStmtSPtr{ new AstFallthroughStmt{ startIdx, endIdx } };
	}

	AstStmtSPtr Parser::ParseGotoStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Goto).spanId;
		StdString label = m_TokTree.Eat(TokenType::Iden).iden;
		u64 endIdx = EatStmtEndIdx();
		return AstStmtSPtr{ new AstGotoStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseReturnStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Return).spanId;
		AstExprSPtr expr;
		if (m_TokTree.Peek().type != TokenType::Semicolon)
			expr = m_IsSwitchStmt ? ParseExpression() : ParseCommaExpression();
		u64 endIdx = EatStmtEndIdx();
		return AstStmtSPtr{ new AstReturnStmt{ startIdx, expr, endIdx } };
	}


	AstStmtSPtr Parser::ParseThrowStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Throw).spanId;
		AstExprSPtr expr = ParseOperand(nullptr);
		u64 endIdx = EatStmtEndIdx();
		return AstStmtSPtr{ new AstThrowStmt{ startIdx, expr, endIdx } };
	}


	AstStmtSPtr Parser::ParseExprOrMacroStmt()
	{
		AstExprSPtr expr = ParseExpression();

		if (expr->exprKind == AstExprKind::MacroInst &&
			m_TokTree.Peek().type != TokenType::Semicolon)
		{
			AstMacroInstExpr* pMacroInst = static_cast<AstMacroInstExpr*>(expr.get());
			return AstStmtSPtr{ new AstMacroInstStmt{ pMacroInst->qualName, std::move(pMacroInst->toks), pMacroInst->ctx->endIdx } };
		}
		
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstStmtSPtr{ new AstExprStmt{ expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseDeferStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Defer).spanId;
		AstExprSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstStmtSPtr{ new AstDeferStmt{ startIdx, expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseErrDeferStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::ErrDefer).spanId;
		AstExprSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = m_TokTree.Eat(TokenType::Semicolon).spanId;
		return AstStmtSPtr{ new AstErrDeferStmt{ startIdx, expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseUnsafeStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Unsafe).spanId;
		AstStmtSPtr blockStmt = ParseBlockStmt();
		AstBlockStmt& block = static_cast<AstBlockStmt&>(*blockStmt);
		StdVector<AstStmtSPtr> stmts = std::move(block.stmts);
		return AstStmtSPtr{ new AstUnsafeStmt{ startIdx, std::move(block.stmts), block.ctx->endIdx } };
	}

	AstStmtSPtr Parser::ParseErrorHandlerStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SErrorHandler).spanId;
		m_TokTree.Eat(TokenType::LParen);
		StdString iden = m_TokTree.Eat(TokenType::Iden).iden;
		AstTypeSPtr type;
		if (m_TokTree.TryEat(TokenType::Colon))
			type = ParseType();
		m_TokTree.Eat(TokenType::RParen);
		AstStmtSPtr block = ParseBlockStmt();
		return AstStmtSPtr{ new AstErrHandler{ startIdx, std::move(iden), type, block } };
	}

	AstStmtSPtr Parser::ParseCompIfStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SIf).spanId;

		m_TokTree.Eat(TokenType::LParen);
		Token& tok = m_TokTree.Peek();
		AstVarDeclSPtr varDecl;
		if (tok.type == TokenType::Comma || tok.type == TokenType::Colon || tok.type == TokenType::ColonEq)
		{
			varDecl = ParseVarDecl(nullptr);
		}

		AstExprSPtr cond = ParseExpression(nullptr, true);
		m_TokTree.Eat(TokenType::RParen);

		AstStmtSPtr body = ParseBlockStmt();
		AstStmtSPtr elseBody;
		if (m_TokTree.TryEat(TokenType::Else))
			elseBody = ParseStatement();

		return AstStmtSPtr{ new AstCompIfStmt{ startIdx, varDecl, cond, body, elseBody } };
	}

	// TODO: version triplets
	AstStmtSPtr Parser::ParseCompCondStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SConditional).spanId;
		Token& cond = m_TokTree.Eat(TokenType::Iden);

		Token cmp{ TokenType::Unknown, u64(-1) },
			  val{ TokenType::Unknown, u64(-1) };
		if (m_TokTree.Peek().type != TokenType::LBrace)
		{
			cmp = m_TokTree.Eat();
			val = m_TokTree.Eat();
		}

		AstStmtSPtr body = ParseBlockStmt();
		AstStmtSPtr elseBody;
		if (m_TokTree.TryEat(TokenType::Else))
			elseBody = ParseStatement();
		return AstStmtSPtr{ new AstCompCondStmt{ startIdx, cond, cmp, val, body, elseBody } };
	}

	// TODO: version triplets
	AstStmtSPtr Parser::ParseCompDebugStmt()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SDebug).spanId;
		Token& cond = m_TokTree.Eat(TokenType::Iden);

		Token cmp{ TokenType::Unknown, u64(-1) },
			  val{ TokenType::Unknown, u64(-1) };
		if (m_TokTree.Peek().type != TokenType::LBrace)
		{
			cmp = m_TokTree.Eat();
			val = m_TokTree.Eat();
		}

		AstStmtSPtr body = ParseBlockStmt();
		AstStmtSPtr elseBody;
		if (m_TokTree.TryEat(TokenType::Else))
			elseBody = ParseStatement();
		return AstStmtSPtr{ new AstCompDebugStmt{ startIdx, cond, cmp, val, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseMacroLoopStmt()
	{
		TokenTree tokTree = m_TokTree.GetSubTree();
		u64 startIdx = tokTree.subToks.front().tok.spanId;
		u64 endIdx = tokTree.subToks.back().tok.spanId;

		tokTree.subToks.erase(tokTree.subToks.begin());
		tokTree.subToks.erase(tokTree.subToks.end() - 1);
		
		Parser subParser = Parser{ tokTree };
		subParser.SetMacroVarSolver(m_pMacroSolver);

		bool hasLoopedToks = m_pMacroSolver->HasLoopedToks();
		StdVector<AstStmtSPtr> stmts;
		m_pMacroSolver->EnterMacroLoop();
		do
		{
			if (hasLoopedToks)
				subParser.m_TokTree.ResetIdx();
			
			while (!subParser.m_TokTree.IsExhausted())
			{
				stmts.push_back(subParser.ParseStatement());
			}
		}
		while (m_pMacroSolver->NextLoopIt());
		m_pMacroSolver->EnterMacroLoop();

		return AstStmtSPtr{ new AstMacroLoopStmt{ startIdx, std::move(stmts), endIdx } };
	}

	AstExprSPtr Parser::ParseCommaExpression()
	{
		StdVector<AstExprSPtr> exprs;
		do
		{
			exprs.push_back(ParseExpression());
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		if (exprs.size() == 1)
			return exprs[0];
		return AstExprSPtr{ new AstCommaExpr{ std::move(exprs) } };
	}

	AstExprSPtr Parser::ParseExpression(AstExprSPtr prev, bool allowBlockExpr)
	{
		AstExprSPtr expr = prev;
		while (true)
		{
			Token tok = m_TokTree.Peek();
			if (m_TokTree.Peek().type == TokenType::MacroIden)
			{
				MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);
				switch (elem.varKind)
				{
				case MacroVarKind::Expr:
				{
					m_TokTree.Eat();
					elem.Parse();
					return elem.GetExpr();
				}
				case MacroVarKind::Toks:
				{
					m_TokTree.InsertAtCur(elem.tokTree);
					tok = m_TokTree.Peek();
					break;
				}
				case MacroVarKind::Stmt:
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pName = tok.iden.c_str();
					g_ErrorSystem.Error(span, "macro variable '$%s' cannot be 'stmt' when an expression is expected", pName);
					return nullptr;
				}
				default: break;
				}
			}
			
			switch (tok.type)
			{
			case TokenType::Plus:
			case TokenType::Minus:
			case TokenType::And:
			case TokenType::Asterisk:
			case TokenType::Tilde:
			{
				if (expr)
					expr = ParseBinaryExpr(expr);
				else
					expr = ParsePrefixExpr();
				break;
			}
			case TokenType::PlusPlus:
			case TokenType::MinusMinus:
			{
				if (expr)
					expr = ParsePostfixExpr(expr);
				else
					expr = ParsePrefixExpr();
				break;
			}
			case TokenType::Exclaim:
			{
				if (expr)
					expr = ParsePostfixExpr(expr);
				else
					expr = ParsePrefixExpr();
				break;
			}
			case TokenType::Percent:
			case TokenType::Slash:
			case TokenType::AndAnd:
			case TokenType::OrOr:
			case TokenType::LessEq:
			case TokenType::LessLess:
			case TokenType::LessLessLess:
			case TokenType::LessLessAsterisk:
			case TokenType::Greater:
			case TokenType::GreaterEq:
			case TokenType::GreaterGreater:
			case TokenType::GreaterGreaterGreater:
			case TokenType::GreaterGreaterAsterisk:
			case TokenType::EqEq:
			case TokenType::ExclaimEq:
			case TokenType::QuestionQuestion:
			case TokenType::QuestionColon:
			case TokenType::In:
			case TokenType::NotIn:
			case TokenType::Caret:
			{
				if (!expr)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				expr = ParseBinaryExpr(expr);
				break;
			}
			case TokenType::DotDot:
			case TokenType::DotDotEq:
			{
				expr = ParseRangeExpr(expr);
				break;
			}
			case TokenType::Less:
			{
				if (!expr)
					expr = ParseQualNameExpr();
				else
					expr = ParseBinaryExpr(expr);
				break;
			}
			case TokenType::Or:
			{
				if (expr)
					expr = ParseBinaryExpr(expr);
				else
					expr = ParseClosureExpr();
				break;
			}
			case TokenType::Eq:
			case TokenType::PlusEq:
			case TokenType::MinusEq:
			case TokenType::AsteriskEq:
			case TokenType::SlashEq:
			case TokenType::TildeEq:
			case TokenType::LessLessEq:
			case TokenType::LessLessLessEq:
			case TokenType::LessLessAsteriskEq:
			case TokenType::GreaterGreaterEq:
			case TokenType::GreaterGreaterGreaterEq:
			case TokenType::GreaterGreaterAsteriskEq:
			case TokenType::AndEq:
			case TokenType::CaretEq:
			case TokenType::OrEq:
			case TokenType::QuestionQuestionEq:
			case TokenType::PercentEq:
			{
				if (!expr)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				expr = ParseAssignmentExpr(expr);
				break;
			}
			case TokenType::Question:
			{
				if (!expr)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				expr = ParseTernaryExpr(expr);
				break;
			}
			case TokenType::MacroIden:
			{
				m_TokTree.Eat();
				MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);

				switch (elem.varKind)
				{
				case MacroVarKind::Expr:
				{
					expr = *reinterpret_cast<AstExprSPtr*>(&elem.node);
					break;
				}
				case MacroVarKind::Qual:
				{
					AstQualNameSPtr qualName = *reinterpret_cast<AstQualNameSPtr*>(&elem.node);
					expr = AstExprSPtr{ new AstQualNameExpr{ qualName } };
					break;
				}
				case MacroVarKind::Iden:
				{
					AstQualIdenSPtr qualIden = *reinterpret_cast<AstQualIdenSPtr*>(&elem.node);
					AstQualNameSPtr qualName{ new AstQualName{ qualIden->ctx->startIdx, false, StdVector<AstQualIdenSPtr>{ qualIden } } };
					expr = AstExprSPtr{ new AstQualNameExpr{ qualName } };
					break;
				}
				default: break;
				}
				break;
			}
			case TokenType::Try:
			{
				expr = ParseTryExpr();
				break;
			}
			case TokenType::LBrace:
			{
				if (!allowBlockExpr && expr)
					return expr;
				[[fallthrough]];
			}	
			default:
				AstExprSPtr tmp = ParseOperand(expr);
				if (!tmp)
					return expr;
				expr = tmp;
				break;
			}
		}
	}

	AstExprSPtr Parser::ParseOperand(AstExprSPtr prev)
	{
		AstExprSPtr expr = ParseOperandImpl(prev);
		if (expr == prev)
			return nullptr;
		
		while (true)
		{
			Token& tok = m_TokTree.Peek();
			bool run = false;
			switch (tok.type)
			{
			case TokenType::Dot:
			case TokenType::QuestionDot:
			case TokenType::LBracket:
			case TokenType::QuestionBracket:
			case TokenType::LParen:
			case TokenType::LBrace:
			{
				run = true;
				break;
			}
			default: break;
			}

			if (!run)
				return expr;
			
			AstExprSPtr tmp = ParseOperandImpl(expr);
			if (!tmp || tmp == expr)
				return expr;
			
			expr = tmp;
		}
	}

	AstExprSPtr Parser::ParseOperandImpl(AstExprSPtr prev)
	{
		while (true)
		{
			Token& tok = m_TokTree.Peek();
			switch (tok.type)
			{
			case TokenType::Less:
			{
				if (prev)
					return nullptr;
				return ParseQualNameExpr();
			}
			case TokenType::Iden:
			{
				if (prev)
					return nullptr;
				if (tok.iden == "_")
					return ParseLiteralExpr();
				return ParseQualNameExpr();
			}
			case TokenType::Or:
				return ParseClosureExpr();
			case TokenType::LBracket:
			case TokenType::QuestionBracket:
			{
				if (!prev)
				{
					if (tok.type == TokenType::LBracket)
					{
						return  ParseArrayInitExpr();
					}
					
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Expected expression before '%s'", pTokName);
					m_TokTree.Eat();
					return nullptr;
				}
				return  ParseIndexSlicExpr(prev);
			}
			case TokenType::LParen:
			{
				if (prev)
					return ParseFuncCallExpr(prev);
				return ParseBracketExpr();
			}
			case TokenType::Dot:
			case TokenType::QuestionDot:
			{
				if (!prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Expected expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}

				if (m_TokTree.Peek(1).type == TokenType::I32Lit)
					return ParseTupleAccessExpr(prev);
				if (m_TokTree.Peek(2).type == TokenType::LParen)
					return ParseMethodcallExpr(prev);
				return ParseMemberAccessExpr(prev);
			}
			case TokenType::True:
			case TokenType::False:
			case TokenType::Null:
			case TokenType::CharLit:
			case TokenType::I8Lit:
			case TokenType::I16Lit:
			case TokenType::I32Lit:
			case TokenType::I64Lit:
			case TokenType::I128Lit:
			case TokenType::U8Lit:
			case TokenType::U16Lit:
			case TokenType::U32Lit:
			case TokenType::U64Lit:
			case TokenType::U128Lit:
			case TokenType::F16Lit:
			case TokenType::F32Lit:
			case TokenType::F64Lit:
			case TokenType::F128Lit:
			case TokenType::StringLit:
			{
				if (prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Unexpected expression before '%s'", pTokName);
					m_TokTree.Eat();
					return nullptr;
				}
				return ParseLiteralExpr();
			}
			case TokenType::LBrace:
			{
				if (prev)
				{
					if (!m_AllowAggrInit)
						return prev;
					
					if (prev->exprKind == AstExprKind::QualName)
					{
						AstQualNameExpr* pQualNameExpr = static_cast<AstQualNameExpr*>(prev.get());
						return ParseAggrInitExpr(pQualNameExpr);
					}
					else
					{
						Span span = g_SpanManager.GetSpan(tok.spanId);
						const char* pTokName = GetTokenTypeName(tok.type).data();
						g_ErrorSystem.Error(span, "Unexpected expression before '%s'", pTokName);
						m_TokTree.Eat();
						return nullptr;
					}
				}
				return ParseBlockExpr();
			}
			case TokenType::As:
			case TokenType::AsQuestion:
			case TokenType::AsExclaim:
			{
				if (!prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Unexpected expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				return ParseCastExpr(prev);
			}
			case TokenType::Transmute:
			{
				if (!prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Unexpected expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				return ParseTransmuteExpr(prev);
			}
			case TokenType::Move:
			{
				if (prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				return ParseMoveExpr();
			}
			case TokenType::Unsafe:
			{
				if (prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Unexpected expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				return ParseUnsafeExpr();
			}
			case TokenType::Is:
			case TokenType::NotIs:
				return ParseIsExpr(prev);
			case TokenType::SRun:
			{
				if (prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Unexpected expression before '%s'", pTokName);
					m_TokTree.Eat();
					m_TokTree.Eat();
					break;
				}

				return ParseCompRunExpr();
			}
			case TokenType::ColonColon:
				return ParseQualNameExpr();
			case TokenType::DollarParen:
			
			{
				if (!prev)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pTokName = GetTokenTypeName(tok.type).data();
					g_ErrorSystem.Error(span, "Expected expression before '%s'", pTokName);
					m_TokTree.Eat();
					break;
				}
				
				AstQualNameSPtr qualName = static_cast<AstQualNameExpr*>(prev.get())->qualName;
				return ParseMacroInstExpr(qualName);
			}
			case TokenType::DollarBracket:
			case TokenType::DollarBrace:
			{
				AstQualNameSPtr qualName = static_cast<AstQualNameExpr*>(prev.get())->qualName;
				return ParseMacroInstExpr(qualName);
			}
			case TokenType::MacroIden:
				return ParseMacroVarExpr();
			default:
				return nullptr;
			}
		}
	}

	AstExprSPtr Parser::ParseAssignmentExpr(AstExprSPtr lExpr)
	{
		TokenType op = m_TokTree.Eat().type;
		AstExprSPtr rExpr = ParseExpression();
		return AstExprSPtr{ new AstAssignExpr{ lExpr, op, rExpr } };
	}

	AstExprSPtr Parser::ParseTernaryExpr(AstExprSPtr cond)
	{
		m_TokTree.Eat(TokenType::Question);
		AstExprSPtr trueExpr = ParseExpression();
		m_TokTree.Eat(TokenType::Colon);
		AstExprSPtr falseExpr = ParseExpression();
		return AstExprSPtr{ new AstTernaryExpr{ cond, trueExpr, falseExpr } };
	}

	AstExprSPtr Parser::ParseBinaryExpr(AstExprSPtr lExpr)
	{
		TokenType op = m_TokTree.Eat().type;
		AstExprSPtr expr = ParseOperand(nullptr);

		if (lExpr->exprKind == AstExprKind::Binary)
		{
			AstBinaryExpr* binExpr = static_cast<AstBinaryExpr*>(lExpr.get());
			OpPrecedence prevPrec = GetPrecedence(op);
			OpPrecedence curPrec = GetPrecedence(binExpr->op);

			// When the previous binary expression has a higher precedence (i.e. will be executed later), switch both side
			// e.g. (a + b) * c -> a + (b * c)
			if (curPrec > prevPrec)
			{
				AstExprSPtr left = binExpr->lExpr;
				AstExprSPtr cur{ new AstBinaryExpr{ lExpr, op, left } };
				binExpr->lExpr = cur;
				return expr;
			}
		}

		return AstExprSPtr{ new AstBinaryExpr{ lExpr, op, expr } };
	}

	AstExprSPtr Parser::ParseRangeExpr(AstExprSPtr lExpr)
	{
		Token& op = m_TokTree.Peek();
		if (op.type == TokenType::DotDotEq)
			m_TokTree.Eat();
		else
			m_TokTree.Eat(TokenType::DotDot);

		u64 startIdx = lExpr ? lExpr->ctx->startIdx : op.spanId;
		AstExprSPtr rExpr = ParseOperand(nullptr);
		u64 endIdx = rExpr ? rExpr->ctx->endIdx : op.spanId;

		return AstExprSPtr{ new AstRangeExpr{ startIdx, lExpr, op.type == TokenType::DotDotEq, rExpr, endIdx } };
	}

	AstExprSPtr Parser::ParsePostfixExpr(AstExprSPtr expr)
	{
		Token& tok = m_TokTree.Eat();
		return AstExprSPtr{ new AstPostfixExpr{ expr, tok } };
	}

	AstExprSPtr Parser::ParsePrefixExpr()
	{
		Token& tok = m_TokTree.Eat();
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstPrefixExpr{ tok, expr } };
	}

	AstExprSPtr Parser::ParseQualNameExpr()
	{
		AstQualNameSPtr qualName = ParseQualName(false);
		return AstExprSPtr{ new AstQualNameExpr{ qualName } };
	}

	AstExprSPtr Parser::ParseIndexSlicExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = m_TokTree.TryEat(TokenType::QuestionBracket);
		if (!nullcoalesce)
			m_TokTree.Eat(TokenType::LBracket);
		
		AstExprSPtr index = ParseExpression();
		u64 endIdx = m_TokTree.Eat(TokenType::RBracket).spanId;
		return AstExprSPtr{ new AstIndexSliceExpr{ expr, nullcoalesce, index, endIdx } };
	}

	AstExprSPtr Parser::ParseFuncCallExpr(AstExprSPtr expr)
	{
		m_TokTree.Eat(TokenType::LParen);
		StdVector<AstArgSPtr> args = ParseArgs();
		u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		return AstExprSPtr{ new AstFuncCallExpr{ expr, std::move(args), endIdx } };
	}

	AstExprSPtr Parser::ParseMemberAccessExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = m_TokTree.TryEat(TokenType::QuestionDot);
		if (!nullcoalesce)
			m_TokTree.Eat(TokenType::Dot);
		
		Token& tok = m_TokTree.Eat(TokenType::Iden);
		StdString iden = tok.iden;
		return AstExprSPtr{ new AstMemberAccessExpr{ expr, nullcoalesce, std::move(iden), tok.spanId } };
	}

	AstExprSPtr Parser::ParseMethodcallExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = m_TokTree.TryEat(TokenType::QuestionDot);
		if (!nullcoalesce)
			m_TokTree.Eat(TokenType::Dot);

		AstQualIdenSPtr qualIden = ParseQualIden(false);
		AstIden& qualIdenRef = static_cast<AstIden&>(*qualIden);
		StdString name = qualIdenRef.iden;
		AstIdenSPtr iden{ new AstIden{ qualIdenRef.ctx->startIdx, std::move(name), std::move(qualIdenRef.args), qualIdenRef.ctx->endIdx } };

		// Make sure that when the AstQualIdenSPtr is destroyed, that we are not trying to delete invalid data (i.e. use after move)
		new (&qualIdenRef.args) StdVector<AstGenericArg>{};

		m_TokTree.Eat(TokenType::LParen);
		StdVector<AstArgSPtr> args = ParseArgs();
		u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		
		return AstExprSPtr{ new AstMethodCallExpr{ expr, nullcoalesce, iden, std::move(args), endIdx } };
	}

	AstExprSPtr Parser::ParseTupleAccessExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = m_TokTree.TryEat(TokenType::QuestionDot);
		if (!nullcoalesce)
			m_TokTree.Eat(TokenType::Dot);
		
		Token& tok = m_TokTree.Eat(TokenType::I32Lit);
		return AstExprSPtr{ new AstTupleAccessExpr{ expr, nullcoalesce, u16(tok.sval), tok.spanId } };
	}

	AstExprSPtr Parser::ParseLiteralExpr()
	{
		Token& tok = m_TokTree.Eat();
		return AstExprSPtr{ new AstLiteralExpr{ tok } };
	}

	AstExprSPtr Parser::ParseAggrInitExpr(AstQualNameExpr* qualName)
	{
		AstTypeSPtr type = AstTypeSPtr{ new AstIdentifierType{ nullptr, qualName->qualName } };
		m_TokTree.Eat(TokenType::LBrace);
		StdVector<AstArgSPtr> args;
		do
		{
			args.push_back(ParseArg());
		} while (m_TokTree.Peek(1).type != TokenType::DotDot && m_TokTree.TryEat(TokenType::Comma));

		bool hasDefInit = false;
		AstExprSPtr defExpr;
		if (m_TokTree.TryEat(TokenType::Comma))
		{
			m_TokTree.Eat(TokenType::DotDot);
			hasDefInit = m_TokTree.Peek().type == TokenType::RBrace;
			if (!hasDefInit)
				defExpr = ParseExpression();
		}
		
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstExprSPtr{ new AstAggrInitExpr{ qualName->ctx->startIdx, type, std::move(args), hasDefInit, defExpr, endIdx } };
	}

	AstExprSPtr Parser::ParseArrayInitExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LBracket).spanId;
		StdVector<AstExprSPtr> exprs;
		do
		{
			exprs.push_back(ParseExpression());
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RBracket).spanId;
		return AstExprSPtr{ new AstArrayInitExpr{ startIdx, std::move(exprs), endIdx } };
	}

	AstExprSPtr Parser::ParseCastExpr(AstExprSPtr expr)
	{
		TokenType tokType = m_TokTree.Peek().type;
		if (tokType == TokenType::AsQuestion ||
			tokType == TokenType::AsExclaim)
			m_TokTree.Eat();
		else
			m_TokTree.Eat(TokenType::As);
		
		AstTypeSPtr type = ParseType();
		return AstExprSPtr{ new AstCastExpr{ expr, tokType, type } };
	}

	AstExprSPtr Parser::ParseTransmuteExpr(AstExprSPtr expr)
	{
		m_TokTree.Eat(TokenType::Transmute);
		AstTypeSPtr type = ParseType();
		return AstExprSPtr{ new AstTransmuteExpr{ expr, type } };
	}

	AstExprSPtr Parser::ParseMoveExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Move).spanId;
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstMoveExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseBracketExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LParen).spanId;
		AstExprSPtr expr = ParseExpression();

		if (m_TokTree.TryEat(TokenType::Comma))
		{
			StdVector<AstExprSPtr> exprs;
			exprs.push_back(expr);
			do
			{
				exprs.push_back(ParseExpression());
			}
			while (m_TokTree.TryEat(TokenType::Comma));
			
			u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
			return AstExprSPtr{ new AstTupleInitExpr{ startIdx, std::move(exprs), endIdx } };
		}

		u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		return AstExprSPtr{ new AstBracketExpr{ startIdx, expr, endIdx } };
	}

	AstExprSPtr Parser::ParseBlockExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LBrace).spanId;
		StdVector<AstStmtSPtr> stmts;
		while (m_TokTree.Peek().type != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = m_TokTree.Eat().spanId;
		return AstExprSPtr{ new AstBlockExpr{ startIdx, std::move(stmts), endIdx } };
	}

	AstExprSPtr Parser::ParseUnsafeExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Unsafe).spanId;
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstUnsafeExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseClosureExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Or).spanId;

		StdVector<AstParamSPtr> params = ParseParams(true);
		m_TokTree.Eat(TokenType::Or);

		AstTypeSPtr retType;
		if (m_TokTree.TryEat(TokenType::Arrow))
			retType = ParseType();

		AstExprSPtr expr = ParseExpression();

		return AstExprSPtr{ new AstClosureExpr{ startIdx, std::move(params), retType, expr } };
	}

	AstExprSPtr Parser::ParseIsExpr(AstExprSPtr expr)
	{
		u64 isIdx = m_TokTree.Eat().spanId;
		AstTypeSPtr type = ParseType();
		return AstExprSPtr{ new AstIsExpr{ expr, isIdx, type } };
	}

	AstExprSPtr Parser::ParseTryExpr()
	{
		Token& tok = m_TokTree.Eat();
		AstExprSPtr	expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstTryExpr{ tok, expr } };
	}

	AstExprSPtr Parser::ParseSpecKwExpr()
	{
		Token& tok = m_TokTree.Eat();
		return AstExprSPtr{ new AstSpecKwExpr{ tok } };
	}

	AstExprSPtr Parser::ParseCompRunExpr()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::SRun).spanId;
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstCompRunExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseMacroVarExpr()
	{
		u64 dollarIdx = m_TokTree.Eat(TokenType::MacroIden).spanId;
		Token& tok = m_TokTree.Eat(TokenType::Iden);
		StdString iden = tok.iden;
		return AstExprSPtr{ new AstMacroVarExpr{ dollarIdx, std::move(iden), tok.spanId } };
	}

	AstTypeSPtr Parser::ParseType(bool structKwOptional)
	{
		AstAttribsSPtr attribs = ParseAttributes();
		
		Token& tok = m_TokTree.Peek();
		switch (tok.type)
		{
		case TokenType::Bool:
		case TokenType::Char:
		case TokenType::I8:
		case TokenType::I16:
		case TokenType::I32:
		case TokenType::I64:
		case TokenType::I128:
		case TokenType::ISize:
		case TokenType::U8:
		case TokenType::U16:
		case TokenType::U32:
		case TokenType::U64:
		case TokenType::U128:
		case TokenType::USize:
		case TokenType::F16:
		case TokenType::F32:
		case TokenType::F64:
		case TokenType::F128:
		{
			m_TokTree.Eat();
			return AstTypeSPtr{ new AstBuiltinType{ attribs, tok } };
		}
		case TokenType::Iden:
			return ParseIdentifierType(attribs);
		case TokenType::Asterisk:
			return ParsePointerType(attribs);
		case TokenType::And:
			return ParseReferenceType(attribs);
		case TokenType::LBracket:
			return ParseArraySliceType(attribs);
		case TokenType::LParen:
			return ParseTupleType(attribs);
		case TokenType::Question:
			return ParseOptionalType(attribs);
		case TokenType::Struct:
			return ParseInlineStructType();
		case TokenType::Enum:
			return ParseInlineEnumType();
		case TokenType::LBrace:
		{
			if (!structKwOptional)
			{
				Span span = g_SpanManager.GetSpan(tok.spanId);
				g_ErrorSystem.Error(span, "Found '{' while parsing type\n");
				return nullptr;
			}
			return ParseInlineStructType(structKwOptional);
		}
		case TokenType::MacroIden:
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);
			switch (elem.varKind)
			{
			case MacroVarKind::Type:
			{
				m_TokTree.Eat();
				elem.Parse();
				AstTypeSPtr type = elem.GetType();
				if (attribs)
				{
					if (type->attribs)
					{
						type->attribs->visibility = attribs->visibility;
						type->attribs->simpleAttribs.insert(type->attribs->simpleAttribs.begin(), attribs->simpleAttribs.begin(), attribs->simpleAttribs.end());
						type->attribs->compAttribs.insert(type->attribs->compAttribs.begin(), attribs->compAttribs.begin(), attribs->compAttribs.end());
						type->attribs->userAttribs.insert(type->attribs->userAttribs.begin(), attribs->userAttribs.begin(), attribs->userAttribs.end());
					}
					else
					{
						type->attribs = attribs;
					}
				}
				return type;
			}
			case MacroVarKind::Qual:
			case MacroVarKind::Iden:
			{
				return ParseIdentifierType(attribs);
			}
			case MacroVarKind::Toks:
			{
				m_TokTree.InsertAtCur(elem.tokTree);
				return ParseType(structKwOptional);
			}
			default:
			{
				Span span = g_SpanManager.GetSpan(tok.spanId);
				const char* pName = tok.iden.c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'type', 'iden', 'qual' or 'toks'", pName);
				return nullptr;
			}
			}
		}
		default:
			return ParseIdentifierType(attribs);
		}
	}

	AstIdentifierTypeSPtr Parser::ParseIdentifierType(AstAttribsSPtr attribs)
	{;
		AstQualNameSPtr qualName = ParseQualName(true);
		return AstIdentifierTypeSPtr{ new AstIdentifierType{ attribs, qualName } };
	}

	AstTypeSPtr Parser::ParsePointerType(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Asterisk).spanId;
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstPointerType{ attribs, startIdx, type } };
	}

	AstTypeSPtr Parser::ParseReferenceType(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::And).spanId;
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstReferenceType{ attribs, startIdx, type } };
	}

	AstTypeSPtr Parser::ParseArraySliceType(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LBracket).spanId;

		if (m_TokTree.TryEat(TokenType::RBracket))
		{
			AstTypeSPtr type = ParseType();
			return AstTypeSPtr{ new AstSliceType{ attribs, startIdx, type } };
		}

		AstExprSPtr expr = ParseExpression();
		m_TokTree.Eat(TokenType::RBracket);
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstArrayType{ attribs, startIdx, expr, type } };
	}

	AstTypeSPtr Parser::ParseTupleType(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LParen).spanId;
		if (m_TokTree.Peek().type == TokenType::RParen)
		{
			u64 endIdx = m_TokTree.Eat().spanId;
			return AstTypeSPtr{ new AstTupleType{ attribs, startIdx, {}, endIdx} };
		}
		
		StdVector<AstTypeSPtr> types;
		do { types.push_back(ParseType()); }
			while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat().spanId;
		return AstTypeSPtr{ new AstTupleType{ attribs, startIdx, std::move(types), endIdx } };
	}

	AstTypeSPtr Parser::ParseOptionalType(AstAttribsSPtr attribs)
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Question).spanId;
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstOptionalType{ attribs, startIdx, type } };
	}

	AstTypeSPtr Parser::ParseInlineStructType(bool structKwOptional)
	{
		u64 startIdx;
		if (structKwOptional && m_TokTree.Peek().type != TokenType::Struct)
		{
			startIdx = m_TokTree.Eat(TokenType::LBrace).spanId;
		}
		else
		{
			startIdx = m_TokTree.Eat(TokenType::Struct).spanId;
			m_TokTree.Eat(TokenType::LBrace);
		}

		StdPairVector<StdVector<StdString>, AstTypeSPtr> members;
		do
		{
			StdVector<StdString> idens;
			do
			{
				idens.push_back(m_TokTree.Eat(TokenType::Iden).iden);
			}
			while (m_TokTree.TryEat(TokenType::Comma));
			m_TokTree.Eat(TokenType::Colon);
			AstTypeSPtr type = ParseType();
			AstExprSPtr bitfield;
			if (m_TokTree.TryEat(TokenType::Colon))
			{
				bitfield = ParseCommaExpression();
			}
			
			members.emplace_back(std::move(idens), type);
		}
		while(m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstTypeSPtr{ new AstInlineStructType{ startIdx, std::move(members), endIdx } };
	}

	AstTypeSPtr Parser::ParseInlineEnumType()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Enum).spanId;
		m_TokTree.Eat(TokenType::LBrace);
		StdPairVector<StdString, AstExprSPtr> members;
		do
		{
			StdPair<StdString, AstExprSPtr> member;
			member.first = m_TokTree.Eat(TokenType::Iden).iden;
			if (m_TokTree.Peek().type == TokenType::Eq)
			{
				m_TokTree.Eat();
				member.second = ParseExpression();
			}
			
			members.push_back(member);
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstTypeSPtr{ new AstInlineEnumType{ startIdx, std::move(members), endIdx } };
	}

	AstPatternSPtr Parser::ParsePattern()
	{
		Token tok = m_TokTree.Peek();
		AstPatternSPtr pattern;
		if (tok.type == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);
			switch (elem.varKind)
			{
			case MacroVarKind::Patr:
			{
				elem.Parse();
				pattern = elem.GetPattern();
				break;
			}
			case MacroVarKind::Toks:
			{
				m_TokTree.InsertAtCur(elem.tokTree);
				tok = m_TokTree.Peek();
				break;
			}
			default:
			{
				Span span = g_SpanManager.GetSpan(tok.spanId);
				const char* pName = tok.iden.c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'patr' or 'toks'", pName);
				return nullptr;
			}
			}
		}
		
		switch (tok.type)
		{
		case TokenType::DotDotDot:
		{
			m_TokTree.Eat();
			pattern = AstPatternSPtr{ new AstWildcardPattern{ tok.spanId } };
			break;
		}
		case TokenType::ColonColon:
		case TokenType::Iden:
		{
			if (tok.iden == "_")
			{
				m_TokTree.Eat();
				pattern = AstPatternSPtr{ new AstPlaceholderPattern{ tok.spanId } };
				break;
			}

			AstQualNameSPtr qualName = ParseQualName(false);
			Token& curTok = m_TokTree.Peek();
			if (curTok.type == TokenType::DollarParen ||
				curTok.type == TokenType::DollarBracket ||
				curTok.type == TokenType::DollarBrace)
			{
				pattern = ParseMacroInstPattern(qualName);
				break;
			}
			
			if (!qualName->hasColonColon && qualName->idens.size() == 1 && qualName->idens[0]->qualIdenKind == AstQualIdenKind::Identifier)
			{
				AstIden* pIden = static_cast<AstIden*>(qualName->idens[0].get());
				StdString iden = pIden->iden;
				pattern = ParseValueBindPattern(std::move(iden));
				break;
			}

			if (m_TokTree.Peek().type == TokenType::LBrace)
			{
				pattern = ParseAggrPattern(qualName);
				break;
			}

			pattern = ParseEnumPattern(qualName);
			break;
		}

		case TokenType::LParen:
		{
			pattern = ParseTuplePattern();
			break;
		}
		case TokenType::LBracket:
		{
			pattern = ParseSlicePattern();
			break;
		}
		case TokenType::Is:
		{
			pattern = ParseTypePattern();
			break;
		}
		case TokenType::True:
		case TokenType::False:
		case TokenType::Null:
		case TokenType::CharLit:
		case TokenType::I8Lit:
		case TokenType::I16Lit:
		case TokenType::I32Lit:
		case TokenType::I64Lit:
		case TokenType::I128Lit:
		case TokenType::U8Lit:
		case TokenType::U16Lit:
		case TokenType::U32Lit:
		case TokenType::U64Lit:
		case TokenType::U128Lit:
		case TokenType::F16Lit:
		case TokenType::F32Lit:
		case TokenType::F64Lit:
		case TokenType::F128Lit:
		case TokenType::StringLit:
		{
			m_TokTree.Eat();
			TokenType nextType = m_TokTree.Peek().type;
			if (nextType == TokenType::DotDot ||
				nextType == TokenType::DotDotEq)
			{
				pattern = ParseRangePattern(tok);
			}
			else
			{
				pattern = AstPatternSPtr{ new AstLiteralPattern{ tok } };
			}
			break;
		}
		default:
		{
			Span span = g_SpanManager.GetSpan(tok.spanId);
			const char* pTokName = GetTokenTypeName(tok.type).data();
			g_ErrorSystem.Error(span, "Unexpected token '%s' during pattern parsing", pTokName);
			m_TokTree.Eat();
			break;
		}
		}

		switch (m_TokTree.Peek().type)
		{
		case TokenType::Or:
			return ParseEitherPattern(pattern);
		default:
			return pattern;
		}
	}

	AstPatternSPtr Parser::ParseValueBindPattern(StdString&& iden)
	{
		u64 startIdx = m_TokTree.Peek().spanId;
		u64 endIdx = startIdx;
		AstPatternSPtr subPattern;
		if (m_TokTree.TryEat(TokenType::Arrow))
		{
			subPattern = ParsePattern();
			endIdx = subPattern->ctx->endIdx;
		}
		return AstPatternSPtr{ new AstValueBindPattern{ startIdx, std::move(iden), subPattern, endIdx } };
	}

	AstPatternSPtr Parser::ParseRangePattern(Token from)
	{
		bool inclusive = m_TokTree.Eat().type == TokenType::DotDotEq;
		Token to = m_TokTree.Eat();
		return AstPatternSPtr{ new AstRangePattern{ from, inclusive, to } };
	}

	AstPatternSPtr Parser::ParseTuplePattern()
	{
		u64 startIdx = m_TokTree.Eat().spanId;
		StdVector<AstPatternSPtr> subPatterns;
		do
		{
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.push_back(pattern);
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		return AstPatternSPtr{ new AstTuplePattern{ startIdx, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseEnumPattern(AstQualNameSPtr iden)
	{
		u64 startIdx = iden->ctx->startIdx;
		u64 endIdx = startIdx;
		StdVector<AstPatternSPtr> subPatterns;
		if (m_TokTree.TryEat(TokenType::LParen))
		{
			do
			{
				AstPatternSPtr pattern = ParsePattern();
				subPatterns.push_back(pattern);
			} while (m_TokTree.TryEat(TokenType::Comma));
			endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		}
		return AstPatternSPtr{ new AstEnumPattern{ startIdx, iden, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseAggrPattern(AstQualNameSPtr qualName)
	{
		m_TokTree.Eat();
		StdPairVector<StdString, AstPatternSPtr> subPatterns;
		do
		{
			StdString iden;
			iden = m_TokTree.Eat(TokenType::Iden).iden;
			m_TokTree.Eat(TokenType::Colon);
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.emplace_back(std::move(iden), pattern);
		} while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstPatternSPtr{ new AstAggrPattern{ qualName, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseSlicePattern()
	{
		u64 startIdx = m_TokTree.Eat().spanId;
		StdVector<AstPatternSPtr> subPatterns;
		do
		{
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.push_back(pattern);
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::RBracket).spanId;
		return AstPatternSPtr{ new AstSlicePattern{ startIdx, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseEitherPattern(AstPatternSPtr pattern)
	{
		u64 startIdx = pattern->ctx->startIdx;
		StdVector<AstPatternSPtr> subPatterns;
		subPatterns.push_back(pattern);
		while (m_TokTree.TryEat(TokenType::Or))
		{
			pattern = ParsePattern();
			subPatterns.push_back(pattern);
		}
		return AstPatternSPtr{ new AstEitherPattern{ std::move(subPatterns) } };
	}

	AstPatternSPtr Parser::ParseTypePattern()
	{
		u64 startIdx = m_TokTree.Eat().spanId;
		AstTypeSPtr type = ParseType();
		return AstPatternSPtr{ new AstTypePattern{ startIdx, type } };
	}

	AstAttribsSPtr Parser::ParseAttributes()
	{
		StdVector<AstCompAttribSPtr> compAttribs;
		StdVector<AstUserAttribSPtr> userAttribs;
		StdVector<AstSimpleAttribSPtr> simpleAttribs;
		AstVisibilityAttribSPtr visibilityAttrib = AstVisibilityAttribSPtr{};

		u64 startIdx = u64(-1), endIdx = u64(-1);
		
		bool run = true;
		while (run)
		{
			switch (m_TokTree.Peek().type)
			{
			case TokenType::Comptime:
			case TokenType::Const:
			case TokenType::Immutable:
			case TokenType::Lazy:
			case TokenType::Static:
			case TokenType::Mut:
			{
				Token& tok = m_TokTree.Eat();
				simpleAttribs.push_back(AstSimpleAttribSPtr{ new AstSimpleAttrib{ tok } });
				break;
			}
			case TokenType::Public:
				visibilityAttrib = ParseVisibilityAttribute();
				break;
			case TokenType::At:
				userAttribs.push_back(ParseUserAttribute());
				break;
			case TokenType::AtColon:
				compAttribs.push_back(ParseCompAttribute());
				break;
			default:
				run = false;
				break;
			}
		}

		if (compAttribs.size() == 0 &&
			userAttribs.size() == 0 &&
			simpleAttribs.size() == 0 &&
			!visibilityAttrib)
			return nullptr;
		
		return AstAttribsSPtr{ new AstAttribs{ startIdx, std::move(compAttribs), std::move(userAttribs), visibilityAttrib, std::move(simpleAttribs), endIdx  } };
	}

	AstCompAttribSPtr Parser::ParseCompAttribute()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::AtColon).spanId;
		Token idenTok = m_TokTree.Eat(TokenType::Iden);
		StdString iden = idenTok.iden;
		u64 endIdx = idenTok.spanId;

		StdVector<AstArgSPtr> args;
		if (m_TokTree.TryEat(TokenType::LParen))
		{
			args = ParseArgs();
			endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		}

		return AstCompAttribSPtr{ new AstCompAttrib{ startIdx, std::move(iden), std::move(args), endIdx } };
	}

	AstUserAttribSPtr Parser::ParseUserAttribute()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::AtColon).spanId;
		Token idenTok = m_TokTree.Eat(TokenType::Iden);
		StdString iden = idenTok.iden;
		u64 endIdx = idenTok.spanId;

		StdVector<AstArgSPtr> args;
		if (m_TokTree.TryEat(TokenType::LParen))
		{
			args = ParseArgs();
			endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		}

		return AstUserAttribSPtr{ new AstUserAttrib{ startIdx, std::move(iden), std::move(args), endIdx } };
	}

	AstVisibilityAttribSPtr Parser::ParseVisibilityAttribute()
	{
		u64 startIdx = m_TokTree.Eat().spanId;
		u64 endIdx = startIdx;

		StdString kind;
		if (m_TokTree.TryEat(TokenType::LParen))
		{
			Token& tok = m_TokTree.Eat();

			if (tok.iden != "module" &&
				tok.iden != "package" &&
				tok.iden != "dynlib")
			{
				Span span = g_SpanManager.GetSpan(tok.spanId);
				const char* pFound = tok.iden.c_str();
				g_ErrorSystem.Error(span, "Found '%', expected 'module', 'package' or 'dynlib'", pFound);
			}
			else
			{
				kind = tok.iden;
			}
			
			endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		}

		return AstVisibilityAttribSPtr{ new AstVisibilityAttrib{ startIdx, std::move(kind), endIdx } };
	}

	AstGenericDeclSPtr Parser::ParseGenericDecl()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Less).spanId;
		StdVector<AstGenericParam> params;
		do
		{
			params.push_back(ParseGenericParam());
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		u64 endIdx = m_TokTree.Eat(TokenType::Greater).spanId;
		return AstGenericDeclSPtr{ new AstGenericDecl{ startIdx, std::move(params), endIdx } };
	}

	AstGenericParam Parser::ParseGenericParam()
	{
		if (m_TokTree.TryEat(TokenType::Colon))
		{
			if (m_TokTree.TryEat(TokenType::LBrace))
			{
				AstExprSPtr expr = ParseExpression();
				m_TokTree.Eat(TokenType::RBrace);
				return AstGenericParam{ expr };
			}
			return AstGenericParam{ ParseType() };
		}

		Token& tok = m_TokTree.Eat(TokenType::Iden);
		u64 startIdx = tok.spanId;
		StdString iden = tok.iden;

		if (m_TokTree.TryEat(TokenType::Colon))
		{
			AstTypeSPtr type = ParseType();
			AstExprSPtr def;
			if (m_TokTree.TryEat(TokenType::Eq))
				def = ParseExpression(nullptr, true);

			return AstGenericParam{ AstGenericValueParamSPtr{ new AstGenericValueParam{ startIdx, std::move(iden), type, def } } };
		}

		StdVector<AstIdentifierTypeSPtr> interfaces;
		if (m_TokTree.TryEat(TokenType::Is))
		{
			do
			{
				interfaces.push_back(ParseIdentifierType());
			}
			while (m_TokTree.TryEat(TokenType::Plus));
		}

		AstTypeSPtr def;
		if (m_TokTree.TryEat(TokenType::Eq))
			def = ParseType();
		return AstGenericParam{ AstGenericTypeParamSPtr{ new AstGenericTypeParam{ startIdx, std::move(iden), std::move(interfaces), def } } };
	}

	AstGenericTypeBoundSPtr Parser::ParseGenericTypeBound()
	{
		AstTypeSPtr type = ParseType();
		m_TokTree.Eat(TokenType::Is);
		AstGenericBoundTypeSPtr bound = ParseGenericBoundType();
		return AstGenericTypeBoundSPtr{ new AstGenericTypeBound{ type, bound } };
	}

	AstGenericBoundTypeSPtr Parser::ParseGenericBoundType()
	{
		AstTypeSPtr type = ParseType();

		StdVector<AstGenericAssocTypeBound> bounds;
		if (m_TokTree.TryEatIden("with"))
		{
			m_TokTree.Eat(TokenType::LParen);
			while (!m_TokTree.TryEat(TokenType::RParen))
			{
				AstGenericAssocTypeBound assocBound = ParseAssocBounds();
				bounds.push_back(std::move(assocBound));

				m_TokTree.TryEat(TokenType::Comma);
			}
		}

		return AstGenericBoundTypeSPtr{ new AstGenericBoundType{ type, std::move(bounds) } };
	}

	AstGenericAssocTypeBound Parser::ParseAssocBounds()
	{
		StdString iden = ParseIden();
		m_TokTree.Eat(TokenType::Is);
		AstGenericBoundTypeSPtr type = ParseGenericBoundType();
		return AstGenericAssocTypeBound{ std::move(iden), type };
	}

	AstGenericWhereClauseSPtr Parser::ParseGenericWhereClause()
	{
		u64 startIdx = m_TokTree.Eat().spanId;
		StdVector<AstGenericTypeBoundSPtr> bounds;
		do
		{
			bounds.push_back(ParseGenericTypeBound());
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		
		return AstGenericWhereClauseSPtr{ new AstGenericWhereClause{ startIdx, std::move(bounds) } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroVar()
	{
		Token& tok = m_TokTree.Eat(TokenType::MacroIden);
		StdString iden = tok.iden;
		m_TokTree.Eat(TokenType::Colon);
		Token& typeTok = m_TokTree.Eat(TokenType::Iden);

		AstMacroVarKind kind = AstMacroVarKind::Unknown;
		if (typeTok.iden == "stmt")
			kind = AstMacroVarKind::Stmt;
		else if (typeTok.iden == "expr")
			kind = AstMacroVarKind::Expr;
		else if (typeTok.iden == "type")
			kind = AstMacroVarKind::Type;
		else if (typeTok.iden == "qual")
			kind = AstMacroVarKind::Qual;
		else if (typeTok.iden == "iden")
			kind = AstMacroVarKind::Iden;
		else if (typeTok.iden == "attr")
			kind = AstMacroVarKind::Attr;
		else if (typeTok.iden == "toks")
			kind = AstMacroVarKind::Toks;
		else if (typeTok.iden == "patr")
			kind = AstMacroVarKind::Toks;

		if (kind == AstMacroVarKind::Unknown)
		{
			Span span = g_SpanManager.GetSpan(typeTok.spanId);
			const char* pKind = typeTok.iden.c_str();
			g_ErrorSystem.Error(span, "Unknown macro variable kind: '%s'", pKind);
		}
		
		return AstMacroPatternElemSPtr{ new AstMacroVar{ tok.spanId, std::move(iden), kind, typeTok.spanId } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroSeparator()
	{
		StdVector<Token> toks;
		Token tok = m_TokTree.Peek();
		do
		{
			toks.push_back(std::move(tok));
			m_TokTree.Eat();
			tok = m_TokTree.Peek();
		}
		while (tok.type != TokenType::RParen && 
			   tok.type != TokenType::MacroIden &&
			   tok.type != TokenType::DollarParen);

		return AstMacroPatternElemSPtr{ new AstMacroSeparator{ std::move(toks) } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroFragment()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::DollarParen).spanId;
		AstMacroPatternSPtr pattern = ParseMacroPattern(true);
		u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		Token repTok = Token{ TokenType::Unknown, "", u64(-1) };
		Token rep = m_TokTree.Peek();
		if (rep.type == TokenType::Question ||
			rep.type == TokenType::Plus ||
			rep.type == TokenType::Asterisk)
		{
			 endIdx = m_TokTree.Eat().spanId;
		}
		else
		{
			repTok = rep;
			rep = m_TokTree.Peek(1);
			if (rep.type == TokenType::Question ||
				rep.type == TokenType::Plus ||
				rep.type == TokenType::Asterisk)
			{
				m_TokTree.Eat();
				endIdx = m_TokTree.Eat().spanId;
			}
			else
			{
				repTok = rep = Token{ TokenType::Unknown, "", u64(-1) };
			}
		}
		return AstMacroPatternElemSPtr{ new AstMacroFragment{ startIdx, pattern, repTok, rep.type, endIdx } };
	}

	AstMacroPatternSPtr Parser::ParseMacroPattern(bool inFragment)
	{
		StdVector<AstMacroPatternElemSPtr> elems;
		while (m_TokTree.Peek().type != TokenType::RParen)
		{
			if (m_TokTree.Peek().type == TokenType::MacroIden)
			{
				elems.push_back(ParseMacroVar());
			}
			else if (m_TokTree.Peek().type == TokenType::DollarParen)
			{
				if (inFragment)
				{
					Span span = g_SpanManager.GetSpan(m_TokTree.Peek().spanId);
					g_ErrorSystem.Error(span, "Cannot have nested macro pattern fragments");
				}
				else
				{
					elems.push_back(ParseMacroFragment());
				}
			}
			else
			{
				elems.push_back(ParseMacroSeparator());
			}
		}

		u64 startIdx = elems.front()->ctx->startIdx;
		u64 endIdx = elems.back()->ctx->endIdx;
		return AstMacroPatternSPtr{ new AstMacroPattern { startIdx, std::move(elems), endIdx } };
	}

	AstMacroRuleSPtr Parser::ParseMacroRule()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::LParen).spanId;
		AstMacroPatternSPtr pattern = ParseMacroPattern();
		m_TokTree.Eat(TokenType::RParen);
		m_TokTree.Eat(TokenType::DblArrow);
		m_TokTree.Eat(TokenType::LBrace);
		TokenTree body = ParseTokenTree();
		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstMacroRuleSPtr{ new AstMacroRule{ startIdx, pattern, std::move(body), endIdx } };
	}

	AstDeclSPtr Parser::ParseDeclMacro()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Macro).spanId;
		StdString iden = m_TokTree.Eat(TokenType::Iden).iden;

		if (m_TokTree.TryEat(TokenType::LParen))
		{
			AstMacroPatternSPtr pattern = ParseMacroPattern();
			m_TokTree.Eat(TokenType::RParen);
			TokenTree body = ParseTokenTree();
			u64 endIdx = body.subToks.back().tok.spanId;
			return AstDeclSPtr{ new AstDeclMacro{ startIdx, std::move(iden), pattern, std::move(body), endIdx } };
		}

		m_TokTree.Eat(TokenType::LBrace);
		StdVector<AstMacroRuleSPtr> rules;
		rules.push_back(ParseMacroRule());
		while (m_TokTree.TryEat(TokenType::Comma))
			rules.push_back(ParseMacroRule());

		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstDeclSPtr{ new AstRulesDeclMacro{ startIdx, std::move(iden), std::move(rules), endIdx } };
	}

	AstDeclSPtr Parser::ParseProcMacro()
	{
		u64 startIdx = m_TokTree.Eat(TokenType::Macro).spanId;
		m_TokTree.Eat(TokenType::Func);
		StdString iden = m_TokTree.Eat(TokenType::Iden).iden;
		m_TokTree.Eat(TokenType::LParen);
		StdString toksIden = m_TokTree.Eat(TokenType::Iden).iden;
		m_TokTree.Eat(TokenType::RParen);

		if (m_TokTree.TryEat(TokenType::LParen))
		{
			AstMacroPatternSPtr pattern = ParseMacroPattern();
			m_TokTree.Eat(TokenType::RParen);
			m_TokTree.Eat(TokenType::LBrace);
			TokenTree body = ParseTokenTree();
			u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
			return AstDeclSPtr{ new AstProcMacro{ startIdx, std::move(iden), std::move(toksIden), pattern, std::move(body), endIdx } };
		}

		m_TokTree.Eat(TokenType::LBrace);
		StdVector<AstMacroRuleSPtr> rules;
		rules.push_back(ParseMacroRule());
		while (m_TokTree.TryEat(TokenType::Comma))
			rules.push_back(ParseMacroRule());

		u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
		return AstDeclSPtr{ new AstRulesProcMacro{ startIdx, std::move(iden), std::move(toksIden), std::move(rules), endIdx } };
	}

	TokenTree Parser::ParseTokenTree(Token firstTok)
	{
		if (firstTok.type == TokenType::Unknown)
			firstTok = m_TokTree.Eat();

		TokenType closeType;
		switch (firstTok.type)
		{
		case TokenType::LParen:
		case TokenType::DollarParen:
		{
			closeType = TokenType::RParen;
			break;
		}
		case TokenType::LBracket:
		case TokenType::DollarBracket:
		{
			closeType = TokenType::RBracket;
			break;
		}
		case TokenType::LBrace:
		case TokenType::DollarBrace:
		{
			closeType = TokenType::RBrace;
			break;
		}
		default:
			return TokenTree{ firstTok };
		}
		
		StdVector<TokenTree> subTrees;

		if (firstTok.type != TokenType::Unknown)
			subTrees.push_back(TokenTree{ firstTok });
		
		Token tok = m_TokTree.Peek();
		while (tok.type != closeType)
		{
			switch (tok.type)
			{
			case TokenType::LParen:
			case TokenType::DollarParen:
			case TokenType::LBracket:
			case TokenType::LBrace:
			case TokenType::DollarBrace:
			{
				m_TokTree.Eat();
				subTrees.push_back(ParseTokenTree(tok));
				break;
			}
			default:
			{
				subTrees.push_back(TokenTree{ m_TokTree.Eat() });
				break;
			}
			}
			tok = m_TokTree.Peek();
		}

		subTrees.push_back(TokenTree{ m_TokTree.Eat() });
		return TokenTree{ std::move(subTrees) };
	}

	AstExprSPtr Parser::ParseMacroInstExpr(AstQualNameSPtr qualName)
	{
		TokenTree tokTree = ParseTokenTree();
		u64 endIdx = tokTree.subToks.back().tok.spanId;

		// Trim first and last tokens, i.e. '!(' '![' '!{' and '}' ']' ')'
		tokTree.subToks.erase(tokTree.subToks.begin());
		tokTree.subToks.erase(tokTree.subToks.end() - 1);
		
		return AstExprSPtr{ new AstMacroInstExpr{ std::move(qualName), std::move(tokTree), endIdx } };
	}

	AstPatternSPtr Parser::ParseMacroInstPattern(AstQualNameSPtr qualName)
	{
		if (m_TokTree.TryEat(TokenType::DollarBrace))
		{
			TokenTree tokTree = ParseTokenTree();
			u64 endIdx = m_TokTree.Eat(TokenType::RBrace).spanId;
			return AstPatternSPtr{ new AstMacroInstPattern{ std::move(qualName), std::move(tokTree), endIdx } };
		}
		if (m_TokTree.TryEat(TokenType::DollarBracket))
		{
			TokenTree tokTree = ParseTokenTree();
			u64 endIdx = m_TokTree.Eat(TokenType::RBracket).spanId;
			return AstPatternSPtr{ new AstMacroInstPattern{ std::move(qualName), std::move(tokTree), endIdx } };
		}

		m_TokTree.Eat(TokenType::DollarParen);
		TokenTree tokTree = ParseTokenTree();
		u64 endIdx = m_TokTree.Eat(TokenType::RParen).spanId;
		return AstPatternSPtr{ new AstMacroInstPattern{ std::move(qualName), std::move(tokTree), endIdx } };
	}

	AstQualNameSPtr Parser::ParseQualName(bool genericInstWithLess)
	{
		u64 startIdx = u64(-1);
		bool global = false;
		if (m_TokTree.Peek().type == TokenType::ColonColon)
		{
			startIdx = m_TokTree.Eat().spanId;
			global = true;
		}

		Token tok = m_TokTree.Peek();
		if (tok.type == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);
			switch (elem.varKind)
			{
			case MacroVarKind::Qual:
			{
				m_TokTree.Eat();
				elem.Parse();
				return elem.GetQualName();
			}
			case MacroVarKind::Iden:
				break;
			case MacroVarKind::Toks:
			{
				m_TokTree.InsertAtCur(elem.tokTree);
				tok = m_TokTree.Peek();
				break;
			}
			default:
			{
				Span span = g_SpanManager.GetSpan(tok.spanId);
				const char* pName = tok.iden.c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'qual', 'iden' or 'toks'", pName);
				return nullptr;
			}
			}
		}
		
		StdVector<AstQualIdenSPtr> qualIdens;
		do
		{
			qualIdens.push_back(ParseQualIden(genericInstWithLess));
		}
		while (m_TokTree.TryEat(TokenType::ColonColon));

		return AstQualNameSPtr{ new AstQualName{ startIdx, global, std::move(qualIdens) } };
	}

	AstQualIdenSPtr Parser::ParseQualIden(bool genericInstWithLess)
	{
		Token tok = m_TokTree.Peek();
		u64 endIdx = tok.spanId;

		if (tok.type == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);
			if (elem.varKind == MacroVarKind::Toks)
			{
				tok = m_TokTree.Peek();
				m_TokTree.InsertAtCur(elem.tokTree);
			}
		}
		
		if (tok.type == TokenType::Less)
		{
			u64 tmpIdx = m_TokTree.Eat(TokenType::Less).spanId;
			AstTypeSPtr type = ParseType();
			m_TokTree.Eat(TokenType::As);
			AstIdentifierTypeSPtr interface = ParseIdentifierType();
			endIdx = m_TokTree.Eat(TokenType::Greater).spanId;
			return AstQualIdenSPtr{ new AstTypeDisambiguation{ tmpIdx, type, interface, endIdx } };
		}
		else
		{
			StdString iden = ParseIden();
			StdVector<AstGenericArg> args;
			if (genericInstWithLess && m_TokTree.TryEat(TokenType::Less) ||
				!genericInstWithLess && m_TokTree.TryEat(TokenType::ExclaimLess))
			{
				do
				{
					if (m_TokTree.Peek().type == TokenType::LBrace)
					{
						m_TokTree.Eat(TokenType::LBrace);
						args.push_back(AstGenericArg{ ParseExpression() });
						m_TokTree.Eat(TokenType::RBrace);
					}
					else
					{
						args.push_back(AstGenericArg{ ParseType() });
					}
				} while (m_TokTree.TryEat(TokenType::Comma));
				endIdx = m_TokTree.Eat(TokenType::Greater).spanId;
			}

			return AstQualIdenSPtr{ new AstIden{ tok.spanId, std::move(iden), std::move(args), endIdx } };
		}
	}

	StdVector<AstParamSPtr> Parser::ParseParams(bool allowNoType)
	{
		StdVector<AstParamSPtr> params;
		if (m_TokTree.Peek().type == TokenType::LParen ||
			m_TokTree.Peek().type == TokenType::Or)
			return params;
		
		do
		{
			params.push_back(ParseParam(allowNoType));
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		return params;
	}

	AstParamSPtr Parser::ParseParam(bool allowNoType)
	{
		u64 startIdx, endIdx;
		StdVector<AstParamVarSPtr> vars;

		startIdx = m_TokTree.Peek().spanId;
		do
		{
			AstAttribsSPtr attribs;
			if (m_TokTree.Peek().type != TokenType::Iden)
				attribs = ParseAttributes();
			StdString iden = ParseIden(endIdx);
			vars.emplace_back(new AstParamVar{ attribs, startIdx, std::move(iden), endIdx });
		}
		while (m_TokTree.TryEat(TokenType::Comma));

		if (m_TokTree.Peek().type != TokenType::Colon && allowNoType)
			return AstParamSPtr{ new AstParam{ startIdx, std::move(vars), nullptr, false, endIdx } };
		
		AstTypeSPtr type;
		bool isVariadic = false;
		if (m_TokTree.Peek().type == TokenType::DotDotDot)
		{
			endIdx = m_TokTree.Eat().spanId;
			isVariadic = true;
		}
		else if (m_TokTree.Peek().type == TokenType::Colon)
		{
			m_TokTree.Eat(TokenType::Colon);
			type = ParseType();
			endIdx = type->ctx->endIdx;
			
			if (m_TokTree.Peek().type == TokenType::DotDotDot)
			{
				endIdx = m_TokTree.Eat().spanId;
				isVariadic = true;
			}
		}

		return AstParamSPtr{ new AstParam{ startIdx, std::move(vars), type, isVariadic, endIdx } };
	}

	StdVector<AstArgSPtr> Parser::ParseArgs()
	{
		StdVector<AstArgSPtr> args;
		if (m_TokTree.Peek().type == TokenType::RParen)
			return args;
		
		do
		{
			args.push_back(ParseArg());
		}
		while (m_TokTree.TryEat(TokenType::Comma));
		return args;
	}

	AstArgSPtr Parser::ParseArg()
	{
		u64 startIdx;
		StdString iden;
		AstExprSPtr expr;
		if (m_TokTree.Peek().type == TokenType::Iden && m_TokTree.Peek(1).type == TokenType::Colon)
		{
			Token& tok = m_TokTree.Eat();
			startIdx = tok.spanId;
			iden = tok.iden;
			m_TokTree.Eat(TokenType::Colon);
			expr = ParseExpression();
		}
		else
		{
			expr = ParseExpression();
			startIdx = expr->ctx->startIdx;
		}

		return AstArgSPtr{ new AstArg{ startIdx, std::move(iden), expr } };
	}

	StdString Parser::ParseIden()
	{
		u64 dummy;
		return ParseIden(dummy);
	}

	StdString Parser::ParseIden(u64& tokIdx)
	{
		Token& tok = m_TokTree.Peek();
		tokIdx = tok.spanId;
		if (tok.type == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.iden);
			switch (elem.varKind)
			{
			case MacroVarKind::Iden:
			{
				m_TokTree.Eat();
				return elem.actIden;
			}
			case MacroVarKind::Toks:
			{
				if (elem.tokTree.subToks.size() != 1)
				{
					Span span = g_SpanManager.GetSpan(tok.spanId);
					const char* pName = tok.iden.c_str();
					g_ErrorSystem.Error("macro variable '$%s' contains more than 1 token when parsed as identifier");
					return "";
				}
				return elem.tokTree.subToks[0].tok.iden;
			}
			default:
			{
				Span span = g_SpanManager.GetSpan(tok.spanId);
				const char* pName = tok.iden.c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'iden' or 'toks'", pName);
				return "";
			}
			}
		}

		return m_TokTree.Eat(TokenType::Iden).iden;
	}

	StdVector<StdString> Parser::ParseIdenList(TokenType separator)
	{
		u64 start;
		return ParseIdenList(separator, start);
	}

	StdVector<StdString> Parser::ParseIdenList(TokenType separator, u64& startIdx)
	{
		Token& startTok = m_TokTree.Peek();
		startIdx = startTok.spanId;

		StdVector<StdString> idens;
		if (startTok.type != TokenType::Iden)
			return idens;
		
		idens.push_back(startTok.iden);
		m_TokTree.Eat();
		
		while (m_TokTree.TryEat(separator))
		{
			Token& tok = m_TokTree.Eat(TokenType::Iden);
			idens.push_back(tok.iden);
		}

		return idens;
	}

	Parser::OpPrecedence Parser::GetPrecedence(TokenType op)
	{
		switch (op)
		{
		case TokenType::Asterisk:
		case TokenType::Slash:
		case TokenType::Percent:
		case TokenType::Tilde:
			return OpPrecedence::MulDivRemCon;
		case TokenType::Plus:
		case TokenType::Minus:
			return OpPrecedence::AddMin;
		case TokenType::LessLess:
		case TokenType::LessLessLess:
		case TokenType::LessLessAsterisk:
		case TokenType::GreaterGreater:
		case TokenType::GreaterGreaterGreater:
		case TokenType::GreaterGreaterAsterisk:
			return OpPrecedence::ShiftRot;
		case TokenType::And:
			return OpPrecedence::BinAnd;
		case TokenType::Caret:
			return OpPrecedence::BinXor;
		case TokenType::Or:
			return OpPrecedence::BinOr;
		case TokenType::DotDot:
		case TokenType::DotDotEq:
			return OpPrecedence::Range;
		case TokenType::In:
			return OpPrecedence::Contains;
		case TokenType::EqEq:
		case TokenType::ExclaimEq:
		case TokenType::Less:
		case TokenType::LessEq:
		case TokenType::Greater:
		case TokenType::GreaterEq:
			return OpPrecedence::Cmp;
		case TokenType::QuestionQuestion:
		case TokenType::QuestionDot:
			return OpPrecedence::NullElvis;
		case TokenType::AndAnd:
			return OpPrecedence::LogAnd;
		case TokenType::OrOr:
			return OpPrecedence::LogOr;
		default:
			return OpPrecedence::None;
		}
	}

	u64 Parser::EatStmtEndIdx()
	{
		if (!m_IsSwitchStmt)
			return m_TokTree.Eat(TokenType::Semicolon).spanId;

		Token& tok = m_TokTree.Peek();
		if (tok.type == TokenType::Comma)
			return tok.spanId;
		return u64(-1);
	}
}
