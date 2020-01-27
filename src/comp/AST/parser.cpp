#include "parser.hpp"
#include "common/errorsystem.hpp"
#include "comp/span.hpp"
#include "common/context.hpp"
#include "comp/compcontext.hpp"
#include "comp/token.hpp"

namespace Noctis
{
	Parser::Parser(const StdVector<Token>& tokens, Context* pCtx)
		: m_Tokens(tokens)
		, m_TokIdx(0)
		, m_pCtx(pCtx)
	{
	}

	StdVector<AstStmtSPtr> Parser::Parse()
	{
		StdVector<AstStmtSPtr> nodes;

		while (m_TokIdx < m_Tokens.size())
		{
			Token& tok = PeekToken();
			switch (tok.Type())
			{
			case TokenType::Module:
				nodes.push_back(ParseModuleDecl());
				break;
			default:
				nodes.push_back(ParseStatement());
				break;
			}
		}

		return nodes;
	}

	AstDeclSPtr Parser::ParseModuleDecl()
	{
		u64 startIdx = EatToken(TokenType::Module).Idx();
		StdVector<StdString> idens;
		do
		{
			idens.push_back(EatToken(TokenType::Iden).Text());
		}
		while (TryEatToken(TokenType::Dot));

		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstDeclSPtr{ new AstModuleDecl{ startIdx, std::move(idens), endIdx } };
	}

	AstDeclSPtr Parser::ParseUnittestDecl()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("unittest");

		StdString name;
		if (PeekToken().Type() == TokenType::StringLit)
			name = EatToken().Text();

		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstDeclSPtr{ new AstUnittestDecl{ startIdx, std::move(name), std::move(stmts), endIdx } };
	}

	AstDeclSPtr Parser::ParseBenchmarkDecl()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("benchmark");

		StdString name;
		if (PeekToken().Type() == TokenType::StringLit)
			name = EatToken().Text();

		EatToken(TokenType::LParen);
		StdString stateIden = EatToken(TokenType::Iden).Text();
		EatToken(TokenType::RParen);

		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstDeclSPtr{ new AstBenchmarkDecl{ startIdx, std::move(name), std::move(stateIden), std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseStatement(AstAttribsSPtr attribs, bool funcAreMethods)
	{
		if (!attribs)
			attribs = ParseAttributes();

		AstLabelStmtSPtr label;
		if (PeekToken().Type() == TokenType::Colon)
			label = ParseLabelStmt();
		
		switch (PeekToken().Type())
		{
		case TokenType::Struct: return ParseStruct(attribs);
		case TokenType::Union: return ParseUnion(attribs);
		case TokenType::Enum: return ParseEnum(attribs);
		case TokenType::Interface: return ParseInterface(attribs);
		case TokenType::Weak: return ParseWeakInterface(attribs);
		case TokenType::Typealias: return ParseTypealias(attribs);
		case TokenType::Typedef: return ParseTypedef(attribs);
		case TokenType::Func:
		{
			if (funcAreMethods)
				return ParseMethodDecl(attribs);
			return ParseFuncDecl(attribs);
		}
		case TokenType::Impl: return ParseImplDecl(attribs);
		case TokenType::Import: return ParseImport(attribs);
		case TokenType::If: return ParseIfStmt();
		case TokenType::Loop: return ParseLoopStmt(label);
		case TokenType::While: return ParseWhileStmt(label);
		case TokenType::Do: return ParseDoWhileStmt(label);
		case TokenType::For: return ParseForStmt(label);
		case TokenType::Switch: return ParseSwitch(label);
		case TokenType::Break: return ParseBreakStmt();
		case TokenType::Continue: return ParseContinueStmt();
		case TokenType::Fallthrough: return ParseFallthroughStmt();
		case TokenType::Goto: return ParseGotoStmt();
		case TokenType::Return: return ParseReturnStmt();
		case TokenType::Defer: return ParseDeferStmt();
		case TokenType::StackDefer: return ParseStackDeferStmt();
		case TokenType::Unsafe: return ParseUnsafeStmt();
		case TokenType::LBrace: return ParseBlockStmt();
		case TokenType::DollarBrace: return ParseMacroLoopStmt();
		case TokenType::Macro:
		{
			if (PeekToken(1).Type() == TokenType::Func)
				return ParseProcMacro();
			return ParseDeclMacro();
		}
		case TokenType::Hash:
		{
			Token& tok = PeekToken(1);
			if (tok.Type() == TokenType::If)
				return ParseCompIfStmt();
			if (tok.Text() == "conditional")
				return ParseCompCondStmt();
			if (tok.Text() == "debug")
				return ParseCompDebugStmt();
			if (tok.Text() == "unittest")
				return ParseUnittestDecl();
			if (tok.Text() == "benchmark")
				return ParseBenchmarkDecl();
			return ParseExprStmt();
		}
		case TokenType::Iden:
		{
			u64 startIdx, endIdx;
			StdVector<StdString> idens = ParseIdenList(TokenType::Comma, startIdx, endIdx);

			Token& tok = PeekToken();
			if (tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
				return ParseVarDecl(attribs, startIdx, std::move(idens));

			m_TokIdx = startIdx;
			return ParseExprStmt();
		}
		default:
		{
			if (label)
				return label;
			return ParseExprStmt();
		}
		}
	}

	AstDeclSPtr Parser::ParseStruct(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Struct).Idx();

		StdString iden;
		if (PeekToken().Type() == TokenType::Iden)
			iden = EatToken(TokenType::Iden).Text();

		AstGenericDeclSPtr generics = nullptr;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		StdVector<AstStmtSPtr> members;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			members.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();
		
		return AstDeclSPtr{ new AstStructDecl{ attribs, startIdx, std::move(iden), generics, std::move(members), endIdx } };
	}

	AstDeclSPtr Parser::ParseUnion(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Union).Idx();

		StdString iden;
		if (PeekToken().Type() == TokenType::Iden)
			iden = EatToken(TokenType::Iden).Text();

		AstGenericDeclSPtr generics = nullptr;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		StdVector<AstStmtSPtr> members;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			members.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();

		return AstDeclSPtr{ new AstUnionDecl{ attribs, startIdx, std::move(iden), generics, std::move(members), endIdx } };
	}

	AstDeclSPtr Parser::ParseEnum(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Enum).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();
		
		AstGenericDeclSPtr generics;
		AstTypeSPtr baseType;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		if (TryEatToken(TokenType::Colon))
			baseType = ParseType();

		bool isAdt = !!generics;
		bool isValue = !!baseType;

		StdPairVector<StdString, AstExprSPtr> valueMembers;
		StdPairVector<StdString, AstTypeSPtr> adtMembers;
		EatToken(TokenType::LBrace);
		if (PeekToken().Type() != TokenType::RBrace)
		{
			do
			{
				StdString memberIden = EatToken(TokenType::Iden).Text();

				if (TryEatToken(TokenType::Eq))
				{
					AstExprSPtr expr = ParseExpression();
					isValue = true;
					if (isAdt)
					{
						Span span = m_pCtx->pCompContext->spanManager.GetSpan(expr->ctx->startIdx - 1);
						g_ErrorSystem.Error(span, "Cannot assign a value to a member of an ADT enum");
						expr = nullptr;
						isValue = false;
					}
					valueMembers.push_back(std::pair{ memberIden, expr });
				}
				else if (PeekToken().Type() == TokenType::LParen ||
					PeekToken().Type() == TokenType::LBrace)
				{
					AstTypeSPtr type = ParseType(true);
					isAdt = true;
					if (isValue)
					{
						Span span = m_pCtx->pCompContext->spanManager.GetSpan(type->ctx->startIdx - 1);
						g_ErrorSystem.Error(span, "Cannot add a type to a member of a value enum");
						isAdt = false;
					}
					adtMembers.push_back(std::pair{ memberIden, type });
				}
			}
			while (TryEatToken(TokenType::Comma) && PeekToken().Type() != TokenType::RBrace);
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();

		if (isAdt)
			return AstDeclSPtr{ new AstAdtEnumDecl{ attribs, startIdx, std::move(iden), generics,
				std::move(adtMembers), endIdx } };
		return AstDeclSPtr{ new AstValueEnumDecl{ attribs, startIdx, std::move(iden), baseType,
			std::move(valueMembers), endIdx } };
	}

	AstDeclSPtr Parser::ParseInterface(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Interface).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		if (PeekToken().Type() == TokenType::Semicolon)
		{
			u64 endIdx = EatToken().Idx();
			return AstDeclSPtr{ new AstMarkerInterfaceDecl{ attribs, startIdx, std::move(iden), endIdx } };
		}

		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, true));
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();

		return AstDeclSPtr{ new AstStrongInterfaceDecl{ attribs, startIdx, std::move(iden), generics, std::move(stmts), endIdx } };
	}

	AstDeclSPtr Parser::ParseWeakInterface(AstAttribsSPtr attribs)
	{
		u64 weakIdx = EatToken(TokenType::Weak).Idx();
		EatToken(TokenType::Interface);
		StdString iden = EatToken(TokenType::Iden).Text();

		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, true));
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();

		return AstDeclSPtr{ new AstWeakInterfaceDecl{ attribs, weakIdx, std::move(iden), std::move(stmts), endIdx } };
	}

	AstDeclSPtr Parser::ParseTypealias(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Typealias).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();
		
		AstTypeSPtr type;
		if (TryEatToken(TokenType::Eq))
		{
			type = ParseType();
		}
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();

		return AstDeclSPtr{ new AstTypeAliasDecl{ attribs, startIdx, std::move(iden), generics, type, endIdx } };
	}

	AstDeclSPtr Parser::ParseTypedef(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Typedef).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::Eq);
		AstTypeSPtr type = ParseType();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();

		return AstDeclSPtr{ new AstTypeDefDecl{ attribs, startIdx, std::move(iden), generics, type, endIdx } };
	}

	AstVarDeclSPtr Parser::ParseVarDecl(AstAttribsSPtr attribs)
	{
		StdVector<StdString> idens;
		Token& tok = EatToken(TokenType::Iden);
		u64 startIdx = tok.Idx();
		idens.push_back(tok.Text());

		while (TryEatToken(TokenType::Comma))
			idens.push_back(EatToken(TokenType::Iden).Text());

		return ParseVarDecl(attribs, startIdx, std::move(idens));
	}

	AstVarDeclSPtr Parser::ParseVarDecl(AstAttribsSPtr attribs, u64 startIdx, StdVector<StdString>&& idens)
	{
		AstTypeSPtr type;
		AstExprSPtr expr;
		if (TryEatToken(TokenType::Colon))
		{
			type = ParseType();
			if (TryEatToken(TokenType::Eq))
				expr = ParseCommaExpression();
		}
		else
		{
			EatToken(TokenType::ColonEq);
			expr = ParseCommaExpression();
		}

		u64 endIdx = 0;
		if (PeekToken().Type() == TokenType::Comma)
			endIdx = EatToken(TokenType::Comma).Idx();
		else if (PeekToken().Type() == TokenType::Semicolon)
			endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstVarDeclSPtr{ new AstVarDecl{ attribs, startIdx, std::move(idens), type, expr, endIdx } };
	}

	AstDeclSPtr Parser::ParseFuncDecl(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Func).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LParen);
		StdVector<AstParamSPtr> params;
		if (PeekToken().Type() != TokenType::RParen)
			params = ParseParams();
		EatToken(TokenType::RParen);

		AstTypeSPtr retType;
		StdPairVector<StdString, AstTypeSPtr> namedRets;
		if (TryEatToken(TokenType::Arrow))
		{
			if (PeekToken(2).Type() == TokenType::Colon)
			{
				EatToken(TokenType::LParen);
				do
				{
					StdString retIden = EatToken(TokenType::Iden).Text();
					EatToken(TokenType::Colon);
					AstTypeSPtr type = ParseType();
					namedRets.push_back(std::pair{ std::move(retIden), type });
				}
				while (TryEatToken(TokenType::Comma));
				
				EatToken(TokenType::RParen);
			}
			else
			{	
				retType = ParseType();
			}
		}

		AstGenericWhereClauseSPtr whereClause;
		if (PeekToken().Type() == TokenType::Where)
			whereClause = ParseGenericWhereClause();

		StdVector<AstStmtSPtr> stmts;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();
		
		return AstDeclSPtr{ new AstFuncDecl { attribs, startIdx, std::move(iden), generics, std::move(params), retType, std::move(namedRets), whereClause, std::move(stmts), endIdx } };
	}

	AstDeclSPtr Parser::ParseMethodDecl(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Func).Idx();

		AstMethodReceiverKind recKind = AstMethodReceiverKind::None;
		if (TryEatToken(TokenType::LParen))
		{
			if (TryEatToken(TokenType::And))
			{
				if (TryEatToken(TokenType::Const))
					recKind = AstMethodReceiverKind::ConstRef;
				else
					recKind = AstMethodReceiverKind::Ref;
			}
			else
			{
				recKind = AstMethodReceiverKind::Value;
			}
			EatToken(TokenType::IdenSelf);
			EatToken(TokenType::RParen);
		}
		StdString iden = EatToken(TokenType::Iden).Text();

		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LParen);
		StdVector<AstParamSPtr> params;
		if (PeekToken().Type() != TokenType::RParen)
			params = ParseParams();
		EatToken(TokenType::RParen);

		AstTypeSPtr retType;
		StdPairVector<StdString, AstTypeSPtr> namedRets;
		if (TryEatToken(TokenType::Arrow))
		{
			if (PeekToken(2).Type() == TokenType::Colon)
			{
				EatToken(TokenType::LParen);
				do
				{
					StdString retIden = EatToken(TokenType::Iden).Text();
					EatToken(TokenType::Colon);
					AstTypeSPtr type = ParseType();
					namedRets.push_back(std::pair{ std::move(retIden), type });
				} while (TryEatToken(TokenType::Comma));

				EatToken(TokenType::RParen);
			}
			else
			{
				retType = ParseType();
			}
		}

		AstGenericWhereClauseSPtr whereClause;
		if (PeekToken().Type() == TokenType::Where)
			whereClause = ParseGenericWhereClause();

		StdVector<AstStmtSPtr> stmts;
		u64 endIdx;
		if (TryEatToken(TokenType::LBrace))
		{
			while (PeekToken().Type() != TokenType::RBrace)
			{
				stmts.push_back(ParseStatement());
			}
			endIdx = EatToken().Idx();

			return AstDeclSPtr{ new AstMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params), retType, std::move(namedRets), whereClause, std::move(stmts), endIdx } };
		}
		else
		{
			if (whereClause)
			{
				Span span = m_pCtx->pCompContext->spanManager.GetSpan(whereClause->ctx->startIdx);
				g_ErrorSystem.Error(span, "An empty method declaration cannot have a where clause");
			}
			
			endIdx = EatToken(TokenType::Semicolon).Idx();
			return AstDeclSPtr{ new AstEmptyMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params), retType, endIdx } };
		}
	}

	AstDeclSPtr Parser::ParseImplDecl(AstAttribsSPtr attribs)
	{
		u64 implIdx = EatToken(TokenType::Impl).Idx();
		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();
		
		AstTypeSPtr type = ParseType();

		StdVector<AstIdentifierTypeSPtr> interfaces;
		if (TryEatToken(TokenType::Colon))
		{
			do
			{
				interfaces.push_back(ParseIdentifierType());
			}
			while (TryEatToken(TokenType::Plus));
		}
		
		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, true));
		}

		u64 endIdx = EatToken().Idx();

		return AstDeclSPtr{ new AstImplDecl{ attribs, implIdx, generics, type, std::move(interfaces), std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseImport(AstAttribsSPtr attribs)
	{
		u64 importIdx = EatToken(TokenType::Import).Idx();
		StdVector<StdString> modIdens = ParseIdenList(TokenType::Dot);

		StdPairVector<StdVector<StdString>, StdString> symbols;
		if (TryEatToken(TokenType::Colon))
		{
			do	
			{
				StdVector<StdString> symIdens = ParseIdenList(TokenType::ColonColon);
				StdString symAlias;
				if (TryEatToken(TokenType::As))
					symAlias = EatToken(TokenType::Iden).Text();
				symbols.push_back(std::pair{ std::move(symIdens), std::move(symAlias) });
			}
			while (TryEatToken(TokenType::Comma));
		}

		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstImportStmt{ attribs, importIdx, std::move(modIdens), std::move(symbols), endIdx } };
	}

	AstStmtSPtr Parser::ParseBlockStmt()
	{
		u64 startIdx = EatToken(TokenType::LBrace).Idx();
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstStmtSPtr{ new AstBlockStmt{ startIdx, std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseIfStmt()
	{
		u64 ifIdx = EatToken(TokenType::If).Idx();

		EatToken(TokenType::LParen);
		u64 predeclIdx = PeekToken().Idx();
		(void)ParseIdenList(TokenType::Comma);
		Token& tok = PeekToken();
		AstVarDeclSPtr varDecl;
		m_TokIdx = predeclIdx;
		if (tok.Type() == TokenType::Comma || tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
		{
			varDecl = ParseVarDecl(nullptr);
		}

		AstExprSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);

		AstStmtSPtr body = ParseStatement();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
			elseBody = ParseStatement();

		return AstStmtSPtr{ new AstIfStmt{ ifIdx, varDecl, cond, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseLoopStmt(AstLabelStmtSPtr label)
	{
		u64 loopIdx = EatToken(TokenType::Loop).Idx();
		AstStmtSPtr body = ParseStatement();
		return AstStmtSPtr{ new AstLoopStmt{ label, loopIdx, body } };
	}

	AstStmtSPtr Parser::ParseWhileStmt(AstLabelStmtSPtr label)
	{
		u64 whileIdx = EatToken(TokenType::While).Idx();
		EatToken(TokenType::LParen);
		AstExprSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		AstStmtSPtr body = ParseStatement();
		return AstStmtSPtr{ new AstWhileStmt{ label, whileIdx, cond, body } };
	}

	AstStmtSPtr Parser::ParseDoWhileStmt(AstLabelStmtSPtr label)
	{
		u64 doIdx = EatToken(TokenType::Do).Idx();
		AstStmtSPtr body = ParseStatement();
		EatToken(TokenType::While);
		EatToken(TokenType::LParen);
		AstExprSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstDoWhileStmt{ label, doIdx, body, cond, endIdx } };
	}

	AstStmtSPtr Parser::ParseForStmt(AstLabelStmtSPtr label)
	{
		u64 forTokIdx = EatToken(TokenType::For).Idx();
		EatToken(TokenType::LParen);

		StdVector<StdString> idens = ParseIdenList(TokenType::Comma);
		EatToken(TokenType::In);
		AstExprSPtr range = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		AstStmtSPtr body = ParseStatement();
		return AstStmtSPtr{ new AstForStmt{ label, forTokIdx, std::move(idens), range, body } };
	}

	AstStmtSPtr Parser::ParseSwitch(AstLabelStmtSPtr label)
	{
		u64 switchIdx = EatToken(TokenType::Switch).Idx();
		EatToken(TokenType::LParen);
		AstExprSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		EatToken(TokenType::LBrace);

		StdVector<AstSwitchCase> cases;
		do
		{
			AstExprSPtr staticExpr = ParseExpression();
			AstExprSPtr dynamicExpr;
			if (TryEatToken(TokenType::Where))
				dynamicExpr = ParseExpression();
			EatToken(TokenType::DblArrow);
			AstStmtSPtr body = ParseStatement();

			cases.push_back(AstSwitchCase{ staticExpr, dynamicExpr, body });
		}
		while (TryEatToken(TokenType::Comma));

		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstStmtSPtr{ new AstSwitchStmt{ label, switchIdx, cond, std::move(cases) , endIdx } };
	}

	AstLabelStmtSPtr Parser::ParseLabelStmt()
	{
		u64 startIdx = EatToken(TokenType::Colon).Idx();
		StdString label = EatToken(TokenType::Iden).Text();
		u64 endIdx = EatToken(TokenType::Colon).Idx();
		return AstLabelStmtSPtr{ new AstLabelStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseBreakStmt()
	{
		u64 startIdx = EatToken(TokenType::Break).Idx();
		StdString label;
		if (PeekToken().Type() == TokenType::Iden)
			label = EatToken().Text();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstBreakStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseContinueStmt()
	{
		u64 startIdx = EatToken(TokenType::Continue).Idx();
		StdString label;
		if (PeekToken().Type() == TokenType::Iden)
			label = EatToken().Text();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstContinueStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseFallthroughStmt()
	{
		u64 startIdx = EatToken(TokenType::Fallthrough).Idx();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstFallthroughStmt{ startIdx, endIdx } };
	}

	AstStmtSPtr Parser::ParseGotoStmt()
	{
		u64 startIdx = EatToken(TokenType::Goto).Idx();
		StdString label = EatToken(TokenType::Iden).Text();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstGotoStmt{ startIdx, std::move(label), endIdx } };
	}

	AstStmtSPtr Parser::ParseReturnStmt()
	{
		u64 startIdx = EatToken(TokenType::Return).Idx();
		AstExprSPtr expr;
		if (PeekToken().Type() != TokenType::Semicolon)
			expr = ParseCommaExpression();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstReturnStmt{ startIdx, expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseExprStmt()
	{
		AstExprSPtr expr = ParseExpression();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstExprStmt{ expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseDeferStmt()
	{
		u64 startIdx = EatToken(TokenType::Defer).Idx();
		AstExprSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstDeferStmt{ startIdx, expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseStackDeferStmt()
	{
		u64 startIdx = EatToken(TokenType::StackDefer).Idx();
		AstExprSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstStackDeferStmt{ startIdx, expr, endIdx } };
	}

	AstStmtSPtr Parser::ParseUnsafeStmt()
	{
		u64 startIdx = EatToken(TokenType::Unsafe).Idx();
		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstStmtSPtr{ new AstUnsafeStmt{ startIdx, std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseCompIfStmt()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatToken(TokenType::If);

		EatToken(TokenType::LParen);
		Token& tok = PeekToken();
		AstVarDeclSPtr varDecl;
		if (tok.Type() == TokenType::Comma || tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
		{
			varDecl = ParseVarDecl(nullptr);
		}

		AstExprSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);

		AstStmtSPtr body = ParseStatement();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
			elseBody = ParseStatement();

		return AstStmtSPtr{ new AstCompIfStmt{ startIdx, varDecl, cond, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseCompCondStmt()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("conditional");
		EatToken(TokenType::LParen);
		Token& cond = EatToken(TokenType::Iden);
		EatToken(TokenType::RParen);
		AstStmtSPtr body = ParseStatement();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
			elseBody = ParseStatement();
		return AstStmtSPtr{ new AstCompCondStmt{ startIdx, cond, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseCompDebugStmt()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("debug");
		EatToken(TokenType::LParen);
		Token& cond = EatToken(TokenType::Iden);
		EatToken(TokenType::RParen);
		AstStmtSPtr body = ParseStatement();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
			elseBody = ParseStatement();
		return AstStmtSPtr{ new AstCompDebugStmt{ startIdx, cond, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseMacroLoopStmt()
	{
		u64 startIdx = EatToken(TokenType::DollarBrace).Idx();
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstStmtSPtr{ new AstMacroLoopStmt{ startIdx, std::move(stmts), endIdx } };
	}

	AstExprSPtr Parser::ParseCommaExpression()
	{
		StdVector<AstExprSPtr> exprs;
		do
		{
			exprs.push_back(ParseExpression());
		}
		while (TryEatToken(TokenType::Comma));
		if (exprs.size() == 1)
			return exprs[0];
		return AstExprSPtr{ new AstCommaExpr{ std::move(exprs) } };
	}

	AstExprSPtr Parser::ParseExpression(AstExprSPtr prev, bool allowBlockExpr)
	{
		AstExprSPtr expr = prev;
		while (true)
		{
			Token& tok = PeekToken();
			switch (tok.Type())
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
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(expr->ctx->startIdx);
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
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
			case TokenType::DotDot:
			case TokenType::DotDotEq:
			case TokenType::QuestionQuestion:
			case TokenType::QuestionColon:
			case TokenType::In:
			case TokenType::NotIn:
			{
				if (!expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseBinaryExpr(expr);
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
			{
				if (!expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseAssignmentExpr(expr);
				break;
			}
			case TokenType::Question:
			{
				if (!expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseTernaryExpr(expr);
				break;
			}
			case TokenType::Iden:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(expr->ctx->startIdx);
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseQualNameExpr();
				break;
			}
			case TokenType::LBracket:
			case TokenType::QuestionBracket:
			{
				if (!expr)
				{
					if (tok.Type() == TokenType::LBracket)
					{
						expr = ParseArrayInitExpr();
					}
					else
					{
						Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
						const char* pTokName = GetTokenTypeName(tok.Type()).data();
						g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
						EatToken();
						break;
					}
				}
				expr = ParseIndexSlicExpr(expr);
				break;
			}
			case TokenType::LParen:
			{
				if (expr)
					expr = ParseFuncCallExpr(expr);
				else
					expr = ParseBracketExpr();
				break;
			}
			case TokenType::Dot:
			case TokenType::QuestionDot:
			{
				if (!expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				
				if (PeekToken(1).Type() == TokenType::I32Lit)
					expr = ParseTupleAccessExpr(expr);
				else if (PeekToken(2).Type() == TokenType::LParen)
					expr = ParseMethodcallExpr(expr);
				else
					expr = ParseMemberAccessExpr(expr);
				break;
			}
			case TokenType::True:
			case TokenType::False:
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
			case TokenType::Void:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseLiteralExpr();
				break;
			}
			case TokenType::LBrace:
			{
				if (expr)
				{
					if (expr->exprKind == AstExprKind::QualName)
					{
						AstQualNameExpr* pQualNameExpr = static_cast<AstQualNameExpr*>(expr.get());
						expr = ParseAggrInitExpr(pQualNameExpr);
					}
					else
					{
						Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
						const char* pTokName = GetTokenTypeName(tok.Type()).data();
						g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
						EatToken();
						break;
					}
				}
				else
				{
					expr = ParseBlockExpr();
				}
				break;
			}
			case TokenType::Cast:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseCastExpr();
				break;
			}
			case TokenType::Transmute:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseTransmuteExpr();
				break;
			}
			case TokenType::Move:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseMoveExpr();
				break;
			}
			case TokenType::Unsafe:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseUnsafeExpr();
				break;
			}
			case TokenType::Is:
			{
				expr = ParseIsExpr(expr);
				break;
			}
			case TokenType::Hash:
			{
				if (expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					EatToken();
					break;
				}
				
				Token& idenTok = PeekToken(1);
				if (idenTok.Text() == "run")
				{
					expr = ParseCompRunExpr();
					break;
				}

				Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
				const char* pTokName = idenTok.Text().c_str();
				g_ErrorSystem.Error(span, "Unknown compile-time Expression before '%s'", pTokName);
				EatToken();
				EatToken();
				break;
			}
			case TokenType::ColonColon:
			{
				expr = ParseQualNameExpr();
				break;
			}
			case TokenType::ExclaimParen:
			case TokenType::ExclaimBracket:
			case TokenType::ExclaimBrace:
			{
				m_TokIdx = expr->ctx->startIdx;
				expr = ParseMacroInst();
				break;
			}
			case TokenType::Dollar:
			{
				expr = ParseMacroVarExpr();
				break;
			}
			default:
				if (!expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = tok.Text().c_str();
					g_ErrorSystem.Error(span, "Unexpected token '%s'", pTokName);
					EatToken();
				}
				
				return expr;
			}
		}
	}

	AstExprSPtr Parser::ParseAssignmentExpr(AstExprSPtr lExpr)
	{
		TokenType op = EatToken().Type();
		AstExprSPtr rExpr = ParseExpression();
		return AstExprSPtr{ new AstAssignExpr{ lExpr, op, rExpr } };
	}

	AstExprSPtr Parser::ParseTernaryExpr(AstExprSPtr cond)
	{
		EatToken(TokenType::Question);
		AstExprSPtr trueExpr = ParseExpression();
		EatToken(TokenType::Colon);
		AstExprSPtr falseExpr = ParseExpression();
		return AstExprSPtr{ new AstTernaryExpr{ cond, trueExpr, falseExpr } };
	}

	AstExprSPtr Parser::ParseBinaryExpr(AstExprSPtr lExpr)
	{
		TokenType op = EatToken().Type();
		AstExprSPtr expr = ParseExpression();

		if (expr->exprKind == AstExprKind::Binary)
		{
			AstBinaryExpr* binExpr = static_cast<AstBinaryExpr*>(expr.get());
			OperatorPrecedence curPrec = GetPrecedence(op);
			OperatorPrecedence nextPrec = GetPrecedence(binExpr->op);

			if (nextPrec < curPrec)
			{
				AstExprSPtr left = binExpr->lExpr;
				AstExprSPtr cur{ new AstBinaryExpr{ lExpr, op, left } };
				binExpr->lExpr = cur;
				return expr;
			}
		}

		return AstExprSPtr{ new AstBinaryExpr{ lExpr, op, expr } };
	}

	AstExprSPtr Parser::ParsePostfixExpr(AstExprSPtr expr)
	{
		Token& tok = EatToken();
		return AstExprSPtr{ new AstPostfixExpr{ expr, tok } };
	}

	AstExprSPtr Parser::ParsePrefixExpr()
	{
		Token& tok = EatToken();
		AstExprSPtr expr = ParseExpression();
		return AstExprSPtr{ new AstPrefixExpr{ tok, expr } };
	}

	AstExprSPtr Parser::ParseQualNameExpr()
	{
		AstQualNameSPtr qualName = ParseQualName();
		return AstExprSPtr{ new AstQualNameExpr{ qualName } };
	}

	AstExprSPtr Parser::ParseIndexSlicExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = TryEatToken(TokenType::QuestionBracket);
		if (!nullcoalesce)
			EatToken(TokenType::LBracket);
		
		if (TryEatToken(TokenType::Colon))
		{
			AstExprSPtr end = ParseExpression();
			u64 endIdx = EatToken(TokenType::RBracket).Idx();
			return AstExprSPtr{ new AstSliceExpr{ expr, nullcoalesce, AstExprSPtr{}, end, endIdx } };
		}
		
		AstExprSPtr index = ParseExpression();
		if (TryEatToken(TokenType::Colon))
		{
			AstExprSPtr end;
			if (PeekToken().Type() != TokenType::RBracket)
				end = ParseExpression();
			u64 endIdx = EatToken(TokenType::RBracket).Idx();
			return AstExprSPtr{ new AstSliceExpr{ expr, nullcoalesce, index, end, endIdx } };
		}

		u64 endIdx = EatToken(TokenType::RBracket).Idx();
		return AstExprSPtr{ new AstIndexSliceExpr{ expr, nullcoalesce, index, endIdx } };
	}

	AstExprSPtr Parser::ParseFuncCallExpr(AstExprSPtr expr)
	{
		EatToken(TokenType::LParen);
		StdVector<AstArgSPtr> args = ParseArgs();
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstExprSPtr{ new AstFuncCallExpr{ expr, std::move(args), endIdx } };
	}

	AstExprSPtr Parser::ParseMemberAccessExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = TryEatToken(TokenType::QuestionDot);
		if (!nullcoalesce)
			EatToken(TokenType::Dot);
		
		Token& tok = EatToken(TokenType::Iden);
		StdString iden = tok.Text();
		return AstExprSPtr{ new AstMemberAccessExpr{ expr, nullcoalesce, std::move(iden), tok.Idx() } };
	}

	AstExprSPtr Parser::ParseMethodcallExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = TryEatToken(TokenType::QuestionDot);
		if (!nullcoalesce)
			EatToken(TokenType::Dot);
		
		StdString iden = EatToken(TokenType::Iden).Text();
		
		EatToken(TokenType::LParen);
		StdVector<AstArgSPtr> args = ParseArgs();
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		
		return AstExprSPtr{ new AstMethodCallExpr{ expr, nullcoalesce, std::move(iden), std::move(args), endIdx } };
	}

	AstExprSPtr Parser::ParseTupleAccessExpr(AstExprSPtr expr)
	{
		bool nullcoalesce = TryEatToken(TokenType::QuestionDot);
		if (!nullcoalesce)
			EatToken(TokenType::Dot);
		
		Token& tok = EatToken(TokenType::I32Lit);
		return AstExprSPtr{ new AstTupleAccessExpr{ expr, nullcoalesce, u16(tok.Signed()), tok.Idx() } };
	}

	AstExprSPtr Parser::ParseLiteralExpr()
	{
		Token& tok = EatToken();
		return AstExprSPtr{ new AstLiteralExpr{ tok } };
	}

	AstExprSPtr Parser::ParseAggrInitExpr(AstQualNameExpr* qualName)
	{
		AstTypeSPtr type = AstTypeSPtr{ new AstIdentifierType{ qualName->qualName } };
		EatToken(TokenType::LBrace);
		StdVector<AstArgSPtr> args = ParseArgs();
		EatToken(TokenType::RParen);
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstExprSPtr{ new AstAggrInitExpr{ qualName->ctx->startIdx, type, std::move(args), endIdx } };
	}

	AstExprSPtr Parser::ParseArrayInitExpr()
	{
		u64 startIdx = EatToken(TokenType::LBracket).Idx();
		StdVector<AstExprSPtr> exprs;
		do
		{
			exprs.push_back(ParseExpression());
		}
		while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RBracket).Idx();
		return AstExprSPtr{ new AstArrayInitExpr{ startIdx, std::move(exprs), endIdx } };
	}

	AstExprSPtr Parser::ParseCastExpr()
	{
		u64 startIdx = EatToken(TokenType::Cast).Idx();
		EatToken(TokenType::LParen);
		AstTypeSPtr type = ParseType();
		EatToken(TokenType::RParen);
		AstExprSPtr expr = ParseExpression();
		return AstExprSPtr{ new AstCastExpr{ startIdx, type, expr } };
	}

	AstExprSPtr Parser::ParseTransmuteExpr()
	{
		u64 startIdx = EatToken(TokenType::Transmute).Idx();
		EatToken(TokenType::LParen);
		AstTypeSPtr type = ParseType();
		EatToken(TokenType::RParen);
		AstExprSPtr expr = ParseExpression();
		return AstExprSPtr{ new AstTransmuteExpr{ startIdx, type, expr } };
	}

	AstExprSPtr Parser::ParseMoveExpr()
	{
		u64 startIdx = EatToken(TokenType::Move).Idx();
		AstExprSPtr expr = ParseExpression();
		return AstExprSPtr{ new AstMoveExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseBracketExpr()
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		AstExprSPtr expr = ParseExpression();

		if (TryEatToken(TokenType::Comma))
		{
			StdVector<AstExprSPtr> exprs;
			do
			{
				exprs.push_back(ParseExpression());
			}
			while (TryEatToken(TokenType::Comma));
			
			u64 endIdx = EatToken(TokenType::RParen).Idx();
			return AstExprSPtr{ new AstTupleInitExpr{ startIdx, std::move(exprs), endIdx } };
		}

		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstExprSPtr{ new AstBracketExpr{ startIdx, expr, endIdx } };
	}

	AstExprSPtr Parser::ParseBlockExpr()
	{
		u64 startIdx = EatToken(TokenType::LBrace).Idx();
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();
		return AstExprSPtr{ new AstBlockExpr{ startIdx, std::move(stmts), endIdx } };
	}

	AstExprSPtr Parser::ParseUnsafeExpr()
	{
		u64 startIdx = EatToken(TokenType::Unsafe).Idx();
		AstExprSPtr expr = ParseExpression();
		return AstExprSPtr{ new AstUnsafeExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseClosureExpr()
	{
		u64 startIdx = EatToken(TokenType::Or).Idx();

		StdVector<AstParamSPtr> params = ParseParams(true);
		EatToken(TokenType::Or);

		AstTypeSPtr retType;
		if (TryEatToken(TokenType::Arrow))
			retType = ParseType();

		AstExprSPtr expr = ParseExpression();

		return AstExprSPtr{ new AstClosureExpr{ startIdx, std::move(params), retType, expr } };
	}

	AstExprSPtr Parser::ParseIsExpr(AstExprSPtr expr)
	{
		u64 isIdx = EatToken(TokenType::Is).Idx();
		AstTypeSPtr type = ParseType();
		return AstExprSPtr{ new AstIsExpr{ expr, isIdx, type } };
	}

	AstExprSPtr Parser::ParseCompRunExpr()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("run");
		AstExprSPtr expr = ParseExpression();
		return AstExprSPtr{ new AstCompRunExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseMacroVarExpr()
	{
		u64 dollarIdx = EatToken(TokenType::Dollar).Idx();
		Token& tok = EatToken(TokenType::Iden);
		StdString iden = tok.Text();
		return AstExprSPtr{ new AstMacroVarExpr{ dollarIdx, std::move(iden), tok.Idx() } };
	}

	AstTypeSPtr Parser::ParseType(bool structKwOptional)
	{
		AstTypeSPtr type;
		Token& tok = PeekToken();
		switch (tok.Type())
		{
		case TokenType::Bool:
		case TokenType::Char:
		case TokenType::I8:
		case TokenType::I16:
		case TokenType::I32:
		case TokenType::I64:
		case TokenType::I128:
		case TokenType::U8:
		case TokenType::U16:
		case TokenType::U32:
		case TokenType::U64:
		case TokenType::U128:
		case TokenType::F16:
		case TokenType::F32:
		case TokenType::F64:
		case TokenType::F128:
		case TokenType::Void:
		{
			EatToken();
			return AstTypeSPtr{ new AstBuiltinType{ tok } };
		}
		case TokenType::Iden:
		{
			AstIdentifierTypeSPtr idenType = ParseIdentifierType();
			if (PeekToken().Type() == TokenType::Plus)
				return ParseCompoundInterfaceType(idenType);
			return idenType;
		}
		case TokenType::Asterisk:
			return ParsePointerType();
		case TokenType::And:
			return ParseReferenceType();
		case TokenType::LBracket:
			return ParseArraySliceType();
		case TokenType::LParen:
			return ParseTupleType();
		case TokenType::Question:
			return ParseOptionalType();
		case TokenType::Struct:
			return ParseInlineStructType();
		case TokenType::Enum:
			return ParseInlineEnumType();
		case TokenType::LBrace:
		{
			if (!structKwOptional)
			{
				Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
				g_ErrorSystem.Error(span, "Found '{' while parsing type\n");
				return nullptr;
			}
			return ParseInlineStructType(structKwOptional);
		}
		default:
			return nullptr;
		}
	}

	AstIdentifierTypeSPtr Parser::ParseIdentifierType()
	{;
		AstQualNameSPtr qualName = ParseQualName();
		return AstIdentifierTypeSPtr{ new AstIdentifierType{ qualName } };
	}

	AstTypeSPtr Parser::ParsePointerType()
	{
		u64 startIdx = EatToken(TokenType::Asterisk).Idx();
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstPointerType{ startIdx, type } };
	}

	AstTypeSPtr Parser::ParseReferenceType()
	{
		u64 startIdx = EatToken(TokenType::And).Idx();
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstPointerType{ startIdx, type } };
	}

	AstTypeSPtr Parser::ParseArraySliceType()
	{
		u64 startIdx = EatToken(TokenType::LBracket).Idx();

		if (TryEatToken(TokenType::RBracket))
		{
			AstTypeSPtr type = ParseType();
			return AstTypeSPtr{ new AstSliceType{ startIdx, type } };
		}

		AstExprSPtr expr = ParseExpression();
		EatToken(TokenType::RBracket);
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstArrayType{ startIdx, expr, type } };
	}

	AstTypeSPtr Parser::ParseTupleType()
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		StdVector<AstTypeSPtr> types;
		types.push_back(ParseType());
		while (TryEatToken(TokenType::Comma))
			types.push_back(ParseType());
		u64 endIdx = EatToken().Idx();
		return AstTypeSPtr{ new AstTupleType{ startIdx, std::move(types), endIdx } };
	}

	AstTypeSPtr Parser::ParseOptionalType()
	{
		u64 startIdx = EatToken(TokenType::Question).Idx();
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstOptionalType{ startIdx, type } };
	}

	AstTypeSPtr Parser::ParseInlineStructType(bool structKwOptional)
	{
		u64 startIdx;
		if (structKwOptional && PeekToken().Type() != TokenType::Struct)
		{
			startIdx = EatToken(TokenType::LBrace).Idx();
		}
		else
		{
			startIdx = EatToken(TokenType::Struct).Idx();
			EatToken(TokenType::LBrace);
		}

		StdPairVector<StdVector<StdString>, AstTypeSPtr> members;
		do
		{
			StdVector<StdString> idens;
			do
			{
				idens.push_back(EatToken(TokenType::Iden).Text());
			}
			while (TryEatToken(TokenType::Comma));
			EatToken(TokenType::Colon);
			AstTypeSPtr type = ParseType();
			members.push_back(std::pair{ std::move(idens), type });
		}
		while(TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstTypeSPtr{ new AstInlineStructType{ startIdx, std::move(members), endIdx } };
	}

	AstTypeSPtr Parser::ParseInlineEnumType()
	{
		u64 startIdx = EatToken(TokenType::Enum).Idx();
		EatToken(TokenType::LBrace);
		StdVector<StdString> members;
		do
		{
			members.push_back(EatToken(TokenType::Iden).Text());
		}
		while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstTypeSPtr{ new AstInlineEnumType{ startIdx, std::move(members), endIdx } };
	}

	AstTypeSPtr Parser::ParseCompoundInterfaceType(AstIdentifierTypeSPtr first)
	{
		StdVector<AstIdentifierTypeSPtr> interfaces;
		interfaces.push_back(first);
		while (TryEatToken(TokenType::Plus))
		{
			interfaces.push_back(ParseIdentifierType());
		}
		return AstTypeSPtr{ new AstCompoundInterfaceType{ std::move(interfaces) } };
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
			switch (PeekToken().Type())
			{
			case TokenType::CConst:
			case TokenType::Const:
			case TokenType::Immutable:
			case TokenType::Lazy:
			case TokenType::Static:
			{
				Token& tok = EatToken();
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
		u64 startIdx = EatToken(TokenType::AtColon).Idx();
		Token idenTok = EatToken(TokenType::Iden);
		StdString iden = idenTok.Text();
		u64 endIdx = idenTok.Idx();

		StdVector<AstArgSPtr> args;
		if (TryEatToken(TokenType::LParen))
		{
			args = ParseArgs();
			endIdx = EatToken(TokenType::RParen).Idx();
		}

		return AstCompAttribSPtr{ new AstCompAttrib{ startIdx, std::move(iden), std::move(args), endIdx } };
	}

	AstUserAttribSPtr Parser::ParseUserAttribute()
	{
		u64 startIdx = EatToken(TokenType::AtColon).Idx();
		Token idenTok = EatToken(TokenType::Iden);
		StdString iden = idenTok.Text();
		u64 endIdx = idenTok.Idx();

		StdVector<AstArgSPtr> args;
		if (TryEatToken(TokenType::LParen))
		{
			args = ParseArgs();
			endIdx = EatToken(TokenType::RParen).Idx();
		}

		return AstUserAttribSPtr{ new AstUserAttrib{ startIdx, std::move(iden), std::move(args), endIdx } };
	}

	AstVisibilityAttribSPtr Parser::ParseVisibilityAttribute()
	{
		u64 startIdx = EatToken(TokenType::AtColon).Idx();
		u64 endIdx = startIdx;

		StdString kind;
		if (TryEatToken(TokenType::LParen))
		{
			Token& tok = EatToken();

			if (tok.Text() != "module" &&
				tok.Text() != "package" &&
				tok.Text() != "dynlib")
			{
				Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
				const char* pFound = tok.Text().c_str();
				g_ErrorSystem.Error(span, "Found '%', expected 'module', 'package' or 'dynlib'", pFound);
			}
			else
			{
				kind = tok.Text();
			}
			
			endIdx = EatToken(TokenType::RParen).Idx();
		}

		return AstVisibilityAttribSPtr{ new AstVisibilityAttrib{ startIdx, std::move(kind), endIdx } };
	}

	AstGenericDeclSPtr Parser::ParseGenericDecl()
	{
		u64 startIdx = EatToken(TokenType::Less).Idx();
		StdVector<AstGenericParam> params;
		do
		{
			params.push_back(ParseGenericParam());
		}
		while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::Greater).Idx();
		return AstGenericDeclSPtr{ new AstGenericDecl{ startIdx, std::move(params), endIdx } };
	}

	AstGenericParam Parser::ParseGenericParam()
	{
		if (TryEatToken(TokenType::Colon))
		{
			if (TryEatToken(TokenType::LBrace))
			{
				AstExprSPtr expr = ParseExpression();
				EatToken(TokenType::RBrace);
				return AstGenericParam{ expr };
			}
			return AstGenericParam{ ParseType() };
		}

		Token& tok = EatToken(TokenType::Iden);
		u64 startIdx = tok.Idx();
		StdString iden = tok.Text();

		if (TryEatToken(TokenType::Colon))
		{
			AstTypeSPtr type = ParseType();
			AstExprSPtr def;
			if (TryEatToken(TokenType::Eq))
				def = ParseExpression(nullptr, true);

			return AstGenericParam{ AstGenericValueParamSPtr{ new AstGenericValueParam{ startIdx, std::move(iden), type, def } } };
		}

		StdVector<AstIdentifierTypeSPtr> interfaces;
		if (TryEatToken(TokenType::Is))
		{
			do
			{
				interfaces.push_back(ParseIdentifierType());
			}
			while (TryEatToken(TokenType::Plus));
		}

		AstTypeSPtr def;
		if (TryEatToken(TokenType::Eq))
			def = ParseType();
		return AstGenericParam{ AstGenericTypeParamSPtr{ new AstGenericTypeParam{ startIdx, std::move(iden), std::move(interfaces), def } } };
	}

	AstGenericTypeBoundSPtr Parser::ParseGenericTypeBound()
	{
		AstTypeSPtr type = ParseType();
		EatToken(TokenType::Is);
		AstTypeSPtr bound = ParseType();
		return AstGenericTypeBoundSPtr{ new AstGenericTypeBound{ type, bound } };
	}

	AstGenericWhereClauseSPtr Parser::ParseGenericWhereClause()
	{
		u64 startIdx = EatToken(TokenType::Where).Idx();
		StdVector<AstGenericTypeBoundSPtr> bounds;
		do
		{
			bounds.push_back(ParseGenericTypeBound());
		}
		while (TryEatToken(TokenType::Comma));
		
		return AstGenericWhereClauseSPtr{ new AstGenericWhereClause{ startIdx, std::move(bounds) } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroVar()
	{
		u64 dollarIdx = EatToken(TokenType::Dollar).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();
		EatToken(TokenType::Colon);
		Token& tok = EatToken(TokenType::Iden);

		AstMacroVarKind kind = AstMacroVarKind::Unknown;
		if (tok.Text() == "stmt")
			kind = AstMacroVarKind::Stmt;
		else if (tok.Text() == "expr")
			kind = AstMacroVarKind::Expr;
		else if (tok.Text() == "type")
			kind = AstMacroVarKind::Type;
		else if (tok.Text() == "qual")
			kind = AstMacroVarKind::Qual;
		else if (tok.Text() == "iden")
			kind = AstMacroVarKind::Iden;
		else if (tok.Text() == "attr")
			kind = AstMacroVarKind::Attr;
		else if (tok.Text() == "toks")
			kind = AstMacroVarKind::Toks;

		if (kind == AstMacroVarKind::Unknown)
		{
			Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
			const char* pKind = tok.Text().c_str();
			g_ErrorSystem.Error(span, "Unknown macro variable kind: '%s'", pKind);
		}
		
		return AstMacroPatternElemSPtr{ new AstMacroVar{ dollarIdx, std::move(iden), kind, tok.Idx() } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroSeparator()
	{
		StdVector<Token> toks;
		Token tok = PeekToken();
		do
		{
			toks.push_back(std::move(tok));
			EatToken();
			tok = PeekToken();
		}
		while (tok.Type() != TokenType::RParen && 
			   tok.Type() != TokenType::Dollar &&
			   tok.Type() != TokenType::DollarParen);

		return AstMacroPatternElemSPtr{ new AstMacroSeparator{ std::move(toks) } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroFragment()
	{
		u64 startIdx = EatToken(TokenType::DollarParen).Idx();
		AstMacroPatternSPtr pattern = ParseMacroPattern();
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		Token rep = PeekToken();
		if (rep.Type() == TokenType::Question ||
			rep.Type() == TokenType::Plus ||
			rep.Type() == TokenType::Asterisk)
		{
			 endIdx = EatToken().Idx();
		}
		else
		{
			rep = Token{ TokenType::Unknown, "", u64(-1) };
		}
		return AstMacroPatternElemSPtr{ new AstMacroFragment{ startIdx, pattern, rep.Type(), endIdx } };
	}

	AstMacroPatternSPtr Parser::ParseMacroPattern()
	{
		StdVector<AstMacroPatternElemSPtr> elems;
		while (PeekToken().Type() != TokenType::RParen)
		{
			if (PeekToken().Type() == TokenType::Dollar)
			{
				elems.push_back(ParseMacroVar());
			}
			else if (PeekToken().Type() == TokenType::DollarParen)
			{
				elems.push_back(ParseMacroFragment());
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

	AstMacroRuleSPtr Parser::ParseMacroRules()
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		AstMacroPatternSPtr pattern = ParseMacroPattern();
		EatToken(TokenType::RParen);
		EatToken(TokenType::DblArrow);
		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstMacroRuleSPtr{ new AstMacroRule{ startIdx, pattern, std::move(stmts), endIdx } };
	}

	AstDeclSPtr Parser::ParseDeclMacro()
	{
		u64 startIdx = EatToken(TokenType::Macro).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		if (TryEatToken(TokenType::LParen))
		{
			AstMacroPatternSPtr pattern = ParseMacroPattern();
			EatToken(TokenType::RParen);
			EatToken(TokenType::LBrace);
			StdVector<AstStmtSPtr> stmts;
			while (PeekToken().Type() != TokenType::RBrace)
			{
				stmts.push_back(ParseStatement());
			}
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstDeclSPtr{ new AstDeclMacro{ startIdx, std::move(iden), pattern, std::move(stmts), endIdx } };
		}

		EatToken(TokenType::LBrace);
		StdVector<AstMacroRuleSPtr> rules;
		rules.push_back(ParseMacroRules());
		while (TryEatToken(TokenType::Comma))
			rules.push_back(ParseMacroRules());

		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstDeclSPtr{ new AstRulesDeclMacro{ startIdx, std::move(iden), std::move(rules), endIdx } };
	}

	AstDeclSPtr Parser::ParseProcMacro()
	{
		u64 startIdx = EatToken(TokenType::Macro).Idx();
		EatToken(TokenType::Func);
		StdString iden = EatToken(TokenType::Iden).Text();
		EatToken(TokenType::LParen);
		StdString toksIden = EatToken(TokenType::Iden).Text();
		EatToken(TokenType::RParen);

		if (TryEatToken(TokenType::LParen))
		{
			AstMacroPatternSPtr pattern = ParseMacroPattern();
			EatToken(TokenType::RParen);
			EatToken(TokenType::LBrace);
			StdVector<AstStmtSPtr> stmts;
			while (PeekToken().Type() != TokenType::RBrace)
			{
				stmts.push_back(ParseStatement());
			}
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstDeclSPtr{ new AstProcMacro{ startIdx, std::move(iden), std::move(toksIden), pattern, std::move(stmts), endIdx } };
		}

		EatToken(TokenType::LBrace);
		StdVector<AstMacroRuleSPtr> rules;
		rules.push_back(ParseMacroRules());
		while (TryEatToken(TokenType::Comma))
			rules.push_back(ParseMacroRules());

		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstDeclSPtr{ new AstRulesProcMacro{ startIdx, std::move(iden), std::move(toksIden), std::move(rules), endIdx } };
	}

	AstExprSPtr Parser::ParseMacroInst()
	{
		AstQualNameSPtr qualName = ParseQualName();
		if (TryEatToken(TokenType::ExclaimBrace))
		{
			StdVector<Token> toks;
			while (PeekToken().Type() != TokenType::RBrace)
			{
				toks.push_back(EatToken());
			}
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstExprSPtr{ new AstMacroInst{ std::move(qualName), toks, endIdx } };
		}
		if (TryEatToken(TokenType::ExclaimBracket))
		{
			StdVector<Token> toks;
			while (PeekToken().Type() != TokenType::RBracket)
			{
				toks.push_back(EatToken());
			}
			u64 endIdx = EatToken(TokenType::RBracket).Idx();
			return AstExprSPtr{ new AstMacroInst{ std::move(qualName), toks, endIdx } };
		}
		
		EatToken(TokenType::ExclaimParen);
		StdVector<Token> toks;
		while (PeekToken().Type() != TokenType::RParen)
		{
			toks.push_back(EatToken());
		}
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstExprSPtr{ new AstMacroInst{ std::move(qualName), toks, endIdx } };
	}

	AstQualNameSPtr Parser::ParseQualName()
	{
		u64 startIdx = u64(-1);
		bool global = false;
		if (PeekToken().Type() == TokenType::ColonColon)
		{
			startIdx = EatToken().Idx();
			global = true;
		}
		
		StdVector<AstQualIdenSPtr> qualIdens;
		do
		{
			if (PeekToken().Type() == TokenType::Iden)
			{
				Token& tok = EatToken(TokenType::Iden);
				StdString iden = tok.Text();
				if (startIdx == u64(-1))
					startIdx = tok.Idx();

				StdVector<AstGenericArg> args;
				u64 endIdx = tok.Idx();
				if (TryEatToken(TokenType::ExclaimLess))
				{
					do
					{
						if (PeekToken().Type() == TokenType::LBrace)
						{
							EatToken(TokenType::LBrace);
							args.push_back(AstGenericArg{ ParseExpression() });
							EatToken(TokenType::RBrace);
						}
						else
						{
							args.push_back(AstGenericArg{ ParseType() });
						}
					} while (TryEatToken(TokenType::Comma));
					endIdx = EatToken(TokenType::Greater).Idx();
				}

				qualIdens.emplace_back(new AstIden{ tok.Idx(), std::move(iden), std::move(args), endIdx });
			}
			else
			{
				u64 tmpIdx = EatToken(TokenType::Less).Idx();
				if (startIdx == u64(-1))
					startIdx = tmpIdx;

				AstTypeSPtr type = ParseType();
				EatToken(TokenType::As);
				AstIdentifierTypeSPtr interface = ParseIdentifierType();
				u64 endIdx = EatToken(TokenType::Greater).Idx();
				qualIdens.emplace_back(new AstTypeDisambiguation{ tmpIdx, type, interface, endIdx });
			}
			
		}
		while (TryEatToken(TokenType::ColonColon));

		return AstQualNameSPtr{ new AstQualName{ startIdx, global, std::move(qualIdens) } };
	}

	StdVector<AstParamSPtr> Parser::ParseParams(bool allowNoType)
	{
		StdVector<AstParamSPtr> params;
		if (PeekToken().Type() == TokenType::LParen ||
			PeekToken().Type() == TokenType::Or)
			return params;
		
		do
		{
			params.push_back(ParseParam(allowNoType));
		}
		while (TryEatToken(TokenType::Comma));
		return params;
	}

	AstParamSPtr Parser::ParseParam(bool allowNoType)
	{
		u64 startIdx, endIdx;
		StdPairVector<AstAttribsSPtr, StdString> idens;

		AstAttribsSPtr attribs;
		if (PeekToken().Type() != TokenType::Iden)
		{
			attribs = ParseAttributes();
			startIdx = endIdx = attribs->ctx->startIdx;
			idens.push_back(std::pair{ attribs, EatToken(TokenType::Iden).Text() });
		}
		else
		{
			Token& tok = EatToken(TokenType::Iden);
			startIdx = endIdx = tok.Idx();
			idens.push_back(std::pair{ attribs, tok.Text() });
		}

		while (TryEatToken(TokenType::Comma))
		{
			attribs = nullptr;
			if (PeekToken().Type() != TokenType::Iden)
				attribs = ParseAttributes();
			Token& tok = EatToken(TokenType::Iden);
			endIdx = tok.Idx();
			idens.push_back(std::pair{ attribs, tok.Text() });
		}

		if (PeekToken().Type() != TokenType::Colon && allowNoType)
			return AstParamSPtr{ new AstParam{ startIdx, std::move(idens), nullptr, false, endIdx } };
		
		EatToken(TokenType::Colon);
		AstTypeSPtr type;
		bool isVariadic = false;
		if (PeekToken().Type() == TokenType::DotDotDot)
		{
			endIdx = EatToken().Idx();
			isVariadic = true;
		}
		else if (PeekToken().Type() != TokenType::Or ||
				 allowNoType && PeekToken().Type() != TokenType::Comma)
		{
			type = ParseType();
			endIdx = type->ctx->endIdx;
			
			if (PeekToken().Type() == TokenType::DotDotDot)
			{
				endIdx = EatToken().Idx();
				isVariadic = true;
			}
		}

		return AstParamSPtr{ new AstParam{ startIdx, std::move(idens), type, isVariadic, endIdx } };
	}

	StdVector<AstArgSPtr> Parser::ParseArgs()
	{
		StdVector<AstArgSPtr> args;
		if (PeekToken().Type() == TokenType::RParen)
			return args;
		
		do
		{
			args.push_back(ParseArg());
		}
		while (TryEatToken(TokenType::Comma));
		return args;
	}

	AstArgSPtr Parser::ParseArg()
	{
		u64 startIdx;
		StdString iden;
		AstExprSPtr expr;
		if (PeekToken().Type() == TokenType::Iden && PeekToken(1).Type() == TokenType::Colon)
		{
			Token& tok = EatToken();
			startIdx = tok.Idx();
			iden = tok.Text();
			expr = ParseExpression();
		}
		else
		{
			expr = ParseExpression();
			startIdx = expr->ctx->startIdx;
		}

		return AstArgSPtr{ new AstArg{ startIdx, std::move(iden), expr } };
	}

	StdVector<StdString> Parser::ParseIdenList(TokenType separator)
	{
		u64 start, end;
		return ParseIdenList(separator, start, end);
	}

	StdVector<StdString> Parser::ParseIdenList(TokenType separator, u64& startIdx, u64& endIdx)
	{
		Token& startTok = PeekToken();
		startIdx = endIdx = startTok.Idx();

		StdVector<StdString> idens;
		if (startTok.Type() != TokenType::Iden)
			return idens;
		
		idens.push_back(startTok.Text());
		EatToken();
		
		while (TryEatToken(separator))
		{
			Token& tok = EatToken(TokenType::Iden);
			endIdx = tok.Idx();
			idens.push_back(tok.Text());
		}

		return idens;
	}

	Parser::OperatorPrecedence Parser::GetPrecedence(TokenType op)
	{
		switch (op)
		{
		case TokenType::Asterisk:
		case TokenType::Slash:
		case TokenType::Percent:
		case TokenType::Tilde:
			return OperatorPrecedence::MulDivRemCon;
		case TokenType::Plus:
		case TokenType::Minus:
			return OperatorPrecedence::AddMin;
		case TokenType::LessLess:
		case TokenType::LessLessLess:
		case TokenType::LessLessAsterisk:
		case TokenType::GreaterGreater:
		case TokenType::GreaterGreaterGreater:
		case TokenType::GreaterGreaterAsterisk:
			return OperatorPrecedence::ShiftRot;
		case TokenType::And:
			return OperatorPrecedence::BinAnd;
		case TokenType::Caret:
			return OperatorPrecedence::BinXor;
		case TokenType::Or:
			return OperatorPrecedence::BinOr;
		case TokenType::DotDot:
		case TokenType::DotDotEq:
			return OperatorPrecedence::Range;
		case TokenType::In:
			return OperatorPrecedence::Contains;
		case TokenType::EqEq:
		case TokenType::ExclaimEq:
		case TokenType::Less:
		case TokenType::LessEq:
		case TokenType::Greater:
		case TokenType::GreaterEq:
			return OperatorPrecedence::Cmp;
		case TokenType::QuestionQuestion:
		case TokenType::QuestionDot:
			return OperatorPrecedence::NullElvis;
		case TokenType::AndAnd:
			return OperatorPrecedence::LogAnd;
		case TokenType::OrOr:
			return OperatorPrecedence::LogOr;
		default:
			return OperatorPrecedence::None;
		}
	}

	Token& Parser::EatToken()
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };
		if (m_TokIdx >= m_Tokens.size())
			return emptyTok;
		return m_Tokens[m_TokIdx++];
	}

	Token& Parser::EatToken(TokenType type)
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };

		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != type)
		{
			Span span = m_pCtx->pCompContext->spanManager.GetSpan(m_TokIdx);
			const char* pFoundName = GetTokenTypeName(tok.Type()).data();
			const char* pExpectedName = GetTokenTypeName(type).data();
			g_ErrorSystem.Error(span, "Found '%s', expected '%s'", pFoundName, pExpectedName);
			++m_TokIdx;
			return emptyTok;
		}

		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return emptyTok;
		}

		++m_TokIdx;
		return tok;
	}

	Token& Parser::EatIdenToken(StdStringView text)
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };
		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return emptyTok;
		}

		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != TokenType::Iden ||
			tok.Text() != text)
		{
			Span span = m_pCtx->pCompContext->spanManager.GetSpan(m_TokIdx);
			const char* pFoundName = GetTokenTypeName(tok.Type()).data();
			const char* pExpectedName = text.data();
			g_ErrorSystem.Error(span, "Found '%s', expected '%s'", pFoundName, pExpectedName);
			++m_TokIdx;
			return emptyTok;
		}

		++m_TokIdx;
		return tok;
	}

	bool Parser::TryEatToken(TokenType type)
	{
		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != type)
			return false;

		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return false;
		}

		++m_TokIdx;
		return true;
	}

	Token& Parser::PeekToken()
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };

		if (m_TokIdx >= m_Tokens.size())
			return emptyTok;
		return m_Tokens[m_TokIdx];
	}

	Token& Parser::PeekToken(u64 offset)
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };

		if (m_TokIdx + offset >= m_Tokens.size())
			return emptyTok;
		return m_Tokens[m_TokIdx + offset];
	}
}
