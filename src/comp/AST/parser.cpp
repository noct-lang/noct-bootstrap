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

	StdVector<AstNodeSPtr> Parser::Parse()
	{
		StdVector<AstNodeSPtr> nodes;

		while (m_TokIdx < m_Tokens.size())
		{
			//nodes.push_back()
		}

		return nodes;
	}

	AstNodeSPtr Parser::ParseModuleDecl()
	{
		u64 startTokIdx = EatToken(TokenType::Module).Idx();
		StdVector<StdString> idens;
		idens.push_back(EatToken(TokenType::Iden).Text());

		while (PeekToken().Type() == TokenType::Dot)
		{
			EatToken();
			idens.push_back(EatToken(TokenType::Iden).Text());
		}

		u64 endTokIdx = EatToken(TokenType::Semicolon).Idx();

		return AstNodeSPtr{ new AstModuleDecl{ startTokIdx, std::move(idens), endTokIdx } };
	}

	AstNodeSPtr Parser::ParseStatement(AstNodeSPtr attribs, bool funcAreMethods)
	{
		if (!attribs)
			attribs = ParseAttributes();

		AstNodeSPtr label;
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
		case TokenType::Hash:
		{
			Token& tok = PeekToken(1);
			if (tok.Type() == TokenType::If)
				return ParseCompIfStmt();
			if (tok.Text() == "conditional")
				return ParseCompCondStmt();
			if (tok.Text() == "debug")
				return ParseCompDebugStmt();
			return ParseExpression();
		}
		case TokenType::Iden:
		{
			u64 startIdx, endIdx;
			StdVector<StdString> idens = ParseIdenList(TokenType::Comma, startIdx, endIdx);

			Token& tok = PeekToken();
			if (tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
				return ParseVarDecl(attribs, startIdx, std::move(idens));

			m_TokIdx = startIdx;
			return ParseExpression();
		}
		default:
		{
			if (label)
				return label;
			return ParseExpression();
		}
		}
	}

	AstNodeSPtr Parser::ParseStruct(AstNodeSPtr attribs)
	{
		u64 structTokIdx = EatToken(TokenType::Struct).Idx();

		StdString iden;
		if (PeekToken().Type() == TokenType::Iden)
			iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics = nullptr;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		StdVector<AstNodeSPtr> members;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			members.push_back(ParseStatement());
		}
		u64 endTokIdx = EatToken().Idx();

		return AstNodeSPtr{ new AstStructDecl{ attribs, structTokIdx, std::move(iden), generics, std::move(members), endTokIdx } };
	}

	AstNodeSPtr Parser::ParseUnion(AstNodeSPtr attribs)
	{
		u64 unionTokIdx = EatToken(TokenType::Struct).Idx();

		StdString iden;
		if (PeekToken().Type() == TokenType::Iden)
			iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics = nullptr;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		StdVector<AstNodeSPtr> members;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			members.push_back(ParseStatement());
		}
		u64 endTokIdx = EatToken().Idx();

		return AstNodeSPtr{ new AstUnionDecl{ attribs, unionTokIdx, std::move(iden), generics, std::move(members), endTokIdx } };
	}

	AstNodeSPtr Parser::ParseEnum(AstNodeSPtr attribs)
	{
		u64 enumTokIdx = EatToken(TokenType::Enum).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();
		
		AstNodeSPtr generics, baseType;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		if (PeekToken().Type() == TokenType::Colon)
			baseType = ParseType();

		bool isAdt = !!generics;
		bool isValue = !!baseType;

		StdVector<std::pair<StdString, AstNodeSPtr>> members;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			StdString iden = EatToken(TokenType::Iden).Text();
			AstNodeSPtr second = nullptr;
			
			if (PeekToken().Type() == TokenType::Eq)
			{
				EatToken();
				second = ParseExpression();

				isValue = true;
				if (isAdt)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(second->startTokIdx - 1);
					g_ErrorSystem.Error(span, "Cannot assign a value to a member of an ADT enum");
					second = nullptr;
					isValue = false;
				}
			}
			else if (PeekToken().Type() == TokenType::LParen)
			{
				second = ParseType();

				isAdt = true;
				if (isValue)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(second->startTokIdx - 1);
					g_ErrorSystem.Error(span, "Cannot add a type to a member of a value enum");
					second = nullptr;
					isAdt = false;
				}
			}
			else if (PeekToken().Type() == TokenType::LBrace)
			{
				StdVector<std::pair<StdVector<StdString>, AstNodeSPtr>> structMembers;
				u64 memberStartIdx = EatToken().Idx();
				do
				{
					if (structMembers.size() > 0 && PeekToken().Type() == TokenType::Comma)
						EatToken();
					
					StdVector<StdString> idens;
					idens.push_back(EatToken(TokenType::Iden).Text());

					while (PeekToken().Type() == TokenType::Comma)
					{
						EatToken();
						idens.push_back(EatToken(TokenType::Iden).Text());
					}
					
					EatToken(TokenType::Colon);
					AstNodeSPtr type = ParseType();
					members.push_back(std::pair{ std::move(iden), type });
				}
				while (PeekToken().Type() != TokenType::RBrace);
				u64 memberEndIdx = EatToken(TokenType::RBrace).Idx();

				second = AstNodeSPtr{ new AstAdtEnumStructMember{ memberStartIdx, std::move(structMembers), memberEndIdx } };
				
				isAdt = true;
				if (isValue)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(second->startTokIdx - 1);
					g_ErrorSystem.Error(span, "Cannot add a type to a member of a value enum");
					second = nullptr;
					isAdt = false;
				}
			}

			if (PeekToken().Type() == TokenType::Comma)
				EatToken();
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();

		if (isAdt)
			return AstNodeSPtr{ new AstAdtEnumDecl{ attribs, enumTokIdx, std::move(iden), generics, std::move(members), endIdx } };
		return AstNodeSPtr{ new AstValueEnumDecl{ attribs, enumTokIdx, std::move(iden), std::move(members), endIdx } };
	}

	AstNodeSPtr Parser::ParseInterface(AstNodeSPtr attribs)
	{
		u64 interfaceTokIdx = EatToken(TokenType::Interface).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		if (PeekToken().Type() == TokenType::Semicolon)
		{
			u64 endIdx = EatToken().Idx();
			return AstNodeSPtr{ new AstMarkerInterfaceDecl{ attribs, interfaceTokIdx, std::move(iden), endIdx } };
		}

		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, true));
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();

		return AstNodeSPtr{ new AstStrongInterfaceDecl{ attribs, interfaceTokIdx, std::move(iden), generics, std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseWeakInterface(AstNodeSPtr attribs)
	{
		u64 weakIdx = EatToken(TokenType::Weak).Idx();
		EatToken(TokenType::Interface);
		StdString iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, true));
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();

		return AstNodeSPtr{ new AstWeakInterfaceDecl{ attribs, weakIdx, std::move(iden), std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseTypealias(AstNodeSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Typedef).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();
		
		AstNodeSPtr type;
		if (PeekToken().Type() == TokenType::Eq)
		{
			EatToken();
			type = ParseType();
		}
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();

		return AstNodeSPtr{ new AstTypeAliasDecl{ attribs, startIdx, std::move(iden), generics, type, endIdx } };
	}

	AstNodeSPtr Parser::ParseTypedef(AstNodeSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Typedef).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::Eq);
		AstNodeSPtr type = ParseType();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();

		return AstNodeSPtr{ new AstTypeDefDecl{ attribs, startIdx, std::move(iden), generics, type, endIdx } };
	}

	AstNodeSPtr Parser::ParseVarDecl(AstNodeSPtr attribs)
	{
		StdVector<StdString> idens;
		Token& tok = EatToken(TokenType::Iden);
		u64 startIdx = tok.Idx();
		idens.push_back(tok.Text());

		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			idens.push_back(EatToken(TokenType::Iden).Text());
		}

		AstNodeSPtr type, expr;
		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			type = ParseType();

			if (PeekToken().Type() == TokenType::Eq)
			{
				EatToken();
				expr = ParseCommaExpression();
			}
		}
		else
		{
			EatToken(TokenType::ColonEq);
			expr = ParseExpression();
		}

		u64 endIdx;
		if (PeekToken().Type() == TokenType::Comma)
			endIdx = EatToken(TokenType::Comma).Idx();
		else
			endIdx = EatToken(TokenType::Semicolon).Idx();
		
		return AstNodeSPtr{ new AstVarDecl{ attribs, startIdx, std::move(idens), type, expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseVarDecl(AstNodeSPtr attribs, u64 startIdx, StdVector<StdString>&& idens)
	{
		AstNodeSPtr type, expr;
		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			type = ParseType();

			if (PeekToken().Type() == TokenType::Eq)
			{
				EatToken();
				expr = ParseCommaExpression();
			}
		}
		else
		{
			EatToken(TokenType::ColonEq);
			expr = ParseExpression();
		}

		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstVarDecl{ attribs, startIdx, std::move(idens), type, expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseFuncDecl(AstNodeSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Func).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LParen);
		StdVector<AstNodeSPtr> params = ParseParams();
		EatToken(TokenType::RParen);

		AstNodeSPtr retType;
		if (PeekToken().Type() == TokenType::Arrow)
		{
			EatToken();
			retType = ParseType();
		}

		AstNodeSPtr whereClause;
		if (PeekToken().Text() == "where")
			whereClause = ParseGenericWhereClause();

		StdVector<AstNodeSPtr> stmts;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();
		
		return AstNodeSPtr{ new AstFuncDecl { attribs, startIdx, std::move(iden), generics, std::move(params), retType, whereClause, std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseMethodDecl(AstNodeSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Func).Idx();

		AstMethodReceiverKind recKind = AstMethodReceiverKind::None;
		if (PeekToken().Type() == TokenType::LParen)
		{
			EatToken();
			if (PeekToken().Type() == TokenType::And)
			{
				EatToken();
				if (PeekToken().Type() == TokenType::Const)
				{
					EatToken();
					recKind = AstMethodReceiverKind::ConstRef;
				}
				else
				{
					recKind = AstMethodReceiverKind::Ref;
				}
			}
			else
			{
				recKind = AstMethodReceiverKind::Value;
			}
			EatToken(TokenType::IdenSelf);
			EatToken(TokenType::RParen);
		}
		StdString iden = EatToken(TokenType::Iden).Text();

		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LParen);
		StdVector<AstNodeSPtr> params = ParseParams();
		EatToken(TokenType::RParen);

		AstNodeSPtr retType;
		if (PeekToken().Type() == TokenType::Arrow)
		{
			EatToken();
			retType = ParseType();
		}

		AstNodeSPtr whereClause;
		if (PeekToken().Text() == "where")
			whereClause = ParseGenericWhereClause();

		StdVector<AstNodeSPtr> stmts;
		u64 endIdx;
		if (PeekToken().Type() == TokenType::LBrace)
		{
			EatToken(TokenType::LBrace);
			while (PeekToken().Type() != TokenType::RBrace)
			{
				stmts.push_back(ParseStatement());
			}
			endIdx = EatToken().Idx();

			return AstNodeSPtr{ new AstMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params), retType, whereClause, std::move(stmts), endIdx } };
		}
		else
		{
			if (whereClause)
			{
				Span span = m_pCtx->pCompContext->spanManager.GetSpan(whereClause->startTokIdx);
				g_ErrorSystem.Error(span, "An empty method declaration cannot have a where clause");
			}
			
			endIdx = EatToken(TokenType::Semicolon).Idx();
			return AstNodeSPtr{ new AstEmptyMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params), retType, whereClause, std::move(stmts), endIdx } };
		}
	}

	AstNodeSPtr Parser::ParseImplDecl(AstNodeSPtr attribs)
	{
		u64 implIdx = EatToken(TokenType::Impl).Idx();
		AstNodeSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();
		
		AstNodeSPtr type = ParseType();

		StdVector<AstNodeSPtr> interfaces;
		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			interfaces.push_back(ParseType());
			while (PeekToken().Type() == TokenType::Plus)
			{
				EatToken();
				interfaces.push_back(ParseType());
			}
		}
		
		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement(nullptr, true));
		}

		u64 endIdx = EatToken().Idx();

		return AstNodeSPtr{ new AstImplDecl{ attribs, implIdx, generics, type, std::move(interfaces), std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseImport(AstNodeSPtr attribs)
	{
		u64 importIdx = EatToken(TokenType::Import).Idx();
		StdVector<StdString> modIdens = ParseIdenList(TokenType::Dot);

		StdVector<std::pair<StdVector<StdString>, StdString>> symbols;
		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			do	
			{
				if (symbols.size() > 0 && PeekToken().Type() == TokenType::Comma)
					EatToken();
				
				StdVector<StdString> symIdens = ParseIdenList(TokenType::Comma);
				StdString symAlias;
				if (PeekToken().Type() == TokenType::As)
				{
					EatToken();
					symAlias = EatToken(TokenType::Iden).Text();
				}
				symbols.push_back(std::pair{ std::move(symIdens), std::move(symAlias) });
			}
			while (PeekToken().Type() != TokenType::Semicolon);
		}

		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstImportStmt{ attribs, importIdx, std::move(modIdens), std::move(symbols), endIdx } };
	}

	AstNodeSPtr Parser::ParseIfStmt()
	{
		u64 ifIdx = EatToken(TokenType::If).Idx();

		EatToken(TokenType::LParen);
		Token& tok = PeekToken();
		AstNodeSPtr varDecl;
		if (tok.Type() == TokenType::Comma || tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
		{
			varDecl = ParseVarDecl(nullptr);
		}

		AstNodeSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);

		AstNodeSPtr body = ParseStatement();
		AstNodeSPtr elseBody;
		if (PeekToken().Type() == TokenType::Else)
		{
			EatToken();
			elseBody = ParseStatement();
		}

		return AstNodeSPtr{ new AstIfStmt{ ifIdx, varDecl, cond, body, elseBody } };
	}

	AstNodeSPtr Parser::ParseLoopStmt(AstNodeSPtr label)
	{
		u64 loopIdx = EatToken(TokenType::Loop).Idx();
		AstNodeSPtr body = ParseStatement();
		return AstNodeSPtr{ new AstLoopStmt{ label, loopIdx, body } };
	}

	AstNodeSPtr Parser::ParseWhileStmt(AstNodeSPtr label)
	{
		u64 whileIdx = EatToken(TokenType::While).Idx();
		EatToken(TokenType::LParen);
		AstNodeSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		AstNodeSPtr body = ParseStatement();
		return AstNodeSPtr{ new AstWhileStmt{ label, whileIdx, cond, body } };
	}

	AstNodeSPtr Parser::ParseDoWhileStmt(AstNodeSPtr label)
	{
		u64 doIdx = EatToken(TokenType::Do).Idx();
		AstNodeSPtr body = ParseStatement();
		EatToken(TokenType::While);
		EatToken(TokenType::LParen);
		AstNodeSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstDoWhileStmt{ label, doIdx, body, cond, endIdx } };
	}

	AstNodeSPtr Parser::ParseForStmt(AstNodeSPtr label)
	{
		u64 forTokIdx = EatToken(TokenType::For).Idx();
		EatToken(TokenType::LParen);

		AstNodeSPtr init;
		if (PeekToken().Type() == TokenType::Iden)
		{
			StdVector<StdString> idens = ParseIdenList(TokenType::Comma);
			if (PeekToken().Type() == TokenType::In)
			{
				AstNodeSPtr range = ParseExpression(nullptr, true);
				EatToken(TokenType::RParen);
				AstNodeSPtr body = ParseStatement();
				return AstNodeSPtr{ new AstForRangeStmt{ label, forTokIdx, std::move(idens), range, body } };
			}
			init = ParseVarDecl(nullptr, forTokIdx + 2, std::move(idens));
		}
		else
		{
			EatToken(TokenType::Semicolon);
		}

		AstNodeSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::Semicolon);
		AstNodeSPtr inc;
		if (PeekToken().Type() != TokenType::RParen)
			inc = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		AstNodeSPtr body = ParseStatement();

		return AstNodeSPtr{ new AstForStmt{ label, forTokIdx, inc, cond, inc, body } };
	}

	AstNodeSPtr Parser::ParseSwitch(AstNodeSPtr label)
	{
		u64 switchIdx = EatToken(TokenType::Switch).Idx();
		EatToken(TokenType::LParen);
		AstNodeSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);
		EatToken(TokenType::RBrace);

		StdVector<AstSwitchCase> cases;
		while (PeekToken().Type() != TokenType::RParen)
		{
			AstNodeSPtr staticExpr = ParseExpression();
			AstNodeSPtr dynamicExpr;
			if (PeekToken().Type() == TokenType::If)
			{
				EatToken();
				dynamicExpr = ParseExpression();
			}
			EatToken(TokenType::DblArrow);
			AstNodeSPtr body = ParseStatement();

			cases.push_back(AstSwitchCase{ staticExpr, dynamicExpr, body });
		}

		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstNodeSPtr{ new AstSwitchStmt{ label, switchIdx, std::move(cases) , endIdx } };
	}

	AstNodeSPtr Parser::ParseLabelStmt()
	{
		u64 startIdx = EatToken(TokenType::Colon).Idx();
		StdString label = EatToken(TokenType::Iden).Text();
		u64 endIdx = EatToken(TokenType::Colon).Idx();
		return AstNodeSPtr{ new AstLabelStmt{ startIdx, std::move(label), endIdx } };
	}

	AstNodeSPtr Parser::ParseBreakStmt()
	{
		u64 startIdx = EatToken(TokenType::Break).Idx();
		StdString label;
		if (PeekToken().Type() == TokenType::Iden)
			label = EatToken().Text();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstBreakStmt{ startIdx, std::move(label), endIdx } };
	}

	AstNodeSPtr Parser::ParseContinueStmt()
	{
		u64 startIdx = EatToken(TokenType::Continue).Idx();
		StdString label;
		if (PeekToken().Type() == TokenType::Iden)
			label = EatToken().Text();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstBreakStmt{ startIdx, std::move(label), endIdx } };
	}

	AstNodeSPtr Parser::ParseFallthroughStmt()
	{
		u64 startIdx = EatToken(TokenType::Continue).Idx();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstFallthroughStmt{ startIdx, endIdx } };
	}

	AstNodeSPtr Parser::ParseGotoStmt()
	{
		u64 startIdx = EatToken(TokenType::Break).Idx();
		StdString label = EatToken(TokenType::Iden).Text();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstBreakStmt{ startIdx, std::move(label), endIdx } };
	}

	AstNodeSPtr Parser::ParseReturnStmt()
	{
		u64 startIdx = EatToken(TokenType::Return).Idx();
		AstNodeSPtr expr;
		if (PeekToken().Type() != TokenType::Semicolon)
			expr = ParseCommaExpression();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstReturnStmt{ startIdx, expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseExprStmt()
	{
		AstNodeSPtr expr = ParseExpression();
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstExprStmt{ expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseDeferStmt()
	{
		u64 startIdx = EatToken(TokenType::Defer).Idx();
		AstNodeSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstDeferStmt{ startIdx, expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseStackDeferStmt()
	{
		u64 startIdx = EatToken(TokenType::Defer).Idx();
		AstNodeSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstNodeSPtr{ new AstDeferStmt{ startIdx, expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseUnsafeStmt()
	{
		u64 startIdx = EatToken(TokenType::Unsafe).Idx();
		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstNodeSPtr{ new AstUnsafeStmt{ startIdx, std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseCompIfStmt()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatToken(TokenType::If);

		EatToken(TokenType::LParen);
		Token& tok = PeekToken();
		AstNodeSPtr varDecl;
		if (tok.Type() == TokenType::Comma || tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
		{
			varDecl = ParseVarDecl(nullptr);
		}

		AstNodeSPtr cond = ParseExpression(nullptr, true);
		EatToken(TokenType::RParen);

		AstNodeSPtr body = ParseStatement();
		AstNodeSPtr elseBody;
		if (PeekToken().Type() == TokenType::Else)
		{
			EatToken();
			elseBody = ParseStatement();
		}

		return AstNodeSPtr{ new AstCompIfStmt{ startIdx, varDecl, cond, body, elseBody } };
	}

	AstNodeSPtr Parser::ParseCompCondStmt()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("conditional");
		EatToken(TokenType::LParen);
		Token& cond = EatToken(TokenType::Iden);
		EatToken(TokenType::RParen);
		AstNodeSPtr body = ParseStatement();
		AstNodeSPtr elseBody;
		if (PeekToken().Type() == TokenType::Else)
		{
			EatToken();
			elseBody = ParseStatement();
		}
		return AstNodeSPtr{ new AstCompCondStmt{ startIdx, cond, body, elseBody } };
	}

	AstNodeSPtr Parser::ParseCompDebugStmt()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("conditional");
		EatToken(TokenType::LParen);
		Token& cond = EatToken(TokenType::Iden);
		EatToken(TokenType::RParen);
		AstNodeSPtr body = ParseStatement();
		AstNodeSPtr elseBody;
		if (PeekToken().Type() == TokenType::Else)
		{
			EatToken();
			elseBody = ParseStatement();
		}
		return AstNodeSPtr{ new AstCompDebugStmt{ startIdx, cond, body, elseBody } };
	}

	AstNodeSPtr Parser::ParseCommaExpression()
	{
		StdVector<AstNodeSPtr> exprs;
		exprs.push_back(ParseExpression());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			exprs.push_back(ParseExpression());
		}
		if (exprs.size() == 1)
			return exprs[0];
		return AstNodeSPtr{ new AstCommaExpr{ std::move(exprs) } };
	}

	AstNodeSPtr Parser::ParseExpression(AstNodeSPtr prev, bool allowBlockExpr)
	{
		AstNodeSPtr expr = prev;
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
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(expr->startTokIdx);
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
			case TokenType::Less:
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
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(expr->startTokIdx);
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
				ParseIndexSlicExpr(expr);
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
				if (!expr)
				{
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
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
					if (expr->nodeKind == AstNodeKind::QualNameExpr)
					{
						AstQualNameExpr* pQualNameExpr = static_cast<AstQualNameExpr*>(expr.get());
						expr = ParseAggrInitExpr(pQualNameExpr);
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
			case TokenType::Hash:
			{
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

				
				
			default:
				return expr;
			}
		}
	}

	AstNodeSPtr Parser::ParseAssignmentExpr(AstNodeSPtr lExpr)
	{
		TokenType op = EatToken().Type();
		AstNodeSPtr rExpr = ParseExpression();
		return AstNodeSPtr{ new AstAssignExpr{ lExpr, op, rExpr } };
	}

	AstNodeSPtr Parser::ParseTernaryExpr(AstNodeSPtr cond)
	{
		EatToken(TokenType::Question);
		AstNodeSPtr trueExpr = ParseExpression();
		EatToken(TokenType::Colon);
		AstNodeSPtr falseExpr = ParseExpression();
		return AstNodeSPtr{ new AstTernaryExpr{ cond, trueExpr, falseExpr } };
	}

	AstNodeSPtr Parser::ParseBinaryExpr(AstNodeSPtr lExpr)
	{
		TokenType op = EatToken().Type();
		AstNodeSPtr expr = ParseExpression();

		if (expr->nodeKind == AstNodeKind::BinaryExpr)
		{
			AstBinaryExpr* binExpr = static_cast<AstBinaryExpr*>(expr.get());
			OperatorPrecedence curPrec = GetPrecedence(op);
			OperatorPrecedence nextPrec = GetPrecedence(binExpr->op);

			if (nextPrec < curPrec)
			{
				AstNodeSPtr left = binExpr->lExpr;
				AstNodeSPtr cur{ new AstBinaryExpr{ lExpr, op, left } };
				binExpr->lExpr = cur;
				return expr;
			}
		}

		return AstNodeSPtr{ new AstBinaryExpr{ lExpr, op, expr } };
	}

	AstNodeSPtr Parser::ParsePostfixExpr(AstNodeSPtr expr)
	{
		Token& tok = EatToken();
		return AstNodeSPtr{ new AstPostfixExpr{ expr, tok.Type(), tok.Idx() } };
	}

	AstNodeSPtr Parser::ParsePrefixExpr()
	{
		Token& tok = EatToken();
		AstNodeSPtr expr = ParseExpression();
		return AstNodeSPtr{ new AstPrefixExpr{ tok.Type(), tok.Idx(), expr } };
	}

	AstNodeSPtr Parser::ParseQualNameExpr()
	{
		u64 startIdx, endIdx;
		StdVector<AstNodeSPtr> idens = ParseAstIdenList(TokenType::ColonColon, startIdx, endIdx);
		return AstNodeSPtr{ new AstQualNameExpr{ startIdx, std::move(idens), endIdx } };
	}

	AstNodeSPtr Parser::ParseIndexSlicExpr(AstNodeSPtr expr)
	{
		bool nullcoalesce = false;
		if (PeekToken().Type() == TokenType::QuestionBracket)
		{
			EatToken();
			nullcoalesce = true;
		}
		else
		{
			EatToken(TokenType::LBracket);
		}
		
		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			AstNodeSPtr end = ParseExpression();
			u64 endIdx = EatToken(TokenType::RBracket).Idx();
			return AstNodeSPtr{ new AstSliceExpr{ expr, nullcoalesce, AstNodeSPtr{}, end, endIdx } };
		}
		
		AstNodeSPtr index = ParseExpression();
		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			AstNodeSPtr end = ParseExpression();
			u64 endIdx = EatToken(TokenType::RBracket).Idx();
			return AstNodeSPtr{ new AstSliceExpr{ expr, nullcoalesce, index, end, endIdx } };
		}

		u64 endIdx = EatToken(TokenType::RBracket).Idx();
		return AstNodeSPtr{ new AstIndexSliceExpr{ expr, nullcoalesce, index, endIdx } };
	}

	AstNodeSPtr Parser::ParseFuncCallExpr(AstNodeSPtr expr)
	{
		EatToken(TokenType::LParen);
		StdVector<AstNodeSPtr> args = ParseArgs();
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstNodeSPtr{ new AstFuncCallExpr{ expr, std::move(args), endIdx } };
	}

	AstNodeSPtr Parser::ParseMemberAccessExpr(AstNodeSPtr expr)
	{
		bool nullcoalesce = false;
		if (PeekToken().Type() == TokenType::QuestionDot)
		{
			EatToken();
			nullcoalesce = true;
		}
		else
		{
			EatToken(TokenType::Dot);
		}
		
		EatToken(TokenType::Dot);
		Token& tok = EatToken(TokenType::Iden);
		StdString iden = tok.Text();
		return AstNodeSPtr{ new AstMemberAccessExpr{ expr, nullcoalesce, std::move(iden), tok.Idx() } };
	}

	AstNodeSPtr Parser::ParseMethodcallExpr(AstNodeSPtr expr)
	{
		bool nullcoalesce = false;
		if (PeekToken().Type() == TokenType::QuestionDot)
		{
			EatToken();
			nullcoalesce = true;
		}
		else
		{
			EatToken(TokenType::Dot);
		}
		
		EatToken(TokenType::Dot);
		StdString iden = EatToken(TokenType::Iden).Text();
		
		EatToken(TokenType::LParen);
		StdVector<AstNodeSPtr> args = ParseArgs();
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		
		return AstNodeSPtr{ new AstMethodCallExpr{ expr, nullcoalesce, std::move(iden), std::move(args), endIdx } };
	}

	AstNodeSPtr Parser::ParseTupleAccessExpr(AstNodeSPtr expr)
	{
		bool nullcoalesce = false;
		if (PeekToken().Type() == TokenType::QuestionDot)
		{
			EatToken();
			nullcoalesce = true;
		}
		else
		{
			EatToken(TokenType::Dot);
		}
		
		EatToken(TokenType::Dot);
		Token& tok = EatToken(TokenType::I32Lit);
		return AstNodeSPtr{ new AstTupleAccessExpr{ expr, nullcoalesce, u16(tok.Signed()), tok.Idx() } };
	}

	AstNodeSPtr Parser::ParseLiteralExpr()
	{
		Token& tok = EatToken();
		return AstNodeSPtr{ new AstLiteralExpr{ tok } };
	}

	AstNodeSPtr Parser::ParseAggrInitExpr(AstQualNameExpr* qualName)
	{
		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> args = ParseArgs();
		while (PeekToken().Type() != TokenType::RParen);
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstNodeSPtr{ new AstAggrInitExpr{ qualName->startTokIdx, std::move(qualName->idens), std::move(args), endIdx } };
	}

	AstNodeSPtr Parser::ParseArrayInitExpr()
	{
		u64 startIdx = EatToken(TokenType::LBracket).Idx();
		StdVector<AstNodeSPtr> exprs;
		exprs.push_back(ParseExpression());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			exprs.push_back(ParseExpression());
		}
		u64 endIdx = EatToken(TokenType::RBracket).Idx();
		return AstNodeSPtr{ new AstArrayInitExpr{ startIdx, std::move(exprs), endIdx } };
	}

	AstNodeSPtr Parser::ParseCastExpr()
	{
		u64 startIdx = EatToken(TokenType::Cast).Idx();
		EatToken(TokenType::LParen);
		AstNodeSPtr type = ParseType();
		EatToken(TokenType::RParen);
		AstNodeSPtr expr = ParseExpression();
		return AstNodeSPtr{ new AstCastExpr{ startIdx, type, expr } };
	}

	AstNodeSPtr Parser::ParseTransmuteExpr()
	{
		u64 startIdx = EatToken(TokenType::Transmute).Idx();
		EatToken(TokenType::LParen);
		AstNodeSPtr type = ParseType();
		EatToken(TokenType::RParen);
		AstNodeSPtr expr = ParseExpression();
		return AstNodeSPtr{ new AstCastExpr{ startIdx, type, expr } };
	}

	AstNodeSPtr Parser::ParseMoveExpr()
	{
		u64 startIdx = EatToken(TokenType::Move).Idx();
		AstNodeSPtr expr = ParseExpression();
		return AstNodeSPtr{ new AstMoveExpr{ startIdx, expr } };
	}

	AstNodeSPtr Parser::ParseBracketExpr()
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		AstNodeSPtr expr = ParseExpression();

		if (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			StdVector<AstNodeSPtr> exprs;
			exprs.push_back(expr);
			while (PeekToken().Type() == TokenType::Comma)
			{
				EatToken();
				exprs.push_back(ParseExpression());
			}
			
			u64 endIdx = EatToken(TokenType::RParen).Idx();
			return AstNodeSPtr{ new AstTupleInitExpr{ startIdx, std::move(exprs), endIdx } };
		}

		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstNodeSPtr{ new AstBracketExpr{ startIdx, expr, endIdx } };
	}

	AstNodeSPtr Parser::ParseBlockExpr()
	{
		u64 startIdx = EatToken(TokenType::LBrace).Idx();
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();
		return AstNodeSPtr{ new AstBlockExpr{ startIdx, std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseUnsafeExpr()
	{
		u64 startIdx = EatToken(TokenType::Unsafe).Idx();
		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();
		return AstNodeSPtr{ new AstBlockExpr{ startIdx, std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseClosureExpr()
	{
		u64 startIdx = EatToken(TokenType::Or).Idx();

		StdVector<AstNodeSPtr> params = ParseParams(true);
		EatToken(TokenType::Or);

		EatToken(TokenType::DblArrow);
		StdVector<AstClosureCapture> captures;
		if (PeekToken().Type() == TokenType::LBracket)
		{
			EatToken();
			do
			{
				if (PeekToken().Type() == TokenType::Comma)
					EatToken();

				switch (PeekToken().Type())
				{
				case TokenType::Eq:
				{
					EatToken();
					captures.push_back(AstClosureCapture{ TokenType::Eq, StdString{} });
				}
				case TokenType::And:
				{
					EatToken();
					StdString iden;
					if (PeekToken().Type() == TokenType::Iden)
						iden = EatToken().Text();
					captures.push_back(AstClosureCapture{ TokenType::And, iden });
				}
				case TokenType::Move:
				{
					EatToken();
					StdString iden;
					if (PeekToken().Type() == TokenType::Iden)
						iden = EatToken().Text();
					captures.push_back(AstClosureCapture{ TokenType::Move, iden });
				}
				case TokenType::Iden:
				{
					captures.push_back(AstClosureCapture{ TokenType::Unknown, EatToken().Text() });
				}
				default:
				{
					Token& tok = PeekToken();
					Span span = m_pCtx->pCompContext->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected token in closure capture: `%s`", pTokName);
				}
				}
			}
			while (PeekToken().Type() == TokenType::Comma);
			EatToken(TokenType::RBracket);
		}

		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();

		return AstNodeSPtr{ new AstClosureExpr{ startIdx, std::move(params), std::move(captures), std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseCompRunExpr()
	{
		u64 startIdx = EatToken(TokenType::Hash).Idx();
		EatIdenToken("run");
		AstNodeSPtr expr = ParseExpression();
		return AstNodeSPtr{ new AstCompRunExpr{ startIdx, expr } };
	}

	AstNodeSPtr Parser::ParseType()
	{
		AstNodeSPtr type;
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
			return AstNodeSPtr(new AstBuiltinType{ tok });
		case TokenType::Iden:
			return ParseIdentifierType();
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
		default:
			return nullptr;
		}
	}

	AstNodeSPtr Parser::ParseIdentifierType()
	{
		u64 startIdx, endIdx;
		StdVector<AstNodeSPtr> idens = ParseAstIdenList(TokenType::ColonColon, startIdx, endIdx);
		return AstNodeSPtr{ new AstIdentifierType{ startIdx, std::move(idens), endIdx } };
	}

	AstNodeSPtr Parser::ParsePointerType()
	{
		u64 startIdx = EatToken(TokenType::Asterisk).Idx();
		AstNodeSPtr type = ParseType();
		return AstNodeSPtr{ new AstPointerType{ startIdx, type } };
	}

	AstNodeSPtr Parser::ParseReferenceType()
	{
		u64 startIdx = EatToken(TokenType::And).Idx();
		AstNodeSPtr type = ParseType();
		return AstNodeSPtr{ new AstPointerType{ startIdx, type } };
	}

	AstNodeSPtr Parser::ParseArraySliceType()
	{
		u64 startIdx = EatToken(TokenType::LBracket).Idx();

		if (PeekToken().Type() == TokenType::RBracket)
		{
			EatToken();
			AstNodeSPtr type = ParseType();
			return AstNodeSPtr{ new AstSliceType{ startIdx, type } };
		}

		AstNodeSPtr expr = ParseExpression();
		EatToken(TokenType::RBracket);
		AstNodeSPtr type = ParseType();
		return AstNodeSPtr{ new AstArrayType{ startIdx, expr, type } };
	}

	AstNodeSPtr Parser::ParseTupleType()
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		StdVector<AstNodeSPtr> types;
		types.push_back(ParseType());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			types.push_back(ParseType());
		}
		u64 endIdx = EatToken().Idx();
		return AstNodeSPtr{ new AstTupleType{ startIdx, std::move(types), endIdx } };
	}

	AstNodeSPtr Parser::ParseOptionalType()
	{
		u64 startIdx = EatToken(TokenType::Question).Idx();
		AstNodeSPtr type = ParseType();
		return AstNodeSPtr{ new AstOptionalType{ startIdx, type } };
	}

	AstNodeSPtr Parser::ParseAttributes()
	{
		StdVector<AstNodeSPtr> compAttribs;
		StdVector<AstNodeSPtr> userAttribs;
		StdVector<AstNodeSPtr> singleAttribs;
		AstNodeSPtr visibilityAttrib = AstNodeSPtr{ nullptr };

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
				singleAttribs.push_back(AstNodeSPtr{ new AstSimpleAttribute{ tok.Idx(), tok.Type() } });
				break;
			}
			case TokenType::Public:
				visibilityAttrib = ParseVisibilityAttribute();
				break;
			case TokenType::At:
				compAttribs.push_back(ParseUserAttribute());
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
			singleAttribs.size() == 0 &&
			!visibilityAttrib)
			return nullptr;
		
		return AstNodeSPtr{ new AstAttributes{ startIdx, std::move(compAttribs), std::move(userAttribs), visibilityAttrib, std::move(singleAttribs), endIdx  } };
	}

	AstNodeSPtr Parser::ParseCompAttribute()
	{
		u64 startIdx = EatToken(TokenType::AtColon).Idx();
		Token idenTok = EatToken(TokenType::Iden);
		StdString iden = idenTok.Text();
		u64 endTokIdx = idenTok.Idx();

		StdVector<AstNodeSPtr> args;
		if (PeekToken().Type() == TokenType::LParen)
		{
			EatToken();
			args.push_back(ParseExpression());
			while (PeekToken().Type() == TokenType::Comma)
			{
				EatToken();
				args.push_back(ParseExpression());
			}
			endTokIdx = EatToken(TokenType::RParen).Idx();
		}

		return AstNodeSPtr{ new AstCompAttribute{ startIdx, std::move(iden), std::move(args), endTokIdx } };
	}

	AstNodeSPtr Parser::ParseUserAttribute()
	{
		u64 startIdx = EatToken(TokenType::AtColon).Idx();
		Token idenTok = EatToken(TokenType::Iden);
		StdString iden = idenTok.Text();
		u64 endTokIdx = idenTok.Idx();

		StdVector<AstNodeSPtr> args;
		if (PeekToken().Type() == TokenType::LParen)
		{
			EatToken();
			args.push_back(ParseExpression());
			while (PeekToken().Type() == TokenType::Comma)
			{
				EatToken();
				args.push_back(ParseExpression());
			}
			endTokIdx = EatToken(TokenType::RParen).Idx();
		}

		return AstNodeSPtr{ new AstUserAttribute{ startIdx, std::move(iden), std::move(args), endTokIdx } };
	}

	AstNodeSPtr Parser::ParseVisibilityAttribute()
	{
		u64 startIdx = EatToken(TokenType::AtColon).Idx();
		u64 endTokIdx = startIdx;

		StdString kind;
		if (PeekToken().Type() == TokenType::LParen)
		{
			EatToken();
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
			
			endTokIdx = EatToken(TokenType::RParen).Idx();
		}

		return AstNodeSPtr{ new AstVisibilityAttribute{ startIdx, std::move(kind), endTokIdx } };
	}

	AstNodeSPtr Parser::ParseGenericDecl()
	{
		u64 startIdx = EatToken(TokenType::Less).Idx();
		StdVector<AstNodeSPtr> params;
		params.push_back(ParseGenericParam());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			params.push_back(ParseGenericParam());
		}
		u64 endIdx = EatToken(TokenType::Greater).Idx();
		return AstNodeSPtr{ new AstGenericDecl{ startIdx, std::move(params), endIdx } };
	}

	AstNodeSPtr Parser::ParseGenericParam()
	{
		if (PeekToken().Type() == TokenType::Colon)
		{
			u64 startIdx = EatToken(TokenType::Colon).Idx();
			if (PeekToken().Type() == TokenType::LBrace)
				return ParseBlockExpr();
			return ParseType();
		}

		Token& tok = EatToken(TokenType::Iden);
		u64 startIdx = tok.Idx();
		StdString iden = tok.Text();

		if (PeekToken().Type() == TokenType::Colon)
		{
			EatToken();
			AstNodeSPtr type = ParseType();

			AstNodeSPtr def;
			if (PeekToken().Type() == TokenType::Eq)
			{
				EatToken();
				def = ParseExpression(nullptr, true);
			}

			return AstNodeSPtr{ new AstGenericValueParam{ startIdx, std::move(iden), type, def } };
		}

		StdVector<AstNodeSPtr> interfaces;
		if (PeekToken().Type() == TokenType::In)
		{
			interfaces.push_back(ParseType());
			while (PeekToken().Type() == TokenType::Plus)
			{
				EatToken();
				interfaces.push_back(ParseType());
			}
		}

		AstNodeSPtr def;
		if (PeekToken().Type() == TokenType::Eq)
		{
			EatToken();
			def = ParseType();
		}
		return AstNodeSPtr{ new AstGenericTypeParam{ startIdx, std::move(iden), std::move(interfaces), def } };
	}

	AstNodeSPtr Parser::ParseGenericWhereClause()
	{
		u64 startIdx = EatIdenToken("where").Idx();
		AstNodeSPtr expr = ParseExpression();
		return AstNodeSPtr{ new AstGenericWhereClause{ startIdx, expr } };
	}

	AstNodeSPtr Parser::ParseGenericInst()
	{
		Token& tok = EatToken(TokenType::Iden);
		u64 idenIdx = tok.Idx();
		StdString iden = tok.Text();
		
		EatToken(TokenType::ExclaimLess);
		StdVector<AstNodeSPtr> args;
		if (PeekToken().Type() == TokenType::LBrace)
			args.push_back(ParseBlockExpr());
		else
			args.push_back(ParseType());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			if (PeekToken().Type() == TokenType::LBrace)
				args.push_back(ParseBlockExpr());
			else
				args.push_back(ParseType());
		}
		u64 endIdx = EatToken(TokenType::Greater).Idx();
		return AstNodeSPtr{ new AstGenericInst{ idenIdx, std::move(iden), std::move(args), endIdx } };
	}

	AstNodeSPtr Parser::ParseMacroVar()
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
		
		return AstNodeSPtr{ new AstMacroVar{ dollarIdx, std::move(iden), kind, tok.Idx() } };
	}

	AstNodeSPtr Parser::ParseMacroSeparator()
	{
		StdVector<Token> toks;
		Token tok = PeekToken();
		do
		{
			toks.push_back(std::move(tok));
			EatToken();
			tok = PeekToken();
		}
		while (tok.Type() != TokenType::LParen && 
			   tok.Type() != TokenType::Dollar &&
			   tok.Type() != TokenType::DollarParen);

		return AstNodeSPtr{ new AstMacroSeparator{ std::move(toks) } };
	}

	AstNodeSPtr Parser::ParseMacroFragment()
	{
		u64 startIdx = EatToken(TokenType::DollarParen).Idx();
		AstNodeSPtr pattern = ParseMacroPattern();
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
		return AstNodeSPtr{ new AstMacroFragment{ startIdx, pattern, rep.Type(), endIdx } };
	}

	AstNodeSPtr Parser::ParseMacroPattern()
	{
		StdVector<AstNodeSPtr> seq;
		while (PeekToken().Type() != TokenType::LParen)
		{
			if (PeekToken().Type() == TokenType::Dollar)
			{
				seq.push_back(ParseMacroVar());
			}
			else if (PeekToken().Type() == TokenType::DollarParen)
			{
				seq.push_back(ParseMacroPattern());
			}
			else
			{
				seq.push_back(ParseMacroSeparator());
			}
		}

		u64 startIdx = seq.front()->startTokIdx;
		u64 endIdx = seq.back()->endTokIdx;
		return AstNodeSPtr{ new AstMacroPattern { startIdx, std::move(seq), endIdx } };
	}

	AstNodeSPtr Parser::ParseMacroRules()
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		AstNodeSPtr pattern = ParseMacroPattern();
		EatToken(TokenType::RParen);
		EatToken(TokenType::DblArrow);
		EatToken(TokenType::LBrace);
		StdVector<AstNodeSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstNodeSPtr{ new AstMacroRule{ startIdx, pattern, std::move(stmts), endIdx } };
	}

	AstNodeSPtr Parser::ParseDeclMacro()
	{
		u64 startIdx = EatToken(TokenType::Macro).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();

		if (PeekToken().Type() == TokenType::LParen)
		{
			EatToken();
			AstNodeSPtr pattern = ParseMacroPattern();
			EatToken(TokenType::RParen);
			EatToken(TokenType::LBrace);
			StdVector<AstNodeSPtr> stmts;
			while (PeekToken().Type() != TokenType::RBrace)
			{
				stmts.push_back(ParseStatement());
			}
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstNodeSPtr{ new AstDeclMacro{ startIdx, std::move(iden), pattern, std::move(stmts), endIdx } };
		}

		StdVector<AstNodeSPtr> rules;
		rules.push_back(ParseMacroRules());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			rules.push_back(ParseMacroRules());
		}

		EatToken(TokenType::LBrace);
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstNodeSPtr{ new AstRulesDeclMacro{ startIdx, std::move(iden), std::move(rules), endIdx } };
	}

	AstNodeSPtr Parser::ParseProcMacro()
	{
		u64 startIdx = EatToken(TokenType::Macro).Idx();
		StdString iden = EatToken(TokenType::Iden).Text();
		EatToken(TokenType::LParen);
		StdString toksIden = EatToken(TokenType::Iden).Text();
		EatToken(TokenType::RParen);

		if (PeekToken().Type() == TokenType::LParen)
		{
			EatToken();
			AstNodeSPtr pattern = ParseMacroPattern();
			EatToken(TokenType::RParen);
			EatToken(TokenType::LBrace);
			StdVector<AstNodeSPtr> stmts;
			while (PeekToken().Type() != TokenType::RBrace)
			{
				stmts.push_back(ParseStatement());
			}
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstNodeSPtr{ new AstProcMacro{ startIdx, std::move(iden), std::move(toksIden), pattern, std::move(stmts), endIdx } };
		}

		StdVector<AstNodeSPtr> rules;
		rules.push_back(ParseMacroRules());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			rules.push_back(ParseMacroRules());
		}

		EatToken(TokenType::LBrace);
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstNodeSPtr{ new AstRulesProcMacro{ startIdx, std::move(iden), std::move(toksIden), std::move(rules), endIdx } };
	}

	AstNodeSPtr Parser::ParseMacroInst()
	{
		u64 startIdx, endIdx;
		StdVector<StdString> idens = ParseIdenList(TokenType::ColonColon, startIdx, endIdx);
		if (PeekToken().Type() == TokenType::ExclaimBrace)
		{
			EatToken();
			StdVector<Token> toks;
			while (PeekToken().Type() != TokenType::RBrace)
			{
				toks.push_back(EatToken());
			}
			endIdx = EatToken(TokenType::RBrace).Idx();
			return AstNodeSPtr{ new AstMacroInst{ startIdx, std::move(idens), toks, endIdx } };
		}
		if (PeekToken().Type() == TokenType::ExclaimBracket)
		{
			EatToken();
			StdVector<Token> toks;
			while (PeekToken().Type() != TokenType::RBracket)
			{
				toks.push_back(EatToken());
			}
			endIdx = EatToken(TokenType::RBracket).Idx();
			return AstNodeSPtr{ new AstMacroInst{ startIdx, std::move(idens), toks, endIdx } };
		}
		
		EatToken(TokenType::ExclaimParen);
		StdVector<Token> toks;
		while (PeekToken().Type() != TokenType::RParen)
		{
			toks.push_back(EatToken());
		}
		endIdx = EatToken(TokenType::RParen).Idx();
		return AstNodeSPtr{ new AstMacroInst{ startIdx, std::move(idens), toks, endIdx } };
	}

	AstNodeSPtr Parser::ParseAstIden()
	{
		Token& tok = EatToken(TokenType::Iden);
		StdString iden = tok.Text();
		return AstNodeSPtr{ new AstIdentifier{ tok.Idx(), std::move(iden) } };
	}

	StdVector<AstNodeSPtr> Parser::ParseParams(bool allowNoType)
	{
		StdVector<AstNodeSPtr> params;
		params.push_back(ParseParam());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			params.push_back(ParseParam(allowNoType));
		}
		return params;
	}

	AstNodeSPtr Parser::ParseParam(bool allowNoType)
	{
		u64 startIdx, endIdx;
		StdVector<std::pair<AstNodeSPtr, StdString>> idens;

		AstNodeSPtr attribs;
		if (PeekToken().Type() != TokenType::Iden)
		{
			attribs = ParseAttributes();
			startIdx = endIdx = attribs->startTokIdx;
			idens.push_back(std::pair{ attribs, EatToken(TokenType::Iden).Text() });
		}
		else
		{
			Token& tok = EatToken(TokenType::Iden);
			startIdx = endIdx = tok.Idx();
			idens.push_back(std::pair{ attribs, tok.Text() });
		}
		
		while (PeekToken().Type() == TokenType::Comma)
		{
			attribs = nullptr;
			if (PeekToken().Type() != TokenType::Iden)
				attribs = ParseAttributes();
			Token& tok = EatToken(TokenType::Iden);
			endIdx = tok.Idx();
			idens.push_back(std::pair{ attribs, tok.Text() });
		}

		if (PeekToken().Type() != TokenType::Colon && allowNoType)
			return AstNodeSPtr{ new AstParam{ startIdx, std::move(idens), nullptr, false, endIdx } };
		
		EatToken(TokenType::Colon);
		AstNodeSPtr type;
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
			endIdx = type->endTokIdx;
			
			if (PeekToken().Type() == TokenType::DotDotDot)
			{
				endIdx = EatToken().Idx();
				isVariadic = true;
			}
		}

		return AstNodeSPtr{ new AstParam{ startIdx, std::move(idens), type, isVariadic, endIdx } };
	}

	StdVector<AstNodeSPtr> Parser::ParseArgs()
	{
		StdVector<AstNodeSPtr> args;
		args.push_back(ParseArg());
		while (PeekToken().Type() == TokenType::Comma)
		{
			EatToken();
			args.push_back(ParseArg());
		}
		return args;
	}

	AstNodeSPtr Parser::ParseArg()
	{
		u64 startIdx;
		StdString iden;
		AstNodeSPtr expr;
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
			startIdx = expr->startTokIdx;
		}

		return AstNodeSPtr{ new AstArg{ startIdx, std::move(iden), expr } };
	}

	StdVector<StdString> Parser::ParseIdenList(TokenType separator)
	{
		u64 start, end;
		return ParseIdenList(separator, start, end);
	}

	StdVector<StdString> Parser::ParseIdenList(TokenType separator, u64& startIdx, u64& endIdx)
	{
		Token& startTok = EatToken(TokenType::Iden);
		startIdx = endIdx = startTok.Idx();

		StdVector<StdString> idens;
		idens.push_back(startTok.Text());
		
		while (PeekToken().Type() == separator)
		{
			EatToken();
			Token& tok = EatToken(TokenType::Iden);
			endIdx = tok.Idx();
			idens.push_back(tok.Text());
		}
		
		return idens;
	}

	StdVector<AstNodeSPtr> Parser::ParseAstIdenList(TokenType separator)
	{
		u64 start, end;
		return ParseAstIdenList(separator, start, end);
	}

	StdVector<AstNodeSPtr> Parser::ParseAstIdenList(TokenType separator, u64& startIdx, u64& endIdx)
	{
		StdVector<AstNodeSPtr> idens;
		if (PeekToken(1).Type() == TokenType::ExclaimLess)
			idens.push_back(ParseGenericInst());
		else
			idens.push_back(ParseAstIden());

		while (PeekToken().Type() == separator)
		{
			EatToken();
			if (PeekToken(1).Type() == TokenType::ExclaimLess)
				idens.push_back(ParseGenericInst());
			else
				idens.push_back(ParseAstIden());
		}

		startIdx = idens.front()->startTokIdx;
		endIdx = idens.back()->endTokIdx;
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
		++m_TokIdx;
		if (m_TokIdx >= m_Tokens.size())
			return emptyTok;
		return m_Tokens[m_TokIdx];
	}

	Token& Parser::EatToken(TokenType type)
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };
		++m_TokIdx;
		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return emptyTok;
		}

		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != type)
		{
			Span span = m_pCtx->pCompContext->spanManager.GetSpan(m_TokIdx);
			const char* pFoundName = GetTokenTypeName(tok.Type()).data();
			const char* pExpectedName = GetTokenTypeName(type).data();
			g_ErrorSystem.Error(span, "Found '%s', expected '%s'", pFoundName, pExpectedName);
			return emptyTok;
		}

		return tok;
	}

	Token& Parser::EatIdenToken(StdStringView text)
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };
		++m_TokIdx;
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
			return emptyTok;
		}

		return tok;
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
