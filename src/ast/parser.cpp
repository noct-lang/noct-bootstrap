#include "parser.hpp"
#include "common/errorsystem.hpp"
#include "common/context.hpp"
#include "module/macro.hpp"
#include "tokens/span.hpp"
#include "tokens/token.hpp"

namespace Noctis
{
	Parser::Parser(const StdVector<Token>& tokens, Context* pCtx)
		: m_Tokens(tokens)
		, m_TokIdx(0)
		, m_pCtx(pCtx)
		, m_pMacroSolver(nullptr)
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
		u64 startIdx = EatToken(TokenType::SUnittest).Idx();

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
		u64 startIdx = EatToken(TokenType::SBenchmark).Idx();

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
		case TokenType::Typealias: return ParseTypealias(attribs);
		case TokenType::Typedef: return ParseTypedef(attribs);
		case TokenType::Func: return ParseFuncDecl(attribs, funcAreMethods);
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
		case TokenType::Throw: return ParseThrowStmt();
		case TokenType::Defer: return ParseDeferStmt();
		case TokenType::ErrDefer: return ParseErrDeferStmt();
		case TokenType::Unsafe: return ParseUnsafeStmt();
		case TokenType::LBrace: return ParseBlockStmt();
		case TokenType::DollarBrace: return ParseMacroLoopStmt();
		case TokenType::Macro:
		{
			if (PeekToken(1).Type() == TokenType::Func)
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
			if (PeekToken().Text() == "weak" && PeekToken(1).Type() == TokenType::Interface)
				return ParseWeakInterface(attribs);
			
			u64 startIdx, endIdx;
			StdVector<StdString> idens = ParseIdenList(TokenType::Comma, startIdx, endIdx);

			Token& tok = PeekToken();
			if (tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
				return ParseVarDecl(attribs, startIdx, std::move(idens));

			m_TokIdx = startIdx;
			return ParseExprOrMacroStmt();
		}
		default:
		{			
			if (label)
				return label;
			return ParseExprOrMacroStmt();
		}
		}
	}

	AstDeclSPtr Parser::ParseStruct(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Struct).Idx();

		StdString iden;
		if (PeekToken().Type() == TokenType::Iden || PeekToken().Type() == TokenType::MacroIden)
			iden = ParseIden();

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
		if (PeekToken().Type() == TokenType::Iden || PeekToken().Type() == TokenType::MacroIden)
			iden = ParseIden();

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
		StdString iden = ParseIden();
		
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
						Span span = m_pCtx->spanManager.GetSpan(expr->ctx->startIdx - 1);
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
						Span span = m_pCtx->spanManager.GetSpan(type->ctx->startIdx - 1);
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
		StdString iden = ParseIden();

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
		u64 weakIdx = EatIdenToken("weak").Idx();
		EatToken(TokenType::Interface);
		StdString iden = ParseIden();

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
		StdString iden = ParseIden();

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
		StdString iden = ParseIden();

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
		Token& tok = PeekToken();
		u64 startIdx = tok.Idx();
		idens.push_back(ParseIden());

		while (TryEatToken(TokenType::Comma))
			idens.push_back(ParseIden());

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

	AstDeclSPtr Parser::ParseFuncDecl(AstAttribsSPtr attribs, bool asMethod)
	{
		u64 startIdx = EatToken(TokenType::Func).Idx();

		AstMethodReceiverKind recKind = AstMethodReceiverKind::None;
		if (asMethod && TryEatToken(TokenType::LParen))
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
			EatIdenToken("self");
			EatToken(TokenType::RParen);
		}

		
		StdString iden = ParseIden();;

		AstGenericDeclSPtr generics;
		if (PeekToken().Type() == TokenType::Less)
			generics = ParseGenericDecl();

		EatToken(TokenType::LParen);
		StdVector<AstParamSPtr> params;
		if (PeekToken().Type() != TokenType::RParen)
			params = ParseParams(false);
		EatToken(TokenType::RParen);

		bool throws = false;
		AstTypeSPtr errorType;
		if (TryEatIdenToken("throws"))
		{
			throws = true;
			if (TryEatToken(TokenType::LParen))
			{
				errorType = ParseType();
				EatToken(TokenType::RParen);
			}
		}
		
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
		if (PeekToken().Text() == "where")
			whereClause = ParseGenericWhereClause();

		if (asMethod && PeekToken().Type() == TokenType::Semicolon)
		{
			if (whereClause)
			{
				Span span = m_pCtx->spanManager.GetSpan(whereClause->ctx->startIdx);
				g_ErrorSystem.Error(span, "An empty method declaration cannot have a where clause");
			}

			u64 endIdx = EatToken(TokenType::Semicolon).Idx();
			return AstDeclSPtr{ new AstEmptyMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params), retType, endIdx } };
		}

		StdVector<AstStmtSPtr> stmts;
		EatToken(TokenType::LBrace);
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken().Idx();

		if (asMethod)
			return AstDeclSPtr{ new AstMethodDecl { attribs, startIdx, recKind, std::move(iden), generics, std::move(params),
				throws, errorType, retType, std::move(namedRets), whereClause, std::move(stmts), endIdx } };
		
		return AstDeclSPtr{ new AstFuncDecl { attribs, startIdx, std::move(iden), generics, std::move(params), throws,
			errorType, retType, std::move(namedRets), whereClause, std::move(stmts), endIdx } };
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
				if (TryEatIdenToken("as"))
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

		u64 predeclIdx = PeekToken().Idx();
		(void)ParseIdenList(TokenType::Comma);
		Token& tok = PeekToken();
		AstVarDeclSPtr varDecl;
		m_TokIdx = predeclIdx;
		if (tok.Type() == TokenType::Comma || tok.Type() == TokenType::Colon || tok.Type() == TokenType::ColonEq)
		{
			varDecl = ParseVarDecl(nullptr);
		}

		AstExprSPtr cond = ParseExpression(nullptr, true, false);

		AstStmtSPtr body = ParseBlockStmt();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
		{
			if (PeekToken().Type() == TokenType::If)
				elseBody = ParseIfStmt();
			else
				elseBody = ParseBlockStmt();
		}

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
		AstExprSPtr cond = ParseExpression(nullptr, true, false);
		AstStmtSPtr body = ParseBlockStmt();
		return AstStmtSPtr{ new AstWhileStmt{ label, whileIdx, cond, body } };
	}

	AstStmtSPtr Parser::ParseDoWhileStmt(AstLabelStmtSPtr label)
	{
		u64 doIdx = EatToken(TokenType::Do).Idx();
		AstStmtSPtr body = ParseBlockStmt();
		EatToken(TokenType::While);
		AstExprSPtr cond = ParseExpression(nullptr, true, false);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstDoWhileStmt{ label, doIdx, body, cond, endIdx } };
	}

	AstStmtSPtr Parser::ParseForStmt(AstLabelStmtSPtr label)
	{
		u64 forTokIdx = EatToken(TokenType::For).Idx();
		StdVector<StdString> idens = ParseIdenList(TokenType::Comma);
		EatToken(TokenType::In);
		AstExprSPtr range = ParseExpression(nullptr, true, false);
		AstStmtSPtr body = ParseBlockStmt();
		return AstStmtSPtr{ new AstForStmt{ label, forTokIdx, std::move(idens), range, body } };
	}

	AstStmtSPtr Parser::ParseSwitch(AstLabelStmtSPtr label)
	{
		u64 switchIdx = EatToken(TokenType::Switch).Idx();
		AstExprSPtr cond = ParseExpression(nullptr, true, false);
		EatToken(TokenType::LBrace);

		StdVector<AstSwitchCase> cases;
		do
		{
			AstPatternSPtr pattern = ParsePattern();
			AstExprSPtr expr;
			if (TryEatIdenToken("where"))
				expr = ParseExpression();
			EatToken(TokenType::DblArrow);
			AstStmtSPtr body = ParseStatement();

			cases.push_back(AstSwitchCase{ pattern, expr, body });
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


	AstStmtSPtr Parser::ParseThrowStmt()
	{
		u64 startIdx = EatToken(TokenType::Throw).Idx();
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstStmtSPtr{ new AstThrowStmt{ startIdx, expr } };
	}


	AstStmtSPtr Parser::ParseExprOrMacroStmt()
	{
		AstExprSPtr expr = ParseExpression();

		if (expr->exprKind == AstExprKind::MacroInst &&
			PeekToken().Type() != TokenType::Semicolon)
		{
			AstMacroInstExpr* pMacroInst = static_cast<AstMacroInstExpr*>(expr.get());
			return AstStmtSPtr{ new AstMacroInstStmt{ pMacroInst->qualName, std::move(pMacroInst->toks), pMacroInst->ctx->endIdx } };
		}
		
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

	AstStmtSPtr Parser::ParseErrDeferStmt()
	{
		u64 startIdx = EatToken(TokenType::ErrDefer).Idx();
		AstExprSPtr expr = ParseExpression(nullptr, true);
		u64 endIdx = EatToken(TokenType::Semicolon).Idx();
		return AstStmtSPtr{ new AstErrDeferStmt{ startIdx, expr, endIdx } };
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

	AstStmtSPtr Parser::ParseErrorHandlerStmt()
	{
		u64 startIdx = EatToken(TokenType::SErrorHandler).Idx();
		EatToken(TokenType::LParen);
		StdString iden = EatToken(TokenType::Iden).Text();
		AstTypeSPtr type;
		if (TryEatToken(TokenType::Colon))
			type = ParseType();
		EatToken(TokenType::RParen);
		EatToken(TokenType::LBrace);
		StdVector<AstStmtSPtr> stmts;
		while (PeekToken().Type() != TokenType::RBrace)
		{
			stmts.push_back(ParseStatement());
		}
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstStmtSPtr{ new AstErrorHandlerStmt{ startIdx, std::move(iden), type, std::move(stmts), endIdx } };
	}

	AstStmtSPtr Parser::ParseCompIfStmt()
	{
		u64 startIdx = EatToken(TokenType::SIf).Idx();

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
		u64 startIdx = EatToken(TokenType::SConditional).Idx();
		EatToken(TokenType::LParen);
		Token& cond = EatToken(TokenType::Iden);

		Token cmp{ TokenType::Unknown, u64(-1) },
			  val{ TokenType::Unknown, u64(-1) };
		if (PeekToken().Type() != TokenType::RParen)
		{
			cmp = EatToken();
			val = EatToken();
		}

		EatToken(TokenType::RParen);
		AstStmtSPtr body = ParseStatement();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
			elseBody = ParseStatement();
		return AstStmtSPtr{ new AstCompCondStmt{ startIdx, cond, cmp, val, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseCompDebugStmt()
	{
		u64 startIdx = EatToken(TokenType::SDebug).Idx();
		EatToken(TokenType::LParen);
		Token& cond = EatToken(TokenType::Iden);

		Token cmp{ TokenType::Unknown, u64(-1) },
			  val{ TokenType::Unknown, u64(-1) };
		if (PeekToken().Type() != TokenType::RParen)
		{
			cmp = EatToken();
			val = EatToken();
		}

		EatToken(TokenType::RParen);
		AstStmtSPtr body = ParseStatement();
		AstStmtSPtr elseBody;
		if (TryEatToken(TokenType::Else))
			elseBody = ParseStatement();
		return AstStmtSPtr{ new AstCompDebugStmt{ startIdx, cond, cmp, val, body, elseBody } };
	}

	AstStmtSPtr Parser::ParseMacroLoopStmt()
	{
		TokenTree tokTree = ParseTokenTree();
		u64 startIdx = tokTree.subToks[0].tok.Idx();
		u64 endIdx = tokTree.subToks.back().tok.Idx();

		tokTree.subToks.erase(tokTree.subToks.begin());
		tokTree.subToks.erase(tokTree.subToks.end() - 1);
		
		StdVector<Token> subToks;
		tokTree.ToToks(subToks);
		Parser subParser = Parser{ subToks, m_pCtx };
		subParser.SetMacroVarSolver(m_pMacroSolver);

		bool hasLoopedToks = m_pMacroSolver->HasLoopedToks();
		StdVector<AstStmtSPtr> stmts;
		m_pMacroSolver->EnterMacroLoop();
		do
		{
			if (hasLoopedToks)
				subParser.m_Tokens = subToks;
			
			subParser.m_TokIdx = 0;
			while (subParser.m_TokIdx < subParser.m_Tokens.size())
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
		while (TryEatToken(TokenType::Comma));
		if (exprs.size() == 1)
			return exprs[0];
		return AstExprSPtr{ new AstCommaExpr{ std::move(exprs) } };
	}

	AstExprSPtr Parser::ParseExpression(AstExprSPtr prev, bool allowBlockExpr, bool allowAggrInit)
	{
		AstExprSPtr expr = prev;
		while (true)
		{
			Token tok = PeekToken();

			if (PeekToken().Type() == TokenType::MacroIden)
			{
				MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());
				switch (elem.varKind)
				{
				case MacroVarKind::Expr:
				{
					EatToken();
					return elem.GetExpr();
				}
				case MacroVarKind::Toks:
				{
					InsertTreeIntoTokens(elem.tokTree);
					tok = PeekToken();
					break;
				}
				case MacroVarKind::Stmt:
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pName = tok.Text().c_str();
					g_ErrorSystem.Error(span, "macro variable '$%s' cannot be 'stmt' when an expression is expected", pName);
					return nullptr;
				}
				default:;
				}
			}
			
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
			case TokenType::ExclaimExclaim:
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
			case TokenType::DotDot:
			case TokenType::DotDotEq:
			case TokenType::QuestionQuestion:
			case TokenType::QuestionColon:
			case TokenType::In:
			case TokenType::NotIn:
			{
				if (!expr)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
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
			case TokenType::QuestionQuestionEq:
			{
				if (!expr)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
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
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				expr = ParseTernaryExpr(expr);
				break;
			}
			case TokenType::MacroIden:
			{
				EatToken();
				MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());

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
					AstQualNameSPtr qualName{ new AstQualName{ m_TokIdx, false, StdVector<AstQualIdenSPtr>{ qualIden } } };
					expr = AstExprSPtr{ new AstQualNameExpr{ qualName } };
					break;
				}
				default:;
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
				if (!allowAggrInit && expr)
					return expr;
				// fallthrough
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
		while (true)
		{
			Token& tok = PeekToken();
			switch (tok.Type())
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
				if (tok.Text() == "_")
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
					if (tok.Type() == TokenType::LBracket)
					{
						return  ParseArrayInitExpr();
					}
					else
					{
						Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
						const char* pTokName = GetTokenTypeName(tok.Type()).data();
						g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
						EatToken();
						return nullptr;
					}
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
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Expected Expression before '%s'", pTokName);
					EatToken();
					break;
				}

				if (PeekToken(1).Type() == TokenType::I32Lit)
					return ParseTupleAccessExpr(prev);
				if (PeekToken(2).Type() == TokenType::LParen)
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
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					return nullptr;
				}
				return ParseLiteralExpr();
			}
			case TokenType::LBrace:
			{
				if (prev)
				{
					if (prev->exprKind == AstExprKind::QualName)
					{
						AstQualNameExpr* pQualNameExpr = static_cast<AstQualNameExpr*>(prev.get());
						return ParseAggrInitExpr(pQualNameExpr);
					}
					else
					{
						Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
						const char* pTokName = GetTokenTypeName(tok.Type()).data();
						g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
						EatToken();
						return nullptr;
					}
				}
				return ParseBlockExpr();
			}
			case TokenType::Cast:
			{
				if (prev)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				return ParseCastExpr();
			}
			case TokenType::Transmute:
			{
				if (prev)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				return ParseTransmuteExpr();
			}
			case TokenType::Move:
			{
				if (prev)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					break;
				}
				return ParseMoveExpr();
			}
			case TokenType::Unsafe:
			{
				if (prev)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
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
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pTokName = GetTokenTypeName(tok.Type()).data();
					g_ErrorSystem.Error(span, "Unexpected Expression before '%s'", pTokName);
					EatToken();
					EatToken();
					break;
				}

				return ParseCompRunExpr();
			}
			case TokenType::ColonColon:
				return ParseQualNameExpr();
			case TokenType::ExclaimParen:
			case TokenType::ExclaimBracket:
			case TokenType::ExclaimBrace:
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
			OpPrecedence curPrec = GetPrecedence(op);
			OpPrecedence nextPrec = GetPrecedence(binExpr->op);

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
		bool nullcoalesce = TryEatToken(TokenType::QuestionBracket);
		if (!nullcoalesce)
			EatToken(TokenType::LBracket);
		
		if (TryEatToken(TokenType::Colon))
		{
			AstExprSPtr end;
			if (PeekToken().Type() != TokenType::RBracket)
				end = ParseExpression();
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
		AstTypeSPtr type = AstTypeSPtr{ new AstIdentifierType{ nullptr, qualName->qualName } };
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
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstCastExpr{ startIdx, type, expr } };
	}

	AstExprSPtr Parser::ParseTransmuteExpr()
	{
		u64 startIdx = EatToken(TokenType::Transmute).Idx();
		EatToken(TokenType::LParen);
		AstTypeSPtr type = ParseType();
		EatToken(TokenType::RParen);
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstTransmuteExpr{ startIdx, type, expr } };
	}

	AstExprSPtr Parser::ParseMoveExpr()
	{
		u64 startIdx = EatToken(TokenType::Move).Idx();
		AstExprSPtr expr = ParseOperand(nullptr);
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
		AstExprSPtr expr = ParseOperand(nullptr);
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
		u64 isIdx = EatToken().Idx();
		AstTypeSPtr type = ParseType();
		return AstExprSPtr{ new AstIsExpr{ expr, isIdx, type } };
	}

	AstExprSPtr Parser::ParseTryExpr()
	{
		u64 startIdx = EatToken(TokenType::Try).Idx();
		AstExprSPtr	expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstTryExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseSpecKwExpr()
	{
		Token& tok = EatToken();
		return AstExprSPtr{ new AstSpecKwExpr{ tok } };
	}

	AstExprSPtr Parser::ParseCompRunExpr()
	{
		u64 startIdx = EatToken(TokenType::SRun).Idx();
		AstExprSPtr expr = ParseOperand(nullptr);
		return AstExprSPtr{ new AstCompRunExpr{ startIdx, expr } };
	}

	AstExprSPtr Parser::ParseMacroVarExpr()
	{
		u64 dollarIdx = EatToken(TokenType::MacroIden).Idx();
		Token& tok = EatToken(TokenType::Iden);
		StdString iden = tok.Text();
		return AstExprSPtr{ new AstMacroVarExpr{ dollarIdx, std::move(iden), tok.Idx() } };
	}

	AstTypeSPtr Parser::ParseType(bool structKwOptional)
	{
		AstAttribsSPtr attribs = ParseAttributes();
		
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
		{
			EatToken();
			return AstTypeSPtr{ new AstBuiltinType{ attribs, tok } };
		}
		case TokenType::Iden:
		{
			AstIdentifierTypeSPtr idenType = ParseIdentifierType(attribs);
			if (PeekToken().Type() == TokenType::Plus)
				return ParseCompoundInterfaceType(idenType);
			return idenType;
		}
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
				Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
				g_ErrorSystem.Error(span, "Found '{' while parsing type\n");
				return nullptr;
			}
			return ParseInlineStructType(structKwOptional);
		}
		case TokenType::MacroIden:
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());
			switch (elem.varKind)
			{
			case MacroVarKind::Type:
			{
				EatToken();
				return elem.GetType();
			}
			case MacroVarKind::Qual:
			case MacroVarKind::Iden:
			{
				return ParseIdentifierType(attribs);
			}
			case MacroVarKind::Toks:
			{
				InsertTreeIntoTokens(elem.tokTree);
				return ParseType(structKwOptional);
			}
			default:
			{
				Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
				const char* pName = tok.Text().c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'type', 'iden', 'qual' or 'toks'", pName);
				return nullptr;
			}
			}
		}
		default:
			return nullptr;
		}
	}

	AstIdentifierTypeSPtr Parser::ParseIdentifierType(AstAttribsSPtr attribs)
	{;
		AstQualNameSPtr qualName = ParseQualName(true);
		return AstIdentifierTypeSPtr{ new AstIdentifierType{ attribs, qualName } };
	}

	AstTypeSPtr Parser::ParsePointerType(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Asterisk).Idx();
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstPointerType{ attribs, startIdx, type } };
	}

	AstTypeSPtr Parser::ParseReferenceType(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::And).Idx();
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstReferenceType{ attribs, startIdx, type } };
	}

	AstTypeSPtr Parser::ParseArraySliceType(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::LBracket).Idx();

		if (TryEatToken(TokenType::RBracket))
		{
			AstTypeSPtr type = ParseType();
			return AstTypeSPtr{ new AstSliceType{ attribs, startIdx, type } };
		}

		AstExprSPtr expr = ParseExpression();
		EatToken(TokenType::RBracket);
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstArrayType{ attribs, startIdx, expr, type } };
	}

	AstTypeSPtr Parser::ParseTupleType(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		StdVector<AstTypeSPtr> types;
		types.push_back(ParseType());
		while (TryEatToken(TokenType::Comma))
			types.push_back(ParseType());
		u64 endIdx = EatToken().Idx();
		return AstTypeSPtr{ new AstTupleType{ attribs, startIdx, std::move(types), endIdx } };
	}

	AstTypeSPtr Parser::ParseOptionalType(AstAttribsSPtr attribs)
	{
		u64 startIdx = EatToken(TokenType::Question).Idx();
		AstTypeSPtr type = ParseType();
		return AstTypeSPtr{ new AstOptionalType{ attribs, startIdx, type } };
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
		StdPairVector<StdString, AstExprSPtr> members;
		do
		{
			StdPair<StdString, AstExprSPtr> member;
			member.first = EatToken(TokenType::Iden).Text();
			if (PeekToken().Type() == TokenType::Eq)
			{
				EatToken();
				member.second = ParseExpression();
			}
			
			members.push_back(member);
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

	AstPatternSPtr Parser::ParsePattern()
	{
		Token tok = PeekToken();
		AstPatternSPtr pattern;
		if (tok.Type() == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());
			switch (elem.varKind)
			{
			case MacroVarKind::Patr:
			{
				pattern = elem.GetPattern();
				break;
			}
			case MacroVarKind::Toks:
			{
				InsertTreeIntoTokens(elem.tokTree);
				tok = PeekToken();
				break;
			}
			default:
			{
				Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
				const char* pName = tok.Text().c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'patr' or 'toks'", pName);
				return nullptr;
			}
			}
		}
		
		switch (tok.Type())
		{
		case TokenType::DotDotDot:
		{
			EatToken();
			pattern = AstPatternSPtr{ new AstWildcardPattern{ tok.Idx() } };
			break;
		}
		case TokenType::Iden:
		{
			if (tok.Text() == "_")
			{
				EatToken();
				pattern = AstPatternSPtr{ new AstPlaceholderPattern{ tok.Idx() } };
				break;
			}

			AstQualNameSPtr qualName = ParseQualName(false);
			Token& curTok = PeekToken();
			if (curTok.Type() == TokenType::ExclaimParen ||
				curTok.Type() == TokenType::ExclaimBracket ||
				curTok.Type() == TokenType::ExclaimBrace)
			{
				pattern = ParseMacroInstPattern(qualName);
				break;
			}
			
			if (!qualName->global && qualName->idens.size() == 1 && qualName->idens[0]->qualIdenKind == AstQualIdenKind::Identifier)
			{
				AstIden* pIden = static_cast<AstIden*>(qualName->idens[0].get());
				StdString iden = pIden->iden;
				
				if (PeekToken().Type() == TokenType::LParen)
				{
					pattern = ParseEnumPattern(qualName);
					break;
				}

				pattern = ParseValueBindPattern(std::move(iden));
				break;
			}

			if (PeekToken().Type() == TokenType::LBrace)
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
			EatToken();
			pattern = AstPatternSPtr{ new AstLiteralPattern{ tok } };
			break;
		}
		default:
		{
			Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
			const char* pTokName = GetTokenTypeName(tok.Type()).data();
			g_ErrorSystem.Error(span, "Unexpected token '%s' during pattern parsing", pTokName);
			EatToken();
			break;
		}
		}

		switch (PeekToken().Type())
		{
		case TokenType::DotDot:
		case TokenType::DotDotEq:
			return ParseRangePattern(pattern);
		case TokenType::Or:
			return ParseEitherPattern(pattern);
		default:
			return pattern;
		}
	}

	AstPatternSPtr Parser::ParseValueBindPattern(StdString&& iden)
	{
		u64 startIdx = m_TokIdx - 1;
		u64 endIdx = startIdx;
		AstPatternSPtr subPattern;
		if (TryEatToken(TokenType::Arrow))
		{
			subPattern = ParsePattern();
			endIdx = subPattern->ctx->endIdx;
		}
		return AstPatternSPtr{ new AstValueBindPattern{ startIdx, std::move(iden), subPattern, endIdx } };
	}

	AstPatternSPtr Parser::ParseRangePattern(AstPatternSPtr pattern)
	{
		bool inclusive = EatToken().Type() == TokenType::DotDotEq;
		AstPatternSPtr to = ParsePattern();
		return AstPatternSPtr{ new AstRangePattern{ pattern, inclusive, to } };
	}

	AstPatternSPtr Parser::ParseTuplePattern()
	{
		u64 startIdx = EatToken().Idx();
		StdVector<AstPatternSPtr> subPatterns;
		do
		{
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.push_back(pattern);
		}
		while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstPatternSPtr{ new AstTuplePattern{ startIdx, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseEnumPattern(AstQualNameSPtr iden)
	{
		u64 startIdx = iden->ctx->startIdx;
		StdVector<AstPatternSPtr> subPatterns;
		do
		{
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.push_back(pattern);
		} while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstPatternSPtr{ new AstEnumPattern{ startIdx, iden, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseAggrPattern(AstQualNameSPtr qualName)
	{
		EatToken();
		StdPairVector<StdString, AstPatternSPtr> subPatterns;
		do
		{
			StdString iden;
			if (PeekToken().Type() == TokenType::Iden && PeekToken(1).Type() == TokenType::Colon)
			{
				iden = EatToken().Text();
				EatToken();
			}
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.emplace_back(std::move(iden), pattern);
		} while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstPatternSPtr{ new AstAggrPattern{ qualName, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseSlicePattern()
	{
		u64 startIdx = EatToken().Idx();
		StdVector<AstPatternSPtr> subPatterns;
		do
		{
			AstPatternSPtr pattern = ParsePattern();
			subPatterns.push_back(pattern);
		}
		while (TryEatToken(TokenType::Comma));
		u64 endIdx = EatToken(TokenType::RBracket).Idx();
		return AstPatternSPtr{ new AstSlicePattern{ startIdx, std::move(subPatterns), endIdx } };
	}

	AstPatternSPtr Parser::ParseEitherPattern(AstPatternSPtr pattern)
	{
		u64 startIdx = pattern->ctx->startIdx;
		StdVector<AstPatternSPtr> subPatterns;
		subPatterns.push_back(pattern);
		while (TryEatToken(TokenType::Or))
		{
			pattern = ParsePattern();
			subPatterns.push_back(pattern);
		}
		return AstPatternSPtr{ new AstEitherPattern{ std::move(subPatterns) } };
	}

	AstPatternSPtr Parser::ParseTypePattern()
	{
		u64 startIdx = EatToken().Idx();
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
			switch (PeekToken().Type())
			{
			case TokenType::Comptime:
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
		u64 startIdx = EatToken().Idx();
		u64 endIdx = startIdx;

		StdString kind;
		if (TryEatToken(TokenType::LParen))
		{
			Token& tok = EatToken();

			if (tok.Text() != "module" &&
				tok.Text() != "package" &&
				tok.Text() != "dynlib")
			{
				Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
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
		u64 startIdx = EatToken().Idx();
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
		Token& tok = EatToken(TokenType::MacroIden);
		StdString iden = tok.Text();
		EatToken(TokenType::Colon);
		Token& typeTok = EatToken(TokenType::Iden);

		AstMacroVarKind kind = AstMacroVarKind::Unknown;
		if (typeTok.Text() == "stmt")
			kind = AstMacroVarKind::Stmt;
		else if (typeTok.Text() == "expr")
			kind = AstMacroVarKind::Expr;
		else if (typeTok.Text() == "type")
			kind = AstMacroVarKind::Type;
		else if (typeTok.Text() == "qual")
			kind = AstMacroVarKind::Qual;
		else if (typeTok.Text() == "iden")
			kind = AstMacroVarKind::Iden;
		else if (typeTok.Text() == "attr")
			kind = AstMacroVarKind::Attr;
		else if (typeTok.Text() == "toks")
			kind = AstMacroVarKind::Toks;
		else if (typeTok.Text() == "patr")
			kind = AstMacroVarKind::Toks;

		if (kind == AstMacroVarKind::Unknown)
		{
			Span span = m_pCtx->spanManager.GetSpan(typeTok.Idx());
			const char* pKind = typeTok.Text().c_str();
			g_ErrorSystem.Error(span, "Unknown macro variable kind: '%s'", pKind);
		}
		
		return AstMacroPatternElemSPtr{ new AstMacroVar{ tok.Idx(), std::move(iden), kind, typeTok.Idx() } };
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
			   tok.Type() != TokenType::MacroIden &&
			   tok.Type() != TokenType::DollarParen);

		return AstMacroPatternElemSPtr{ new AstMacroSeparator{ std::move(toks) } };
	}

	AstMacroPatternElemSPtr Parser::ParseMacroFragment()
	{
		u64 startIdx = EatToken(TokenType::DollarParen).Idx();
		AstMacroPatternSPtr pattern = ParseMacroPattern(true);
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		Token repTok = Token{ TokenType::Unknown, "", u64(-1) };
		Token rep = PeekToken();
		if (rep.Type() == TokenType::Question ||
			rep.Type() == TokenType::Plus ||
			rep.Type() == TokenType::Asterisk)
		{
			 endIdx = EatToken().Idx();
		}
		else
		{
			repTok = rep;
			rep = PeekToken(1);
			if (rep.Type() == TokenType::Question ||
				rep.Type() == TokenType::Plus ||
				rep.Type() == TokenType::Asterisk)
			{
				EatToken();
				endIdx = EatToken().Idx();
			}
			else
			{
				repTok = rep = Token{ TokenType::Unknown, "", u64(-1) };
			}
		}
		return AstMacroPatternElemSPtr{ new AstMacroFragment{ startIdx, pattern, repTok, rep.Type(), endIdx } };
	}

	AstMacroPatternSPtr Parser::ParseMacroPattern(bool inFragment)
	{
		StdVector<AstMacroPatternElemSPtr> elems;
		while (PeekToken().Type() != TokenType::RParen)
		{
			if (PeekToken().Type() == TokenType::MacroIden)
			{
				elems.push_back(ParseMacroVar());
			}
			else if (PeekToken().Type() == TokenType::DollarParen)
			{
				if (inFragment)
				{
					Span span = m_pCtx->spanManager.GetSpan(PeekToken().Idx());
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
		u64 startIdx = EatToken(TokenType::LParen).Idx();
		AstMacroPatternSPtr pattern = ParseMacroPattern();
		EatToken(TokenType::RParen);
		EatToken(TokenType::DblArrow);
		EatToken(TokenType::LBrace);
		TokenTree body = ParseTokenTree();
		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstMacroRuleSPtr{ new AstMacroRule{ startIdx, pattern, std::move(body), endIdx } };
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
			TokenTree body = ParseTokenTree();
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstDeclSPtr{ new AstDeclMacro{ startIdx, std::move(iden), pattern, std::move(body), endIdx } };
		}

		EatToken(TokenType::LBrace);
		StdVector<AstMacroRuleSPtr> rules;
		rules.push_back(ParseMacroRule());
		while (TryEatToken(TokenType::Comma))
			rules.push_back(ParseMacroRule());

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
			TokenTree body = ParseTokenTree();
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstDeclSPtr{ new AstProcMacro{ startIdx, std::move(iden), std::move(toksIden), pattern, std::move(body), endIdx } };
		}

		EatToken(TokenType::LBrace);
		StdVector<AstMacroRuleSPtr> rules;
		rules.push_back(ParseMacroRule());
		while (TryEatToken(TokenType::Comma))
			rules.push_back(ParseMacroRule());

		u64 endIdx = EatToken(TokenType::RBrace).Idx();
		return AstDeclSPtr{ new AstRulesProcMacro{ startIdx, std::move(iden), std::move(toksIden), std::move(rules), endIdx } };
	}

	TokenTree Parser::ParseTokenTree(Token firstTok)
	{
		if (firstTok.Type() == TokenType::Unknown)
			firstTok = EatToken();

		TokenType closeType;
		switch (firstTok.Type())
		{
		case TokenType::LParen:
		case TokenType::ExclaimParen:
		case TokenType::DollarParen:
		{
			closeType = TokenType::RParen;
			break;
		}
		case TokenType::LBracket:
		case TokenType::ExclaimBracket:
		{
			closeType = TokenType::RBracket;
			break;
		}
		case TokenType::LBrace:
		case TokenType::ExclaimBrace:
		case TokenType::DollarBrace:
		{
			closeType = TokenType::RBrace;
			break;
		}
		default:
			return TokenTree{ firstTok };
		}
		
		StdVector<TokenTree> subTrees;

		if (firstTok.Type() != TokenType::Unknown)
			subTrees.push_back(TokenTree{ firstTok });
		
		Token tok = PeekToken();
		while (tok.Type() != closeType)
		{
			switch (tok.Type())
			{
			case TokenType::LParen:
			case TokenType::ExclaimParen:
			case TokenType::DollarParen:
			case TokenType::LBracket:
			case TokenType::ExclaimBracket:
			case TokenType::LBrace:
			case TokenType::ExclaimBrace:
			case TokenType::DollarBrace:
			{
				EatToken();
				subTrees.push_back(ParseTokenTree(tok));
				break;
			}
			default:
			{
				subTrees.push_back(TokenTree{ EatToken() });
				break;
			}
			}
			tok = PeekToken();
		}

		subTrees.push_back(TokenTree{ EatToken() });
		return TokenTree{ std::move(subTrees) };
	}

	AstExprSPtr Parser::ParseMacroInstExpr(AstQualNameSPtr qualName)
	{
		TokenTree tokTree = ParseTokenTree();
		u64 endIdx = tokTree.subToks.back().tok.Idx();

		// Trim first and last tokens, i.e. '!(' '![' '!{' and '}' ']' ')'
		tokTree.subToks.erase(tokTree.subToks.begin());
		tokTree.subToks.erase(tokTree.subToks.end() - 1);
		
		return AstExprSPtr{ new AstMacroInstExpr{ std::move(qualName), std::move(tokTree), endIdx } };
	}

	AstPatternSPtr Parser::ParseMacroInstPattern(AstQualNameSPtr qualName)
	{
		if (TryEatToken(TokenType::ExclaimBrace))
		{
			TokenTree tokTree = ParseTokenTree();
			u64 endIdx = EatToken(TokenType::RBrace).Idx();
			return AstPatternSPtr{ new AstMacroInstPattern{ std::move(qualName), std::move(tokTree), endIdx } };
		}
		if (TryEatToken(TokenType::ExclaimBracket))
		{
			TokenTree tokTree = ParseTokenTree();
			u64 endIdx = EatToken(TokenType::RBracket).Idx();
			return AstPatternSPtr{ new AstMacroInstPattern{ std::move(qualName), std::move(tokTree), endIdx } };
		}

		EatToken(TokenType::ExclaimParen);
		TokenTree tokTree = ParseTokenTree();
		u64 endIdx = EatToken(TokenType::RParen).Idx();
		return AstPatternSPtr{ new AstMacroInstPattern{ std::move(qualName), std::move(tokTree), endIdx } };
	}

	AstQualNameSPtr Parser::ParseQualName(bool genericInstWithLess)
	{
		u64 startIdx = u64(-1);
		bool global = false;
		if (PeekToken().Type() == TokenType::ColonColon)
		{
			startIdx = EatToken().Idx();
			global = true;
		}

		Token tok = PeekToken();
		if (tok.Type() == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());
			switch (elem.varKind)
			{
			case MacroVarKind::Qual:
			{
				EatToken();
				return elem.GetQualName();
			}
			case MacroVarKind::Iden:
				break;
			case MacroVarKind::Toks:
			{
				InsertTreeIntoTokens(elem.tokTree);
				tok = PeekToken();
				break;
			}
			default:
			{
				Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
				const char* pName = tok.Text().c_str();
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
		while (TryEatToken(TokenType::ColonColon));

		return AstQualNameSPtr{ new AstQualName{ startIdx, global, std::move(qualIdens) } };
	}

	AstQualIdenSPtr Parser::ParseQualIden(bool genericInstWithLess)
	{
		Token tok = PeekToken();
		u64 endIdx = tok.Idx();

		if (tok.Type() == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());
			if (elem.varKind == MacroVarKind::Toks)
			{
				tok = PeekToken();
				InsertTreeIntoTokens(elem.tokTree);
			}
		}
		
		if (tok.Type() == TokenType::Less)
		{
			u64 tmpIdx = EatToken(TokenType::Less).Idx();
			AstTypeSPtr type = ParseType();
			EatIdenToken("as");
			AstIdentifierTypeSPtr interface = ParseIdentifierType();
			endIdx = EatToken(TokenType::Greater).Idx();
			return AstQualIdenSPtr{ new AstTypeDisambiguation{ tmpIdx, type, interface, endIdx } };
		}
		else
		{
			StdString iden = ParseIden();
			StdVector<AstGenericArg> args;
			if (genericInstWithLess && TryEatToken(TokenType::Less) ||
				!genericInstWithLess && TryEatToken(TokenType::ExclaimLess))
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

			return AstQualIdenSPtr{ new AstIden{ tok.Idx(), std::move(iden), std::move(args), endIdx } };
		}
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
		StdVector<AstParamVarSPtr> vars;

		AstAttribsSPtr attribs;
		if (PeekToken().Type() != TokenType::Iden)
		{
			attribs = ParseAttributes();
			startIdx = attribs->ctx->startIdx;
			u64 tmpIdx = m_TokIdx;
			StdString iden = ParseIden();
			endIdx = m_TokIdx - 1;
			vars.emplace_back(new AstParamVar{ attribs, tmpIdx, std::move(iden), endIdx });
		}
		else
		{
			Token& tok = PeekToken();
			startIdx = tok.Idx();
			u64 tmpIdx = m_TokIdx;
			StdString iden = ParseIden();
			endIdx = m_TokIdx - 1;
			vars.emplace_back(new AstParamVar{ attribs, tmpIdx, std::move(iden), endIdx });
		}

		while (TryEatToken(TokenType::Comma))
		{
			attribs = nullptr;
			if (PeekToken().Type() != TokenType::Iden)
				attribs = ParseAttributes();
			u64 tmpIdx = m_TokIdx;
			StdString iden = ParseIden();
			endIdx = m_TokIdx - 1;
			vars.emplace_back(new AstParamVar{ attribs, tmpIdx, std::move(iden), endIdx });
		}

		if (PeekToken().Type() != TokenType::Colon && allowNoType)
			return AstParamSPtr{ new AstParam{ startIdx, std::move(vars), nullptr, false, endIdx } };
		
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

		return AstParamSPtr{ new AstParam{ startIdx, std::move(vars), type, isVariadic, endIdx } };
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

	StdString Parser::ParseIden()
	{
		u64 dummy;
		return ParseIden(dummy);
	}

	StdString Parser::ParseIden(u64& tokIdx)
	{
		Token& tok = PeekToken();
		tokIdx = tok.Idx();
		if (tok.Type() == TokenType::MacroIden)
		{
			MacroExtractedElem& elem = m_pMacroSolver->GetElem(tok.Text());
			switch (elem.varKind)
			{
			case MacroVarKind::Iden:
			{
				EatToken();
				return elem.actIden;
			}
			case MacroVarKind::Toks:
			{
				if (elem.tokTree.subToks.size() != 1)
				{
					Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
					const char* pName = tok.Text().c_str();
					g_ErrorSystem.Error("macro variable '$%s' contains more than 1 token when parsed as identifier");
					return "";
				}
				return elem.tokTree.subToks[0].tok.Text();
			}
			default:
			{
				Span span = m_pCtx->spanManager.GetSpan(tok.Idx());
				const char* pName = tok.Text().c_str();
				g_ErrorSystem.Error(span, "macro variable '$%s' is not 'iden' or 'toks'", pName);
				return "";
			}
			}
		}

		return EatToken(TokenType::Iden).Text();
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

	Token& Parser::EatToken()
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };
		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return emptyTok;
		}
		return m_Tokens[m_TokIdx++];
	}

	Token& Parser::EatToken(TokenType type)
	{
		static Token emptyTok{ TokenType::Unknown, "", u64(-1) };

		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return emptyTok;
		}
		
		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != type)
		{
			Span span = m_pCtx->spanManager.GetSpan(m_TokIdx);
			const char* pFoundName = GetTokenTypeName(tok.Type()).data();
			const char* pExpectedName = GetTokenTypeName(type).data();
			g_ErrorSystem.Error(span, "Found '%s', expected '%s'", pFoundName, pExpectedName);
			++m_TokIdx;
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
		if (tok.Type() != TokenType::Iden || tok.Text() != text)
		{
			Span span = m_pCtx->spanManager.GetSpan(m_TokIdx);
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
		if (m_TokIdx >= m_Tokens.size())
			return false;
		
		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != type)
			return false;

		++m_TokIdx;
		return true;
	}

	bool Parser::TryEatIdenToken(StdStringView text)
	{
		if (m_TokIdx >= m_Tokens.size())
		{
			g_ErrorSystem.Error(0, 0, "Reached end of token stream");
			return false;
		}

		Token& tok = m_Tokens[m_TokIdx];
		if (tok.Type() != TokenType::Iden || tok.Text() != text)
			return false;

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

	void Parser::InsertTreeIntoTokens(TokenTree& tree)
	{
		StdVector<Token> toks;
		tree.ToToks(toks);
		m_Tokens.erase(m_Tokens.begin() + m_TokIdx);
		m_Tokens.insert(m_Tokens.begin() + m_TokIdx, toks.begin(), toks.end());
	}
}
