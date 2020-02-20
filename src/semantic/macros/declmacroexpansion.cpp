#include "declmacroexpansion.hpp"
#include "common/context.hpp"
#include "common/compcontext.hpp"
#include "ast/ast.hpp"
#include "common/errorsystem.hpp"
#include "ast/parser.hpp"

namespace Noctis
{
	DeclMacroExpansion::DeclMacroExpansion(Context* pCtx)
		: SemanticPass("decl macro expansion", pCtx)
	{
	}

	void DeclMacroExpansion::Visit(AstMacroInstStmt& node)
	{
		MacroContext& macroCtx = m_pCtx->pCompContext->activeModule->macroCtx;
		StdVector<DeclMacro> macros = macroCtx.GetDeclMacros(node.ctx->scope, node.ctx->qualName);

		for (DeclMacro& macro : macros)
		{
			StdVector<MacroExtractedElem> extractedElems;
			if (!macro.MatchPatternAndExtract(node.toks, extractedElems))
				continue;

			// Expand the actual macro
			StdVector<Token> toks;
			toks.push_back(Token{ TokenType::LBrace, u64(-1) });
			macro.body.ToToks(toks);
			toks.push_back(Token{ TokenType::RBrace, u64(-1) });

			Parser parser{ toks, m_pCtx };
			MacroVarSolver solver{ std::move(extractedElems), m_pCtx };
			solver.PreparseMacroVars();
			solver.CollectMacroVarsForParsing();
			parser.SetMacroVarSolver(&solver);

			node.expandedStmt = parser.ParseBlockStmt();

			if (!parser.HasParsedAllTokens())
			{
				Span span = m_pCtx->pCompContext->spanManager.GetSpan(node.ctx->startIdx);
				g_ErrorSystem.Error(span, "Failed to parse complete statement macro");
			}
		}
	}

	void DeclMacroExpansion::Visit(AstMacroInstExpr& node)
	{
		MacroContext& macroCtx = m_pCtx->pCompContext->activeModule->macroCtx;
		StdVector<DeclMacro> macros = macroCtx.GetDeclMacros(node.ctx->scope, node.ctx->qualName);

		for (DeclMacro& macro : macros)
		{
			StdVector<MacroExtractedElem> extractedElems;
			if (!macro.MatchPatternAndExtract(node.toks, extractedElems))
				continue;

			// Expand the actual macro
			StdVector<Token> toks;
			toks.push_back(Token{ TokenType::LBrace, u64(-1) });
			macro.body.ToToks(toks);
			toks.push_back(Token{ TokenType::RBrace, u64(-1) });
			
			Parser parser{ toks, m_pCtx };
			MacroVarSolver solver{ std::move(extractedElems), m_pCtx };
			solver.PreparseMacroVars();
			parser.SetMacroVarSolver(&solver);
			  
			node.expandedExpr = parser.ParseBlockExpr();
		}
		
	}
}
