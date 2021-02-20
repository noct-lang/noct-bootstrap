#include "decl-macro-expansion.hpp"
#include "common/context.hpp"
#include "ast/ast.hpp"
#include "common/errorsystem.hpp"
#include "ast/parser.hpp"
#include "module/macro.hpp"
#include "module/module.hpp"

namespace Noctis
{
	DeclMacroExpansion::DeclMacroExpansion()
		: AstSemanticPass("decl macro expansion")
	{
	}

	void DeclMacroExpansion::Visit(AstMacroInstStmt& node)
	{
		MacroContext& macroCtx = g_Ctx.activeModule->macroCtx;
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

			Parser parser{ toks };
			MacroVarSolver solver{ std::move(extractedElems) };
			solver.PreparseMacroVars();
			solver.CollectMacroVarsForParsing();
			parser.SetMacroVarSolver(&solver);

			node.expandedStmt = parser.ParseBlockStmt();

			if (!parser.HasParsedAllTokens())
			{
				Span span = g_Ctx.spanManager.GetSpan(node.ctx->startIdx);
				g_ErrorSystem.Error(span, "Failed to parse complete statement macro");
			}
		}
	}

	void DeclMacroExpansion::Visit(AstMacroInstExpr& node)
	{
		MacroContext& macroCtx = g_Ctx.activeModule->macroCtx;
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
			
			Parser parser{ toks };
			MacroVarSolver solver{ std::move(extractedElems) };
			solver.PreparseMacroVars();
			parser.SetMacroVarSolver(&solver);
			  
			node.expandedExpr = parser.ParseBlockExpr();
		}
		
	}
}
