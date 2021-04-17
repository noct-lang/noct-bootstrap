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
			node.toks.ResetIdx();
			
			StdVector<MacroExtractedElem> extractedElems;
			if (!macro.MatchPatternAndExtract(node.toks, extractedElems))
				continue;

			// Expand the actual macro
			Parser parser{ macro.body };
			MacroVarSolver solver{ std::move(extractedElems) };
			solver.PreparseMacroVars();
			solver.CollectMacroVarsForParsing();
			parser.SetMacroVarSolver(&solver);

			node.expandedStmt = parser.ParseBlockStmt();

			if (!parser.HasParsedAllTokens())
			{
				Span span = g_SpanManager.GetSpan(node.ctx->startIdx);
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
			Parser parser{ macro.body };
			MacroVarSolver solver{ std::move(extractedElems) };
			solver.PreparseMacroVars();
			parser.SetMacroVarSolver(&solver);
			  
			node.expandedExpr = parser.ParseBlockExpr();
		}
		
	}
}
