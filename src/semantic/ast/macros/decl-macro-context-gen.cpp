#include "decl-macro-context-gen.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "ast/ast.hpp"
#include "module/macro.hpp"
#include "module/module.hpp"

namespace Noctis
{
	DeclMacroContextGen::DeclMacroContextGen()
		: AstSemanticPass("decl macro context gen")
	{
	}

	void DeclMacroContextGen::Visit(AstDeclSPtr& node)
	{
		switch (node->declKind)
		{
		case AstDeclKind::DeclMacro:
		{
			AstDeclMacroSPtr macroNode = *reinterpret_cast<AstDeclMacroSPtr*>(&node);
			bool res = g_Ctx.activeModule->macroCtx.AddMacro(macroNode->ctx->scope, macroNode->ctx->qualName->LastIden(), macroNode);

			if (!res)
			{
				Span span = g_Ctx.spanManager.GetSpan(node->ctx->startIdx);
				QualNameSPtr qualName = node->ctx->scope->Append(node->ctx->qualName->LastIden());
				StdString qualNameStr = qualName->ToString();
				const char* pQualNameStr = qualNameStr.c_str();
				g_ErrorSystem.Error(span, "Macro '%s' is already defined with the same pattern", pQualNameStr);
			}
			
			return;
		}
		case AstDeclKind::RulesDeclMacro:
		{
			//AstRulesDeclMacroSPtr macroNode = *reinterpret_cast<AstRulesDeclMacroSPtr*>(&node);
			//m_pCtx->pCompContext->macroCtx.AddMacro(macroNode->ctx->scope, macroNode);
			return;
		}
		default:;
		}

		AstVisitor::Visit(node);
	}
}
