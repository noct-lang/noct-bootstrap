#include "declmacrocontextgen.hpp"
#include "common/context.hpp"
#include "common/compcontext.hpp"
#include "common/errorsystem.hpp"
#include "ast/ast.hpp"
#include "module/macro.hpp"

namespace Noctis
{
	DeclMacroContextGen::DeclMacroContextGen(Context* pCtx)
		: SemanticPass("decl macro processor", pCtx)
	{
	}

	void DeclMacroContextGen::Visit(AstDeclSPtr node)
	{
		switch (node->declKind)
		{
		case AstDeclKind::DeclMacro:
		{
			AstDeclMacroSPtr macroNode = *reinterpret_cast<AstDeclMacroSPtr*>(&node);
			bool res = m_pCtx->pCompContext->activeModule->macroCtx.AddMacro(macroNode->ctx->scope, macroNode->ctx->iden, macroNode);

			if (!res)
			{
				Span span = m_pCtx->pCompContext->spanManager.GetSpan(node->ctx->startIdx);
				QualNameSPtr qualName = QualName::Create(node->ctx->scope, node->ctx->iden);
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
