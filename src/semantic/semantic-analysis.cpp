#include "semantic-analysis.hpp"
#include "basic/iden-scope-pass.hpp"
#include "semantic-utils.hpp"
#include "common/context.hpp"
#include "macros/decl-macro-context-gen.hpp"
#include "macros/decl-macro-expansion.hpp"

namespace Noctis
{
	SemanticAnalysis::SemanticAnalysis(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	void SemanticAnalysis::Run(AstTree& tree)
	{
		{
			IdenScopePass idenScopePass{ m_pCtx };
			idenScopePass.Process(tree);
		}

		{
			StdUnorderedSet<QualNameSPtr> extractedImportMods = ExtractImportModules(tree, m_pCtx);
			for (QualNameSPtr mod : extractedImportMods)
			{
				m_pCtx->modules.try_emplace(mod, nullptr);
			}
		}

		// TODO: Load modules

		{
			DeclMacroContextGen contextGen{ m_pCtx };
			contextGen.Process(tree);
		}

		{
			DeclMacroExpansion expansion{ m_pCtx };
			expansion.Process(tree);
		}
		
	}
}
