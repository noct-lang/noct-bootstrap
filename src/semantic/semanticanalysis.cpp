#include "semanticanalysis.hpp"
#include "basic/idenscopepass.hpp"
#include "semanticutils.hpp"
#include "common/context.hpp"
#include "common/compcontext.hpp"

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
			idenScopePass.Visit(tree);
		}

		{
			StdUnorderedSet<QualNameSPtr> extractedImportMods = ExtractImportModules(tree, m_pCtx);
			for (QualNameSPtr mod : extractedImportMods)
			{
				m_pCtx->pCompContext->importModules.try_emplace(mod, nullptr);
			}
		}
		
	}
}
