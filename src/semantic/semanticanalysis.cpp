#include "semanticanalysis.hpp"
#include "basic/idenscopepass.hpp"
#include "semanticutils.hpp"
#include "common/context.hpp"
#include "common/compcontext.hpp"
#include "macros/declmacrocontextgen.hpp"
#include "macros/declmacroexpansion.hpp"

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
				m_pCtx->pCompContext->modules.try_emplace(mod, nullptr);
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
