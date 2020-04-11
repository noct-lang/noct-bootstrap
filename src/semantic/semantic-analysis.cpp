#include "semantic-analysis.hpp"
#include "common/context.hpp"
#include "ast/misc/iden-scope-pass.hpp"
#include "ast/macros/decl-macro-context-gen.hpp"
#include "ast/macros/decl-macro-expansion.hpp"
#include "ast/misc/ast-to-itr-lowering.hpp"
#include "itr/attribute/simple-attribute-pass.hpp"
#include "itr/types/type-collection.hpp"
#include "semantic-utils.hpp"

namespace Noctis
{
	template<typename T>
	void RunAstSemanticPass(Context* pCtx, AstTree& tree)
	{
		static_assert(std::is_base_of_v<AstSemanticPass, T>, "");
		T pass{ pCtx };
		pass.Process(tree);
	}
	
	AstSemanticAnalysis::AstSemanticAnalysis(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	void AstSemanticAnalysis::Run(AstTree& tree)
	{
		RunAstSemanticPass<IdenScopePass>(m_pCtx, tree);

		{
			StdUnorderedSet<QualNameSPtr> extractedImportMods = ExtractImportModules(tree, m_pCtx);
			for (QualNameSPtr mod : extractedImportMods)
			{
				m_pCtx->modules.try_emplace(mod, nullptr);
			}
		}

		// TODO: Load modules

		RunAstSemanticPass<DeclMacroContextGen>(m_pCtx, tree);
		RunAstSemanticPass<DeclMacroExpansion>(m_pCtx, tree);

		// Rerun for names inside expanded macros
		RunAstSemanticPass<IdenScopePass>(m_pCtx, tree);

		RunAstSemanticPass<AstToITrLowering>(m_pCtx, tree);
	}

	template<typename T>
	void RunITrSemanticPass(Context* pCtx, ITrModule& mod)
	{
		static_assert(std::is_base_of_v<ITrSemanticPass, T>, "");
		T pass{ pCtx };
		pass.Process(mod);
	}
	
	ITrSemanticAnalysis::ITrSemanticAnalysis(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	void ITrSemanticAnalysis::Run(ITrModule& mod)
	{
		RunITrSemanticPass<SimpleAttributePass>(m_pCtx, mod);
		RunITrSemanticPass<TypeCollection>(m_pCtx, mod);
	}
}
