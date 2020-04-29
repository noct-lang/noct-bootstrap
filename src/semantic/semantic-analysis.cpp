#include "semantic-analysis.hpp"
#include "common/context.hpp"
#include "ast/misc/iden-scope-pass.hpp"
#include "ast/macros/decl-macro-context-gen.hpp"
#include "ast/macros/decl-macro-expansion.hpp"
#include "ast/misc/ast-to-itr-lowering.hpp"
#include "itr/attribute/simple-attribute-pass.hpp"
#include "itr/comptime/comptime-resolution.hpp"
#include "itr/misc/function-processing.hpp"
#include "itr/types/type-collection.hpp"
#include "itr/types/type-inference.hpp"
#include "itr/types/type-resolution.hpp"
#include "module/module.hpp"
#include "semantic-utils.hpp"

namespace Noctis
{
	template<typename T>
	void AstSemanticAnalysis::RunPass()
	{
		static_assert(std::is_base_of_v<AstSemanticPass, T>, "");
		T pass{ m_pCtx };
		pass.Process(*m_pTree);
	}
	
	AstSemanticAnalysis::AstSemanticAnalysis(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pTree(nullptr)
	{
	}

	void AstSemanticAnalysis::Run(AstTree& tree)
	{
		m_pTree = &tree;
		
		RunPass<IdenScopePass>();

		{
			StdUnorderedSet<QualNameSPtr> extractedImportMods = ExtractImportModules(tree, m_pCtx);
			for (QualNameSPtr mod : extractedImportMods)
			{
				m_pCtx->modules.try_emplace(mod, nullptr);
			}
		}

		// TODO: Load modules

		RunPass<DeclMacroContextGen>();
		RunPass<DeclMacroExpansion>();

		// Rerun for names inside expanded macros
		RunPass<IdenScopePass>();

		RunPass<AstToITrLowering>();
	}

	template <typename T>
	void ITrSemanticAnalysis::RunPass()
	{
		static_assert(std::is_base_of_v<ITrSemanticPass, T>, "");
		T pass{ m_pCtx };
		pass.Process(*m_pMod);
	}
	
	ITrSemanticAnalysis::ITrSemanticAnalysis(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pMod(nullptr)
	{
	}

	void ITrSemanticAnalysis::Run(ITrModule& mod)
	{
		m_pMod = &mod;
		
		RunPass<SimpleAttributePass>();
		RunPass<TypeCollection>();

		RunPass<ComptimeCollection>();


		RunPass<TypealiasReplacing>();
		
		m_pCtx->activeModule->opTable.Collect(m_pCtx->activeModule->symTable);
		
		RunPass<LocalVarCollection>();
		
		
		RunPass<TypeInference>();
		
	}
}
