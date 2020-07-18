#include "semantic-analysis.hpp"
#include "common/context.hpp"
#include "ast/misc/iden-scope-pass.hpp"
#include "ast/macros/decl-macro-context-gen.hpp"
#include "ast/macros/decl-macro-expansion.hpp"
#include "ast/misc/ast-to-itr-lowering.hpp"
#include "common/errorsystem.hpp"
#include "il/il-gen.hpp"
#include "itr/attribute/simple-attribute-pass.hpp"
#include "itr/misc/function-processing.hpp"
#include "itr/misc/misc-passes.hpp"
#include "itr/types/type-collection.hpp"
#include "itr/types/type-inference.hpp"
#include "itr/types/type-resolution.hpp"
#include "module/encode.hpp"
#include "module/module.hpp"
#include "semantic-utils.hpp"
#include "ast/ast-printer.hpp"

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
			ModuleDecode decode(m_pCtx);
			StdUnorderedSet<QualNameSPtr> extractedImportMods = ExtractImportModules(tree, m_pCtx);
			for (QualNameSPtr modQualName : extractedImportMods)
			{
				auto it = m_pCtx->modules.find(modQualName);
				if (it == m_pCtx->modules.end())
				{
					StdString modName = modQualName->ToString();
					StringReplace(modName, "::", ".");
					g_ErrorSystem.Error("Could not open module: %s", modName.c_str());
				}
				
				ModuleSPtr mod = it->second;

				if (!mod->isDecoded)
				{
					decode.Decode(*mod);
				}
				
				m_pCtx->modules.try_emplace(modQualName, mod);
				m_pCtx->activeModule->imports.try_emplace(modQualName, mod);
				m_pCtx->activeModule->symTable.Merge(mod->symTable);

				// TODO: only when not a static import
				m_pCtx->activeModule->symTable.AddImport(modQualName);
			}
		}

		RunPass<DeclMacroContextGen>();
		RunPass<DeclMacroExpansion>();

		// Rerun for names inside expanded macros
		RunPass<IdenScopePass>();

		if (m_pCtx->options.GetBuildOptions().logAst)
		{
			Noctis::AstPrinter printer;
			printer.Visit(tree);
		}

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

		RunPass<InterfaceResolve>();
		RunPass<ImplCollection>();

		RunPass<TypealiasReplacing>();

		RunPass<CompilerImplPass>();

		{
			TypeInference pass{ m_pCtx };
			pass.SetPrepass();
			pass.Process(*m_pMod);
		}
		
		m_pCtx->activeModule->opTable.Collect(m_pCtx->activeModule->symTable);
		m_pCtx->typeReg.CalculateSizeAlign();
		
		RunPass<LocalVarCollection>();
		
		
		RunPass<TypeInference>();


		RunPass<NameManglePass>();
		RunPass<ILGen>();
	}
}
