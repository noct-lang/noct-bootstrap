#include "semantic-analysis.hpp"
#include "common/context.hpp"
#include "ast/misc/iden-scope-pass.hpp"
#include "ast/macros/decl-macro-context-gen.hpp"
#include "ast/macros/decl-macro-expansion.hpp"
#include "ast/misc/ast-to-itr-lowering.hpp"
#include "common/errorsystem.hpp"
#include "il/il-gen.hpp"
#include "itr/attribute/attribute-passes.hpp"
#include "itr/misc/function-processing.hpp"
#include "itr/misc/misc-passes.hpp"
#include "itr/types/type-collection.hpp"
#include "itr/types/type-inference.hpp"
#include "itr/types/type-resolution.hpp"
#include "module/encode.hpp"
#include "module/module.hpp"
#include "semantic-utils.hpp"
#include "ast/ast-printer.hpp"
#include "common/logger.hpp"
#include "itr/misc/compiler-impl-passes.hpp"

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
		g_Logger.Log("-- AST SEMANTIC ANALYSIS (file: %s)\n", tree.filepath.c_str());
		Timer timer(true);
		
		m_pTree = &tree;
		
		RunPass<IdenScopePass>();

		{
			Timer importTimer(true);
			
			ModuleDecode decode(m_pCtx);
			StdUnorderedSet<QualNameSPtr> extractedImportMods = ExtractImportModules(tree, m_pCtx);

			StdStack<QualNameSPtr> toImport;
			for (QualNameSPtr toImportName : extractedImportMods)
			{
				toImport.push(toImportName);
			}

			while (!toImport.empty())
			{
				QualNameSPtr modQualName = toImport.top();
				toImport.pop();
				
				Import(modQualName);
			}

			importTimer.Stop();
			g_Logger.Log("%16s : module import\n", importTimer.GetSMSFormat().c_str());
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

		timer.Stop();
		g_Logger.Log("-- TOOK %s\n", timer.GetSMSFormat().c_str());
	}

	void AstSemanticAnalysis::Import(QualNameSPtr modQualName)
	{
		auto it = m_pCtx->modules.find(modQualName);
		if (it == m_pCtx->modules.end())
		{
			StdString modName = modQualName->ToString();
			StringReplace(modName, "::", ".");
			g_ErrorSystem.Error("Could not open module: %s", modName.c_str());
		}

		ModuleSPtr mod = it->second;
		if (mod->isDecoded)
			return;

		if (!mod->isDecoded)
		{
			for (QualNameSPtr subImport : mod->header.imports)
			{
				Import(subImport);
			}
			
			ModuleDecode decode{ m_pCtx };
			decode.Decode(*mod);

			m_pCtx->modules.try_emplace(modQualName, mod);
			m_pCtx->activeModule->imports.try_emplace(modQualName, mod);
			m_pCtx->activeModule->symTable.Merge(mod->symTable);

			// TODO: only when not a static import
			m_pCtx->activeModule->symTable.AddImport(modQualName);
		}
	}

	template <typename T, typename... Args>
	void ITrSemanticAnalysis::RunPass(const Args&... args)
	{
		static_assert(std::is_base_of_v<ITrSemanticPass, T>, "");
		T pass{ m_pCtx, args... };
		pass.SetModule(*m_pMod);
		pass.Process(*m_pMod);
	}
	
	ITrSemanticAnalysis::ITrSemanticAnalysis(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pMod(nullptr)
	{
	}

	void ITrSemanticAnalysis::Run(ITrModule& mod)
	{
		g_Logger.Log("-- ITR SEMANTIC ANALYSIS\n");
		Timer timer(true);
		
		m_pMod = &mod;
		
		RunPass<SimpleAttributePass>();
		RunPass<GenericTypeCollection>();
		RunPass<TypeCollection>();

		RunPass<InterfaceResolve>();

		RunPass<ImplCollection>();

		RunPass<TypealiasReplacing>();

		RunPass<CompilerImplPass>();

		RunPass<TypeInference>(true);
		RunPass<ImplEliminationPass>();

		RunPass<MarkingPass>();
		
		m_pCtx->activeModule->opTable.Collect(m_pCtx->activeModule->symTable);
		m_pCtx->typeReg.CalculateSizeAlign();
		
		RunPass<LocalVarCollection>();
		
		RunPass<TypeInference>(false);

		RunPass<SwitchProcessPass>();

		RunPass<ErrorHandlerCollectionPass>();

		RunPass<CopyCheckPass>();
		RunPass<TryCheckPass>();

		RunPass<NameManglePass>();
		RunPass<ILGen>();

		timer.Stop();
		g_Logger.Log("-- TOOK %s\n", timer.GetSMSFormat().c_str());
	}
}
