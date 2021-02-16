#pragma once
#include "common/defs.hpp"
#include "semantic-pass.hpp"

namespace Noctis
{
	struct AstTree;
	struct Context;

	FWDECL_CLASS_SPTR(QualName);
	
	class AstSemanticAnalysis
	{
	public:
		AstSemanticAnalysis(Context* pCtx);

		void Run(AstTree& tree);

	private:
		template<typename T>
		void RunPass();

		void Import(QualNameSPtr modQualName);
		
		Context* m_pCtx;
		AstTree* m_pTree;
	};

	class ITrSemanticAnalysis
	{
	public:
		ITrSemanticAnalysis(Context* pCtx);

		void Run(ITrModule& mod);

		
	private:
		template<typename T, typename... Args>
		void RunPass(const Args&... args);

		Context* m_pCtx;
		ITrModule* m_pMod;
	};
}
 