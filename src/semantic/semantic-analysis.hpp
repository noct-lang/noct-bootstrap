#pragma once
#include "common/defs.hpp"
#include "semantic-pass.hpp"

namespace Noctis
{
	struct AstTree;
	struct Context;
	
	class AstSemanticAnalysis
	{
	public:
		AstSemanticAnalysis(Context* pCtx);

		void Run(AstTree& tree);

	private:
		template<typename T>
		void RunPass();
		
		Context* m_pCtx;
		AstTree* m_pTree;
	};

	class ITrSemanticAnalysis
	{
	public:
		ITrSemanticAnalysis(Context* pCtx);

		void Run(ITrModule& mod);

		
	private:
		template<typename T>
		void RunPass();

		Context* m_pCtx;
		ITrModule* m_pMod;
	};
}
 