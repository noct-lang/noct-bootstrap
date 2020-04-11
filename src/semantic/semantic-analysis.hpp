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
		Context* m_pCtx;
	};

	class ITrSemanticAnalysis
	{
	public:
		ITrSemanticAnalysis(Context* pCtx);

		void Run(ITrModule& mod);

	private:
		Context* m_pCtx;
	};
	
}
 