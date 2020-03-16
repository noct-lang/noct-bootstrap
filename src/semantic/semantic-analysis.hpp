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

		StdVector<std::unique_ptr<AstSemanticPass>> m_Passes;
	};
	
}
 