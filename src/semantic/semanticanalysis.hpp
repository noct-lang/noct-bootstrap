#pragma once
#include "common/defs.hpp"
#include "semanticpass.hpp"

namespace Noctis
{
	struct AstTree;
	struct Context;
	
	class SemanticAnalysis
	{
	public:
		SemanticAnalysis(Context* pCtx);

		void Run(AstTree& tree);

	private:
		Context* m_pCtx;

		StdVector<std::unique_ptr<SemanticPass>> m_Passes;
	};
	
}
