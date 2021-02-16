#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	struct TypeHandle;
	struct Context;
	struct ITrModule;

	class CompilerImplPass : public ITrSemanticPass
	{
	public:
		CompilerImplPass(Context* pCtx);

		void Process(ITrModule& mod) override;
	};

	class ImplEliminationPass : public ITrSemanticPass
	{
	public:
		ImplEliminationPass(Context* pCtx);

		void Process(ITrModule& mod) override;
	};
	
}
