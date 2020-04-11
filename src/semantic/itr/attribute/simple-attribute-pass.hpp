#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{

	class SimpleAttributePass : public ITrSemanticPass
	{
	public:
		SimpleAttributePass(Context* pCtx);
		
		void Process(ITrModule& mod) override;
		void Visit(ITrLocalVar& node) override;
		void Visit(ITrType& node) override;
	};
	
}
