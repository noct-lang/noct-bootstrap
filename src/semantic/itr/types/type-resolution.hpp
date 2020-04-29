#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{

	class TypealiasReplacing : public ITrSemanticPass
	{
	public:
		TypealiasReplacing(Context* pCtx);

		void Process(ITrModule& mod) override;
	};
	
}
