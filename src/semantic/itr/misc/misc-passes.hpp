#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	// for any impl that implements multiple interfaces, split it up in multiple impl blocks for each interface + additional one for non interface methods
	//class ImplSplitPass : public ITrSemanticPass
	//{
	//	
	//};

	class NameManglePass : public ITrSemanticPass
	{
	public:
		NameManglePass(Context* pCtx);
		
		void Process(ITrModule& mod) override;
	};
	
}
