#pragma once
#include "il/processing/il-pass.hpp"

namespace Noctis
{

	class ILMarkerPass : public ILPass
	{
	public:
		ILMarkerPass(Context* pCtx);

		void Process(ILModule& mod) override;
	};
	
}
