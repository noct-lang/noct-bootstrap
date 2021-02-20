#pragma once
#include "il/processing/il-pass.hpp"

namespace Noctis
{

	class ILMarkerPass : public ILPass
	{
	public:
		ILMarkerPass();

		void Process(ILModule& mod) override;
	};
	
}
