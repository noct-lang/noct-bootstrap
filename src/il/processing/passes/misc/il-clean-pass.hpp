#pragma once
#include "il/processing/il-pass.hpp"

namespace Noctis
{

	class ILRemoveUntouchableBlockPass : public ILPass
	{
	public:
		ILRemoveUntouchableBlockPass(Context* pCtx);

		void Process(ILModule& mod) override;
	};

	class ILBlockMergePass : public ILPass
	{
	public:
		ILBlockMergePass(Context* pCtx);

		void Process(ILModule& mod) override;
	};

	class ILRemoveGotoOnlyPass : public ILPass
	{
	public:
		ILRemoveGotoOnlyPass(Context* pCtx);

		void Process(ILModule& mod) override;
	};
	
}
