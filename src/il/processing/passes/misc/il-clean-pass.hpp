#pragma once
#include "il/processing/il-pass.hpp"

namespace Noctis
{

	class ILRemoveUntouchableBlockPass : public ILPass
	{
	public:
		ILRemoveUntouchableBlockPass();

		void Process(ILModule& mod) override;
	};

	class ILBlockMergePass : public ILPass
	{
	public:
		ILBlockMergePass();

		void Process(ILModule& mod) override;
	};

	class ILRemoveGotoOnlyPass : public ILPass
	{
	public:
		ILRemoveGotoOnlyPass();

		void Process(ILModule& mod) override;

	private:
		u32 GetGoto(u32 id, StdUnorderedMap<u32, u32>& gotoMapping);
	};
	
}
