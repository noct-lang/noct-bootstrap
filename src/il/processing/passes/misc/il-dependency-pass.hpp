#pragma once
#include "il/processing/il-pass.hpp"
#include "semantic/itr/misc/function-processing.hpp"


namespace Noctis
{

	class ILDependencyPass : public ILPass
	{
	public:
		ILDependencyPass(Context* pCtx);

		void Process(ILModule& mod) override;

		void Visit(ILFuncCall& node) override;

	private:

		FuncDependencyNodeSPtr m_FuncNode;
	};
	
}
