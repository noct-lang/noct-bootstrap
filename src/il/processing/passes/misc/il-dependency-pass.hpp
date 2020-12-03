#pragma once
#include "il/processing/il-pass.hpp"
#include "module/graph.hpp"
#include "semantic/itr/misc/function-processing.hpp"

namespace Noctis
{

	FWDECL_STRUCT_SPTR(ILFuncDef);
	FWDECL_STRUCT_SPTR(FuncDependencyNode);

	class ILDependencyPass : public ILPass
	{
	public:
		ILDependencyPass(Context* pCtx);

		void Process(ILModule& mod) override;

		void Visit(ILBlock& node) override;
		
		void Visit(ILStaticCall& node) override;

		void Visit(ILIf& node) override;
		void Visit(ILSwitch& node) override;
		void Visit(ILGoto& node) override;

	private:
		ILFuncDefSPtr m_Func;
		
		FuncDependencyNodeSPtr m_FuncNode;
		ILBlockDependencyNodeSPtr m_BlockNode;
	};
	
}
