#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(Symbol);

	class TypealiasReplacing : public ITrSemanticPass
	{
	public:
		TypealiasReplacing(Context* pCtx);

		void Process(ITrModule& mod) override;
	};

	class InterfaceResolve : public ITrSemanticPass
	{
	public:
		InterfaceResolve(Context* pCtx);

		void Process(ITrModule& mod) override;
	};

	class CompilerImplPass : public ITrSemanticPass
	{
	public:
		CompilerImplPass(Context* pCtx);

		void Process(ITrModule& mod) override;
	};
	
}
