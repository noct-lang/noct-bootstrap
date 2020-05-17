#pragma once
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(FuncContext);

	class LocalVarCollection : public ITrSemanticPass
	{
	public:
		LocalVarCollection(Context* pCtx);

		void Process(ITrModule& mod) override;


		void Visit(ITrBlock& node) override;
		void Visit(ITrLoop& node) override;
		void Visit(ITrSwitch& node) override;
		void Visit(ITrLocalVar& node) override;
		void Visit(ITrBlockExpr& node) override;
		void Visit(ITrUnsafeExpr& node) override;

		
	private:
		FuncContextSPtr m_FuncCtx;
		StdVector<StdString> m_ScopeNames;
	};
	
}