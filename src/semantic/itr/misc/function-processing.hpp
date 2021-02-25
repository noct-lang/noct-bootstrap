#pragma once
#include "common/type.hpp"
#include "semantic/semantic-pass.hpp"

namespace Noctis
{
	FWDECL_STRUCT_SPTR(FuncContext);

	class LocalVarCollection : public ITrSemanticPass
	{
	public:
		LocalVarCollection();

		void Process(ITrModule& mod) override;

		void Visit(ITrBlock& node) override;
		void Visit(ITrLoop& node) override;
		void Visit(ITrForRange & node) override;
		void Visit(ITrSwitch& node) override;
		void Visit(ITrLocalVar& node) override;

		void Visit(ITrAdtAggrEnumPattern& node) override;
		void Visit(ITrAdtTupleEnumPattern& node) override;
		void Visit(ITrAggrPattern& node) override;
		void Visit(ITrPatternSPtr& ptr, ITrAmbiguousAggrPattern& node) override;
		void Visit(ITrSlicePattern& node) override;
		void Visit(ITrTuplePattern& node) override;
		void Visit(ITrValueBindPattern& node) override;

	private:
		
		FuncContextSPtr m_FuncCtx;
		StdVector<StdString> m_ScopeNames;
		u64 m_SwitchImmIdx;
	};

	class ErrorHandlerCollectionPass : public ITrSemanticPass
	{
	public:
		ErrorHandlerCollectionPass();

		void Process(ITrModule& mod) override;

		void Visit(ITrErrHandler& node) override;
	};
}
