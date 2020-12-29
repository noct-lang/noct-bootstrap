#pragma once
#include "common/defs.hpp"
#include "il-visitor.hpp"

namespace Noctis
{
	struct ILModule;
	struct ILVar;

	class ILPrinter : public ILVisitor
	{
	public:
		ILPrinter(Context* pCtx);

		void Print(ILModule& mod);

		void Visit(ILFuncDef& node) override;
		void Visit(ILBlock& node) override;
		void Visit(ILIf& node) override;
		void Visit(ILSwitch& node) override;
		void Visit(ILGoto& node) override;
		void Visit(ILReturn& node) override;
		void Visit(ILUnreachable& node) override;
		void Visit(ILAssign& node) override;
		void Visit(ILPrimAssign& node) override;
		void Visit(ILPrimBinary& node) override;
		void Visit(ILPrimUnary& node) override;
		void Visit(ILPrimCast& node) override;
		void Visit(ILTernary& node) override;
		void Visit(ILTransmute& node) override;
		void Visit(ILIndex& node) override;
		void Visit(ILGenVal& node) override;
		void Visit(ILCompIntrin& node) override;
		void Visit(ILStaticCall& node) override;
		void Visit(ILDynamicCall& node) override;
		void Visit(ILIndirectCall& node) override;
		void Visit(ILMemberAccess& node) override;
		void Visit(ILTupleAccess& node) override;
		void Visit(ILStructInit& node) override;
		void Visit(ILUnionInit& node) override;
		void Visit(ILValEnumInit& node) override;
		void Visit(ILAdtEnumInit& node) override;
		void Visit(ILTupInit& node) override;
		void Visit(ILArrInit& node) override;

	private:
		void PrintIndent();
		void LogVar(ILVar& var);
		
		u8 m_Indent;
	};
	
}
