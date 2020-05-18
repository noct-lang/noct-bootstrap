#pragma once

namespace Noctis
{
	struct ILElem;
	struct ILAssign;
	struct ILPrimAssign;
	struct ILPrimBinary;
	struct ILPrimUnary;
	struct ILPrimCast;
	struct ILTernary;
	struct ILTransmute;
	struct ILFuncCall;
	struct ILMethodCall;
	struct ILMemberAccess;
	struct ILTupleAccess;
	struct ILAggrInit;
	struct ILValEnumInit;
	struct ILAdtEnumInit;
	struct ILTupInit;
	struct ILArrInit;
	struct ILReturn;
	struct ILGoto;
	struct ILLabel;
	struct ILSwitch;
	struct ILLoop;
	struct ILIfElse;
	struct ILIf;
	struct ILBlock;
	struct ILFuncDef;
	struct Context;

	class ILVisitor
	{
	public:

		ILVisitor(Context* pCtx);
		virtual ~ILVisitor();

		virtual void Visit(ILElem& elem);
		
		virtual void Visit(ILFuncDef& node);

		virtual void Visit(ILBlock& node);
		virtual void Visit(ILIf& node);
		virtual void Visit(ILIfElse& node);
		virtual void Visit(ILLoop& node);
		virtual void Visit(ILSwitch& node);
		virtual void Visit(ILLabel& node);
		virtual void Visit(ILGoto& node);
		virtual void Visit(ILReturn& node);
		virtual void Visit(ILAssign& node);
		virtual void Visit(ILPrimAssign& node);
		virtual void Visit(ILPrimBinary& node);
		virtual void Visit(ILPrimUnary& node);
		virtual void Visit(ILPrimCast& node);
		virtual void Visit(ILTernary& node);
		virtual void Visit(ILTransmute& node);
		virtual void Visit(ILFuncCall& node);
		virtual void Visit(ILMethodCall& node);
		virtual void Visit(ILMemberAccess& node);
		virtual void Visit(ILTupleAccess& node);
		virtual void Visit(ILAggrInit& node);
		virtual void Visit(ILValEnumInit& node);
		virtual void Visit(ILAdtEnumInit& node);
		virtual void Visit(ILTupInit& node);
		virtual void Visit(ILArrInit& node);
		
		Context* m_pCtx;
	};
	
}