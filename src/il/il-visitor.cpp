#include "il-visitor.hpp"
#include "il.hpp"

namespace Noctis
{
	ILVisitor::ILVisitor(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	ILVisitor::~ILVisitor()
	{
	}

	void ILVisitor::Visit(ILElem& elem)
	{
		switch (elem.kind)
		{
		case ILKind::Block: Visit(static_cast<ILBlock&>(elem)); break;
		case ILKind::If: Visit(static_cast<ILIf&>(elem)); break;
		case ILKind::Else: Visit(static_cast<ILIfElse&>(elem)); break;
		case ILKind::Loop: Visit(static_cast<ILLoop&>(elem)); break;
		case ILKind::Switch: Visit(static_cast<ILSwitch&>(elem)); break;
		case ILKind::Label: Visit(static_cast<ILLabel&>(elem)); break;
		case ILKind::Goto: Visit(static_cast<ILGoto&>(elem)); break;
		case ILKind::ReturnNoVal: Visit(static_cast<ILReturn&>(elem)); break;
		case ILKind::ReturnVal: Visit(static_cast<ILReturn&>(elem)); break;
		case ILKind::Assign: Visit(static_cast<ILAssign&>(elem)); break;
		case ILKind::PrimAssign: Visit(static_cast<ILPrimAssign&>(elem)); break;
		case ILKind::PrimBinary: Visit(static_cast<ILPrimBinary&>(elem)); break;
		case ILKind::PrimUnary: Visit(static_cast<ILPrimUnary&>(elem)); break;
		case ILKind::PrimCast: Visit(static_cast<ILPrimCast&>(elem)); break;
		case ILKind::Ternary: Visit(static_cast<ILTernary&>(elem)); break;
		case ILKind::Transmute: Visit(static_cast<ILTransmute&>(elem)); break;
		// TODO
		//case ILKind::CompIntrin: Visit(static_cast<ILBlock&>(elem)); break;
		case ILKind::FuncCallNoRet: Visit(static_cast<ILFuncCall&>(elem)); break;
		case ILKind::FuncCallRet: Visit(static_cast<ILFuncCall&>(elem)); break;
		case ILKind::MethodCallNoRet: Visit(static_cast<ILMethodCall&>(elem)); break;
		case ILKind::MethodCallRet: Visit(static_cast<ILMethodCall&>(elem)); break;
		case ILKind::MemberAccess: Visit(static_cast<ILMemberAccess&>(elem)); break;
		case ILKind::TupleAccess: Visit(static_cast<ILTupleAccess&>(elem)); break;
		case ILKind::AggrInit: Visit(static_cast<ILAggrInit&>(elem)); break;
		case ILKind::ValEnumInit: Visit(static_cast<ILValEnumInit&>(elem)); break;
		case ILKind::AdtEnumInit: Visit(static_cast<ILAdtEnumInit&>(elem)); break;
		case ILKind::TupInit: Visit(static_cast<ILTupInit&>(elem)); break;
		case ILKind::ArrInit: Visit(static_cast<ILArrInit&>(elem)); break;
		case ILKind::FuncDef: Visit(static_cast<ILFuncDef&>(elem)); break;
		default: ;
		}
	}

	void ILVisitor::Visit(ILFuncDef& node)
	{
	}

	void ILVisitor::Visit(ILBlock& node)
	{
		for (ILElemSPtr elem : node.elems)
		{
			Visit(*elem);
		}
	}

	void ILVisitor::Visit(ILIf& node)
	{
		for (ILElemSPtr elem : node.elems)
		{
			Visit(*elem);
		}
	}

	void ILVisitor::Visit(ILIfElse& node)
	{
		for (ILElemSPtr elem : node.elems)
		{
			Visit(*elem);
		}
	}

	void ILVisitor::Visit(ILLoop& node)
	{
		for (ILElemSPtr elem : node.elems)
		{
			Visit(*elem);
		}
	}

	void ILVisitor::Visit(ILSwitch& node)
	{
		// TODO
	}

	void ILVisitor::Visit(ILLabel& node)
	{
	}

	void ILVisitor::Visit(ILGoto& node)
	{
	}

	void ILVisitor::Visit(ILReturn& node)
	{
	}

	void ILVisitor::Visit(ILAssign& node)
	{
	}

	void ILVisitor::Visit(ILPrimAssign& node)
	{
	}

	void ILVisitor::Visit(ILPrimBinary& node)
	{
	}

	void ILVisitor::Visit(ILPrimUnary& node)
	{
	}

	void ILVisitor::Visit(ILPrimCast& node)
	{
	}

	void ILVisitor::Visit(ILTernary& node)
	{
	}

	void ILVisitor::Visit(ILTransmute& node)
	{
	}

	void ILVisitor::Visit(ILFuncCall& node)
	{
	}

	void ILVisitor::Visit(ILMethodCall& node)
	{
	}

	void ILVisitor::Visit(ILMemberAccess& node)
	{
	}

	void ILVisitor::Visit(ILTupleAccess& node)
	{
	}

	void ILVisitor::Visit(ILAggrInit& node)
	{
	}

	void ILVisitor::Visit(ILValEnumInit& node)
	{
	}

	void ILVisitor::Visit(ILAdtEnumInit& node)
	{
	}

	void ILVisitor::Visit(ILTupInit& node)
	{
	}

	void ILVisitor::Visit(ILArrInit& node)
	{
	}
}
