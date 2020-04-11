#pragma once
#include "ast/ast-visitor.hpp"
#include "common/perf.hpp"
#include "itr/itr-visitor.hpp"

namespace Noctis
{
	struct Context;
	
	class AstSemanticPass : public AstVisitor
	{
	public:
		AstSemanticPass(StdStringView pName, Context* pCtx);
		virtual ~AstSemanticPass();

		virtual void Process(AstTree& tree);

	protected:
		Timer m_Timer;
		StdStringView m_Name;
		Context* m_pCtx;
	};

	class ITrSemanticPass : public ITrVisitor
	{
	public:
		ITrSemanticPass(StdStringView name, Context* pCtx);
		virtual ~ITrSemanticPass();

		virtual void Process(ITrModule& mod)= 0;

	protected:
		Timer m_Timer;
		StdStringView m_Name;
		Context* m_pCtx;
	};
	
}
