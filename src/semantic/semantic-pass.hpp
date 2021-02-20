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
		AstSemanticPass(StdStringView pName);
		virtual ~AstSemanticPass();

		virtual void Process(AstTree& tree);

	protected:
		Timer m_Timer;
		StdStringView m_Name;
	};
	
	class ITrSemanticPass : public ITrVisitor
	{
	public:
		ITrSemanticPass(StdStringView name);
		virtual ~ITrSemanticPass();

		virtual void Process(ITrModule& mod) = 0;

	protected:
		Timer m_Timer;
		StdStringView m_Name;
	};
	
}
