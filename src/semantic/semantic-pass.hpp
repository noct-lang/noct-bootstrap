#pragma once
#include "ast/ast-visitor.hpp"
#include "common/perf.hpp"

namespace Noctis
{
	struct Context;
	
	class AstSemanticPass : public AstVisitor
	{
	public:
		AstSemanticPass(const char* pName, Context* pCtx);
		virtual ~AstSemanticPass();

		virtual void Process(AstTree& tree);

	protected:
		Timer m_Timer;
		const char* m_pName;
		Context* m_pCtx;
	};
	
}
