#pragma once
#include "AST/ast-visitor.hpp"
#include "common/perf.hpp"

namespace Noctis
{
	struct Context;
	
	class SemanticPass : public AstVisitor
	{
	public:
		SemanticPass(const char* pName, Context* pCtx);
		virtual ~SemanticPass();

		virtual void Process(AstTree& tree);

	protected:
		Timer m_Timer;
		const char* m_pName;
		Context* m_pCtx;
	};
	
}
