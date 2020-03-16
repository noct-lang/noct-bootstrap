#include "semantic-pass.hpp"
#include "common/logger.hpp"

namespace Noctis
{
	AstSemanticPass::AstSemanticPass(const char* pName, Context* pCtx)
		: AstVisitor()
		, m_pName(pName)
		, m_pCtx(pCtx)
		, m_Timer(true)
	{
	}

	AstSemanticPass::~AstSemanticPass()
	{
		m_Timer.Stop();
		g_Logger.Log("%s took %fms\n", m_pName, m_Timer.GetTimeMS());
	}

	void AstSemanticPass::Process(AstTree& tree)
	{
		Visit(tree);
	}
}
