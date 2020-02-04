#include "semanticpass.hpp"
#include "common/logger.hpp"

namespace Noctis
{
	SemanticPass::SemanticPass(const char* pName, Context* pCtx)
		: AstVisitor()
		, m_pName(pName)
		, m_pCtx(pCtx)
		, m_Timer(true)
	{
	}

	SemanticPass::~SemanticPass()
	{
		m_Timer.Stop();
		g_Logger.Log("%s took %fms\n", m_pName, m_Timer.GetTimeMS());
	}
}
