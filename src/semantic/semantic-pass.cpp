#include "semantic-pass.hpp"
#include "common/logger.hpp"

namespace Noctis
{
	AstSemanticPass::AstSemanticPass(StdStringView pName, Context* pCtx)
		: AstVisitor()
		, m_Name(pName)
		, m_pCtx(pCtx)
		, m_Timer(true)
	{
	}

	AstSemanticPass::~AstSemanticPass()
	{
		m_Timer.Stop();
		g_Logger.Log("%s took %fms\n", m_Name.data(), m_Timer.GetTimeMS());
	}

	void AstSemanticPass::Process(AstTree& tree)
	{
		Visit(tree);
	}

	ITrSemanticPass::ITrSemanticPass(StdStringView name, Context* pCtx)
		: m_Name(name)
		, m_pCtx(pCtx)
		, m_Timer(true)
	{
	}

	ITrSemanticPass::~ITrSemanticPass()
	{
		m_Timer.Stop();
		g_Logger.Log("%s took %fms\n", m_Name.data(), m_Timer.GetTimeMS());
	}
}
