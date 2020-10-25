#include "semantic-pass.hpp"
#include "common/logger.hpp"
#include "itr/itr.hpp"

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
		g_Logger.Log("%16s : %s\n", m_Timer.GetSMSFormat().c_str(), m_Name.data());
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
		double timeMS = m_Timer.GetTimeMS();
		g_Logger.Log("%16s : %s\n", m_Timer.GetSMSFormat().c_str(), m_Name.data());
	}
}
