#include "il-pass.hpp"

#include "common/logger.hpp"

namespace Noctis
{
	ILPass::ILPass(const char* pName, Context* pCtx)
		: ILVisitor(pCtx)
		, m_Name(pName)
		, m_Timer(true)
	{
	}

	ILPass::~ILPass()
	{
		m_Timer.Stop();
		double timeMS = m_Timer.GetTimeMS();
		g_Logger.Log("%16s : %s\n", m_Timer.GetSMSFormat().c_str(), m_Name.data());
	}
}
