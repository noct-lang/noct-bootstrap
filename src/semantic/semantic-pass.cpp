#include "semantic-pass.hpp"
#include "common/logger.hpp"
#include "itr/itr.hpp"

namespace Noctis
{
	AstSemanticPass::AstSemanticPass(StdStringView pName)
		: AstVisitor()
		, m_Name(pName)
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

	ITrSemanticPass::ITrSemanticPass(StdStringView name)
		: m_Name(name)
		, m_Timer(true)
	{
	}

	ITrSemanticPass::~ITrSemanticPass()
	{
		m_Timer.Stop();
		g_Logger.Log("%16s : %s\n", m_Timer.GetSMSFormat().c_str(), m_Name.data());
	}
}
