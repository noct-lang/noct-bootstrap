#include "il-processing.hpp"
#include <type_traits>


#include "common/logger.hpp"
#include "il-pass.hpp"
#include "passes/misc/il-dependency-pass.hpp"

namespace Noctis
{
	template<typename T, typename... Args>
	void ILProcessing::RunPass(ILModule& mod, const Args&... args)
	{
		static_assert(std::is_base_of_v<ILPass, T>, "");
		T pass{ m_pCtx, args... };
		pass.Process(mod);
	}
	
	ILProcessing::ILProcessing(Context* pCtx)
		: m_pCtx(pCtx)
	{
	}

	void ILProcessing::Process(ILModule& module)
	{
		g_Logger.Log("-- IL PROCESSING\n");
		Timer timer(true);
		
		RunPass<ILDependencyPass>(module);

		timer.Stop();
		g_Logger.Log("-- TOOK %s\n", timer.GetSMSFormat().c_str());
	}
}
