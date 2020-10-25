#pragma once

namespace Noctis
{
	struct ILModule;
	struct Context;

	class ILProcessing
	{
	public:
		ILProcessing(Context* pCtx);

		void Process(ILModule& module);

		template<typename T, typename... Args>
		void RunPass(ILModule& module, const Args&... args);

	private:
		Context* m_pCtx;
	};
}
