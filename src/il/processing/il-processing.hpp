#pragma once

namespace Noctis
{
	struct ILModule;
	struct Context;

	class ILProcessing
	{
	public:
		void Process(ILModule& module);

		template<typename T, typename... Args>
		void RunPass(ILModule& module, const Args&... args);
	};
}
