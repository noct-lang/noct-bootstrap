#pragma once

namespace Noctis
{
	struct Context;
	
	class DependencyGraph
	{
	public:
		DependencyGraph(Noctis::Context* pCtx);

	private:

		Context* m_pCtx;
	};

	
}
