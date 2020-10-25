#pragma once
#include "common/perf.hpp"
#include "il/il-visitor.hpp"


namespace Noctis
{
	struct ILModule;

	class ILPass : public ILVisitor
	{
	public:
		ILPass(const char* pName, Context* pCtx);
		~ILPass();

		virtual void Process(ILModule& mod) = 0;

	protected:
		StdStringView m_Name;
		Timer m_Timer;
	};
	
}
