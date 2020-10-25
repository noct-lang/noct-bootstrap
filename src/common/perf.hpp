#pragma once
#include "defs.hpp"
#include <chrono>

namespace Noctis
{

	class Timer
	{
	public:
		Timer(bool start);

		void Start();
		void Stop();

		double GetTimeMS();
		double GetTimeUS();
		double GetTimeNS();

		StdString GetSMSFormat();
		
	private:
		std::chrono::time_point<std::chrono::high_resolution_clock> m_StartTime;
		std::chrono::time_point<std::chrono::high_resolution_clock> m_StopTime;
	};
	
}
