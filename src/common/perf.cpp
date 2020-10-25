#include "perf.hpp"

#include "utils.hpp"

namespace Noctis
{
	Timer::Timer(bool start)
		: m_StartTime(std::chrono::high_resolution_clock::now())
	{
	}

	void Timer::Start()
	{
		m_StartTime = std::chrono::high_resolution_clock::now();
		m_StopTime = std::chrono::time_point<std::chrono::high_resolution_clock>::min();
	}

	void Timer::Stop()
	{
		m_StopTime = std::chrono::high_resolution_clock::now();
	}

	double Timer::GetTimeMS()
	{
		std::chrono::duration<double, std::milli> dur = m_StopTime - m_StartTime;
		return dur.count();
	}

	double Timer::GetTimeUS()
	{
		std::chrono::duration<double, std::micro> dur = m_StopTime - m_StartTime;
		return dur.count();
	}

	double Timer::GetTimeNS()
	{
		std::chrono::duration<double, std::nano> dur = m_StopTime - m_StartTime;
		return dur.count();
	}

	StdString Timer::GetSMSFormat()
	{
		double time = GetTimeMS();
		u32 seconds = u32(time / 1000);
		double ms = time - seconds * 1000;
		return Format("%us %07.3fms", seconds, ms);
	}
}
