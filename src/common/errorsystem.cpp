#include "errorsystem.hpp"
#include "logger.hpp"

void Noctis::ErrorSystem::Error(StdStringView text)
{
	g_Logger.SetForeColor(LoggerColor::ForeRed);
	g_Logger.Log("[%s] error:", m_CurFile.c_str());
	g_Logger.Log(text);
	g_Logger.SetForeColor(LoggerColor::ForeGray);
}

void Noctis::ErrorSystem::Warning(StdStringView text)
{
	g_Logger.SetForeColor(LoggerColor::ForeYellow);
	g_Logger.Log("[%s] warning:", m_CurFile.c_str());
	g_Logger.Log(text);
	g_Logger.SetForeColor(LoggerColor::ForeGray);
}

void Noctis::ErrorSystem::SetCurrentFile(const StdString& file)
{
	m_CurFile = file;
}

void Noctis::ErrorSystem::LogError(StdStringView filePath, u64 line, u64 column, StdStringView text)
{
	g_Logger.SetForeColor(LoggerColor::ForeRed);
	g_Logger.Log("[%s:%u:%u] error:", filePath.empty() ? m_CurFile.c_str() : filePath.data(), line, column);
	g_Logger.Log(text);
	g_Logger.Log("\n");
	g_Logger.SetForeColor(LoggerColor::ForeGray);
}

void Noctis::ErrorSystem::LogWarning(StdStringView filePath, u64 line, u64 column, StdStringView text)
{
	g_Logger.SetForeColor(LoggerColor::ForeYellow);
	g_Logger.Log("[%s:%u:%u] warning:", filePath.empty() ? m_CurFile.c_str() : filePath.data(), line, column);
	g_Logger.Log(text);
	g_Logger.Log("\n");
	g_Logger.SetForeColor(LoggerColor::ForeGray);
}

Noctis::ErrorSystem& Noctis::GetErrorSystem()
{
	static ErrorSystem errSys;
	return errSys;
}
