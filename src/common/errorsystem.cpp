#include "errorsystem.hpp"
#include "logger.hpp"
#include "utils.hpp"

void Noctis::ErrorSystem::Error(u64 line, u64 column, StdStringView text)
{
	Error(line, column, text.data());
}

void Noctis::ErrorSystem::Error(u64 line, u64 column, const StdString& text)
{
	Error(line, column, text.c_str());
}

void Noctis::ErrorSystem::Error(u64 line, u64 column, const char* text)
{
	g_Logger.SetForeColor(LoggerColor::ForeRed);
	StdString stamp = Format("[%s:%u:%u]", m_CurFile.c_str(), line, column);
	g_Logger.Log(stamp);
	g_Logger.Log(text);
	g_Logger.Log("\n");
	g_Logger.SetForeColor(LoggerColor::ForeLightGray);
}

void Noctis::ErrorSystem::SetCurrentFile(const StdString& file)
{
	m_CurFile = file;
}

Noctis::ErrorSystem& Noctis::GetErrorSystem()
{
	static ErrorSystem errSys;
	return errSys;
}
