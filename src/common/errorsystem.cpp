#include "errorsystem.hpp"
#include "logger.hpp"
#include "utils.hpp"
#include "tokens/span.hpp"

void Noctis::ErrorSystem::Error(u64 line, u64 column, StdStringView text)
{
	g_Logger.SetForeColor(LoggerColor::ForeRed);
	StdString stamp = Format("[%s:%u:%u]", m_CurFile.c_str(), line, column);
	g_Logger.Log(stamp);
	g_Logger.Log(text);
	g_Logger.Log("\n");
	g_Logger.SetForeColor(LoggerColor::ForeGray);
}

void Noctis::ErrorSystem::Error(const Span& span, StdStringView text)
{
	Error(span.line, span.column, text);
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
