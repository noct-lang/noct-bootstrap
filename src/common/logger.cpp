#include "logger.hpp"
#include <iostream>
#include "utils.hpp"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include "Windows.h"
#endif

namespace Noctis
{
	DEFINE_ENUM_FLAGS(LoggerColor)
	
	Logger::Logger()
		: m_CanWriteToStdOut(true)
		, m_CanWriteToFile(true)
		, m_HasFile(false)
		, m_Color(LoggerColor::BackBlack | LoggerColor::ForeLightGray)
	{
#ifdef _WIN32
		m_ConsoleHandle = GetStdHandle(STD_OUTPUT_HANDLE);
#endif
	}

	void Logger::Log(StdStringView text)
	{
		LogStdOut(text);
		LogFile(text);
	}

	void Logger::Log(const StdString& text)
	{
		LogStdOut(text);
		LogFile(text);
	}

	void Logger::Log(const char* text)
	{
		LogStdOut(text);
		LogFile(text);
	}

	void Logger::LogStdOut(StdStringView text)
	{
		if (m_CanWriteToStdOut)
		{
			std::cout << text;
		}
	}

	void Logger::LogStdOut(const StdString& text)
	{
		LogStdOut(StdStringView(text));
	}

	void Logger::LogStdOut(const char* text)
	{
		LogStdOut(StdStringView(text));
	}

	void Logger::LogFile(StdStringView text)
	{
		if (m_CanWriteToFile && m_HasFile)
		{
			m_OutFile << text;
		}
	}

	void Logger::LogFile(const StdString& text)
	{
		LogFile(StdStringView(text));
	}

	void Logger::LogFile(const char* text)
	{
		LogFile(StdStringView(text));
	}

	void Logger::SetColor(LoggerColor color)
	{
#ifdef _WIN32
		SetConsoleTextAttribute(m_ConsoleHandle, WORD(color));
#else
		// TODO: Non-windows console color
#endif
	}

	void Logger::SetForeColor(LoggerColor color)
	{
#ifdef _WIN32
		color |= m_Color & LoggerColor::BackWhite;
		SetColor(color);
#else
		// TODO: Non-windows console color
#endif
	}

	void Logger::SetBackColor(LoggerColor color)
	{
#ifdef _WIN32
		color |= m_Color & LoggerColor::ForeWhite;
		SetColor(color);
#else
		// TODO: Non-windows console color
#endif
	}

	void Logger::SetOutFile(const StdString& filepath)
	{
		m_OutFile = std::ofstream{ filepath.c_str() };
		m_HasFile = true;
	}

	Logger& GetLogger()
	{
		static Logger logger;
		return logger;
	}
}
