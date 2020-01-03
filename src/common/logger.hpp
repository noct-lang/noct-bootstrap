#pragma once
#include <fstream>
#include "defs.hpp"
#include "utils.hpp"

namespace Noctis
{
	enum class LoggerColor
	{
		ForeBlack = 0x00,
		ForeDarkBlue = 0x01,
		ForeDarkGreen = 0x02,
		ForeDarkCyan = 0x03,
		ForeDarkRed = 0x04,
		ForeDarkMagenta = 0x05,
		ForeDarkYellow = 0x06,
		ForeGray = 0x07,
		ForeLightGray = 0x08,
		ForeBlue = 0x09,
		ForeGreen = 0x0A,
		ForeCyan = 0x0B,
		ForeRed = 0x0C,
		ForeMagenta = 0x0D,
		ForeYellow = 0x0E,
		ForeWhite = 0x0F,
		BackBlack = 0x00,
		BackDarkBlue = 0x10,
		BackDarkGreen = 0x20,
		BackDarkCyan = 0x30,
		BackDarkRed = 0x40,
		BackDarkMagenta = 0x50,
		BackDarkYellow = 0x60,
		BackGray = 0x70,
		BackLightGray = 0x80,
		BackBlue = 0x90,
		BackGreen = 0xA0,
		BackCyan = 0xB0,
		BackRed = 0xC0,
		BackMagenta = 0xD0,
		BackYellow = 0xE0,
		BackWhite = 0xF0
	};
	DECLARE_ENUM_FLAGS(LoggerColor);
	
	class Logger
	{
	public:
		Logger();

		void Log(StdStringView text);
		void Log(const StdString& text);
		void Log(const char* text);

		template<typename ...T>
		void Log(StdStringView format, T... args)
		{
			StdString text = Format(format, args...);
		}
		
		template<typename ...T>
		void Log(const StdString& format, T... args)
		{
			StdString text = Format(format, args...);
		}
		
		template<typename ...T>
		void Log(const char* format, T... args)
		{
			StdString text = Format(format, args...);
		}
		
		void LogStdOut(StdStringView text);
		void LogStdOut(const StdString& text);
		void LogStdOut(const char* text);
		
		void LogFile(StdStringView text);
		void LogFile(const StdString& text);
		void LogFile(const char* text);

		void SetColor(LoggerColor color);
		void SetForeColor(LoggerColor color);
		void SetBackColor(LoggerColor color);

		void SetOutFile(const StdString& filepath);

		void SetCanWriteToStdOut(bool canWrite) { m_CanWriteToStdOut = canWrite; };
		void SetCanWriteToFile(bool canWrite) { m_CanWriteToFile = canWrite; };

	private:

		bool m_CanWriteToStdOut : 1;
		bool m_CanWriteToFile : 1;
		bool m_HasFile : 1;

		std::ofstream m_OutFile;

		LoggerColor m_Color;

#ifdef _WIN32
		void* m_ConsoleHandle;
#else
		
#endif
	};

	Logger& GetLogger();
	
}

#define g_Logger ::Noctis::GetLogger()