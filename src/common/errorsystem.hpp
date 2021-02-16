#pragma once
#include "defs.hpp"
#include "tokens/span.hpp"
#include "utils.hpp"

namespace Noctis
{
	struct Span;

	class ErrorSystem
	{
	public:

		void Error(StdStringView text);
		void Warning(StdStringView text);
		
		template<typename ...Args>
		void Error(StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			Error(text);
		}

		// Line and column
		template<typename ...Args>
		void Error(u64 line, u64 column, StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			LogError(StdStringView{}, line, column, text);
		}

		// Span
		template<typename ...Args>
		void Error(const Span& span, StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			LogError(span.filePath, span.line, span.column, text);
		}

		template<typename ...Args>
		void Warning(StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			Error(text);
		}

		// Line and column
		template<typename ...Args>
		void Warning(u64 line, u64 column, StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			LogWarning(StdStringView{}, line, column, text);
		}

		// Span
		template<typename ...Args>
		void Warning(const Span& span, StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			LogWarning(span.filePath, span.line, span.column, text);
		}

		// MultiSpan
		template<typename ...Args>
		void Error(const MultiSpan& span, StdStringView format, const Args&... args)
		{
			StdString text = Format(format, args...);
			const Span& tmp = span.start;
			LogWarning(tmp.filePath, tmp.line, tmp.column, text);
		}

		void SetCurrentFile(const StdString& file);
		
	private:
		void LogError(StdStringView filePath, u64 line, u64 column, StdStringView text);
		void LogWarning(StdStringView filePath, u64 line, u64 column, StdStringView text);
		
		StdString m_CurFile;
		
	};

	ErrorSystem& GetErrorSystem();
	
}

#define g_ErrorSystem ::Noctis::GetErrorSystem()
