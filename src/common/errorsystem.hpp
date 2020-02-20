#pragma once
#include "defs.hpp"
#include "utils.hpp"

namespace Noctis
{
	struct Span;

	class ErrorSystem
	{
	public:

		void Error(StdStringView text);
		template<typename ...Args>
		void Error(StdStringView format, Args&... args)
		{
			StdString text = Format(format, args...);
			Error(text);
		}

		// Line and column
		void Error(u64 line, u64 column, StdStringView text);
		template<typename ...Args>
		void Error(u64 line, u64 column, StdStringView format, Args&... args)
		{
			StdString text = Format(format, args...);
			Error(line, column, text);
		}

		// Span
		void Error(const Span& span, StdStringView text);
		template<typename ...Args>
		void Error(const Span& span, StdStringView format, Args&... args)
		{
			StdString text = Format(format, args...);
			Error(span, text);
		}

		void SetCurrentFile(const StdString& file);
		
	private:
		StdString m_CurFile;
		
	};

	ErrorSystem& GetErrorSystem();
	
}

#define g_ErrorSystem ::Noctis::GetErrorSystem()
