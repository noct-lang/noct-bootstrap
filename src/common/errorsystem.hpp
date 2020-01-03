#pragma once
#include "defs.hpp"
#include "utils.hpp"

namespace Noctis
{

	class ErrorSystem
	{
	public:


		void Error(u64 line, u64 column, StdStringView text);
		template<typename ...Args>
		void Error(u64 line, u64 column, StdStringView format, Args&... args)
		{
			StdString text = Format(format, args...);
			Error(line, column, text);
		}
		
		void Error(u64 line, u64 column, const StdString& format);
		template<typename ...Args>
		void Error(u64 line, u64 column, const StdString& format, Args&... args)
		{
			StdString text = Format(format, args...);
			Error(line, column, text);
		}
		
		void Error(u64 line, u64 column, const char* format);
		template<typename ...Args>
		void Error(u64 line, u64 column, const char* format, Args&... args)
		{
			StdString text = Format(format, args...);
			Error(line, column, text);
		}

		void SetCurrentFile(const StdString& file);
		
	private:
		StdString m_CurFile;
		
	};

	ErrorSystem& GetErrorSystem();
	
}

#define g_ErrorSystem ::Noctis::GetErrorSystem()
