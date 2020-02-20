#include "utils.hpp"
#include <cstdarg>
#include <fstream>

namespace Noctis
{
	StdString Format(const char* format, ...)
	{
		va_list varList;
		va_start(varList, format);
		StdString res = Format(format, varList);
		va_end(varList);
		return res;
	}

	StdString Format(const char* format, va_list varList)
	{
		char dummybuffer[1];
		va_list varList2;
		va_copy(varList2, varList);
		i64 size = vsnprintf(dummybuffer, 1, format, varList);

		if (size <= 0)
			return "";
		++size;

		StdString res;
		res.resize(size);
		i32 err = vsnprintf_s(res.data(), size, size, format, varList2);
		if (err < 0)
			return "";

		return res;
	}
	
	StdString Format(const StdString format, ...)
	{
		va_list varList;
		va_start(varList, format);
		StdString res = Format(format.c_str(), varList);
		va_end(varList);
		return res;
	}

	StdString Format(const StdString& format, va_list varList)
	{
		return Format(format.c_str(), varList);
	}

	StdString Format(StdStringView format, ...)
	{
		va_list varList;
		va_start(varList, format);
		StdString res = Format(format.data(), varList);
		va_end(varList);
		return res;
	}

	StdString Format(StdStringView format, va_list varList)
	{
		return Format(format.data(), varList);
	}

	bool ReadFileAsString(const StdString& filepath, StdString& content)
	{
		std::ifstream ifs{ filepath.c_str() };
		if (!ifs.is_open())
			return false;

		ifs.seekg(0, std::ios::end);
		usize fileSize = ifs.tellg();

		content.resize(fileSize + 1);
		
		ifs.seekg(0, std::ios::beg);
		ifs.read(content.data(), fileSize);

		ifs.close();
		return true;
	}
}
