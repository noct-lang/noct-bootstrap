#pragma once
#include "defs.hpp"

namespace Noctis
{
	template<typename ...Args>
	StdString Format(const char* format, Args... args)
	{
		char dummybuffer[1];
		i64 size = snprintf(dummybuffer, 1, format, args...);

		if (size <= 0)
			return "";
		++size;

		StdString res;
		res.resize(size);
		i32 err = snprintf(res.data(), size, format, args...);
		if (err < 0)
			return "";

		return res;
	}

	template<typename ...Args>
	StdString Format(StdStringView format, Args... args)
	{
		return Format(format.data(), args...);
	}

	template<typename ...Args>
	StdString Format(const StdString& format, Args... args)
	{
		return Format(format.c_str(), args...);
	}

	StdString ReadFileAsString(const StdString& filepath);
	
}
