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

		StdString res;
		res.resize(size);
		i32 err = snprintf(res.data(), size + 1, format, args...);
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

	bool ReadFileAsString(const StdString& filepath, StdString& content);

	// Calculated using Hamming weight
	template<typename T>
	u8 CountBits(T& val)
	{
		static_assert(std::is_integral_v<T> || std::is_enum_v<T>, "Can only count bits for integer and enum types");

		constexpr u64 m1 = 0x5555555555555555;
		constexpr u64 m2 = 0x3333333333333333;
		constexpr u64 m4 = 0x0F0F0F0F0F0F0F0F;

		u64 uval = u64(val);
		uval -= (uval >> 1) & m1;                 // put count of each 2 bits into those 2 bits
		uval =  (uval & m2) + ((uval >> 2) & m2); // put count of each 4 bits into those 4 bits
		uval =  (uval + (uval >> 4)) & m4;        // put count of each 8 bits into those 8 bits
		uval += uval >> 8;                        // put count of each 16 bits into the lowest 8 bits
		uval += uval >> 16;                       // put count of each 32 bits into the lowest 8 bits
		uval += uval >> 32;                       // put count of each 64 bits into the lowest 8 bits
		return uval & 0x7f;	                      // mask out count
	}
	
}
