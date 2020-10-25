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

	void StringReplace(StdString& str, const StdStringView toReplace, const StdStringView with);

	StdString ExtractNullTermString(const StdVector<u8>& data, usize& idx);

	StdVector<StdString> SplitString(const StdString& str, char splitOn);

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

	template<typename T>
	bool operator==(const StdWeakPtr<T>& first, const StdWeakPtr<T>& second)
	{
		return !first.expired() && !second.expired() && first.lock() == second.lock();
	}

	template<typename T>
	bool AddUnique(StdVector<T>& vec, const T& elem)
	{
		auto it = std::find(vec.begin(), vec.end(), elem);
		if (it == vec.end())
		{
			vec.emplace_back(elem);
			return true;
		}
		return false;
	}

	template<typename T0, typename T1>
	bool AddUniquePair(StdPairVector<T0, T1>& vec, const T0& first, const T1& second)
	{
		auto it = std::find_if(vec.begin(), vec.end(), [&first, &second](const StdPair<T0, T1>& pair)
		{
			return pair.first == first && pair.second == second;
		});
		if (it == vec.end())
		{
			vec.emplace_back(first, second);
			return true;
		}
		return false;
	}
	
}
