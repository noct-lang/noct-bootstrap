#pragma once
#include <stdint.h>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <memory>

using i8  = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

using u8  = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using f32 = float;
using f64 = double;

using usize = uintptr_t;
using isize = ptrdiff_t;


template<typename T>
using StdVector = std::pmr::vector<T>;

template<typename T0, typename T1>
using StdPair = std::pair<T0, T1>;

template<typename T0, typename T1>
using StdPairVector = StdVector<StdPair<T0, T1>>;

using StdString = std::pmr::string;
using StdStringView = std::string_view;

template<typename Key, typename Value, typename Hash = std::hash<Key>, typename KeyEq = std::equal_to<Key>>
using StdUnorderedMap = std::pmr::unordered_map<Key, Value, Hash, KeyEq>;

template<typename T>
using StdSharedPtr = std::shared_ptr<T>;
template<typename T>
using StdUniquePtr = std::unique_ptr<T>;

#define DECLARE_ENUM_FLAGS(enumType) \
	enumType operator|(enumType e0, enumType e1);\
	enumType operator^(enumType e0, enumType e1);\
	enumType operator&(enumType e0, enumType e1);\
	enumType& operator|=(enumType& e0, enumType e1);\
	enumType& operator^=(enumType& e0, enumType e1);\
	enumType& operator&=(enumType& e0, enumType e1);

#define DEFINE_ENUM_FLAGS(enumType)\
	enumType operator|(enumType e0, enumType e1) { return enumType( u64(e0) | u64(e1)); } \
	enumType operator^(enumType e0, enumType e1) { return enumType( u64(e0) ^ u64(e1)); } \
	enumType operator&(enumType e0, enumType e1) { return enumType( u64(e0) & u64(e1)); } \
	enumType& operator|=(enumType& e0, enumType e1) { e0 = enumType( u64(e0) | u64(e1)); return e0; } \
	enumType& operator^=(enumType& e0, enumType e1) { e0 = enumType( u64(e0) ^ u64(e1)); return e0; } \
	enumType& operator&=(enumType& e0, enumType e1) { e0 = enumType( u64(e0) & u64(e1)); return e0; }