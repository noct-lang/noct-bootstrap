#pragma once
#include "common/defs.hpp"

namespace Noctis
{
	enum class Visibility : u8
	{
		Private,
		Module,
		Package,
		Public,
		Dynlib,
	};

	enum class Attribute : u8
	{
		None = 0x00,
		
		Const = 0x01,
		Immutable = 0x02,
		
		Static = 0x10,
		Comptime = 0x20,
		Lazy = 0x40,
		Move = 0x80,
	};
	DECLARE_ENUM_FLAGS(Attribute);

	StdString ToString(Attribute attribs);
	
	
}
