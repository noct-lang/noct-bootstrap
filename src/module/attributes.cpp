#include "attributes.hpp"

namespace Noctis
{

	DEFINE_ENUM_FLAGS(Attribute);

	StdString ToString(Attribute attribs)
	{
		StdString res;

		if (ENUM_IS_SET(attribs, Attribute::Const))
			res = "const";
		if (ENUM_IS_SET(attribs, Attribute::Immutable))
			res += res.empty() ? "immutable" : " | immutable";
		if (ENUM_IS_SET(attribs, Attribute::Immutable))
			res += res.empty() ? "immutable" : " | immutable";
		
		if (ENUM_IS_SET(attribs, Attribute::Static))
			res += res.empty() ? "static" : " | static";
		if (ENUM_IS_SET(attribs, Attribute::Comptime))
			res += res.empty() ? "comptime" : " | comptime";
		if (ENUM_IS_SET(attribs, Attribute::Lazy))
			res += res.empty() ? "lazy" : " | lazy";
		if (ENUM_IS_SET(attribs, Attribute::Move))
			res += res.empty() ? "move" : " | move";

		return res;
	}
}