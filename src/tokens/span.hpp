#pragma once
#include "common/defs.hpp"

namespace Noctis
{
	using SpanId = u64;

	struct Span
	{
		Span(u64 startChar, u64 endChar, u64 line, u64 column);

		const u64 startChar;
		const u64 endChar;
		const u64 line;
		const u64 column;
		StdStringView filePath;
	};

	struct MultiSpan
	{
		MultiSpan(Span start, Span end);
		
		const Span start;
		const Span end;
	};

	class SpanManager
	{
	public:

		SpanManager();

		SpanId AddSpan(const StdString& filepath, Span span);
		Span GetSpan(u64 id) const;
		MultiSpan GetSpan(u64 startId, u64 endId) const;

	private:

		StdUnorderedSet<StdString> m_FileNames;
		StdVector<Span> m_Spans;
		StdString m_CurFile;
	};
	
}
