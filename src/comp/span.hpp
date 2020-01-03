#pragma once
#include "common/defs.hpp"

namespace Noctis
{

	struct Span
	{
		Span(u64 startChar, u64 endChar, u64 line, u64 column);

		const u64 startChar;
		const u64 endChar;
		const u64 line;
		const u64 column;
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

		void SetCurFile(const StdString& filepath);

		void AddSpan(Span span);
		Span GetSpan(u64 idx) const;
		MultiSpan GetSpan(u64 start, u64 end) const;

	private:

		StdUnorderedMap<StdString, StdVector<Span>> m_Spans;
		StdString m_CurFile;
	};
	
}
