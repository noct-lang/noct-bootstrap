#include "span.hpp"

namespace Noctis
{
	Span::Span(u64 startChar, u64 endChar, u64 line, u64 column)
		: startChar(startChar)
		, endChar(endChar)
		, line(line)
		, column(column)
	{
	}

	MultiSpan::MultiSpan(Span start, Span end)
		: start(start)
		, end(end)
	{
	}

	SpanManager::SpanManager()
	{
	}

	SpanId SpanManager::AddSpan(const StdString& filepath, Span span)
	{
		auto it = m_FileNames.insert(filepath).first;
		span.filePath = StdStringView{ *it };
		SpanId id = SpanId(m_Spans.size());
		m_Spans.push_back(std::move(span));
		return id;
	}
	Span SpanManager::GetSpan(u64 id) const
	{
		if (id >= m_Spans.size())
			return Span(u64(-1), u64(-1), 0, 0);
		return m_Spans[id];
	}

	MultiSpan SpanManager::GetSpan(u64 startId, u64 endId) const
	{
		Span startSpan = GetSpan(startId);
		Span endSpan = GetSpan(endId);
		return MultiSpan{ std::move(startSpan), std::move(endSpan) };
	}
}
