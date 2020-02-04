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

	void SpanManager::SetCurFile(const StdString& filepath)
	{
		m_CurFile = filepath;
	}

	void SpanManager::AddSpan(Span span)
	{
		auto it = m_Spans.find(m_CurFile);
		if (it == m_Spans.end())
			it = m_Spans.insert(std::pair{ m_CurFile, StdVector<Span>{} }).first;

		it->second.push_back(span);
	}

	Span SpanManager::GetSpan(u64 idx) const
	{
		auto it = m_Spans.find(m_CurFile);
		if (it == m_Spans.end() || idx >= it->second.size())
			return Span{ u64(-1), u64(-1), u64(-1), u64(-1) };
		return it->second[idx];
	}

	MultiSpan SpanManager::GetSpan(u64 start, u64 end) const
	{
		Span startSpan = GetSpan(start);
		Span endSpan = GetSpan(end);
		return MultiSpan{ std::move(startSpan), std::move(endSpan) };
	}
}
