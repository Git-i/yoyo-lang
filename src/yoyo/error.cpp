#include "error.h"
#include <ranges>
#include <span>
#include <algorithm>
namespace Yoyo
{
	void replaceCharWithStr(std::string& str, char ch, const std::string& repl)
	{
		size_t start_pos = 0;
		while ((start_pos = str.find(ch, start_pos)) != std::string::npos)
		{
			str.replace(start_pos, 1, repl);
			start_pos += repl.length();
		}
	}
	std::string markers_for(size_t line, std::span<std::pair<SourceSpan, std::string>> markers, const SourceView& view)
	{
		using namespace std::views;
		using namespace std::ranges;
		auto ends_here = [line](const std::pair<SourceSpan, std::string>& sp) {
			return sp.first.end.line == line;
			};
		std::vector<std::string> rows;
		auto line_size = view.lines[line].size();
		rows.emplace_back(line_size, ' ');
		for (auto& marker : markers | filter(ends_here) | views::reverse)
		{
			auto& span = marker.first;
			size_t col_begin = span.begin.line == line ? span.begin.column : 1;
			size_t col_end = span.end.column;
			// $ will be replaced with hyper horizontal line
			// £ will be replaced with the T thing
			// ¬ will be replaced with the hyper vertical bar
			// ¦ will be replaced with the L thing
			rows.front().replace(col_begin - 1, col_end - col_begin, std::string(col_end - col_begin, '$'));
			size_t bar_position = col_begin + (col_end - col_begin - 2) / 2;
			rows.front()[bar_position] = char{ 5 };
			for (auto& line : subrange(rows.begin() + 1, rows.end()))
				line[bar_position] = char{ 6 };
			rows.emplace_back(' ', line_size);
			rows.back()[bar_position] = char{ 7 };
			rows.back()[bar_position + 1] = '$';
			rows.back().insert(bar_position + 3, marker.second);
		}
		std::string final_str;
		for (auto& row : rows)
		{
			replaceCharWithStr(row, '$', "─");
			replaceCharWithStr(row, char{ 5 }, "┬");
			replaceCharWithStr(row, char{ 6 }, "│");
			replaceCharWithStr(row, char{ 7 }, "╰");
			final_str.append(row + "\n");
		}
		return final_str;
	}
	std::string Error::to_string(const SourceView& view)
	{
		std::vector<std::string> lines;
		for (auto line : std::views::iota(span.begin.line, span.end.line))
			lines.emplace_back(view.lines[line]);
	}
}