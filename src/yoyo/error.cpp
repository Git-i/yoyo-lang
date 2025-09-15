#include "error.h"
#include <ranges>
#include <span>
#include <algorithm>
#include <format>
namespace Yoyo
{
	SourceView::SourceView(std::string& source, std::string filename) : filename(std::move(filename))
	{
		using namespace std::views;
		using std::string_view;
		for (auto line : split(string_view(source), string_view("\n")))
			lines.emplace_back(line.begin(), line.end());
	}
	void replaceCharWithStr(std::string& str, char ch, const std::string& repl)
	{
		size_t start_pos = 0;
		while ((start_pos = str.find(ch, start_pos)) != std::string::npos)
		{
			str.replace(start_pos, 1, repl);
			start_pos += repl.length();
		}
	}
	std::string markers_for(std::string prefix, bool color, size_t line, std::span<const std::pair<SourceSpan, std::string>> markers, const SourceView& view)
	{
		using namespace std::views;
		using namespace std::ranges;
		auto ends_here = [line](const std::pair<SourceSpan, std::string>& sp) {
			return sp.first.end.line == line;
			};
		auto begins_but_not_ends = [line](const std::pair<SourceSpan, std::string>& sp) {
			return sp.first.begin.line == line && sp.first.end.line != line;
			};
		std::vector<std::string> rows;
		auto line_size = view.lines[line - 1].size();
		rows.emplace_back(line_size, ' ');
		bool consider = false;
		for (auto& marker : markers | filter(ends_here) | views::reverse)
		{
			consider = true;
			auto& span = marker.first;
			size_t col_begin = span.begin.line == line ? span.begin.column : 1;//view.lines[line - 1].find_first_not_of(' ') + 1;
			size_t col_end = span.end.column;
			// $ will be replaced with hyper horizontal line
			// char{5} will be replaced with the T thing
			// char{6} will be replaced with the hyper vertical bar
			// char{7} will be replaced with the L thing
			rows.front().replace(col_begin - 1, col_end - col_begin, std::string(col_end - col_begin, '$'));
			size_t bar_position = col_end - col_begin == 1 ? col_begin - 1 : col_begin + (col_end - col_begin - 2) / 2;
			rows.front()[bar_position] = char{ 5 };
			for (auto& line : subrange(rows.begin() + 1, rows.end()))
				line[bar_position] = char{ 6 };
			rows.emplace_back(line_size + 3, ' ');
			rows.back()[bar_position] = char{ 7 };
			rows.back()[bar_position + 1] = '$';
			if(color) rows.back().insert(bar_position + 3, std::format("\033[1;35m{}\033[0m", marker.second));
			else rows.back().insert(bar_position + 3,  marker.second);
		}
		for (auto& span : markers | filter(begins_but_not_ends) | keys)
		{
			consider = true;
			size_t col_begin = span.begin.column;
			rows.front().replace(col_begin - 1, line_size - col_begin, std::string(line_size - col_begin, '$'));
		}
		std::string final_str;
		if(consider)
		for (auto& row : rows)
		{
			replaceCharWithStr(row, '$', "─");
			replaceCharWithStr(row, char{ 5 }, "┬");
			replaceCharWithStr(row, char{ 6 }, "│");
			replaceCharWithStr(row, char{ 7 }, "╰");
			final_str.append(prefix + row + "\n");
		}
		return final_str;
	}
	std::string Error::to_string(const SourceView& view, bool enable_color) const
	{
		std::vector<std::string> lines;
		auto result = std::format("{}{}:{}:{} error: {}{}\n",
			enable_color ? "\033[1;31m" : "",
			view.filename,
			span.begin.line,
			span.begin.column,
			summary,
			enable_color ? "\033[0m" : "");
		auto render_line = [this, &view, enable_color, &lines](size_t line) {
			std::string line_body(view.lines[line - 1]);
			//apply color for the portion of text that is in error
			if (enable_color && span.end.line >= line && span.begin.line <= line)
			{
				size_t col_begin = span.begin.line == line ? span.begin.column - 1 : 0;
				size_t col_end = span.end.line == line ? span.end.column - 1 : line_body.size();
				std::string_view red = "\033[1;31m";
				std::string_view reset = "\033[0m";
				line_body.insert(col_begin, red);
				line_body.insert(col_end + red.size(), reset);
			}
			lines.emplace_back(std::format("{: >4} │ {}\n", line, line_body));
			lines.emplace_back(markers_for("       ", enable_color, line, markers, view));
			};
		size_t begin = std::max(size_t(1), span.begin.line - 1);
		size_t end = std::min(view.lines.size() + 1, span.end.line + 2);
		std::vector<size_t> rendered_lines;
		for (auto line : std::views::iota(begin, end))
		{
			rendered_lines.push_back(line);
			render_line(line);
		}

		for (auto& marker : markers) {
			if (auto it = std::ranges::find(rendered_lines, marker.first.begin.line); it != rendered_lines.end())
				continue;
			std::string_view red = enable_color ? "\033[1;31m" : "";
			std::string reset = enable_color ? "\033[0m\n" : "\n";
			lines.emplace_back("\n" + 
				std::string(red) + 
				std::string(result.size() - (enable_color ? red.size() + reset.size() : 0), '=') +
				reset);
			
			for (auto line : std::views::iota(marker.first.begin.line, marker.first.end.line + 1)) {
				rendered_lines.push_back(line);
				render_line(line);
			}
		}
		
		for (const auto& line : lines)
			result.append(line);
		result.append(description);
		return result;
	}
}