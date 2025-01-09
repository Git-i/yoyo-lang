#pragma once
#include "ast_node.h"
#include <string>
#include <vector>
#include <string_view>
#include <utility>
namespace Yoyo
{
	struct YOYO_API SourceView
	{
		std::vector<std::string_view> lines;
		std::string filename;
		SourceView(std::string& source, std::string filename);
	};
	struct SourceSpan
	{
		SourceLocation begin;
		SourceLocation end;
	};
	struct YOYO_API Error
	{
		SourceSpan span;
		std::string summary;
		std::string description;
		std::vector<std::pair<SourceSpan, std::string>> markers;
		Error() = default;
		Error(SourceLocation beg, SourceLocation end, std::string sum, std::string desc = "") :
			span{ beg, end },
			summary(std::move(sum)),
			description(std::move(desc)) {}
		Error(ASTNode* node, std::string sum, std::string desc = "") :
			span{ node->beg, node->end },
			summary(std::move(sum)),
			description(std::move(desc)) {}
		std::string to_string(const SourceView& view, bool enable_color) const;
		std::string to_string_ascii(const SourceView& view);
	};
}