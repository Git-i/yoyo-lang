#pragma once
#include "ast_node.h"
#include <string>
#include <vector>
#include <string_view>
#include <utility>
namespace Yoyo
{
	struct SourceView
	{
		std::vector<std::string_view> lines;
	};
	struct SourceSpan
	{
		SourceLocation begin;
		SourceLocation end;
	};
	struct Error
	{
		SourceSpan span;
		std::string summary;
		std::string description;
		std::vector<std::pair<SourceSpan, std::string>> markers;
		Error() = default;
		Error(SourceLocation beg, SourceLocation end, std::string sum, std::string desc = "") :
			span(beg, end),
			summary(std::move(sum)),
			description(std::move(desc)) {}
		Error(ASTNode* node, std::string sum, std::string desc = "") :
			begin(node->beg), 
			end(node->end), 
			summary(std::move(sum)),
			description(std::move(desc)) {}
		std::string to_string(const SourceView& view);
		std::string to_string_ascii(const SourceView& view);
	};
}