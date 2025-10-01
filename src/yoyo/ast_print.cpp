#include "ast_print.h"
#include <format>
#include <iostream>
namespace Yoyo {
	std::string sig_to_str(FunctionSignature& sig);
	void ASTPrinter::operator()(VariableDeclaration* decl)
	{
		stream << std::format("{}[variable| name: {}, type: {}]\n", prefix, decl->identifier.text, decl->type->pretty_name(""));
		if (decl->initializer) {
			prefix += "    ";
			std::visit(*this, decl->initializer->toVariant());
			prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
		}
	}
	void ASTPrinter::operator()(IfExpression* stat)
	{
		stream << std::format("{}[if type: {}]\n", prefix, stat->evaluated_type.pretty_name(""));
		prefix += "    ";
		std::visit(*this, stat->condition->toVariant());
		std::visit(*this, stat->then_expr->toVariant());
		if (stat->else_expr) std::visit(*this, stat->else_expr->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(WhileStatement* stat)
	{
		stream << std::format("{}[while]\n", prefix);
		prefix += "    ";
		std::visit(*this, stat->condition->toVariant());
		std::visit(*this, stat->body->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(ForStatement* stat)
	{
		stream << std::format("{}[for| iter = {}]\n", prefix, stat->names[0].text);
		prefix += "    ";
		std::visit(*this, stat->iterable->toVariant());
		std::visit(*this, stat->body->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(BlockExpression* stats)
	{
		stream << std::format("{}[block type: {}]\n", prefix, stats->evaluated_type.pretty_name(""));
		prefix += "    ";
		for (auto& stat : stats->statements) {
			std::visit(*this, stat->toVariant());
		}
		if (stats->expr) std::visit(*this, stats->expr->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(ReturnStatement* stat)
	{
		stream << std::format("{}[return]\n", prefix);
		if (stat->expression) {
			prefix += "    ";
			std::visit(*this, stat->expression->toVariant());
			prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
		}
	}

	void ASTPrinter::operator()(ExpressionStatement* stat)
	{
		stream << std::format("{}[expression statement]\n", prefix);
		prefix += "    ";
		std::visit(*this, stat->expression->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(ConditionalExtraction* conde)
	{
		stream << std::format("{}[conditional extraction, into: {}]\n", prefix, conde->captured_name);
		prefix += "    ";
		std::visit(*this, conde->body->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
		if (conde->else_body) {
			stream << std::format("{}[else{}\n", prefix,
				conde->else_capture.empty() ?
				std::string("]") : std::format(", into: {}]", conde->else_capture));
			prefix += "    ";
			std::visit(*this, conde->else_body->toVariant());
			prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
		}
	}

	void ASTPrinter::operator()(WithStatement* stat)
	{
		stream << std::format("{}[with statement| capture: {}]\n", prefix, stat->name);
		prefix += "    ";
		std::visit(*this, stat->expression->toVariant());
		std::visit(*this, stat->body->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(BreakStatement*)
	{
		stream << std::format("{}[break]\n", prefix);
	}

	void ASTPrinter::operator()(ContinueStatement*)
	{
		stream << std::format("{}[continue]\n", prefix);
	}

	void ASTPrinter::operator()(IntegerLiteral* lit)
	{
		stream << std::format("{}[integer literal| text: \"{}\", type: {}]\n", prefix, lit->text, lit->evaluated_type.full_name());
	}

	void ASTPrinter::operator()(BooleanLiteral* lit)
	{
		stream << std::format("{}[bool literal| text: {}, type: {}]\n", prefix, lit->token.text, lit->evaluated_type.full_name());
	}

	void ASTPrinter::operator()(TupleLiteral* lit)
	{
		stream << std::format("{}[tuple literal| type: {}]\n", prefix, lit->evaluated_type.pretty_name(""));
		prefix += "    ";
		for (auto& elem : lit->elements) {
			std::visit(*this, elem->toVariant());
		}
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(StringLiteral*)
	{
		stream << std::format("{}[string literal| TODO]\n", prefix);
	}

	void ASTPrinter::operator()(NameExpression* nm)
	{
		stream << std::format("{}[name| text: {}, type: {}]\n", prefix, nm->text, nm->evaluated_type.pretty_name(""));
	}

	void ASTPrinter::operator()(BinaryOperation* op)
	{
		stream << std::format("{}[binary operation| operator: {}, type: {}]\n", prefix, op->op.text, op->evaluated_type.pretty_name(""));
		prefix += "    ";
		std::visit(*this, op->lhs->toVariant());
		std::visit(*this, op->rhs->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(CallOperation* op)
	{
		stream << std::format("{}[function call| return: {}]\n", prefix, op->evaluated_type.pretty_name(""));
		prefix += "    ";
		std::visit(*this, op->callee->toVariant());
		for (auto& arg : op->arguments) {
			std::visit(*this, arg->toVariant());
		}
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(ScopeOperation* op)
	{
		stream << std::format("{}[scope| path: {}]\n", prefix, op->evaluated_type.pretty_name(""));
	}

	void ASTPrinter::operator()(ObjectLiteral* obj)
	{
		stream << std::format("{}[object| type: {}]\n", prefix, obj->evaluated_type.pretty_name(""));
		prefix += "    ";
		for (auto& [field, expr] : obj->values) {
			stream << std::format("{}[field| name: {}]\n", prefix, field);
			prefix += "    ";
			if (expr) std::visit(*this, expr->toVariant());
			else {
				NameExpression nm(field);
				(*this)(&nm);
			}
			prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
		}
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

	void ASTPrinter::operator()(AsExpression* as_expr)
	{
		stream << std::format("{}[as| type: {}]\n", prefix, as_expr->evaluated_type.pretty_name(""));
		prefix += "    ";
		std::visit(*this, as_expr->expr->toVariant());
		prefix.erase(prefix.begin() + prefix.size() - 4, prefix.end());
	}

}