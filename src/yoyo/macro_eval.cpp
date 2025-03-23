#include "ir_gen.h"
#include <ranges>


namespace Yoyo
{
	MacroEvaluator::ObjectTy MacroEvaluator::ObjectTy::clone_or_ref_to()
	{
		auto visitor = []<typename T>(T & val) -> ObjectTy {
			if constexpr (std::is_same_v<T, std::unique_ptr<ASTNode>>) {
				return { &val };
			}
			else return { val };
		};
		return std::visit(visitor, *this);
	}

	struct MacroExprEval
	{
		IRGenerator* irgen;
		MacroEvaluator* eval;
		MacroEvaluator::ObjectTy operator()(IntegerLiteral* lit)
		{
			return { std::stoll(lit->text) };
		}
		MacroEvaluator::ObjectTy operator()(BooleanLiteral* lit)
		{
			if (lit->token.type == TokenType::True)
				return { true };
			else 
				return { false };
		}
		MacroEvaluator::ObjectTy operator()(ArrayLiteral*) { return {}; }
		MacroEvaluator::ObjectTy operator()(RealLiteral* lit)
		{
			return { std::stod(std::string(lit->token.text)) };
		}
		MacroEvaluator::ObjectTy operator()(StringLiteral* lit)
		{
			if (lit->literal.size() != 1) irgen->error(Error(lit, "Strings in macros cannot have interpolation"));
			if (!std::holds_alternative<std::string>(lit->literal[0]))
				irgen->error(Error(lit, "Strings in macros cannot have interpolation"));
			return { std::get<0>(lit->literal[0]) };
		}
		MacroEvaluator::ObjectTy operator()(NameExpression* expr) { 
			for (auto& stack : eval->variables | std::views::reverse) {
				if (stack.contains(expr->text)) {
					return stack.at(expr->text).clone_or_ref_to();
				}
			}
		}
		MacroEvaluator::ObjectTy doBexprInt(int64_t left, MacroEvaluator::ObjectTy right, TokenType tp) {
			if(std::holds_alternative<int64_t>(right)) {
				auto rhs = std::get<int64_t>(right);
				switch (tp)
				{
				case Yoyo::TokenType::Plus: return left + rhs;
				case Yoyo::TokenType::Minus: return left - rhs;
				case Yoyo::TokenType::Slash: return left / rhs;
				case Yoyo::TokenType::Star: return left * rhs;
				case Yoyo::TokenType::Percent: return left % rhs;
				case Yoyo::TokenType::Less: return left < rhs;
				case Yoyo::TokenType::Greater: return left > rhs;
				case Yoyo::TokenType::DoubleEqual: return left == rhs;
				case Yoyo::TokenType::LessEqual: return left <= rhs;
				case Yoyo::TokenType::GreaterEqual: return left >= rhs;
				case Yoyo::TokenType::Ampersand: return left & rhs;
				case Yoyo::TokenType::Pipe: return left | rhs;
				case Yoyo::TokenType::Caret: return left ^ rhs;
				case Yoyo::TokenType::DoubleLess: return left << rhs;
				default: return std::monostate{};
				}
			}

		}
		MacroEvaluator::ObjectTy doBexprCall(MacroEvaluator::ObjectTy& left, std::string& fn_name)
		{
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			using NodeType = std::unique_ptr<ASTNode>;
			auto node_ptr = std::holds_alternative<NodeType>(left) ?
				std::get<NodeType>(left).get() : std::get<NodeType*>(left)->get();
			if (auto ptr = dynamic_cast<TupleLiteral*>(node_ptr)) {
				// tuple literal methods
				if (fn_name == "child_size") {
					return { FnType{ "TupleLiteral::child_size", [ptr, this](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
						return { static_cast<int64_t>(ptr->elements.size()) };
						}
					} };
				}
				if (fn_name == "extract_child_at") {
					return { FnType{ "TupleLiteral::extract_child_at", [ptr, this](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
						if (objs.size() != 1) irgen->error(Error(nullptr, "Too many args"));
						if (!std::holds_alternative<int64_t>(objs[0])) irgen->error(Error(nullptr, "Arg must be an int"));
						auto idx = std::get<int64_t>(objs[0]);
						ASTNode* res = ptr->elements[idx].release();
						ptr->elements.erase(ptr->elements.begin() + idx);
						return { std::unique_ptr<ASTNode>(res) };
						}
					} };
				}
			}
			else if (auto ptr = dynamic_cast<StringLiteral*>(node_ptr)) {
				if (fn_name == "push_expr") {
					return { FnType{"StrLit::push_expr", [ptr, this](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
							if (objs.size() != 1) irgen->error(Error(nullptr, "Too many args"));
							using Expr = std::unique_ptr<ASTNode>;
							if (!std::holds_alternative<Expr>(objs[0])) irgen->error(Error(nullptr, "Arg must be an expression"));
							auto& expr = std::get<Expr>(objs[0]);
							auto as_expr = dynamic_cast<Expression*>(expr.get());

							if (!as_expr)
								if (!expr) irgen->error(Error(nullptr, "Expression is empty"));
								else irgen->error(Error(nullptr, "Arg must be an expression"));
							expr.release();
							ptr->literal.push_back(std::unique_ptr<Expression>{ as_expr });
							return { std::monostate{} };
						}
					} };
				}
				if (fn_name == "push_str") {
					return { FnType{"StrLit::push_str", [ptr, this](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
							if (objs.size() != 1) irgen->error(Error(nullptr, "Too many args"));
							if (!std::holds_alternative<std::string>(objs[0])) irgen->error(Error(nullptr, "Arg must be a string"));
							auto& expr = std::get<std::string>(objs[0]);
							
							ptr->literal.push_back(expr);
							return { std::monostate{} };
						}
					} };
				}
			}
			else if (auto ptr = dynamic_cast<IntegerLiteral*>(node_ptr)) {
				if (fn_name == "value") {
					return { FnType{"IntLit::value", [ptr, this](std::vector<ObjTy> objs) -> ObjTy {
							return std::stoll(ptr->text);
						}
					} };
				}
				if (fn_name == "set_value") {
					return { FnType{"IntLit::set_value", [ptr, this](std::vector<ObjTy> objs) -> ObjTy {
							if (objs.size() != 1) irgen->error(Error(nullptr, "Too many args"));
							if (!std::holds_alternative<int64_t>(objs[0])) irgen->error(Error(nullptr, "Arg must be an int"));
							ptr->text = std::to_string(std::get<int64_t>(objs[0]));
							return std::monostate{};
						}
					} };
				}
			}
			return {};
		}
		MacroEvaluator::ObjectTy operator()(BinaryOperation* bexpr) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			using NodeType = std::unique_ptr<ASTNode>;
			auto left = std::visit(*this, bexpr->lhs->toVariant());

			if (std::holds_alternative<NodeType>(left) || std::holds_alternative<NodeType*>(left)) {
				auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
				return doBexprCall(left, fn_name);
			}
			if (std::holds_alternative<int64_t>(left)) {
				auto rhs = std::visit(*this, bexpr->rhs->toVariant());
				return doBexprInt(std::get<int64_t>(left), std::move(rhs), bexpr->op.type);
			}

		}
		MacroEvaluator::ObjectTy operator()(GroupingExpression*) {
			return {};
		}
		MacroEvaluator::ObjectTy operator()(LogicalOperation*) { return {}; }
		MacroEvaluator::ObjectTy operator()(PostfixOperation*) { return {}; }
		MacroEvaluator::ObjectTy operator()(CallOperation* expr) {
			using FnType = std::pair<std::string, std::function<MacroEvaluator::ObjectTy(std::vector<MacroEvaluator::ObjectTy>)>>;
			auto callee = std::visit(*this, expr->callee->toVariant());
			if (!std::holds_alternative<FnType>(callee)) irgen->error(Error(expr, "Expression can't be called"));
			auto& fn = std::get<FnType>(callee);
			std::vector<MacroEvaluator::ObjectTy> args;
			for (auto& arg : expr->arguments) {
				args.push_back(std::visit(*this, arg->toVariant()));
			}
			return fn.second(std::move(args));
		}
		MacroEvaluator::ObjectTy operator()(SubscriptOperation*) { return {}; }
		MacroEvaluator::ObjectTy operator()(LambdaExpression* expr) { 
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<MacroEvaluator::ObjectTy(std::vector<MacroEvaluator::ObjectTy>)>>;
			return {
				FnType{"assignable", [this, expr](std::vector<ObjTy> objects) -> ObjTy {
					auto lambda_impl = [this, expr](std::vector<ObjTy>& objects, auto& this_ref) -> ObjTy {
						if (objects.size() != expr->sig.parameters.size()) irgen->error(Error(expr, "Invalid number of args"));
						auto mac_eval = MacroEvaluator{ irgen };
						mac_eval.variables.emplace_back();
						for (auto& var : expr->sig.parameters) {
							mac_eval.variables.back()[var.name] = std::move(objects[objects.size() - 1]);
							objects.pop_back();
						}
						mac_eval.variables.back()["this"] = FnType{ "this", [&](std::vector<ObjTy> objects)->ObjTy {
								return this_ref(objects, this_ref);
							}
						};
						std::visit(mac_eval, expr->body->toVariant());
						mac_eval.variables.pop_back();
						return std::move(mac_eval.return_addr);
					};
					return lambda_impl(objects, lambda_impl);
				}
			}
			};
		}
		MacroEvaluator::ObjectTy operator()(ScopeOperation* op) { 
			using FnType = std::pair<std::string, std::function<MacroEvaluator::ObjectTy(std::vector<MacroEvaluator::ObjectTy>)>>;
			using ObjTy = MacroEvaluator::ObjectTy;
			if (op->type.name == "StrLit::new") {
				
				return { FnType{"StrLit::new", [](std::vector<ObjTy> objs) -> ObjTy {
						return { std::make_unique<StringLiteral>(decltype(StringLiteral::literal){}) };
					}
				} };
			}
		}
		MacroEvaluator::ObjectTy operator()(ObjectLiteral* lit)
		{
			if (!lit->t.block_hash.empty()) irgen->error(Error(lit, "Type is not usable from macro"));
			if (lit->t.name == "StrLit") {
				if (lit->values.size() != 0) irgen->error(Error(lit, "String literal must be created empty"));
				return { std::make_unique<StringLiteral>(decltype(StringLiteral::literal){}) };
			}
		}
		MacroEvaluator::ObjectTy operator()(NullLiteral*) { return {}; }
		MacroEvaluator::ObjectTy operator()(AsExpression*) { return {}; }
		MacroEvaluator::ObjectTy operator()(CharLiteral*) { return {}; }
		MacroEvaluator::ObjectTy operator()(GCNewExpression*) { return {}; }
		MacroEvaluator::ObjectTy operator()(Expression* exp) {
			irgen->error(Error(exp, "Expression is not allowed inside macro"));
			return {};
		}
	};
	void MacroEvaluator::operator()(VariableDeclaration* decl)
	{
		std::string name(decl->identifier.text);
		if (variables.back().contains(name)) irgen->error(Error(decl, "Duplicate variables"));
		if (!decl->initializer) irgen->error(Error(decl, "Macro variable cannot be left uninitialized"));
		
		variables.back()[name] = std::visit(MacroExprEval{ irgen, this }, decl->initializer->toVariant());
	}
	void MacroEvaluator::operator()(IfStatement* stat)
	{
		auto cond = std::visit(MacroExprEval{ irgen, this }, stat->condition->toVariant());
		if (!std::holds_alternative<bool>(cond)) irgen->error(Error(stat->condition.get(), "If statement condition must evaluate to bool"));
		if (std::get<bool>(cond)) {
			std::visit(*this, stat->then_stat->toVariant());
			if (has_returned) return;
		}
		else {
			if (stat->else_stat) {
				std::visit(*this, stat->else_stat->toVariant());
			}
		}
	}
	void MacroEvaluator::operator()(WhileStatement*)
	{
	}
	// for only supoprts ranges expressions
	void MacroEvaluator::operator()(ForStatement* stat)
	{
		auto as_rexp = dynamic_cast<BinaryOperation*>(stat->iterable.get());
		if (!as_rexp || as_rexp->op.type != TokenType::DoubleDot) 
			irgen->error(Error(stat->iterable.get(), "Only range expressions are allowed"));
		auto lhs = std::visit(MacroExprEval{ irgen, this }, as_rexp->lhs->toVariant());
		auto rhs = std::visit(MacroExprEval{ irgen, this }, as_rexp->rhs->toVariant());

		if(!std::holds_alternative<int64_t>(lhs) || !std::holds_alternative<int64_t>(rhs))
			irgen->error(Error(as_rexp, "Range must be with integer types"));

		auto left = std::get<int64_t>(lhs);
		auto right = std::get<int64_t>(rhs);

		if (stat->names.size() != 1) irgen->error(Error(stat, "Invalid number of captures"));
		std::string var_name(stat->names[0].text);

		variables.emplace_back();
		for (auto i : std::views::iota(left, right))
		{
			variables.back()[var_name] = { i };
			std::visit(*this, stat->body->toVariant());
		}
		variables.pop_back();
	}
	void MacroEvaluator::operator()(BlockStatement* stats)
	{
		variables.emplace_back();
		for (auto& stat : stats->statements) {
			std::visit(*this, stat->toVariant());
			if (has_returned) {
				variables.pop_back();
				return;
			}
		}
		variables.pop_back();
	}
	void MacroEvaluator::operator()(ReturnStatement* ret)
	{
		if (!ret->expression) irgen->error(Error(ret, "Must return a value"));
		return_addr = std::visit(MacroExprEval{ irgen, this }, ret->expression->toVariant());
		if (std::holds_alternative<std::unique_ptr<ASTNode>*>(return_addr)) {
			return_addr = 
				std::unique_ptr<ASTNode>(std::get<std::unique_ptr<ASTNode>*>(return_addr)->release());
		}
		has_returned = true;
	}
	void MacroEvaluator::operator()(ExpressionStatement* expr)
	{
		std::visit(MacroExprEval{ irgen, this }, expr->expression->toVariant());
	}
	void MacroEvaluator::operator()(ConditionalExtraction*)
	{
	}
	void MacroEvaluator::operator()(BreakStatement*)
	{
	}
	void MacroEvaluator::operator()(ContinueStatement*)
	{
	}
	void MacroEvaluator::operator()(Statement* stat)
	{
		irgen->error(Error(stat, "Statement is not allowed inside macro"));
	}
	void MacroEvaluator::eval(MacroInvocation* invc)
	{
		if (invc->result) return;
		if (auto nexpr = dynamic_cast<NameExpression*>(invc->macro_name.get())) {
			auto decl = irgen->module->findMacro(irgen->block_hash, nexpr->text);
			if (!decl) {
				irgen->error(Error(invc, "Macro " + nexpr->text, " cannot be found in the current context"));
			}
			variables.emplace_back();
			variables.back()[decl->first_param.first] = { std::move(std::get<std::unique_ptr<Expression>>(invc->left)) };
			std::visit(*this, decl->body->toVariant());
			variables.pop_back();

			ASTNode* node = nullptr;
			if (std::holds_alternative<std::unique_ptr<ASTNode>>(return_addr)) {
				node = std::get<std::unique_ptr<ASTNode>>(return_addr).release();
			}

			if (!node) irgen->error(Error(invc, "Macro did not return expression"));
			else {
				auto expr = dynamic_cast<Expression*>(node);
				if(!expr) irgen->error(Error(invc, "Macro did not return expression"));
				invc->result = std::unique_ptr<Expression>(expr);
			}
			return;
		};
		irgen->error(Error(invc, "Not implemented"));
	}
}