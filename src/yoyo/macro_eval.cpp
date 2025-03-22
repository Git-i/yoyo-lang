#include "ir_gen.h"



namespace Yoyo
{
	struct MacroExprEval
	{
		IRGenerator* irgen;
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
		MacroEvaluator::ObjectTy operator()(ArrayLiteral*);
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
		MacroEvaluator::ObjectTy operator()(NameExpression*);
		Yoyo::MacroEvaluator::ObjectTy doBexprCall(Yoyo::MacroEvaluator::ObjectTy& left, std::string& fn_name)
		{
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			using NodeType = std::unique_ptr<ASTNode>;
			auto& node = std::get<NodeType>(left);
			auto node_ptr = node.get();
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
			}
			return {};
		}
		MacroEvaluator::ObjectTy operator()(BinaryOperation* bexpr) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			using NodeType = std::unique_ptr<ASTNode>;
			if (!dynamic_cast<NameExpression*>(bexpr->rhs.get()));
			auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
			auto left = std::visit(*this, bexpr->lhs->toVariant());

			if (std::holds_alternative<NodeType>(left)) {
				return doBexprCall(left, fn_name);
			}

		}
		MacroEvaluator::ObjectTy operator()(GroupingExpression*) {

		}
		MacroEvaluator::ObjectTy operator()(LogicalOperation*);
		MacroEvaluator::ObjectTy operator()(PostfixOperation*);
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
		MacroEvaluator::ObjectTy operator()(SubscriptOperation*);
		MacroEvaluator::ObjectTy operator()(LambdaExpression*);
		MacroEvaluator::ObjectTy operator()(ScopeOperation*);
		MacroEvaluator::ObjectTy operator()(ObjectLiteral* lit)
		{
			if (!lit->t.block_hash.empty()) irgen->error(Error(lit, "Type is not usable from macro"));
			if (lit->t.name == "StrLit") {
				if (lit->values.size() != 0) irgen->error(Error(lit, "String literal must be created empty"));
				return { std::make_unique<StringLiteral>(decltype(StringLiteral::literal){}) };
			}
		}
		MacroEvaluator::ObjectTy operator()(NullLiteral*);
		MacroEvaluator::ObjectTy operator()(AsExpression*);
		MacroEvaluator::ObjectTy operator()(CharLiteral*);
		MacroEvaluator::ObjectTy operator()(GCNewExpression*);
	};
	void MacroEvaluator::operator()(VariableDeclaration*)
	{
	}
	void MacroEvaluator::operator()(IfStatement*)
	{
	}
	void MacroEvaluator::operator()(WhileStatement*)
	{
	}
	void MacroEvaluator::operator()(ForStatement*)
	{
	}
	void MacroEvaluator::operator()(BlockStatement*)
	{
	}
	void MacroEvaluator::operator()(ReturnStatement*)
	{
	}
	void MacroEvaluator::operator()(ExpressionStatement*)
	{
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
}