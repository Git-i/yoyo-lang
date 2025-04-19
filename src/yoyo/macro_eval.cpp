#include "ir_gen.h"
#include <ranges>
#include <parser.h>


namespace Yoyo
{
	MacroEvaluator::ObjectTy MacroEvaluator::ObjectTy::clone_or_ref_to()
	{
		auto visitor = []<typename T>(T & val) -> ObjectTy {
			if constexpr (std::is_same_v<T, std::unique_ptr<ASTNode>>)
				return { &val };
			else if constexpr (std::is_same_v<T, std::vector<ObjectTy>>)
				return { &val };
			else if constexpr (std::is_same_v<T, TokSeq>)
				return { &val };
			else
				return { val };
		};
		return std::visit(visitor, *this);
	}

	struct MacroExprEval
	{
		IRGenerator* irgen;
		MacroEvaluator* eval;
		using TokSeq = MacroEvaluator::TokSeq;
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
		MacroEvaluator::ObjectTy operator()(ArrayLiteral* lit) { 
			std::vector<MacroEvaluator::ObjectTy> object;
			if (!std::holds_alternative<std::vector<std::unique_ptr<Expression>>>(lit->elements)) 
				irgen->error(Error(lit, "this form is not permitted in macros"));
			for (auto& expr : std::get<std::vector<std::unique_ptr<Expression>>>(lit->elements)) {
				object.push_back(std::visit(*this, expr->toVariant()));
			}
			return std::move(object); 
		}
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
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			IRGenerator* irgen = this->irgen;
			if (expr->text == "const_eval") {
				return FnType{ "const_eval", [irgen](std::vector<ObjTy> object) -> ObjTy {
						if (object.size() != 1) irgen->error(Error(nullptr, "Expected one argument"));
						Expression* this_expr = nullptr;
						if (std::holds_alternative<std::unique_ptr<ASTNode>>(object[0])) {
							this_expr = dynamic_cast<Expression*>(std::get<std::unique_ptr<ASTNode>>(object[0]).get());
						}
						else if (std::holds_alternative<std::unique_ptr<ASTNode>*>(object[0])) {
							this_expr = dynamic_cast<Expression*>(std::get<std::unique_ptr<ASTNode>*>(object[0])->get());
						}
						if (!this_expr) irgen->error(Error(nullptr, "Parameter is not an expression"));
						auto answer = std::visit(ConstantEvaluator{ irgen }, this_expr->toVariant());
						if (!std::holds_alternative<uint64_t>(answer.internal_repr) && std::holds_alternative<int64_t>(answer.internal_repr)) {
							irgen->error(Error(nullptr, "Cannot convert constant to valid macro type"));
							return std::monostate{};
						}
						//TODO
						irgen->error(Error(nullptr, "Cannot convert constant to valid macro type"));
					} };
			}
			if (expr->text == "parse_expr") {
				return FnType{ "", [irgen](std::vector<ObjTy> object) -> ObjTy {
						if (object.size() != 1) irgen->error(Error(nullptr, "Expected one argument"));

						if (!std::holds_alternative<TokSeq>(object[0])
						 && !std::holds_alternative<TokSeq*>(object[0])) 
							irgen->error(Error(nullptr, "Expected token sequence"));
						auto& seq = std::holds_alternative<TokSeq>(object[0]) ?
							std::get<TokSeq>(object[0]) : *std::get<TokSeq*>(object[0]);
						std::string text;
						Parser p(text);
						for (auto& tok : seq.tokens | std::views::reverse) p.peekBuffer.emplace_back(std::move(tok));
						std::vector<ObjTy> vec_obj;
						vec_obj.push_back(p.parseExpression(0));
						vec_obj.push_back(static_cast<int64_t>(seq.tokens.size() - p.peekBuffer.size()));
						return std::move(vec_obj);
					} };
			}
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
		MacroEvaluator::ObjectTy doBexprASTCall(MacroEvaluator::ObjectTy& left, std::string& fn_name)
		{
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			using NodeType = std::unique_ptr<ASTNode>;
			auto node_ptr = std::holds_alternative<NodeType>(left) ?
				std::get<NodeType>(left).get() : std::get<NodeType*>(left)->get();
			IRGenerator* irgen = this->irgen;
			if (auto ptr = dynamic_cast<TupleLiteral*>(node_ptr)) {
				// tuple literal methods
				if (fn_name == "child_size") {
					return { FnType{ "TupleLiteral::child_size", [ptr](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
						return { static_cast<int64_t>(ptr->elements.size()) };
						}
					} };
				}
				if (fn_name == "extract_child_at") {
					return { FnType{ "TupleLiteral::extract_child_at", [ptr, irgen](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
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
					return { FnType{"StrLit::push_expr", [ptr, irgen](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
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
					return { FnType{"StrLit::push_str", [ptr, irgen](std::vector<ObjTy> objs) -> MacroEvaluator::ObjectTy {
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
					return { FnType{"IntLit::value", [ptr, irgen](std::vector<ObjTy> objs) -> ObjTy {
							return std::stoll(ptr->text);
						}
					} };
				}
				if (fn_name == "set_value") {
					return { FnType{"IntLit::set_value", [ptr, irgen](std::vector<ObjTy> objs) -> ObjTy {
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
		MacroEvaluator::ObjectTy doBexprTokSeqRefCall(TokSeq* left, std::string& fn_name) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			IRGenerator* irgen = this->irgen;
			if (fn_name == "pop_front") {
				return { FnType{"", [left](std::vector<ObjTy> objs) -> ObjTy {
					auto ret_val = MacroEvaluator::OwnedToken::from_token(left->tokens[0].first);
						left->tokens.erase(left->tokens.begin());
						return ret_val;
					}
				} };
			}
			if (fn_name == "pop_n") {
				return { FnType{"", [left, irgen](std::vector<ObjTy> objs) -> ObjTy {
						if (objs.size() != 1) irgen->error(Error(nullptr, "Expected one argument"));
						if (!std::holds_alternative<int64_t>(objs[0])) irgen->error(Error(nullptr, "Expected integer arg"));

						left->tokens.erase(left->tokens.begin(), left->tokens.begin() + std::get<int64_t>(objs[0]));
						return std::monostate{};
					}
				} };
			}
			if (fn_name == "pop_or_fail") {
				return { FnType{"", [left, irgen](std::vector<ObjTy> objs) -> ObjTy {
						if (objs.size() != 1) irgen->error(Error(nullptr, "Expected one argument"));
						if (!std::holds_alternative<int64_t>(objs[0])) irgen->error(Error(nullptr, "Expected integer arg"));
						if (static_cast<int64_t>(left->tokens[0].first.type) != std::get<int64_t>(objs[0])) {
							//fail
							irgen->error(Error(nullptr, "Pop or fail, failed"));
						}
						left->tokens.erase(left->tokens.begin());
						return std::monostate{};
					}
				} };
			}
			return doBexprTokSeqCall(*left, fn_name);
		}

		MacroEvaluator::ObjectTy doBexprTokSeqCall(TokSeq left, std::string& fn_name) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			IRGenerator* irgen = this->irgen;
			if (fn_name == "size") {
				return { FnType{"", [left](std::vector<ObjTy> objs) -> ObjTy {
						return static_cast<int64_t>(left.tokens.size());
					}
				} };
			}
			if (fn_name == "front") {
				return { FnType{"", [left](std::vector<ObjTy> objs) -> ObjTy {
						auto ret_val = MacroEvaluator::OwnedToken::from_token(left.tokens[0].first);
						return ret_val;
					}
				} };
			}
			
			
		}
		MacroEvaluator::ObjectTy doBexprOwnedTokenCall(MacroEvaluator::OwnedToken tk, std::string& fn_name) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			if (fn_name == "type") {
				auto type = tk.type;
				return { FnType{"", [type](std::vector<ObjTy> objs) -> ObjTy {
						return static_cast<int64_t>(type);
					}
				} };
			}
			if (fn_name == "is_binary_operator") {
				auto type = tk.type;
				return { FnType{"", [type](std::vector<ObjTy> objs) -> ObjTy {
						using enum TokenType;
						Token tk {.type = type};
						if (tk.can_be_overloaded_binary_only()) return true;
						return type == Minus
							|| type == Greater
							|| type == Less
							|| type == GreaterEqual
							|| type == LessEqual;
					}
				} };
			}
		}
		MacroEvaluator::ObjectTy doBexprArrayRefCall(std::vector<MacroEvaluator::ObjectTy>* tk, std::string& fn_name) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			if (fn_name == "size") {
				return FnType{ "", [tk](std::vector<ObjTy> objs) -> ObjTy { return static_cast<int64_t>(tk->size()); } };
			}
			if (fn_name == "push") {
				return FnType{ "", [tk](std::vector<ObjTy> objs) -> ObjTy {
						tk->push_back(std::move(objs[0]));
						return std::monostate{};
					}
				};
			}
		}
		MacroEvaluator::ObjectTy operator()(BinaryOperation* bexpr) {
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<ObjTy(std::vector<ObjTy>)>>;
			using NodeType = std::unique_ptr<ASTNode>;
			auto left = std::visit(*this, bexpr->lhs->toVariant());

			if (std::holds_alternative<NodeType>(left) || std::holds_alternative<NodeType*>(left)) {
				auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
				return doBexprASTCall(left, fn_name);
			}
			if (std::holds_alternative<TokSeq*>(left)) {
				auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
				return doBexprTokSeqRefCall(std::get<TokSeq*>(left), fn_name);
			}
			if (std::holds_alternative<std::vector<ObjTy>*>(left)) {
				auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
				return doBexprArrayRefCall(std::get<std::vector<ObjTy>*>(left), fn_name);
			}
			if (std::holds_alternative<TokSeq>(left)) {
				auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
				return doBexprTokSeqCall(std::get<TokSeq>(left), fn_name);
			}
			if (std::holds_alternative<MacroEvaluator::OwnedToken>(left)) {
				auto fn_name = reinterpret_cast<NameExpression*>(bexpr->rhs.get())->text;
				return doBexprOwnedTokenCall(std::get<MacroEvaluator::OwnedToken>(left), fn_name);
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
		MacroEvaluator::ObjectTy operator()(PrefixOperation* op) {
			if (op->op.type != TokenType::Bang) irgen->error(Error(op, "Expression is not allowed"));
			auto result = std::visit(*this, op->operand->toVariant());
			if (!std::holds_alternative<bool>(result)) irgen->error(Error(op, "Expression must evaluate to bool"));
			return !std::get<bool>(result);
		}
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
		MacroEvaluator::ObjectTy operator()(SubscriptOperation* expr) {
			auto object = std::visit(*this, expr->object->toVariant());
			auto index = std::visit(*this, expr->index->toVariant());
			using ArrayTy = std::vector<MacroEvaluator::ObjectTy>;
			if (std::holds_alternative<ArrayTy>(object) && std::holds_alternative<int64_t>(index)) {
				return std::move(std::get<ArrayTy>(object)[std::get<int64_t>(index)]);
			}
			else if (std::holds_alternative<ArrayTy*>(object) && std::holds_alternative<int64_t>(index)) {
				return std::move(std::get<ArrayTy*>(object)->at(std::get<int64_t>(index)));
			}
			return std::monostate{}; 
		}
		MacroEvaluator::ObjectTy operator()(LambdaExpression* expr) { 
			using ObjTy = MacroEvaluator::ObjectTy;
			using FnType = std::pair<std::string, std::function<MacroEvaluator::ObjectTy(std::vector<MacroEvaluator::ObjectTy>)>>;
			IRGenerator* irgen = this->irgen;
			return {
				FnType{"assignable", [irgen, expr](std::vector<ObjTy> objects) -> ObjTy {
					auto lambda_impl = [irgen, expr](std::vector<ObjTy>& objects, auto& this_ref) -> ObjTy {
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
			IRGenerator* irgen = this->irgen;
			if (op->type.name == "StrLit::new") {
				
				return { FnType{"StrLit::new", [](std::vector<ObjTy> objs) -> ObjTy {
						return { std::make_unique<StringLiteral>(decltype(StringLiteral::literal){}) };
					}
				} };
			}
			if (op->type.name == "IntLit::new") {

				return { FnType{"IntLit::new", [irgen](std::vector<ObjTy> objs) -> ObjTy {
						if (objs.size() != 1) irgen->error(Error(nullptr, "Expected one argument"));
						if (!std::holds_alternative<int64_t>(objs[0])) irgen->error(Error(nullptr, "Argument is not of type 'int'"));
						return { std::make_unique<IntegerLiteral>(std::to_string(std::get<int64_t>(objs[0]))) };
					}
				} };
			}
			if (op->type.name == "CallExpr::new") {
				return { FnType{"CallExpr::new", [irgen](std::vector<ObjTy> objs) -> ObjTy {
						if (objs.size() != 2) irgen->error(Error(nullptr, "Expected one argument"));
						auto get_expr = [](auto& objs) {
							if (std::holds_alternative<std::unique_ptr<ASTNode>>(objs))
								return dynamic_cast<Expression*>(std::get<std::unique_ptr<ASTNode>>(objs).get());
							else if(std::holds_alternative< std::unique_ptr<ASTNode>*>(objs))
								return dynamic_cast<Expression*>(std::get<std::unique_ptr<ASTNode>*>(objs)->get());
							return static_cast<Expression*>(nullptr);
							};
						auto release = []<typename T>(T & obj) {
							if constexpr (std::is_same_v<T, std::unique_ptr<ASTNode>>)
								obj.release();
							else if constexpr (std::is_same_v<T, std::unique_ptr<ASTNode>*>)
								obj->release();
						};
						auto get_arr = [](auto& objs) {
							if (std::holds_alternative<std::vector<ObjTy>>(objs))
								return &std::get<std::vector<ObjTy>>(objs);
							else if (std::holds_alternative<std::vector<ObjTy>*>(objs))
								return std::get<std::vector<ObjTy>*>(objs);
							return static_cast<std::vector<ObjTy>*>(nullptr);
							};
						auto callee = get_expr(objs[0]);
						if (!callee) irgen->error(Error(nullptr, "Argument 1 is not of type Expression"));
						std::visit(release, objs[0]);
						std::vector<std::unique_ptr<Expression>> args;
						auto args_arr = get_arr(objs[1]);
						if (!args_arr) irgen->error(Error(nullptr, "Argument 2 is not of array type"));
						for (auto& arg : *args_arr) {
							auto this_arg = get_expr(arg);
							if (!this_arg) irgen->error(Error(nullptr, "Argument 2 is supposed to be array of expressions"));
							std::visit(release, arg);
							args.emplace_back(this_arg);
						}
						return std::make_unique<CallOperation>(std::unique_ptr<Expression>(callee), std::move(args));
					}
				} };
			}
			if (op->type.name == "BinaryExpr::new") {
				return { FnType{"BinaryExpr::new", [irgen](std::vector<ObjTy> objs) -> ObjTy {
						using OwnedToken = MacroEvaluator::OwnedToken;
						if (objs.size() != 3) irgen->error(Error(nullptr, "Expected 3 args"));
						auto is_ast_node = [](auto& objs) {
							return std::holds_alternative< std::unique_ptr<ASTNode>>(objs)
								|| std::holds_alternative< std::unique_ptr<ASTNode>*>(objs);
							};
						auto get_expression = [](auto& objs) {
							return dynamic_cast<Expression*>(std::holds_alternative< std::unique_ptr<ASTNode>>(objs) ?
								std::get<std::unique_ptr<ASTNode>>(objs).get() :
								std::get<std::unique_ptr<ASTNode>*>(objs)->get());
							};
						auto release = []<typename T>(T & obj) {
							if constexpr (std::is_same_v<T, std::unique_ptr<ASTNode>>)
								obj.release();
							else if constexpr (std::is_same_v<T, std::unique_ptr<ASTNode>*>)
								obj->release();
							};
						if (!is_ast_node(objs[0])) irgen->error(Error(nullptr, "Arg 1 must be an expression"));
						if (!std::holds_alternative<OwnedToken>(objs[1])) irgen->error(Error(nullptr, "Arg 2 must be an integer"));
						if (!is_ast_node(objs[2])) irgen->error(Error(nullptr, "Arg 3 must be an expression"));

						Token tk{.type = std::get<OwnedToken>(objs[1]).type };
						if (!tk.can_be_overloaded()) {
							irgen->error(Error(nullptr, "Invalid token type for binary expression"));
							return std::monostate{};
						}
						auto left_expr = get_expression(objs[0]);
						if (!left_expr) {
							irgen->error(Error(nullptr, "Arg 1 must be an expression"));
							return std::monostate{};
						}
						auto right_expr = get_expression(objs[2]);
						if (!right_expr) {
							irgen->error(Error(nullptr, "Arg 3 must be an expression"));
							return std::monostate{};
						}

						std::visit(release, objs[0]);
						std::visit(release, objs[2]);
						return std::make_unique<BinaryOperation>(tk,
							std::unique_ptr<Expression>(left_expr),
							std::unique_ptr<Expression>(right_expr));
					}
				} };
			}
			if (op->type.name.starts_with("TokenType::")) {
				TokenType ret_val;
				if (op->type.name == "TokenType::LBracket") ret_val = TokenType::LParen;
				if (op->type.name == "TokenType::RBracket") ret_val = TokenType::RParen;
				return static_cast<int64_t>(ret_val);
			}
			return std::monostate{};
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
	void MacroEvaluator::operator()(WhileStatement* stat)
	{
		auto expr_eval = MacroExprEval{ irgen, this };
		auto as_variant = stat->condition->toVariant();
		auto body_var = stat->body->toVariant();
		while (true) {
			auto cond_eval = std::visit(expr_eval, as_variant);
			if (std::holds_alternative<std::monostate>(cond_eval)) break;
			if (!std::holds_alternative<bool>(cond_eval)) 
				irgen->error(Error(stat->condition.get(), "While condition did not eval to bool or null"));
			if (std::get<bool>(cond_eval) == false) break;

			variables.emplace_back();
			std::visit(*this, body_var);
			variables.pop_back();
		}
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
				return;
			}
			variables.emplace_back();
			if (std::holds_alternative<std::unique_ptr<Expression>>(invc->left))
				variables.back()[decl->first_param.first] = { std::move(std::get<std::unique_ptr<Expression>>(invc->left)) };
			else {
				TokSeq t;
				t.tokens = std::move(std::get<std::vector<std::pair<Token, SourceLocation>>>(invc->left));
				variables.back()[decl->first_param.first] = { std::move(t) };
			}
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