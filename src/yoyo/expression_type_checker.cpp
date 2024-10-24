#include "ir_gen.h"
namespace Yoyo
{
    static std::optional<Type> unaryNegateResult(Module* mod, const Type& typ)
    {
        if(typ.is_unsigned_integral()) return std::nullopt;
        if(typ.is_signed_integral() || typ.is_floating_point()) return typ;
        if(auto fn = mod->findFunction("__op_unary_negate__" + typ.name))
            return fn->returnType;
        return std::nullopt;
    }
    static std::optional<Type> unaryNotResult(Module* module, const Type& type)
    {
        if(type.is_boolean()) return type;
        if(auto fn = module->findFunction("__op_unary_not__" + type.name))
            return fn->returnType;
        return std::nullopt;
    }
    std::optional<Type> ExpressionTypeChecker::operator()(ArrayLiteral* lit)
    {
        Type t{.name = "__arr", .subtypes = {}};
        Type subtype;
        for(auto& elem : lit->elements)
        {
            auto sub_t = std::visit(*this, elem->toVariant());
            if(!sub_t) return std::nullopt;
            subtype = Type::variant_merge(std::move(subtype), std::move(sub_t).value());
        }
        return subtype;
    }

    std::optional<Type> ExpressionTypeChecker::operator()(BinaryOperation* expr)
    {
        auto lhs = std::visit(*this, expr->lhs->toVariant());
        auto rhs = std::visit(*this, expr->rhs->toVariant());
        //builtin operators
        if(!lhs || !rhs) return std::nullopt;
        auto left_int_width = lhs->integer_width();
        auto right_int_width = rhs->integer_width();
        auto left_float_width = lhs->float_wwdth();
        switch(expr->op.type)
        {
            using enum TokenType;
        case Plus:
            {
                if(left_int_width && right_int_width) //both integers
                {
                    //7:13(shet up)
                }
            }
        }
        //TODO
    }
    std::optional<Type> ExpressionTypeChecker::operator()(BooleanLiteral*)
    {
        return Type{.name = "bool", .subtypes = {}};
    }
    std::optional<Type> ExpressionTypeChecker::operator()(GroupingExpression* expr)
    {
        return std::visit(*this, expr->toVariant());
    }
    std::optional<Type> ExpressionTypeChecker::operator()(IntegerLiteral*)
    {
        return Type{.name = "i32", .subtypes = {}};
    }



    std::optional<Type> ExpressionTypeChecker::operator()(PrefixOperation* op)
    {
        auto op_type_opt = std::visit(*this, op->operand->toVariant());
        if(!op_type_opt) return std::nullopt;
        Type& op_type = op_type_opt.value();
        switch(op->op.type)
        {
            using enum TokenType;
        case Minus: return unaryNegateResult(irgen->module, op_type);
        case Bang: return unaryNotResult(irgen->module, op_type);
        default: return std::nullopt;
        }

    }

    std::optional<Type> ExpressionTypeChecker::operator()(CallOperation* op)
    {
        auto callee_ty = std::visit(*this, op->callee->toVariant());
        if(!callee_ty) return std::nullopt;
        if(callee_ty->name != "__fn") return std::nullopt;
        if(auto fn = irgen->module->findFunction(callee_ty->subtypes[0].name))
        {
            return fn->returnType;
        }
        return std::nullopt;
    }
    std::optional<Type> ExpressionTypeChecker::operator()(RealLiteral*)
    {
        return Type{.name = "f64", .subtypes = {}};
    }
    std::optional<Type> ExpressionTypeChecker::operator()(StringLiteral*)
    {
        return Type{.name = "string", .subtypes = {}};
    }


}