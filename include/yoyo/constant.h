#pragma once
#include <variant>
namespace Yoyo
{
    struct Constant
    {
        std::variant<void*, uint64_t, int64_t, double, bool> internal_repr;
        Constant() = default;
        Constant(uint64_t val) : internal_repr(val){}
        Constant(int64_t val) : internal_repr(val) {}
        Constant(bool val) : internal_repr(val) {}
        Constant(double val) : internal_repr(val) {}
        Constant(void* ptr) : internal_repr(ptr) {}
    };
    class ConstantEvaluator
    {
        Constant constConvert(const Constant& in, const Type& source, const Type& dest);
    public:
        IRGenerator* irgen;
        std::vector<std::pair<std::string, std::string>> disallowed_consts;
        Constant operator()(IntegerLiteral*);
        Constant operator()(BooleanLiteral*);
        Constant operator()(RealLiteral*);
        Constant operator()(PrefixOperation*);
        Constant operator()(NameExpression*);
        Constant operator()(BinaryOperation*);
        Constant operator()(GroupingExpression*);
        Constant operator()(LogicalOperation*);
        Constant operator()(ScopeOperation*);
        //llvm::Constant* operator()(AsExpression*);
        Constant operator()(CharLiteral*);
        Constant operator()(ObjectLiteral*);
        Constant operator()(StringLiteral*);
        Constant operator()(MacroInvocation*);
        Constant operator()(Expression*);
    };
}