#pragma once
#include <string>
namespace Yoyo
{
    // The IR is SSA based and the only values either:
    // - variable names
    // - field accesses
    class Value {
        std::string base_name;
        // we could do with one name separated by .
        // but I feel this would be easier to process
        std::vector<std::string> subpaths;
    public:
        static Value from(std::string&& name) {
            Value val;
            val.base_name = name;
            return val;
        }
        Value& member(std::string&& member_name) & {
            subpaths.push_back(member_name);
            return *this;
        }
        Value member(std::string&& member_name) && {
            subpaths.push_back(member_name);
            return std::move(*this);
        }
        static Value empty() { return Value(); }
        bool is_empty() { return base_name.empty();  }
    };
    class Instruction {
    public:
        ASTNode* origin;
        virtual ~Instruction() = default;
        virtual bool is_terminator() { return false; }
    };
    class BasicBlock {
        std::vector<std::unique_ptr<Instruction>> instructions;
    public:
        void add_instruction(Instruction* inst) {
            instructions.emplace_back(inst);
        }
        bool is_terminated() const {
            return !instructions.empty() && instructions.back()->is_terminator();
        }
    };

    // most instruction bind thier result into a variable, hence the into parameter

    // Create a new primitive object
    class NewPrimitiveInstruction : public Instruction {
    public:
        std::string into;
        NewPrimitiveInstruction(std::string&& into): into(into) {};
    };
    // borrow a value
    class BorrowValueInstruction : public Instruction {
    public:
        Value val;
        std::string into;
        BorrowValueInstruction(Value&& val, std::string&& into) : val(val), into(into) {};
    };
    class RelocateValueInstruction : public Instruction {
        Value val;
        std::string into;
    };
    class CallFunctionInstruction : public Instruction {
    public:
        std::vector<Value> val;
        std::string function_name;
        std::string into;
        CallFunctionInstruction(std::string&& function_name, std::string&& into, std::vector<Value>&& val)
            : val(val), function_name(function_name), into(into) {}
    };
    class AssignInstruction : public Instruction {
        Value lhs;
        Value rhs;
    };
    class PhiInstruction : public Instruction {
    public:
        std::vector<Value> args;
        std::string into;
        PhiInstruction(std::vector<Value>&& args, std::string&& into) : args(args), into(into) {};

    };
    // These instructions must be at the end of each basic block
    class RetInstruction : public Instruction {
        std::optional<Value> ret_val;
        bool is_terminator() override { return true; }
    };
    class BrInstruction : public Instruction {
    public:
        BasicBlock* next;
        BrInstruction(BasicBlock* next) : next(next) {};
        bool is_terminator() override { return true;  }
    };
    class CondBrInstruction : public Instruction {
    public:
        std::vector<BasicBlock*> options;
        Value br_on;
        bool is_terminator() override { return true; }
        CondBrInstruction(std::vector<BasicBlock*>&& options, Value&& br_on) : options(options), br_on(br_on) {};
    };

    struct BorrowCheckerFunction {
        std::vector<std::unique_ptr<BasicBlock>> blocks;
        BasicBlock* new_block(std::string debug_name);
    };

    class BorrowCheckerEmitter {
        IRGenerator* irgen;
        TypeCheckerState* stt;
        // std::vector -- each block
        //    std::vector -- each variable
        //        std::pair -- variable entry
        //            std::string -- variable name
        //            std::string -- borrow checker id
        std::vector<std::vector<std::pair<std::string, std::string>>> variables;
        BorrowCheckerFunction* function;
        BasicBlock* current_block;
        std::string temporary_name();
    public:
        BorrowCheckerEmitter(IRGenerator* irgen, TypeCheckerState* stt) : irgen(irgen), variables(1), stt(stt) {}
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(EnumDeclaration*);
        void operator()(UsingStatement*);
        void operator()(ModuleImport*);
        void operator()(ConditionalExtraction*);
        void operator()(WithStatement*);
        void operator()(OperatorOverload*);
        void operator()(GenericFunctionDeclaration*);
        void operator()(AliasDeclaration*);
        void operator()(GenericAliasDeclaration*);
        void operator()(GenericClassDeclaration*);
        void operator()(InterfaceDeclaration*);
        void operator()(BreakStatement*);
        void operator()(ContinueStatement*);
        void operator()(ConstantDeclaration*);
        void operator()(CImportDeclaration*);
        void operator()(UnionDeclaration*);
        void operator()(MacroDeclaration*);

        Value operator()(IfExpression*);
        Value operator()(BlockExpression*);
        Value operator()(IntegerLiteral*);
        Value operator()(BooleanLiteral*);
        Value operator()(TupleLiteral*);
        Value operator()(ArrayLiteral*);
        Value operator()(RealLiteral*);
        Value operator()(StringLiteral*);
        Value operator()(NameExpression*);
        Value operator()(GenericNameExpression*);
        Value operator()(PrefixOperation*);
        Value operator()(BinaryOperation*);
        Value operator()(GroupingExpression*);
        Value operator()(LogicalOperation*);
        Value operator()(PostfixOperation*);
        Value operator()(CallOperation*);
        Value operator()(SubscriptOperation*);
        Value operator()(LambdaExpression*);
        Value operator()(ScopeOperation*);
        Value operator()(ObjectLiteral*);
        Value operator()(NullLiteral*);
        Value operator()(AsExpression*);
        Value operator()(CharLiteral*);
        Value operator()(GCNewExpression*);
        Value operator()(MacroInvocation*);
        Value operator()(SpawnExpression*);
        Value operator()(TryExpression*);
    };
}