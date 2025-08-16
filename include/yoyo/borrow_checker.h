#pragma once
#include <string>
namespace Yoyo
{
    struct BorrowCheckerBlock;
    struct Instruction {
        struct BorrowInstruction { std::string into; std::string operand; };
        std::variant<
            BorrowInstruction, // borrow, borrow mut and borrow temp
            std::string, // drop
            BorrowCheckerBlock*, // br
            std::pair<BorrowCheckerBlock*, BorrowCheckerBlock*>, // cond_br
            std::optional<std::string>, // ret
            std::pair<std::string, std::string> //move
        > data;
        enum {
            Borrow,
            BorrowMut,
            Move,
            Br,
            CondBr,
            Ret,
            Drop,
            BorrowTemp,
        } type;
        ASTNode* expr; // the ast expression corresponding to this instruction
    };
    enum class BorrowType {
        Const, Mut
    };
    struct BorrowState {
        std::unordered_map<std::string, std::set<Instruction*>> immutable_borrows;
        std::unordered_map<std::string, Instruction*> mutable_borrow;

        std::unordered_map<std::string,
            std::vector<std::tuple<std::string, BorrowType, Instruction*>>
        > is_borrowing;

        std::unordered_map<std::string, std::string> temporary_holders;

        static BorrowState aggregate(std::ranges::input_range auto&& range)
            requires std::same_as<std::ranges::range_value_t<decltype(range)>, BorrowState>
        {
            BorrowState result;
            for (BorrowState&& st : range) {
                // merge in immutable borrows
                for (auto& [var, borrows] : st.immutable_borrows) {
                    // given a var, we don't merge it in if there is already a mutable one
                    if (result.mutable_borrow.contains(var)) continue;
                    result.immutable_borrows[var].merge(borrows);
                }
                // merge in mutable borrows
                for (auto& [var, borrow] : st.mutable_borrow) {
                    if (result.immutable_borrows.contains(var)) {
                        result.immutable_borrows.erase(var);
                    }
                    result.mutable_borrow[var] = borrow;
                }
                // merge in is_borrowing
                for (auto& [var, other] : st.is_borrowing) {
                    result.is_borrowing[var].insert(result.is_borrowing[var].end(), other.begin(), other.end());
                }

                for (auto& [holdee, holder] : st.temporary_holders) {
                    if (result.temporary_holders.contains(holdee)) debugbreak();
                    result.temporary_holders[holdee] = holder;
                }
            }
            return result;
        }
    };
    struct BorrowCheckerBlock {
        std::list<Instruction> insts;
        std::unordered_map<BorrowCheckerBlock*, std::optional<BorrowState>> preds;
        bool is_checked = false;
    };
    struct BorrowChecker {
        size_t counter = 0;
        /// Represents a variable
        struct Object : public std::string {
        };
        Object make_object();
        Object make_external_object();
        BorrowCheckerBlock* make_block();
        void drop_object(const std::string&, ASTNode* expr);
        void set_block(BorrowCheckerBlock*);
        std::string borrow_values(std::span<const std::pair<std::string, BorrowType>>, Expression*);
        std::vector<std::unique_ptr<BorrowCheckerBlock>> blocks;
        void check_and_report(IRGenerator* irgen);
        void create_cond_br(BorrowCheckerBlock*, BorrowCheckerBlock*);
        void create_br(BorrowCheckerBlock*);
        void make_preds();
        std::string move_object(const std::string&, Expression*);
        /// move one object into another composite object
        /// this transfers all borrows and drops the original
        void move_into(const std::string&, const std::string&, Expression*);
        /// borrow an object and register it as a temporary
        /// meaning it will be dropped once the borrow is dropped
        std::string borrow_as_temporary(const std::string& val, Expression*);
        BorrowCheckerBlock* current_block;
    };
    // emits borrow checker IR for statements and expressions
    class BorrowCheckerEmitter {
        IRGenerator* irgen;
        // std::vector -- each block
        //    std::vector -- each variable
        //        std::pair -- variable entry
        //            std::string -- variable name
        //            std::string -- borrow checker id
        std::vector<std::vector<std::pair<std::string, std::string>>> variables;
    public:
        BorrowCheckerEmitter(IRGenerator* irgen) : irgen(irgen), variables(1) {}
        void operator()(FunctionDeclaration*);
        void operator()(ClassDeclaration*);
        void operator()(VariableDeclaration*);
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
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

        std::string operator()(IntegerLiteral*);
        std::string operator()(BooleanLiteral*);
        std::string operator()(TupleLiteral*);
        std::string operator()(ArrayLiteral*);
        std::string operator()(RealLiteral*);
        std::string operator()(StringLiteral*);
        std::string operator()(NameExpression*);
        std::string operator()(GenericNameExpression*);
        std::string operator()(PrefixOperation*);
        std::string operator()(BinaryOperation*);
        std::string operator()(GroupingExpression*);
        std::string operator()(LogicalOperation*);
        std::string operator()(PostfixOperation*);
        std::string operator()(CallOperation*);
        std::string operator()(SubscriptOperation*);
        std::string operator()(LambdaExpression*);
        std::string operator()(ScopeOperation*);
        std::string operator()(ObjectLiteral*);
        std::string operator()(NullLiteral*);
        std::string operator()(AsExpression*);
        std::string operator()(CharLiteral*);
        std::string operator()(GCNewExpression*);
        std::string operator()(MacroInvocation*);
        std::string operator()(SpawnExpression*);
    };
}