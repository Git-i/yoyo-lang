#pragma once
#include "fn_type.h"
#include "statement.h"
#include "expression.h"
#include <charconv>
#include <vector>
namespace Yoyo
{
    class IRGenerator;
    // type must be an integer i8-i64/u8-u64
    struct IsIntegerConstraint {
        Type type;
        ASTNode* expr;
    };
    // type must be able to store a given value (i8 cannot store 1000)
    struct CanStoreIntegerConstraint {
        Type type;
        std::variant<uint64_t, int64_t> value;
        ASTNode* expr;
    };
    // type must be a floating point value (f32/f64)
    struct IsFloatConstraint {
        Type type;
        ASTNode* expr;
    };
    // type must be able to store a given real value
    struct CanStoreRealConstraint {
        Type type;
        double value;
        ASTNode* expr;
    };
    // type can be converted to string
    struct ToStringConstraint {
        Type type;
        ASTNode* expr;
    };
    // type has unary minus returning ret
    struct HasUnaryMinusConstraint {
        Type type;
        Type ret;
        ASTNode* expr;
    };
    // type has unary not returning ret
    struct HasUnaryNotConstraint {
        Type type;
        Type ret;
        ASTNode* expr;
    };
    // type is &other or &mut other
    struct IsReferenceToConstraint {
        Type type;
        Type other;
        ASTNode* expr;
    };
    // type is not a reference
    struct IsNotReferenceConstraint {
        Type type;
        ASTNode* expr;
    };
    // the binary expression is <left> <op> <right> defined and returns result
    struct BinaryOperableConstraint {
        Type left;
        Type right;
        Type result;
        TokenType op;
        // we keep a constraint local cache of all substitutions we've made as they may create new type
        // variables and we want to avoid making too many unnecesary type variables
        std::unordered_map<OperatorOverload*, std::unordered_map<std::string, Type>> substitution_cache;
        ASTNode* expr;
    };
    
    struct IndexOperableConstraint {
        Type left;
        Type right;
        Type result;
        bool is_mutable;
        ASTNode* expr;
    };
    struct BinaryDotCompatibleConstraint {
        Type tp;
        Expression* right;
        Type result;
        ASTNode* expr;
        std::unordered_map<OperatorOverload*, std::unordered_map<std::string, Type>> substitution_cache;
        std::optional<Type> deref_result_opt;
    };
    // type is either CmpOrd or CmpPartOrd
    struct ComparableConstraint {
        Type type;
        ASTNode* expr;
    };
    // ensure 2 types are equal
    struct EqualConstraint {
        Type type1;
        Type type2;
        ASTNode* expr;
    };
    // enseure 2 types are equal or type1 is void
    struct EqualOrIsVoidConstraint {
        Type type1;
        Type type2;
        ASTNode* expr;
    };
    // ensure type is owning
    struct OwningConstraint {
        Type type;
        ASTNode* expr;
    };
    struct NonOwningConstraint {
        Type type;
        ASTNode* expr;
    };
    // type can be called (a function or lambda)
    struct IsInvocableConstraint {
        Type type;
        ASTNode* expr;
    };
    // type can be put in the function as arg number
    struct ValidAsFunctionArgConstraint {
        Type type;
        Type function;
        uint32_t arg_no;
        ASTNode* expr;
    };
    // type is the return value of the given function
    struct IsReturnOfConstraint {
        Type type;
        Type function;
        ASTNode* expr;
    };
    // Type must implement specified interface
    struct ImplInterfaceConstraint {
        Type type;
        Type interface;
        ASTNode* expr;
    };
    // type can be used in conditional extraction
    // i.e if |value| (object as type) { ... }
    struct ExtractsToConstraint {
        Type type;
        Type dst;
        ASTNode* expr;
    };
    // type can be used in else block of conditional extraction
    // if |_| (object as type) else |value: dst| {}
    struct ElseExtractsToConstraint {
        Type type;
        Type dst;
        ASTNode* expr;
    };
    // Similar to extracts to but when the extraction results borrows
    struct RefExtractsToConstraint {
        Type type;
        Type dst;
        ASTNode* expr;
    };
    // Similar to else extracts to but the result borrows
    struct ElseRefExtractsToConstraint {
        Type type;
        Type dst;
        ASTNode* expr;
    };
    struct ConvertibleToConstraint {
        Type from;
        Type to;
        ASTNode* expr;
    };
    struct IfStatementConstraint {
        Type then_type;
        Type else_type;
        bool then_transfers_control;
        bool else_transfers_control;
        Type result;
        ASTNode* expr;
    };
    struct HasFieldConstraint {
        Type subject;
        std::string field_name;
        Type result;
        ASTNode* expr;
    };
    struct AllFieldsConstraint {
        std::vector<std::string> fields;
        Type subject;
        ASTNode* expr;
    };
    struct BorrowResultConstraint {
        Type subject;
        Type result;
        ASTNode* expr;

        // see binary operator constraint for why this exists
        std::unordered_map<OperatorOverload*, std::unordered_map<std::string, Type>> substitution_cache;
    };
    struct BorrowResultMutConstraint {
        Type subject;
        Type result;
        ASTNode* expr;
        // see binary operator constraint for why this exists
        std::unordered_map<OperatorOverload*, std::unordered_map<std::string, Type>> substitution_cache;
    };
    // This is technically not a constraint
    // its just a way to conditionally apply constraints
    struct TypeCheckerConstraint;
    struct IfEqualThenConstrain {
        Type type1;
        Type type2;
        // have to use std::vector like this because I can't
        // forward declare using
        std::vector<TypeCheckerConstraint> apply_if_true;
    };
    using TypeCheckerConstraintUnderlyingType = std::variant<
        IsIntegerConstraint,
        CanStoreIntegerConstraint,
        IsFloatConstraint,
        CanStoreRealConstraint,
        ToStringConstraint,
        HasUnaryMinusConstraint,
        HasUnaryNotConstraint,
        IsReferenceToConstraint,
        IsNotReferenceConstraint,
        BinaryOperableConstraint,
        ComparableConstraint,
        EqualConstraint,
        OwningConstraint,
        IsInvocableConstraint,
        ValidAsFunctionArgConstraint,
        IsReturnOfConstraint,
        ImplInterfaceConstraint,
        ExtractsToConstraint,
        RefExtractsToConstraint,
        NonOwningConstraint,
        ConvertibleToConstraint,
        BinaryDotCompatibleConstraint,
        ElseExtractsToConstraint,
        ElseRefExtractsToConstraint,
        IfStatementConstraint,
        EqualOrIsVoidConstraint,
        IndexOperableConstraint,
        HasFieldConstraint,
        AllFieldsConstraint,
        BorrowResultConstraint,
        BorrowResultMutConstraint,
        IfEqualThenConstrain
    >;
    struct TypeCheckerConstraint : public TypeCheckerConstraintUnderlyingType {
        template<typename T>
        TypeCheckerConstraint(T&& t) noexcept : TypeCheckerConstraintUnderlyingType(t) {}
    };
    
    /// represents the possible types a variable can be
    class Domain {
        bool is_infinite = true;
    public:
        // set of concrete types a type can be
        // it must be one of the elements in a group
        struct Group {
            std::vector<Type> types;
            void add_type(Type&&);
        };
        // add a group of types and intersect it with the current group
        [[nodiscard]] std::optional<Error> add_and_intersect(Group&&, TypeCheckerState*);
        [[nodiscard]] std::optional<Error> constrain_to_store(uint64_t);
        [[nodiscard]] std::optional<Error> constrain_to_store(int64_t);
        [[nodiscard]] std::optional<Error> constrain_to_store(double);
        // basically `add_and_intersect` but with an entire domain
        [[nodiscard]] std::optional<Error> merge_intersect(Domain&&, TypeCheckerState*);
        // this solves the domain
        [[nodiscard]] std::optional<Error> equal_constrain(Type other);
        bool is_solved();
        Type get_solution();
        Group concrete_types;
    };
    // This is a union-find
    // but variables maintain their domain (set of types they can possibly be)
    // checked with each constraint
    struct UnificationTable {
        std::vector<uint32_t> parents;
        std::vector<uint32_t> rank;
        std::unordered_map<uint32_t, Domain> domains; // not every id has a domain

        uint32_t type_to_id(const Type& tp) {
            // type variables are types but in the form "?<num>"
            uint32_t res;
            auto [ptr, ec] = std::from_chars(tp.name.c_str() + 1, tp.name.c_str() + tp.name.size(), res);
            if (ec != std::errc{}) __debugbreak();
            return res;
        }
        Type id_to_type(uint32_t id) {
            return Type{ "?" + std::to_string(id) };
        }
        uint32_t find(uint32_t id) {
            // this is not a root
            if (parents[id] != id)
                // path compress
                parents[id] = find(parents[id]);
            return parents[id];
        }
        uint32_t unite(uint32_t id, uint32_t id2, IRGenerator *irgen, TypeCheckerState*);
        Domain* domain_of(uint32_t id) {
            return &domains[find(id)];
        }
    };
    struct TypeCheckerState {
        std::vector<std::unordered_map<std::string, Type>> variables;
        std::vector<TypeCheckerConstraint> constraints;
        std::vector<TypeCheckerConstraint>* write_new_constraints_to;
        UnificationTable tbl;
        // the return type of the function being checked
        Type return_type;
        // Creates a new type variable
        Type new_type_var();
        // Adds a new constraint to be checked
        // (should not be called by the constraint checker, it has a special method for that)
        void add_constraint(TypeCheckerConstraint);
        // given 2 type variables, make them equal to each other
        void unify_types(const Type&, const Type&, IRGenerator* irgen);
        // append a new block for variables
        void push_variable_block();
        // remove a block for variables
        void pop_variable_block();
        // register a new variable
        void create_variable(std::string name, Type type);
        // applies a type substitution if possible
        // else converts it to its most solved form
        Type best_repr(const Type&);
        Domain* get_type_domain(const Type&);

        void resolve_function(FunctionDeclaration* decl, IRGenerator* irgen, const FunctionSignature&);
        // A variation of Type::is_non_owning that is guaranteed to not instantiate generics
        bool is_non_owning(const Type&, IRGenerator*);
    };
    struct ConstraintSolver {
        bool has_error;
        IRGenerator* irgen;
        TypeCheckerState* state;
        std::vector<TypeCheckerConstraint> temp_constraints;
        bool operator()(IsIntegerConstraint& con);
        bool operator()(CanStoreIntegerConstraint& con);
        bool operator()(IsFloatConstraint& con);
        bool operator()(CanStoreRealConstraint& con);
        bool operator()(ToStringConstraint& con);
        bool operator()(HasUnaryMinusConstraint& con);
        bool operator()(HasUnaryNotConstraint& con);
        bool operator()(IsReferenceToConstraint& con);
        bool operator()(IsNotReferenceConstraint& con);
        bool operator()(BinaryOperableConstraint& con);
        bool operator()(ComparableConstraint& con);
        bool operator()(EqualConstraint& con);
        bool operator()(OwningConstraint& con);
        bool operator()(IsInvocableConstraint& con);
        bool operator()(ValidAsFunctionArgConstraint& con);
        bool operator()(IsReturnOfConstraint& con);
        bool operator()(ImplInterfaceConstraint& con);
        bool operator()(ExtractsToConstraint& con);
        bool operator()(RefExtractsToConstraint& con);
        bool operator()(NonOwningConstraint& con);
        bool operator()(ConvertibleToConstraint& con);
        bool operator()(BinaryDotCompatibleConstraint& con);
        bool operator()(ElseRefExtractsToConstraint& con);
        bool operator()(ElseExtractsToConstraint& con);
        bool operator()(IfStatementConstraint& con);
        bool operator()(EqualOrIsVoidConstraint& con);
        bool operator()(IndexOperableConstraint& con);
        bool operator()(HasFieldConstraint& con);
        bool operator()(AllFieldsConstraint& con);
        bool operator()(BorrowResultConstraint& con);
        bool operator()(BorrowResultMutConstraint& con);
        bool operator()(IfEqualThenConstrain& con);
        void add_new_constraint(TypeCheckerConstraint);
    };
	struct TypeChecker
	{   
        std::optional<Type> target;
        IRGenerator* irgen;
        TypeCheckerState* state;
        bool in_mutable_ctx;
        void operator()(FunctionDeclaration*) {}
        void operator()(ClassDeclaration*) {}
        void operator()(VariableDeclaration*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(EnumDeclaration*) {}
        void operator()(UsingStatement*) {}
        void operator()(ModuleImport*) {}
        void operator()(ConditionalExtraction*);
        void operator()(WithStatement*);
        void operator()(OperatorOverload*) {}
        void operator()(GenericFunctionDeclaration*) {}
        void operator()(AliasDeclaration*) {}
        void operator()(GenericAliasDeclaration*) {}
        void operator()(GenericClassDeclaration*) {}
        void operator()(InterfaceDeclaration*) {}
        void operator()(BreakStatement*) {}
        void operator()(ContinueStatement*) {}
        void operator()(ConstantDeclaration*){}
        void operator()(CImportDeclaration*) {}
        void operator()(UnionDeclaration*) {}
        void operator()(MacroDeclaration*) {}

        FunctionType operator()(IfExpression*) const;
        FunctionType operator()(BlockExpression*) const;
        FunctionType operator()(TryExpression*) const;
        FunctionType operator()(IntegerLiteral*) const;
        FunctionType operator()(BooleanLiteral*) const;
        FunctionType operator()(TupleLiteral*) const;
        FunctionType operator()(ArrayLiteral*) const;
        FunctionType operator()(RealLiteral*) const;
        FunctionType operator()(StringLiteral*) const;
        FunctionType operator()(NameExpression*) const;
        FunctionType operator()(GenericNameExpression*) const;
        FunctionType operator()(PrefixOperation*) const;
        FunctionType operator()(BinaryOperation*) const;
        FunctionType operator()(GroupingExpression*) const;
        FunctionType operator()(LogicalOperation*) const;
        FunctionType operator()(PostfixOperation*) const;
        FunctionType operator()(CallOperation*) const;
        FunctionType operator()(SubscriptOperation*) const;
        FunctionType operator()(LambdaExpression*) const;
        FunctionType operator()(ScopeOperation*) const;
        FunctionType operator()(ObjectLiteral*) const;
        FunctionType operator()(NullLiteral*) const;
        FunctionType operator()(AsExpression*) const;
        FunctionType operator()(CharLiteral*) const;
        FunctionType operator()(GCNewExpression*) const;
        FunctionType operator()(MacroInvocation*) const;
        FunctionType operator()(SpawnExpression*) const;

        
        TypeChecker new_target(std::optional<Type>, bool in_mutable_ctx = false) const;
        TypeChecker targetless(bool in_mutable_ctx = false) const;
	};
}