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
        Expression* expr;
    };
    // type must be able to store a given value (i8 cannot store 1000)
    struct CanStoreIntegerConstraint {
        Type type;
        std::variant<uint64_t, int64_t> value;
        Expression* expr;
    };
    // type must be a floating point value (f32/f64)
    struct IsFloatConstraint {
        Type type;
        Expression* expr;
    };
    // type must be able to store a given real value
    struct CanStoreRealConstraint {
        Type type;
        double value;
        Expression* expr;
    };
    // type can be converted to string
    struct ToStringConstraint {
        Type type;
        Expression* expr;
    };
    // type has unary minus returning ret
    struct HasUnaryMinusConstraint {
        Type type;
        Type ret;
        Expression* expr;
    };
    // type has unary not returning ret
    struct HasUnaryNotConstraint {
        Type type;
        Type ret;
        Expression* expr;
    };
    // type is &other or &mut other
    struct IsReferenceToConstraint {
        Type type;
        Type other;
        Expression* expr;
    };
    // type is not a reference
    struct IsNotReferenceConstraint {
        Type type;
        Expression* expr;
    };
    // the binary expression is <left> <op> <right> defined and returns result
    struct BinaryOperableConstraint {
        Type left;
        Type right;
        Type result;
        TokenType op;
        Expression* expr;
    };
    // type is either CmpOrd or CmpPartOrd
    struct ComparableConstraint {
        Type type;
        Expression* expr;
    };
    // ensure 2 types are equal
    struct EqualConstraint {
        Type type1;
        Type type2;
        Expression* expr;
    };
    // ensure type is owning
    struct OwningConstraint {
        Type type;
        Expression* expr;
    };
    // type can be called (a function or lambda)
    struct IsInvocableConstraint {
        Type type;
        Expression* expr;
    };
    // type can be put in the function as arg number
    struct ValidAsFunctionArgConstraint {
        Type type;
        Type function;
        uint32_t arg_no;
        Expression* expr;
    };
    // type is the return value of the given function
    struct IsReturnOfConstraint {
        Type type;
        Type function;
        Expression* expr;
    };
    // Type must implement specified interface
    struct ImplInterfaceConstraint {
        Type type;
        Type interface;
        Expression* expr;
    };
    // type can be used in conditional extraction
    // i.e if |value| (object as type) { ... }
    struct ExtractsToConstraint {
        Type type;
        Type dst;
        Expression* expr;
    };
    // Similar to extracts to but when the extraction results borrows
    struct RefExtractsToConstraint {
        Type type;
        Type dst;
        Expression* expr;
    };
    using TypeCheckerConstraint = std::variant<
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
        RefExtractsToConstraint
    >;
    /// represents the possible types a variable can be
    class Domain {
        enum TriState : uint8_t { None = 0, False, True };
        bool is_infinite = true;
        TriState is_owning = None;

    public:
        // set of concrete types a type can be
        // it must be one of the elements in a group
        struct Group {
            std::vector<Type> types;
            void add_type(Type&&);
        };
        std::optional<Error> add_and_intersect(Group&&);
        void constrain_to_store(uint64_t);
        void constrain_to_store(int64_t);
        void constrain_to_store(double);
        std::optional<Error> merge_intersect(Domain&&);
        std::optional<Error> equal_constrain(Type&& other);
    private:
        std::vector<Group> concrete_types;
    };
    // This is a union-find
    // but variables maintain thier domain (set of types they can possibly be)
    // checked with each constraint
    struct UnificationTable {
        std::vector<uint32_t> parents;
        std::vector<uint32_t> rank;
        std::unordered_map<uint32_t, Domain> domains; // not every id has a domain

        uint32_t type_to_id(const Type& tp) {
            // type variables are types but in the form "?<num>"
            uint32_t res;
            std::from_chars(tp.name.c_str() + 1, tp.name.c_str() + tp.name.size(), res);
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
        uint32_t unite(uint32_t id, uint32_t id2, IRGenerator *irgen);
        Domain* domain_of(uint32_t id) {
            return &domains[find(id)];
        }
    };
    struct TypeCheckerState {
        std::vector<std::unordered_map<std::string, Type>> variables;
        std::vector<TypeCheckerConstraint> constraints;
        Type return_type;
        Type new_type_var();
        void add_constraint(TypeCheckerConstraint);
        void unify_types(const Type&, const Type&);
        void push_variable_block();
        void pop_variable_block();
        void create_variable(std::string name, Type type);
        // applies a type substitution if possible
        // else converts it to its most solved form
        Type best_repr(const Type&);
        Domain* get_type_domain(const Type&);

        void resolve_function(FunctionDeclaration* decl, IRGenerator* irgen);
    };
    struct ConstraintSolver {
        bool has_error;
        IRGenerator* irgen;
        TypeCheckerState* state;
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

        void add_new_constraint(TypeCheckerConstraint);
    };
	struct TypeChecker
	{   
        std::optional<Type> target;
        IRGenerator* irgen;
        TypeCheckerState* state;
        void operator()(FunctionDeclaration*) {}
        void operator()(ClassDeclaration*) {}
        void operator()(VariableDeclaration*) {}
        void operator()(IfStatement*);
        void operator()(WhileStatement*);
        void operator()(ForStatement*);
        void operator()(BlockStatement*);
        void operator()(ReturnStatement*);
        void operator()(ExpressionStatement*);
        void operator()(EnumDeclaration*) {}
        void operator()(UsingStatement*) {}
        void operator()(ModuleImport*) {}
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

        
        TypeChecker new_target(std::optional<Type>) const;
        TypeChecker targetless() const;
	};
}