#pragma once
#include <ir_gen.h>
#include "yoyo_vm/vm.h"
#include "yvm/native_type.h"
#include "yoyo_vm/emitter.h"
#include "yvm/yvm_module.h"
namespace Yoyo {
	class YVMIRGenerator : public IRGenerator {
    public:
		Yvm::Module* code;
		std::unique_ptr<Yvm::Emitter> builder;
		std::vector< //< the list of variables by each scope
            /// the list of variables this scope
            /// the std::pair<> is the variable name and the stack_addr and type of the variable
            std::vector<std::pair<std::string, std::pair<size_t, Type>>>
        > variables;
		std::string break_to;
		std::string continue_to;
        virtual void doFunction(FunctionDeclaration* decl) override { (*this)(decl); };
        virtual void doClass(ClassDeclaration* decl) override { (*this)(decl); };
        virtual void doAlias(AliasDeclaration* decl) override { (*this)(decl); };
        virtual void doConst(ConstantDeclaration* decl) override { (*this)(decl); };
        NativeTy* toNativeType(const Type& type);
        Yvm::Type toTypeEnum(const Type& type);
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
        void operator()(CImportDeclaration*) {}
        void operator()(UnionDeclaration*);
        void operator()(MacroDeclaration*);
        void callDestructors(size_t depth = 0);
        void pushScope();
        void popScope();
        bool GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, YVMModule* md, Engine* eng);
        size_t nextKnownAddr();
        std::optional<Type> getVariableType(const std::string& name, Expression*) override;
	};
    class YVMExpressionEvaluator
    {
    public:
        std::optional<Type> target;
        YVMIRGenerator* irgen;
        Type lastDeducedType;
        enum ComparisonPredicate
        {
            EQ, GT, LT, EQ_GT, EQ_LT, NE, SPACE
        };
        explicit YVMExpressionEvaluator(YVMIRGenerator* gen, std::optional<Type> target = std::nullopt) : irgen(gen),
            target(std::move(target)) {
        }
        /// on_stack means to write it to a pointer on the stack top other wise put it on new `alloca`
        /// if `on_stack` is on, we except the value to be below the stack top
        /// if `on_stack` is off, we expect the value to be at the stack top
        /// either ways the value is the resulting value is pushed on the stack top
        /// 
        /// perform_load controls if we should output a value for non-sret type (ints)
        /// for non-sret types perform_load used with on_stack effectively duplicates the value
        /// perform_load set and on_stack not set does not allocate
        /// perform_load not set and on_stack set does not allocate
        /// perform_load not set and on_stack not set will allocate
        void implicitConvert(Expression* xp, const Type&, const Type&, bool on_stack, bool perform_load) const;
        void clone(Expression* xp, const Type& left_type, bool on_stack, bool perform_load) const;
        
        void destroy(const Type& type) const;
        std::vector<Type> doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_prim = true);
        std::vector<Type> doAddition(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doShl(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doShr(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doMinus(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doMult(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doDiv(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doRem(Expression*, Expression*, const Type&, const Type&);
        std::vector<Type> doRange(Expression*, Expression*, const Type&, const Type&, const Type&);
        std::vector<Type> doCmp(ComparisonPredicate p, Expression*, Expression*, const Type& left_type,
            const Type& right_type, const Type&);
        std::vector<Type> doSingleStringLiteral(const std::string& text, StructNativeTy* str_type);
        std::vector<Type> fillArgs(bool, const FunctionSignature&, const std::unique_ptr<Expression>&,
            std::vector<std::unique_ptr<Expression>>& exprs);
        std::vector<Type> doInvoke(CallOperation* op, const Type&);
        std::vector<Type> doUnionVar(CallOperation* op, Type&);
        //the malloc and the size
        struct LValueEvaluator
        {
            YVMIRGenerator* irgen;
            std::vector<Type> operator()(NameExpression*);
            std::vector<Type> operator()(BinaryOperation*);
            std::vector<Type> operator()(Expression*);
            std::vector<Type> operator()(PrefixOperation*);
            std::vector<Type> operator()(GroupingExpression*);
        };
        std::vector<Type> operator()(IntegerLiteral*);
        std::vector<Type> operator()(BooleanLiteral*);
        std::vector<Type> operator()(TupleLiteral*);
        std::vector<Type> operator()(ArrayLiteral*);
        std::vector<Type> operator()(RealLiteral*);
        std::vector<Type> operator()(StringLiteral*);
        std::vector<Type> operator()(NameExpression*);
        std::vector<Type> operator()(GenericNameExpression*);
        std::vector<Type> operator()(PrefixOperation*);
        std::vector<Type> operator()(BinaryOperation*);
        std::vector<Type> operator()(GroupingExpression*);
        std::vector<Type> operator()(LogicalOperation*);
        std::vector<Type> operator()(PostfixOperation*);
        std::vector<Type> operator()(CallOperation*);
        std::vector<Type> operator()(SubscriptOperation*);
        std::vector<Type> operator()(LambdaExpression*);
        std::vector<Type> operator()(ScopeOperation*);
        std::vector<Type> operator()(ObjectLiteral*);
        std::vector<Type> operator()(NullLiteral*);
        std::vector<Type> operator()(AsExpression*);
        std::vector<Type> operator()(CharLiteral*);
        std::vector<Type> operator()(GCNewExpression*);
        std::vector<Type> operator()(MacroInvocation*);
        std::vector<Type> operator()(SpawnExpression*);
    };
}