#pragma once
#include <ir_gen.h>
#include "yoyo_vm/vm.h"
#include "yvm/native_type.h"
#include "yoyo_vm/emitter.h"
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
        void callDestructors(size_t depth = 0);
        void pushScope();
        void popScope();
        bool GenerateIR(std::string_view name, std::vector<std::unique_ptr<Statement>> statements, LLModule* md, Engine* eng);
        size_t nextKnownAddr();
        std::optional<Type> getVariableType(const std::string& name, Expression*) override;
        llvm::StructType* hanldeClassDeclaration(std::span<const ClassVariable> vars, Ownership own, std::string_view name);
	};
    class YVMExpressionEvaluator
    {
        LLVMIRGenerator* irgen;
        std::optional<Type> target;
    public:
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
        void implicitConvert(Expression* xp, const Type&, const Type&, bool on_stack, bool perform_load) const;
        void clone(Expression* xp, const Type& left_type, bool on_stack, bool perform_load) const;
        
        void destroy(const Type& type) const;
        void doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_prim = true);
        void doAddition(Expression*, Expression*, const Type&, const Type&);
        void doShl(Expression*, Expression*, const Type&, const Type&);
        void doShr(Expression*, Expression*, const Type&, const Type&);
        void doMinus(Expression*, Expression*, const Type&, const Type&);
        void doMult(Expression*, Expression*, const Type&, const Type&);
        void doDiv(Expression*, Expression*, const Type&, const Type&);
        void doRem(Expression*, Expression*, const Type&, const Type&);
        void doRange(Expression*, Expression*, const Type&, const Type&, const Type&);
        void doCmp(ComparisonPredicate p, Expression*, Expression*, const Type& left_type,
            const Type& right_type, const Type&);
        void fillArgs(bool, const FunctionSignature&, std::vector<llvm::Value*>&, llvm::Value*,
            std::vector<std::unique_ptr<Expression>>& exprs);
        void doInvoke(CallOperation* op, const Type&);
        void doUnionVar(CallOperation* op, Type&);
        //the malloc and the size
        std::pair<llvm::Value*, llvm::Value*> doToStr(llvm::Value*, const Type&);
        struct LValueEvaluator
        {
            LLVMIRGenerator* irgen;
            llvm::Value* operator()(NameExpression*);
            llvm::Value* operator()(BinaryOperation*);
            llvm::Value* operator()(Expression*);
            llvm::Value* operator()(PrefixOperation*);
            llvm::Value* operator()(GroupingExpression*);
        };
        void operator()(IntegerLiteral*);
        void operator()(BooleanLiteral*);
        void operator()(TupleLiteral*);
        void operator()(ArrayLiteral*);
        void operator()(RealLiteral*);
        void operator()(StringLiteral*);
        void operator()(NameExpression*);
        void operator()(GenericNameExpression*);
        void operator()(PrefixOperation*);
        void operator()(BinaryOperation*);
        void operator()(GroupingExpression*);
        void operator()(LogicalOperation*);
        void operator()(PostfixOperation*);
        void operator()(CallOperation*);
        void operator()(SubscriptOperation*);
        void operator()(LambdaExpression*);
        void operator()(ScopeOperation*);
        void operator()(ObjectLiteral*);
        void operator()(NullLiteral*);
        void operator()(AsExpression*);
        void operator()(CharLiteral*);
        void operator()(GCNewExpression*);
        void operator()(MacroInvocation*);
    };
}