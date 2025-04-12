#pragma once
#include <ir_gen.h>

namespace Yoyo {
	class LLVMIRGenerator : public IRGenerator {
    public:
        explicit LLVMIRGenerator(llvm::LLVMContext& ctx) : context(ctx) {}
		llvm::Value* currentReturnAddress;
		llvm::BasicBlock* returnBlock;
		llvm::LLVMContext& context;
		llvm::Module* code;
		std::unique_ptr<llvm::IRBuilder<>> builder;
		//we keep the alloca of the variable, its type and its drop flag
		std::vector<std::unordered_map<std::string, std::tuple<llvm::Value*, Type, llvm::Value*>>> variables;
		llvm::BasicBlock* break_to = nullptr;
		llvm::BasicBlock* continue_to = nullptr;
		llvm::Type* ToLLVMType(const Type& type, bool is_ref);
		llvm::FunctionType* ToLLVMSignature(const FunctionSignature& sig);
		llvm::AllocaInst* Alloca(std::string_view name, llvm::Type* type);
		llvm::Value* Malloc(std::string_view name, llvm::Value* size);
		llvm::Value* GCMalloc(llvm::Value* size);
		llvm::Value* GCMalloc(size_t size);
		void Free(llvm::Value* value);
		Type reduceLiteral(const Type& src, llvm::Value* val);
		void pushScope();
		void popScope();
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
        llvm::StructType* hanldeClassDeclaration(std::span<const ClassVariable> vars, Ownership own, std::string_view name);
	};
    class LLVMExpressionEvaluator
    {
        LLVMIRGenerator* irgen;
        std::optional<Type> target;
    public:
        Type lastDeducedType;
        enum ComparisonPredicate
        {
            EQ, GT, LT, EQ_GT, EQ_LT, NE, SPACE
        };
        explicit LLVMExpressionEvaluator(LLVMIRGenerator* gen, std::optional<Type> target = std::nullopt) : irgen(gen),
            target(std::move(target)) {
        }
        llvm::Value* implicitConvert(Expression* xp, llvm::Value*, const Type&, const Type&, llvm::Value* = nullptr) const;
        llvm::Value* clone(Expression* xp, llvm::Value* value, const Type& left_type, llvm::Value* into = nullptr) const;
        void destroy(llvm::Value* value, const Type& type) const;
        llvm::Value* doDot(Expression* lhs, Expression* rhs, const Type& left_type, bool load_prim = true);
        llvm::Value* doAddition(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doShl(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doShr(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doMinus(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doMult(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doDiv(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doRem(Expression*, Expression*, const Type&, const Type&);
        llvm::Value* doRange(Expression*, Expression*, const Type&, const Type&, const Type&);
        llvm::Value* doCmp(ComparisonPredicate p, Expression*, Expression*, const Type& left_type,
            const Type& right_type, const Type&);
        llvm::Value* fillArgs(bool, const FunctionSignature&, std::vector<llvm::Value*>&, llvm::Value*,
            std::vector<std::unique_ptr<Expression>>& exprs);
        llvm::Value* doInvoke(CallOperation* op, const Type&);
        llvm::Value* doUnionVar(CallOperation* op, Type&);
        void generateGenericFunction(Module*, const std::string&, GenericFunctionDeclaration*, std::span<Type>);
        void generateGenericAlias(Module*, const std::string&, GenericAliasDeclaration*, std::span<Type>);
        void generateGenericInterface(Module*, const std::string&, GenericInterfaceDeclaration*, std::span<Type>);
        void generateGenericClass(Module*, const std::string&, GenericClassDeclaration*, std::span<Type>);
        void generateGenericClass(Module*, const std::string&, GenericClassDeclaration*, std::span<const Type>);
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
        llvm::Value* operator()(IntegerLiteral*);
        llvm::Value* operator()(BooleanLiteral*);
        llvm::Value* operator()(TupleLiteral*);
        llvm::Value* operator()(ArrayLiteral*);
        llvm::Value* operator()(RealLiteral*);
        llvm::Value* operator()(StringLiteral*);
        llvm::Value* operator()(NameExpression*);
        llvm::Value* operator()(GenericNameExpression*);
        llvm::Value* operator()(PrefixOperation*);
        llvm::Value* operator()(BinaryOperation*);
        llvm::Value* operator()(GroupingExpression*);
        llvm::Value* operator()(LogicalOperation*);
        llvm::Value* operator()(PostfixOperation*);
        llvm::Value* operator()(CallOperation*);
        llvm::Value* operator()(SubscriptOperation*);
        llvm::Value* operator()(LambdaExpression*);
        llvm::Value* operator()(ScopeOperation*);
        llvm::Value* operator()(ObjectLiteral*);
        llvm::Value* operator()(NullLiteral*);
        llvm::Value* operator()(AsExpression*);
        llvm::Value* operator()(CharLiteral*);
        llvm::Value* operator()(GCNewExpression*);
        llvm::Value* operator()(MacroInvocation*);
    };
}