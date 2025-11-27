#pragma once
#include "statement.h"
#include <string>
#include <memory>
#include <span>
#include <vector>
#include <optional>
#include <variant>
#include <map>
namespace Yoyo {
    void debugbreak(); // in src/yoyo/irgen.cpp
    class IRGenerator;
    struct TypeCheckerState;
    class ASTNode;
    namespace BorrowChecker
    {
        // The IR is SSA based and the only values either:
        // - variable names
        // - field accesses
        struct Domain {
            std::string name;
            std::string to_string() const { return name; }
            bool is_var() const { return name.starts_with("'?"); };
        };
        class Value {
        public:
            std::string base_name;
            // we could do with one name separated by .
            // but I feel this would be easier to process
            std::vector<std::string> subpaths;
            static Value from(std::string&& name) {
                Value val;
                val.base_name = name;
                return val;
            }
            Value& member(std::string&& member_name)& {
                subpaths.push_back(member_name);
                return *this;
            }
            Value member(std::string&& member_name)&& {
                subpaths.push_back(member_name);
                return std::move(*this);
            }
            static Value empty() { return Value(); }
            bool is_empty() { return base_name.empty(); }
            // verified by the type system to never be borrowed immutably
            // so we don't need much wrt to borrow checking these
            static Value constant() { return Value::from("__constant__"); }
            static Value function(std::string&& func_name) { return Value::from("__constant__fn__" + func_name); }
            std::optional<std::string> function_name() {
                using namespace std::literals::string_view_literals;
                if (!base_name.starts_with("__constant__fn__")) return std::nullopt;
                return std::string(base_name.begin() + "__constant__fn__"sv.size(), base_name.end());
            }
            std::string to_string() {
                std::string result = "%" + base_name;
                for (auto& path : subpaths) result += "." + path;
                return result;
            }
            Domain as_domain() {
                auto final_str = base_name;
                for (auto& subpath : subpaths) {
                    final_str += "." + subpath;
                }
                return Domain{ std::move(final_str) };
            }
        };
        class InstructionVariant;
        struct BasicBlock;
        class Instruction {
        public:
            ASTNode* origin;
            virtual ~Instruction() = default;
            virtual bool is_terminator() { return false; }
            virtual std::span<BasicBlock*> children() { return {}; };
            virtual InstructionVariant to_variant() = 0;
        };
        struct BasicBlock {
            std::string debug_name;
            std::vector<std::unique_ptr<Instruction>> instructions;
            std::vector<BasicBlock*> preds;
            std::string to_string();
            void add_instruction(Instruction* inst) {
                instructions.emplace_back(inst);
            }
            bool is_terminated() const {
                return !instructions.empty() && instructions.back()->is_terminator();
            }
        };
        // most instructions bind thier result into a variable, hence the "into" parameter
        // Create a new primitive object
        class NewPrimitiveInstruction : public Instruction {
        public:
            std::string into;
            NewPrimitiveInstruction(std::string&& into) : into(into) {};
            InstructionVariant to_variant() override;
        };
        class NewArrayInstruction : public Instruction {
        public:
            std::vector<Value> values;
            std::string into;
            InstructionVariant to_variant() override;
            NewArrayInstruction(std::vector<Value>&& values, std::string&& into) :
                values(values), into(into) {
            }
        };
        // borrow a value
        class BorrowValueInstruction : public Instruction {
        public:
            Value val;
            std::string into;
            InstructionVariant to_variant() override;
            BorrowValueInstruction(Value&& val, std::string&& into) : val(val), into(into) {};
        };
        class RelocateValueInstruction : public Instruction {
        public:
            Value val;
            std::string into;
            InstructionVariant to_variant() override;
            RelocateValueInstruction(Value&& val, std::string&& into) : val(val), into(into) {};
        };
        class CallFunctionInstruction : public Instruction {
        public:
            std::vector<Value> val;
            std::string function_name;
            std::string into;
            InstructionVariant to_variant() override;
            CallFunctionInstruction(std::string&& function_name, std::string&& into, std::vector<Value>&& val)
                : val(val), function_name(function_name), into(into) {
            }
        };
        class AssignInstruction : public Instruction {
        public:
            Value lhs;
            Value rhs;
            AssignInstruction(Value&& lhs, Value&& rhs) : lhs(lhs), rhs(rhs) {}
            InstructionVariant to_variant() override;
        };
        class PhiInstruction : public Instruction {
        public:
            std::vector<Value> args;
            std::string into;
            InstructionVariant to_variant() override;
            PhiInstruction(std::vector<Value>&& args, std::string&& into) : args(args), into(into) {};

        };
        class DomainPhiInstruction : public Instruction {
        public:
            std::vector<Domain> args;
            std::string into;
            InstructionVariant to_variant() override;
            DomainPhiInstruction(std::vector<Domain>&& args, std::string&& into) : args(args), into(into) {};
        };
        // These instructions must be at the end of each basic block
        class RetInstruction : public Instruction {
        public:
            std::optional<Value> ret_val;
            InstructionVariant to_variant() override;
            RetInstruction() : ret_val(std::nullopt) {}
            RetInstruction(Value&& val) : ret_val(val) {}
            RetInstruction(std::optional<Value>&& val) : ret_val(val) {}
            bool is_terminator() override { return true; }
        };
        class BrInstruction : public Instruction {
        public:
            BasicBlock* next;
            InstructionVariant to_variant() override;
            BrInstruction(BasicBlock* next) : next(next) {};
            bool is_terminator() override { return true; }
            std::span<BasicBlock*> children() override { return std::span{ &next, 1 }; };
        };
        class CondBrInstruction : public Instruction {
        public:
            std::vector<BasicBlock*> options;
            Value br_on;
            InstructionVariant to_variant() override;
            bool is_terminator() override { return true; }
            std::span<BasicBlock*> children() override { return options; };
            CondBrInstruction(std::vector<BasicBlock*>&& options, Value&& br_on) : options(options), br_on(br_on) {};
        };
        class DomainSubsetConstraint : public Instruction {
        public:
            Domain super;
            Domain sub;
            DomainSubsetConstraint(Domain&& super, Domain&& sub) : super(super), sub(sub) {}
            InstructionVariant to_variant() override;
        };
        class DomainDependenceEdgeConstraint : public Instruction {
        public:
            Domain d1;
            Domain d2;
            DomainDependenceEdgeConstraint(Domain&& d1, Domain&& d2) : d1(std::move(d1)), d2(std::move(d2)) {}
            InstructionVariant to_variant() override;
        };
        class DomainExtensionConstraint : public Instruction {
        public:
            Domain super;
            Domain sub;
            // only used in the SSA phase
            Domain old_super;
            DomainExtensionConstraint(Domain&& super, Domain&& sub) : super(super), sub(sub) {}
            InstructionVariant to_variant() override;
        };
        using InstructionVariantBase = std::variant<
            CondBrInstruction*,
            BrInstruction*,
            RetInstruction*,
            PhiInstruction*,
            AssignInstruction*,
            CallFunctionInstruction*,
            RelocateValueInstruction*,
            NewArrayInstruction*,
            NewPrimitiveInstruction*,
            BorrowValueInstruction*,
            DomainSubsetConstraint*,
            DomainDependenceEdgeConstraint*,
            DomainExtensionConstraint*,
            DomainPhiInstruction*
        >;
        class InstructionVariant : public InstructionVariantBase {
        public:
            template<typename T>
            InstructionVariant(T* val) : InstructionVariantBase(val) {}
        };

        struct BorrowCheckerFunction {
            std::vector<std::unique_ptr<BasicBlock>> blocks;
            size_t idx = 0;
            BasicBlock* new_block(std::string debug_name) {
                return blocks.emplace_back(new BasicBlock{ .debug_name = debug_name + std::to_string(idx++) }).get();
            }
            std::string to_string() {
                std::string out;
                for (auto& block : blocks) {
                    out += block->to_string() + "\n\n";
                }
                return out;
            }
        };
        
        class BorrowCheckerEmitter {
            IRGenerator* irgen;
            TypeCheckerState* stt;
            // std::vector -- each block
            //    std::vector -- each variable
            //        std::pair -- variable entry
            //            std::string -- variable name
            //            std::string -- borrow checker id

            size_t counter = 0;
            std::vector<std::vector<std::pair<std::string, std::string>>> variables;
            BorrowCheckerFunction* function;
            BasicBlock* current_block;
            std::string temporary_name() { return "__tmp" + std::to_string(counter++); }
            std::string name_based_on(std::string_view other) { return std::to_string(counter++) + std::string(other); }
            void drop_object(Value&& val) { /* TODO */ }
        public:
            BorrowCheckerEmitter(
                IRGenerator* irgen,
                TypeCheckerState* stt,
                BorrowCheckerFunction* function,
                BasicBlock* current_block) :
                irgen(irgen),
                variables(1),
                stt(stt),
                function(function),
                current_block(current_block) {
            }
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
        struct DomainCheckerState;
        
        struct BorrowCheckerType {
            enum TypeType {
                Primitive = 0, // Unit type cant be divided any further (stability doesn't matter)
                Aggregate, // Product type (stable)
                Union, // Sum type (unstable)
                // I'm building these two as primitves to make my life easier
                UniquePtr, // Owning poiner type
                Array, // Owning Heap type with many elems
                RefPtr, // Non owning pointer type
            };
            struct PrimitiveDetails {};
            // because std::map is wierd
            struct FieldMap: std::map<std::string, BorrowCheckerType> {
                FieldMap(const FieldMap&) = delete;
                FieldMap() = default;
                FieldMap(FieldMap&&) noexcept = default;
            };
            struct AggregateDetails {
                AggregateDetails() = default;
                AggregateDetails(AggregateDetails&&) noexcept = default;
                AggregateDetails& operator=(AggregateDetails&&) noexcept = default;

                AggregateDetails(const AggregateDetails&) = delete;
                AggregateDetails& operator=(const AggregateDetails&) = delete;

                AggregateDetails(FieldMap&& fields) : fields(std::move(fields)) {}
                FieldMap fields;
            };
            struct UnionDetails {
                UnionDetails() = default;
                UnionDetails(const UnionDetails&) = delete;
                UnionDetails& operator=(const UnionDetails&) = delete;

                UnionDetails(UnionDetails&&) noexcept = default;
                UnionDetails& operator=(UnionDetails&&) noexcept = default;

                UnionDetails(FieldMap&& fields) : fields(std::move(fields)) {}
                FieldMap fields;
            };
            struct UniquePtrDetails {
                UniquePtrDetails() = default;
                UniquePtrDetails(const UniquePtrDetails&) = delete;
                UniquePtrDetails& operator=(const UniquePtrDetails&) = delete;

                UniquePtrDetails(UniquePtrDetails&&) noexcept = default;
                UniquePtrDetails& operator=(UniquePtrDetails&&) noexcept = default;

                UniquePtrDetails(std::unique_ptr<BorrowCheckerType>&& subtype) : subtype(std::move(subtype)) {}
                std::unique_ptr<BorrowCheckerType> subtype;
            };
            struct ArrayDetails {
                ArrayDetails() = default;
                ArrayDetails(const ArrayDetails&) = delete;
                ArrayDetails& operator=(const ArrayDetails&) = delete;

                ArrayDetails& operator=(ArrayDetails&&) noexcept = default;
                ArrayDetails(ArrayDetails&&) noexcept = default;

                ArrayDetails(std::unique_ptr<BorrowCheckerType>&& subtype) : subtype(std::move(subtype)) {}
                std::unique_ptr<BorrowCheckerType> subtype;
            };
            struct RefPtrDetails {
                RefPtrDetails() = default;
                RefPtrDetails(const RefPtrDetails&) = delete;
                RefPtrDetails& operator=(const RefPtrDetails&) = delete;

                RefPtrDetails(RefPtrDetails&&) noexcept = default;
                RefPtrDetails& operator=(RefPtrDetails&&) noexcept = default;
                RefPtrDetails(std::unique_ptr<BorrowCheckerType>&& subtype) : subtype(std::move(subtype)) {}
                std::unique_ptr<BorrowCheckerType> subtype;
            };
            BorrowCheckerType() = default;
            BorrowCheckerType(const BorrowCheckerType&) = delete;
            BorrowCheckerType(BorrowCheckerType&&) noexcept = default;
            BorrowCheckerType& operator=(BorrowCheckerType&&) noexcept = default;
            BorrowCheckerType& operator=(const BorrowCheckerType&) = delete;
            std::variant<
                PrimitiveDetails,
                AggregateDetails,
                UnionDetails,
                UniquePtrDetails,
                ArrayDetails,
                RefPtrDetails> details;
            // the "bool" field marks if the domain is bidirectional
            // for example &'a &'b i32, 'a is not bidirectional because
            // if the reference were to be duplicated the new 'a can expand freely
            // but the new 'b cannot expand freely and must remainn equal to the original 'b
            std::vector<std::pair<Domain, bool>> domains;

            // Bring up all nexted domains
            void normalize();
            // Get a borrowed version of a type into the specified domain
            BorrowCheckerType borrowed(Domain&&) const;
            // Make a new type with fresh domains
            BorrowCheckerType cloned(DomainCheckerState*) const;
            // Duplicate a type with the same domains
            BorrowCheckerType moved(DomainCheckerState*) const;
            // create a new primitive type
            static BorrowCheckerType new_primitive();
            // does not change domains of the provided type
            static BorrowCheckerType new_array_of(BorrowCheckerType&&);
            std::string to_string() const;
        };
        // Maps "Value"s to types
        struct ValueTypeMapping: std::unordered_map<std::string, BorrowCheckerType> {
            ValueTypeMapping(const ValueTypeMapping&) = delete;
            ValueTypeMapping() = default;
            ValueTypeMapping(ValueTypeMapping&&) noexcept = default;
            std::string to_string();
        };
        // Types the CFG and inserts domains
        class DomainVariableInserter {
        public:
            // insert instructions to satisfy domain relationships when left is assiged right
            void add_assign_constraints_between_types(const BorrowCheckerType&, const BorrowCheckerType&);
            // insert instructions to satisfy domain relationships when left is extended to store right (like array push)
            void add_extend_constraints_between_types(const BorrowCheckerType&, const BorrowCheckerType&);
            void initialize_domains_to_null(const BorrowCheckerType&);
            using InstructionListTy = decltype(BasicBlock::instructions);
            using BlockIteratorTy = InstructionListTy::iterator;

            DomainCheckerState* state;
            InstructionListTy& instructions;
            BlockIteratorTy current_position;

            BlockIteratorTy operator()(CondBrInstruction*);
            BlockIteratorTy operator()(BrInstruction*);
            BlockIteratorTy operator()(RetInstruction*);
            BlockIteratorTy operator()(PhiInstruction*);
            BlockIteratorTy operator()(AssignInstruction*);
            BlockIteratorTy operator()(CallFunctionInstruction*);
            BlockIteratorTy operator()(RelocateValueInstruction*);
            BlockIteratorTy operator()(NewArrayInstruction*);
            BlockIteratorTy operator()(NewPrimitiveInstruction*);
            BlockIteratorTy operator()(BorrowValueInstruction*);
            BlockIteratorTy operator()(DomainSubsetConstraint*) { debugbreak(); return current_position; }
            BlockIteratorTy operator()(DomainDependenceEdgeConstraint*) { debugbreak(); return current_position; }
            BlockIteratorTy operator()(DomainExtensionConstraint*) { debugbreak(); return current_position; }
            BlockIteratorTy operator()(DomainPhiInstruction*) { debugbreak(); return current_position; }
        };
        struct DominatorList {
            // stores pairs (a, b) where b = idom(a)
            // we can probably find all d
            std::unordered_map<BasicBlock*, BasicBlock*> idoms;
            std::vector<BasicBlock*> get_all_dominators_of(BasicBlock* input) const;
            void register_for(BasicBlock* node, BasicBlock* dominator) {
                if (idoms.contains(node)) debugbreak();
                idoms[node] = dominator;
            }
        };
        // produces unique names for each variable instance
        // used solely for the ssa related stuff
        struct ValueProducer {
            std::unordered_map<std::string, size_t> var_map;
            std::string name_for(const std::string& val);
        };
        using UseStorageTy = std::unordered_map<
            BasicBlock*,
            std::unordered_map<std::string, std::string>>;
        struct DomainCheckerState {
            // TODO make the union find
            size_t last_id = 0;
            ValueTypeMapping type_mapping;
            Domain new_domain_var();
            void register_value_base_type(const std::string& value, BorrowCheckerType&&);
            const BorrowCheckerType& get_value_type(const Value&);
            DominatorList dominators;
            BorrowCheckerFunction* func;
            BasicBlock* entry_block;

            std::unique_ptr<BorrowCheckerFunction> check_function(FunctionDeclaration* decl, IRGenerator* irgen, const FunctionSignature& sig, TypeCheckerState* stt);
            // doesn't do anything for now
            void build_dominators();
            // assigns predecessors to blocks
            void calc_block_preds();
            // transforms domain-related code to SSA
            void transform_to_ssa();
            // remove all "DomainDependenceEdge" constraints
            // and add new assignments 
            void clear_dependencies();
        };
    }
}