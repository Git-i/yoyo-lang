#include <ranges>
#include <algorithm>
#include "ir_gen.h"
namespace Yoyo
{
    
	using Object = BorrowChecker::Object;
	using Block = BorrowCheckerBlock;
	Object BorrowChecker::make_object() {
		return Object(std::to_string(counter++));
	}
	Object BorrowChecker::make_external_object()
	{
		return Object(std::to_string(counter++));
	}
	Block* BorrowChecker::make_block()
	{
		return blocks.emplace_back(new Block).get();
	}
    void Yoyo::BorrowChecker::drop_object(const std::string& obj, ASTNode * expr)
    {
        current_block->insts.push_back(
            Instruction{
                .data = obj,
                .type = Instruction::Drop,
                .expr = expr
            }
        );
    }
	void BorrowChecker::set_block(BorrowCheckerBlock* block)
	{
		current_block = block;
	}
    std::string BorrowChecker::borrow_values(std::span<const std::pair<std::string, BorrowType>> arg, Expression* expr)
    {
        std::string into = std::to_string(counter++);
        for (auto& [value, type] : arg) {
            current_block->insts.push_back(
                Instruction{
                    .data = Instruction::BorrowInstruction{
                        .into = into,
                        .operand = value
                    },
                    .type = type == BorrowType::Const ? Instruction::Borrow : Instruction::BorrowMut, 
                    .expr = expr
                }
            );
        }
        return Object(std::move(into));
    }
    
    void check_block(BorrowCheckerBlock* block, BorrowState stt, BorrowCheckerBlock* from, IRGenerator* irgen) {
        using namespace std::ranges;
        using namespace std::views;
        if (from) {
            block->preds.at(from) = std::move(stt);
        }
        if (!all_of(block->preds | values, [](auto& param) { return param.has_value(); })) {
            return;
        }
        auto state = BorrowState::aggregate(block->preds | values | views::transform([](auto& value) { return value.value();}));
        block->is_checked = true;
        for (auto& inst : block->insts) {

            if (inst.type == Instruction::Br) {
                return check_block(std::get<BorrowCheckerBlock*>(inst.data), std::move(state), block, irgen);
            }
            else if (inst.type == Instruction::CondBr) {
                auto& br_inst = std::get<std::pair<BorrowCheckerBlock*, BorrowCheckerBlock*>>(inst.data);
                check_block(br_inst.first, state, block, irgen);
                check_block(br_inst.second, state, block, irgen);
                return;
            }
            else if (inst.type == Instruction::Borrow) {
                auto& cinst = std::get<Instruction::BorrowInstruction>(inst.data);
                if (state.mutable_borrow.contains(cinst.operand)) {
                    auto elem = state.mutable_borrow.at(cinst.operand);
                    auto error = Error(inst.expr, "Attempt to borrow a value that has been mutably borrowed");
                    error.markers.emplace_back(SourceSpan{ elem->expr->beg, elem->expr->end }, "Mutable borrow occurs here");
                    irgen->error(error);
                    return;
                }
                // this instruction is where the borrow occurs
                state.immutable_borrows[cinst.operand].insert(&inst);
                // "into" borrows "operand" as immutable in this instruction
                state.is_borrowing[cinst.into].emplace_back(cinst.operand, BorrowType::Const, &inst);
            }
            else if (inst.type == Instruction::BorrowMut) {
                auto cinst = std::get<Instruction::BorrowInstruction>(inst.data);
                if (state.immutable_borrows.contains(cinst.operand)
                    || state.mutable_borrow.contains(cinst.operand)) {
                    irgen->error(Error(inst.expr, "Attempt to mutably borrow already borrowed value TODO show other borrow"));
                    return;
                }
                // this instruction is where the borrow occurs
                state.mutable_borrow[cinst.operand] = &inst;
                // "into" borrows "operand" as immutable in this instruction
                state.is_borrowing[cinst.into].emplace_back(cinst.operand, BorrowType::Mut, &inst);
            }
            else if (inst.type == Instruction::Ret) {
                // TODO lifetime
                return;
            }
            else if (inst.type == Instruction::Drop) {
                auto& var = std::get<std::string>(inst.data);
                if (state.immutable_borrows.contains(var)
                    || state.mutable_borrow.contains(var)) {
                    irgen->error(Error(inst.expr, "Attempt to drop already borrowed value TODO show other borrow"));
                    return;
                }
                // if it was borrowing it stops borrowing now
                if (state.is_borrowing.contains(var)) {
                    for (auto& borrow : state.is_borrowing.at(var)) {
                        if (std::get<1>(borrow) == BorrowType::Mut) {
                            state.mutable_borrow.erase(std::get<0>(borrow));
                        }
                        else {
                            auto& imm = state.immutable_borrows.at(std::get<0>(borrow));
                            imm.erase(std::get<2>(borrow));
                            if (imm.empty()) state.immutable_borrows.erase(std::get<0>(borrow));
                        }
                    }
                    state.is_borrowing.erase(var);
                }
            }
        }
    }
    void BorrowChecker::check_and_report(IRGenerator* irgen)
    {
        auto entry = blocks[0].get();
        make_preds();
        check_block(entry, BorrowState{}, nullptr, irgen);
    }
    void BorrowChecker::create_cond_br(BorrowCheckerBlock* success, BorrowCheckerBlock* fail)
    {
        current_block->insts.push_back(Instruction{
            .data = std::pair{success, fail},
            .type = Instruction::CondBr,
            .expr = nullptr
            });
    }
    void BorrowChecker::create_br(BorrowCheckerBlock* block)
    {
        current_block->insts.push_back(Instruction{
            .data = block,
            .type = Instruction::Br,
            .expr = nullptr
            });
    }
    void BorrowChecker::make_preds()
    {
        for (auto& block : blocks) {
            if (block->insts.empty()) continue;
            auto& back = block->insts.back();
            if (back.type == Instruction::Br) {
                std::get<BorrowCheckerBlock*>(back.data)->preds[block.get()] = {};
            }
            else if (back.type == Instruction::CondBr) {
                auto& br_inst = std::get<std::pair<BorrowCheckerBlock*, BorrowCheckerBlock*>>(back.data);
                br_inst.first->preds[block.get()] = {};
                br_inst.second->preds[block.get()] = {};
            }
        }
    }
    std::string BorrowChecker::move_object(const std::string& from, Expression* expr)
    {
        auto obj = make_object();
        move_into(from, obj, expr);
        return obj;
    }
    void BorrowChecker::move_into(const std::string& from, const std::string& to, Expression* expr)
    {
        current_block->insts.push_back(Instruction{
            .data = std::pair{from, to},
            .type = Instruction::Move,
            .expr = expr
            });
    }
    std::string BorrowChecker::borrow_as_temporary(const std::string& value, Expression* expr)
    {
        std::string into = make_object();
        current_block->insts.push_back(
            Instruction{
                .data = Instruction::BorrowInstruction{
                    .into = into,
                    .operand = value
                },
                .type = Instruction::BorrowTemp,
                .expr = expr
            }
        );
        return into;
    }
}
