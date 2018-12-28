/*
 * SandboxWrites.cpp
 */

#include "FunctionManager.hpp"
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/MemoryLocation.h"
using namespace llvm;

namespace {
  struct SandboxWritesPass : public ModulePass {
    static char ID;
    SandboxWritesPass() : ModulePass(ID) {}
    virtual bool runOnModule(Module &M);
    void SandBoxWrites(Module *pMod, StoreInst* inst, Function::iterator *BB,
    		Value* upperBound, Value* lowerBound);

  };
}



bool SandboxWritesPass::runOnModule(Module &M)
{
	FunctionManager funcManager(&M);
	for (Module::iterator F = M.begin(), ME = M.end(); F != ME; ++F)
	{
		for (Function::iterator BB = F->begin(), FE = F->end(); BB != FE; ++BB)
		{
			for (BasicBlock::iterator Inst = BB->begin(), BBE = BB->end();
					Inst != BBE; ++Inst)
			{

/*

				// every time we allocate memory we want to store
				// the memory address of the allocated memory
				if (isa<AllocaInst>(Inst))
				{
					//AllocaInst* inst = dyn_cast<AllocaInst>(Inst);


					// Test for mmap:
					// 1. Have pointer variable point to new mmaped memory
					// 2. Assign a value to the memory
					// 3. Print from the actual source file
					LoadInst* ptr_23 = new LoadInst(ptr_test, "", false, inst);
					ptr_23->setAlignment(8);
					ConstantInt* const_int32_99 = ConstantInt::get(M.getContext(),
							APInt(64, StringRef("999"), 10));
					StoreInst* void_24 = new StoreInst(const_int32_99, ptr_23, false, inst);
					void_24->setAlignment(4);

					Inst++;
		    		StoreInst *store_inst = new StoreInst(ptr_23, inst,
		    				dyn_cast<Instruction>(Inst));
		    		Inst--;

				}

				if (isa<StoreInst>(Inst))
				{
					StoreInst* inst = dyn_cast<StoreInst>(Inst);
					//SandBoxWrites(&M, inst, &BB, NULL, NULL);

					// Break since current iterator is invalidated after
					// we split a basic block.
					//break;
				}
*/
				if (isa<CallInst>(Inst))
				{
					CallInst *callInst = dyn_cast<CallInst>(Inst);
					funcManager.isMmapCall(callInst);
					if (funcManager.isMallocCall(callInst))
					{
						FunctionManager::MallocArgs args = funcManager.extractMallocArgs(callInst);
						Instruction* newInst = funcManager.replaceMallocWithMmap(callInst);
						CallInst* mmapCall = dyn_cast<CallInst>(newInst);
						Value* v = mmapCall->getCalledValue();
						Value* sv = v->stripPointerCasts();
						errs() << "-------INSERTED MMAP CALL-------\n";
						errs() << *mmapCall;
						errs() << "\n";
						errs() << *v;
						errs() << "\n";
						errs() << *sv;
						errs() << "\n";
						errs() << mmapCall->getCallingConv();
						errs() << "\n";
						errs() << mmapCall->getTailCallKind();
						errs() << "\n";
						BasicBlock::iterator BI(newInst);
						Inst = BI;
					}
				}
			}
		}
	}
	return true;
}

/*** Function summary - SandboxWritesPass::SandBoxWrites ***
Takes in a module and an instruction, and inserts a call to mmap()
before the given instruction.

@Inputs:
- pMod: pointer to a Module
- inst: pointer to a (store) instruction
- BB: address of a basic block iterator
- upperBound: upper address bound to allow memory writes
- lowerBound: lower address bound to allow memory writes

@brief
An if statement is wrapped around the (store) instruction, and the
write only takes place if it is within the upper/lower bound address
range

@Outputs:
- none
*/
// TODO: Currently not using arguments upperBound and lowerBound
// Eventually these will be used for the address bounds, and
// not the dummy address range defined inside the function
void SandboxWritesPass::SandBoxWrites(Module *pMod, StoreInst* inst, Function::iterator *BB,
		Value* upperBound, Value* lowerBound)
{
	// For now use void ptr type to store memory addresses
	PointerType* voidPtrType = PointerType::get(IntegerType::get(pMod->getContext(), 8), 0);

	// allocate memory to store upper and lower address bounds
	AllocaInst* ptrToMemoryAddrTop = new AllocaInst(voidPtrType, 0, nullptr,
			8, "addrRangeTop", inst);
	AllocaInst* ptrToMemoryAddrBot = new AllocaInst(voidPtrType, 0, nullptr,
			8, "addrRangeBot", inst);

	// this is the address that will be compared (i.e. is it > and < some range)
	CastInst *intAddrToVoid = new BitCastInst(inst->getOperand(1), voidPtrType,
			"", inst);

	// Store the upper and lower address bounds in the allocated memory
	StoreInst *upperAddressRange = new StoreInst(intAddrToVoid, ptrToMemoryAddrTop,
			inst);
	StoreInst *lowerAddressRange = new StoreInst(intAddrToVoid, ptrToMemoryAddrBot,
								inst);
	// Comparison variables (TODO: currently dummy for testing purposes)
	LoadInst *upperAddrBound = new LoadInst(/*upperAddressRange->getOperand(1),*/
			ptrToMemoryAddrTop, "", false, inst);
	LoadInst *lowerAddrBound = new LoadInst(/*lowerAddressRange->getOperand(1),*/
			ptrToMemoryAddrBot, "", false, inst);

	// First if statement (i.e. if address >= X)
	ICmpInst *cmpInst = new ICmpInst(inst,
			CmpInst::Predicate::ICMP_SGE, intAddrToVoid, lowerAddrBound, "");
	TerminatorInst *outerIfTerm = SplitBlockAndInsertIfThen(cmpInst,
			inst, false);
	BasicBlock* outerIfBB = outerIfTerm->getParent();

	// Second if statement (i.e. if address <= Y)
	ICmpInst *cmpInst2 = new ICmpInst(outerIfTerm,
			CmpInst::Predicate::ICMP_SLE, intAddrToVoid, upperAddrBound, "");
	BasicBlock* innerIfBB = outerIfBB->splitBasicBlock(outerIfTerm->getIterator());

	// BB iterator is new pointing at the "Tail" of the original BasicBlock that was split:
	// Head > If (then), else goto Tail > if (then), else goto Tail > Tail
	*BB = inst->getParent()->getIterator();

	// the TerminatorInst of the outerIfBlock changed after we split it
	outerIfTerm = outerIfBB->getTerminator();
	TerminatorInst *newOuterIfTerm = BranchInst::Create(/*ifTrue*/innerIfBB,
			/*ifFalse*/dyn_cast<BasicBlock>(*BB), cmpInst2);
	ReplaceInstWithInst(outerIfTerm, newOuterIfTerm);

	// On the next iteration of the for loop, BB iterator will increment,
	// so we decrement here since we want to apply this same algorithm
	// on other store instructions in the "Tail" basic block
	// (before the decrement BB was pointing to Tail)
	(*BB)--;
	inst->removeFromParent();
	inst->insertBefore(innerIfBB->getTerminator());

	// For testing, write a value to the variable  that we
	// put inside the "if" statement
	Constant *const_int = ConstantInt::get(Type::getInt32Ty(pMod->getContext()),
			1234, true);

	StoreInst *store_inst2 = new StoreInst(const_int, inst->getOperand(1),
			innerIfBB->getTerminator());

}

char SandboxWritesPass::ID = 0;

// Automatically enable the pass.
// http://adriansampson.net/blog/clangpass.html
static void registerSandboxWritesPass(const PassManagerBuilder &,
                         legacy::PassManagerBase &PM) {
  PM.add(new SandboxWritesPass());
}
static RegisterStandardPasses
  RegisterMyPass(PassManagerBuilder::EP_ModuleOptimizerEarly,
                 registerSandboxWritesPass);

static RegisterStandardPasses
    RegisterMyPass0(PassManagerBuilder::EP_EnabledOnOptLevel0, registerSandboxWritesPass);
