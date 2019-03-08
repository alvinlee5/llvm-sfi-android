/*
 * SandboxWrites.cpp
 */

#include "FunctionManager.hpp"
#include "TypeManager.hpp"
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
    SandboxWritesPass() : ModulePass(ID)
    {
    	m_pFreeMemBlockHead = NULL;
    	m_pHaveAllocedMem = NULL;
    	m_pPtrToHeap = NULL;
    	m_pSizeOfHeap = NULL;
    	m_pStackTop = NULL;
    	m_pStackBot = NULL;
    	m_pPtrToHeapTop = NULL;
    }
    virtual bool runOnModule(Module &M);
    void SandBoxWrites(Module *pMod, StoreInst* inst, Function::iterator *BB,
    		Value* upperBound, Value* lowerBound);
    void SandBoxWritesV3(Module *pMod, StoreInst* inst, Function::iterator *BB,
    		Value *heapLowerBound, Value *heapUpperBound, Value *stackTop, Value *stackBot);
    void InsertGlobalVars(Module *pMod, TypeManager* typeManager);
    void GetSFIRegion(LoadInst** heapLower, LoadInst** heapUpper,
    		LoadInst** stackBot, LoadInst** stackTop, StoreInst* inst);
    Instruction* UpdateStackPointers(AllocaInst* allocaInst, TypeManager *pTm/*, FunctionManager* pFm*/);

    // Make inserted globals members for now for easy access
    GlobalVariable *m_pFreeMemBlockHead;
    GlobalVariable *m_pHaveAllocedMem;
    GlobalVariable *m_pPtrToHeap;
    GlobalVariable *m_pPtrToHeapTop;
    GlobalVariable *m_pSizeOfHeap;
    GlobalVariable *m_pStackTop;
    GlobalVariable *m_pStackBot;
  };
}

bool SandboxWritesPass::runOnModule(Module &M)
{
	int count = 0;
	TypeManager typeManager (&M);
	InsertGlobalVars(&M, &typeManager);
	FunctionManager funcManager(&M, &typeManager, m_pFreeMemBlockHead, m_pHaveAllocedMem,
			m_pPtrToHeap, m_pPtrToHeapTop);
	for (Module::iterator F = M.begin(), ME = M.end(); F != ME; ++F)
	{
		Function *func = dyn_cast<Function>(F);
		StringRef funcName1("llvm_add_memory_block");
		StringRef funcName2("llvm_split_memory_block");
		StringRef funcName3("llvm_remove_memory_block");
		StringRef funcName4("llvm_malloc");
		StringRef funcName5("llvm_scan_merge");
		StringRef funcName6("llvm_free");

		if ((func->getName()).equals(funcName1)||
				(func->getName()).equals(funcName2)||
				(func->getName()).equals(funcName3)||
				(func->getName()).equals(funcName4) ||
				(func->getName()).equals(funcName5) ||
				(func->getName()).equals(funcName6))
		{
			// We don't want to instrument on our own inserted functions.
			// We don't want to instrument on system calls either. Even though
			// the function system calls will be looped through here (if used)
			// it seems that LLVM doesn't have the BB's or instructions to loop
			// through. This makes sense since we only compile our own source code,
			// and not source code which implements system calls like printf.
			continue;
		}
		count++;
		//errs()<<count<<": "<<func->getName()<<"\n";
		for (Function::iterator BB = F->begin(), FE = F->end(); BB != FE; ++BB)
		{
			//errs()<<"New BB \n";
			for (BasicBlock::iterator Inst = BB->begin(), BBE = BB->end();
					Inst != BBE; ++Inst)
			{


				// every time we allocate memory we want to store
				// the memory address of the allocated memory
				if (isa<AllocaInst>(Inst))
				{
					AllocaInst *allocaInst = dyn_cast<AllocaInst>(Inst);
					Instruction* finalInst = UpdateStackPointers(allocaInst, &typeManager);

					// We absolutely don't want to instrument on the code
					// we've inserted ourselves, so skip all basic blocks
					// and instructions we've just inserted
					BB = finalInst->getParent()->getIterator();
					Inst = finalInst->getIterator();
					BBE = BB->end();
					continue;

				}

				if (isa<StoreInst>(Inst))
				{
					StoreInst *inst = dyn_cast<StoreInst>(Inst);
					LoadInst *heapLower;
					LoadInst *heapUpper;
					LoadInst *stackBot;
					LoadInst *stackTop;
					GetSFIRegion(&heapLower, &heapUpper, &stackBot, &stackTop, inst);
					SandBoxWritesV3(&M, inst, &BB, heapLower, heapUpper, stackTop, stackBot);

					// Break since current iterator is invalidated after
					// we split a basic block.
					break;
				}

				if (isa<CallInst>(Inst))
				{
					CallInst *callInst = dyn_cast<CallInst>(Inst);
					if (funcManager.isMallocCall(callInst))
					{
						errs() << "LLVM_Malloc\n";
						Value *args = funcManager.
								extractMallocArgs(callInst);
						Instruction* newInst = funcManager.replaceMallocWithMalloc(callInst, args);
						BasicBlock::iterator BI(newInst);
						Inst = BI;

					}
					if (funcManager.isFreeCall(callInst))
					{
						errs() << "LLVM_Free\n";
						Value *args = funcManager.extractFreeArgs(callInst);
						Instruction* newInst = funcManager.replaceFreeWithFree(callInst, args);
						BasicBlock::iterator BI(newInst);
						Inst = BI;
					}
				}
			}
		}
	}
	return true;
}

Instruction* SandboxWritesPass::UpdateStackPointers(AllocaInst* allocaInst, TypeManager *pTm/*, FunctionManager* pFm*/)
{
	Instruction *nextInst = allocaInst->getNextNode();
	LoadInst *loadStackBot = new LoadInst(m_pStackBot, "", false, nextInst);
	loadStackBot->setAlignment(4);
	ICmpInst *cmpInst = new ICmpInst(nextInst,
			CmpInst::Predicate::ICMP_EQ, loadStackBot,
			pTm->GetVoidPtrNull(), "");

	TerminatorInst *ifTerm = SplitBlockAndInsertIfThen(cmpInst,
			nextInst, false);

	// *** Instructions inside the If statement ***
	CastInst *castToVoidPtr = new BitCastInst(allocaInst,
			pTm->GetVoidPtrType(),
			"", ifTerm);
	StoreInst *storeStackAddr = new StoreInst(castToVoidPtr, m_pStackBot,
			false, ifTerm);
	storeStackAddr->setAlignment(4);

/*	// *** Used for testing ***
	LoadInst *load = new LoadInst(m_pStackBot, "", false, ifTerm);
	pFm->insertPrintfCall(load, true, ifTerm);*/

	// *** Instructions inside the If statement END ***

	CastInst *castToVoidPtr2 = new BitCastInst(allocaInst,
			pTm->GetVoidPtrType(),
			"", nextInst);
	StoreInst *storeStackAddr2 = new StoreInst(castToVoidPtr2, m_pStackTop,
			false, nextInst);
	storeStackAddr2->setAlignment(4);

/*	// ***Used for testing***
	LoadInst *load2 = new LoadInst(m_pStackTop, "", false, nextInst);
	pFm->insertPrintfCall(load2, true, nextInst);*/

	// Return the last inserted StoreInst
	// This instruction will be used to update the
	// basic block and instruction iterators so we
	// don't instrument our own inserted code.
	return storeStackAddr2;
}

/*** Function summary - SandboxWritesPass::GetHeapRegion ***
Takes in a module and a StoreInst and inserts instructions
to compute the upper and lower address range of the SFI
heap region and returns those values.

@Inputs:
- inst: pointer to a (store) instruction

@brief
An if statement is wrapped around the (store) instruction, and the
write only takes place if it is within the upper/lower bound address
range

@Outputs:
- lowerBound: lower bound of heap address range to be passed to SandBoxWrites()
- upperBound: upper bound of heap address range to be passed to SandBoxWrites()
*/
void SandboxWritesPass::GetSFIRegion(LoadInst** heapLower, LoadInst** heapUpper,
		LoadInst** stackBot, LoadInst** stackTop, StoreInst* inst)
{
	LoadInst* loadHeapLower = new LoadInst(m_pPtrToHeap, "", false, inst);
	loadHeapLower->setAlignment(4);

	LoadInst* loadHeapUpper = new LoadInst(m_pPtrToHeapTop, "", false, inst);
	loadHeapUpper->setAlignment(4);

	LoadInst* loadStackBot = new LoadInst(m_pStackBot, "", false, inst);
	loadStackBot->setAlignment(4);

	LoadInst* loadStackTop = new LoadInst(m_pStackTop, "", false, inst);
	loadStackTop->setAlignment(4);

	*heapLower = loadHeapLower;
	*heapUpper = loadHeapUpper;
	*stackBot = loadStackBot;
	*stackTop = loadStackTop;
}

/*** Function summary - SandboxWritesPass::SandBoxWritesV3 ***
Wrap an "if" statement around a store instruction. Only allow the
write if the address being written to is within the SFI heap and
stack range.

@Inputs:
- pMod: pointer to a Module
- storeInst: pointer to a (store) instruction
- BB: address of a basic block iterator
- heapLowerBound: lower address bound to allow memory writes (heap)
- heapUpperBound: upper address bound to allow memory writes (heap)
- stackTop: top of stack (used as lower bound)
- stackBot: "bottom" of stack (used as upper bound for stack)

@brief
if ((address >= heapLowerBound && address <= heapUpperBound) ||
	(address >= stackTop && address <= stackBot))

@Outputs:
- none
*/

void SandboxWritesPass::SandBoxWritesV3(Module *pMod, StoreInst* storeInst,
		Function::iterator *BB, Value *heapLowerBound, Value *heapUpperBound,
		Value *stackTop, Value *stackBot)
{
	TerminatorInst *thenTermHeapLower;
	TerminatorInst *elseTermHeapLower;

	// This BB is where we will allow the write.
	// Also this terminator (should) branch to thenTermHeapLower
	TerminatorInst *thenTermHeapUpper;

	TerminatorInst *thenTermStackBot;
	TerminatorInst *thenTermStackTop;

	// For now use void ptr type to store memory addresses
	PointerType* voidPtrType = PointerType::get(IntegerType::get(pMod->getContext(), 8), 0);

	// this is the address that will be compared (i.e. is it > and < some range)
	CastInst *intAddrToVoid = new BitCastInst(storeInst->getOperand(1), voidPtrType,
			"", storeInst);

	// First if statement (i.e. if address >= heapLowerBound)
	ICmpInst *cmpHeapLower = new ICmpInst(storeInst,
			CmpInst::Predicate::ICMP_SGE, intAddrToVoid, heapLowerBound, "");
	SplitBlockAndInsertIfThenElse(cmpHeapLower, storeInst,
			&thenTermHeapLower, &elseTermHeapLower);

	// Second if statement (if address <= heapUpperBound)
	ICmpInst *cmpHeapUpper = new ICmpInst(thenTermHeapLower,
			CmpInst::Predicate::ICMP_SLE, intAddrToVoid, heapUpperBound, "");
	thenTermHeapUpper = SplitBlockAndInsertIfThen(cmpHeapUpper, thenTermHeapLower,
			false);

	// Change terminator inst of if (heapUpper) to branch to originalTail
	// If (heapLower) -> if (heapUpper) -> originalTail
	BasicBlock *originalTail = storeInst->getParent();
	TerminatorInst *branchToOriginalTail = BranchInst::Create(originalTail);
	ReplaceInstWithInst(thenTermHeapUpper, branchToOriginalTail);

	// Branch to else (heapLower) if the 2nd if statement fails [If (!heapUpper)]
	BasicBlock *elseHeapLowerBB = elseTermHeapLower->getParent();
	TerminatorInst *newThenTermHeapLower = BranchInst::Create(elseHeapLowerBB);
	ReplaceInstWithInst(thenTermHeapLower, newThenTermHeapLower);

	// If address <= stackBot
	ICmpInst *cmpStackBot = new ICmpInst(elseTermHeapLower,
			CmpInst::Predicate::ICMP_SLE, intAddrToVoid, stackBot, "");
	thenTermStackBot = SplitBlockAndInsertIfThen(cmpStackBot, elseTermHeapLower,
			false);

	TerminatorInst *branchToOriginalTail2 = BranchInst::Create(originalTail);
	ReplaceInstWithInst(elseTermHeapLower, branchToOriginalTail2);

	// If address >= stackTop
	ICmpInst *cmpStackTop = new ICmpInst(thenTermStackBot,
			CmpInst::Predicate::ICMP_SGE, intAddrToVoid, stackTop, "");
	thenTermStackTop = SplitBlockAndInsertIfThen(cmpStackTop, thenTermStackBot,
			false);
	TerminatorInst *branchToStore = BranchInst::Create(branchToOriginalTail->getParent());
	ReplaceInstWithInst(thenTermStackTop, branchToStore);

	TerminatorInst *branchToOriginalTail3 = BranchInst::Create(originalTail);
	ReplaceInstWithInst(thenTermStackBot, branchToOriginalTail3);

	// On the next iteration of the for loop, BB iterator will increment,
	// so we decrement here since we want to apply this same algorithm
	// on other store instructions in the "Tail" basic block
	// (before the decrement BB was pointing to Tail)
	*BB = storeInst->getParent()->getIterator();
	(*BB)--;

	// Do this last: Insert the storeInst before the 2nd if-then terminator
	storeInst->removeFromParent();
	storeInst->insertBefore(branchToOriginalTail);
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

void SandboxWritesPass::InsertGlobalVars(Module *pMod, TypeManager* typeManager)
{
	/*Head of free memory linked list*/
	m_pFreeMemBlockHead = new GlobalVariable(/*Module=*/*pMod,
			 /*Type=*/typeManager->GetFreeMemBlockPtTy(),
			 /*isConstant=*/false,
			 /*Linkage=*/GlobalValue::ExternalLinkage,
			 /*Initializer=*/0, // has initializer, specified below
			 /*Name=*/"llvm_head");
	m_pFreeMemBlockHead->setAlignment(4);
	m_pFreeMemBlockHead->setInitializer(typeManager->GetFreeMemBlockNull());

	/*Boolean to keep track if mem has been alloced*/
	m_pHaveAllocedMem = new GlobalVariable(/*Module=*/*pMod,
			 /*Type=*/IntegerType::get(pMod->getContext(), 32),
			 /*isConstant=*/false,
			 /*Linkage=*/GlobalValue::ExternalLinkage,
			 /*Initializer=*/0, // has initializer, specified below
			 /*Name=*/"llvm_haveAllocedMem");
	m_pHaveAllocedMem->setAlignment(4);
	ConstantInt* const_int32_val0 = ConstantInt::get(pMod->getContext(),
			APInt(32, StringRef("0"), 10));
	m_pHaveAllocedMem->setInitializer(const_int32_val0);

	/* Size of sfi heap*/
	m_pSizeOfHeap = new GlobalVariable(/*Module=*/*pMod,
	/*Type=*/IntegerType::get(pMod->getContext(), 32),
	/*isConstant=*/false,
	/*Linkage=*/GlobalValue::ExternalLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_sizeOfHeap");
	m_pSizeOfHeap->setAlignment(4);
	ConstantInt* const_int64_22 = ConstantInt::get(pMod->getContext(),
		 APInt(32, StringRef("20480"), 10));
	m_pSizeOfHeap->setInitializer(const_int64_22);

	/* Pointer to beginning of sfi heap */
	PointerType* voidPtrType = PointerType::get(IntegerType::get(pMod->getContext(), 8), 0);
	m_pPtrToHeap = new GlobalVariable(/*Module=*/*pMod,
	/*Type=*/voidPtrType,
	/*isConstant=*/false,
	/*Linkage=*/GlobalValue::ExternalLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_ptrToHeap");
	m_pPtrToHeap->setAlignment(4);
	ConstantPointerNull* nullForVoidPtr = ConstantPointerNull::get(voidPtrType);
	m_pPtrToHeap->setInitializer(nullForVoidPtr);

	m_pPtrToHeapTop = new GlobalVariable(/*Module=*/*pMod,
	/*Type=*/voidPtrType,
	/*isConstant=*/false,
	/*Linkage=*/GlobalValue::ExternalLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_ptrToHeapTop");
	m_pPtrToHeapTop->setAlignment(4);
	m_pPtrToHeapTop->setInitializer(nullForVoidPtr);

	m_pStackTop = new GlobalVariable(/*Module=*/*pMod,
	/*Type=*/voidPtrType,
	/*isConstant=*/false,
	/*Linkage=*/GlobalValue::ExternalLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_ptrToStackTop");
	m_pStackTop->setAlignment(4);
	m_pStackTop->setInitializer(nullForVoidPtr);

	m_pStackBot = new GlobalVariable(/*Module=*/*pMod,
	/*Type=*/voidPtrType,
	/*isConstant=*/false,
	/*Linkage=*/GlobalValue::ExternalLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_ptrToStackBot");
	m_pStackBot->setAlignment(4);
	m_pStackBot->setInitializer(nullForVoidPtr);

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
