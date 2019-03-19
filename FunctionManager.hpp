/*
 * FunctionManager.hpp
 */

#include "TypeManager.hpp"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
using namespace llvm;


#ifndef LIB_TRANSFORMS_SANDBOXWRITES_FUNCTIONMANAGER_HPP_
#define LIB_TRANSFORMS_SANDBOXWRITES_FUNCTIONMANAGER_HPP_

class FunctionManager
{
	public:
		FunctionManager(Module *pMod, TypeManager *pTypeManager,
				GlobalVariable *freeMemBlockHead, GlobalVariable *haveAllocedMem,
				GlobalVariable *ptrToHeap,
				GlobalVariable *ptrToHeapTop);

		// mmap related functions
		Instruction* replaceMallocWithMmap(Instruction *inst);
		Function* getMmapFunction();

		// Malloc related calls
		bool isMallocCall(CallInst *callInst);
		bool isFreeCall(CallInst *callInst);
		bool isNewCall(CallInst *callInst);
		bool isDeleteCall(CallInst *callInst);
		bool isMmapCall(CallInst *callInst);
		Value* extractMallocArgs(CallInst *callInst);
		Value* extractFreeArgs(CallInst *callInst);

		// custom malloc functions
		CallInst* insertAddMemoryBlockCall(/*InsertBefore */Instruction *inst, Value *param); // for testing
		CallInst* replaceMallocWithMalloc(Instruction *inst, Value *sizeToAlloc);
		CallInst* replaceFreeWithFree(Instruction *inst, Value *ptrToMemoryToFree);

		CallInst* insertPrintfCall(Value *val, bool printPtr, /*InsertBefore*/ Instruction *inst);


	//members
	private:
		Function *m_pFuncMmap;
		Function *m_pFuncPrintf;
		Module *m_pMod;
		TypeManager* m_pTypeManager;

		// custom malloc functions
		Function *m_pFuncAddMemBlock;
		Function *m_pFuncSplitMemBlock;
		Function *m_pFuncRemovemMemBlock;
		Function *m_pFuncMalloc;
		Function *m_pFuncScanMerge;
		Function *m_pFuncFree;

		// Globals we need access to (makes sense to put them here?)
	    GlobalVariable *m_pFreeMemBlockHead;
	    GlobalVariable *m_pHaveAllocedMem;
	    GlobalVariable *m_pPtrToHeap;
	    GlobalVariable *m_pPtrToHeapTop;

	    // Only needed in FunctionManager, so not defined in
	    // SandboxWrites
	    GlobalVariable *m_pPrintfStrPtr;
	    GlobalVariable *m_pPrintfStrInt;

	// helpers
	private:
		void declareMmap();
		void declarePrintf();

		void declareAddMemoryBlock();
		void defineAddMemoryBlock();

		void declareSplitMemBlock();
		void defineSplitMemBlock();

		void declareRemoveMemBlock();
		void defineRemoveMemBlock();

		void declareMalloc();
		void defineMalloc();

		void declareScanMerge();
		void defineScanMerge();

		void declareFree();
		void defineFree();
};



#endif /* LIB_TRANSFORMS_SANDBOXWRITES_FUNCTIONMANAGER_HPP_ */
