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
	// Private by default, might need to specify public
	public:
		struct MallocArgs
		{
			// Union because the argument is either constant or not
			// If not, we will store the value in a newly alloc'd variable
			union
			{
				ConstantInt* constArg;
				Instruction* allocaInst; // will be an allocInst
			};
			bool isConstantArg;
		};

	public:
		FunctionManager(Module *pMod, TypeManager *pTypeManager,
				GlobalVariable *freeMemBlockHead, GlobalVariable *haveAllocedMem,
				GlobalVariable *ptrToHeap);
		Instruction* replaceMallocWithMmap(Instruction *inst);
		Function* getMmapFunction();
		bool isMallocCall(CallInst *callInst);
		bool isFreeCall(CallInst *callInst);
		bool isMmapCall(CallInst *callInst);
		MallocArgs extractMallocArgs(CallInst *callInst);

		CallInst* insertAddMemoryBlockCall(/*InsertBefore */Instruction *inst, Value *param); // for testing
		CallInst* insertPrintfCall(Value *val, bool printPtr, /*InsertBefore*/ Instruction *inst);
		void testFunction();

	//members
	private:
		Function *m_pFuncMmap;
		Function *m_pFuncPrintf;
		Module *m_pMod;
		TypeManager* m_pTypeManager;

		// custom malloc functions
		Function *m_pFuncAddMemBlock;

		// Globals we need access to (makes sense to put them here?)
	    GlobalVariable *m_pFreeMemBlockHead;
	    GlobalVariable *m_pHaveAllocedMem;
	    GlobalVariable *m_pPtrToHeap;

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


};



#endif /* LIB_TRANSFORMS_SANDBOXWRITES_FUNCTIONMANAGER_HPP_ */
