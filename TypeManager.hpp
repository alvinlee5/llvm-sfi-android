/*
 * TypeManager.hpp
 */

#ifndef LIB_TRANSFORMS_SANDBOXWRITES_TYPEMANAGER_HPP_
#define LIB_TRANSFORMS_SANDBOXWRITES_TYPEMANAGER_HPP_

#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
using namespace llvm;

// This class will manage/hold the "non basic" types which are inserted
// into the source code via instrumentation / a pass.

class TypeManager
{
	public:
		TypeManager(Module *mod);
		void InitFreeMemBlockTy();
		PointerType* GetFreeMemBlockPtTy();
		StructType* GetFreeMemBlockStructTy();
		ConstantPointerNull* GetFreeMemBlockNull();

		PointerType* GetVoidPtrType();
		ConstantPointerNull* GetVoidPtrNull();

	private:
		Module *m_pMod;
		PointerType *m_pFreeMemBlockPtTy;
		StructType *m_pFreeMemBlockStructTy;
		ConstantPointerNull *m_pFreeMemBlockNull;
		PointerType *m_pVoidPtrType;
		ConstantPointerNull *m_pNullForVoidPtr;
};

#endif /* LIB_TRANSFORMS_SANDBOXWRITES_TYPEMANAGER_HPP_ */
