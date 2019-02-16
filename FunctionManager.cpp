/*
 * FunctionManager.cpp
 */
#include "FunctionManager.hpp"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CallSite.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <stdio.h>

using namespace llvm;

FunctionManager::FunctionManager(Module* pMod, TypeManager *pTypeManager,
		GlobalVariable *freeMemBlockHead, GlobalVariable *haveAllocedMem,
		GlobalVariable *ptrToHeap)
: m_pMod(pMod), m_pTypeManager(pTypeManager), m_pFreeMemBlockHead(freeMemBlockHead),
  m_pHaveAllocedMem(haveAllocedMem), m_pPtrToHeap(ptrToHeap)
{
	declareMmap();
	//declarePrintf();

	declareAddMemoryBlock();
	defineAddMemoryBlock();

	// Initialize Global variables (global strings for printf)
	// Probably should put this in a function call
	ArrayType* ArrayTy_13Elements = ArrayType::get(IntegerType::get(m_pMod->getContext(), 8), 13);
	m_pPrintfStrPtr = new GlobalVariable(/*Module=*/*m_pMod,
	/*Type=*/ArrayTy_13Elements,
	/*isConstant=*/true,
	/*Linkage=*/GlobalValue::PrivateLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_.str");
	m_pPrintfStrPtr->setAlignment(1);
	Constant *printPtrStr = ConstantDataArray::getString(m_pMod->getContext(),
			"Pointer: %p\x0A", true);
	m_pPrintfStrPtr->setInitializer(printPtrStr);

	ArrayType* ArrayTy_11Elements = ArrayType::get(IntegerType::get(m_pMod->getContext(), 8), 11);
	m_pPrintfStrInt = new GlobalVariable(/*Module=*/*m_pMod,
	/*Type=*/ArrayTy_11Elements,
	/*isConstant=*/true,
	/*Linkage=*/GlobalValue::PrivateLinkage,
	/*Initializer=*/0, // has initializer, specified below
	/*Name=*/"llvm_.str.1");
	m_pPrintfStrInt->setAlignment(1);
	Constant *printIntStr = ConstantDataArray::getString(m_pMod->getContext(),
			"Value: %d\x0A", true);
	m_pPrintfStrInt->setInitializer(printIntStr);
}

void FunctionManager::declareMmap()
{
	// Put function arguments into vector
	 std::vector<Type*> mmapFuncParams;
	 PointerType* voidPtrType =
			 PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	 mmapFuncParams.push_back(voidPtrType);
	 mmapFuncParams.push_back(IntegerType::get(m_pMod->getContext(), 32));
	 mmapFuncParams.push_back(IntegerType::get(m_pMod->getContext(), 32));
	 mmapFuncParams.push_back(IntegerType::get(m_pMod->getContext(), 32));
	 mmapFuncParams.push_back(IntegerType::get(m_pMod->getContext(), 32));
	 mmapFuncParams.push_back(IntegerType::get(m_pMod->getContext(), 32));

	 // Create the function type, used for creating the function
	 // specifies return type, parameters, variable arguments
	 FunctionType* mmapFuncType = FunctionType::get(
	  /*Result=*/voidPtrType,
	  /*Params=*/mmapFuncParams,
	  /*isVarArg=*/false);

	 // Get or create function:
	 // If the function is already in the modules symbol table, we can just get it.
	 // Otherwise it must be declared for use (i.e. created)
	 m_pFuncMmap = m_pMod->getFunction("mmap");
	 if (!m_pFuncMmap)
	 {
		 errs() << "mmap doesn't exist in the symbol table...?\n";
		 m_pFuncMmap = Function::Create(
				  /*Type=*/mmapFuncType,
				  /*Linkage=*/GlobalValue::ExternalLinkage,
				  /*Name=*/"mmap", m_pMod); // (external, no body)
		 m_pFuncMmap->setCallingConv(CallingConv::C);
	 }

	 AttributeList func_mmap_PAL;
	 {
	  SmallVector<AttributeList, 4> Attrs;
	  AttributeList PAS;
	   {
	    AttrBuilder B;
	    B.addAttribute(Attribute::NoUnwind);
	    PAS = AttributeList::get(m_pMod->getContext(), ~0U, B);
	   }

	  Attrs.push_back(PAS);
	  func_mmap_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	 }
	 m_pFuncMmap->setAttributes(func_mmap_PAL);
}

void FunctionManager::declarePrintf()
{
	PointerType* voidPtrType = PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	std::vector<Type*>printfParams;
	printfParams.push_back(voidPtrType);
	FunctionType* printfFuncType = FunctionType::get(
	/*Result=*/IntegerType::get(m_pMod->getContext(), 32),
	/*Params=*/printfParams,
	/*isVarArg=*/true);

	m_pFuncPrintf = m_pMod->getFunction("printf");
	if (!m_pFuncPrintf)
	{
		m_pFuncPrintf = Function::Create(
		/*Type=*/printfFuncType,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"printf", m_pMod); // (external, no body)
		m_pFuncPrintf->setCallingConv(CallingConv::C);
	}
	AttributeList func_printf_PAL;
	{
		SmallVector<AttributeList, 4> Attrs;
		AttributeList PAS;
		{
			AttrBuilder B;
			PAS = AttributeList::get(m_pMod->getContext(), ~0U, B);
		}
		Attrs.push_back(PAS);
		func_printf_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	}
	m_pFuncPrintf->setAttributes(func_printf_PAL);
}

CallInst* FunctionManager::insertPrintfCall(Value *val, bool printPtr, /*InsertBefore*/ Instruction *inst)
{
	ConstantInt* const_val_0 = ConstantInt::get(m_pMod->getContext(),
			APInt(32, StringRef("0"), 10));

	// TODO: The if statements below are duplicated, only need to change constPtrToStr
	if (printPtr)
	{
		// print ptr variable
		std::vector<Constant*> ptrIndices;
		ptrIndices.push_back(const_val_0);
		ptrIndices.push_back(const_val_0);
		Constant* constPtrToStr = ConstantExpr::getGetElementPtr(nullptr, m_pPrintfStrPtr, ptrIndices);

		std::vector<Value*> printf_params;
		printf_params.push_back(constPtrToStr);
		printf_params.push_back(val);
		CallInst* printfCall = CallInst::Create(m_pFuncPrintf, printf_params, "", inst);
		printfCall->setCallingConv(CallingConv::C);
		printfCall->setTailCall(false);
		AttributeList printfCall_PAL;
		printfCall->setAttributes(printfCall_PAL);	// probably not necessary
		return printfCall;
	}
	else
	{
		// print int variable
		std::vector<Constant*> ptrIndices;
		ptrIndices.push_back(const_val_0);
		ptrIndices.push_back(const_val_0);
		Constant* constPtrToStr = ConstantExpr::getGetElementPtr(nullptr, m_pPrintfStrInt, ptrIndices);

		std::vector<Value*> printf_params;
		printf_params.push_back(constPtrToStr);
		printf_params.push_back(val);
		CallInst* printfCall = CallInst::Create(m_pFuncPrintf, printf_params, "", inst);
		printfCall->setCallingConv(CallingConv::C);
		printfCall->setTailCall(false);
		AttributeList printfCall_PAL;
		printfCall->setAttributes(printfCall_PAL);
		return printfCall;
	}
}

void FunctionManager::declareAddMemoryBlock()
{
	 std::vector<Type*>addMemBlock_Args;
	 addMemBlock_Args.push_back(m_pTypeManager->GetFreeMemBlockPtTy());
	 FunctionType* addMemBlockType = FunctionType::get(
	  /*Result=*/Type::getVoidTy(m_pMod->getContext()),
	  /*Params=*/addMemBlock_Args,
	  /*isVarArg=*/false);

	m_pFuncAddMemBlock = m_pMod->getFunction("llvm_add_memory_block");
	if (!m_pFuncAddMemBlock)
	{
		m_pFuncAddMemBlock = Function::Create(
				  /*Type=*/addMemBlockType,
				  /*Linkage=*/GlobalValue::ExternalLinkage,
				  /*Name=*/"llvm_add_memory_block", m_pMod);
		m_pFuncAddMemBlock->setCallingConv(CallingConv::C);
	}

	AttributeList FuncAddMemBlock_PAL;
	{
		SmallVector<AttributeList, 4> Attrs;
		AttributeList PAS;
		{
			AttrBuilder B;
			B.addAttribute(Attribute::NoUnwind);
			B.addAttribute(Attribute::UWTable);
			PAS = AttributeList::get(m_pMod->getContext(), ~0U, B);
		}
		Attrs.push_back(PAS);
		FuncAddMemBlock_PAL = AttributeList::get(m_pMod->getContext(), Attrs);
	}
	m_pFuncAddMemBlock->setAttributes(FuncAddMemBlock_PAL);
}

void FunctionManager::defineAddMemoryBlock()
{
	ConstantInt* int_val_0 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10));
	ConstantInt* int_val_1 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("1"), 10));
	ConstantInt* int_val_2 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("2"), 10));
	ConstantInt* one_bit_0 = ConstantInt::get(m_pMod->getContext(), APInt(1, StringRef("0"), 10));

	Function::arg_iterator args = m_pFuncAddMemBlock->arg_begin();
	Value *ptr_b = &(*args);
	ptr_b->setName("b");

	BasicBlock* label_22 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_23 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_24 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_25 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_26 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_27 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_28 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_29 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_30 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_31 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_32 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);
	BasicBlock* label_33 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncAddMemBlock,0);

	// Block  (label_22) - Initializes variables and checks first cond of if statement (!head)
	AllocaInst* ptr_34 = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "", label_22);
	ptr_34->setAlignment(8);
	AllocaInst* ptr_curr = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "curr", label_22);
	ptr_curr->setAlignment(8);
	StoreInst* void_35 = new StoreInst(ptr_b, ptr_34, false, label_22);
	void_35->setAlignment(8);
	LoadInst* ptr_36 = new LoadInst(ptr_34, "", false, label_22);
	ptr_36->setAlignment(8);
	GetElementPtrInst* ptr_37 = GetElementPtrInst::Create(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_36,
			{int_val_0, int_val_2}, "", label_22);
	StoreInst* void_38 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(),
			ptr_37, false, label_22);
	void_38->setAlignment(8);
	LoadInst* ptr_39 = new LoadInst(ptr_34, "", false, label_22);
	ptr_39->setAlignment(8);
	GetElementPtrInst* ptr_40 = GetElementPtrInst::Create(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_39,
			{int_val_0, int_val_1}, "", label_22);
	StoreInst* void_41 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(),
			ptr_40, false, label_22);
	void_41->setAlignment(8);
	LoadInst* ptr_42 = new LoadInst(m_pFreeMemBlockHead, "", false, label_22);
	ptr_42->setAlignment(8);
	ICmpInst* int1_43 = new ICmpInst(*label_22, ICmpInst::ICMP_NE, ptr_42,
			m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_23, label_24, int1_43, label_22);

	// Block  (label_23) - check second condition of if statement
	// if ((unsigned long)head > (unsigned long)b)
	LoadInst* ptr_45 = new LoadInst(m_pFreeMemBlockHead, "", false, label_23);
	ptr_45->setAlignment(8);
	CastInst* int64_46 = new PtrToIntInst(ptr_45, IntegerType::get(m_pMod->getContext(), 64), "", label_23);
	LoadInst* ptr_47 = new LoadInst(ptr_34, "", false, label_23);
	ptr_47->setAlignment(8);
	CastInst* int64_48 = new PtrToIntInst(ptr_47, IntegerType::get(m_pMod->getContext(), 64), "", label_23);
	ICmpInst* int1_49 = new ICmpInst(*label_23, ICmpInst::ICMP_UGT, int64_46, int64_48, "");
	BranchInst::Create(label_24, label_27, int1_49, label_23);

	// Block  (label_24) - if (head) [inner if statement]
	LoadInst* ptr_51 = new LoadInst(m_pFreeMemBlockHead, "", false, label_24);
	ptr_51->setAlignment(8);
	ICmpInst* int1_52 = new ICmpInst(*label_24, ICmpInst::ICMP_NE, ptr_51, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_25, label_26, int1_52, label_24);

	// Block  (label_25) - head->prev = b;
	LoadInst* ptr_54 = new LoadInst(ptr_34, "", false, label_25);
	ptr_54->setAlignment(8);
	LoadInst* ptr_55 = new LoadInst(m_pFreeMemBlockHead, "", false, label_25);
	ptr_55->setAlignment(8);
	GetElementPtrInst* ptr_56 = GetElementPtrInst::Create(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_55, {int_val_0, int_val_2}, "", label_25);
	StoreInst* void_57 = new StoreInst(ptr_54, ptr_56, false, label_25);
	void_57->setAlignment(8);
	BranchInst::Create(label_26, label_25);

	// Block  (label_26) - b->next = head; head = b;
	LoadInst* ptr_59 = new LoadInst(m_pFreeMemBlockHead, "", false, label_26);
	ptr_59->setAlignment(8);
	LoadInst* ptr_60 = new LoadInst(ptr_34, "", false, label_26);
	ptr_60->setAlignment(8);
	GetElementPtrInst* ptr_61 = GetElementPtrInst::Create(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_60, {int_val_0, int_val_1}, "", label_26);
	StoreInst* void_62 = new StoreInst(ptr_59, ptr_61, false, label_26);
	void_62->setAlignment(8);
	LoadInst* ptr_63 = new LoadInst(ptr_34, "", false, label_26);
	ptr_63->setAlignment(8);
	StoreInst* void_64 = new StoreInst(ptr_63, m_pFreeMemBlockHead, false, label_26);
	void_64->setAlignment(8);
	BranchInst::Create(label_33, label_26);

	// Block  (label_27) - curr = head;
	LoadInst* ptr_66 = new LoadInst(m_pFreeMemBlockHead, "", false, label_27);
	ptr_66->setAlignment(8);
	StoreInst* void_67 = new StoreInst(ptr_66, ptr_curr, false, label_27);
	void_67->setAlignment(8);
	BranchInst::Create(label_28, label_27);

	// Block  (label_28) - if (curr->next)
	LoadInst* ptr_69 = new LoadInst(ptr_curr, "", false, label_28);
	ptr_69->setAlignment(8);
	GetElementPtrInst* ptr_70 = GetElementPtrInst::Create(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_69, {int_val_0, int_val_1}, "", label_28);
	LoadInst* ptr_71 = new LoadInst(ptr_70, "", false, label_28);
	ptr_71->setAlignment(8);
	ICmpInst* int1_72 = new ICmpInst(*label_28, ICmpInst::ICMP_NE, ptr_71,
			m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_29, label_30, int1_72, label_28);

	// Block  (label_29) - if ((unsigned long)curr->next < (unsigned long)b)
	LoadInst* ptr_74 = new LoadInst(ptr_curr, "", false, label_29);
	ptr_74->setAlignment(8);
	GetElementPtrInst* ptr_75 = GetElementPtrInst::Create(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_74, {int_val_0, int_val_1}, "", label_29);
	LoadInst* ptr_76 = new LoadInst(ptr_75, "", false, label_29);
	ptr_76->setAlignment(8);
	CastInst* int64_77 = new PtrToIntInst(ptr_76, IntegerType::get(m_pMod->getContext(), 64),
			"", label_29);
	LoadInst* ptr_78 = new LoadInst(ptr_34, "", false, label_29);
	ptr_78->setAlignment(8);
	CastInst* int64_79 = new PtrToIntInst(ptr_78, IntegerType::get(m_pMod->getContext(), 64),
			"", label_29);
	ICmpInst* int1_80 = new ICmpInst(*label_29, ICmpInst::ICMP_ULT, int64_77, int64_79, "");
	BranchInst::Create(label_30, label_29);

	// Block  (label_30) - This phi node probably isn't necessary...
	PHINode* int1_82 = PHINode::Create(IntegerType::get(m_pMod->getContext(), 1),
			2, "", label_30);
	int1_82->addIncoming(one_bit_0, label_28);
	int1_82->addIncoming(int1_80, label_29);
	BranchInst::Create(label_31, label_32, int1_82, label_30);

	// Block  (label_31) - curr = curr->next
	LoadInst* ptr_84 = new LoadInst(ptr_curr, "", false, label_31);
	ptr_84->setAlignment(8);
	GetElementPtrInst* ptr_85 = GetElementPtrInst::Create(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_84, {int_val_0, int_val_1}, "", label_31);
	LoadInst* ptr_86 = new LoadInst(ptr_85, "", false, label_31);
	ptr_86->setAlignment(8);
	StoreInst* void_87 = new StoreInst(ptr_86, ptr_curr, false, label_31);
	void_87->setAlignment(8);
	BranchInst::Create(label_28, label_31);

	// Block  (label_32) - b->next = curr->next; curr->next = b
	LoadInst* ptr_89 = new LoadInst(ptr_curr, "", false, label_32);
	ptr_89->setAlignment(8);
	GetElementPtrInst* ptr_90 = GetElementPtrInst::Create(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_89,
			{int_val_0, int_val_1}, "", label_32);
	LoadInst* ptr_91 = new LoadInst(ptr_90, "", false, label_32);
	ptr_91->setAlignment(8);
	LoadInst* ptr_92 = new LoadInst(ptr_34, "", false, label_32);
	ptr_92->setAlignment(8);
	GetElementPtrInst* ptr_93 = GetElementPtrInst::Create(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_92,
			{int_val_0, int_val_1}, "", label_32);
	StoreInst* void_94 = new StoreInst(ptr_91, ptr_93, false, label_32);
	void_94->setAlignment(8);
	LoadInst* ptr_95 = new LoadInst(ptr_34, "", false, label_32);
	ptr_95->setAlignment(8);
	LoadInst* ptr_96 = new LoadInst(ptr_curr, "", false, label_32);
	ptr_96->setAlignment(8);
	GetElementPtrInst* ptr_97 = GetElementPtrInst::Create(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_96,
			{int_val_0, int_val_1}, "", label_32);
	StoreInst* void_98 = new StoreInst(ptr_95, ptr_97, false, label_32);
	void_98->setAlignment(8);
	BranchInst::Create(label_33, label_32);

	// Block  (label_33)
	ReturnInst::Create(m_pMod->getContext(), label_33);
}

/*** Function summary - FunctionManager::replaceMallocWithMmap ***
Takes in an instruction, and replaces it with a call to
before the given instruction.

@Inputs:
- inst: pointer to an instruction (should be the CallInst to malloc)

@brief:
The CallInst to malloc() is replaced with a call to mmap()

@Outputs:
- mmapCallInst: The CallInst to mmap() that replaced the CallInst to malloc()
*/

// MallocArgs should also be an argument to choose size of the mapping
Instruction* FunctionManager::replaceMallocWithMmap(Instruction *inst/*, MallocArgs args*/)
{
	 // Constant Definitions
	PointerType* voidPtrType = PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);

	// TODO: The address we map memory to should not be a constant, and the byte to alloc as well
	// Address to mmap must be read from a variable at runtime (inserting a global to keep track
	// is probably required)
	ConstantInt* addrToMapMem = ConstantInt::get(m_pMod->getContext(), APInt(64, StringRef("196608"), 10));
	Constant* ptrToMmapAddr = ConstantExpr::getCast(Instruction::IntToPtr, addrToMapMem, voidPtrType);
	ConstantInt* bytesToAlloc = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("4"), 10));
	ConstantInt* mmap_prot_arg = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("3"), 10));
	ConstantInt* mmap_flags_arg = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("50"), 10));
	ConstantInt* mmap_fd_arg = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("-1"), 10));
	ConstantInt* mmap_offset_arg = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10));

	AllocaInst* pMmapAddr = new AllocaInst(voidPtrType, 0, "pMmapAddr", inst);
	pMmapAddr->setAlignment(8);
	StoreInst* void_17 = new StoreInst(ptrToMmapAddr, pMmapAddr, false, inst);
	void_17->setAlignment(8);
	LoadInst* mmapAddr = new LoadInst(pMmapAddr, "", false, inst);
	mmapAddr->setAlignment(8);
	std::vector<Value*> mmapFuncParams;
	mmapFuncParams.push_back(/*ptrToMmapAddr*/mmapAddr);
	mmapFuncParams.push_back(bytesToAlloc);
	mmapFuncParams.push_back(mmap_prot_arg);
	mmapFuncParams.push_back(mmap_flags_arg);
	mmapFuncParams.push_back(mmap_fd_arg);
	mmapFuncParams.push_back(mmap_offset_arg);

	CallInst* mmapCallInst = CallInst::Create(m_pFuncMmap,
			mmapFuncParams, ""/*, inst*/);
	mmapCallInst->setCallingConv(CallingConv::C);
	mmapCallInst->setTailCall(false);
	AttributeList mmap_PAL;
	{
	SmallVector<AttributeList, 4> Attrs;
	AttributeList PAS;
	{
	 AttrBuilder B;
	 B.addAttribute(Attribute::NoUnwind);
	 PAS = AttributeList::get(m_pMod->getContext(), ~0U, B);
	}

	Attrs.push_back(PAS);
	mmap_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	}
	mmapCallInst->setAttributes(mmap_PAL);

	ReplaceInstWithInst(inst, mmapCallInst);
	return mmapCallInst;
}

Function* FunctionManager::getMmapFunction()
{
	return m_pFuncMmap;
}

bool FunctionManager::isMallocCall(CallInst* callInst)
{
	Function* funcCalled = callInst->getCalledFunction();
	if (!funcCalled)
	{
		Value* v = callInst->getCalledValue();
		Value* sv = v->stripPointerCasts();
		StringRef funcName = sv->getName();
		StringRef strMalloc("malloc");
		if (funcName.equals(strMalloc))
		{
			errs() << "-------THIS IS MALLOC-------\n";
			errs() << *callInst;
			errs() << "\n";
			errs() << *v;
			errs() << "\n";
			errs() << *sv;
			errs() << "\n";

			return true;
		}

		return false;
	}
	StringRef funcName = funcCalled->getName();
	StringRef strMalloc("malloc");
	if (funcName.equals(strMalloc))
	{
		return true;
	}
	return false;
}

bool FunctionManager::isMmapCall(CallInst* callInst)
{
	Function* funcCalled = callInst->getCalledFunction();
	if (!funcCalled)
	{
		Value* v = callInst->getCalledValue();
		Value* sv = v->stripPointerCasts();
		StringRef funcName = sv->getName();
		StringRef strMalloc("mmap");
		if (funcName.equals(strMalloc))
		{
			AttributeList attrs = callInst->getAttributes();

			for (unsigned int i = attrs.index_begin(), e = attrs.index_end(); i != e; ++i)
			{
				errs()<<attrs.getAsString(i);
				errs()<<"\n";
/*				AttributeSet AS = attrs.getAttributes(i);
				if (AS.hasAttributes())
				{
					errs()<<AS.getAsString();
					errs()<<"\n";
				}*/
			}

			return true;
		}

		return false;
	}
	StringRef funcName = funcCalled->getName();
	StringRef strMalloc("mmap");
	if (funcName.equals(strMalloc))
	{
		errs() << "mmap has no pointer casts\n";
		return true;
	}
	return false;
}

bool FunctionManager::isFreeCall(CallInst* callInst)
{
	Function* funcCalled = callInst->getCalledFunction();
	StringRef funcName = funcCalled->getName();
	StringRef strFree("free");
	if (funcName.equals(strFree))
	{
		return true;
	}
	return false;
}

FunctionManager::MallocArgs FunctionManager::extractMallocArgs(CallInst *callInst)
{
	MallocArgs args;
	CallSite CS(callInst);
	for (auto arg = CS.arg_begin(); arg != CS.arg_end(); arg++)
	{
		// For constant args, cast to ConstantInt. Pass this
		// value into call to mmap()
		if (ConstantInt* CI = dyn_cast<ConstantInt>(arg))
		{
			args.isConstantArg = true;
			args.constArg = CI;
		}
		// For non-const args, cast to Inst. Load the value from
		// this inst (then store it), and pass the loaded value
		// into call to mmap()
		else if (Instruction* Inst = dyn_cast<Instruction>(arg))
		{
			Type* intType = IntegerType::get(m_pMod->getContext(), 64);
			args.isConstantArg = false;
			// Insert Variable to store the argument passed to malloc
			// This is required for the new call to mmap (size to map)
			args.allocaInst = new AllocaInst(intType, 0, "mallocSize", callInst);
			new StoreInst(Inst, args.allocaInst, callInst);
		}
	}
	return args;
}

// This is just used for testing.
CallInst* FunctionManager::insertAddMemoryBlockCall(/*InsertBefore*/Instruction *inst, Value *param)
{
	CallInst* addMemBlockCall = CallInst::Create(m_pFuncAddMemBlock, param, "", inst);
	addMemBlockCall->setCallingConv(CallingConv::C);
	addMemBlockCall->setTailCall(false);
	AttributeList addMemBlockCall_PAL;
	addMemBlockCall->setAttributes(addMemBlockCall_PAL);
	return addMemBlockCall;
}

void FunctionManager::testFunction()
{
	printf("Test\n");
}

