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

	declareSplitMemBlock();
	defineSplitMemBlock();

	declareRemoveMemBlock();
	defineRemoveMemBlock();

	declareMalloc();
	defineMalloc();

	declareScanMerge();
	defineScanMerge();

	declareFree();
	defineFree();

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
	ptr_34->setAlignment(4);
	AllocaInst* ptr_curr = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "curr", label_22);
	ptr_curr->setAlignment(4);
	StoreInst* void_35 = new StoreInst(ptr_b, ptr_34, false, label_22);
	void_35->setAlignment(4);
	LoadInst* ptr_36 = new LoadInst(ptr_34, "", false, label_22);
	ptr_36->setAlignment(4);
	GetElementPtrInst* ptr_37 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_36,
			{int_val_0, int_val_2}, "", label_22);
	StoreInst* void_38 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(),
			ptr_37, false, label_22);
	void_38->setAlignment(4);
	LoadInst* ptr_39 = new LoadInst(ptr_34, "", false, label_22);
	ptr_39->setAlignment(4);
	GetElementPtrInst* ptr_40 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_39,
			{int_val_0, int_val_1}, "", label_22);
	StoreInst* void_41 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(),
			ptr_40, false, label_22);
	void_41->setAlignment(4);
	LoadInst* ptr_42 = new LoadInst(m_pFreeMemBlockHead, "", false, label_22);
	ptr_42->setAlignment(4);
	ICmpInst* int1_43 = new ICmpInst(*label_22, ICmpInst::ICMP_NE, ptr_42,
			m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_23, label_24, int1_43, label_22);

	// Block  (label_23) - check second condition of if statement
	// if ((unsigned long)head > (unsigned long)b)
	LoadInst* ptr_45 = new LoadInst(m_pFreeMemBlockHead, "", false, label_23);
	ptr_45->setAlignment(4);
	CastInst* int64_46 = new PtrToIntInst(ptr_45, IntegerType::get(m_pMod->getContext(), 32), "", label_23);
	LoadInst* ptr_47 = new LoadInst(ptr_34, "", false, label_23);
	ptr_47->setAlignment(4);
	CastInst* int64_48 = new PtrToIntInst(ptr_47, IntegerType::get(m_pMod->getContext(), 32), "", label_23);
	ICmpInst* int1_49 = new ICmpInst(*label_23, ICmpInst::ICMP_UGT, int64_46, int64_48, "");
	BranchInst::Create(label_24, label_27, int1_49, label_23);

	// Block  (label_24) - if (head) [inner if statement]
	LoadInst* ptr_51 = new LoadInst(m_pFreeMemBlockHead, "", false, label_24);
	ptr_51->setAlignment(4);
	ICmpInst* int1_52 = new ICmpInst(*label_24, ICmpInst::ICMP_NE, ptr_51, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_25, label_26, int1_52, label_24);

	// Block  (label_25) - head->prev = b;
	LoadInst* ptr_54 = new LoadInst(ptr_34, "", false, label_25);
	ptr_54->setAlignment(4);
	LoadInst* ptr_55 = new LoadInst(m_pFreeMemBlockHead, "", false, label_25);
	ptr_55->setAlignment(4);
	GetElementPtrInst* ptr_56 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_55, {int_val_0, int_val_2}, "", label_25);
	StoreInst* void_57 = new StoreInst(ptr_54, ptr_56, false, label_25);
	void_57->setAlignment(4);
	BranchInst::Create(label_26, label_25);

	// Block  (label_26) - b->next = head; head = b;
	LoadInst* ptr_59 = new LoadInst(m_pFreeMemBlockHead, "", false, label_26);
	ptr_59->setAlignment(4);
	LoadInst* ptr_60 = new LoadInst(ptr_34, "", false, label_26);
	ptr_60->setAlignment(4);
	GetElementPtrInst* ptr_61 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_60, {int_val_0, int_val_1}, "", label_26);
	StoreInst* void_62 = new StoreInst(ptr_59, ptr_61, false, label_26);
	void_62->setAlignment(4);
	LoadInst* ptr_63 = new LoadInst(ptr_34, "", false, label_26);
	ptr_63->setAlignment(4);
	StoreInst* void_64 = new StoreInst(ptr_63, m_pFreeMemBlockHead, false, label_26);
	void_64->setAlignment(4);
	BranchInst::Create(label_33, label_26);

	// Block  (label_27) - curr = head;
	LoadInst* ptr_66 = new LoadInst(m_pFreeMemBlockHead, "", false, label_27);
	ptr_66->setAlignment(4);
	StoreInst* void_67 = new StoreInst(ptr_66, ptr_curr, false, label_27);
	void_67->setAlignment(4);
	BranchInst::Create(label_28, label_27);

	// Block  (label_28) - if (curr->next)
	LoadInst* ptr_69 = new LoadInst(ptr_curr, "", false, label_28);
	ptr_69->setAlignment(4);
	GetElementPtrInst* ptr_70 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_69, {int_val_0, int_val_1}, "", label_28);
	LoadInst* ptr_71 = new LoadInst(ptr_70, "", false, label_28);
	ptr_71->setAlignment(4);
	ICmpInst* int1_72 = new ICmpInst(*label_28, ICmpInst::ICMP_NE, ptr_71,
			m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_29, label_30, int1_72, label_28);

	// Block  (label_29) - if ((unsigned long)curr->next < (unsigned long)b)
	LoadInst* ptr_74 = new LoadInst(ptr_curr, "", false, label_29);
	ptr_74->setAlignment(4);
	GetElementPtrInst* ptr_75 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_74, {int_val_0, int_val_1}, "", label_29);
	LoadInst* ptr_76 = new LoadInst(ptr_75, "", false, label_29);
	ptr_76->setAlignment(4);
	CastInst* int64_77 = new PtrToIntInst(ptr_76, IntegerType::get(m_pMod->getContext(), 32),
			"", label_29);
	LoadInst* ptr_78 = new LoadInst(ptr_34, "", false, label_29);
	ptr_78->setAlignment(4);
	CastInst* int64_79 = new PtrToIntInst(ptr_78, IntegerType::get(m_pMod->getContext(), 32),
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
	ptr_84->setAlignment(4);
	GetElementPtrInst* ptr_85 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_84, {int_val_0, int_val_1}, "", label_31);
	LoadInst* ptr_86 = new LoadInst(ptr_85, "", false, label_31);
	ptr_86->setAlignment(4);
	StoreInst* void_87 = new StoreInst(ptr_86, ptr_curr, false, label_31);
	void_87->setAlignment(4);
	BranchInst::Create(label_28, label_31);

	// Block  (label_32) - b->next = curr->next; curr->next = b
	LoadInst* ptr_89 = new LoadInst(ptr_curr, "", false, label_32);
	ptr_89->setAlignment(4);
	GetElementPtrInst* ptr_90 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_89,
			{int_val_0, int_val_1}, "", label_32);
	LoadInst* ptr_91 = new LoadInst(ptr_90, "", false, label_32);
	ptr_91->setAlignment(4);
	LoadInst* ptr_92 = new LoadInst(ptr_34, "", false, label_32);
	ptr_92->setAlignment(4);
	GetElementPtrInst* ptr_93 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_92,
			{int_val_0, int_val_1}, "", label_32);
	StoreInst* void_94 = new StoreInst(ptr_91, ptr_93, false, label_32);
	void_94->setAlignment(4);
	LoadInst* ptr_95 = new LoadInst(ptr_34, "", false, label_32);
	ptr_95->setAlignment(4);
	LoadInst* ptr_96 = new LoadInst(ptr_curr, "", false, label_32);
	ptr_96->setAlignment(4);
	GetElementPtrInst* ptr_97 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(), ptr_96,
			{int_val_0, int_val_1}, "", label_32);
	StoreInst* void_98 = new StoreInst(ptr_95, ptr_97, false, label_32);
	void_98->setAlignment(4);
	BranchInst::Create(label_33, label_32);

	// Block  (label_33)
	ReturnInst::Create(m_pMod->getContext(), label_33);
}

void FunctionManager::declareSplitMemBlock()
{
	std::vector<Type*>splitMemBlock_Args;
	splitMemBlock_Args.push_back(m_pTypeManager->GetFreeMemBlockPtTy());
	splitMemBlock_Args.push_back(IntegerType::get(m_pMod->getContext(), 32));
	FunctionType* splitMemBlockType = FunctionType::get(
	/*Result=*/m_pTypeManager->GetFreeMemBlockPtTy(),
	/*Params=*/splitMemBlock_Args,
	/*isVarArg=*/false);

	m_pFuncSplitMemBlock = m_pMod->getFunction("llvm_split_memory_block");
	if (!m_pFuncSplitMemBlock)
	{
		m_pFuncSplitMemBlock = Function::Create(
				  /*Type=*/splitMemBlockType,
				  /*Linkage=*/GlobalValue::ExternalLinkage,
				  /*Name=*/"llvm_split_memory_block", m_pMod);
		m_pFuncSplitMemBlock->setCallingConv(CallingConv::C);
	}
	AttributeList func_split_PAL;
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
		func_split_PAL = AttributeList::get(m_pMod->getContext(), Attrs);
	}
	m_pFuncSplitMemBlock->setAttributes(func_split_PAL);
}

void FunctionManager::defineSplitMemBlock()
{
	PointerType* voidPtrType =
		 PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	ConstantInt* int_val_0 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10));
	// This is the size of block_t, need to be careful in case we change the structure and the
	// size of the struct changes.
	ConstantInt* int_val_12 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("12"), 10));

	Function::arg_iterator args = m_pFuncSplitMemBlock->arg_begin();
	Value* ptr_b = &(*args);
	ptr_b->setName("b");
	args++;
	Value *size = &(*args);
	size->setName("size");

	BasicBlock* label_12 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncSplitMemBlock,0);

	// Block  (label_12)
	AllocaInst* ptr_13 = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "", label_12);
	ptr_13->setAlignment(4);
	AllocaInst* ptr_14 = new AllocaInst(IntegerType::get(m_pMod->getContext(), 32), 0, "", label_12);
	ptr_14->setAlignment(4);
	AllocaInst* ptr_mem_block = new AllocaInst(voidPtrType, 0, "mem_block", label_12);
	ptr_mem_block->setAlignment(4);
	AllocaInst* ptr_newptr = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "newptr", label_12);
	ptr_newptr->setAlignment(4);
	StoreInst* void_15 = new StoreInst(ptr_b, ptr_13, false, label_12);
	void_15->setAlignment(4);
	StoreInst* void_16 = new StoreInst(size, ptr_14, false, label_12);
	void_16->setAlignment(4);
	LoadInst* ptr_17 = new LoadInst(ptr_13, "", false, label_12);
	ptr_17->setAlignment(4);
	CastInst* int64_18 = new PtrToIntInst(ptr_17, IntegerType::get(m_pMod->getContext(), 32), "", label_12);
	BinaryOperator* int64_19 = BinaryOperator::Create(Instruction::Add, int64_18, int_val_12, "", label_12);
	CastInst* ptr_20 = new IntToPtrInst(int64_19, voidPtrType, "", label_12);
	StoreInst* void_21 = new StoreInst(ptr_20, ptr_mem_block, false, label_12);
	void_21->setAlignment(4);
	LoadInst* ptr_22 = new LoadInst(ptr_mem_block, "", false, label_12);
	ptr_22->setAlignment(4);
	CastInst* int64_23 = new PtrToIntInst(ptr_22, IntegerType::get(m_pMod->getContext(), 32), "", label_12);
	LoadInst* int64_24 = new LoadInst(ptr_14, "", false, label_12);
	int64_24->setAlignment(4);
	BinaryOperator* int64_25 = BinaryOperator::Create(Instruction::Add, int64_23, int64_24, "", label_12);
	CastInst* ptr_26 = new IntToPtrInst(int64_25, m_pTypeManager->GetFreeMemBlockPtTy(), "", label_12);
	StoreInst* void_27 = new StoreInst(ptr_26, ptr_newptr, false, label_12);
	void_27->setAlignment(4);
	LoadInst* ptr_28 = new LoadInst(ptr_13, "", false, label_12);
	ptr_28->setAlignment(4);
	GetElementPtrInst* ptr_29 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_28, {int_val_0, int_val_0}, "", label_12);
	LoadInst* int64_30 = new LoadInst(ptr_29, "", false, label_12);
	int64_30->setAlignment(4);
	LoadInst* int64_31 = new LoadInst(ptr_14, "", false, label_12);
	int64_31->setAlignment(4);
	BinaryOperator* int64_32 = BinaryOperator::Create(Instruction::Add, int64_31, int_val_12, "", label_12);
	BinaryOperator* int64_33 = BinaryOperator::Create(Instruction::Sub, int64_30, int64_32, "", label_12);
	LoadInst* ptr_34 = new LoadInst(ptr_newptr, "", false, label_12);
	ptr_34->setAlignment(4);
	GetElementPtrInst* ptr_35 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_34, {int_val_0, int_val_0}, "", label_12);
	StoreInst* void_36 = new StoreInst(int64_33, ptr_35, false, label_12);
	void_36->setAlignment(4);
	LoadInst* int64_37 = new LoadInst(ptr_14, "", false, label_12);
	int64_37->setAlignment(4);
	LoadInst* ptr_38 = new LoadInst(ptr_13, "", false, label_12);
	ptr_38->setAlignment(4);
	GetElementPtrInst* ptr_39 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_38, {int_val_0, int_val_0}, "", label_12);
	StoreInst* void_40 = new StoreInst(int64_37, ptr_39, false, label_12);
	void_40->setAlignment(4);
	LoadInst* ptr_41 = new LoadInst(ptr_newptr, "", false, label_12);
	ptr_41->setAlignment(4);
	ReturnInst::Create(m_pMod->getContext(), ptr_41, label_12);

}

void FunctionManager::declareRemoveMemBlock()
{
	std::vector<Type*>removeMemBlock_Args;
	removeMemBlock_Args.push_back(m_pTypeManager->GetFreeMemBlockPtTy());
	FunctionType* removeMemBlockTy = FunctionType::get(
	/*Result=*/Type::getVoidTy(m_pMod->getContext()),
	/*Params=*/removeMemBlock_Args,
	/*isVarArg=*/false);

	m_pFuncRemovemMemBlock = m_pMod->getFunction("llvm_remove_memory_block");
	if (!m_pFuncRemovemMemBlock) {
		m_pFuncRemovemMemBlock = Function::Create(
		/*Type=*/removeMemBlockTy,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"llvm_remove_memory_block", m_pMod);
		m_pFuncRemovemMemBlock->setCallingConv(CallingConv::C);
	}
	AttributeList func_fl_remove_PAL;
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
		func_fl_remove_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	}
	m_pFuncRemovemMemBlock->setAttributes(func_fl_remove_PAL);
}

void FunctionManager::defineRemoveMemBlock()
{
	ConstantInt* int_val_0 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10));
	ConstantInt* int_val_1 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("1"), 10));
	ConstantInt* int_val_2 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("2"), 10));

	Function::arg_iterator args = m_pFuncRemovemMemBlock->arg_begin();
	Value* ptr_b_45 = &(*args);
	ptr_b_45->setName("b");

	BasicBlock* label_46 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_47 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_48 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_49 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_50 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_51 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_52 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_53 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);
	BasicBlock* label_54 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncRemovemMemBlock,0);

	// Block  (label_46)
	AllocaInst* ptr_55 = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "", label_46);
	ptr_55->setAlignment(4);
	StoreInst* void_56 = new StoreInst(ptr_b_45, ptr_55, false, label_46);
	void_56->setAlignment(4);
	LoadInst* ptr_57 = new LoadInst(ptr_55, "", false, label_46);
	ptr_57->setAlignment(4);
	GetElementPtrInst* ptr_58 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_57, {int_val_0, int_val_2}, "", label_46);
	LoadInst* ptr_59 = new LoadInst(ptr_58, "", false, label_46);
	ptr_59->setAlignment(4);
	ICmpInst* int1_60 = new ICmpInst(*label_46, ICmpInst::ICMP_NE, ptr_59,
			m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_51, label_47, int1_60, label_46);

	// Block  (label_47)
	LoadInst* ptr_62 = new LoadInst(ptr_55, "", false, label_47);
	ptr_62->setAlignment(4);
	GetElementPtrInst* ptr_63 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_62, {int_val_0, int_val_1}, "", label_47);
	LoadInst* ptr_64 = new LoadInst(ptr_63, "", false, label_47);
	ptr_64->setAlignment(4);
	ICmpInst* int1_65 = new ICmpInst(*label_47, ICmpInst::ICMP_NE, ptr_64,
			m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_48, label_49, int1_65, label_47);

	// Block  (label_48)
	LoadInst* ptr_67 = new LoadInst(ptr_55, "", false, label_48);
	ptr_67->setAlignment(4);
	GetElementPtrInst* ptr_68 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_67, {int_val_0, int_val_1}, "", label_48);
	LoadInst* ptr_69 = new LoadInst(ptr_68, "", false, label_48);
	ptr_69->setAlignment(4);
	StoreInst* void_70 = new StoreInst(ptr_69, m_pFreeMemBlockHead, false, label_48);
	void_70->setAlignment(4);
	BranchInst::Create(label_50, label_48);

	// Block  (label_49)
	StoreInst* void_72 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(), m_pFreeMemBlockHead, false, label_49);
	void_72->setAlignment(4);
	BranchInst::Create(label_50, label_49);

	// Block  (label_50)
	BranchInst::Create(label_52, label_50);

	// Block  (label_51)
	LoadInst* ptr_75 = new LoadInst(ptr_55, "", false, label_51);
	ptr_75->setAlignment(4);
	GetElementPtrInst* ptr_76 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_75, {int_val_0, int_val_1}, "", label_51);
	LoadInst* ptr_77 = new LoadInst(ptr_76, "", false, label_51);
	ptr_77->setAlignment(4);
	LoadInst* ptr_78 = new LoadInst(ptr_55, "", false, label_51);
	ptr_78->setAlignment(4);
	GetElementPtrInst* ptr_79 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_78, {int_val_0, int_val_2}, "", label_51);
	LoadInst* ptr_80 = new LoadInst(ptr_79, "", false, label_51);
	ptr_80->setAlignment(4);
	GetElementPtrInst* ptr_81 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_80, {int_val_0, int_val_1}, "", label_51);
	StoreInst* void_82 = new StoreInst(ptr_77, ptr_81, false, label_51);
	void_82->setAlignment(4);
	BranchInst::Create(label_52, label_51);

	// Block  (label_52)
	LoadInst* ptr_84 = new LoadInst(ptr_55, "", false, label_52);
	ptr_84->setAlignment(4);
	GetElementPtrInst* ptr_85 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_84, {int_val_0, int_val_1}, "", label_52);
	LoadInst* ptr_86 = new LoadInst(ptr_85, "", false, label_52);
	ptr_86->setAlignment(4);
	ICmpInst* int1_87 = new ICmpInst(*label_52, ICmpInst::ICMP_NE, ptr_86, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_53, label_54, int1_87, label_52);

	// Block  (label_53)
	LoadInst* ptr_89 = new LoadInst(ptr_55, "", false, label_53);
	ptr_89->setAlignment(4);
	GetElementPtrInst* ptr_90 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_89, {int_val_0, int_val_2}, "", label_53);
	LoadInst* ptr_91 = new LoadInst(ptr_90, "", false, label_53);
	ptr_91->setAlignment(4);
	LoadInst* ptr_92 = new LoadInst(ptr_55, "", false, label_53);
	ptr_92->setAlignment(4);
	GetElementPtrInst* ptr_93 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_92, {int_val_0, int_val_1}, "", label_53);
	LoadInst* ptr_94 = new LoadInst(ptr_93, "", false, label_53);
	ptr_94->setAlignment(4);
	GetElementPtrInst* ptr_95 = GetElementPtrInst::CreateInBounds(
			m_pTypeManager->GetFreeMemBlockStructTy(),
			ptr_94, {int_val_0, int_val_2}, "", label_53);
	StoreInst* void_96 = new StoreInst(ptr_91, ptr_95, false, label_53);
	void_96->setAlignment(4);
	BranchInst::Create(label_54, label_53);

	// Block  (label_54)
	ReturnInst::Create(m_pMod->getContext(), label_54);
}

void FunctionManager::declareMalloc()
{
	PointerType* voidPtrType = PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	std::vector<Type*>malloc_Args;
	malloc_Args.push_back(IntegerType::get(m_pMod->getContext(), 32));
	FunctionType* mallocTy = FunctionType::get(
			/*Result=*/voidPtrType,
			/*Params=*/malloc_Args,
			/*isVarArg=*/false);

	m_pFuncMalloc = m_pMod->getFunction("llvm_malloc");
	if (!m_pFuncMalloc)
	{
		m_pFuncMalloc = Function::Create(
		/*Type=*/mallocTy,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"llvm_malloc", m_pMod);
		m_pFuncMalloc->setCallingConv(CallingConv::C);
	}
	AttributeList func__malloc_PAL;
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
		func__malloc_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	}
	m_pFuncMalloc->setAttributes(func__malloc_PAL);
}

void FunctionManager::defineMalloc()
{
	PointerType* voidPtrType = PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	ConstantInt* int_val_0 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10));
	ConstantInt* int_val_1 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("1"), 10));
	ConstantInt* int_val_2 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("2"), 10));
	ConstantInt* int_val_3 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("3"), 10)); // param for mmap
	ConstantInt* int_val_34 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("34"), 10)); // param for mmap
	ConstantInt* int_val_neg1 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("-1"), 10)); // param for mmap
	ConstantInt* const_int64_0 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10)); // param for mmap
	// This is the size of block_t, need to be careful in case we change the structure and the
	// size of the struct changes.
	ConstantInt* int_val_12 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("12"), 10));
	ConstantPointerNull* voidPtrNull = ConstantPointerNull::get(voidPtrType);
	ConstantInt* allocMem_5Pages = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("20480"), 10)); // param for mmap
	ConstantInt* allocMemSize_MinusStructSize = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("20468"), 10));

	Function::arg_iterator args = m_pFuncMalloc->arg_begin();
	Value* int64_size_192 = &(*args);
	int64_size_192->setName("size");

	BasicBlock* label_193 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_194 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_195 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_196 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_197 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_198 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_199 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_200 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_201 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_202 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_203 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_204 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_205 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_206 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_207 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_208 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_209 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_210 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);
	BasicBlock* label_211 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncMalloc,0);

	// Block  (label_193)
	AllocaInst* ptr_212 = new AllocaInst(voidPtrType, 0, "", label_193);
	ptr_212->setAlignment(4);
	AllocaInst* ptr_213 = new AllocaInst(IntegerType::get(m_pMod->getContext(), 32), 0, "", label_193);
	ptr_213->setAlignment(4);
	AllocaInst* ptr_block_mem = new AllocaInst(voidPtrType, 0, "block_mem", label_193);
	ptr_block_mem->setAlignment(4);
	AllocaInst* ptr_ptr = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "ptr", label_193);
	ptr_ptr->setAlignment(4);
	AllocaInst* ptr_newptr_214 = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "newptr", label_193);
	ptr_newptr_214->setAlignment(4);
	StoreInst* void_215 = new StoreInst(int64_size_192, ptr_213, false, label_193);
	void_215->setAlignment(4);
	LoadInst* ptr_216 = new LoadInst(m_pFreeMemBlockHead, "", false, label_193);
	ptr_216->setAlignment(4);
	StoreInst* void_217 = new StoreInst(ptr_216, ptr_ptr, false, label_193);
	void_217->setAlignment(4);
	BranchInst::Create(label_194, label_193);

	// Block  (label_194)
	LoadInst* ptr_219 = new LoadInst(ptr_ptr, "", false, label_194);
	ptr_219->setAlignment(4);
	ICmpInst* int1_220 = new ICmpInst(*label_194, ICmpInst::ICMP_NE, ptr_219, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_195, label_201, int1_220, label_194);

	// Block  (label_195)
	LoadInst* ptr_222 = new LoadInst(ptr_ptr, "", false, label_195);
	ptr_222->setAlignment(4);
	GetElementPtrInst* ptr_223 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_222, {
	int_val_0,
	int_val_0
	}, "", label_195);
	LoadInst* int64_224 = new LoadInst(ptr_223, "", false, label_195);
	int64_224->setAlignment(4);
	LoadInst* int64_225 = new LoadInst(ptr_213, "", false, label_195);
	int64_225->setAlignment(4);
	BinaryOperator* int64_226 = BinaryOperator::Create(Instruction::Add, int64_225, int_val_12, "", label_195);
	ICmpInst* int1_227 = new ICmpInst(*label_195, ICmpInst::ICMP_UGE, int64_224, int64_226, "");
	BranchInst::Create(label_196, label_199, int1_227, label_195);

	// Block  (label_196)
	LoadInst* ptr_229 = new LoadInst(ptr_ptr, "", false, label_196);
	ptr_229->setAlignment(4);
	CastInst* int64_230 = new PtrToIntInst(ptr_229, IntegerType::get(m_pMod->getContext(), 32), "", label_196);
	BinaryOperator* int64_231 = BinaryOperator::Create(Instruction::Add, int64_230, int_val_12, "", label_196);
	CastInst* ptr_232 = new IntToPtrInst(int64_231, voidPtrType, "", label_196);
	StoreInst* void_233 = new StoreInst(ptr_232, ptr_block_mem, false, label_196);
	void_233->setAlignment(4);
	LoadInst* ptr_234 = new LoadInst(ptr_ptr, "", false, label_196);
	ptr_234->setAlignment(4);
	CallInst* void_235 = CallInst::Create(m_pFuncRemovemMemBlock, ptr_234, "", label_196);
	void_235->setCallingConv(CallingConv::C);
	void_235->setTailCall(false);
	AttributeList void_235_PAL;
	void_235->setAttributes(void_235_PAL);

	LoadInst* ptr_236 = new LoadInst(ptr_ptr, "", false, label_196);
	ptr_236->setAlignment(4);
	GetElementPtrInst* ptr_237 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_236, {
	int_val_0,
	int_val_0
	}, "", label_196);
	LoadInst* int64_238 = new LoadInst(ptr_237, "", false, label_196);
	int64_238->setAlignment(4);
	LoadInst* int64_239 = new LoadInst(ptr_213, "", false, label_196);
	int64_239->setAlignment(4);
	ICmpInst* int1_240 = new ICmpInst(*label_196, ICmpInst::ICMP_EQ, int64_238, int64_239, "");
	BranchInst::Create(label_197, label_198, int1_240, label_196);

	// Block  (label_197)
	LoadInst* ptr_242 = new LoadInst(ptr_block_mem, "", false, label_197);
	ptr_242->setAlignment(4);
	StoreInst* void_243 = new StoreInst(ptr_242, ptr_212, false, label_197);
	void_243->setAlignment(4);
	BranchInst::Create(label_211, label_197);

	// Block  (label_198)
	LoadInst* ptr_245 = new LoadInst(ptr_ptr, "", false, label_198);
	ptr_245->setAlignment(4);
	LoadInst* int64_246 = new LoadInst(ptr_213, "", false, label_198);
	int64_246->setAlignment(4);
	std::vector<Value*> ptr_247_params;
	ptr_247_params.push_back(ptr_245);
	ptr_247_params.push_back(int64_246);
	CallInst* ptr_247 = CallInst::Create(m_pFuncSplitMemBlock, ptr_247_params, "", label_198);
	ptr_247->setCallingConv(CallingConv::C);
	ptr_247->setTailCall(false);
	AttributeList ptr_247_PAL;
	ptr_247->setAttributes(ptr_247_PAL);

	StoreInst* void_248 = new StoreInst(ptr_247, ptr_newptr_214, false, label_198);
	void_248->setAlignment(4);
	LoadInst* ptr_249 = new LoadInst(ptr_newptr_214, "", false, label_198);
	ptr_249->setAlignment(4);
	CallInst* void_250 = CallInst::Create(m_pFuncAddMemBlock, ptr_249, "", label_198);
	void_250->setCallingConv(CallingConv::C);
	void_250->setTailCall(false);
	AttributeList void_250_PAL;
	void_250->setAttributes(void_250_PAL);

	LoadInst* ptr_251 = new LoadInst(ptr_block_mem, "", false, label_198);
	ptr_251->setAlignment(4);
	StoreInst* void_252 = new StoreInst(ptr_251, ptr_212, false, label_198);
	void_252->setAlignment(4);
	BranchInst::Create(label_211, label_198);

	// Block  (label_199)
	LoadInst* ptr_254 = new LoadInst(ptr_ptr, "", false, label_199);
	ptr_254->setAlignment(4);
	GetElementPtrInst* ptr_255 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_254, {
	int_val_0,
	int_val_1
	}, "", label_199);
	LoadInst* ptr_256 = new LoadInst(ptr_255, "", false, label_199);
	ptr_256->setAlignment(4);
	StoreInst* void_257 = new StoreInst(ptr_256, ptr_ptr, false, label_199);
	void_257->setAlignment(4);
	BranchInst::Create(label_200, label_199);

	// Block  (label_200)
	BranchInst::Create(label_194, label_200);

	// Block  (label_201)
	LoadInst* int32_260 = new LoadInst(m_pHaveAllocedMem, "", false, label_201);
	int32_260->setAlignment(4);
	ICmpInst* int1_261 = new ICmpInst(*label_201, ICmpInst::ICMP_NE, int32_260, int_val_0, "");
	BranchInst::Create(label_210, label_202, int1_261, label_201);

	// Block  (label_202)
/*
	// Test for mallocing at a specific starting address (0x30000) to test if the
	// insertion/replacement actually works (print the address to see if its 0x3000c)
	// 0x3000c because llvm_malloc returns the address after the free.mem.block struct
    ConstantInt* addrToMapMem = ConstantInt::get(m_pMod->getContext(), APInt(64, StringRef("196608"), 10));
	Constant* ptrToMmapAddr = ConstantExpr::getCast(Instruction::IntToPtr, addrToMapMem, voidPtrType);
*/
	StoreInst* void_263 = new StoreInst(int_val_1, m_pHaveAllocedMem, false, label_202);
	void_263->setAlignment(4);
	std::vector<Value*> ptr_264_params;
	ptr_264_params.push_back(voidPtrNull/*ptrToMmapAddr*/);
	ptr_264_params.push_back(allocMem_5Pages);
	ptr_264_params.push_back(int_val_3);
	ptr_264_params.push_back(int_val_34);
	ptr_264_params.push_back(int_val_neg1);
	ptr_264_params.push_back(const_int64_0);
	CallInst* ptr_264 = CallInst::Create(m_pFuncMmap, ptr_264_params, "", label_202);
	ptr_264->setCallingConv(CallingConv::C);
	ptr_264->setTailCall(false);
	AttributeList ptr_264_PAL;
	{
	SmallVector<AttributeList, 4> Attrs;
	AttributeList PAS;
	{
	 AttrBuilder B;
	 B.addAttribute(Attribute::NoUnwind);
	 PAS = AttributeList::get(m_pMod->getContext(), ~0U, B);
	}

	Attrs.push_back(PAS);
	ptr_264_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	}
	ptr_264->setAttributes(ptr_264_PAL);

	CastInst* ptr_265 = new BitCastInst(ptr_264, m_pTypeManager->GetFreeMemBlockPtTy(), "", label_202);
	StoreInst* void_266 = new StoreInst(ptr_265, ptr_ptr, false, label_202);
	void_266->setAlignment(4);

	// This "block" was added after the initial implementation
	// Have gvar point to beginning of llvm_heap
	LoadInst* ptrFromMmap = new LoadInst(ptr_ptr, "", false, label_202);
	ptrFromMmap->setAlignment(4);
	CastInst* castPtrToVoidType = new BitCastInst(ptrFromMmap, voidPtrType, "", label_202);
	StoreInst* storeHeapAddrToGvar = new StoreInst(castPtrToVoidType, m_pPtrToHeap, false, label_202);
	storeHeapAddrToGvar->setAlignment(4);

	LoadInst* ptr_267 = new LoadInst(ptr_ptr, "", false, label_202);
	ptr_267->setAlignment(4);
	ICmpInst* int1_268 = new ICmpInst(*label_202, ICmpInst::ICMP_NE, ptr_267, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_204, label_203, int1_268, label_202);

	// Block  (label_203)
	StoreInst* void_270 = new StoreInst(voidPtrNull, ptr_212, false, label_203);
	void_270->setAlignment(4);
	BranchInst::Create(label_211, label_203);

	// Block  (label_204)
	LoadInst* ptr_272 = new LoadInst(ptr_ptr, "", false, label_204);
	ptr_272->setAlignment(4);
	GetElementPtrInst* ptr_273 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_272, {
	int_val_0,
	int_val_1
	}, "", label_204);
	StoreInst* void_274 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(), ptr_273, false, label_204);
	void_274->setAlignment(4);
	LoadInst* ptr_275 = new LoadInst(ptr_ptr, "", false, label_204);
	ptr_275->setAlignment(4);
	GetElementPtrInst* ptr_276 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_275, {
	int_val_0,
	int_val_2
	}, "", label_204);
	StoreInst* void_277 = new StoreInst(m_pTypeManager->GetFreeMemBlockNull(), ptr_276, false, label_204);
	void_277->setAlignment(4);
	LoadInst* ptr_278 = new LoadInst(ptr_ptr, "", false, label_204);
	ptr_278->setAlignment(4);
	GetElementPtrInst* ptr_279 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_278, {
	int_val_0,
	int_val_0
	}, "", label_204);
	StoreInst* void_280 = new StoreInst(allocMemSize_MinusStructSize, ptr_279, false, label_204);
	void_280->setAlignment(4);
	LoadInst* ptr_281 = new LoadInst(ptr_ptr, "", false, label_204);
	ptr_281->setAlignment(4);
	GetElementPtrInst* ptr_282 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_281, {
	int_val_0,
	int_val_0
	}, "", label_204);
	LoadInst* int64_283 = new LoadInst(ptr_282, "", false, label_204);
	int64_283->setAlignment(4);
	LoadInst* int64_284 = new LoadInst(ptr_213, "", false, label_204);
	int64_284->setAlignment(4);
	ICmpInst* int1_285 = new ICmpInst(*label_204, ICmpInst::ICMP_UGT, int64_283, int64_284, "");
	BranchInst::Create(label_205, label_206, int1_285, label_204);

	// Block  (label_205)
	LoadInst* ptr_287 = new LoadInst(ptr_ptr, "", false, label_205);
	ptr_287->setAlignment(4);
	LoadInst* int64_288 = new LoadInst(ptr_213, "", false, label_205);
	int64_288->setAlignment(4);
	std::vector<Value*> ptr_289_params;
	ptr_289_params.push_back(ptr_287);
	ptr_289_params.push_back(int64_288);
	CallInst* ptr_289 = CallInst::Create(m_pFuncSplitMemBlock, ptr_289_params, "", label_205);
	ptr_289->setCallingConv(CallingConv::C);
	ptr_289->setTailCall(false);
	AttributeList ptr_289_PAL;
	ptr_289->setAttributes(ptr_289_PAL);

	StoreInst* void_290 = new StoreInst(ptr_289, ptr_newptr_214, false, label_205);
	void_290->setAlignment(4);
	LoadInst* ptr_291 = new LoadInst(ptr_newptr_214, "", false, label_205);
	ptr_291->setAlignment(4);
	CallInst* void_292 = CallInst::Create(m_pFuncAddMemBlock, ptr_291, "", label_205);
	void_292->setCallingConv(CallingConv::C);
	void_292->setTailCall(false);
	AttributeList void_292_PAL;
	void_292->setAttributes(void_292_PAL);

	LoadInst* ptr_293 = new LoadInst(ptr_ptr, "", false, label_205);
	ptr_293->setAlignment(4);
	CastInst* int64_294 = new PtrToIntInst(ptr_293, IntegerType::get(m_pMod->getContext(), 32), "", label_205);
	BinaryOperator* int64_295 = BinaryOperator::Create(Instruction::Add, int64_294, int_val_12, "", label_205);
	CastInst* ptr_296 = new IntToPtrInst(int64_295, voidPtrType, "", label_205);
	StoreInst* void_297 = new StoreInst(ptr_296, ptr_212, false, label_205);
	void_297->setAlignment(4);
	BranchInst::Create(label_211, label_205);

	// Block  (label_206)
	LoadInst* ptr_299 = new LoadInst(ptr_ptr, "", false, label_206);
	ptr_299->setAlignment(4);
	GetElementPtrInst* ptr_300 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_299, {
	int_val_0,
	int_val_0
	}, "", label_206);
	LoadInst* int64_301 = new LoadInst(ptr_300, "", false, label_206);
	int64_301->setAlignment(4);
	LoadInst* int64_302 = new LoadInst(ptr_213, "", false, label_206);
	int64_302->setAlignment(4);
	ICmpInst* int1_303 = new ICmpInst(*label_206, ICmpInst::ICMP_EQ, int64_301, int64_302, "");
	BranchInst::Create(label_207, label_208, int1_303, label_206);

	// Block  (label_207)
	LoadInst* ptr_305 = new LoadInst(ptr_ptr, "", false, label_207);
	ptr_305->setAlignment(4);
	CastInst* int64_306 = new PtrToIntInst(ptr_305, IntegerType::get(m_pMod->getContext(), 32), "", label_207);
	BinaryOperator* int64_307 = BinaryOperator::Create(Instruction::Add, int64_306, int_val_12, "", label_207);
	CastInst* ptr_308 = new IntToPtrInst(int64_307, voidPtrType, "", label_207);
	StoreInst* void_309 = new StoreInst(ptr_308, ptr_212, false, label_207);
	void_309->setAlignment(4);
	BranchInst::Create(label_211, label_207);

	// Block  (label_208)
	BranchInst::Create(label_209, label_208);

	// Block  (label_209)
	BranchInst::Create(label_210, label_209);

	// Block  (label_210)
	StoreInst* void_313 = new StoreInst(voidPtrNull, ptr_212, false, label_210);
	void_313->setAlignment(4);
	BranchInst::Create(label_211, label_210);

	// Block  (label_211)
	LoadInst* ptr_315 = new LoadInst(ptr_212, "", false, label_211);
	ptr_315->setAlignment(4);
	ReturnInst::Create(m_pMod->getContext(), ptr_315, label_211);
}

void FunctionManager::declareScanMerge()
{
	std::vector<Type*>scanMerge_params;
	FunctionType* scanMergeTy = FunctionType::get(
	/*Result=*/Type::getVoidTy(m_pMod->getContext()),
	/*Params=*/scanMerge_params,
	/*isVarArg=*/false);

	m_pFuncScanMerge = m_pMod->getFunction("llvm_scan_merge");
	if (!m_pFuncScanMerge)
	{
		m_pFuncScanMerge = Function::Create(
		/*Type=*/scanMergeTy,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"llvm_scan_merge", m_pMod);
		m_pFuncScanMerge->setCallingConv(CallingConv::C);
	}
	AttributeList func_scan_merge_PAL;
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
		func_scan_merge_PAL = AttributeList::get(m_pMod->getContext(), Attrs);

	}
	m_pFuncScanMerge->setAttributes(func_scan_merge_PAL);
}

void FunctionManager::defineScanMerge()
{
	ConstantInt* int_val_0 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("0"), 10));
	ConstantInt* int_val_1 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("1"), 10));
	ConstantInt* int_val_2 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("2"), 10));
	// This is the size of block_t, need to be careful in case we change the structure and the
	// size of the struct changes.
	ConstantInt* int_val_12 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("12"), 10));

	BasicBlock* label_99 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_100 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_101 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_102 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_103 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_104 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_105 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_106 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);
	BasicBlock* label_107 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncScanMerge,0);

	// Block  (label_99)
	AllocaInst* ptr_curr_108 = new AllocaInst(m_pTypeManager->GetFreeMemBlockPtTy(), 0, "curr", label_99);
	ptr_curr_108->setAlignment(4);
	AllocaInst* ptr_header_curr = new AllocaInst(IntegerType::get(m_pMod->getContext(), 32), 0, "header_curr", label_99);
	ptr_header_curr->setAlignment(4);
	AllocaInst* ptr_header_next = new AllocaInst(IntegerType::get(m_pMod->getContext(), 32), 0, "header_next", label_99);
	ptr_header_next->setAlignment(4);
	LoadInst* ptr_109 = new LoadInst(m_pFreeMemBlockHead, "", false, label_99);
	ptr_109->setAlignment(4);
	StoreInst* void_110 = new StoreInst(ptr_109, ptr_curr_108, false, label_99);
	void_110->setAlignment(4);
	BranchInst::Create(label_100, label_99);

	// Block  (label_100)
	LoadInst* ptr_112 = new LoadInst(ptr_curr_108, "", false, label_100);
	ptr_112->setAlignment(4);
	GetElementPtrInst* ptr_113 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_112, {
			int_val_0,
			int_val_1
	}, "", label_100);
	LoadInst* ptr_114 = new LoadInst(ptr_113, "", false, label_100);
	ptr_114->setAlignment(4);
	ICmpInst* int1_115 = new ICmpInst(*label_100, ICmpInst::ICMP_NE, ptr_114, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_101, label_107, int1_115, label_100);

	// Block  (label_101)
	LoadInst* ptr_117 = new LoadInst(ptr_curr_108, "", false, label_101);
	ptr_117->setAlignment(4);
	CastInst* int64_118 = new PtrToIntInst(ptr_117, IntegerType::get(m_pMod->getContext(), 32), "", label_101);
	StoreInst* void_119 = new StoreInst(int64_118, ptr_header_curr, false, label_101);
	void_119->setAlignment(4);
	LoadInst* ptr_120 = new LoadInst(ptr_curr_108, "", false, label_101);
	ptr_120->setAlignment(4);
	GetElementPtrInst* ptr_121 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_120, {
			int_val_0,
			int_val_1
	}, "", label_101);
	LoadInst* ptr_122 = new LoadInst(ptr_121, "", false, label_101);
	ptr_122->setAlignment(4);
	CastInst* int64_123 = new PtrToIntInst(ptr_122, IntegerType::get(m_pMod->getContext(), 32), "", label_101);
	StoreInst* void_124 = new StoreInst(int64_123, ptr_header_next, false, label_101);
	void_124->setAlignment(4);
	LoadInst* int64_125 = new LoadInst(ptr_header_curr, "", false, label_101);
	int64_125->setAlignment(4);
	LoadInst* ptr_126 = new LoadInst(ptr_curr_108, "", false, label_101);
	ptr_126->setAlignment(4);
	GetElementPtrInst* ptr_127 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_126, {
			int_val_0,
			int_val_0
	}, "", label_101);
	LoadInst* int64_128 = new LoadInst(ptr_127, "", false, label_101);
	int64_128->setAlignment(4);
	BinaryOperator* int64_129 = BinaryOperator::Create(Instruction::Add, int64_125, int64_128, "", label_101);
	BinaryOperator* int64_130 = BinaryOperator::Create(Instruction::Add, int64_129, int_val_12, "", label_101);
	LoadInst* int64_131 = new LoadInst(ptr_header_next, "", false, label_101);
	int64_131->setAlignment(4);
	ICmpInst* int1_132 = new ICmpInst(*label_101, ICmpInst::ICMP_EQ, int64_130, int64_131, "");
	BranchInst::Create(label_102, label_106, int1_132, label_101);

	// Block  (label_102)
	LoadInst* ptr_134 = new LoadInst(ptr_curr_108, "", false, label_102);
	ptr_134->setAlignment(4);
	GetElementPtrInst* ptr_135 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_134, {
			int_val_0,
			int_val_1
	}, "", label_102);
	LoadInst* ptr_136 = new LoadInst(ptr_135, "", false, label_102);
	ptr_136->setAlignment(4);
	GetElementPtrInst* ptr_137 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_136, {
			int_val_0,
			int_val_0
	}, "", label_102);
	LoadInst* int64_138 = new LoadInst(ptr_137, "", false, label_102);
	int64_138->setAlignment(4);
	BinaryOperator* int64_139 = BinaryOperator::Create(Instruction::Add, int64_138, int_val_12, "", label_102);
	LoadInst* ptr_140 = new LoadInst(ptr_curr_108, "", false, label_102);
	ptr_140->setAlignment(4);
	GetElementPtrInst* ptr_141 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_140, {
			int_val_0,
			int_val_0
	}, "", label_102);
	LoadInst* int64_142 = new LoadInst(ptr_141, "", false, label_102);
	int64_142->setAlignment(4);
	BinaryOperator* int64_143 = BinaryOperator::Create(Instruction::Add, int64_142, int64_139, "", label_102);
	StoreInst* void_144 = new StoreInst(int64_143, ptr_141, false, label_102);
	void_144->setAlignment(4);
	LoadInst* ptr_145 = new LoadInst(ptr_curr_108, "", false, label_102);
	ptr_145->setAlignment(4);
	GetElementPtrInst* ptr_146 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_145, {
			int_val_0,
			int_val_1
	}, "", label_102);
	LoadInst* ptr_147 = new LoadInst(ptr_146, "", false, label_102);
	ptr_147->setAlignment(4);
	GetElementPtrInst* ptr_148 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_147, {
			int_val_0,
			int_val_1
	}, "", label_102);
	LoadInst* ptr_149 = new LoadInst(ptr_148, "", false, label_102);
	ptr_149->setAlignment(4);
	LoadInst* ptr_150 = new LoadInst(ptr_curr_108, "", false, label_102);
	ptr_150->setAlignment(4);
	GetElementPtrInst* ptr_151 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_150, {
			int_val_0,
			int_val_1
	}, "", label_102);
	StoreInst* void_152 = new StoreInst(ptr_149, ptr_151, false, label_102);
	void_152->setAlignment(4);
	LoadInst* ptr_153 = new LoadInst(ptr_curr_108, "", false, label_102);
	ptr_153->setAlignment(4);
	GetElementPtrInst* ptr_154 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_153, {
			int_val_0,
			int_val_1
	}, "", label_102);
	LoadInst* ptr_155 = new LoadInst(ptr_154, "", false, label_102);
	ptr_155->setAlignment(4);
	ICmpInst* int1_156 = new ICmpInst(*label_102, ICmpInst::ICMP_NE, ptr_155, m_pTypeManager->GetFreeMemBlockNull(), "");
	BranchInst::Create(label_103, label_104, int1_156, label_102);

	// Block  (label_103)
	LoadInst* ptr_158 = new LoadInst(ptr_curr_108, "", false, label_103);
	ptr_158->setAlignment(4);
	LoadInst* ptr_159 = new LoadInst(ptr_curr_108, "", false, label_103);
	ptr_159->setAlignment(4);
	GetElementPtrInst* ptr_160 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_159, {
			int_val_0,
			int_val_1
	}, "", label_103);
	LoadInst* ptr_161 = new LoadInst(ptr_160, "", false, label_103);
	ptr_161->setAlignment(4);
	GetElementPtrInst* ptr_162 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_161, {
			int_val_0,
			int_val_2
	}, "", label_103);
	StoreInst* void_163 = new StoreInst(ptr_158, ptr_162, false, label_103);
	void_163->setAlignment(4);
	BranchInst::Create(label_105, label_103);

	// Block  (label_104)
	BranchInst::Create(label_107, label_104);

	// Block  (label_105)
	BranchInst::Create(label_106, label_105);

	// Block  (label_106)
	LoadInst* ptr_167 = new LoadInst(ptr_curr_108, "", false, label_106);
	ptr_167->setAlignment(4);
	GetElementPtrInst* ptr_168 = GetElementPtrInst::CreateInBounds(m_pTypeManager->GetFreeMemBlockStructTy(), ptr_167, {
			int_val_0,
			int_val_1
	}, "", label_106);
	LoadInst* ptr_169 = new LoadInst(ptr_168, "", false, label_106);
	ptr_169->setAlignment(4);
	StoreInst* void_170 = new StoreInst(ptr_169, ptr_curr_108, false, label_106);
	void_170->setAlignment(4);
	BranchInst::Create(label_100, label_106);

	// Block  (label_107)
	ReturnInst::Create(m_pMod->getContext(), label_107);
}

void FunctionManager::declareFree()
{
	PointerType* voidPtrType = PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	std::vector<Type*>free_args;
	free_args.push_back(voidPtrType);
	FunctionType* freeTy = FunctionType::get(
	/*Result=*/Type::getVoidTy(m_pMod->getContext()),
	/*Params=*/free_args,
	/*isVarArg=*/false);

	m_pFuncFree = m_pMod->getFunction("llvm_free");
	if (!m_pFuncFree)
	{
		m_pFuncFree = Function::Create(
		/*Type=*/freeTy,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"llvm_free", m_pMod);
		m_pFuncFree->setCallingConv(CallingConv::C);
	}
	AttributeList func__free_PAL;
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
		func__free_PAL = AttributeList::get(m_pMod->getContext(), Attrs);
	}
	m_pFuncFree->setAttributes(func__free_PAL);
}

void FunctionManager::defineFree()
{
	PointerType* voidPtrType = PointerType::get(IntegerType::get(m_pMod->getContext(), 8), 0);
	// This is the size of block_t, need to be careful in case we change the structure and the
	// size of the struct changes.
	ConstantInt* int_val_12 = ConstantInt::get(m_pMod->getContext(), APInt(32, StringRef("12"), 10));
	Function::arg_iterator args = m_pFuncFree->arg_begin();
	Value* ptr_ptr = &(*args);
	ptr_ptr->setName("ptr");

	BasicBlock* label_173 = BasicBlock::Create(m_pMod->getContext(), "",m_pFuncFree,0);

	// Block  (label_173)
	AllocaInst* ptr_174 = new AllocaInst(voidPtrType, 0, "", label_173);
	ptr_174->setAlignment(4);
	StoreInst* void_175 = new StoreInst(ptr_ptr, ptr_174, false, label_173);
	void_175->setAlignment(4);
	LoadInst* ptr_176 = new LoadInst(ptr_174, "", false, label_173);
	ptr_176->setAlignment(4);
	CastInst* int64_177 = new PtrToIntInst(ptr_176, IntegerType::get(m_pMod->getContext(), 32), "", label_173);
	BinaryOperator* int64_178 = BinaryOperator::Create(Instruction::Sub, int64_177, int_val_12, "", label_173);
	CastInst* ptr_179 = new IntToPtrInst(int64_178, voidPtrType, "", label_173);
	CastInst* ptr_180 = new BitCastInst(ptr_179, m_pTypeManager->GetFreeMemBlockPtTy(), "", label_173);
	CallInst* void_181 = CallInst::Create(m_pFuncAddMemBlock, ptr_180, "", label_173);
	void_181->setCallingConv(CallingConv::C);
	void_181->setTailCall(false);
	AttributeList void_181_PAL;
	void_181->setAttributes(void_181_PAL);

	CallInst* void_182 = CallInst::Create(m_pFuncScanMerge, "", label_173);
	void_182->setCallingConv(CallingConv::C);
	void_182->setTailCall(false);
	AttributeList void_182_PAL;
	void_182->setAttributes(void_182_PAL);

	ReturnInst::Create(m_pMod->getContext(), label_173);
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
	pMmapAddr->setAlignment(4);
	StoreInst* void_17 = new StoreInst(ptrToMmapAddr, pMmapAddr, false, inst);
	void_17->setAlignment(4);
	LoadInst* mmapAddr = new LoadInst(pMmapAddr, "", false, inst);
	mmapAddr->setAlignment(4);
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

CallInst* FunctionManager::replaceMallocWithMalloc(Instruction *inst, Value *sizeToAlloc)
{
	// allocate 4 bytes of memory for now (hardcode)
	// eventually we need to use the passed in value
	ConstantInt* const_int64_28 = ConstantInt::get(m_pMod->getContext(),
			APInt(32, StringRef("4"), 10));

	CallInst* mallocCallInst = CallInst::Create(m_pFuncMalloc, const_int64_28, ""/*, inst*/);
	mallocCallInst->setCallingConv(CallingConv::C);
	mallocCallInst->setTailCall(false);
	AttributeList malloc_PAL;
	mallocCallInst->setAttributes(malloc_PAL);
	ReplaceInstWithInst(inst, mallocCallInst);
	return mallocCallInst;
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

Value* FunctionManager::extractMallocArgs(CallInst *callInst)
{
	Value *args;
	CallSite CS(callInst);
	for (auto arg = CS.arg_begin(); arg != CS.arg_end(); arg++)
	{
		args = dyn_cast<Value>(arg);
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

