/*
 * TypeManager.cpp
 */

#include "TypeManager.hpp"
#include "llvm/IR/DerivedTypes.h"


TypeManager::TypeManager(Module *mod)
{
	m_pMod = mod;
	InitFreeMemBlockTy();
}

void TypeManager::InitFreeMemBlockTy()
{
	m_pFreeMemBlockStructTy = m_pMod->getTypeByName("llvm.struct.free.mem.block");
	if (!m_pFreeMemBlockStructTy)
	{
		m_pFreeMemBlockStructTy = StructType::create(m_pMod->getContext(),
				"llvm.struct.free.mem.block");
	}
	std::vector<Type*>FreeMemBlock_fields;
	FreeMemBlock_fields.push_back(IntegerType::get(m_pMod->getContext(), 64));

	m_pFreeMemBlockPtTy = PointerType::get(m_pFreeMemBlockStructTy, 0);
	FreeMemBlock_fields.push_back(m_pFreeMemBlockPtTy);
	FreeMemBlock_fields.push_back(m_pFreeMemBlockPtTy);

	if (m_pFreeMemBlockStructTy->isOpaque())
	{
		m_pFreeMemBlockStructTy->setBody(FreeMemBlock_fields, /*isPacked=*/false);
	}
	m_pFreeMemBlockNull = ConstantPointerNull::get(m_pFreeMemBlockPtTy);
}

PointerType* TypeManager::GetFreeMemBlockPtTy()
{
	return m_pFreeMemBlockPtTy;
}

StructType* TypeManager::GetFreeMemBlockStructTy()
{
	return m_pFreeMemBlockStructTy;
}

ConstantPointerNull* TypeManager::GetFreeMemBlockNull()
{
	return m_pFreeMemBlockNull;
}

