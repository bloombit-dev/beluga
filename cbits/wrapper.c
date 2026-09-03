#include "binaryninjacore.h"

void BNGetLowLevelILByIndexPtr(BNLowLevelILInstruction* out,
                               BNLowLevelILFunction* func, size_t i)
{
  *out = BNGetLowLevelILByIndex(func, i);
}


void BNGetMediumLevelILByIndexPtr(BNMediumLevelILInstruction* out,
                                  BNMediumLevelILFunction* func, size_t i)
{
  *out = BNGetMediumLevelILByIndex(func, i);
}


void BNGetMediumLevelSSAILByIndexPtr(BNMediumLevelILInstruction* out,
                                     BNMediumLevelILFunction* func, size_t i)
{
  *out = BNGetMediumLevelILByIndex(func, i);
}


void BNFromVariableIdentifierPtr(BNVariable* out, uint64_t index)
{
  *out = BNFromVariableIdentifier(index);
}


void BNGetCachedMediumLevelILPossibleValueSetPtr(BNPossibleValueSet* out,
                                                 BNMediumLevelILFunction* func, size_t i)
{
  *out = BNGetCachedMediumLevelILPossibleValueSet(func, i);
}

void BNGetFunctionParameterVariablesPtr(BNParameterVariablesWithConfidence* out,
                                        BNFunction* func)
{
  *out = BNGetFunctionParameterVariables(func);
}

void BNIsTypeSignedPtr(BNBoolWithConfidence* out, BNType* ty)
{
  *out = BNIsTypeSigned(ty);
}

void BNIsTypeConstPtr(BNBoolWithConfidence* out, BNType* ty)
{
  *out = BNIsTypeConst(ty);
}

void BNIsTypeVolatilePtr(BNBoolWithConfidence* out, BNType* ty)
{
  *out = BNIsTypeVolatile(ty);
}

void BNGetChildTypePtr(BNTypeWithConfidence* out, BNType* ty)
{
  *out = BNGetChildType(ty);
}


