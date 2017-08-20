#include "jit_common.hpp"

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static Module* TheModule;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::map<std::string, BasicBlock*> TheLabels;

/////////////////////////////////////////////////////////////
// JIT main
//////////////////////////////////////////////////////////////
#define MAX_COND_JUMP 128

extern "C"
{

/////////////////////////////////////////////////////////////
// LLVM debug functions
/////////////////////////////////////////////////////////////
void show_stack_for_llvm_stack(LVALUE* llvm_stack, LVALUE* llvm_stack_ptr, int var_num)
{
    printf("llvm_stack %p\n", llvm_stack);
    printf("llvm_stack_ptr %p\n", llvm_stack_ptr);
    printf("llvm_stack_ptr - llvm_stack %d\n", llvm_stack_ptr - llvm_stack);
    printf("var_num %d\n", var_num);

    int i;
    for(i=0; i<10; i++) {
        printf("stack[%d] %p %d\n", i, llvm_stack[i].value, llvm_stack[i].value);
    }
}

//////////////////////////////////////////////////////////////
// LLVM operation functions
//////////////////////////////////////////////////////////////
static LVALUE trunc_value(LVALUE* llvm_value, int size)
{
    LVALUE result = *llvm_value;

    Type* llvm_type = llvm_value->value->getType();
    Type::TypeID type_id = llvm_type->getTypeID();

    /// Constant Int ///
    if(llvm_value->constant_int_value) {
        ConstantInt* constant_int_value = dynamic_cast<ConstantInt*>(llvm_value->value);
        APInt apint_value = constant_int_value->getValue();

        int bit_width = constant_int_value->getBitWidth();
        bool signed_value = apint_value.isSignBit();

        switch(size) {
            case 1:
                if(signed_value) {
                    result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(1));
                }
                else {
                    result.value = ConstantInt::get(TheContext, apint_value.zextOrTrunc(1));
                }
                break;

            case 8:
                if(signed_value) {
                    result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(8));
                }
                else {
                    result.value = ConstantInt::get(TheContext, apint_value.zextOrTrunc(8));
                }
                break;

            case 16:
                if(signed_value) {
                    result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(16));
                }
                else {
                    result.value = ConstantInt::get(TheContext, apint_value.zextOrTrunc(16));
                }
                break;

            case 32:
                if(signed_value) {
                    result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(32));
                }
                else {
                    result.value = ConstantInt::get(TheContext, apint_value.zextOrTrunc(32));
                }
                break;

            case 64:
                if(signed_value) {
                    result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(64));
                }
                else {
                    result.value = ConstantInt::get(TheContext, apint_value.zextOrTrunc(64));
                }
                break;
        }
    }
    else if(llvm_value->constant_float_value) {
        ConstantFP* constant_float_value = dynamic_cast<ConstantFP*>(llvm_value->value);
        const APFloat apfloat_value = constant_float_value->getValueAPF();

        switch(size) {
            case 1: {
                APInt apint_value = apfloat_value.bitcastToAPInt();
                ConstantInt* value = ConstantInt::get(TheContext, apint_value);
                result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(1));
                }
                break;

            case 8: {
                APInt apint_value = apfloat_value.bitcastToAPInt();
                ConstantInt* value = ConstantInt::get(TheContext, apint_value);
                result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(8));
                }
                break;

            case 16: {
                APInt apint_value = apfloat_value.bitcastToAPInt();
                ConstantInt* value = ConstantInt::get(TheContext, apint_value);
                result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(16));
                }
                break;

            case 32: {
                APInt apint_value = apfloat_value.bitcastToAPInt();
                ConstantInt* value = ConstantInt::get(TheContext, apint_value);
                result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(32));
                }
                break;

            case 64: {
                APInt apint_value = apfloat_value.bitcastToAPInt();
                ConstantInt* value = ConstantInt::get(TheContext, apint_value);
                result.value = ConstantInt::get(TheContext, apint_value.sextOrTrunc(64));
                }
                break;
        }
    }
    /// Memory ///
    else if(type_id == Type::TypeID::FloatTyID) {
        switch(size) {
            case 1:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt1Ty(TheContext));
                break;

            case 8:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt8Ty(TheContext));
                break;

            case 16:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt16Ty(TheContext));
                break;

            case 32:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt32Ty(TheContext));
                break;

            case 64:
                result.value = Builder.CreateCast(Instruction::FPExt, llvm_value->value, Type::getDoubleTy(TheContext));
                result.value = Builder.CreateCast(Instruction::BitCast, result.value, Type::getInt64Ty(TheContext));
                break;
        }
    }
    else if(type_id == Type::TypeID::DoubleTyID) {
        switch(size) {
            case 1:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt1Ty(TheContext));
                break;

            case 8:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt8Ty(TheContext));
                break;

            case 16:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt16Ty(TheContext));
                break;

            case 32:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt32Ty(TheContext));
                break;

            case 64:
                result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt64Ty(TheContext));
                break;
        }
    }
    else if(llvm_type->isPointerTy()) {
        switch(size) {
            case 1:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt1Ty(TheContext));
                break;

            case 8:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt8Ty(TheContext));
                break;

            case 16:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt16Ty(TheContext));
                break;

            case 32:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt32Ty(TheContext));
                break;

            case 64:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt64Ty(TheContext));
                break;
        }
    }
    else {
        switch(size) {
            case 1:
                if(!llvm_type->isIntegerTy(1)) {
                    result.value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt1Ty(TheContext));
                }
                break;

            case 8:
                if(llvm_type->isIntegerTy(1)) {
                    result.value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt8Ty(TheContext));
                }
                else if(!llvm_type->isIntegerTy(8)) {
                    result.value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt8Ty(TheContext));
                }
                break;

            case 16:
                if(llvm_type->isIntegerTy(1) || llvm_type->isIntegerTy(8)) {
                    result.value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt16Ty(TheContext));
                }
                else if(llvm_type->isIntegerTy(16)) {
                }
                else {
                    result.value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt16Ty(TheContext));
                }
                break;

            case 32:
                if(llvm_type->isIntegerTy(1) ||llvm_type->isIntegerTy(8) || llvm_type->isIntegerTy(16)) {
                    result.value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt32Ty(TheContext));
                }
                else {
                    result.value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt32Ty(TheContext));
                }
                break;

            case 64:
                if(!llvm_type->isIntegerTy(64)) {
                    result.value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt64Ty(TheContext));
                }
                break;
        }
    }

    return result;
}

// size --> 32: float 64:double
static LVALUE trunc_value_to_float_or_double(LVALUE* llvm_value, int size)
{
    LVALUE result = *llvm_value;

    Type* llvm_type = llvm_value->value->getType();
    Type::TypeID type_id = llvm_type->getTypeID();

    /// Constant Int ///
    if(llvm_value->constant_int_value) {
        ConstantInt* constant_int_value = dynamic_cast<ConstantInt*>(llvm_value->value);
        APInt apint_value = constant_int_value->getValue();

        int bit_width = constant_int_value->getBitWidth();
        bool signed_value = apint_value.isSignBit();

        switch(size) {
            case 32:
                if(signed_value) {
                    result.value = Builder.CreateCast(Instruction::SIToFP, llvm_value->value, Type::getFloatTy(TheContext));
                }
                else {
                    result.value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getFloatTy(TheContext));
                }
                break;

            case 64:
                if(signed_value) {
                    result.value = Builder.CreateCast(Instruction::SIToFP, llvm_value->value, Type::getDoubleTy(TheContext));
                }
                else {
                    result.value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getDoubleTy(TheContext));
                }
                break;
        }
    }
    else if(llvm_value->constant_float_value) {
    }
    /// Memory ///
    else if(type_id == Type::TypeID::FloatTyID) {
        switch(size) {
            case 64:
                result.value = Builder.CreateCast(Instruction::FPExt, llvm_value->value, Type::getDoubleTy(TheContext));
                break;
        }
    }
    else if(type_id == Type::TypeID::DoubleTyID) {
        switch(size) {
            case 32:
                result.value = Builder.CreateCast(Instruction::FPTrunc, llvm_value->value, Type::getFloatTy(TheContext));
                break;

            case 64:
                break;
        }
    }
    else if(llvm_type->isPointerTy()) {
        switch(size) {
            case 32:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt32Ty(TheContext));
                result.value = Builder.CreateCast(Instruction::UIToFP, result.value, Type::getFloatTy(TheContext));
                break;

            case 64:
                result.value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt64Ty(TheContext));
                result.value = Builder.CreateCast(Instruction::UIToFP, result.value, Type::getDoubleTy(TheContext));
                break;
        }
    }
    else {
        switch(size) {
            case 32:
                result.value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getFloatTy(TheContext));
                break;

            case 64:
                result.value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getDoubleTy(TheContext));
                break;
        }
    }

    return result;
}

static LVALUE trunc_value_to_pointer(LVALUE* llvm_value)
{
    LVALUE result = *llvm_value;

    Type* llvm_type = llvm_value->value->getType();
    Type::TypeID type_id = llvm_type->getTypeID();

    /// Constant Int ///
    if(llvm_value->constant_int_value) {
        ConstantInt* constant_int_value = dynamic_cast<ConstantInt*>(llvm_value->value);
        APInt apint_value = constant_int_value->getValue();

        int bit_width = constant_int_value->getBitWidth();
        bool signed_value = apint_value.isSignBit();

        result.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value->value, PointerType::get(IntegerType::get(TheContext, 8), 0));
    }
    else if(llvm_value->constant_float_value) {
        ConstantFP* constant_float_value = dynamic_cast<ConstantFP*>(llvm_value->value);
        const APFloat apfloat_value = constant_float_value->getValueAPF();

        result.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value->value, PointerType::get(IntegerType::get(TheContext, 8), 0));
    }
    /// Memory ///
    else if(type_id == Type::TypeID::FloatTyID) {
        result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt64Ty(TheContext));
        result.value = Builder.CreateCast(Instruction::IntToPtr, result.value, PointerType::get(IntegerType::get(TheContext, 8), 0));
    }
    else if(type_id == Type::TypeID::DoubleTyID) {
        result.value = Builder.CreateCast(Instruction::BitCast, llvm_value->value, Type::getInt64Ty(TheContext));
        result.value = Builder.CreateCast(Instruction::IntToPtr, result.value, PointerType::get(IntegerType::get(TheContext, 8), 0));
    }
    else if(llvm_type->isPointerTy()) {
    }
    else {
        result.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value->value, PointerType::get(IntegerType::get(TheContext, 8), 0));
    }

    return result;
}

static void trunc_variable(LVALUE* llvm_value, int size)
{
    switch(size) {
        case 1:
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt8Ty(TheContext));

            break;

        case 2:
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt16Ty(TheContext));
            break;

        case 4:
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt32Ty(TheContext));
            break;

        case 8:
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt64Ty(TheContext));
            break;

        case 16:
            llvm_value->value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getFloatTy(TheContext));
            break;

        case 32:
            llvm_value->value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getDoubleTy(TheContext));
            break;

        case 64:
            llvm_value->value = Builder.CreateCast(Instruction::IntToPtr, llvm_value->value, PointerType::get(IntegerType::get(TheContext, 8), 0));
            break;
    }
}

static void cast_llvm_value_from_inst(LVALUE* llvm_value, int inst) 
{
    switch(inst) {
        case OP_BYTE_TO_CDOUBLE_CAST:
        case OP_SHORT_TO_CDOUBLE_CAST:
        case OP_INT_TO_CDOUBLE_CAST:
        case OP_LONG_TO_CDOUBLE_CAST:
        case OP_BOOL_TO_CDOUBLE_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::SIToFP, llvm_value->value, Type::getFloatTy(TheContext), "fvalue");
            llvm_value->value = Builder.CreateCast(Instruction::FPExt, llvm_value->value, Type::getDoubleTy(TheContext), "fvalue");
            break;

        case OP_UBYTE_TO_CDOUBLE_CAST:
        case OP_USHORT_TO_CDOUBLE_CAST:
        case OP_UINT_TO_CDOUBLE_CAST:
        case OP_CHAR_TO_CDOUBLE_CAST:
        case OP_ULONG_TO_CDOUBLE_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getFloatTy(TheContext), "fvalue");
            llvm_value->value = Builder.CreateCast(Instruction::FPExt, llvm_value->value, Type::getDoubleTy(TheContext), "fvalue");
            break;

        case OP_FLOAT_TO_CDOUBLE_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::FPExt, llvm_value->value, Type::getDoubleTy(TheContext), "fvalue");
            break;

        case OP_DOUBLE_TO_CDOUBLE_CAST:
            break;

        case OP_BYTE_TO_CFLOAT_CAST:
        case OP_SHORT_TO_CFLOAT_CAST:
        case OP_INT_TO_CFLOAT_CAST:
        case OP_LONG_TO_CFLOAT_CAST:
        case OP_BOOL_TO_CFLOAT_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::SIToFP, llvm_value->value, Type::getFloatTy(TheContext), "fvalue");
            break;

        case OP_UBYTE_TO_CFLOAT_CAST:
        case OP_USHORT_TO_CFLOAT_CAST:
        case OP_UINT_TO_CFLOAT_CAST:
        case OP_CHAR_TO_CFLOAT_CAST:
        case OP_ULONG_TO_CFLOAT_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getFloatTy(TheContext), "fvalue");
            break;

        case OP_BYTE_TO_CULONG_CAST:
        case OP_UBYTE_TO_CULONG_CAST:
        case OP_SHORT_TO_CULONG_CAST:
        case OP_USHORT_TO_CULONG_CAST:
        case OP_INT_TO_CULONG_CAST:
        case OP_UINT_TO_CULONG_CAST:
        case OP_LONG_TO_CULONG_CAST:
        case OP_ULONG_TO_CULONG_CAST:
        case OP_CHAR_TO_CULONG_CAST:
        case OP_POINTER_TO_CULONG_CAST:
        case OP_BOOL_TO_CULONG_CAST: {
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 64);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CULONG_CAST:
        case OP_DOUBLE_TO_CULONG_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::FPToUI, llvm_value->value, Type::getInt64Ty(TheContext));
            break;

        case OP_BYTE_TO_CLONG_CAST:
        case OP_UBYTE_TO_CLONG_CAST:
        case OP_SHORT_TO_CLONG_CAST:
        case OP_USHORT_TO_CLONG_CAST:
        case OP_INT_TO_CLONG_CAST:
        case OP_UINT_TO_CLONG_CAST:
        case OP_LONG_TO_CLONG_CAST:
        case OP_ULONG_TO_CLONG_CAST:
        case OP_CHAR_TO_CLONG_CAST:
        case OP_POINTER_TO_CLONG_CAST:
        case OP_BOOL_TO_CLONG_CAST: {
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 64);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CLONG_CAST:
        case OP_DOUBLE_TO_CLONG_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::FPToSI, llvm_value->value, Type::getInt64Ty(TheContext));
            break;

        case OP_BYTE_TO_CUSHORT_CAST:
        case OP_UBYTE_TO_CUSHORT_CAST:
        case OP_SHORT_TO_CUSHORT_CAST:
        case OP_USHORT_TO_CUSHORT_CAST:
        case OP_INT_TO_CUSHORT_CAST:
        case OP_UINT_TO_CUSHORT_CAST:
        case OP_LONG_TO_CUSHORT_CAST:
        case OP_ULONG_TO_CUSHORT_CAST:
        case OP_CHAR_TO_CUSHORT_CAST:
        case OP_POINTER_TO_CUSHORT_CAST:
        case OP_BOOL_TO_CUSHORT_CAST: {
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 16);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CUSHORT_CAST:
        case OP_DOUBLE_TO_CUSHORT_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::FPToUI, llvm_value->value, Type::getInt16Ty(TheContext));
            break;

        case OP_BYTE_TO_CSHORT_CAST:
        case OP_UBYTE_TO_CSHORT_CAST:
        case OP_SHORT_TO_CSHORT_CAST:
        case OP_USHORT_TO_CSHORT_CAST:
        case OP_INT_TO_CSHORT_CAST:
        case OP_UINT_TO_CSHORT_CAST:
        case OP_LONG_TO_CSHORT_CAST:
        case OP_ULONG_TO_CSHORT_CAST:
        case OP_CHAR_TO_CSHORT_CAST:
        case OP_POINTER_TO_CSHORT_CAST:
        case OP_BOOL_TO_CSHORT_CAST: {
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 16);
            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CSHORT_CAST:
        case OP_DOUBLE_TO_CSHORT_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::FPToSI, llvm_value->value, Type::getInt16Ty(TheContext));
            break;

        case OP_BYTE_TO_CUBYTE_CAST:
        case OP_UBYTE_TO_CUBYTE_CAST:
        case OP_SHORT_TO_CUBYTE_CAST:
        case OP_USHORT_TO_CUBYTE_CAST:
        case OP_INT_TO_CUBYTE_CAST:
        case OP_UINT_TO_CUBYTE_CAST:
        case OP_LONG_TO_CUBYTE_CAST:
        case OP_ULONG_TO_CUBYTE_CAST:
        case OP_CHAR_TO_CUBYTE_CAST:
        case OP_POINTER_TO_CUBYTE_CAST:
        case OP_BOOL_TO_CUBYTE_CAST: {
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 8);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CUBYTE_CAST:
        case OP_DOUBLE_TO_CUBYTE_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::FPToUI, llvm_value->value, Type::getInt8Ty(TheContext));
            break;

        case OP_BYTE_TO_CBYTE_CAST:
        case OP_UBYTE_TO_CBYTE_CAST:
        case OP_SHORT_TO_CBYTE_CAST:
        case OP_USHORT_TO_CBYTE_CAST:
        case OP_INT_TO_CBYTE_CAST:
        case OP_UINT_TO_CBYTE_CAST:
        case OP_LONG_TO_CBYTE_CAST:
        case OP_ULONG_TO_CBYTE_CAST:
        case OP_CHAR_TO_CBYTE_CAST:
        case OP_BOOL_TO_CBYTE_CAST: 
        case OP_POINTER_TO_CBYTE_CAST: {
            LVALUE llvm_value2 = trunc_value(llvm_value, 8);
            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CBYTE_CAST:
        case OP_DOUBLE_TO_CBYTE_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::FPToSI, llvm_value->value, Type::getInt8Ty(TheContext));
            break;

        case OP_BYTE_TO_UINTEGER_CAST:
        case OP_UBYTE_TO_UINTEGER_CAST:
        case OP_SHORT_TO_UINTEGER_CAST:
        case OP_USHORT_TO_UINTEGER_CAST:
        case OP_INT_TO_UINTEGER_CAST:
        case OP_UINT_TO_UINTEGER_CAST:
        case OP_LONG_TO_UINTEGER_CAST:
        case OP_ULONG_TO_UINTEGER_CAST:
        case OP_CHAR_TO_UINTEGER_CAST:
        case OP_POINTER_TO_UINTEGER_CAST:
        case OP_BOOL_TO_UINTEGER_CAST: {
            LVALUE llvm_value2 = trunc_value(llvm_value, 32);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_UINTEGER_CAST:
        case OP_DOUBLE_TO_UINTEGER_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::FPToUI, llvm_value->value, Type::getInt32Ty(TheContext));
            break;

        case OP_FLOAT_TO_INTEGER_CAST:
        case OP_DOUBLE_TO_INTEGER_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::FPToSI, llvm_value->value, Type::getInt32Ty(TheContext));
            break;

        case OP_BYTE_TO_INTEGER_CAST:
        case OP_UBYTE_TO_INTEGER_CAST:
        case OP_SHORT_TO_INTEGER_CAST:
        case OP_USHORT_TO_INTEGER_CAST:
        case OP_INT_TO_INTEGER_CAST:
        case OP_UINT_TO_INTEGER_CAST:
        case OP_LONG_TO_INTEGER_CAST:
        case OP_ULONG_TO_INTEGER_CAST:
        case OP_CHAR_TO_INTEGER_CAST:
        case OP_POINTER_TO_INTEGER_CAST:
        case OP_BOOL_TO_INTEGER_CAST:{
            LVALUE llvm_value2 = trunc_value(llvm_value, 32);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CFLOAT_CAST:
            break;

        case OP_DOUBLE_TO_CFLOAT_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::FPTrunc, llvm_value->value, Type::getFloatTy(TheContext), "fvalue");
            break;


        case OP_CBYTE_TO_SHORT_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::SExt, llvm_value->value, Type::getInt16Ty(TheContext), "value2");
            break;

        case OP_CBYTE_TO_INT_CAST:
        case OP_CSHORT_TO_INT_CAST : 
            llvm_value->value = Builder.CreateCast(Instruction::SExt, llvm_value->value, Type::getInt32Ty(TheContext), "value2");
            break;

        case OP_CBYTE_TO_LONG_CAST:
        case OP_CSHORT_TO_LONG_CAST : 
        case OP_INTEGER_TO_LONG_CAST:
        case OP_CBOOL_TO_LONG_CAST:
        case OP_CFLOAT_TO_LONG_CAST : 
        case OP_CDOUBLE_TO_LONG_CAST : 
            llvm_value->value = Builder.CreateCast(Instruction::SExt, llvm_value->value, Type::getInt64Ty(TheContext), "value2");
            break;

        case OP_CBYTE_TO_USHORT_CAST:
        case OP_CUBYTE_TO_SHORT_CAST:
        case OP_CUBYTE_TO_USHORT_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt16Ty(TheContext), "value2");
            break;

        case OP_CBYTE_TO_UINT_CAST:
        case OP_CBYTE_TO_CHAR_CAST:
        case OP_CUBYTE_TO_INT_CAST:
        case OP_CUBYTE_TO_UINT_CAST:
        case OP_CUBYTE_TO_CHAR_CAST:
        case OP_CSHORT_TO_UINT_CAST :
        case OP_CSHORT_TO_CHAR_CAST:
        case OP_CUSHORT_TO_INT_CAST : 
        case OP_CUSHORT_TO_UINT_CAST :
        case OP_CUSHORT_TO_CHAR_CAST:
        case OP_CFLOAT_TO_CHAR_CAST:
        case OP_CDOUBLE_TO_CHAR_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt32Ty(TheContext), "value2");
            break;

        case OP_CBYTE_TO_ULONG_CAST:
        case OP_CUBYTE_TO_LONG_CAST:
        case OP_CUBYTE_TO_ULONG_CAST:
        case OP_CSHORT_TO_ULONG_CAST :
        case OP_CUSHORT_TO_LONG_CAST : 
        case OP_CUSHORT_TO_ULONG_CAST :
        case OP_INTEGER_TO_ULONG_CAST:
        case OP_UINTEGER_TO_LONG_CAST : 
        case OP_UINTEGER_TO_ULONG_CAST:
        case OP_CBOOL_TO_ULONG_CAST:
        case OP_CCHAR_TO_LONG_CAST : 
        case OP_CFLOAT_TO_ULONG_CAST :
        case OP_CDOUBLE_TO_ULONG_CAST :
            llvm_value->value = Builder.CreateCast(Instruction::ZExt, llvm_value->value, Type::getInt64Ty(TheContext), "value2");
            break;


        case OP_CBYTE_TO_FLOAT_CAST:
        case OP_CSHORT_TO_FLOAT_CAST:
        case OP_INTEGER_TO_FLOAT_CAST:
        case OP_CLONG_TO_FLOAT_CAST: 
        case OP_CBOOL_TO_FLOAT_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::SIToFP, llvm_value->value, Type::getFloatTy(TheContext));
            break;



        case OP_CBYTE_TO_DOUBLE_CAST:
        case OP_CSHORT_TO_DOUBLE_CAST:
        case OP_INTEGER_TO_DOUBLE_CAST:
        case OP_CLONG_TO_DOUBLE_CAST: 
        case OP_CBOOL_TO_DOUBLE_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::SIToFP, llvm_value->value, Type::getDoubleTy(TheContext));
            break;




        case OP_CUBYTE_TO_FLOAT_CAST:
        case OP_CUSHORT_TO_FLOAT_CAST:
        case OP_UINTEGER_TO_FLOAT_CAST:
        case OP_CULONG_TO_FLOAT_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getFloatTy(TheContext));
            break;





        case OP_CUBYTE_TO_DOUBLE_CAST: 
        case OP_CUSHORT_TO_DOUBLE_CAST:
        case OP_UINTEGER_TO_DOUBLE_CAST:
        case OP_CULONG_TO_DOUBLE_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::UIToFP, llvm_value->value, Type::getDoubleTy(TheContext));
            break;





            
        case OP_CSHORT_TO_BYTE_CAST:
        case OP_CSHORT_TO_UBYTE_CAST :
        case OP_CUSHORT_TO_BYTE_CAST:
        case OP_CUSHORT_TO_UBYTE_CAST :
        case OP_INTEGER_TO_BYTE_CAST:
        case OP_INTEGER_TO_UBYTE_CAST:
        case OP_UINTEGER_TO_BYTE_CAST:
        case OP_UINTEGER_TO_UBYTE_CAST:
        case OP_CLONG_TO_BYTE_CAST:
        case OP_CLONG_TO_UBYTE_CAST:
        case OP_CULONG_TO_UBYTE_CAST:
        case OP_CULONG_TO_BYTE_CAST:
        case OP_CBOOL_TO_BYTE_CAST:
        case OP_CBOOL_TO_UBYTE_CAST:
        case OP_CCHAR_TO_BYTE_CAST:
        case OP_CCHAR_TO_UBYTE_CAST :
        case OP_CFLOAT_TO_BYTE_CAST:
        case OP_CFLOAT_TO_UBYTE_CAST :
        case OP_CDOUBLE_TO_BYTE_CAST:
        case OP_CDOUBLE_TO_UBYTE_CAST :
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt8Ty(TheContext), "value2");
            break;


        case OP_INTEGER_TO_SHORT_CAST:
        case OP_INTEGER_TO_USHORT_CAST:
        case OP_UINTEGER_TO_SHORT_CAST:
        case OP_UINTEGER_TO_USHORT_CAST:
        case OP_CLONG_TO_SHORT_CAST:
        case OP_CLONG_TO_USHORT_CAST:
        case OP_CULONG_TO_USHORT_CAST:
        case OP_CULONG_TO_SHORT_CAST:
        case OP_CBOOL_TO_SHORT_CAST:
        case OP_CBOOL_TO_USHORT_CAST:
        case OP_CCHAR_TO_SHORT_CAST:
        case OP_CCHAR_TO_USHORT_CAST :
        case OP_CFLOAT_TO_SHORT_CAST:
        case OP_CFLOAT_TO_USHORT_CAST :
        case OP_CDOUBLE_TO_SHORT_CAST:
        case OP_CDOUBLE_TO_USHORT_CAST :
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt16Ty(TheContext), "value2");
            break;

        case OP_CLONG_TO_INT_CAST:
        case OP_CLONG_TO_UINT_CAST:
        case OP_CULONG_TO_INT_CAST:
        case OP_CULONG_TO_UINT_CAST:
        case OP_CULONG_TO_CHAR_CAST:
        case OP_CCHAR_TO_INT_CAST : 
        case OP_CLONG_TO_CHAR_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::Trunc, llvm_value->value, Type::getInt32Ty(TheContext), "value2");
            break;


        case OP_CPOINTER_TO_BYTE_CAST:
        case OP_CPOINTER_TO_UBYTE_CAST :
            llvm_value->value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt8Ty(TheContext), "value2");
            break;

        case OP_CPOINTER_TO_SHORT_CAST:
        case OP_CPOINTER_TO_USHORT_CAST :
            llvm_value->value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt16Ty(TheContext), "value2");
            break;

        case OP_CPOINTER_TO_INT_CAST : 
        case OP_CPOINTER_TO_UINT_CAST :
        case OP_CPOINTER_TO_CHAR_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt32Ty(TheContext), "value2");
            break;

        case OP_CPOINTER_TO_LONG_CAST : 
        case OP_CPOINTER_TO_ULONG_CAST :
            llvm_value->value = Builder.CreateCast(Instruction::PtrToInt, llvm_value->value, Type::getInt64Ty(TheContext), "value2");
            break;




        case OP_CFLOAT_TO_DOUBLE_CAST: 
            llvm_value->value = Builder.CreateCast(Instruction::FPExt, llvm_value->value, Type::getDoubleTy(TheContext));
            break;

        case OP_CDOUBLE_TO_FLOAT_CAST:
            llvm_value->value = Builder.CreateCast(Instruction::FPTrunc, llvm_value->value, Type::getFloatTy(TheContext));
            break;



        case OP_CBYTE_TO_BYTE_CAST:
        case OP_CBYTE_TO_UBYTE_CAST:
        case OP_CUBYTE_TO_BYTE_CAST:
        case OP_CUBYTE_TO_UBYTE_CAST:

        case OP_CSHORT_TO_SHORT_CAST:
        case OP_CSHORT_TO_USHORT_CAST :
        case OP_CUSHORT_TO_SHORT_CAST:
        case OP_CUSHORT_TO_USHORT_CAST :

        case OP_INTEGER_TO_INT_CAST:
        case OP_INTEGER_TO_UINT_CAST:
        case OP_UINTEGER_TO_INT_CAST:
        case OP_UINTEGER_TO_UINT_CAST:

        case OP_CLONG_TO_LONG_CAST:
        case OP_CLONG_TO_ULONG_CAST:
        case OP_CULONG_TO_LONG_CAST:
        case OP_CULONG_TO_ULONG_CAST:

        case OP_INTEGER_TO_CHAR_CAST:
        case OP_UINTEGER_TO_CHAR_CAST:

        case OP_CBOOL_TO_INT_CAST : 
        case OP_CBOOL_TO_UINT_CAST :
        case OP_CBOOL_TO_CHAR_CAST: 

        case OP_CCHAR_TO_CHAR_CAST:
        case OP_CCHAR_TO_UINT_CAST :

        case OP_CFLOAT_TO_INT_CAST : 
        case OP_CFLOAT_TO_UINT_CAST :

        case OP_CFLOAT_TO_FLOAT_CAST:
        case OP_CDOUBLE_TO_INT_CAST : 
        case OP_CDOUBLE_TO_UINT_CAST :
            break;

        case OP_BYTE_TO_CPOINTER_CAST:
        case OP_UBYTE_TO_CPOINTER_CAST:
        case OP_SHORT_TO_CPOINTER_CAST:
        case OP_USHORT_TO_CPOINTER_CAST:
        case OP_INT_TO_CPOINTER_CAST:
        case OP_UINT_TO_CPOINTER_CAST:
        case OP_LONG_TO_CPOINTER_CAST:
        case OP_ULONG_TO_CPOINTER_CAST:
        case OP_CHAR_TO_CPOINTER_CAST: 
        case OP_BOOL_TO_CPOINTER_CAST: {
            llvm_value->value = Builder.CreateCast(Instruction::IntToPtr, llvm_value->value, PointerType::get(IntegerType::get(TheContext, 8), 0));
            }
            break;

        case OP_POINTER_TO_CPOINTER_CAST:
            break;

        case OP_BYTE_TO_CBOOL_CAST:
        case OP_UBYTE_TO_CBOOL_CAST:
        case OP_SHORT_TO_CBOOL_CAST:
        case OP_USHORT_TO_CBOOL_CAST:
        case OP_INT_TO_CBOOL_CAST:
        case OP_UINT_TO_CBOOL_CAST:
        case OP_LONG_TO_CBOOL_CAST:
        case OP_ULONG_TO_CBOOL_CAST:
        case OP_CHAR_TO_CBOOL_CAST:
        case OP_POINTER_TO_CBOOL_CAST:{
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 32);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_BOOL_TO_CBOOL_CAST: 
            break;

        case OP_FLOAT_TO_CBOOL_CAST:
        case OP_DOUBLE_TO_CBOOL_CAST: {
            llvm_value->value = Builder.CreateCast(Instruction::FPToUI, llvm_value->value, Type::getInt32Ty(TheContext));
            }
            break;

        case OP_BYTE_TO_CCHAR_CAST :
        case OP_UBYTE_TO_CCHAR_CAST:
        case OP_SHORT_TO_CCHAR_CAST:
        case OP_USHORT_TO_CCHAR_CAST:
        case OP_INT_TO_CCHAR_CAST:
        case OP_UINT_TO_CCHAR_CAST:
        case OP_LONG_TO_CCHAR_CAST:
        case OP_ULONG_TO_CCHAR_CAST:
        case OP_CHAR_TO_CCHAR_CAST:
        case OP_POINTER_TO_CCHAR_CAST:
        case OP_BOOL_TO_CCHAR_CAST: {
            LVALUE llvm_value2;
            llvm_value2 = trunc_value(llvm_value, 32);

            llvm_value->value = llvm_value2.value;
            }
            break;

        case OP_FLOAT_TO_CCHAR_CAST:
        case OP_DOUBLE_TO_CCHAR_CAST: {
            llvm_value->value = Builder.CreateCast(Instruction::FPToUI, llvm_value->value, Type::getInt32Ty(TheContext));
            }
            break;
    }
}

static LVALUE* get_stack_ptr_value_from_index(LVALUE* llvm_stack_ptr, int index)
{
    return llvm_stack_ptr + index;
}

static void dec_stack_ptr(LVALUE** llvm_stack_ptr, int value)
{
    int i;
    for(i=0; i<value; i++) {
        (*llvm_stack_ptr)->value = 0;
        (*llvm_stack_ptr)->lvar_address_index = -1;
        (*llvm_stack_ptr)->lvar_stored = FALSE;
        (*llvm_stack_ptr)->constant_int_value = FALSE;
        (*llvm_stack_ptr)->constant_float_value = FALSE;

        (*llvm_stack_ptr)--;
    }

    (*llvm_stack_ptr)->value = 0;
    (*llvm_stack_ptr)->lvar_address_index = -1;
    (*llvm_stack_ptr)->lvar_stored = FALSE;
    (*llvm_stack_ptr)->constant_int_value = FALSE;
    (*llvm_stack_ptr)->constant_float_value = FALSE;
}

static void push_value_to_stack_ptr(LVALUE** llvm_stack_ptr, LVALUE* value)
{
    (**llvm_stack_ptr) = *value;
    (*llvm_stack_ptr)++;
}

static void insert_value_to_stack_ptr_with_offset(LVALUE** llvm_stack_ptr, LVALUE* value, int offset)
{
    (*(*llvm_stack_ptr+offset)) = *value;
}

static void store_llvm_value_to_lvar_with_offset(LVALUE* llvm_stack, int index, LVALUE* llvm_value)
{
    /// 0 clear align 8 byte ///
    Value* zero = ConstantInt::get(TheContext, llvm::APInt(64, 0, true));
    Builder.CreateStore(zero, llvm_stack[index].value);

    /// store ///
    LVALUE llvm_value2;
    llvm_value2 = trunc_value(llvm_value, 64);
    
    /// go ///
    Builder.CreateAlignedStore(llvm_value2.value, llvm_stack[index].value, 8);

    llvm_stack[index].lvar_address_index = llvm_value->lvar_address_index;
    llvm_stack[index].lvar_stored = TRUE;
    llvm_stack[index].constant_int_value = FALSE;
    llvm_stack[index].constant_float_value = FALSE;
}

static void get_llvm_value_from_lvar_with_offset(LVALUE* result, LVALUE* llvm_stack, int index)
{
    LVALUE* llvm_value = llvm_stack + index;

    result->value = Builder.CreateLoad(llvm_value->value, "lvar"); // load from allocated value

    result->lvar_address_index = llvm_value->lvar_address_index;
    result->lvar_stored = llvm_value->lvar_stored;
    result->constant_int_value = FALSE;
    result->constant_float_value = FALSE;
}

static LVALUE get_vm_stack_ptr_value_from_index_with_aligned(std::map<std::string, Value*>& params, BasicBlock* current_block, int index, int align)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

/*
    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*index, true));
    Value* stack_pointer_offset_value = Builder.CreateAdd(lvalue, rvalue, "stack_pointer_offset_value", true, true);
*/
    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, index, true));
    Value* stack_pointer_offset_value = Builder.CreateGEP(lvalue, rvalue, "stack_pointer_offset_value");

    LVALUE result;
    result.value = Builder.CreateAlignedLoad(stack_pointer_offset_value, align, "stack_pointer_offset_value");

    switch(align) {
        case 1:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt8Ty(TheContext));
            break;

        case 2:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt16Ty(TheContext));
            break;

        case 4:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt32Ty(TheContext));
            break;

        case 8:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt64Ty(TheContext));
            break;
    }

    result.lvar_address_index = -1;
    result.lvar_stored = FALSE;
    result.constant_int_value = FALSE;
    result.constant_float_value = FALSE;

    return result;
}

static void inc_vm_stack_ptr(std::map<std::string, Value*>& params, BasicBlock* current_block, int value)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

/*
    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*value, true));
    Value* inc_ptr_value = Builder.CreateAdd(lvalue, rvalue, "inc_ptr_value", false, false);
*/

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, value, true));
    Value* inc_ptr_value = Builder.CreateGEP(lvalue, rvalue, "inc_ptr_value");

    std::string stack_ptr_arg_name("stack_ptr");
    params[stack_ptr_arg_name] = inc_ptr_value;

    Builder.CreateStore(inc_ptr_value, stack_ptr_address_value);
}

static void push_value_to_vm_stack_ptr_with_aligned(std::map<std::string, Value*>& params, BasicBlock* current_block, LVALUE* llvm_value)
{
    Builder.SetInsertPoint(current_block);

    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateAlignedLoad(stack_ptr_address_value, 8, "loaded_stack_ptr_address_value");

    /// zero clear///
    Value* zero = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)0);
    Builder.CreateAlignedStore(zero, loaded_stack_ptr_address_value, 8);

    /// trunc ///
    LVALUE llvm_value2;
    llvm_value2 = trunc_value(llvm_value, 64);

    /// store ///
    Builder.CreateAlignedStore(llvm_value2.value, loaded_stack_ptr_address_value, 8);

    inc_vm_stack_ptr(params, current_block, 1);
}

static LVALUE get_stack_value_from_index_with_aligned(std::map<std::string, Value*>& params, BasicBlock* current_block, int index, int align)
{
    std::string stack_name("stack");
    Value* stack_address_value = params[stack_name];

    LVALUE result;
    if(index > 0) {
        Value* lvalue = stack_address_value;
        Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, index, true));
        Value* stack_offset_address_value = Builder.CreateGEP(lvalue, rvalue, "stack_offset_address_value");


        result.value = Builder.CreateAlignedLoad(stack_offset_address_value, align, "stack_offset_value");
    }
    else {
        result.value = Builder.CreateAlignedLoad(stack_address_value, align, "stack_offset_value");
    }


    switch(align) {
        case 1:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt8Ty(TheContext));
            break;

        case 2:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt16Ty(TheContext));
            break;

        case 4:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt32Ty(TheContext));
            break;

        case 8:
            result.value = Builder.CreateCast(Instruction::Trunc, result.value, Type::getInt64Ty(TheContext));
            break;
    }

    result.lvar_address_index = -1;
    result.lvar_stored = FALSE;
    result.constant_int_value = FALSE;
    result.constant_float_value = FALSE;

    return result;
}

//////////////////////////////////////////////////////////////////////
// method call
//////////////////////////////////////////////////////////////////////
static void llvm_stack_to_vm_stack(LVALUE* llvm_stack_ptr, std::map<std::string, Value*> params, BasicBlock* current_block, int num)
{
    int i;
    for(i=0; i<num; i++) {
        LVALUE* llvm_value = llvm_stack_ptr + i - num;
        push_value_to_vm_stack_ptr_with_aligned(params, current_block, llvm_value);
    }
}

static void if_value_is_zero_ret_zero(Value* value, std::map<std::string, Value *> params, Function* function, BasicBlock** current_block)
{
    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Value* zero = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));

    Value* comp = Builder.CreateICmpEQ(value, zero, "ifcond");

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend);
    *current_block = entry_ifend;
}

static void if_value_is_null_ret_zero(Value* value, int value_bit, std::map<std::string, Value *> params, Function* function, BasicBlock** current_block)
{
    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Value* zero = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Value* null_ptr = Builder.CreateCast(Instruction::IntToPtr, zero, PointerType::get(IntegerType::get(TheContext, value_bit), 0));

    Value* comp = Builder.CreateICmpEQ(value, null_ptr, "ifcond");

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend);
    *current_block = entry_ifend;
}

static Value* get_value_from_char_array(char* str)
{
    Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)str);
    return ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
}

static void store_value_to_lvar_with_offset(std::map<std::string, Value*>& params, BasicBlock* current_block, int index, LVALUE* llvm_value)
{
    std::string lvar_arg_name("lvar");
    Value* lvar_value = params[lvar_arg_name];
    
    Value* lvalue = lvar_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, index, true));
    Value* lvar_offset_value = Builder.CreateGEP(lvalue, rvalue, "lvar_offset_value");

    LVALUE llvm_value2;
    llvm_value2 = trunc_value(llvm_value, 64);

    Builder.CreateAlignedStore(llvm_value2.value, lvar_offset_value, 8);
}

static void dec_vm_stack_ptr(std::map<std::string, Value*>& params, BasicBlock* current_block, int value)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

/*
    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*value, true));
    Value* dec_ptr_value = Builder.CreateSub(lvalue, rvalue, "dec_ptr_value", true, true);
*/

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, value, true));
    Value* dec_ptr_value = Builder.CreateGEP(lvalue, rvalue, "dec_ptr_value");

    Builder.CreateStore(dec_ptr_value, stack_ptr_address_value);
}

static LVALUE get_lvar_value_from_offset(std::map<std::string, Value*>& params, BasicBlock* current_block, int offset)
{
    std::string lvar_arg_name("lvar");
    Value* lvar_value = params[lvar_arg_name];

    Value* lvalue = lvar_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*offset, true));
    Value* offset_lvar = Builder.CreateAdd(lvalue, rvalue, "offset_lvar", true, true);

    LVALUE result;
    result.value = Builder.CreateAlignedLoad(offset_lvar, 8, "offset_lvar");
    result.lvar_address_index = -1;
    result.lvar_stored = TRUE;
    result.constant_int_value = FALSE;
    result.constant_float_value = FALSE;

    return result;
}

static StructType* get_vm_info_struct_type()
{
    StructType* result_type = StructType::create(TheContext, "vm_info_struct");
    std::vector<Type*> fields;
    Type* field_type1 = PointerType::get(IntegerType::get(TheContext, 8), 0); // try_catch_label_name
    fields.push_back(field_type1);
    Type* field_type2 = PointerType::get(IntegerType::get(TheContext, 64), 0); // current_stack
    fields.push_back(field_type2);
    Type* field_type3 = IntegerType::get(TheContext, 32); // current_var_num
    fields.push_back(field_type3);
    Type* field_type4 = PointerType::get(IntegerType::get(TheContext, 8), 0); // sname
    fields.push_back(field_type4);
    Type* field_type5 = IntegerType::get(TheContext, 32);                       // sline
    fields.push_back(field_type5);
    Type* field_type6 = IntegerType::get(TheContext, 32);                       // try_offset
    fields.push_back(field_type6);
    Type* field_type7 = PointerType::get(PointerType::get(IntegerType::get(TheContext, 8), 0), 0); // try_pc
    fields.push_back(field_type7);
    Type* field_type8 = PointerType::get(IntegerType::get(TheContext, 64), 0);      // try_code
    fields.push_back(field_type7);
    Type* field_type9 = IntegerType::get(TheContext, 64);                           // stack_id
    fields.push_back(field_type9);
    Type* field_type10 = ArrayType::get(IntegerType::get(TheContext, 8), EXCEPTION_MESSAGE_MAX);  // exception_message
    fields.push_back(field_type10);

    if(result_type->isOpaque()) {
        result_type->setBody(fields, false);
    }

    return result_type;
}

static AllocaInst* create_entry_block_alloca(Function* function, int index)
{
    IRBuilder<> builder(&function->getEntryBlock(), function->getEntryBlock().begin());
    char var_name[128];
    snprintf(var_name, 128, "lvar$%d$", index);
    return builder.CreateAlloca(Type::getInt64Ty(TheContext), 0, var_name);
}

static void call_entry_exception_object_with_class_name2(std::map<std::string, Value *> params, char* class_name, char* message)
{
    Function* entry_exception_object_fun = TheModule->getFunction("entry_exception_object_with_class_name2");

    std::vector<Value*> params2;

    std::string stack_ptr_address_name("stack_ptr_address");
    Value* param1 = params[stack_ptr_address_name];
    params2.push_back(param1);

    std::string stack_value_name("stack");
    Value* param2 = params[stack_value_name];
    params2.push_back(param2);

    std::string var_num_value_name("var_num");
    Value* param3 = params[var_num_value_name];
    params2.push_back(param3);

    std::string info_value_name("info");
    Value* param4 = params[info_value_name];
    params2.push_back(param4);

    Value* param5 = get_value_from_char_array(class_name);
    params2.push_back(param5);

    Value* param6 = get_value_from_char_array(message);
    params2.push_back(param6);

    (void)Builder.CreateCall(entry_exception_object_fun, params2);
}

static void if_value_is_zero_entry_exception_object(Value* value, int value_size, BOOL value_is_float, BOOL value_is_double, std::map<std::string, Value *> params, Function* function, BasicBlock** current_block, char* class_name, char* message)
{
    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Value* rvalue;
    if(value_is_float) {
        double value = 0.0;
        rvalue = ConstantFP::get(TheContext, llvm::APFloat(value)); 
        //rvalue = Builder.CreateCast(Instruction::FPTrunc, rvalue, Type::getFloatTy(TheContext));
    }
    else if(value_is_double) {
        double value = 0.0;
        rvalue = ConstantFP::get(TheContext, llvm::APFloat(value)); 
    }
    else {
        rvalue = ConstantInt::get(TheContext, llvm::APInt(value_size, 0, true));
    }

    Value* comp = Builder.CreateICmpEQ(value, rvalue, "ifcond");

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    call_entry_exception_object_with_class_name2(params, class_name, message);

    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend);
    *current_block = entry_ifend;
}

static void vm_lvar_to_llvm_lvar(LVALUE* llvm_stack,std::map<std::string, Value*>& params, BasicBlock* current_block, int var_num)
{
    int i;
    for(i=0; i<var_num; i++) {
        LVALUE llvm_value = get_stack_value_from_index_with_aligned(params, current_block, i, 8);
        store_llvm_value_to_lvar_with_offset(llvm_stack, i, &llvm_value);
    }
}

static void finish_method_call(Value* result, std::map<std::string, Value *> params, BasicBlock** current_block, Function* function, char** try_catch_label_name, sByteCode* code, int real_param_num, int var_num, LVALUE* llvm_stack, LVALUE* llvm_stack_ptr)
{
    // if result is FALSE ret 0
    Value* comp = Builder.CreateICmpNE(result, ConstantInt::get(TheContext, llvm::APInt(32, 1, true)), "ifcond");

    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    Function* try_catch_label_name_fun = TheModule->getFunction("get_try_catch_label_name");

    std::vector<Value*> params2;

    std::string info_value_name("info");
    Value* info_value = params[info_value_name];
    params2.push_back(info_value);

    Value* try_catch_label_name_value = Builder.CreateCall(try_catch_label_name_fun, params2);

    Value* zero = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Value* null_ptr = Builder.CreateCast(Instruction::IntToPtr, zero, PointerType::get(IntegerType::get(TheContext, 8), 0));

    Value* comp2 = Builder.CreateICmpNE(try_catch_label_name_value, null_ptr, "catchcond");

    BasicBlock* then_block2 = BasicBlock::Create(TheContext, "then_block_b", function);
    BasicBlock* entry_ifend2 = BasicBlock::Create(TheContext, "entry_ifend_b", function);

    Builder.CreateCondBr(comp2, then_block2, entry_ifend2);

    Builder.SetInsertPoint(then_block2);

    if(*try_catch_label_name == nullptr) {
/*
        Function* catch_function = TheModule->getFunction("catch_function");

        std::vector<Value*> params2;

        std::string info_value_name("info");
        Value* info_value = params[info_value_name];
        params2.push_back(info_value);

        Value* try_code = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)code);
        params2.push_back(try_code);

        Builder.CreateCall(catch_function, params2);
*/

        Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
        Builder.CreateRet(ret_value);
    }
    else {
        std::string try_catch_label_name_string(*try_catch_label_name);
        BasicBlock* label = TheLabels[try_catch_label_name_string];
        if(label == nullptr) {
            label = BasicBlock::Create(TheContext, *try_catch_label_name, function);
            TheLabels[try_catch_label_name_string] = label;
        }

        if(label == nullptr) {
/*
            Function* catch_function = TheModule->getFunction("catch_function");

            std::vector<Value*> params2;

            std::string info_value_name("info");
            Value* info_value = params[info_value_name];
            params2.push_back(info_value);

            Value* try_code = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)code);
            params2.push_back(try_code);

            Builder.CreateCall(catch_function, params2);
*/

            Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
            Builder.CreateRet(ret_value);
        }
        else {
            Function* try_fun = TheModule->getFunction("try_function");

            std::vector<Value*> params2;

            std::string info_value_name("info");
            Value* vminfo_value = params[info_value_name];
            params2.push_back(vminfo_value);

            Value* catch_label_name_offset_value  = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)0);
            params2.push_back(catch_label_name_offset_value);

            Value* try_offset_value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)0);
            params2.push_back(try_offset_value);

            std::string code_value_name("code");
            Value* code_value = params[code_value_name];
            params2.push_back(code_value);

            std::string constant_value_name("constant");
            Value* constant_value = params[constant_value_name];
            params2.push_back(constant_value);

            (void)Builder.CreateCall(try_fun, params2);

            Builder.CreateBr(label);
        }
    }

    Builder.SetInsertPoint(entry_ifend2);
    
    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend); 
    *current_block = entry_ifend;
}

static void lvar_of_vm_to_lvar_of_llvm(std::map<std::string, Value *> params, BasicBlock* current_block, LVALUE* llvm_stack, int var_num)
{
    int i;
    for(i=0; i<var_num; i++) {
        LVALUE llvm_value = get_stack_value_from_index_with_aligned(params, current_block, i, 8);
        store_llvm_value_to_lvar_with_offset(llvm_stack, i, &llvm_value);
    }
}

static void lvar_of_llvm_to_lvar_of_vm(std::map<std::string, Value *> params, BasicBlock* current_block, LVALUE* llvm_stack, int var_num)
{
    int i;
    for(i=0; i<var_num; i++) {
        LVALUE llvm_value;
        get_llvm_value_from_lvar_with_offset(&llvm_value, llvm_stack, i);

        if(llvm_value.value != 0) {
            store_value_to_lvar_with_offset(params, current_block, i, &llvm_value);
        }
    }
}

static void trunc_value_from_inst(LVALUE* value, int inst) 
{
    switch(inst) {
        case OP_ANDAND: 
        case OP_OROR:
            *value = trunc_value(value, 1);
            break;

        case OP_BADD:
        case OP_BSUB:
        case OP_BMULT:
        case OP_BDIV:
        case OP_BMOD: 
        case OP_BLSHIFT:
        case OP_BRSHIFT:
        case OP_BAND: 
        case OP_BXOR: 
        case OP_BOR: 
        case OP_BCOMPLEMENT:
        case OP_BEQ:
        case OP_BNOTEQ:
        case OP_BGT:
        case OP_BLE:
        case OP_BGTEQ:
        case OP_BLEEQ:
        case OP_UBADD: 
        case OP_UBSUB:
        case OP_UBMULT:
        case OP_UBDIV:
        case OP_UBMOD:
        case OP_UBLSHIFT:
        case OP_UBRSHIFT: 
        case OP_UBAND:
        case OP_UBXOR:
        case OP_UBOR:
        case OP_UBCOMPLEMENT:
        case OP_UBEQ:
        case OP_UBNOTEQ:
        case OP_UBGT:
        case OP_UBLE: 
        case OP_UBGTEQ: 
        case OP_UBLEEQ:
            *value = trunc_value(value, 8);
            break;

        case OP_SADD:
        case OP_SSUB:
        case OP_SMULT:
        case OP_SDIV:
        case OP_SMOD:
        case OP_SLSHIFT:
        case OP_SRSHIFT:
        case OP_SAND:
        case OP_SXOR:
        case OP_SOR:
        case OP_SCOMPLEMENT:
        case OP_SEQ:
        case OP_SNOTEQ:
        case OP_SGT:
        case OP_SLE:
        case OP_SGTEQ:
        case OP_SLEEQ:
        case OP_USADD:
        case OP_USSUB:
        case OP_USMULT:
        case OP_USDIV:
        case OP_USMOD:
        case OP_USLSHIFT:
        case OP_USRSHIFT:
        case OP_USAND:
        case OP_USXOR:
        case OP_USOR:
        case OP_USCOMPLEMENT:
        case OP_USEQ:
        case OP_USNOTEQ:
        case OP_USGT:
        case OP_USLE:
        case OP_USGTEQ:
        case OP_USLEEQ:
            *value = trunc_value(value, 16);
            break;

        case OP_IADD: 
        case OP_ISUB:
        case OP_IMULT:
        case OP_IDIV:
        case OP_IMOD:
        case OP_ILSHIFT: 
        case OP_IRSHIFT:
        case OP_IAND:
        case OP_IXOR: 
        case OP_IOR:
        case OP_ICOMPLEMENT:
        case OP_IEQ:
        case OP_INOTEQ: 
        case OP_IGT: 
        case OP_ILE: 
        case OP_ILEEQ: 
        case OP_IGTEQ: 
        case OP_UIADD:
        case OP_UISUB:
        case OP_UIMULT:
        case OP_UIDIV:
        case OP_UIMOD:
        case OP_UILSHIFT:
        case OP_UIRSHIFT:
        case OP_UIAND:
        case OP_UIXOR:
        case OP_UIOR:
        case OP_UICOMPLEMENT:
        case OP_UIEQ:
        case OP_UINOTEQ:
        case OP_UILE:
        case OP_UIGT:
        case OP_UIGTEQ:
        case OP_UILEEQ: 
        case OP_CADD:
        case OP_CSUB:
        case OP_CEQ:
        case OP_CNOTEQ:
        case OP_CGT:
        case OP_CLE:
        case OP_CGTEQ:
        case OP_CLEEQ:
        case OP_OBJ_IDENTIFY:
        case OP_LOGICAL_DENIAL:
        case OP_REGEQ: 
        case OP_REGNOTEQ:
            *value = trunc_value(value, 32);
            break;

        case OP_LADD:
        case OP_LSUB:
        case OP_LMULT:
        case OP_LMOD: 
        case OP_LDIV:
        case OP_LLSHIFT: 
        case OP_LRSHIFT:
        case OP_LAND: 
        case OP_LXOR: 
        case OP_LOR: 
        case OP_LCOMPLEMENT:
        case OP_LEQ: 
        case OP_LNOTEQ: 
        case OP_LGT:
        case OP_LLE:
        case OP_LGTEQ:
        case OP_LLEEQ:
        case OP_ULADD: 
        case OP_ULMOD:
        case OP_ULLSHIFT:
        case OP_ULRSHIFT:
        case OP_ULAND:
        case OP_ULXOR:
        case OP_ULOR:
        case OP_ULCOMPLEMENT:
        case OP_ULEQ: 
        case OP_ULNOTEQ: 
        case OP_ULGT: 
        case OP_ULLE: 
        case OP_ULGTEQ: 
        case OP_ULLEEQ:
            *value = trunc_value(value, 64);
            break;

        case OP_FADD:
        case OP_FSUB:
        case OP_FMULT:
        case OP_FDIV:
        case OP_FEQ:
        case OP_FNOTEQ:
        case OP_FGT:
        case OP_FLE:
        case OP_FGTEQ:
        case OP_FLEEQ:
            *value = trunc_value_to_float_or_double(value, 32);
            break;

        case OP_DADD:
        case OP_DSUB:
        case OP_DMULT:
        case OP_DDIV:
        case OP_DEQ:
        case OP_DNOTEQ:
        case OP_DGT:
        case OP_DLE:
        case OP_DGTEQ:
        case OP_DLEEQ:
            *value = trunc_value_to_float_or_double(value, 64);
            break;

        case OP_PADD: 
        case OP_PEQ: 
        case OP_PNOTEQ: 
        case OP_PGT: 
        case OP_PLE:
        case OP_PGTEQ: 
        case OP_PLEEQ: 
            *value = trunc_value_to_pointer(value);
            break;
    }
}



static void store_value_to_vm_lvar(std::map<std::string, Value*>& params, BasicBlock* current_block, int offset, LVALUE* llvm_value)
{
    std::string lvar_arg_name("lvar");
    Value* lvar_value = params[lvar_arg_name];

    Value* lvalue = lvar_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, offset, true));
    Value* lvar_offset_value = Builder.CreateGEP(lvalue, rvalue, "lvar_offset_value");

    /// 0 clear align 8 byte ///
    Value* zero = ConstantInt::get(TheContext, llvm::APInt(64, 0, true));
    Builder.CreateAlignedStore(zero, lvar_offset_value, 8);

    /// trunc ///
    LVALUE llvm_value2;
    llvm_value2 = trunc_value(llvm_value, 64);

    /// go ///
    Builder.CreateAlignedStore(llvm_value2.value, lvar_offset_value, 8);
}

static void llvm_lvar_to_vm_lvar(LVALUE* llvm_stack,std::map<std::string, Value*>& params, BasicBlock* current_block, int var_num)
{
    int i;
    for(i=0; i<var_num; i++) {
        LVALUE llvm_value;
        get_llvm_value_from_lvar_with_offset(&llvm_value, llvm_stack, i);

        if(llvm_value.lvar_stored) {
            store_value_to_vm_lvar(params, current_block, i, &llvm_value);
        }
    }
}

/////////////////////////////////////////////////////////////////////////////
/// JIT debug functions
/////////////////////////////////////////////////////////////////////////////
static Value* llvm_make_str_value(char* str)
{
    Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)str);
    Value* value = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));

    return value;
}

void show_stack_stat(CLVALUE** stack_ptr, CLVALUE* stack)
{
    printf("stack_ptr %p\n", stack_ptr);
    printf("*stack_ptr %p\n", *stack_ptr);
    printf("stack_ptr - stack %d\n", (int)(*stack_ptr - stack));
}

BOOL show_stack_in_jit(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info)
{
    printf("var_num %d\n", var_num);
    show_stack_stat(stack_ptr, stack);

    int i;
    for(i=0; i<10; i++) {
        if(*stack_ptr == stack + i) {
            printf("! stack [%d] %d(%ld) on %p\n", i, stack[i].mIntValue, stack[i].mULongValue, stack + i);
        }
        else {
            printf("  stack [%d] %d(%ld) on %p\n", i, stack[i].mIntValue, stack[i].mULongValue, stack + i);
        }
    }

    return TRUE;
}

void show_inst_in_jit(int opecode)
{
    switch(opecode) {
        case OP_BYTE_TO_CULONG_CAST:
            puts("OP_BYTE_TO_CULONG_CAST");
            break;

        case OP_UBYTE_TO_CULONG_CAST:
            puts("OP_UBYTE_TO_CULONG_CAST");
            break;

        case OP_SHORT_TO_CULONG_CAST :
            puts("OP_SHORT_TO_CULONG_CAST ");
            break;

        case OP_USHORT_TO_CULONG_CAST :
            puts("OP_USHORT_TO_CULONG_CAST ");
            break;

        case OP_INT_TO_CULONG_CAST :
            puts("OP_INT_TO_CULONG_CAST ");
            break;

        case OP_UINT_TO_CULONG_CAST :
            puts("OP_UINT_TO_CULONG_CAST ");
            break;

        case OP_LONG_TO_CULONG_CAST :
            puts("OP_LONG_TO_CULONG_CAST ");
            break;

        case OP_ULONG_TO_CULONG_CAST :
            puts("OP_ULONG_TO_CULONG_CAST ");
            break;

        case OP_FLOAT_TO_CULONG_CAST :
            puts("OP_FLOAT_TO_CULONG_CAST ");
            break;

        case OP_DOUBLE_TO_CULONG_CAST :
            puts("OP_DOUBLE_TO_CULONG_CAST ");
            break;

        case OP_CHAR_TO_CULONG_CAST :
            puts("OP_CHAR_TO_CULONG_CAST ");
            break;

        case OP_POINTER_TO_CULONG_CAST :
            puts("OP_POINTER_TO_CULONG_CAST ");
            break;

        case OP_LONG_TO_ULONG_CAST:
            puts("OP_LONG_TO_ULONG_CAST");
            break;

        case OP_BOOL_TO_CULONG_CAST :
            puts("OP_BOOL_TO_CULONG_CAST ");
            break;

        case OP_INT_TO_ULONG_CAST:
            puts("OP_INT_TO_ULONG_CAST");
            break;

        case OP_CREATE_BUFFER :
            puts("OP_CREATE_BUFFER");
            break;

        case OP_CREATE_PATH :
            puts("OP_CREATE_PATH");
            break;

        case OP_CREATE_ARRAY :
            puts("OP_CREATE_ARRAY");
            break;

        case OP_CREATE_CARRAY :
            puts("OP_CREATE_CARRAY");
            break;

        case OP_CREATE_SORTABLE_CARRAY :
            puts("OP_CREATE_SORTABLE_CARRAY");
            break;

        case OP_CREATE_EQUALABLE_CARRAY :
            puts("OP_CREATE_EQUALABLE_CARRAY");
            break;

        case OP_CREATE_LIST :
            puts("OP_CREATE_LIST");
            break;

        case OP_CREATE_SORTALBE_LIST :
            puts("OP_CREATE_SORTALBE_LIST");
            break;

        case OP_CREATE_EQUALABLE_LIST :
            puts("OP_CREATE_EQUALABLE_LIST");
            break;

        case OP_CREATE_TUPLE :
            puts("OP_CREATE_TUPLE");
            break;

        case OP_CREATE_HASH :
            puts("OP_CREATE_HASH");
            break;

        case OP_CREATE_BLOCK_OBJECT :
            puts("OP_CREATE_BLOCK_OBJECT");
            break;

        case OP_BYTE_TO_STRING_CAST :
            puts("OP_BYTE_TO_STRING_CAST");
            break;

        case OP_SHORT_TO_STRING_CAST :
            puts("OP_SHORT_TO_STRING_CAST");
            break;

        case OP_INT_TO_STRING_CAST :
            puts("OP_INT_TO_STRING_CAST");
            break;

        case OP_LONG_TO_STRING_CAST :
            puts("OP_LONG_TO_STRING_CAST");
            break;

        case OP_UBYTE_TO_STRING_CAST :
            puts("OP_UBYTE_TO_STRING_CAST");
            break;

        case OP_USHORT_TO_STRING_CAST :
            puts("OP_USHORT_TO_STRING_CAST");
            break;

        case OP_UINT_TO_STRING_CAST :
            puts("OP_UINT_TO_STRING_CAST");
            break;

        case OP_ULONG_TO_STRING_CAST :
            puts("OP_ULONG_TO_STRING_CAST");
            break;

        case OP_FLOAT_TO_STRING_CAST :
            puts("OP_FLOAT_TO_STRING_CAST");
            break;

        case OP_DOUBLE_TO_STRING_CAST :
            puts("OP_DOUBLE_TO_STRING_CAST");
            break;

        case OP_BOOL_TO_STRING_CAST :
            puts("OP_BOOL_TO_STRING_CAST");
            break;

        case OP_REGEX_TO_STRING_CAST :
            puts("OP_REGEX_TO_STRING_CAST");
            break;

        case OP_POINTER_TO_STRING_CAST :
            puts("OP_POINTER_TO_STRING_CAST");
            break;

        case OP_BYTE_TO_INTEGER_CAST :
            puts("OP_BYTE_TO_INTEGER_CAST");
            break;

        case OP_UBYTE_TO_INTEGER_CAST :
            puts("OP_UBYTE_TO_INTEGER_CAST");
            break;

        case OP_SHORT_TO_INTEGER_CAST :
            puts("OP_SHORT_TO_INTEGER_CAST");
            break;

        case OP_USHORT_TO_INTEGER_CAST :
            puts("OP_USHORT_TO_INTEGER_CAST");
            break;

        case OP_INT_TO_INTEGER_CAST :
            puts("OP_INT_TO_INTEGER_CAST");
            break;

        case OP_UINT_TO_INTEGER_CAST :
            puts("OP_UINT_TO_INTEGER_CAST");
            break;

        case OP_LONG_TO_INTEGER_CAST :
            puts("OP_LONG_TO_INTEGER_CAST");
            break;

        case OP_ULONG_TO_INTEGER_CAST :
            puts("OP_ULONG_TO_INTEGER_CAST");
            break;

        case OP_FLOAT_TO_INTEGER_CAST :
            puts("OP_FLOAT_TO_INTEGER_CAST");
            break;

        case OP_DOUBLE_TO_INTEGER_CAST :
            puts("OP_DOUBLE_TO_INTEGER_CAST");
            break;

        case OP_CHAR_TO_INTEGER_CAST :
            puts("OP_CHAR_TO_INTEGER_CAST");
            break;

        case OP_POINTER_TO_INTEGER_CAST :
            puts("OP_POINTER_TO_INTEGER_CAST");
            break;

        case OP_BOOL_TO_INTEGER_CAST :
            puts("OP_BOOL_TO_INTEGER_CAST");
            break;

        case OP_CHAR_TO_STRING_CAST :
            puts("OP_CHAR_TO_STRING_CAST");
            break;

        case OP_STORE_FIELD:
            puts("OP_STORE_FIELD");
            break;

        case OP_POP:
            puts("OP_POP");
            break;

        case OP_DUPE:
            puts("OP_DUPE");
            break;

        case OP_COND_JUMP :
            puts("OP_COND_JUMP");
            break;

        case OP_COND_NOT_JUMP:
            puts("OP_COND_NOT_JUMP");
            break;

        case OP_GOTO:
            puts("OP_GOTO");
            break;

        case OP_LABEL:
            puts("OP_LABEL");
            break;

        case OP_LOAD:
            puts("OP_LOAD");
            break;

        case OP_STORE:
            puts("OP_STORE");
            break;

        case OP_LDCBYTE: 
            puts("OP_LDCBYTE");
            break;

        case OP_LDCINT: 
            puts("OP_LDCINT");
            break;

        case OP_LDCNULL:
            puts("OP_LDCNULL");
            break;

        case OP_BADD:
            puts("OP_BAND");
            break;

        case OP_BSUB:
            puts("OP_BSUB");
            break;

        case OP_BMULT:
            puts("OP_BMULT");
            break;

        case OP_BDIV:
            puts("OP_BDIV");
            break;

        case OP_BMOD:
            puts("OP_BMOD");
            break;

        case OP_BLSHIFT:
            puts("OP_BLSHIFT");
            break;

        case OP_BRSHIFT:
            puts("OP_BRSHIFT");
            break;

        case OP_BAND:
            puts("OP_BAND");
            break;

        case OP_BXOR:
            puts("OP_BXOR");
            break;

        case OP_BOR:
            puts("OP_BOR");
            break;

        case OP_UBADD:
            puts("OP_UBAND");
            break;

        case OP_UBSUB:
            puts("OP_UBSUB");
            break;

        case OP_UBMULT:
            puts("OP_UBMULT");
            break;

        case OP_UBDIV:
            puts("OP_UBDIV");
            break;

        case OP_UBMOD:
            puts("OP_UBMOD");
            break;

        case OP_UBLSHIFT:
            puts("OP_UBLSHIFT");
            break;

        case OP_UBRSHIFT:
            puts("OP_UBRSHIFT");
            break;

        case OP_UBAND:
            puts("OP_UBAND");
            break;

        case OP_UBXOR:
            puts("OP_UBXOR");
            break;

        case OP_UBOR:
            puts("OP_BOR");
            break;

        case OP_SADD:
            puts("OP_SADD");
            break;

        case OP_SSUB:
            puts("OP_SSUB");
            break;

        case OP_SMULT: 
            puts("OP_SMULT");
            break;

        case OP_SDIV: 
            puts("OP_SDIV");
            break;

        case OP_SMOD: 
            puts("OP_SMOD");
            break;

        case OP_SLSHIFT: 
            puts("OP_SLSHIFT");
            break;

        case OP_SRSHIFT: 
            puts("OP_SRSHIFT");
            break;

        case OP_SAND: 
            puts("OP_SAND");
            break;

        case OP_SXOR: 
            puts("OP_SXOR");
            break;

        case OP_SOR: 
            puts("OP_SOR");
            break;

        case OP_USADD: 
            puts("OP_USADD");
            break;

        case OP_USSUB: 
            puts("OP_USSUB");
            break;

        case OP_USMULT: 
            puts("OP_USMULT");
            break;

        case OP_USDIV: 
            puts("OP_USDIV");
            break;

        case OP_USMOD: 
            puts("OP_USMOD");
            break;

        case OP_USLSHIFT: 
            puts("OP_USLSHIFT");
            break;

        case OP_USRSHIFT: 
            puts("OP_USRSHIFT");
            break;

        case OP_USAND: 
            puts("OP_USAND");
            break;

        case OP_USXOR: 
            puts("OP_USXOR");
            break;

        case OP_USOR: 
            puts("OP_USOR");
            break;

        case OP_IADD: 
            puts("OP_IADD");
            break;

        case OP_ISUB: 
            puts("OP_ISUB");
            break;

        case OP_IMULT: 
            puts("OP_IMULT");
            break;

        case OP_IDIV: 
            puts("OP_IDIV");
            break;

        case OP_IMOD: 
            puts("OP_IMOD");
            break;

        case OP_ILSHIFT: 
            puts("OP_ILSHIFT");
            break;

        case OP_IRSHIFT: 
            puts("OP_IRSHIFT");
            break;

        case OP_IAND: 
            puts("OP_IAND");
            break;

        case OP_IXOR: 
            puts("OP_IXOR");
            break;

        case OP_IOR: 
            puts("OP_IOR");
            break;

        case OP_UIADD: 
            puts("OP_UIADD");
            break;

        case OP_UISUB: 
            puts("OP_UISUB");
            break;

        case OP_UIMULT: 
            puts("OP_UIMULT");
            break;

        case OP_UIDIV: 
            puts("OP_UIDIV");
            break;

        case OP_UIMOD: 
            puts("OP_UIMOD");
            break;

        case OP_UILSHIFT: 
            puts("OP_UILSHIFT");
            break;

        case OP_UIRSHIFT: 
            puts("OP_UIRSHIFT");
            break;

        case OP_UIAND: 
            puts("OP_UIAND");
            break;

        case OP_UIXOR: 
            puts("OP_UIXOR");
            break;

        case OP_UIOR: 
            puts("OP_UIOR");
            break;

        case OP_LADD: 
            puts("OP_LADD");
            break;

        case OP_LSUB: 
            puts("OP_LSUB");
            break;

        case OP_LMULT: 
            puts("OP_LMULT");
            break;

        case OP_LDIV: 
            puts("OP_LDIV");
            break;

        case OP_LMOD: 
            puts("OP_LMOD");
            break;

        case OP_LLSHIFT: 
            puts("OP_LLSHIFT");
            break;

        case OP_LRSHIFT: 
            puts("OP_LRSHIFT");
            break;

        case OP_INVOKE_BLOCK:
            puts("OP_INVOKE_BLOCK");
            break;

        case OP_LAND: 
            puts("OP_LAND");
            break;

        case OP_LXOR: 
            puts("OP_LXOR");
            break;

        case OP_LOR: 
            puts("OP_LOR");
            break;

        case OP_ULADD: 
            puts("OP_ULADD");
            break;

        case OP_ULSUB: 
            puts("OP_ULSUB");
            break;

        case OP_ULMULT: 
            puts("OP_ULMULT");
            break;

        case OP_ULDIV: 
            puts("OP_ULDIV");
            break;

        case OP_ULMOD: 
            puts("OP_ULMOD");
            break;

        case OP_ULLSHIFT: 
            puts("OP_ULLSHIFT");
            break;

        case OP_ULRSHIFT: 
            puts("OP_ULRSHIFT");
            break;

        case OP_ULAND: 
            puts("OP_ULAND");
            break;

        case OP_ULXOR: 
            puts("OP_ULXOR");
            break;

        case OP_UBCOMPLEMENT:
            puts("OP_UBCOMPLEMENT");
            break;

        case OP_SCOMPLEMENT:
            puts("OP_SCOMPLEMENT");
            break;

        case OP_USCOMPLEMENT:
            puts("OP_USCOMPLEMENT");
            break;

        case OP_ICOMPLEMENT:
            puts("OP_ICOMPLEMENT");
            break;

        case OP_UICOMPLEMENT:
            puts("OP_UICOMPLEMENT");
            break;

        case OP_LCOMPLEMENT:
            puts("OP_LCOMPLEMENT");
            break;

        case OP_ULCOMPLEMENT:
            puts("OP_ULCOMPLEMENT");
            break;


        case OP_FADD:
            puts("OP_FADD");
            break;

        case OP_FSUB:
            puts("OP_FSUB");
            break;

        case OP_FMULT:
            puts("OP_FMULT");
            break;

        case OP_FDIV:
            puts("OP_FDIV");
            break;


        case OP_DADD:
            puts("OP_DADD");
            break;

        case OP_DSUB:
            puts("OP_DSUB");
            break;

        case OP_DMULT:
            puts("OP_DMULT");
            break;

        case OP_DDIV:
            puts("OP_DDIV");
            break;


        case OP_PADD:
            puts("OP_PADD");
            break;

        case OP_PSUB:
            puts("OP_PSUB");
            break;

        case OP_PPSUB:
            puts("OP_PPSUB");
            break;


        case OP_CADD:
            puts("OP_CADD");
            break;

        case OP_CSUB:
            puts("OP_CSUB");
            break;


        case OP_BEQ :
            puts("OP_BEQ ");
            break;

        case OP_BNOTEQ :
            puts("OP_BNOTEQ ");
            break;

        case OP_BGT :
            puts("OP_BGT ");
            break;

        case OP_BLE :
            puts("OP_BLE ");
            break;

        case OP_BGTEQ :
            puts("OP_BGTEQ ");
            break;

        case OP_BLEEQ :
            puts("OP_BLEEQ ");
            break;


        case OP_UBEQ :
            puts("OP_UBEQ ");
            break;

        case OP_UBNOTEQ :
            puts("OP_UBNOTEQ ");
            break;

        case OP_UBGT :
            puts("OP_UBGT ");
            break;

        case OP_UBLE :
            puts("OP_UBLE ");
            break;

        case OP_UBGTEQ :
            puts("OP_UBGTEQ ");
            break;

        case OP_UBLEEQ :
            puts("OP_UBLEEQ ");
            break;


        case OP_SEQ :
            puts("OP_SEQ ");
            break;

        case OP_SNOTEQ :
            puts("OP_SNOTEQ ");
            break;

        case OP_SGT :
            puts("OP_SGT ");
            break;

        case OP_SLE :
            puts("OP_SLE ");
            break;

        case OP_SGTEQ :
            puts("OP_SGTEQ ");
            break;

        case OP_SLEEQ :
            puts("OP_SLEEQ ");
            break;


        case OP_USEQ :
            puts("OP_USEQ ");
            break;

        case OP_USNOTEQ :
            puts("OP_USNOTEQ ");
            break;

        case OP_USGT :
            puts("OP_USGT ");
            break;

        case OP_USLE :
            puts("OP_USLE ");
            break;

        case OP_USGTEQ :
            puts("OP_USGTEQ ");
            break;

        case OP_USLEEQ :
            puts("OP_USLEEQ ");
            break;

        case OP_INOTEQ :
            puts("OP_INOTEQ ");
            break;

        case OP_IGT :
            puts("OP_IGT ");
            break;

        case OP_UIEQ :
            puts("OP_UIEQ ");
            break;

        case OP_UINOTEQ :
            puts("OP_UINOTEQ ");
            break;

        case OP_UIGT :
            puts("OP_UIGT ");
            break;

        case OP_UILE :
            puts("OP_UILE ");
            break;

        case OP_UIGTEQ :
            puts("OP_UIGTEQ ");
            break;

        case OP_UILEEQ :
            puts("OP_UILEEQ ");
            break;



        case OP_LEQ :
            puts("OP_LEQ ");
            break;

        case OP_LNOTEQ :
            puts("OP_LNOTEQ ");
            break;

        case OP_LGT :
            puts("OP_LGT ");
            break;

        case OP_LLE :
            puts("OP_LLE ");
            break;

        case OP_LGTEQ :
            puts("OP_LGTEQ ");
            break;

        case OP_LLEEQ :
            puts("OP_LLEEQ ");
            break;


        case OP_ULEQ :
            puts("OP_ULEQ ");
            break;

        case OP_ULNOTEQ :
            puts("OP_ULNOTEQ ");
            break;

        case OP_ULGT :
            puts("OP_ULGT ");
            break;

        case OP_ULLE :
            puts("OP_ULLE ");
            break;

        case OP_ULGTEQ :
            puts("OP_ULGTEQ ");
            break;

        case OP_ULLEEQ :
            puts("OP_ULLEEQ ");
            break;



        case OP_FEQ :
            puts("OP_FEQ ");
            break;

        case OP_FNOTEQ :
            puts("OP_FNOTEQ ");
            break;

        case OP_FGT :
            puts("OP_FGT ");
            break;

        case OP_FLE :
            puts("OP_FLE ");
            break;

        case OP_FGTEQ :
            puts("OP_FGTEQ ");
            break;

        case OP_FLEEQ :
            puts("OP_FLEEQ ");
            break;



        case OP_DEQ :
            puts("OP_DEQ ");
            break;

        case OP_DNOTEQ :
            puts("OP_DNOTEQ ");
            break;

        case OP_DGT :
            puts("OP_DGT ");
            break;

        case OP_DLE :
            puts("OP_DLE ");
            break;

        case OP_DGTEQ :
            puts("OP_DGTEQ ");
            break;

        case OP_DLEEQ :
            puts("OP_DLEEQ ");
            break;



        case OP_PEQ :
            puts("OP_PEQ ");
            break;

        case OP_PNOTEQ :
            puts("OP_PNOTEQ ");
            break;

        case OP_PGT :
            puts("OP_PGT ");
            break;

        case OP_PLE :
            puts("OP_PLE ");
            break;

        case OP_PGTEQ :
            puts("OP_PGTEQ ");
            break;

        case OP_PLEEQ :
            puts("OP_PLEEQ ");
            break;


        case OP_CEQ :
            puts("OP_CEQ ");
            break;

        case OP_CNOTEQ :
            puts("OP_CNOTEQ ");
            break;

        case OP_CGT :
            puts("OP_CGT ");
            break;

        case OP_CLE :
            puts("OP_CLE ");
            break;

        case OP_CGTEQ :
            puts("OP_CGTEQ ");
            break;

        case OP_CLEEQ :
            puts("OP_CLEEQ ");
            break;

        case OP_REGEQ :
            puts("OP_REGEQ ");
            break;

        case OP_REGNOTEQ :
            puts("OP_REGNOTEQ ");
            break;

        case OP_ULOR: 
            puts("OP_ULOR");
            break;

        case OP_RETURN: 
            puts("OP_RETURN");
            break;

        case OP_TRY:
            puts("OP_TRY");
            break;

        case OP_INVOKE_METHOD:
            puts("OP_INVOKE_METHOD");
            break;

        case OP_THROW:
            puts("OP_THROW");
            break;

        case OP_CREATE_STRING:
            puts("OP_CREATE_STRING");
            break;

        case OP_CREATE_REGEX:
            puts("OP_CREATE_REGEX");
            break;

        case OP_HEAD_OF_EXPRESSION: 
            puts("OP_HEAD_OF_EXPRESSION");
            break;

        case OP_SIGINT:
            puts("OP_SIGINT");
            break;

        case OP_NEW:
            puts("OP_NEW");
            break;

        case OP_BCOMPLEMENT:
            puts("OP_BCOMPLEMENT");
            break;

        case OP_IEQ:
            puts("OP_IEQ");
            break;

        case OP_ILE:
            puts("OP_ILE");
            break;

        case OP_IGTEQ:
            puts("OP_IGTEQ");
            break;

        case OP_ANDAND:
            puts("OP_ANDAND");
            break;

        case OP_LOAD_FIELD:
            puts("OP_LOAD_FIELD");
            break;

        case OP_BYTE_TO_INT_CAST:
            puts("OP_BYTE_TO_INT_CAST");
            break;

        case OP_LOAD_ELEMENT :
            puts("OP_LOAD_ELEMENT");
            break;

        case OP_STORE_ELEMENT:
            puts("OP_STORE_ELEMENT");
            break;

/*
        case OP_INT_TO_BYTE_CAST:
            puts("OP_INT_TO_BYTE_CAST");
            break;
*/

        default:
            printf("opecode %d\n", opecode);
            break;
    }
}

void show_number_in_jit(int number)
{
    printf("%d(%p)\n", number, number);
}

void call_show_number_in_jit(int number)
{
    Function* show_number = TheModule->getFunction("show_number_in_jit");

    std::vector<Value*> params2;
    Value* param1 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)number);
    params2.push_back(param1);

    Value* result = Builder.CreateCall(show_number, params2);
}

void call_show_value_in_jit(Value* value)
{
    Function* show_number = TheModule->getFunction("show_number_in_jit");

    std::vector<Value*> params2;
    Value* param1 = value;
    params2.push_back(param1);

    Value* result = Builder.CreateCall(show_number, params2);
}

void show_str_in_jit(char* str)
{
    printf("%s\n", str);
}

void call_show_str_in_jit(Value* value)
{
    Function* show_str = TheModule->getFunction("show_str_in_jit");

    std::vector<Value*> params2;
    params2.push_back(value);

    Value* result = Builder.CreateCall(show_str, params2);
}

void call_show_stack_stat(std::map<std::string, Value *> params)
{
    Function* show_address_fun = TheModule->getFunction("show_stack_stat");

    std::vector<Value*> params2;

    std::string stack_ptr_address_arg_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_arg_name];
    Value* param1 = stack_ptr_address_value;
    params2.push_back(param1);

    std::string stack_arg_name("stack");
    Value* stack_value = params[stack_arg_name];
    Value* param2 = stack_value;
    params2.push_back(param2);

    Value* result = Builder.CreateCall(show_address_fun, params2);
}


void call_show_inst_in_jit(int opecode)
{
    Function* show_inst = TheModule->getFunction("show_inst_in_jit");

    std::vector<Value*> params2;
    Value* param1 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)opecode);
    params2.push_back(param1);

    Value* result = Builder.CreateCall(show_inst, params2);
}

void call_show_stack(std::map<std::string, Value *> params)
{
    Function* show_stack_fun = TheModule->getFunction("show_stack_in_jit");

    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");
    std::string stack_name("stack");
    Value* stack_value = params[stack_name];

    std::vector<Value*> params2;
    Value* param1 = stack_ptr_address_value;
    params2.push_back(param1);

    Value* param2 = stack_value;
    params2.push_back(param2);

    std::string var_num_value_name("var_num");
    Value* param3 = params[var_num_value_name];
    params2.push_back(param3);

    std::string info_value_name("info");
    Value* param4 = params[info_value_name];
    params2.push_back(param4);

    Value* result = Builder.CreateCall(show_stack_fun, params2);
}

//////////////////////////////////////////////////////////////
// declare jit functions
//////////////////////////////////////////////////////////////
GlobalVariable* gSigIntValue;
StructType* gCLValueAndBoolStruct;
StructType* gPointerAndBoolStruct;

static void create_internal_functions()
{
    Type* result_type;
    std::vector<Type *> type_params;
    Type* param1_type;
    Type* param2_type;
    Type* param3_type;
    Type* param4_type;
    Type* param5_type;
    Type* param6_type;
    Type* param7_type;
    Type* param8_type;
    Type* param9_type;
    Type* param10_type;
    Type* param11_type;
    Type* param12_type;
    FunctionType* function_type;

    /// create_string_object ///
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_string_object", TheModule);

    /// create_byte ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 8);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_byte", TheModule);

    /// create_ubyte ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 8);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_ubyte", TheModule);

    /// create_short ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 16);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_short", TheModule);

    /// create_ushort ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 16);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_ushort", TheModule);

    /// create_integer ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_integer", TheModule);

    /// create_uinteger ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_uinteger", TheModule);

    /// create_long ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,64);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_long", TheModule);

    /// create_ulong ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,64);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_ulong", TheModule);

    /// create_float ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = Type::getFloatTy(TheContext);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_float", TheModule);

    /// create_double ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = Type::getDoubleTy(TheContext);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_double", TheModule);

    /// create_pointer ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_pointer", TheModule);

    /// create_char ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_char", TheModule);

    /// create_bool ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_bool", TheModule);

    /// create_buffer_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_buffer_object", TheModule);

    /// create_path_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_path_object", TheModule);

    /// create_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_object", TheModule);

    /// create_regex_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param7_type);

    param8_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param8_type);

    param9_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param9_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_regex_object", TheModule);

    /// create_array_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_array_object", TheModule);

    /// show_inst_in_jit ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_inst_in_jit", TheModule);

    /// show_number_in_jit ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_number_in_jit", TheModule);

    /// show_str_in_jit ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_str_in_jit", TheModule);

    /// show_stack_stat ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_stack_stat", TheModule);

    /// show_stack_in_jit ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_stack_in_jit", TheModule);

    /// call_invoke_method ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);
    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param3_type);

    param4_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param5_type);

    param6_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "call_invoke_method", TheModule);

    /// push_jit_object ///
    type_params.clear();

    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "push_jit_object", TheModule);

    /// try_function ///
    type_params.clear();

    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext,32);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext,32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param4_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "try_function", TheModule);

    /// catch_function ///
    type_params.clear();

    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "catch_function", TheModule);

    /// get_try_catch_label_name ///
    type_params.clear();

    result_type = PointerType::get(IntegerType::get(TheContext,8), 0);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_try_catch_label_name", TheModule);

    /// entry_exception_object ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "entry_exception_object", TheModule);

    /// get_field_from_object ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_field_from_object", TheModule);

    /// regex_equals ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "regex_equals", TheModule);

    /// get_string_object_of_object_name ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_string_object_of_object_name", TheModule);

    /// object_implements_interface ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);
    
    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "object_implements_interface", TheModule);

    /// call_invoke_virtual_method ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param5_type);

    param6_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param6_type);

    param7_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "call_invoke_virtual_method", TheModule);

    /// call_invoke_dynamic_method ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    param8_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param8_type);

    param9_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param9_type);

    param10_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param10_type);

    param11_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param11_type);

    param12_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param12_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "call_invoke_dynamic_method", TheModule);

    /// invoke_block_in_jit ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param5_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "invoke_block_in_jit", TheModule);

    /// store_field ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 64);
    type_params.push_back(param6_type);

    param7_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "store_field", TheModule);

    /// load_class_field ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "load_class_field", TheModule);

    /// run_load_class_field_address ///
    type_params.clear();
    
    result_type = PointerType::get(gPointerAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_class_field_address", TheModule);

    /// store_class_field ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    param8_type = IntegerType::get(TheContext, 64);
    type_params.push_back(param8_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "store_class_field", TheModule);

    /// load_element ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "load_element", TheModule);

    /// run_store_element ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = IntegerType::get(TheContext, 64);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_store_element", TheModule);

    /// get_array_length ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_array_length", TheModule);

    /// get_regex_global ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_global", TheModule);

    /// get_regex_ignorecase ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_ignorecase", TheModule);

    /// get_regex_multiline ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_multiline", TheModule);

    /// get_regex_extended ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_extended", TheModule);

    /// get_regex_dotall ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_dotall", TheModule);

    /// get_regex_anchored ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_anchored", TheModule);

    /// get_regex_dollar_endonly ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_dollar_endonly", TheModule);

    /// get_regex_ungreedy ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_ungreedy", TheModule);

    /// get_regex_anchored ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_regex_multiline", TheModule);

    /// char_uppercase ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "char_uppercase", TheModule);

    /// char_lowercase ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "char_lowercase", TheModule);

    /// run_create_array ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_array", TheModule);

    /// run_create_carray ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_carray", TheModule);

    /// run_create_equalable_carray ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);


    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_equalable_carray", TheModule);

    /// run_create_sortable_carray ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_sortable_carray", TheModule);

    /// run_create_list ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_list", TheModule);

    /// run_create_sortable_list ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_sortable_list", TheModule);

    /// run_create_equalable_list ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_equalable_list", TheModule);

    /// run_create_tuple ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_tuple", TheModule);

    /// run_create_hash ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param7_type);

    param8_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param8_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_hash", TheModule);

    /// run_create_block_object ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param3_type);

    param4_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    param7_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param7_type);

    param8_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param8_type);

    param9_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param9_type);

    param10_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param10_type);

    param11_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param11_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_block_object", TheModule);

    /// run_int_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_int_to_string_cast", TheModule);

    /// run_long_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 64);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_long_to_string_cast", TheModule);

    /// run_uint_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uint_to_string_cast", TheModule);

    /// run_ulong_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 64);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ulong_to_string_cast", TheModule);

    /// run_float_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = Type::getFloatTy(TheContext);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_float_to_string_cast", TheModule);

    /// run_double_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = Type::getDoubleTy(TheContext);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_double_to_string_cast", TheModule);

    /// run_bool_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_bool_to_string_cast", TheModule);

    /// run_regex_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_regex_to_string_cast", TheModule);

    /// run_pointer_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_pointer_to_string_cast", TheModule);
    /// run_char_to_string_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_char_to_string_cast", TheModule);

    /// run_cbyte_to_byte_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 8);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_byte_cast", TheModule);

    /// run_cshort_to_short_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 16);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_short_cast", TheModule);

    /// run_integer_to_int_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_int_cast", TheModule);

    /// run_cfloat_to_int_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_int_cast", TheModule);

    /// run_cdouble_to_int_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_int_cast", TheModule);

    /// run_clong_to_long_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 64);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_long_cast", TheModule);

    /// run_cubyte_to_ubyte_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 8);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_ubyte_cast", TheModule);

    /// run_cushort_to_ushort_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 16);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_ushort_cast", TheModule);
    /// run_culong_to_ulong_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 64);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_ulong_cast", TheModule);

    /// run_cpointer_to_pointer_cast ///
    type_params.clear();
    
    result_type = PointerType::get(IntegerType::get(TheContext, 8), 0);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_pointer_cast", TheModule);

    /// run_uinteger_to_uint_cast ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_uint_cast", TheModule);

    /// run_cfloat_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getFloatTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_float_cast", TheModule);

    /// run_cdouble_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getDoubleTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_double_cast", TheModule);

    /// entry_exception_object_with_class_name2 ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param5_type);

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "entry_exception_object_with_class_name2", TheModule);

    /// run_load_field_address ///
    type_params.clear();
    
    result_type = PointerType::get(gPointerAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_field_address", TheModule);

    /// run_array_to_carray_cast ///
    type_params.clear();
    
    result_type = PointerType::get(gCLValueAndBoolStruct, 0);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param5_type);

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_array_to_carray_cast", TheModule);

    /// get_class_with_load_and_initialize_in_jit ///
    type_params.clear();
    
    result_type = PointerType::get(IntegerType::get(TheContext, 64), 0);

    param1_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_class_with_load_and_initialize_in_jit", TheModule);

    /// gSigInt ///
    Type* variable_type = IntegerType::get(TheContext, 32);
    gSigIntValue = new GlobalVariable(*TheModule, variable_type, false, GlobalValue::ExternalLinkage, nullptr, "gSigInt");
}

//////////////////////////////////////////////////////////////
// JIT init
//////////////////////////////////////////////////////////////
void jit_init()
{
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    /// CLVALUE and BOOL Struct type ///
    gCLValueAndBoolStruct = StructType::create(TheContext, "clvalue_and_bool_struct");

    std::vector<Type*> fields;
    Type* field_type1 = IntegerType::get(TheContext, 64);
    fields.push_back(field_type1);
    Type* field_type2 = IntegerType::get(TheContext, 32);
    fields.push_back(field_type2);

    if(gCLValueAndBoolStruct->isOpaque()) {
        gCLValueAndBoolStruct->setBody(fields, false);
    }

    /// CLVALUE and BOOL Struct type ///
    fields.clear();

    gPointerAndBoolStruct = StructType::create(TheContext, "pointer_and_bool_struct");

    field_type1 = PointerType::get(IntegerType::get(TheContext, 8), 0);
    fields.push_back(field_type1);
    field_type2 = IntegerType::get(TheContext, 32);
    fields.push_back(field_type2);

    if(gPointerAndBoolStruct->isOpaque()) {
        gPointerAndBoolStruct->setBody(fields, false);
    }
}

void jit_final()
{
}

//////////////////////////////////////////////////////////////
// LLVM core
//////////////////////////////////////////////////////////////
#define ANDAND_OROR_MAX 128

static Function* create_llvm_function(const std::string& name)
{
    std::vector<Type *> params;

    Type* param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    params.push_back(param1_type);
    Type* param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    params.push_back(param2_type);
    Type* param3_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    params.push_back(param3_type);
    Type* param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    params.push_back(param4_type);
    Type* param5_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    params.push_back(param5_type);
    Type* param6_type = IntegerType::get(TheContext, 32);
    params.push_back(param6_type);
    Type* param7_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    params.push_back(param7_type);
    Type* param8_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    params.push_back(param8_type);

    Type* result_type = IntegerType::get(TheContext, 32);
    FunctionType* function_type = FunctionType::get(result_type, params, false);

    Function* function = Function::Create(function_type, Function::ExternalLinkage, name, TheModule);
    std::vector<std::string> args;

    std::string stack_ptr_name("stack_ptr");
    args.push_back(stack_ptr_name);
    std::string lvar_name("lvar");
    args.push_back(lvar_name);
    std::string info_name("info");
    args.push_back(info_name);
    std::string stack_name("stack");
    args.push_back(stack_name);
    std::string stack_ptr_address_name("stack_ptr_address");
    args.push_back(stack_ptr_address_name);
    std::string var_num_name("var_num");
    args.push_back(var_num_name);
    std::string constant_name("constant");
    args.push_back(constant_name);
    std::string code_name("code");
    args.push_back(code_name);

    unsigned index = 0;
    for (auto &arg : function->args()) {
        arg.setName(args[index++]);
    }

    return function;
}

static BOOL compile_to_native_code(sByteCode* code, sConst* constant, sCLClass* klass, sCLMethod* method, char* method_path2)
{
    char* try_catch_label_name = NULL;

    std::string func_name(method_path2);
    Function* function = create_llvm_function(func_name);

    // Create a new basic block to start insertion into.
    BasicBlock* current_block = BasicBlock::Create(TheContext, "entry", function);
    Builder.SetInsertPoint(current_block);
  
    register char* pc = code->mCodes;

    std::map<std::string, Value *> params;
    for(auto &param : function->args()) {
        params[param.getName()] = &param;
    }

    int num_cond_jump = 0;
    char* cond_jump_labels[MAX_COND_JUMP];
    BasicBlock* entry_condends[MAX_COND_JUMP];

    int num_cond_not_jump = 0;
    char* cond_not_jump_labels[MAX_COND_JUMP];
    BasicBlock* entry_condnotends[MAX_COND_JUMP];

    /// Stack to LLVM ///
    int var_num = method->mVarNum;

    LVALUE llvm_stack[CLOVER_STACK_SIZE];
    memset(llvm_stack, 0, sizeof(LVALUE)*CLOVER_STACK_SIZE);

    LVALUE* llvm_stack_ptr = llvm_stack + var_num;

    /// alloc local variables ///
    int i;
    for(i=0; i<var_num; i++) {
        llvm_stack[i].value = create_entry_block_alloca(function, i);

        llvm_stack[i].lvar_address_index = -1;
        llvm_stack[i].lvar_stored = FALSE;
        llvm_stack[i].constant_int_value = FALSE;
        llvm_stack[i].constant_float_value = FALSE;
    }

    /// parametor from VM stack ptr ///
    int real_param_num = method->mNumParams + ((method->mFlags & METHOD_FLAGS_CLASS_METHOD) ? 0:1);
    for(i=0; i<real_param_num; i++) {
        LVALUE llvm_value = get_stack_value_from_index_with_aligned(params, current_block, i, 8);

        store_llvm_value_to_lvar_with_offset(llvm_stack, i, &llvm_value);
    }

    /// clear local variable ///
    for(i=real_param_num; i<var_num; i++) {
        LVALUE llvm_value;
        llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(64, 0, true));
        llvm_stack[i].lvar_address_index = -1;
        llvm_stack[i].lvar_stored = FALSE;
        llvm_stack[i].constant_int_value = FALSE;
        llvm_stack[i].constant_float_value = FALSE;

        store_llvm_value_to_lvar_with_offset(llvm_stack, i, &llvm_value);
    }

    Value* value_for_andand_oror[ANDAND_OROR_MAX];
    memset(value_for_andand_oror, 0, sizeof(Value*)*ANDAND_OROR_MAX);
    int num_value_for_andand_oror = 0;

    while(pc - code->mCodes < code->mLen) {
        int k;
        for(k=0; k<num_cond_jump; k++) {
            if(pc == cond_jump_labels[k]) {
                //Builder.CreateBr(entry_condends[k]);
                Builder.SetInsertPoint(entry_condends[k]);
                current_block = entry_condends[k];

                int j;
                for(j=k; j<num_cond_jump; j++) {
                    cond_jump_labels[j] = cond_jump_labels[j+1];
                    entry_condends[j] = entry_condends[j+1];
                }
                num_cond_jump--;
            }
        }
        for(k=0; k<num_cond_not_jump; k++) {
            if(pc == cond_not_jump_labels[k]) {
                //Builder.CreateBr(entry_condnotends[k]);
                Builder.SetInsertPoint(entry_condnotends[k]);
                current_block = entry_condnotends[k];

                int j;
                for(j=k; j<num_cond_not_jump; j++) {
                    cond_not_jump_labels[j] = cond_not_jump_labels[j+1];
                    entry_condnotends[j] = entry_condnotends[j+1];
                }
                num_cond_not_jump--;
            }
        }

        unsigned int inst = *(unsigned int*)pc;
        pc+=sizeof(int);

//#ifdef MDEBUG
if(inst != OP_HEAD_OF_EXPRESSION && inst != OP_SIGINT) {
//call_show_inst_in_jit(inst);
//show_inst_in_jit(inst);
}
//#endif

        switch(inst) {
            case OP_POP:
                dec_stack_ptr(&llvm_stack_ptr, 1);
                break;

            case OP_POP_N:
                {
                    int value = *(int*)pc;
                    pc += sizeof(int);

                    dec_stack_ptr(&llvm_stack_ptr, value);
                }
                break;

            case OP_REVERSE: {
                LVALUE llvm_value = *get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE llvm_value2 = *get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                
                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_DUPE: {
                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                push_value_to_stack_ptr(&llvm_stack_ptr, llvm_value);
                }
                break;

            case OP_COND_JUMP: {
                int jump_value = *(int*)pc;
                pc += sizeof(int);

                LVALUE* conditional_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                BasicBlock* cond_jump_then_block = BasicBlock::Create(TheContext, "cond_jump_then", function);
                entry_condends[num_cond_jump] = BasicBlock::Create(TheContext, "entry_condend", function);

                LVALUE llvm_value;
                llvm_value = trunc_value(conditional_value, 1);

                Builder.CreateCondBr(llvm_value.value, entry_condends[num_cond_jump], cond_jump_then_block);

                Builder.SetInsertPoint(cond_jump_then_block);

                current_block = cond_jump_then_block;

                cond_jump_labels[num_cond_jump] = pc + jump_value;
                num_cond_jump++;

                if(num_cond_jump >= MAX_COND_JUMP) {
                    fprintf(stderr, "overflow number of condjump\n");
                    return FALSE;
                }

                dec_stack_ptr(&llvm_stack_ptr, 1);
                }
                break;

            case OP_COND_NOT_JUMP: {
                int jump_value = *(int*)pc;
                pc += sizeof(int);

                LVALUE* conditional_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                BasicBlock* cond_not_jump_then_block = BasicBlock::Create(TheContext, "cond_not_jump_then", function);
                entry_condnotends[num_cond_not_jump] = BasicBlock::Create(TheContext, "entry_condnotend", function);
                LVALUE llvm_value;
                llvm_value = trunc_value(conditional_value, 1);

                Builder.CreateCondBr(llvm_value.value, cond_not_jump_then_block, entry_condnotends[num_cond_not_jump]);

                Builder.SetInsertPoint(cond_not_jump_then_block);

                current_block = cond_not_jump_then_block;

                cond_not_jump_labels[num_cond_not_jump] = pc + jump_value;
                num_cond_not_jump++;

                if(num_cond_not_jump >= MAX_COND_JUMP) {
                    fprintf(stderr, "overflow number of condnotjump\n");
                    return FALSE;
                }

                dec_stack_ptr(&llvm_stack_ptr, 1);
                }
                break;

            case OP_GOTO: {
                int jump_value = *(int*)pc;
                pc += sizeof(int);

                int label_offset = *(int*)pc;
                pc += sizeof(int);

                char* label_name = CONS_str(constant, label_offset);
                std::string label_name_string(label_name);

                BasicBlock* label = TheLabels[label_name_string];
                if(label == nullptr) {
                    label = BasicBlock::Create(TheContext, label_name, function);
                    TheLabels[label_name_string] = label;
                }

                Builder.CreateBr(label);

                Builder.SetInsertPoint(label);
                current_block = label;
                
                BasicBlock* entry_after_goto = BasicBlock::Create(TheContext, "entry_after_goto", function);
                Builder.SetInsertPoint(entry_after_goto);
                current_block = entry_after_goto;
                }
                break;

            case OP_RETURN: {
                std::string stack_param_name("stack");
                Value* stack_value = params[stack_param_name];

                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(llvm_value, 64);

                Builder.CreateAlignedStore(llvm_value2.value, stack_value, 8);

                Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 1, true));
                Builder.CreateRet(ret_value);

                BasicBlock* entry_after_return = BasicBlock::Create(TheContext, "entry_after_return", function);
                Builder.SetInsertPoint(entry_after_return);
                current_block = entry_after_return;
                }
                break;

            case OP_THROW: {
                std::string stack_param_name("stack");
                Value* stack_value = params[stack_param_name];

                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(llvm_value, 64);

                Builder.CreateAlignedStore(llvm_value2.value, stack_value, 8);

                Function* entry_exception_object_fun = TheModule->getFunction("entry_exception_object");

                LVALUE llvm_value3;
                llvm_value3 = trunc_value(llvm_value, 32);

                std::vector<Value*> params2;

                Value* param1 = llvm_value3.value;
                params2.push_back(param1);

                std::string info_value_name("info");
                Value* vminfo_value = params[info_value_name];
                params2.push_back(vminfo_value);

                (void)Builder.CreateCall(entry_exception_object_fun, params2);

                Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
                Builder.CreateRet(ret_value);

                current_block = BasicBlock::Create(TheContext, "entry_after_throw", function);
                Builder.SetInsertPoint(current_block);
                }
                break;

            case OP_TRY: {
                int try_offset = *(int*)pc;
                pc += sizeof(int);

                int catch_label_name_offset = *(int*)pc;
                pc += sizeof(int);

                Function* try_fun = TheModule->getFunction("try_function");

                std::vector<Value*> params2;

                std::string info_value_name("info");
                Value* vminfo_value = params[info_value_name];
                params2.push_back(vminfo_value);

                Value* catch_label_name_offset_value  = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)catch_label_name_offset);
                params2.push_back(catch_label_name_offset_value);

                Value* try_offset_value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)try_offset);
                params2.push_back(try_offset_value);

                std::string code_value_name("code");
                Value* code_value = params[code_value_name];
                params2.push_back(code_value);

                std::string constant_value_name("constant");
                Value* constant_value = params[constant_value_name];
                params2.push_back(constant_value);

                (void)Builder.CreateCall(try_fun, params2);
                }
                break;

            case OP_TRY_END: {
                try_catch_label_name = NULL;

                Function* try_fun = TheModule->getFunction("try_function");

                std::vector<Value*> params2;

                std::string info_value_name("info");
                Value* vminfo_value = params[info_value_name];
                params2.push_back(vminfo_value);

                Value* catch_label_name_offset_value  = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)0);
                params2.push_back(catch_label_name_offset_value);

                Value* try_offset_value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)0);
                params2.push_back(try_offset_value);

                std::string code_value_name("code");
                Value* code_value = params[code_value_name];
                params2.push_back(code_value);

                std::string constant_value_name("constant");
                Value* constant_value = params[constant_value_name];
                params2.push_back(constant_value);

                (void)Builder.CreateCall(try_fun, params2);

                }
                break;

            case OP_CATCH_POP:
                break;

            case OP_CATCH_STORE: {
                pc += sizeof(int);
                }
                break;

            case OP_HEAD_OF_EXPRESSION: {
                Value* sig_int_value = ConstantInt::get(Type::getInt32Ty(TheContext), 0);
                Builder.CreateStore(sig_int_value, gSigIntValue);
                }
                break;

            case OP_SIGINT: {
                Value* sig_int_value = Builder.CreateLoad(gSigIntValue, "sig_int_value");

                BasicBlock* then_block = BasicBlock::Create(TheContext, "sigint_then_block", function);
                BasicBlock* else_block = BasicBlock::Create(TheContext, "entry_after_sigint", function);

                Value* value = Builder.CreateCast(Instruction::Trunc, sig_int_value, Type::getInt8Ty(TheContext));

                Builder.CreateCondBr(value, then_block, else_block);

                Builder.SetInsertPoint(then_block);

                Value* llvm_value1 = ConstantInt::get(Type::getInt32Ty(TheContext), 0);
                Builder.CreateStore(llvm_value1, gSigIntValue);

                call_entry_exception_object_with_class_name2(params, "Exception", "Signal Interrupt");

                Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
                Builder.CreateRet(ret_value);

                Builder.SetInsertPoint(else_block);
                current_block = else_block;
                }
                break;

            case OP_MARK_SOURCE_CODE_POSITION: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                char* sname = CONS_str(constant, offset);

                int sline = *(int*)pc;
                pc += sizeof(int);

                std::string info_value_name("info");
                Value* vminfo_value = params[info_value_name];

                StructType* vm_info_struct_type = get_vm_info_struct_type();

                Value* sname_field = Builder.CreateStructGEP(vm_info_struct_type, vminfo_value, 3);
                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)sname);
                Value* sname_value = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                Builder.CreateStore(sname_value, sname_field, "sname_store");

                Value* sline_field = Builder.CreateStructGEP(vm_info_struct_type, vminfo_value, 4);
                Value* sline_value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)sline);
                Builder.CreateStore(sline_value, sline_field, "sline_store");
                }
                break;

            case OP_VALUE_FOR_ANDAND_OROR: {
                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                IRBuilder<> builder(&function->getEntryBlock(), function->getEntryBlock().begin());
                value_for_andand_oror[num_value_for_andand_oror] = builder.CreateAlloca(Type::getInt64Ty(TheContext), 0, "VALUE_FOR_ANDAND_OROR");
                Value* zero = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)0);
                Builder.CreateAlignedStore(zero, value_for_andand_oror[num_value_for_andand_oror], 8);

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(llvm_value, 64);

                Builder.CreateAlignedStore(llvm_value2.value, value_for_andand_oror[num_value_for_andand_oror], 8);

                num_value_for_andand_oror++;

                MASSERT(num_value_for_andand_oror >= ANDAND_OROR_MAX);
                }
                break;

            case OP_STORE_VALUE_FOR_ANDAND_OROR: {
                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                num_value_for_andand_oror--;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(llvm_value, 64);

                Builder.CreateAlignedStore(llvm_value2.value, value_for_andand_oror[num_value_for_andand_oror], 8);

                MASSERT(num_value_for_andand_oror >= 0);
                }
                break;

            case OP_LOAD_VALUE_FOR_ANDAND_OROR: {
                dec_stack_ptr(&llvm_stack_ptr, 1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateLoad(value_for_andand_oror[num_value_for_andand_oror], "value_for_andand_oror");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LABEL: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                char* label_name = CONS_str(constant, offset);

                std::string label_name_string(label_name);

                BasicBlock* label = TheLabels[label_name_string];
                if(label == nullptr) {
                    label = BasicBlock::Create(TheContext, label_name, function);
                    TheLabels[label_name_string] = label;
                }

                Builder.CreateBr(label);
                Builder.SetInsertPoint(label);
                current_block = label;
                }
                break;

            case OP_STORE: {
                int index = *(int*)pc;
                pc += sizeof(int);

                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                store_llvm_value_to_lvar_with_offset(llvm_stack, index, llvm_value);
                }
                break;

            case OP_LOAD: {
                int index = *(int*)pc;
                pc += sizeof(int);

                int size = *(int*)pc;
                pc += sizeof(int);

                LVALUE llvm_value;
                get_llvm_value_from_lvar_with_offset(&llvm_value, llvm_stack, index);

                trunc_variable(&llvm_value, size);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_ADDRESS: {
                int index = *(int*)pc;
                pc += sizeof(int);

                std::string lvar_arg_name("lvar");
                Value* lvar_value = params[lvar_arg_name];

                LVALUE llvm_value;
                Value* add_value = ConstantInt::get(TheContext, llvm::APInt(64, index, true)); 
                llvm_value.value = Builder.CreateGEP(lvar_value, add_value, "gepaddtmp");
                llvm_value.lvar_address_index = index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCBYTE: 
                {
                    int value = *(int*)pc;
                    pc += sizeof(int);

                    LVALUE llvm_value;
                    llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(8, value, true)); 
                    llvm_value.lvar_address_index = -1;
                    llvm_value.lvar_stored = FALSE;
                    llvm_value.constant_int_value = TRUE;
                    llvm_value.constant_float_value = FALSE;

                    push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCUBYTE: 
                {
                    int value = *(int*)pc;
                    pc += sizeof(int);

                    LVALUE llvm_value;
                    llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(8, value, false)); 
                    llvm_value.lvar_address_index = -1;
                    llvm_value.lvar_stored = FALSE;
                    llvm_value.constant_int_value = TRUE;
                    llvm_value.constant_float_value = FALSE;

                    push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCSHORT: 
                {
                    int value = *(int*)pc;
                    pc += sizeof(int);

                    LVALUE llvm_value;
                    llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(16, value, true)); 
                    llvm_value.lvar_address_index = -1;
                    llvm_value.lvar_stored = FALSE;
                    llvm_value.constant_int_value = TRUE;
                    llvm_value.constant_float_value = FALSE;

                    push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCUSHORT: 
                {
                    int value = *(int*)pc;
                    pc += sizeof(int);

                    LVALUE llvm_value;
                    llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(16, value, false)); 
                    llvm_value.lvar_address_index = -1;
                    llvm_value.lvar_stored = FALSE;
                    llvm_value.constant_int_value = TRUE;
                    llvm_value.constant_float_value = FALSE;

                    push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCINT: {
                int value = *(int*)pc;
                pc += sizeof(int);

                LVALUE llvm_value;
                llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(32, value, true)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = TRUE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCUINT: {
                unsigned int value = *(unsigned int*)pc;
                pc += sizeof(int);

                LVALUE llvm_value;
                llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(32, value, false)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = TRUE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCLONG: {
                int value1 = *(int*)pc;
                pc += sizeof(int);

                int value2 = *(int*)pc;
                pc += sizeof(int);

                long lvalue;

                memcpy(&lvalue, &value1, sizeof(int));
                memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

                LVALUE llvm_value;
                llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(64, lvalue, true)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = TRUE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCULONG: {
                int value1 = *(int*)pc;
                pc += sizeof(int);

                int value2 = *(int*)pc;
                pc += sizeof(int);

                long lvalue;

                memcpy(&lvalue, &value1, sizeof(int));
                memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

                LVALUE llvm_value;
                llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(64, lvalue, false)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = TRUE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCNULL: {
                int value = 0;

                LVALUE llvm_value;
                llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(32, value, true)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = TRUE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCPOINTER: {
                int value1 = *(int*)pc;
                pc += sizeof(int);

                int value2 = *(int*)pc;
                pc += sizeof(int);

                long lvalue;

                memcpy(&lvalue, &value1, sizeof(int));
                memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

                LVALUE llvm_value;
                llvm_value.value = ConstantInt::get(TheContext, llvm::APInt(64, lvalue, false)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value.value, PointerType::get(IntegerType::get(TheContext, 64), 0));

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCFLOAT: {
                float value1 = *(float*)pc;
                pc += sizeof(float);

                LVALUE llvm_value;
                llvm_value.value = ConstantFP::get(TheContext, llvm::APFloat(value1)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = TRUE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LDCDOUBLE: {
                int value1 = *(int*)pc;
                pc += sizeof(int);

                int value2 = *(int*)pc;
                pc += sizeof(int);

                double lvalue;

                memcpy(&lvalue, &value1, sizeof(int));
                memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

                LVALUE llvm_value;
                llvm_value.value = ConstantFP::get(TheContext, llvm::APFloat(lvalue)); 
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = TRUE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BADD:
            case OP_SADD:
            case OP_IADD: 
            case OP_LADD: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAdd(lvalue->value, rvalue->value, "addtmp", true, false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBADD: 
            case OP_USADD:
            case OP_UIADD:
            case OP_ULADD: 
            case OP_CADD: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value  = Builder.CreateAdd(lvalue->value, rvalue->value, "addtmp", false, true);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_PADD: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);

                LVALUE rvalue2;
                rvalue2 = trunc_value(rvalue, 64);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateGEP(lvalue->value, rvalue2.value, "addtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;
                break;

            case OP_BSUB:
            case OP_SSUB:
            case OP_ISUB:
            case OP_LSUB: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateSub(lvalue->value, rvalue->value, "subtmp", true, false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_PSUB: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                lvalue->value = Builder.CreateCast(Instruction::PtrToInt, lvalue->value, Type::getInt64Ty(TheContext), "value2");

                LVALUE rvalue2;
                rvalue2 = trunc_value(rvalue, 64);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateSub(lvalue->value, rvalue2.value, "subtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value.value, PointerType::get(IntegerType::get(TheContext, 8), 0));

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_PPSUB: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                lvalue->value = Builder.CreateCast(Instruction::PtrToInt, lvalue->value, Type::getInt64Ty(TheContext), "value2");
                rvalue->value = Builder.CreateCast(Instruction::PtrToInt, rvalue->value, Type::getInt64Ty(TheContext), "value2");

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateSub(lvalue->value, rvalue->value, "subtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 64);

                //llvm_value.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value.value, PointerType::get(IntegerType::get(TheContext, 8), 0));

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value2);
                }
                break;

            case OP_UBSUB: 
            case OP_USSUB:
            case OP_UISUB:
            case OP_ULSUB: 
            case OP_CSUB: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateSub(lvalue->value, rvalue->value, "subtmp", false, true);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BMULT:
            case OP_SMULT:
            case OP_IMULT: 
            case OP_LMULT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateMul(lvalue->value, rvalue->value, "multmp", true, false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBMULT: 
            case OP_USMULT:
            case OP_UIMULT:
            case OP_ULMULT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateMul(lvalue->value, rvalue->value, "multmp", false, true);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BDIV:
            case OP_SDIV:
            case OP_IDIV: 
            case OP_LDIV: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                int value_size;
                if(inst == OP_BDIV) {
                    value_size = 8;
                }
                else if(inst == OP_SDIV) {
                    value_size = 16;
                }
                else if(inst == OP_IDIV) {
                    value_size = 32;
                }
                else {
                    value_size = 64;
                }

                if_value_is_zero_entry_exception_object(rvalue->value, value_size, FALSE, FALSE, params, function, &current_block, "Exception", "division by zero");

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateSDiv(lvalue->value, rvalue->value, "divtmp", false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBDIV: 
            case OP_USDIV:
            case OP_UIDIV:
            case OP_ULDIV: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                int value_size;
                if(inst == OP_UBDIV) {
                    value_size = 8;
                }
                else if(inst == OP_USDIV) {
                    value_size = 16;
                }
                else if(inst == OP_UIDIV) {
                    value_size = 32;
                }
                else {
                    value_size = 64;
                }

                if_value_is_zero_entry_exception_object(rvalue->value, value_size, FALSE, FALSE, params, function, &current_block, "Exception", "division by zero");

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateUDiv(lvalue->value, rvalue->value, "divtmp", false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BMOD: 
            case OP_SMOD:
            case OP_IMOD:
            case OP_LMOD: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                int value_size;
                if(inst == OP_BMOD) {
                    value_size = 8;
                }
                else if(inst == OP_SMOD) {
                    value_size = 16;
                }
                else if(inst == OP_IMOD) {
                    value_size = 32;
                }
                else {
                    value_size = 64;
                }

                if_value_is_zero_entry_exception_object(rvalue->value, value_size, FALSE, FALSE, params, function, &current_block, "Exception", "division by zero");

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateSRem(lvalue->value, rvalue->value, "remtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBMOD:
            case OP_USMOD:
            case OP_UIMOD:
            case OP_ULMOD: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                int value_size;

                if(inst == OP_UBMOD) {
                    value_size = 8;
                }
                else if(inst == OP_USMOD) {
                    value_size = 16;
                }
                else if(inst == OP_UIMOD) {
                    value_size = 32;
                }
                else {
                    value_size = 64;
                }

                if_value_is_zero_entry_exception_object(rvalue->value, value_size, FALSE, FALSE, params, function, &current_block, "Exception", "division by zero");

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateURem(lvalue->value, rvalue->value, "remtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BLSHIFT:
            case OP_SLSHIFT:
            case OP_ILSHIFT: 
            case OP_LLSHIFT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateShl(lvalue->value, rvalue->value, "lshifttmp", true, false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBLSHIFT: 
            case OP_USLSHIFT:
            case OP_UILSHIFT:
            case OP_ULLSHIFT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateShl(lvalue->value, rvalue->value, "lshifttmp", false, true);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BRSHIFT:
            case OP_SRSHIFT:
            case OP_IRSHIFT:
            case OP_LRSHIFT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAShr(lvalue->value, rvalue->value, "rshifttmp", false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBRSHIFT: 
            case OP_USRSHIFT:
            case OP_UIRSHIFT:
            case OP_ULRSHIFT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateLShr(lvalue->value, rvalue->value, "rshifttmp", false);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BAND: 
            case OP_UBAND:
            case OP_SAND:
            case OP_USAND:
            case OP_IAND:
            case OP_UIAND:
            case OP_LAND: 
            case OP_ULAND: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAnd(lvalue->value, rvalue->value, "andtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BXOR: 
            case OP_UBXOR:
            case OP_SXOR:
            case OP_USXOR:
            case OP_IXOR: 
            case OP_UIXOR:
            case OP_LXOR: 
            case OP_ULXOR: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateXor(lvalue->value, rvalue->value, "xortmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BOR: 
            case OP_UBOR:
            case OP_SOR:
            case OP_USOR:
            case OP_IOR:
            case OP_UIOR:
            case OP_LOR: 
            case OP_ULOR: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateOr(lvalue->value, rvalue->value, "ortmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BCOMPLEMENT:
            case OP_UBCOMPLEMENT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                trunc_value_from_inst(lvalue, inst);

                LVALUE rvalue;
                rvalue.value = ConstantInt::get(Type::getInt8Ty(TheContext), (uint8_t)0xFF);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateXor(lvalue->value, rvalue.value, "xortmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_SCOMPLEMENT:
            case OP_USCOMPLEMENT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                trunc_value_from_inst(lvalue, inst);

                LVALUE rvalue;
                rvalue.value = ConstantInt::get(Type::getInt16Ty(TheContext), (uint16_t)0xFFFF);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateXor(lvalue->value, rvalue.value, "xortmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_ICOMPLEMENT:
            case OP_UICOMPLEMENT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                trunc_value_from_inst(lvalue, inst);

                LVALUE rvalue;
                rvalue.value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)0xFFFFFFFF);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateXor(lvalue->value, rvalue.value, "xortmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LCOMPLEMENT:
            case OP_ULCOMPLEMENT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                trunc_value_from_inst(lvalue, inst);

                LVALUE rvalue;
                rvalue.value = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)0xFFFFFFFFFFFFFFFF);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateXor(lvalue->value, rvalue.value, "xortmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FADD:
            case OP_DADD: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFAdd(lvalue->value, rvalue->value, "faddtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FSUB: 
            case OP_DSUB: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFSub(lvalue->value, rvalue->value, "fsubtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FMULT:
            case OP_DMULT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFMul(lvalue->value, rvalue->value, "fmulttmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FDIV:
            case OP_DDIV: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                BOOL value_is_float = inst == OP_FDIV;
                BOOL value_is_double = inst == OP_DDIV;

                if_value_is_zero_entry_exception_object(rvalue->value, 0, value_is_float, value_is_double, params, function, &current_block, "Exception", "division by zero");

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFDiv(lvalue->value, rvalue->value, "fdivtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BEQ:
            case OP_UBEQ:
            case OP_SEQ:
            case OP_USEQ:
            case OP_IEQ:
            case OP_UIEQ:
            case OP_LEQ: 
            case OP_ULEQ: 
            case OP_PEQ: 
            case OP_OBJ_IDENTIFY:
            case OP_CEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpEQ(lvalue->value, rvalue->value, "eqtmpx");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BNOTEQ:
            case OP_UBNOTEQ:
            case OP_SNOTEQ:
            case OP_USNOTEQ:
            case OP_INOTEQ: 
            case OP_UINOTEQ:
            case OP_LNOTEQ: 
            case OP_ULNOTEQ: 
            case OP_PNOTEQ: 
            case OP_CNOTEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpNE(lvalue->value, rvalue->value, "noteqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BGT:
            case OP_SGT:
            case OP_IGT: 
            case OP_LGT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpSGT(lvalue->value, rvalue->value, "gttmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBGT:
            case OP_USGT:
            case OP_UIGT:
            case OP_ULGT: 
            case OP_PGT: 
            case OP_CGT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpUGT(lvalue->value, rvalue->value, "ugttmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BLE:
            case OP_SLE:
            case OP_ILE: 
            case OP_LLE: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpSLT(lvalue->value, rvalue->value, "letmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBLE: 
            case OP_USLE:
            case OP_UILE:
            case OP_ULLE: 
            case OP_PLE:
            case OP_CLE: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpULT(lvalue->value, rvalue->value, "uletmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;
            
            case OP_BGTEQ:
            case OP_SGTEQ:
            case OP_IGTEQ: 
            case OP_LGTEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpSGE(lvalue->value, rvalue->value, "gteqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBGTEQ: 
            case OP_USGTEQ:
            case OP_UIGTEQ:
            case OP_ULGTEQ: 
            case OP_PGTEQ: 
            case OP_CGTEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpUGE(lvalue->value, rvalue->value, "ugeqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BLEEQ:
            case OP_SLEEQ:
            case OP_ILEEQ: 
            case OP_LLEEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpSLE(lvalue->value, rvalue->value, "lteqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBLEEQ:
            case OP_USLEEQ:
            case OP_UILEEQ: 
            case OP_ULLEEQ:
            case OP_PLEEQ: 
            case OP_CLEEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpULE(lvalue->value, rvalue->value, "lteqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FEQ:
            case OP_DEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFCmpOEQ(lvalue->value, rvalue->value, "eqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FNOTEQ:
            case OP_DNOTEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFCmpONE(lvalue->value, rvalue->value, "noteqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FGT:
            case OP_DGT: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFCmpOGT(lvalue->value, rvalue->value, "gttmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLE:
            case OP_DLE: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFCmpOLT(lvalue->value, rvalue->value, "letmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FGTEQ:
            case OP_DGTEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFCmpOGE(lvalue->value, rvalue->value, "gteqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLEEQ:
            case OP_DLEEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateFCmpOLE(lvalue->value, rvalue->value, "leeqtmp");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_REGEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                Function* function = TheModule->getFunction("regex_equals");

                std::vector<Value*> params2;
                params2.push_back(lvalue->value);
                params2.push_back(rvalue->value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(function, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_REGNOTEQ: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                Function* function = TheModule->getFunction("regex_equals");

                std::vector<Value*> params2;
                params2.push_back(lvalue->value);
                params2.push_back(rvalue->value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(function, params2);
                llvm_value.value = Builder.CreateICmpEQ(llvm_value.value, ConstantInt::get(TheContext, llvm::APInt(32, 0, true)), "bool_value_reverse");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CLASSNAME: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                if_value_is_zero_entry_exception_object(value->value, 32, FALSE, FALSE, params, function, &current_block, "Exception", "Null pointer exception(1)");

                Function* fun = TheModule->getFunction("get_string_object_of_object_name");

                std::vector<Value*> params2;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(value, 32);
                params2.push_back(llvm_value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value3;
                llvm_value3 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value3.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_IMPLEMENTS: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                char* class_name = CONS_str(constant, offset);

                /// load class in runtime ///
                Function* load_class_fun = TheModule->getFunction("get_class_with_load_and_initialize_in_jit");

                std::vector<Value*> params2;

                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)class_name);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);

                Value* klass_value = Builder.CreateCall(load_class_fun, params2);

                if_value_is_null_ret_zero(klass_value, 64, params, function, &current_block);

                /// go ///
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);
                if_value_is_zero_entry_exception_object(value->value, 32, FALSE, FALSE, params, function, &current_block, "Exception", "Null pointer exception(2)");

                Function* fun = TheModule->getFunction("object_implements_interface");

                params2.clear();

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(value, 32);

                params2.push_back(llvm_value2.value);

                Value* param2 = klass_value;
                params2.push_back(param2);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_ANDAND: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAnd(lvalue->value, rvalue->value, "ANDAND");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_OROR: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* rvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);
                trunc_value_from_inst(rvalue, inst);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateOr(lvalue->value, rvalue->value, "OROR");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 2);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOGICAL_DENIAL: {
                LVALUE* lvalue = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                trunc_value_from_inst(lvalue, inst);

                LVALUE rvalue;
                rvalue.value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)0);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateICmpEQ(lvalue->value, rvalue.value, "LOGICAL_DIANEAL");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_INVOKE_METHOD: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int method_index = *(int*)pc;
                pc += sizeof(int);

                char* class_name = CONS_str(constant, offset);

                sCLClass* klass = get_class_with_load(class_name);

                if(klass == NULL) {
                    fprintf(stderr, "class not found(2) (%s)\n", class_name);
                    return FALSE;
                }

                if(method_index < 0 || method_index >= klass->mNumMethods) {
                    fprintf(stderr, "OP_INVOKE_METHOD: Method not found");
                    return FALSE;
                }

                sCLMethod* method = klass->mMethods + method_index;

                /// load class in runtime ///
                Function* load_class_fun = TheModule->getFunction("get_class_with_load_and_initialize_in_jit");

                std::vector<Value*> params2;

                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)class_name);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);

                Value* klass_value = Builder.CreateCall(load_class_fun, params2);

                if_value_is_null_ret_zero(klass_value, 64, params, function, &current_block);

                /// llvm stack to VM stack ///
                llvm_lvar_to_vm_lvar(llvm_stack, params, current_block, var_num);

                int real_param_num = method->mNumParams + (method->mFlags & METHOD_FLAGS_CLASS_METHOD ? 0:1);
                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, real_param_num);

                /// go ///
                Function* fun = TheModule->getFunction("call_invoke_method");

                params2.clear();

                param1 = klass_value;
                params2.push_back(param1);

                Value* param2 = ConstantInt::get(Type::getInt32Ty(TheContext), APInt(32, method_index, true));
                params2.push_back(param2);

                std::string stack_value_name("stack");
                Value* param3 = params[stack_value_name];
                params2.push_back(param3);

                std::string var_num_value_name("var_num");
                Value* param4 = params[var_num_value_name];
                params2.push_back(param4);

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param5 = params[stack_ptr_address_name];
                params2.push_back(param5);

                std::string info_value_name("info");
                Value* param6 = params[info_value_name];
                params2.push_back(param6);

                Value* result = Builder.CreateCall(fun, params2);

                finish_method_call(result, params, &current_block, function, &try_catch_label_name, code, real_param_num, var_num, llvm_stack, llvm_stack_ptr);
                

                /// the pointer of lvar syncs to llvm stack ///
                for(i=0; i<real_param_num; i++) {
                    LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -i-1);
                    if(llvm_value->lvar_address_index != -1) {
                        LVALUE llvm_value_of_vm_stack = get_stack_value_from_index_with_aligned(params, current_block, llvm_value->lvar_address_index, 8);
                        store_llvm_value_to_lvar_with_offset(llvm_stack, llvm_value->lvar_address_index, &llvm_value_of_vm_stack);
                        llvm_value->lvar_address_index = -1;
                    }
                }

                /// VM stack to llvm stack ///
                vm_lvar_to_llvm_lvar(llvm_stack, params, current_block, var_num);

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, real_param_num);

                /// vm stack_ptr to llvm stack ///
                LVALUE llvm_value = get_vm_stack_ptr_value_from_index_with_aligned(params, current_block, -1, 8);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);

                /// delete result from vm stack ///
                dec_vm_stack_ptr(params, current_block, 1);
                }
                break;

            case OP_INVOKE_VIRTUAL_METHOD: {
                int num_real_params = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                /// llvm stack to VM stack ///
                llvm_lvar_to_vm_lvar(llvm_stack, params, current_block, var_num);

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_real_params);

                /// get object value from llvm stack ///
                LVALUE* object_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -num_real_params);

                /// go ///
                Function* fun = TheModule->getFunction("call_invoke_virtual_method");

                std::vector<Value*> params2;

                Value* param1 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)offset);
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param4 = params[stack_ptr_address_name];
                params2.push_back(param4);

                std::string info_value_name("info");
                Value* param5 = params[info_value_name];
                params2.push_back(param5);

                std::string constant_value_name("constant");
                Value* param6 = params[constant_value_name];
                params2.push_back(param6);

                LVALUE object_value2;
                object_value2 = trunc_value(object_value, 32);

                Value* param7 = object_value2.value;
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                finish_method_call(result, params, &current_block, function, &try_catch_label_name, code, num_real_params-1, var_num, llvm_stack, llvm_stack_ptr);
                
                /// the pointer of lvar syncs to llvm stack ///
                int i;
                for(i=0; i<num_real_params-1; i++) {
                    LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -i-1);
                    if(llvm_value->lvar_address_index != -1) {
                        LVALUE llvm_value_of_vm_stack = get_stack_value_from_index_with_aligned(params, current_block, llvm_value->lvar_address_index, 8);
                        store_llvm_value_to_lvar_with_offset(llvm_stack, llvm_value->lvar_address_index, &llvm_value_of_vm_stack);
                    }
                }

                /// VM stack to llvm stack ///
                vm_lvar_to_llvm_lvar(llvm_stack, params, current_block, var_num);

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_real_params);

                /// vm stack_ptr to llvm stack ///
                LVALUE llvm_value = get_vm_stack_ptr_value_from_index_with_aligned(params, current_block, -1, 8);
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);

                /// delete result from vm stack ///
                dec_vm_stack_ptr(params, current_block, 1);
                }
                break;

            case OP_INVOKE_DYNAMIC_METHOD: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int offset2 = *(int*)pc;
                pc += sizeof(int);

                int num_params = *(int*)pc;
                pc += sizeof(int);

                BOOL static_ = *(int*)pc;
                pc += sizeof(int);

                int num_method_chains = *(int*)pc;
                pc += sizeof(int);

                int max_method_chains = *(int*)pc;
                pc += sizeof(int);

                int num_real_params = num_params + (static_ ? 0:1);

                /// llvm stack to VM stack ///
                llvm_lvar_to_vm_lvar(llvm_stack, params, current_block, var_num);

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_real_params);

                /// go ///
                Function* fun = TheModule->getFunction("call_invoke_dynamic_method");

                std::vector<Value*> params2;

                Value* param1 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)offset);
                params2.push_back(param1);

                Value* param2 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)offset2);
                params2.push_back(param2);

                Value* param3 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_params);
                params2.push_back(param3);

                Value* param4 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)static_);
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_method_chains);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)max_method_chains);
                params2.push_back(param6);

                std::string stack_value_name("stack");
                Value* param7 = params[stack_value_name];
                params2.push_back(param7);

                std::string var_num_value_name("var_num");
                Value* param8 = params[var_num_value_name];
                params2.push_back(param8);

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param9 = params[stack_ptr_address_name];
                params2.push_back(param9);

                std::string info_value_name("info");
                Value* param10 = params[info_value_name];
                params2.push_back(param10);

                std::string code_value_name("code");
                Value* param11 = params[code_value_name];
                params2.push_back(param11);

                std::string constant_value_name("constant");
                Value* param12 = params[constant_value_name];
                params2.push_back(param12);

                Value* result = Builder.CreateCall(fun, params2);

                finish_method_call(result, params, &current_block, function, &try_catch_label_name, code, num_real_params-1, var_num, llvm_stack, llvm_stack_ptr);
                
                /// the pointer of lvar syncs to llvm stack ///
                int i;
                for(i=0; i<num_real_params-1; i++) {
                    LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -i-1);
                    if(llvm_value->lvar_address_index != -1) {
                        LVALUE llvm_value_of_vm_stack = get_stack_value_from_index_with_aligned(params, current_block, llvm_value->lvar_address_index, 8);
                        store_llvm_value_to_lvar_with_offset(llvm_stack, llvm_value->lvar_address_index, &llvm_value_of_vm_stack);
                    }
                }

                /// VM stack to llvm stack ///
                vm_lvar_to_llvm_lvar(llvm_stack, params, current_block, var_num);

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_real_params);

                /// vm stack_ptr to llvm stack ///
                LVALUE llvm_value = get_vm_stack_ptr_value_from_index_with_aligned(params, current_block, -1, 8);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);

                /// delete result from vm stack ///
                dec_vm_stack_ptr(params, current_block, 1);
                }
                break;

            case OP_INVOKE_BLOCK: {
                int num_params = *(int*)pc;
                pc += sizeof(int);

                /// llvm stack to VM stack ///
                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_params + 1);

                /// go ///
                Function* fun = TheModule->getFunction("invoke_block_in_jit");

                std::vector<Value*> params2;

                Value* param1 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_params);
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param4 = params[stack_ptr_address_name];
                params2.push_back(param4);

                std::string info_value_name("info");
                Value* param5 = params[info_value_name];
                params2.push_back(param5);

                Value* result = Builder.CreateCall(fun, params2);

                if_value_is_zero_ret_zero(result, params, function, &current_block);

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_params+1);

                /// vm stack_ptr to llvm stack ///
                LVALUE llvm_value = get_vm_stack_ptr_value_from_index_with_aligned(params, current_block, -1, 8);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);

                /// delete result from vm stack ///
                dec_vm_stack_ptr(params, current_block, 1);
                }
                break;

            case OP_NEW: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int flg_array = *(int*)pc;
                pc += sizeof(int);

                char* class_name = CONS_str(constant, offset);

                /// load class in runtime ///
                Function* load_class_fun = TheModule->getFunction("get_class_with_load_and_initialize_in_jit");

                std::vector<Value*> params2;

                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)class_name);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);

                Value* klass_value = Builder.CreateCall(load_class_fun, params2);

                if_value_is_null_ret_zero(klass_value, 64, params, function, &current_block);

                /// go ///
                if(flg_array) {
                    LVALUE* array_num_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                    Function* function = TheModule->getFunction("create_array_object");

                    std::vector<Value*> params2;
                    params2.push_back(klass_value);

                    LVALUE array_num_value2 = trunc_value(array_num_value, 32);

                    params2.push_back(array_num_value2.value);

                    Value* value = Builder.CreateCall(function, params2);

                    LVALUE llvm_value;
                    llvm_value.value = value;
                    llvm_value.lvar_address_index = -1;
                    llvm_value.lvar_stored = FALSE;
                    llvm_value.constant_int_value = FALSE;
                    llvm_value.constant_float_value = FALSE;

                    dec_stack_ptr(&llvm_stack_ptr, 1);
                    push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                    /// push object to jit objects ///
                    Function* fun2 = TheModule->getFunction("push_jit_object");

                    std::vector<Value*> params3;

                    LVALUE llvm_value2;
                    llvm_value2 = trunc_value(&llvm_value, 32);

                    Value* param1 = llvm_value2.value;
                    params3.push_back(param1);

                    (void)Builder.CreateCall(fun2, params3);
                }
                else {
                    Function* function = TheModule->getFunction("create_object");

                    std::vector<Value*> params2;
                    params2.push_back(klass_value);

                    Value* value = Builder.CreateCall(function, params2);

                    LVALUE llvm_value;
                    llvm_value.value = value;
                    llvm_value.lvar_address_index = -1;
                    llvm_value.lvar_stored = FALSE;
                    llvm_value.constant_int_value = FALSE;
                    llvm_value.constant_float_value = FALSE;

//value->dump();
//sleep(3);

                    push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                    /// push object to jit objects ///
                    Function* fun2 = TheModule->getFunction("push_jit_object");

                    std::vector<Value*> params3;

                    LVALUE llvm_value2;
                    llvm_value2 = trunc_value(&llvm_value, 32);

                    Value* param1 = llvm_value2.value;
                    params3.push_back(param1);

                    (void)Builder.CreateCall(fun2, params3);
                }
                }
                break;

            case OP_LOAD_FIELD: {
                int field_index = *(int*)pc;
                pc += sizeof(int);

                int size = *(int*)pc;
                pc += sizeof(int);

                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* get_field_fun = TheModule->getFunction("get_field_from_object");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                LVALUE value2 = trunc_value(value, 32);

                Value* param5; // CLObject
                param5 = Builder.CreateCast(Instruction::Trunc, value2.value, Type::getInt32Ty(TheContext));
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)field_index);
                params2.push_back(param6);

                Value* result = Builder.CreateCall(get_field_fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);
                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                trunc_variable(&llvm_value, size);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_FIELD_ADDRESS: {
                int field_index = *(int*)pc;
                pc += sizeof(int);

                LVALUE* obj_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_load_field_address");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)field_index);
                params2.push_back(param5);

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(obj_value, 32);

                Value* param6 = llvm_value2.value;
                params2.push_back(param6);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gPointerAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1, 8);
                Value* result2 = Builder.CreateStructGEP(gPointerAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2, 4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_STORE_FIELD: {
                int field_index = *(int*)pc;
                pc += sizeof(int);

                LVALUE* obj = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("store_field");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                LVALUE obj2 = trunc_value(obj, 32);

                Value* param5 = obj2.value;
                params2.push_back(param5);

                LVALUE value2;
                value2 = trunc_value(value, 64);

                Value* param6 = value2.value;
                params2.push_back(param6);

                Value* param7 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)field_index);
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                if_value_is_zero_ret_zero(result, params, function, &current_block);
                }
                break;

            case OP_LOAD_CLASS_FIELD: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int field_index = *(int*)pc;
                pc += sizeof(int);

                int size = *(int*)pc;
                pc += sizeof(int);

                Function* fun = TheModule->getFunction("load_class_field");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)field_index);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                trunc_variable(&llvm_value, size);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_CLASS_FIELD_ADDRESS: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int field_index = *(int*)pc;
                pc += sizeof(int);

                Function* fun = TheModule->getFunction("run_load_class_field_address");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)field_index);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gPointerAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1, 8);
                Value* result2 = Builder.CreateStructGEP(gPointerAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2, 4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_STORE_CLASS_FIELD: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int field_index = *(int*)pc;
                pc += sizeof(int);

                LVALUE* llvm_value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("store_class_field");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)field_index);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                LVALUE value2;
                value2 = trunc_value(llvm_value, 64);

                Value* param8 = value2.value;
                params2.push_back(param8);

                Value* result = Builder.CreateCall(fun, params2);

                if_value_is_zero_ret_zero(result, params, function, &current_block);
                }
                break;

            case OP_STORE_VALUE_TO_INT_ADDRESS: 
            case OP_STORE_VALUE_TO_UINT_ADDRESS:
            case OP_STORE_VALUE_TO_CHAR_ADDRESS:
            case OP_STORE_VALUE_TO_BOOL_ADDRESS:
            case OP_STORE_VALUE_TO_OBJECT_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 32), 0));

                LVALUE value2 = trunc_value(value, 32);

                Builder.CreateAlignedStore(value2.value, address->value, 4);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }
                break;

            case OP_STORE_VALUE_TO_FLOAT_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(Type::getFloatTy(TheContext), 0));

                LVALUE value2 = trunc_value_to_float_or_double(value, 32);

                Builder.CreateAlignedStore(value2.value, address->value, 4);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }

            case OP_STORE_VALUE_TO_BYTE_ADDRESS:
            case OP_STORE_VALUE_TO_UBYTE_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 8), 0));

                LVALUE value2 = trunc_value_to_float_or_double(value, 8);

                Builder.CreateAlignedStore(value2.value, address->value, 1);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }
                break;

            case OP_STORE_VALUE_TO_SHORT_ADDRESS:
            case OP_STORE_VALUE_TO_USHORT_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE value2 = trunc_value_to_float_or_double(value, 16);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 16), 0));

                Builder.CreateAlignedStore(value2.value, address->value, 2);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }
                break;

            case OP_LOAD_VALUE_FROM_INT_ADDRESS: 
            case OP_LOAD_VALUE_FROM_UINT_ADDRESS: 
            case OP_LOAD_VALUE_FROM_CHAR_ADDRESS: 
            case OP_LOAD_VALUE_FROM_BOOL_ADDRESS: 
            case OP_LOAD_VALUE_FROM_OBJECT_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 32), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 4, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;
                
                llvm_value.value = Builder.CreateCast(Instruction::Trunc, llvm_value.value, Type::getInt32Ty(TheContext));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_VALUE_FROM_FLOAT_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(Type::getFloatTy(TheContext), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 4, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;
                
                llvm_value.value = Builder.CreateCast(Instruction::BitCast, llvm_value.value, Type::getFloatTy(TheContext));
                //llvm_value.value = Builder.CreateCast(Instruction::Trunc, llvm_value.value, Type::getFloatTy(TheContext));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_VALUE_FROM_BYTE_ADDRESS: 
            case OP_LOAD_VALUE_FROM_UBYTE_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 8), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 1, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::Trunc, llvm_value.value, Type::getInt8Ty(TheContext));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_VALUE_FROM_SHORT_ADDRESS:
            case OP_LOAD_VALUE_FROM_USHORT_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 16), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 2, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::Trunc, llvm_value.value, Type::getInt16Ty(TheContext));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_VALUE_FROM_LONG_ADDRESS:
            case OP_LOAD_VALUE_FROM_ULONG_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 64), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 8, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::Trunc, llvm_value.value, Type::getInt64Ty(TheContext));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_VALUE_FROM_POINTER_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 8), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 8, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value.value, PointerType::get(IntegerType::get(TheContext, 8), 0));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_VALUE_FROM_DOUBLE_ADDRESS: {
                /// lvar of llvm stack to lvar of vm stack ///
                lvar_of_llvm_to_lvar_of_vm(params, current_block, llvm_stack, var_num);

                /// go ///
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(Type::getDoubleTy(TheContext), 0));

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateAlignedLoad(address->value, 8, "llvm_value");
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::BitCast, llvm_value.value, Type::getDoubleTy(TheContext));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_LOAD_ELEMENT: {
                int size = *(int*)pc;
                pc += sizeof(int);

                LVALUE* array = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* element_num = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("load_element");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                LVALUE array2 = trunc_value(array, 32);

                Value* param5 = array2.value;
                params2.push_back(param5);

                LVALUE element_num2 = trunc_value(element_num, 32);

                Value* param6 = element_num2.value;
                params2.push_back(param6);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                trunc_variable(&llvm_value, size);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_STORE_ELEMENT: {
                LVALUE* array = get_stack_ptr_value_from_index(llvm_stack_ptr, -3);
                LVALUE* element_num = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_store_element");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                LVALUE array2 = trunc_value(array, 32);
                Value* param5 = array2.value;
                params2.push_back(param5);

                LVALUE element_num2 = trunc_value(element_num, 32);
                Value* param6 = element_num2.value;
                params2.push_back(param6);

                LVALUE value2 = trunc_value(value, 64);
                Value* param7 = value2.value;
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                if_value_is_zero_ret_zero(result, params, function, &current_block);

                LVALUE llvm_value = *value;

                dec_stack_ptr(&llvm_stack_ptr, 3);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_STORE_VALUE_TO_LONG_ADDRESS:
            case OP_STORE_VALUE_TO_ULONG_ADDRESS: {
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE value2 = trunc_value(value, 64);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(IntegerType::get(TheContext, 64), 0));

                Builder.CreateAlignedStore(value2.value, address->value, 8);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }
                break;

            case OP_STORE_VALUE_TO_DOUBLE_ADDRESS: {
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(Type::getDoubleTy(TheContext), 0));

                LVALUE value2 = trunc_value_to_float_or_double(value, 64);

                Builder.CreateAlignedStore(value2.value, address->value, 8);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }
                break;

            case OP_STORE_VALUE_TO_POINTER_ADDRESS: {
                LVALUE* address = get_stack_ptr_value_from_index(llvm_stack_ptr, -2);
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                address->value = Builder.CreateCast(Instruction::BitCast, address->value, PointerType::get(PointerType::get(IntegerType::get(TheContext, 8), 0), 0));

                LVALUE value2 = trunc_value(value, 8);

                Builder.CreateAlignedStore(value2.value, address->value, 8);

                dec_stack_ptr(&llvm_stack_ptr, 2);

                push_value_to_stack_ptr(&llvm_stack_ptr, &value2);

                /// lvar of vm stack to lvar of llvm stack ///
                lvar_of_vm_to_lvar_of_llvm(params, current_block, llvm_stack, var_num);
                }
                break;

            case OP_BYTE_TO_INT_CAST:
            case OP_SHORT_TO_INT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::SExt, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBYTE_TO_INT_CAST:
            case OP_USHORT_TO_INT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::ZExt, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BYTE_TO_UINT_CAST:
            case OP_UBYTE_TO_UINT_CAST:
            case OP_SHORT_TO_UINT_CAST:
            case OP_USHORT_TO_UINT_CAST: 
            case OP_BYTE_TO_CHAR_CAST:
            case OP_UBYTE_TO_CHAR_CAST:
            case OP_SHORT_TO_CHAR_CAST:
            case OP_USHORT_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::ZExt, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_INT_TO_CHAR_CAST:
            case OP_UINT_TO_CHAR_CAST:
            case OP_INT_TO_UINT_CAST :
            case OP_UINT_TO_INT_CAST:
            case OP_CHAR_TO_UINT_CAST:
            case OP_CHAR_TO_INT_CAST:
                break;

            case OP_LONG_TO_INT_CAST:
            case OP_ULONG_TO_INT_CAST:
            case OP_LONG_TO_UINT_CAST:
            case OP_ULONG_TO_UINT_CAST:
            case OP_LONG_TO_CHAR_CAST:
            case OP_ULONG_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::Trunc, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_INT_CAST:
            case OP_DOUBLE_TO_INT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToSI, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_UINT_CAST:
            case OP_DOUBLE_TO_UINT_CAST: 
            case OP_FLOAT_TO_CHAR_CAST:
            case OP_DOUBLE_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToUI, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_POINTER_TO_INT_CAST:
            case OP_POINTER_TO_UINT_CAST:
            case OP_POINTER_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::PtrToInt, value->value, Type::getInt32Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BYTE_TO_UBYTE_CAST:
            case OP_UBYTE_TO_BYTE_CAST:
                break;

            case OP_SHORT_TO_BYTE_CAST:
            case OP_USHORT_TO_BYTE_CAST:
            case OP_INT_TO_BYTE_CAST:
            case OP_UINT_TO_BYTE_CAST:
            case OP_LONG_TO_BYTE_CAST:
            case OP_ULONG_TO_BYTE_CAST:
            case OP_CHAR_TO_BYTE_CAST: 
            case OP_SHORT_TO_UBYTE_CAST:
            case OP_USHORT_TO_UBYTE_CAST:
            case OP_INT_TO_UBYTE_CAST:
            case OP_UINT_TO_UBYTE_CAST:
            case OP_LONG_TO_UBYTE_CAST:
            case OP_ULONG_TO_UBYTE_CAST:
            case OP_CHAR_TO_UBYTE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::Trunc, value->value, Type::getInt8Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_BYTE_CAST:
            case OP_DOUBLE_TO_BYTE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToSI, value->value, Type::getInt8Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_UBYTE_CAST:
            case OP_DOUBLE_TO_UBYTE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToUI, value->value, Type::getInt8Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_POINTER_TO_BYTE_CAST: 
            case OP_POINTER_TO_UBYTE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::PtrToInt, value->value, Type::getInt8Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_INT_TO_SHORT_CAST:
            case OP_UINT_TO_SHORT_CAST:
            case OP_LONG_TO_SHORT_CAST:
            case OP_ULONG_TO_SHORT_CAST:
            case OP_CHAR_TO_SHORT_CAST: 
            case OP_INT_TO_USHORT_CAST:
            case OP_UINT_TO_USHORT_CAST:
            case OP_LONG_TO_USHORT_CAST:
            case OP_ULONG_TO_USHORT_CAST:
            case OP_CHAR_TO_USHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::Trunc, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_SHORT_CAST:
            case OP_DOUBLE_TO_SHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToSI, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_USHORT_CAST:
            case OP_DOUBLE_TO_USHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToUI, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BYTE_TO_SHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::SExt, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;
                
            case OP_UBYTE_TO_SHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::ZExt, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;
               
            case OP_BYTE_TO_USHORT_CAST:
            case OP_UBYTE_TO_USHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::ZExt, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_SHORT_TO_USHORT_CAST:
            case OP_USHORT_TO_SHORT_CAST:
                break;

            case OP_POINTER_TO_SHORT_CAST:
            case OP_POINTER_TO_USHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::PtrToInt, value->value, Type::getInt16Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BYTE_TO_LONG_CAST:
            case OP_SHORT_TO_LONG_CAST:
            case OP_INT_TO_LONG_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::SExt, value->value, Type::getInt64Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBYTE_TO_LONG_CAST:
            case OP_USHORT_TO_LONG_CAST:
            case OP_UINT_TO_LONG_CAST:
            case OP_CHAR_TO_LONG_CAST:
            case OP_BYTE_TO_ULONG_CAST:
            case OP_UBYTE_TO_ULONG_CAST:
            case OP_SHORT_TO_ULONG_CAST:
            case OP_USHORT_TO_ULONG_CAST:
            case OP_INT_TO_ULONG_CAST:
            case OP_UINT_TO_ULONG_CAST:
            case OP_CHAR_TO_ULONG_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::ZExt, value->value, Type::getInt64Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_ULONG_TO_LONG_CAST:
                break;

            case OP_LONG_TO_ULONG_CAST:
                break;

            case OP_FLOAT_TO_LONG_CAST:
            case OP_DOUBLE_TO_LONG_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToSI, value->value, Type::getInt64Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_ULONG_CAST:
            case OP_DOUBLE_TO_ULONG_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPToUI, value->value, Type::getInt64Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_POINTER_TO_LONG_CAST: 
            case OP_POINTER_TO_ULONG_CAST:  {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::PtrToInt, value->value, Type::getInt64Ty(TheContext), "value2");
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BYTE_TO_POINTER_CAST :
            case OP_UBYTE_TO_POINTER_CAST:
            case OP_SHORT_TO_POINTER_CAST:
            case OP_USHORT_TO_POINTER_CAST:
            case OP_INT_TO_POINTER_CAST:
            case OP_UINT_TO_POINTER_CAST:
            case OP_LONG_TO_POINTER_CAST:
            case OP_ULONG_TO_POINTER_CAST:
            case OP_CHAR_TO_POINTER_CAST: {
                }
                break;

            case OP_BYTE_TO_FLOAT_CAST:
            case OP_SHORT_TO_FLOAT_CAST:
            case OP_INT_TO_FLOAT_CAST:
            case OP_LONG_TO_FLOAT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::SIToFP, value->value, Type::getFloatTy(TheContext));
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBYTE_TO_FLOAT_CAST:
            case OP_USHORT_TO_FLOAT_CAST:
            case OP_UINT_TO_FLOAT_CAST:
            case OP_ULONG_TO_FLOAT_CAST:
            case OP_CHAR_TO_FLOAT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::UIToFP, value->value, Type::getFloatTy(TheContext));
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_DOUBLE_TO_FLOAT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPTrunc, value->value, Type::getFloatTy(TheContext));
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_BYTE_TO_DOUBLE_CAST:
            case OP_SHORT_TO_DOUBLE_CAST:
            case OP_INT_TO_DOUBLE_CAST:
            case OP_LONG_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::SIToFP, value->value, Type::getDoubleTy(TheContext));
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UBYTE_TO_DOUBLE_CAST:
            case OP_USHORT_TO_DOUBLE_CAST:
            case OP_UINT_TO_DOUBLE_CAST:
            case OP_ULONG_TO_DOUBLE_CAST:
            case OP_CHAR_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::UIToFP, value->value, Type::getDoubleTy(TheContext));
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_FLOAT_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCast(Instruction::FPExt, value->value, Type::getDoubleTy(TheContext));
                llvm_value.lvar_address_index = value->lvar_address_index;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;




            case OP_BYTE_TO_STRING_CAST: 
            case OP_SHORT_TO_STRING_CAST :
            case OP_INT_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_int_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_LONG_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_long_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 64);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_UBYTE_TO_STRING_CAST :
            case OP_USHORT_TO_STRING_CAST :
            case OP_UINT_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_uint_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_ULONG_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_ulong_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 64);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_FLOAT_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_float_to_string_cast");

                std::vector<Value*> params2;

                params2.push_back(value->value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_DOUBLE_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_double_to_string_cast");

                std::vector<Value*> params2;

                params2.push_back(value->value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CHAR_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_char_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);
                
                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_REGEX_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_regex_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BOOL_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_bool_to_string_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                params2.push_back(value2.value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_POINTER_TO_STRING_CAST : {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_pointer_to_string_cast");

                std::vector<Value*> params2;

                params2.push_back(value->value);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                Value* param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;


            case OP_BYTE_TO_INTEGER_CAST:
            case OP_UBYTE_TO_INTEGER_CAST:
            case OP_SHORT_TO_INTEGER_CAST:
            case OP_USHORT_TO_INTEGER_CAST:
            case OP_INT_TO_INTEGER_CAST:
            case OP_UINT_TO_INTEGER_CAST:
            case OP_LONG_TO_INTEGER_CAST:
            case OP_ULONG_TO_INTEGER_CAST:
            case OP_CHAR_TO_INTEGER_CAST:
            case OP_POINTER_TO_INTEGER_CAST:
            case OP_BOOL_TO_INTEGER_CAST:
            case OP_FLOAT_TO_INTEGER_CAST:
            case OP_DOUBLE_TO_INTEGER_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_integer");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);
                (void)Builder.CreateCall(fun2, params3);
                }
                break;


            case OP_BYTE_TO_UINTEGER_CAST:
            case OP_UBYTE_TO_UINTEGER_CAST:
            case OP_SHORT_TO_UINTEGER_CAST:
            case OP_USHORT_TO_UINTEGER_CAST:
            case OP_INT_TO_UINTEGER_CAST:
            case OP_UINT_TO_UINTEGER_CAST:
            case OP_LONG_TO_UINTEGER_CAST:
            case OP_ULONG_TO_UINTEGER_CAST:
            case OP_CHAR_TO_UINTEGER_CAST:
            case OP_POINTER_TO_UINTEGER_CAST:
            case OP_BOOL_TO_UINTEGER_CAST:
            case OP_FLOAT_TO_UINTEGER_CAST:
            case OP_DOUBLE_TO_UINTEGER_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_uinteger");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CBYTE_CAST:
            case OP_UBYTE_TO_CBYTE_CAST:
            case OP_SHORT_TO_CBYTE_CAST:
            case OP_USHORT_TO_CBYTE_CAST:
            case OP_INT_TO_CBYTE_CAST:
            case OP_UINT_TO_CBYTE_CAST:
            case OP_LONG_TO_CBYTE_CAST:
            case OP_ULONG_TO_CBYTE_CAST:
            case OP_CHAR_TO_CBYTE_CAST:
            case OP_POINTER_TO_CBYTE_CAST:
            case OP_BOOL_TO_CBYTE_CAST: 
            case OP_FLOAT_TO_CBYTE_CAST:
            case OP_DOUBLE_TO_CBYTE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_byte");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;


            case OP_BYTE_TO_CUBYTE_CAST:
            case OP_UBYTE_TO_CUBYTE_CAST:
            case OP_SHORT_TO_CUBYTE_CAST:
            case OP_USHORT_TO_CUBYTE_CAST:
            case OP_INT_TO_CUBYTE_CAST:
            case OP_UINT_TO_CUBYTE_CAST:
            case OP_LONG_TO_CUBYTE_CAST:
            case OP_ULONG_TO_CUBYTE_CAST:
            case OP_CHAR_TO_CUBYTE_CAST:
            case OP_POINTER_TO_CUBYTE_CAST:
            case OP_BOOL_TO_CUBYTE_CAST:
            case OP_FLOAT_TO_CUBYTE_CAST:
            case OP_DOUBLE_TO_CUBYTE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_ubyte");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");


                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;


            case OP_BYTE_TO_CSHORT_CAST:
            case OP_UBYTE_TO_CSHORT_CAST:
            case OP_SHORT_TO_CSHORT_CAST:
            case OP_USHORT_TO_CSHORT_CAST:
            case OP_INT_TO_CSHORT_CAST:
            case OP_UINT_TO_CSHORT_CAST:
            case OP_LONG_TO_CSHORT_CAST:
            case OP_ULONG_TO_CSHORT_CAST:
            case OP_CHAR_TO_CSHORT_CAST:
            case OP_POINTER_TO_CSHORT_CAST:
            case OP_BOOL_TO_CSHORT_CAST: 
            case OP_FLOAT_TO_CSHORT_CAST:
            case OP_DOUBLE_TO_CSHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_short");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CUSHORT_CAST:
            case OP_UBYTE_TO_CUSHORT_CAST:
            case OP_SHORT_TO_CUSHORT_CAST:
            case OP_USHORT_TO_CUSHORT_CAST:
            case OP_INT_TO_CUSHORT_CAST:
            case OP_UINT_TO_CUSHORT_CAST:
            case OP_LONG_TO_CUSHORT_CAST:
            case OP_ULONG_TO_CUSHORT_CAST:
            case OP_CHAR_TO_CUSHORT_CAST:
            case OP_POINTER_TO_CUSHORT_CAST:
            case OP_BOOL_TO_CUSHORT_CAST:
            case OP_FLOAT_TO_CUSHORT_CAST:
            case OP_DOUBLE_TO_CUSHORT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_ushort");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CLONG_CAST:
            case OP_UBYTE_TO_CLONG_CAST:
            case OP_SHORT_TO_CLONG_CAST:
            case OP_USHORT_TO_CLONG_CAST:
            case OP_INT_TO_CLONG_CAST:
            case OP_UINT_TO_CLONG_CAST:
            case OP_LONG_TO_CLONG_CAST:
            case OP_ULONG_TO_CLONG_CAST:
            case OP_CHAR_TO_CLONG_CAST:
            case OP_POINTER_TO_CLONG_CAST:
            case OP_BOOL_TO_CLONG_CAST: 
            case OP_FLOAT_TO_CLONG_CAST:
            case OP_DOUBLE_TO_CLONG_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_long");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CULONG_CAST:
            case OP_UBYTE_TO_CULONG_CAST:
            case OP_SHORT_TO_CULONG_CAST:
            case OP_USHORT_TO_CULONG_CAST:
            case OP_INT_TO_CULONG_CAST:
            case OP_UINT_TO_CULONG_CAST:
            case OP_LONG_TO_CULONG_CAST:
            case OP_ULONG_TO_CULONG_CAST:
            case OP_CHAR_TO_CULONG_CAST:
            case OP_POINTER_TO_CULONG_CAST:
            case OP_BOOL_TO_CULONG_CAST:
            case OP_FLOAT_TO_CULONG_CAST:
            case OP_DOUBLE_TO_CULONG_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_ulong");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CFLOAT_CAST:
            case OP_SHORT_TO_CFLOAT_CAST:
            case OP_INT_TO_CFLOAT_CAST:
            case OP_LONG_TO_CFLOAT_CAST:
            case OP_BOOL_TO_CFLOAT_CAST: 
            case OP_UBYTE_TO_CFLOAT_CAST:
            case OP_USHORT_TO_CFLOAT_CAST:
            case OP_UINT_TO_CFLOAT_CAST:
            case OP_CHAR_TO_CFLOAT_CAST:
            case OP_ULONG_TO_CFLOAT_CAST:
            case OP_FLOAT_TO_CFLOAT_CAST: 
            case OP_DOUBLE_TO_CFLOAT_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_float");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CDOUBLE_CAST:
            case OP_SHORT_TO_CDOUBLE_CAST:
            case OP_INT_TO_CDOUBLE_CAST:
            case OP_LONG_TO_CDOUBLE_CAST:
            case OP_BOOL_TO_CDOUBLE_CAST: 
            case OP_UBYTE_TO_CDOUBLE_CAST:
            case OP_USHORT_TO_CDOUBLE_CAST:
            case OP_UINT_TO_CDOUBLE_CAST:
            case OP_CHAR_TO_CDOUBLE_CAST:
            case OP_ULONG_TO_CDOUBLE_CAST:
            case OP_FLOAT_TO_CDOUBLE_CAST:
            case OP_DOUBLE_TO_CDOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_double");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CPOINTER_CAST:
            case OP_UBYTE_TO_CPOINTER_CAST:
            case OP_SHORT_TO_CPOINTER_CAST:
            case OP_USHORT_TO_CPOINTER_CAST:
            case OP_INT_TO_CPOINTER_CAST:
            case OP_UINT_TO_CPOINTER_CAST:
            case OP_LONG_TO_CPOINTER_CAST:
            case OP_ULONG_TO_CPOINTER_CAST:
            case OP_CHAR_TO_CPOINTER_CAST: 
            case OP_POINTER_TO_CPOINTER_CAST:
            case OP_BOOL_TO_CPOINTER_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_pointer");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CBOOL_CAST :
            case OP_UBYTE_TO_CBOOL_CAST:
            case OP_SHORT_TO_CBOOL_CAST:
            case OP_USHORT_TO_CBOOL_CAST:
            case OP_INT_TO_CBOOL_CAST:
            case OP_UINT_TO_CBOOL_CAST:
            case OP_LONG_TO_CBOOL_CAST:
            case OP_ULONG_TO_CBOOL_CAST:
            case OP_FLOAT_TO_CBOOL_CAST:
            case OP_DOUBLE_TO_CBOOL_CAST:
            case OP_CHAR_TO_CBOOL_CAST:
            case OP_POINTER_TO_CBOOL_CAST:
            case OP_BOOL_TO_CBOOL_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_bool");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_BYTE_TO_CCHAR_CAST :
            case OP_UBYTE_TO_CCHAR_CAST:
            case OP_SHORT_TO_CCHAR_CAST:
            case OP_USHORT_TO_CCHAR_CAST:
            case OP_INT_TO_CCHAR_CAST:
            case OP_UINT_TO_CCHAR_CAST:
            case OP_LONG_TO_CCHAR_CAST:
            case OP_ULONG_TO_CCHAR_CAST:
            case OP_FLOAT_TO_CCHAR_CAST:
            case OP_DOUBLE_TO_CCHAR_CAST:
            case OP_CHAR_TO_CCHAR_CAST:
            case OP_POINTER_TO_CCHAR_CAST:
            case OP_BOOL_TO_CCHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                cast_llvm_value_from_inst(value, inst);

                Function* fun = TheModule->getFunction("create_char");

                std::vector<Value*> params2;

                Value* param1 = value->value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CBYTE_TO_BYTE_CAST:
            case OP_CBYTE_TO_SHORT_CAST:
            case OP_CBYTE_TO_INT_CAST:
            case OP_CBYTE_TO_LONG_CAST:
            case OP_CBYTE_TO_UBYTE_CAST:
            case OP_CBYTE_TO_USHORT_CAST:
            case OP_CBYTE_TO_UINT_CAST:
            case OP_CBYTE_TO_ULONG_CAST:
            case OP_CBYTE_TO_CHAR_CAST:
            case OP_CBYTE_TO_FLOAT_CAST:
            case OP_CBYTE_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cbyte_to_byte_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                cast_llvm_value_from_inst(&llvm_value, inst);

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CUBYTE_TO_BYTE_CAST:
            case OP_CUBYTE_TO_SHORT_CAST:
            case OP_CUBYTE_TO_INT_CAST:
            case OP_CUBYTE_TO_LONG_CAST:
            case OP_CUBYTE_TO_UBYTE_CAST:
            case OP_CUBYTE_TO_USHORT_CAST:
            case OP_CUBYTE_TO_UINT_CAST:
            case OP_CUBYTE_TO_ULONG_CAST:
            case OP_CUBYTE_TO_CHAR_CAST:
            case OP_CUBYTE_TO_FLOAT_CAST:
            case OP_CUBYTE_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cubyte_to_ubyte_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                cast_llvm_value_from_inst(&llvm_value, inst);

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CSHORT_TO_BYTE_CAST:
            case OP_CSHORT_TO_SHORT_CAST:
            case OP_CSHORT_TO_INT_CAST : 
            case OP_CSHORT_TO_LONG_CAST : 
            case OP_CSHORT_TO_UBYTE_CAST :
            case OP_CSHORT_TO_USHORT_CAST :
            case OP_CSHORT_TO_UINT_CAST :
            case OP_CSHORT_TO_ULONG_CAST :
            case OP_CSHORT_TO_CHAR_CAST:
            case OP_CSHORT_TO_FLOAT_CAST: 
            case OP_CSHORT_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cshort_to_short_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CUSHORT_TO_BYTE_CAST:
            case OP_CUSHORT_TO_SHORT_CAST:
            case OP_CUSHORT_TO_INT_CAST : 
            case OP_CUSHORT_TO_LONG_CAST : 
            case OP_CUSHORT_TO_UBYTE_CAST :
            case OP_CUSHORT_TO_USHORT_CAST :
            case OP_CUSHORT_TO_UINT_CAST :
            case OP_CUSHORT_TO_ULONG_CAST :
            case OP_CUSHORT_TO_CHAR_CAST:
            case OP_CUSHORT_TO_FLOAT_CAST:
            case OP_CUSHORT_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cushort_to_ushort_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_INTEGER_TO_BYTE_CAST:
            case OP_INTEGER_TO_SHORT_CAST:
            case OP_INTEGER_TO_INT_CAST : 
            case OP_INTEGER_TO_LONG_CAST : 
            case OP_INTEGER_TO_UBYTE_CAST :
            case OP_INTEGER_TO_USHORT_CAST :
            case OP_INTEGER_TO_UINT_CAST :
            case OP_INTEGER_TO_ULONG_CAST :
            case OP_INTEGER_TO_CHAR_CAST:
            case OP_INTEGER_TO_FLOAT_CAST:
            case OP_INTEGER_TO_DOUBLE_CAST:
            case OP_CBOOL_TO_BYTE_CAST:
            case OP_CBOOL_TO_SHORT_CAST:
            case OP_CBOOL_TO_INT_CAST : 
            case OP_CBOOL_TO_LONG_CAST : 
            case OP_CBOOL_TO_UBYTE_CAST :
            case OP_CBOOL_TO_USHORT_CAST :
            case OP_CBOOL_TO_UINT_CAST :
            case OP_CBOOL_TO_ULONG_CAST :
            case OP_CBOOL_TO_CHAR_CAST: 
            case OP_CBOOL_TO_FLOAT_CAST:
            case OP_CBOOL_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_integer_to_int_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_UINTEGER_TO_BYTE_CAST:
            case OP_UINTEGER_TO_SHORT_CAST:
            case OP_UINTEGER_TO_INT_CAST : 
            case OP_UINTEGER_TO_LONG_CAST : 
            case OP_UINTEGER_TO_UBYTE_CAST :
            case OP_UINTEGER_TO_USHORT_CAST :
            case OP_UINTEGER_TO_UINT_CAST :
            case OP_UINTEGER_TO_ULONG_CAST :
            case OP_UINTEGER_TO_CHAR_CAST:
            case OP_UINTEGER_TO_FLOAT_CAST: 
            case OP_UINTEGER_TO_DOUBLE_CAST:
            case OP_CCHAR_TO_BYTE_CAST:
            case OP_CCHAR_TO_SHORT_CAST:
            case OP_CCHAR_TO_INT_CAST : 
            case OP_CCHAR_TO_LONG_CAST : 
            case OP_CCHAR_TO_UBYTE_CAST :
            case OP_CCHAR_TO_USHORT_CAST :
            case OP_CCHAR_TO_UINT_CAST :
            case OP_CCHAR_TO_ULONG_CAST :
            case OP_CCHAR_TO_CHAR_CAST:
            case OP_CCHAR_TO_FLOAT_CAST:
            case OP_CCHAR_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_uinteger_to_uint_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CLONG_TO_BYTE_CAST:
            case OP_CLONG_TO_SHORT_CAST:
            case OP_CLONG_TO_INT_CAST : 
            case OP_CLONG_TO_LONG_CAST : 
            case OP_CLONG_TO_UBYTE_CAST :
            case OP_CLONG_TO_USHORT_CAST :
            case OP_CLONG_TO_UINT_CAST :
            case OP_CLONG_TO_ULONG_CAST :
            case OP_CLONG_TO_CHAR_CAST:
            case OP_CLONG_TO_FLOAT_CAST: 
            case OP_CLONG_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_clong_to_long_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CULONG_TO_BYTE_CAST:
            case OP_CULONG_TO_SHORT_CAST:
            case OP_CULONG_TO_INT_CAST : 
            case OP_CULONG_TO_LONG_CAST : 
            case OP_CULONG_TO_UBYTE_CAST :
            case OP_CULONG_TO_USHORT_CAST :
            case OP_CULONG_TO_UINT_CAST :
            case OP_CULONG_TO_ULONG_CAST :
            case OP_CULONG_TO_CHAR_CAST:
            case OP_CULONG_TO_FLOAT_CAST:
            case OP_CULONG_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_culong_to_ulong_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CPOINTER_TO_BYTE_CAST:
            case OP_CPOINTER_TO_SHORT_CAST:
            case OP_CPOINTER_TO_INT_CAST : 
            case OP_CPOINTER_TO_LONG_CAST : 
            case OP_CPOINTER_TO_UBYTE_CAST :
            case OP_CPOINTER_TO_USHORT_CAST :
            case OP_CPOINTER_TO_UINT_CAST :
            case OP_CPOINTER_TO_ULONG_CAST :
            case OP_CPOINTER_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cpointer_to_pointer_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                llvm_value.value = Builder.CreateCast(Instruction::IntToPtr, llvm_value.value, PointerType::get(IntegerType::get(TheContext, 64), 0));

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CFLOAT_TO_BYTE_CAST:
            case OP_CFLOAT_TO_SHORT_CAST:
            case OP_CFLOAT_TO_INT_CAST : 
            case OP_CFLOAT_TO_LONG_CAST : 
            case OP_CFLOAT_TO_UBYTE_CAST :
            case OP_CFLOAT_TO_USHORT_CAST :
            case OP_CFLOAT_TO_UINT_CAST :
            case OP_CFLOAT_TO_ULONG_CAST :
            case OP_CFLOAT_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cfloat_to_int_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CFLOAT_TO_FLOAT_CAST:
            case OP_CFLOAT_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cfloat_to_float_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CDOUBLE_TO_BYTE_CAST:
            case OP_CDOUBLE_TO_SHORT_CAST:
            case OP_CDOUBLE_TO_INT_CAST : 
            case OP_CDOUBLE_TO_LONG_CAST : 
            case OP_CDOUBLE_TO_UBYTE_CAST :
            case OP_CDOUBLE_TO_USHORT_CAST :
            case OP_CDOUBLE_TO_UINT_CAST :
            case OP_CDOUBLE_TO_ULONG_CAST :
            case OP_CDOUBLE_TO_CHAR_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cdouble_to_int_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CDOUBLE_TO_FLOAT_CAST:
            case OP_CDOUBLE_TO_DOUBLE_CAST: {
                LVALUE* value = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_cdouble_to_double_cast");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(value, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                cast_llvm_value_from_inst(&llvm_value, inst);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_ARRAY_TO_CARRAY_CAST: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                char* class_name = CONS_str(constant, offset);

                LVALUE* array = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("run_array_to_carray_cast");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                LVALUE array2 = trunc_value(array, 32);
                Value* param5 = array2.value;
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)class_name);
                params2.push_back(param6);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value2);
                }
                break;

            case OP_GET_ARRAY_LENGTH: {
                LVALUE* array_ = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_array_length");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(array_, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value2);
                }
                break;

            case OP_GET_REGEX_GLOBAL: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_global");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_GET_REGEX_IGNORE_CASE: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_ignorecase");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_GET_REGEX_MULTILINE: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_multiline");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_GET_REGEX_EXTENDED: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_extended");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_GET_REGEX_DOTALL: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_dotall");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_GET_REGEX_ANCHORED: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_anchored");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_GET_REGEX_DOLLAR_ENDONLY: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_dollar_endonly");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;


            case OP_GET_REGEX_UNGREEDY: {
                LVALUE* regex = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("get_regex_ungreedy");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(regex, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CHAR_UPPERCASE: {
                LVALUE* c = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("char_uppercase");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(c, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CHAR_LOWERCASE: {
                LVALUE* c = get_stack_ptr_value_from_index(llvm_stack_ptr, -1);

                Function* fun = TheModule->getFunction("char_lowercase");

                std::vector<Value*> params2;

                LVALUE value2;
                value2 = trunc_value(c, 32);

                Value* param1 = value2.value;
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                dec_stack_ptr(&llvm_stack_ptr, 1);

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);
                }
                break;

            case OP_CREATE_STRING: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                char* str = CONS_str(constant, offset);

                Function* function = TheModule->getFunction("create_string_object");

                std::vector<Value*> params2;
                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)str);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(function, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_BUFFER: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                int size = *(int*)pc;
                pc += sizeof(int);

                char* str = CONS_str(constant, offset);

                Function* function = TheModule->getFunction("create_buffer_object");

                std::vector<Value*> params2;
                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)str);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);

                Value* param2 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)size);
                params2.push_back(param2);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(function, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_PATH: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                char* buf = CONS_str(constant, offset);

                Function* function = TheModule->getFunction("create_path_object");

                std::vector<Value*> params2;
                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)buf);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(function, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_ARRAY: {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_array");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);

                /// delete vm stack ///
                dec_vm_stack_ptr(params, current_block, num_elements);
                }
                break;

            case OP_CREATE_CARRAY : {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_carray");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_SORTABLE_CARRAY : {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_sortable_carray");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_EQUALABLE_CARRAY : {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_equalable_carray");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_LIST : {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_list");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_SORTALBE_LIST: {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_sortable_list");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_EQUALABLE_LIST: {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_equalable_list");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                std::string constant_value_name("constant");
                Value* param7 = params[constant_value_name];
                params2.push_back(param7);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_TUPLE: {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements);

                Function* fun = TheModule->getFunction("run_create_tuple");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_HASH: {
                int num_elements = *(int*)pc;
                pc += sizeof(int);

                int offset = *(int*)pc;
                pc += sizeof(int);

                int offset2 = *(int*)pc;
                pc += sizeof(int);

                int class_name_offset = offset;
                int class_name_offset2 = offset2;

                llvm_stack_to_vm_stack(llvm_stack_ptr, params, current_block, num_elements*2);

                Function* fun = TheModule->getFunction("run_create_hash");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string var_num_value_name("var_num");
                Value* param3 = params[var_num_value_name];
                params2.push_back(param3);

                std::string info_value_name("info");
                Value* param4 = params[info_value_name];
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)num_elements);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset);
                params2.push_back(param6);

                Value* param7 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)class_name_offset2);
                params2.push_back(param7);

                std::string constant_value_name("constant");
                Value* param8 = params[constant_value_name];
                params2.push_back(param8);

                Value* result = Builder.CreateCall(fun, params2);

                Value* result1 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 0);
                result1  = Builder.CreateAlignedLoad(result1,  4);
                Value* result2 = Builder.CreateStructGEP(gCLValueAndBoolStruct, result, 1);
                result2  = Builder.CreateAlignedLoad(result2,  4);

                if_value_is_zero_ret_zero(result2, params, function, &current_block);

                LVALUE llvm_value;
                llvm_value.value = result1;
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// dec llvm stack pointer ///
                dec_stack_ptr(&llvm_stack_ptr, num_elements*2);

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_BLOCK_OBJECT : {
                int code_offset = *(int*)pc;
                pc += sizeof(int);

                int code_len = *(int*)pc;
                pc += sizeof(int);

                int constant_offset = *(int*)pc;
                pc += sizeof(int);

                int constant_len = *(int*)pc;
                pc += sizeof(int);

                int block_var_num = *(int*)pc;
                pc += sizeof(int);

                int parent_var_num = *(int*)pc;
                pc += sizeof(int);

                int lambda = *(int*)pc;
                pc += sizeof(int);

                Function* fun = TheModule->getFunction("run_create_block_object");

                std::vector<Value*> params2;

                std::string stack_ptr_address_name("stack_ptr_address");
                Value* param1 = params[stack_ptr_address_name];
                params2.push_back(param1);

                std::string stack_value_name("stack");
                Value* param2 = params[stack_value_name];
                params2.push_back(param2);

                std::string constant_value_name("constant");
                Value* param3 = params[constant_value_name];
                params2.push_back(param3);

                Value* param4 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)code_offset);
                params2.push_back(param4);

                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)code_len);
                params2.push_back(param5);

                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)constant_offset);
                params2.push_back(param6);

                Value* param7 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)constant_len);
                params2.push_back(param7);

                Value* param8 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)block_var_num);
                params2.push_back(param8);

                Value* param9 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)parent_var_num);
                params2.push_back(param9);

                Value* param10 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)lambda);
                params2.push_back(param10);

                std::string info_value_name("info");
                Value* param11 = params[info_value_name];
                params2.push_back(param11);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(fun, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                /// vm stack_ptr to llvm stack ///
                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            case OP_CREATE_REGEX: {
                int offset = *(int*)pc;
                pc += sizeof(int);

                BOOL global = *(int*)pc;
                pc += sizeof(int);

                BOOL ignore_case = *(int*)pc;
                pc += sizeof(int);

                BOOL multiline = *(int*)pc;
                pc += sizeof(int);

                BOOL extended = *(int*)pc;
                pc += sizeof(int);

                BOOL dotall = *(int*)pc;
                pc += sizeof(int);

                BOOL anchored = *(int*)pc;
                pc += sizeof(int);

                BOOL dollar_endonly = *(int*)pc;
                pc += sizeof(int);

                BOOL ungreedy = *(int*)pc;
                pc += sizeof(int);

                char* regex_str = CONS_str(constant, offset);

                Function* function = TheModule->getFunction("create_regex_object");

                std::vector<Value*> params2;
                Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)regex_str);
                Value* param1 = ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
                params2.push_back(param1);
                Value* param2 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)global);
                params2.push_back(param2);
                Value* param3 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)ignore_case);
                params2.push_back(param3);
                Value* param4 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)multiline);
                params2.push_back(param4);
                Value* param5 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)extended);
                params2.push_back(param5);
                Value* param6 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)dotall);
                params2.push_back(param6);
                Value* param7 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)anchored);
                params2.push_back(param7);
                Value* param8 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)dollar_endonly);
                params2.push_back(param8);
                Value* param9 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)ungreedy);
                params2.push_back(param9);

                LVALUE llvm_value;
                llvm_value.value = Builder.CreateCall(function, params2);
                llvm_value.lvar_address_index = -1;
                llvm_value.lvar_stored = FALSE;
                llvm_value.constant_int_value = FALSE;
                llvm_value.constant_float_value = FALSE;

                push_value_to_stack_ptr(&llvm_stack_ptr, &llvm_value);

                /// push object to jit objects ///
                Function* fun2 = TheModule->getFunction("push_jit_object");

                std::vector<Value*> params3;

                LVALUE llvm_value2;
                llvm_value2 = trunc_value(&llvm_value, 32);

                param1 = llvm_value2.value;
                params3.push_back(param1);

                (void)Builder.CreateCall(fun2, params3);
                }
                break;

            default:
                printf("inst %d\n", inst);
                exit(1);
        }

#ifdef MDEBUG
if(inst != OP_HEAD_OF_EXPRESSION && inst != OP_SIGINT) {
    //show_stack_for_llvm_stack(llvm_stack, llvm_stack_ptr, var_num);
    //call_show_stack(params);
}
#endif
    }

    // Finish off the function.
    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 1, true));

    //Builder.SetInsertPoint(current_block);
    Builder.CreateRet(ret_value);

    // Validate the generated code, checking for consistency.
    verifyFunction(*function);

    // Run the optimizer on the function.
    TheFPM->run(*function);

    return TRUE;
}

static BOOL compile_jit_methods(sCLClass* klass)
{
    char module_name[PATH_MAX + 128];
    snprintf(module_name, PATH_MAX, "Module %s", CLASS_NAME(klass));
    TheModule = new Module(module_name, TheContext);

    TheFPM = llvm::make_unique<legacy::FunctionPassManager>(TheModule);
    
    //TheFPM->add(createInstructionCombiningPass());
    TheFPM->add(createReassociatePass());
    TheFPM->add(createGVNPass());
    TheFPM->add(createCFGSimplificationPass());
    TheFPM->doInitialization();

    create_internal_functions();
    TheLabels.clear();

    int i;
    int num_compiled_method = 0;

    if(!(klass->mFlags & CLASS_FLAGS_INTERFACE)) {
        for(i=0; i<klass->mNumMethods; i++) {
            sCLMethod* method = klass->mMethods + i;

            if(strcmp(METHOD_NAME2(klass, method), "initialize") != 0 && strcmp(METHOD_NAME2(klass, method), "finalize") != 0 && !(method->mFlags & METHOD_FLAGS_NATIVE)) 
            {
                char method_path2[METHOD_NAME_MAX + 128];
                create_method_path_for_jit(klass, method, method_path2, METHOD_NAME_MAX + 128);

                sByteCode* code = &method->uCode.mByteCodes;
                sConst* constant = &klass->mConst;

                if(!compile_to_native_code(code, constant, klass, method, method_path2)) {
                    return FALSE;
                }

                num_compiled_method++;
            }
        }
    }

    if(num_compiled_method > 0) {
        char path[PATH_MAX];
        snprintf(path, PATH_MAX, "%s.bc", CLASS_NAME(klass));

        (void)unlink(path);

        std::error_code ecode;
        llvm::raw_fd_ostream output_stream(path, ecode, llvm::sys::fs::F_None);

        std::string err_str;
        raw_string_ostream err_ostream(err_str);

///    if(verifyModule(*TheModule, &err_ostream)) {
//
if(strcmp(CLASS_NAME(klass), "JITTest") == 0) {
TheModule->dump();
}

        llvm::WriteBitcodeToFile(TheModule, output_stream);
        output_stream.flush();
/*
        }
        else {
            errs() << module_name << "\n";
            errs() << "assembly parsed, but does not verify as correct\n";
            errs() << err_ostream.str();
        }
*/
    }

    delete TheModule;

    return TRUE;
}

BOOL jit_compile_all_classes()
{
    jit_init();

    sClassTable* p = gHeadClassTable;

    while(p) {
        sCLClass* klass = p->mItem;
        if(klass->mFlags & CLASS_FLAGS_MODIFIED) {
            if(!compile_jit_methods(klass)) {
                jit_final();
                return FALSE;
            }
        }
        p = p->mNextClass;
    }

    jit_final();

    return TRUE;
}

} // extern "C"
