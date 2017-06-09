#include "jit_common.hpp"

extern "C" 
{

void create_internal_functions()
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
    Function::Create(function_type, Function::ExternalLinkage, "create_string_object", TheModule.get());

    /// create_byte ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 8);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_byte", TheModule.get());

    /// create_ubyte ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 8);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_ubyte", TheModule.get());

    /// create_short ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 16);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_short", TheModule.get());

    /// create_ushort ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 16);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_ushort", TheModule.get());

    /// create_integer ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_integer", TheModule.get());

    /// create_integer ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_uinteger", TheModule.get());

    /// create_long ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,64);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_long", TheModule.get());

    /// create_ulong ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext,64);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_ulong", TheModule.get());

    /// create_float ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = Type::getFloatTy(TheContext);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_float", TheModule.get());

    /// create_double ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = Type::getDoubleTy(TheContext);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_double", TheModule.get());

    /// create_pointer ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_pointer", TheModule.get());

    /// create_char ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_char", TheModule.get());

    /// create_bool ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_bool", TheModule.get());

    /// create_buffer_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_buffer_object", TheModule.get());

    /// create_path_object ///
    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_path_object", TheModule.get());

    /// create_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_object", TheModule.get());

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
    Function::Create(function_type, Function::ExternalLinkage, "create_regex_object", TheModule.get());

    /// create_array_object ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "create_array_object", TheModule.get());

    /// run_head_of_expression ///
    type_params.clear();

    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_head_of_expression", TheModule.get());

    /// run_sigint ///
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
    Function::Create(function_type, Function::ExternalLinkage, "run_sigint", TheModule.get());
    /// run_load_address ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param5_type);

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_address", TheModule.get());

    /// run_ldclong ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ldclong", TheModule.get());

    /// run_ldcpointer ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ldcpointer", TheModule.get());

    /// run_lduclong ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ldculong", TheModule.get());

    /// show_inst ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_inst_in_jit", TheModule.get());

    /// show_number ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_number_in_jit", TheModule.get());

    /// show_str ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_str_in_jit", TheModule.get());

    /// show_stack_stat ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "show_stack_stat", TheModule.get());

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
    Function::Create(function_type, Function::ExternalLinkage, "show_stack_in_jit", TheModule.get());

    /// invoke_method ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);
    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
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

    Function::Create(function_type, Function::ExternalLinkage, "invoke_method", TheModule.get());

    /// try_function ///
    type_params.clear();

    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext,8), 0);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "try_function", TheModule.get());

    /// get_try_catch_label_name ///
    type_params.clear();

    result_type = PointerType::get(IntegerType::get(TheContext,8), 0);

    param1_type = PointerType::get(IntegerType::get(TheContext,64), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_try_catch_label_name", TheModule.get());

    /// entry_exception_object ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "entry_exception_object", TheModule.get());

    /// run_load_field ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_field", TheModule.get());

    /// regex_equals ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "regex_equals", TheModule.get());

    /// entry_exception_object_with_class_name ///
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

    param5_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param5_type);

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "entry_exception_object_with_class_name2", TheModule.get());

    /// get_string_object_of_object_name ///
    type_params.clear();
    
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "get_string_object_of_object_name", TheModule.get());

    /// object_implements_interface ///
    type_params.clear();

    result_type = IntegerType::get(TheContext, 32);
    
    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "object_implements_interface", TheModule.get());

    /// invoke_virtual_method ///
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param2_type);

    param3_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param3_type);

    param4_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param5_type);

    param6_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param7_type);

    param8_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param8_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "invoke_virtual_method", TheModule.get());

    /// invoke_dynamic_method ///
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

    param9_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param9_type);

    param10_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param10_type);

    param11_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param11_type);

    param12_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param12_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "invoke_dynamic_method", TheModule.get());

    /// invoke_block_in_jit ///
    result_type = IntegerType::get(TheContext, 32);

    param1_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param3_type);

    param4_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param4_type);

    param5_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param5_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "invoke_block_in_jit", TheModule.get());

    /// run_load_field_adrress ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_field_address", TheModule.get());

    /// run_store_field ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_store_field", TheModule.get());

    /// run_load_class_field ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_class_field", TheModule.get());

    /// run_load_class_field_address ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_class_field_address", TheModule.get());

    /// run_store_class_field ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_store_class_field", TheModule.get());

    /// run_load_element ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_load_element", TheModule.get());

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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_store_element", TheModule.get());

    /// run_get_array_length ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_array_length", TheModule.get());

    /// run_get_regex_global ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_global", TheModule.get());

    /// run_get_regex_ignorecase ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_ignorecase", TheModule.get());

    /// run_get_regex_multiline ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_multiline", TheModule.get());

    /// run_get_regex_extended ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_extended", TheModule.get());

    /// run_get_regex_dotall ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_dotall", TheModule.get());

    /// run_get_regex_anchored ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_anchored", TheModule.get());

    /// run_get_regex_dollar_endonly ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_dollar_endonly", TheModule.get());

    /// run_get_regex_ungreedy ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_ungreedy", TheModule.get());

    /// run_get_regex_anchored ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_get_regex_multiline", TheModule.get());

    /// run_char_uppercase ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_char_uppercase", TheModule.get());

    /// run_char_lowercase ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_char_lowercase", TheModule.get());

    /// run_create_array ///
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

    param6_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_array", TheModule.get());

    /// run_create_carray ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_carray", TheModule.get());

    /// run_create_equalable_carray ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_equalable_carray", TheModule.get());

    /// run_create_sortable_carray ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_sortable_carray", TheModule.get());

    /// run_create_list ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_list", TheModule.get());

    /// run_create_sortable_list ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_sortable_list", TheModule.get());

    /// run_create_equalable_list ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_equalable_list", TheModule.get());

    /// run_create_tuple ///
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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_tuple", TheModule.get());

    /// run_create_hash ///
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

    param6_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param6_type);

    param7_type = PointerType::get(IntegerType::get(TheContext, 8), 0);
    type_params.push_back(param7_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_create_hash", TheModule.get());

    /// run_create_block_object ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

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
    Function::Create(function_type, Function::ExternalLinkage, "run_create_block_object", TheModule.get());

    /// run_byte_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_byte_to_string_cast", TheModule.get());

    /// run_short_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_short_to_string_cast", TheModule.get());

    /// run_int_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_int_to_string_cast", TheModule.get());

    /// run_long_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_long_to_string_cast", TheModule.get());

    /// run_ubyte_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ubyte_to_string_cast", TheModule.get());

    /// run_ushort_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ushort_to_string_cast", TheModule.get());
    /// run_uint_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uint_to_string_cast", TheModule.get());

    /// run_ulong_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_ulong_to_string_cast", TheModule.get());

    /// run_float_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_float_to_string_cast", TheModule.get());

    /// run_double_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_double_to_string_cast", TheModule.get());
    /// run_bool_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_bool_to_string_cast", TheModule.get());

    /// run_regex_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_regex_to_string_cast", TheModule.get());

    /// run_pointer_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_pointer_to_string_cast", TheModule.get());

    /// run_char_to_string_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_char_to_string_cast", TheModule.get());

    /// run_cbyte_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_byte_cast", TheModule.get());

    /// run_cubyte_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_byte_cast", TheModule.get());

    /// run_cshort_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_byte_cast", TheModule.get());

    /// run_cushort_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_byte_cast", TheModule.get());

    /// run_integer_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_byte_cast", TheModule.get());

    /// run_uinteger_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_byte_cast", TheModule.get());

    /// run_clong_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_byte_cast", TheModule.get());

    /// run_culong_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_byte_cast", TheModule.get());

    /// run_cfloat_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_byte_cast", TheModule.get());

    /// run_cdouble_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_byte_cast", TheModule.get());

    /// run_cpointer_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_byte_cast", TheModule.get());

    /// run_cchar_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_byte_cast", TheModule.get());

    /// run_cbool_to_byte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_byte_cast", TheModule.get());

    /// run_cbyte_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_short_cast", TheModule.get());

    /// run_cubyte_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_short_cast", TheModule.get());

    /// run_cshort_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_short_cast", TheModule.get());

    /// run_cushort_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_short_cast", TheModule.get());

    /// run_integer_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_short_cast", TheModule.get());

    /// run_uinteger_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_short_cast", TheModule.get());

    /// run_clong_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_short_cast", TheModule.get());

    /// run_culong_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_short_cast", TheModule.get());

    /// run_cfloat_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_short_cast", TheModule.get());

    /// run_cdouble_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_short_cast", TheModule.get());
    /// run_cpointer_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_short_cast", TheModule.get());
    /// run_cchar_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_short_cast", TheModule.get());

    /// run_cbool_to_short_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_short_cast", TheModule.get());

    /// run_cbyte_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_int_cast", TheModule.get());

    /// run_cubyte_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_int_cast", TheModule.get());

    /// run_cshort_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_int_cast", TheModule.get());

    /// run_cushort_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_int_cast", TheModule.get());

    /// run_integer_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_int_cast", TheModule.get());

    /// run_uinteger_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_int_cast", TheModule.get());

    /// run_clong_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_int_cast", TheModule.get());

    /// run_culong_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_int_cast", TheModule.get());

    /// run_cfloat_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_int_cast", TheModule.get());

    /// run_cdouble_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_int_cast", TheModule.get());

    /// run_cpointer_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_int_cast", TheModule.get());

    /// run_cchar_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_int_cast", TheModule.get());

    /// run_cbool_to_int_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_int_cast", TheModule.get());


    /// run_cbyte_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_long_cast", TheModule.get());

    /// run_cubyte_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_long_cast", TheModule.get());

    /// run_cshort_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_long_cast", TheModule.get());

    /// run_cushort_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_long_cast", TheModule.get());

    /// run_integer_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_long_cast", TheModule.get());

    /// run_uinteger_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_long_cast", TheModule.get());

    /// run_clong_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_long_cast", TheModule.get());

    /// run_culong_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_long_cast", TheModule.get());

    /// run_cfloat_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_long_cast", TheModule.get());

    /// run_cdouble_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_long_cast", TheModule.get());

    /// run_cpointer_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_long_cast", TheModule.get());

    /// run_cchar_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_long_cast", TheModule.get());

    /// run_cbool_to_long_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_long_cast", TheModule.get());

    /// run_cbyte_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_ubyte_cast", TheModule.get());

    /// run_cubyte_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_ubyte_cast", TheModule.get());

    /// run_cshort_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_ubyte_cast", TheModule.get());

    /// run_cushort_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_ubyte_cast", TheModule.get());

    /// run_integer_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_ubyte_cast", TheModule.get());

    /// run_uinteger_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_ubyte_cast", TheModule.get());

    /// run_clong_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_ubyte_cast", TheModule.get());

    /// run_culong_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_ubyte_cast", TheModule.get());

    /// run_cfloat_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_ubyte_cast", TheModule.get());

    /// run_cdouble_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_ubyte_cast", TheModule.get());

    /// run_cpointer_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_ubyte_cast", TheModule.get());

    /// run_cchar_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_ubyte_cast", TheModule.get());

    /// run_cbool_to_ubyte_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_ubyte_cast", TheModule.get());

    /// run_cbyte_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_ushort_cast", TheModule.get());

    /// run_cubyte_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_ushort_cast", TheModule.get());

    /// run_cshort_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_ushort_cast", TheModule.get());

    /// run_cushort_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_ushort_cast", TheModule.get());

    /// run_integer_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_ushort_cast", TheModule.get());

    /// run_uinteger_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_ushort_cast", TheModule.get());

    /// run_clong_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_ushort_cast", TheModule.get());

    /// run_culong_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_ushort_cast", TheModule.get());

    /// run_cfloat_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_ushort_cast", TheModule.get());

    /// run_cdouble_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_ushort_cast", TheModule.get());
    /// run_cpointer_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_ushort_cast", TheModule.get());
    /// run_cchar_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_ushort_cast", TheModule.get());

    /// run_cbool_to_ushort_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_ushort_cast", TheModule.get());

    /// run_cbyte_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_uint_cast", TheModule.get());

    /// run_cubyte_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_uint_cast", TheModule.get());

    /// run_cshort_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_uint_cast", TheModule.get());

    /// run_cushort_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_uint_cast", TheModule.get());

    /// run_integer_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_uint_cast", TheModule.get());

    /// run_uinteger_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_uint_cast", TheModule.get());

    /// run_clong_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_uint_cast", TheModule.get());

    /// run_culong_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_uint_cast", TheModule.get());

    /// run_cfloat_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_uint_cast", TheModule.get());

    /// run_cdouble_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_uint_cast", TheModule.get());

    /// run_cpointer_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_uint_cast", TheModule.get());

    /// run_cchar_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_uint_cast", TheModule.get());

    /// run_cbool_to_uint_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_uint_cast", TheModule.get());

    /// run_cbyte_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_ulong_cast", TheModule.get());

    /// run_cubyte_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_ulong_cast", TheModule.get());

    /// run_cshort_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_ulong_cast", TheModule.get());

    /// run_cushort_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_ulong_cast", TheModule.get());

    /// run_integer_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_ulong_cast", TheModule.get());

    /// run_uinteger_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_ulong_cast", TheModule.get());

    /// run_clong_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_ulong_cast", TheModule.get());

    /// run_culong_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_ulong_cast", TheModule.get());

    /// run_cfloat_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_ulong_cast", TheModule.get());

    /// run_cdouble_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_ulong_cast", TheModule.get());

    /// run_cpointer_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cpointer_to_ulong_cast", TheModule.get());

    /// run_cchar_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_ulong_cast", TheModule.get());

    /// run_cbool_to_ulong_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_ulong_cast", TheModule.get());

    /// run_cbyte_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_float_cast", TheModule.get());

    /// run_cubyte_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_float_cast", TheModule.get());

    /// run_cshort_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_float_cast", TheModule.get());

    /// run_cushort_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_float_cast", TheModule.get());

    /// run_integer_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_float_cast", TheModule.get());

    /// run_uinteger_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_float_cast", TheModule.get());

    /// run_clong_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_float_cast", TheModule.get());

    /// run_culong_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_float_cast", TheModule.get());

    /// run_cfloat_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_float_cast", TheModule.get());

    /// run_cdouble_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_float_cast", TheModule.get());


    /// run_cchar_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_float_cast", TheModule.get());

    /// run_cbool_to_float_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_float_cast", TheModule.get());

    /// run_cbyte_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbyte_to_double_cast", TheModule.get());

    /// run_cubyte_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cubyte_to_double_cast", TheModule.get());

    /// run_cshort_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cshort_to_double_cast", TheModule.get());

    /// run_cushort_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cushort_to_double_cast", TheModule.get());

    /// run_integer_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_integer_to_double_cast", TheModule.get());

    /// run_uinteger_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_uinteger_to_double_cast", TheModule.get());

    /// run_clong_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_clong_to_double_cast", TheModule.get());

    /// run_culong_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_culong_to_double_cast", TheModule.get());

    /// run_cfloat_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cfloat_to_double_cast", TheModule.get());

    /// run_cdouble_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cdouble_to_double_cast", TheModule.get());


    /// run_cchar_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cchar_to_double_cast", TheModule.get());

    /// run_cbool_to_double_cast ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_cbool_to_double_cast", TheModule.get());

    /// run_array_to_carray_cast ///
    type_params.clear();
    
    result_type = Type::getInt32Ty(TheContext);

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

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_array_to_carray_cast", TheModule.get());


    /// run_native_method_dec_stack_ptr ///
    type_params.clear();
    
    result_type = Type::getVoidTy(TheContext);

    param1_type = PointerType::get(PointerType::get(IntegerType::get(TheContext, 64), 0), 0);
    type_params.push_back(param1_type);

    param2_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param2_type);

    param3_type = PointerType::get(IntegerType::get(TheContext, 64), 0);
    type_params.push_back(param3_type);

    param4_type = IntegerType::get(TheContext, 32);
    type_params.push_back(param4_type);

    param5_type = IntegerType::get(TheContext, 64);
    type_params.push_back(param5_type);

    function_type = FunctionType::get(result_type, type_params, false);
    Function::Create(function_type, Function::ExternalLinkage, "run_native_method_dec_stack_ptr", TheModule.get());
}

void InitializeModuleAndPassManager() 
{
    TheModule = llvm::make_unique<Module>("Clover2 jit", TheContext);
    TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());
    
    TheFPM = llvm::make_unique<legacy::FunctionPassManager>(TheModule.get());
    
    //TheFPM->add(createInstructionCombiningPass()); // --> Segmentation Fault
    TheFPM->add(createReassociatePass());
    TheFPM->add(createGVNPass());
    TheFPM->add(createCFGSimplificationPass());
    TheFPM->doInitialization();

    create_internal_functions();
    TheLabels.clear();
}

void jit_init()
{
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    TheJIT = llvm::make_unique<CloverJIT>();

    InitializeModuleAndPassManager();
}

void jit_final()
{
}

void create_method_path_for_jit(sCLClass* klass, sCLMethod* method, char* result, int size_result)
{
    snprintf(result, size_result, "%s$$%d", METHOD_PATH(klass, method), method->mMethodIndex);
}

/////////////////////////////////////////////////////////////////////////////
/// JIT debug functions
/////////////////////////////////////////////////////////////////////////////
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
    printf("%d\n", number);
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

void call_show_str_in_jit(char* str)
{
    Function* show_str = TheModule->getFunction("show_str_in_jit");

    std::vector<Value*> params2;
    Value* param1 = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)str);
    params2.push_back(param1);

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

void call_show_stack(int var_num, sVMInfo* info, std::map<std::string, Value *> params)
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

    Value* param3 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)var_num);
    params2.push_back(param3);

    Value* param4 = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)info);
    params2.push_back(param4);

    Value* result = Builder.CreateCall(show_stack_fun, params2);
}

//////////////////////////////////////////////////////////////////////
// VM functions
//////////////////////////////////////////////////////////////////////
void run_head_of_expression(sVMInfo* info, char* sname, int sline)
{
    info->sname = sname;
    info->sline = sline;

    gSigInt = FALSE;
}

BOOL run_sigint(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info)
{
    if(gSigInt) {
        gSigInt = FALSE;
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "Signal Interrupt");
        return FALSE;
    }

    return TRUE;
}

BOOL run_load_field(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int field_index)
{
    CLObject obj = ((*stack_ptr) -1)->mObjectValue;
    (*stack_ptr)--;

    if(obj == 0) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "Null pointer exception(3)");
        return FALSE;
    }

    sCLObject* object_pointer = CLOBJECT(obj);
    sCLClass* klass = object_pointer->mClass;

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(4)");
        return FALSE;
    }

    if(field_index < 0 || field_index >= klass->mNumFields) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "field index is invalid");
        return FALSE;
    }

    CLVALUE value = object_pointer->mFields[field_index];
    **stack_ptr = value;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_load_address(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, CLVALUE* lvar, int index)
{
    (*stack_ptr)->mPointerValue = (char*)&lvar[index];
    (*stack_ptr)++;

    return TRUE;
}

void run_ldclong(CLVALUE** stack_ptr, int value1, int value2)
{
    long lvalue;

    memcpy(&lvalue, &value1, sizeof(int));
    memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

    (*stack_ptr)->mLongValue = lvalue;
    (*stack_ptr)++;
}

void run_ldcpointer(CLVALUE** stack_ptr, int value1, int value2)
{
    void* lvalue;

    memcpy(&lvalue, &value1, sizeof(int));
    memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

    (*stack_ptr)->mPointerValue = (char*)lvalue;
    (*stack_ptr)++;
}

void run_ldculong(CLVALUE** stack_ptr, int value1, int value2)
{
    long lvalue;

    memcpy(&lvalue, &value1, sizeof(int));
    memcpy((char*)&lvalue + sizeof(int), &value2, sizeof(int));

    (*stack_ptr)->mULongValue = lvalue;
    (*stack_ptr)++;
}

void try_function(sVMInfo* info, char* try_cach_label)
{
    info->try_catch_label_name = try_cach_label;
}

char* get_try_catch_label_name(sVMInfo* info)
{
    return info->try_catch_label_name;
}

//////////////////////////////////////////////////////////////////////
// LLVM operation functions
//////////////////////////////////////////////////////////////////////
void store_value(Value* llvm_value, Value* stored_value, BasicBlock* current_block)
{
    Builder.CreateStore(llvm_value, stored_value);
}

void store_value_with_aligned(Value* llvm_value, Value* stored_value, BasicBlock* current_block, int align)
{
    Builder.CreateAlignedStore(llvm_value, stored_value, align);
}

void inc_stack_ptr(std::map<std::string, Value*>& params, BasicBlock* current_block, int value)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");


    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*value, true));
    Value* inc_ptr_value = Builder.CreateAdd(lvalue, rvalue, "inc_ptr_value", false, false);

    std::string stack_ptr_arg_name("stack_ptr");
    params[stack_ptr_arg_name] = inc_ptr_value;

    store_value(inc_ptr_value, stack_ptr_address_value, current_block);
}

void dec_stack_ptr(std::map<std::string, Value*>& params, BasicBlock* current_block, int value)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*value, true));
    Value* dec_ptr_value = Builder.CreateSub(lvalue, rvalue, "dec_ptr_value", true, true);

    std::string stack_ptr_arg_name("stack_ptr");
    params[stack_ptr_arg_name] = dec_ptr_value;

    store_value(dec_ptr_value, stack_ptr_address_value, current_block);
}

Value* get_stack_ptr_value_from_offset(std::map<std::string, Value*>& params, BasicBlock* current_block, int offset)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*offset, true));
    return Builder.CreateSub(lvalue, rvalue, "offset_stack_ptr", true, true);
}

Value* get_lvar_value_from_offset(std::map<std::string, Value*>& params, BasicBlock* current_block, int offset)
{
    std::string lvar_arg_name("lvar");
    Value* lvar_value = params[lvar_arg_name];

    Value* lvalue = lvar_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*offset, true));
    Value* offset_lvar = Builder.CreateAdd(lvalue, rvalue, "offset_lvar", true, true);

    return Builder.CreateAlignedLoad(offset_lvar, 8, "offset_lvar");
}

void store_value_to_lvar_with_offset(std::map<std::string, Value*>& params, BasicBlock* current_block, int index, Value* llvm_value)
{
    std::string lvar_arg_name("lvar");
    Value* lvar_value = params[lvar_arg_name];
    
    Value* lvalue = lvar_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*index, true));
    Value* lvar_offset_value = Builder.CreateAdd(lvalue, rvalue, "lvar_offset_value", true, true);

    store_value_with_aligned(llvm_value, lvar_offset_value, current_block, 8);
}


Value* get_stack_ptr_value_from_index_with_aligned(std::map<std::string, Value*>& params, BasicBlock* current_block, int index, int align)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*index, true));
    Value* stack_pointer_offset_value = Builder.CreateAdd(lvalue, rvalue, "stack_pointer_offset_value", true, true);

    Value* value = Builder.CreateAlignedLoad(stack_pointer_offset_value, align, "stack_pointer_offset_value");

    Value* result;
    switch(align) {
        case 1:
            result = Builder.CreateCast(Instruction::Trunc, value, Type::getInt8Ty(TheContext));
            break;

        case 2:
            result = Builder.CreateCast(Instruction::Trunc, value, Type::getInt16Ty(TheContext));
            break;

        case 4:
            result = Builder.CreateCast(Instruction::Trunc, value, Type::getInt32Ty(TheContext));
            break;

        case 8:
            result = Builder.CreateCast(Instruction::Trunc, value, Type::getInt64Ty(TheContext));
            break;
    }

    return result;
}

Value* get_stack_ptr_float_value_from_index_with_aligned(std::map<std::string, Value*>& params, BasicBlock* current_block, int index, int align)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*index, true));
    Value* stack_pointer_offset_value = Builder.CreateAdd(lvalue, rvalue, "stack_pointer_offset_value", true, true);

    Value* value = Builder.CreateAlignedLoad(stack_pointer_offset_value, align, "stack_pointer_offset_value");

    Value* result;
    switch(align) {
        case 4:
            result = Builder.CreateCast(Instruction::Trunc, value, Type::getFloatTy(TheContext), "fvalue");
            break;

        case 8:
            result = Builder.CreateCast(Instruction::Trunc, value, Type::getDoubleTy(TheContext), "dvalue");
            break;
    }

    return result;
}

Value* get_stack_ptr_pointer_value_from_index(std::map<std::string, Value*>& params, BasicBlock* current_block, int index)
{
    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

    Value* lvalue = loaded_stack_ptr_address_value;
    Value* rvalue = ConstantInt::get(TheContext, llvm::APInt(64, 8*index, true));
    Value* stack_pointer_offset_value = Builder.CreateAdd(lvalue, rvalue, "stack_pointer_offset_value", true, true);

    Value* value = Builder.CreateAlignedLoad(stack_pointer_offset_value, 8, "stack_pointer_offset_value");

    Value* result = Builder.CreateCast(Instruction::IntToPtr, value, PointerType::get(IntegerType::get(TheContext, 32), 0));

    return result;
}

void push_value_to_stack_ptr(std::map<std::string, Value*>& params, BasicBlock* current_block, Value* value)
{
    Builder.SetInsertPoint(current_block);

    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateLoad(stack_ptr_address_value, "loaded_stack_ptr_address_value");

    Value* zero = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)0);
    store_value(zero, loaded_stack_ptr_address_value, current_block);

    store_value(value, loaded_stack_ptr_address_value, current_block);

    inc_stack_ptr(params, current_block, 1);
}

void push_value_to_stack_ptr_with_aligned(std::map<std::string, Value*>& params, BasicBlock* current_block, Value* value, int align)
{
    Builder.SetInsertPoint(current_block);

    std::string stack_ptr_address_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_name];

    Value* loaded_stack_ptr_address_value = Builder.CreateAlignedLoad(stack_ptr_address_value, align, "loaded_stack_ptr_address_value");

    Value* zero = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)0);
    store_value_with_aligned(zero, loaded_stack_ptr_address_value, current_block, 8);

    store_value_with_aligned(value, loaded_stack_ptr_address_value, current_block, align);

    inc_stack_ptr(params, current_block, 1);
}

Value* get_value_from_char_array(char* str)
{
    Constant* str_constant = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)str);
    return ConstantExpr::getIntToPtr(str_constant, PointerType::get(IntegerType::get(TheContext,8), 0));
}

void run_entry_exception_object_with_class_name2(std::map<std::string, Value *> params, int var_num, sVMInfo* info, char* class_name, char* message)
{
    Function* entry_exception_object_fun = TheModule->getFunction("entry_exception_object_with_class_name2");

    std::vector<Value*> params2;

    std::string stack_ptr_address_name("stack_ptr_address");
    Value* param1 = params[stack_ptr_address_name];
    params2.push_back(param1);

    std::string stack_value_name("stack");
    Value* param2 = params[stack_value_name];
    params2.push_back(param2);

    Value* param3 = ConstantInt::get(Type::getInt32Ty(TheContext), (uint32_t)var_num);
    params2.push_back(param3);

    Value* param4 = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)info);
    params2.push_back(param4);

    Value* param5 = get_value_from_char_array(class_name);
    params2.push_back(param5);

    Value* param6 = get_value_from_char_array(message);
    params2.push_back(param6);

    (void)Builder.CreateCall(entry_exception_object_fun, params2);
}

void if_value_is_zero_entry_exception_object(Value* value, std::map<std::string, Value *> params, int var_num, sVMInfo* info, Function* function, BasicBlock** current_block, char* class_name, char* message)
{
    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Value* comp = Builder.CreateICmpEQ(value, ConstantInt::get(TheContext, llvm::APInt(32, 0, true)), "ifcond");

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    run_entry_exception_object_with_class_name2(params, var_num, info, class_name, message);

    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend);
    *current_block = entry_ifend;
}

void if_value_is_zero_ret_zero(Value* value, std::map<std::string, Value *> params, int var_num, sVMInfo* info, Function* function, BasicBlock** current_block)
{
    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Value* comp = Builder.CreateICmpEQ(value, ConstantInt::get(TheContext, llvm::APInt(32, 0, true)), "ifcond");

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend);
    *current_block = entry_ifend;
}

CLObject get_string_object_of_object_name(CLObject object)
{
    sCLObject* object_data = CLOBJECT(object);

    CLObject object2 = create_string_object(CLASS_NAME(object_data->mClass));

    return object2;
}

////////////////////////////////////////////////////////////
// LLVM invoking method
////////////////////////////////////////////////////////////
void finish_method_call(Value* result, sCLClass* klass, sCLMethod* method, sVMInfo* info, std::map<std::string, Value *> params, BasicBlock** current_block, Function* function, char* try_catch_label_name)
{
    // if result is FALSE ret 0
    Value* comp = Builder.CreateICmpNE(result, ConstantInt::get(TheContext, llvm::APInt(32, 1, true)), "ifcond");

    BasicBlock* then_block = BasicBlock::Create(TheContext, "then_block", function);
    BasicBlock* entry_ifend = BasicBlock::Create(TheContext, "entry_ifend", function);

    Builder.CreateCondBr(comp, then_block, entry_ifend);

    Builder.SetInsertPoint(then_block);

    Function* try_catch_label_name_fun = TheModule->getFunction("get_try_catch_label_name");

    std::vector<Value*> params2;

    Value* vminfo_value = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)info);
    params2.push_back(vminfo_value);

    Value* try_catch_label_name_value = Builder.CreateCall(try_catch_label_name_fun, params2);

    Value* comp2 = Builder.CreateICmpNE(try_catch_label_name_value, ConstantInt::get(TheContext, llvm::APInt(32, 0, false)), "catchcond");

    BasicBlock* then_block2 = BasicBlock::Create(TheContext, "then_block_b", function);
    BasicBlock* entry_ifend2 = BasicBlock::Create(TheContext, "entry_ifend_b", function);

    Builder.CreateCondBr(comp2, then_block2, entry_ifend2);

    Builder.SetInsertPoint(then_block2);

    if(try_catch_label_name == nullptr) {
        Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
        Builder.CreateRet(ret_value);
    }
    else {
        std::string try_catch_label_name_string(try_catch_label_name);
        BasicBlock* label = TheLabels[try_catch_label_name_string];
        if(label == nullptr) {
            label = BasicBlock::Create(TheContext, try_catch_label_name, function);
            TheLabels[try_catch_label_name_string] = label;
        }

        if(label == nullptr) {
            Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
            Builder.CreateRet(ret_value);
        }
        else {
            Builder.CreateBr(label);
        }
    }

    Builder.SetInsertPoint(entry_ifend2);
    
    Value* ret_value = ConstantInt::get(TheContext, llvm::APInt(32, 0, true));
    Builder.CreateRet(ret_value);

    Builder.SetInsertPoint(entry_ifend); 
    *current_block = entry_ifend;
}

BOOL compile_invoking_method(sCLClass* klass, sCLMethod* method, CLVALUE* stack, int var_num, CLVALUE** stack_ptr, sVMInfo* info, std::map<std::string, Value *> params, BasicBlock** current_block, Function* function, char* try_catch_label_name)
{
    Function* llvm_function = TheModule->getFunction("invoke_method");

    MASSERT(llvm_function != null_ptr);

    std::vector<Value*> params2;

    Value* klass_value = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)klass);
    params2.push_back(klass_value);

    Value* method_value = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)method);
    params2.push_back(method_value);

    std::string stack_value_name("stack");
    Value* stack_value = params[stack_value_name];
    params2.push_back(stack_value);

    Value* var_num_value = ConstantInt::get(Type::getInt32Ty(TheContext), (uint64_t)var_num);
    params2.push_back(var_num_value);

    std::string stack_ptr_address_value_name("stack_ptr_address");
    Value* stack_ptr_address_value = params[stack_ptr_address_value_name];
    params2.push_back(stack_ptr_address_value);

    Value* vminfo_value = ConstantInt::get(Type::getInt64Ty(TheContext), (uint64_t)info);
    params2.push_back(vminfo_value);

    Value* result = Builder.CreateCall(llvm_function, params2);

    finish_method_call(result, klass, method, info, params, current_block, function, try_catch_label_name);

    return TRUE;
}

BOOL invoke_virtual_method(int num_real_params, int offset, CLVALUE* stack, int var_num, CLVALUE** stack_ptr, sVMInfo* info, sByteCode* code, sConst* constant)
{
    CLObject object = ((*stack_ptr)-num_real_params)->mObjectValue;

    sCLObject* object_data = CLOBJECT(object);

    sCLClass* klass = object_data->mClass;

    MASSERT(klass != NULL);

    char* method_name_and_params = CONS_str(constant, offset);

    sCLMethod* method = search_for_method_from_virtual_method_table(klass, method_name_and_params);

    if(method == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "OP_INVOKE_VIRTUAL_METHOD: Method not found");
        return FALSE;
    }
    else {
        if(!invoke_method(klass, method, stack, var_num, stack_ptr, info)) {
            if(*info->try_offset != 0) {
                *info->pc = code->mCodes + *info->try_offset;
                *info->try_offset = *info->try_offset_before;
            }
            else {
                return FALSE;
            }
        }
    }

    return TRUE;
}

BOOL invoke_dynamic_method(int offset, int offset2, int num_params, int static_, int num_method_chains, int max_method_chains, CLVALUE* stack, int var_num, CLVALUE** stack_ptr, sVMInfo* info, sByteCode* code, sConst* constant)
{
    /// none static method ////
    if(static_ == 0) {
        int num_real_params = num_params + 1;
        char* method_name = CONS_str(constant, offset2);

        CLObject object = ((*stack_ptr)-num_real_params)->mObjectValue;

        sCLObject* object_data = CLOBJECT(object);

        sCLClass* klass = object_data->mClass;

        MASSERT(klass != NULL);

        if(klass->mCallingMethodIndex == -1) {
            entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "OP_INVOKE_DYNAMIC_METHOD: Method not found(1)");
            return FALSE;
        }

        sCLMethod* method = klass->mMethods + klass->mCallingMethodIndex;

        CLObject elements[ARRAY_VALUE_ELEMENT_MAX];

        int i;
        for(i=0; i<num_params; i++) {
            CLObject object = ((*stack_ptr)-num_params + i)->mObjectValue;

            elements[i] = object;
        }

        CLObject carray = create_carray_object_with_elements(num_params, elements);

        gGlobalStackPtr->mObjectValue = carray;
        gGlobalStackPtr++;

        (*stack_ptr)-=num_params;

        (*stack_ptr)->mObjectValue = create_string_object(method_name);
        (*stack_ptr)++;
        (*stack_ptr)->mObjectValue = carray;
        (*stack_ptr)++;
        (*stack_ptr)->mIntValue = num_method_chains;
        (*stack_ptr)++;
        (*stack_ptr)->mIntValue = max_method_chains;
        (*stack_ptr)++;

        gGlobalStackPtr--;

        if(!invoke_method(klass, method, stack, var_num, stack_ptr, info)) {
            if(*info->try_offset != 0) {
                *info->pc = code->mCodes + *info->try_offset;
                *info->try_offset = *info->try_offset_before;
            }
            else {
                return FALSE;
            }
        }
    }
    /// static method ///
    else {
        char* class_name = CONS_str(constant, offset);
        char* method_name = CONS_str(constant, offset2);

        sCLClass* klass = get_class_with_load_and_initialize(class_name);

        if(klass == NULL) {
            entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(3)");
            return FALSE;
        }

        if(klass->mCallingClassMethodIndex == -1) {
            entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "OP_INVOKE_DYNAMIC_METHOD: Method not found(2)");
            return FALSE;
        }

        sCLMethod* method = klass->mMethods + klass->mCallingClassMethodIndex;

        CLObject elements[ARRAY_VALUE_ELEMENT_MAX];

        int i;
        for(i=0; i<num_params; i++) {
            CLObject object = ((*stack_ptr)-num_params + i)->mObjectValue;

            elements[i] = object;
        }

        CLObject carray = create_carray_object_with_elements(num_params, elements);

        gGlobalStackPtr->mObjectValue = carray;
        gGlobalStackPtr++;

        (*stack_ptr)-=num_params;

        (*stack_ptr)->mObjectValue = create_string_object(method_name);
        (*stack_ptr)++;
        (*stack_ptr)->mObjectValue = carray;
        (*stack_ptr)++;
        (*stack_ptr)->mIntValue = num_method_chains;
        (*stack_ptr)++;
        (*stack_ptr)->mIntValue = max_method_chains;
        (*stack_ptr)++;

        gGlobalStackPtr--;

        if(!invoke_method(klass, method, stack, var_num, stack_ptr, info)) {
            if(*info->try_offset != 0) {
                *info->pc = code->mCodes + *info->try_offset;
                *info->try_offset = *info->try_offset_before;
            }
            else {
                return FALSE;
            }
        }
    }

    return TRUE;
}

BOOL invoke_block_in_jit(int num_params, CLVALUE* stack, int var_num, CLVALUE** stack_ptr, sVMInfo* info)
{
    CLObject block_object = ((*stack_ptr)-num_params-1)->mObjectValue;

    if(!invoke_block(block_object, stack, var_num, num_params, stack_ptr, info)) 
    {
        return FALSE;
    }

    CLVALUE result = *((*stack_ptr)-1);

    (*stack_ptr) -= num_params+1+1;

    **stack_ptr = result;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_load_field_address(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int field_index)
{
    CLObject obj = ((*stack_ptr) -1)->mObjectValue;
    (*stack_ptr)--;

    if(obj == 0) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "Null pointer exception(4)");
        return FALSE;
    }

    sCLObject* object_pointer = CLOBJECT(obj);
    sCLClass* klass = object_pointer->mClass;

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(5)");
        return FALSE;
    }

    if(field_index < 0 || field_index >= klass->mNumFields) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "field index is invalid");
        return FALSE;
    }

    char* value = (char*)&object_pointer->mFields[field_index];
    (*stack_ptr)->mPointerValue = value;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_store_field(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int field_index)
{
    CLObject obj = ((*stack_ptr) -2)->mObjectValue;
    CLVALUE value = *(*(stack_ptr)-1);

    if(obj == 0) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "Null pointer exception(5)");
        return FALSE;
    }

    sCLObject* object_pointer = CLOBJECT(obj);
    sCLClass* klass = object_pointer->mClass;


    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(6)");
        return FALSE;
    }

    if(field_index < 0 || field_index >= klass->mNumFields) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "field index is invalid");
        return FALSE;
    }

    object_pointer->mFields[field_index] = value;
    (*stack_ptr)-=2;
    **stack_ptr = value;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_load_class_field(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int field_index, int offset, sConst* constant)
{
    char* class_name = CONS_str(constant, offset);

    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(7)");
        return FALSE;
    }

    if(field_index < 0 || field_index >= klass->mNumClassFields) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "field index is invalid");
        return FALSE;
    }

    sCLField* field = klass->mClassFields + field_index;

    **stack_ptr = field->mValue;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_load_class_field_address(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int field_index, int offset, sConst* constant)
{
    char* class_name = CONS_str(constant, offset);

    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(8)");
        return FALSE;
    }

    if(field_index < 0 || field_index >= klass->mNumClassFields) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "field index is invalid");
        return FALSE;
    }

    sCLField* field = klass->mClassFields + field_index;
    char* value = (char*)&field->mValue;

    (*stack_ptr)->mPointerValue = value;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_store_class_field(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int field_index, int offset, sConst* constant)
{
    char* class_name = CONS_str(constant, offset);

    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(9)");
        return FALSE;
    }

    if(field_index < 0 || field_index >= klass->mNumClassFields) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "field index is invalid");
        return FALSE;
    }

    CLVALUE value = *((*stack_ptr)-1);

    sCLField* field = klass->mClassFields + field_index;
    field->mValue = value;

    return TRUE;
}

BOOL run_load_element(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info)
{
    CLObject array = ((*stack_ptr) -2)->mObjectValue;
    int element_num = ((*stack_ptr) -1)->mIntValue;
    (*stack_ptr)-=2;

    if(array == 0) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "Null pointer exception(7)");
        return FALSE;
    }

    sCLObject* object_pointer = CLOBJECT(array);

    if(element_num < 0 || element_num >= object_pointer->mArrayNum) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "element index is invalid");
        return FALSE;
    }

    CLVALUE value = object_pointer->mFields[element_num];
    **stack_ptr = value;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_store_element(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info)
{
    CLObject array = ((*stack_ptr) -3)->mObjectValue;
    int element_num = ((*stack_ptr) -2)->mIntValue;
    CLVALUE value = *((*stack_ptr)-1);

    if(array == 0) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "Null pointer exception(8)");
        return FALSE;
    }

    sCLObject* object_pointer = CLOBJECT(array);

    if(element_num < 0 || element_num >= object_pointer->mArrayNum) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "element index is invalid");
        return FALSE;
    }

    object_pointer->mFields[element_num] = value;

    (*stack_ptr)-=3;

    **stack_ptr = value;
    (*stack_ptr)++;

    return TRUE;
}

void run_get_array_length(CLVALUE** stack_ptr)
{
    CLObject array_ = ((*stack_ptr)-1)->mObjectValue;
    sCLObject* array_data = CLOBJECT(array_);
    (*stack_ptr)--;

    (*stack_ptr)->mIntValue = array_data->mArrayNum;
    (*stack_ptr)++;
}

void run_get_regex_global(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mGlobal;
    (*stack_ptr)++;
}

void run_get_regex_ignorecase(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mIgnoreCase;
    (*stack_ptr)++;
}

void run_get_regex_multiline(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mMultiline;
    (*stack_ptr)++;
}

void run_get_regex_extended(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mExtended;
    (*stack_ptr)++;
}

void run_get_regex_dotall(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mDotAll;
    (*stack_ptr)++;
}

void run_get_regex_anchored(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mAnchored;
    (*stack_ptr)++;
}

void run_get_regex_dollar_endonly(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mDollarEndOnly;
    (*stack_ptr)++;
}

void run_get_regex_ungreedy(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;
    sRegexObject* regex_object = CLREGEX(regex);
    (*stack_ptr)--;

    (*stack_ptr)->mBoolValue = regex_object->mUngreedy;
    (*stack_ptr)++;
}

void run_char_uppercase(CLVALUE** stack_ptr)
{
    wchar_t c = ((*stack_ptr)-1)->mCharValue;

    wchar_t result = c;
    if(c >= 'a' && c <= 'z') {
        result = c - 'a' + 'A';
    }

    ((*stack_ptr)-1)->mCharValue = result;
}

void run_char_lowercase(CLVALUE** stack_ptr)
{
    wchar_t c = ((*stack_ptr)-1)->mCharValue;

    wchar_t result = c;
    if(c >= 'A' && c <= 'Z') {
        result = c - 'A' + 'a';
    }

    ((*stack_ptr)-1)->mCharValue = result;
}

BOOL run_create_array(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, char* class_name, int num_elements)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(11)");
        return FALSE;
    }

    CLObject array_object = create_array_object(klass, num_elements);
    (*stack_ptr)->mObjectValue = array_object; // push object
    (*stack_ptr)++;

    sCLObject* object_data = CLOBJECT(array_object);

    int i;
    for(i=0; i<num_elements; i++) {
        object_data->mFields[i] = *((*stack_ptr)-1-num_elements+i);
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = array_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_carray(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(12)");
        return FALSE;
    }

    CLObject array_object = create_carray_object();
    (*stack_ptr)->mObjectValue = array_object; // push object
    (*stack_ptr)++;

    CLObject items[ARRAY_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_carray_object(array_object, num_elements, items, stack, var_num, stack_ptr, info, klass))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = array_object;
    (*stack_ptr)++;

    return TRUE;
}


BOOL run_create_equalable_carray(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(12)");
        return FALSE;
    }

    CLObject array_object = create_equalable_carray_object();
    (*stack_ptr)->mObjectValue = array_object; // push object
    (*stack_ptr)++;

    CLObject items[ARRAY_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_equalable_carray_object(array_object, num_elements, items, stack, var_num, stack_ptr, info, klass))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = array_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_sortable_carray(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(12)");
        return FALSE;
    }

    CLObject array_object = create_sortable_carray_object();
    (*stack_ptr)->mObjectValue = array_object; // push object
    (*stack_ptr)++;

    CLObject items[ARRAY_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_sortable_carray_object(array_object, num_elements, items, stack, var_num, stack_ptr, info, klass))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = array_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_list(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(13)");
        return FALSE;
    }

    CLObject list_object = create_list_object();
    (*stack_ptr)->mObjectValue = list_object; // push object
    (*stack_ptr)++;

    CLObject items[LIST_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_list_object(list_object, num_elements, items, stack, var_num, stack_ptr, info, klass))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = list_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_sortable_list(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(13)");
        return FALSE;
    }

    CLObject list_object = create_sortable_list_object();
    (*stack_ptr)->mObjectValue = list_object; // push object
    (*stack_ptr)++;

    CLObject items[LIST_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_sortable_list_object(list_object, num_elements, items, stack, var_num, stack_ptr, info, klass))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = list_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_equalable_list(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(13)");
        return FALSE;
    }

    CLObject list_object = create_equalable_list_object();
    (*stack_ptr)->mObjectValue = list_object; // push object
    (*stack_ptr)++;

    CLObject items[LIST_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_equalable_list_object(list_object, num_elements, items, stack, var_num, stack_ptr, info, klass))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = list_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_tuple(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements)
{
    CLObject tuple_object = create_tuple_object(num_elements);

    (*stack_ptr)->mObjectValue = tuple_object; // push object
    (*stack_ptr)++;

    CLObject items[TUPLE_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        CLVALUE element = *((*stack_ptr)-1-num_elements+i);
        items[i] = (*((*stack_ptr)-1-num_elements+i)).mObjectValue;
    }

    if(!initialize_tuple_object(tuple_object, num_elements, items, stack, var_num, stack_ptr, info))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements;
    (*stack_ptr)->mObjectValue = tuple_object;
    (*stack_ptr)++;

    return TRUE;
}

BOOL run_create_hash(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, int num_elements, char* class_name, char* class_name2)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(14)");
        return FALSE;
    }

    sCLClass* klass2 = get_class_with_load_and_initialize(class_name2);

    if(klass2 == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(15)");
        return FALSE;
    }

    CLObject keys[HASH_VALUE_ELEMENT_MAX];

    int i;
    for(i=0; i<num_elements; i++) {
        keys[i] = ((*stack_ptr) - num_elements * 2 + i * 2)->mObjectValue;
    }

    CLObject items[HASH_VALUE_ELEMENT_MAX];

    for(i=0; i<num_elements; i++) {
        items[i] = ((*stack_ptr) - num_elements * 2 + i * 2 + 1)->mObjectValue;
    }

    CLObject hash_object = create_hash_object();
    (*stack_ptr)->mObjectValue = hash_object; // push object
    (*stack_ptr)++;

    if(!initialize_hash_object(hash_object, num_elements, keys, items, stack, var_num, stack_ptr, info, klass, klass2))
    {
        return FALSE;
    }

    (*stack_ptr)--; // pop_object

    (*stack_ptr)-=num_elements*2;
    (*stack_ptr)->mObjectValue = hash_object;
    (*stack_ptr)++;

    return TRUE;
}

void run_create_block_object(CLVALUE** stack_ptr, CLVALUE* stack, sConst* constant, int code_offset, int code_len, int constant_offset, int constant_len, int block_var_num, int parent_var_num, BOOL lambda, sVMInfo* info)

{
    sByteCode codes2;
    codes2.mCodes = CONS_str(constant, code_offset);
    codes2.mLen = code_len;

    sConst constant2;
    constant2.mConst = CONS_str(constant, constant_offset);
    constant2.mLen = constant_len;

    CLVALUE* parent_stack = stack;

    CLObject block_object = create_block_object(&codes2, &constant2, parent_stack, parent_var_num, block_var_num, info->stack_id, lambda);

    (*stack_ptr)->mObjectValue = block_object;
    (*stack_ptr)++;
}

void run_byte_to_string_cast(CLVALUE** stack_ptr)
{
    char value = ((*stack_ptr)-1)->mCharValue;

    char buf[32];
    snprintf(buf, 32, "%d", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_short_to_string_cast(CLVALUE** stack_ptr)
{
    short value = ((*stack_ptr)-1)->mShortValue;

    char buf[32];
    snprintf(buf, 32, "%d", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_int_to_string_cast(CLVALUE** stack_ptr)
{
    int value = ((*stack_ptr)-1)->mIntValue;

    char buf[32];
    snprintf(buf, 32, "%d", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_long_to_string_cast(CLVALUE** stack_ptr)
{
    long value = ((*stack_ptr)-1)->mLongValue;

    char buf[32];
    snprintf(buf, 32, "%ld", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_ubyte_to_string_cast(CLVALUE** stack_ptr)
{
    unsigned char value = ((*stack_ptr)-1)->mUByteValue;

    char buf[32];
    snprintf(buf, 32, "%u", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_ushort_to_string_cast(CLVALUE** stack_ptr)
{
    unsigned short value = ((*stack_ptr)-1)->mUShortValue;

    char buf[32];
    snprintf(buf, 32, "%u", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_uint_to_string_cast(CLVALUE** stack_ptr)
{
    unsigned int value = ((*stack_ptr)-1)->mUIntValue;

    char buf[32];
    snprintf(buf, 32, "%u", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_ulong_to_string_cast(CLVALUE** stack_ptr)
{
    unsigned long value = ((*stack_ptr)-1)->mULongValue;

    char buf[32];
    snprintf(buf, 32, "%lu", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_float_to_string_cast(CLVALUE** stack_ptr)
{
    float value = ((*stack_ptr)-1)->mFloatValue;

    char buf[32];
    snprintf(buf, 32, "%f", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_double_to_string_cast(CLVALUE** stack_ptr)
{
    double value = ((*stack_ptr)-1)->mDoubleValue;

    char buf[32];
    snprintf(buf, 32, "%lf", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_bool_to_string_cast(CLVALUE** stack_ptr)
{
    BOOL value = ((*stack_ptr)-1)->mBoolValue;

    char buf[32];
    if(value) {
        snprintf(buf, 32, "true");
    }
    else {
        snprintf(buf, 32, "false");
    }

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_regex_to_string_cast(CLVALUE** stack_ptr)
{
    CLObject regex = ((*stack_ptr)-1)->mObjectValue;

    sRegexObject* object_data = CLREGEX(regex);

    CLObject str = create_string_object(object_data->mRegexString);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_pointer_to_string_cast(CLVALUE** stack_ptr)
{
    char* value = ((*stack_ptr)-1)->mPointerValue;

    char buf[32];
    snprintf(buf, 32, "%p", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_char_to_string_cast(CLVALUE** stack_ptr)
{
    wchar_t value = ((*stack_ptr)-1)->mCharValue;

    char buf[32];
    snprintf(buf, 32, "%lc", value);

    CLObject str = create_string_object(buf);

    ((*stack_ptr)-1)->mObjectValue = str;
}

void run_cbyte_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cubyte_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cshort_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cushort_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_integer_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_uinteger_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_clong_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_culong_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cfloat_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cdouble_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cpointer_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)0;// obj_data->mFields[0].mPointerValue; 

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cchar_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cbool_to_byte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    char value = (char)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mByteValue = value;
}

void run_cbyte_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cubyte_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cshort_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cushort_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_integer_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_uinteger_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_clong_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_culong_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cfloat_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cdouble_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cpointer_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)0; //obj_data->mFields[0].mPointerValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cchar_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cbool_to_short_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    short value = (short)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mShortValue = value;
}

void run_cbyte_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cubyte_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cshort_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cushort_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_integer_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_uinteger_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_clong_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_culong_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mIntValue = value;

}

void run_cfloat_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cdouble_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cpointer_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)0; //obj_data->mFields[0].mPointerValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cchar_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cbool_to_int_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    int value = (int)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mIntValue = value;
}

void run_cbyte_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cubyte_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cshort_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cushort_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_integer_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_uinteger_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_clong_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_culong_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cfloat_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cdouble_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cpointer_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mPointerValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cchar_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cbool_to_long_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    long value = (long)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mLongValue = value;
}

void run_cbyte_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cubyte_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cshort_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cushort_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_integer_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_uinteger_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_clong_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_culong_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cfloat_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cdouble_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cpointer_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)0;// obj_data->mFields[0].mPointerValue; 

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cchar_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cbool_to_ubyte_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned char value = (unsigned char)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mUByteValue = value;
}

void run_cbyte_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cubyte_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cshort_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cushort_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_integer_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_uinteger_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_clong_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_culong_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cfloat_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cdouble_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cpointer_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)0; //obj_data->mFields[0].mPointerValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cchar_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cbool_to_ushort_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned short value = (unsigned short)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mUShortValue = value;
}

void run_cbyte_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cubyte_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cshort_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cushort_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_integer_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_uinteger_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_clong_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_culong_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mUIntValue = value;

}

void run_cfloat_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cdouble_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cpointer_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)0; //obj_data->mFields[0].mPointerValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cchar_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cbool_to_uint_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned int value = (unsigned int)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mUIntValue = value;
}

void run_cbyte_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cubyte_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cshort_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cushort_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_integer_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_uinteger_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_clong_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_culong_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cfloat_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cdouble_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cpointer_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mPointerValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cchar_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cbool_to_ulong_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    unsigned long value = (unsigned long)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mULongValue = value;
}

void run_cbyte_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cubyte_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cshort_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cushort_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_integer_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_uinteger_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_clong_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_culong_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cfloat_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mFloatValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cdouble_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cchar_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cbool_to_float_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    float value = (float)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mFloatValue = value;
}

void run_cbyte_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mByteValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cubyte_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mUByteValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cshort_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mShortValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cushort_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mUShortValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_integer_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mIntValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_uinteger_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mUIntValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_clong_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mLongValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_culong_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mULongValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cfloat_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cdouble_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mDoubleValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cchar_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mCharValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

void run_cbool_to_double_cast(CLVALUE** stack_ptr)
{
    CLObject obj = ((*stack_ptr)-1)->mObjectValue;

    sCLObject* obj_data = CLOBJECT(obj);

    double value = (double)obj_data->mFields[0].mBoolValue;

    ((*stack_ptr)-1)->mDoubleValue = value;
}

BOOL run_array_to_carray_cast(CLVALUE** stack_ptr, CLVALUE* stack, int var_num, sVMInfo* info, char* class_name)
{
    sCLClass* klass = get_class_with_load_and_initialize(class_name);

    if(klass == NULL) {
        entry_exception_object_with_class_name(stack_ptr, stack, var_num, info, "Exception", "class not found(10)");
        return FALSE;
    }

    CLObject array = ((*stack_ptr)-1)->mObjectValue;
    sCLObject* array_data = CLOBJECT(array);
    int array_num = array_data->mArrayNum;

    sCLClass* klass2 = get_class("Array");
    MASSERT(klass2 != NULL);

    CLObject new_array = create_object(klass2);

    (*stack_ptr)->mObjectValue = new_array;   // push object
    (*stack_ptr)++;

    CLObject new_primitive_array;
    if(klass->mFlags & CLASS_FLAGS_PRIMITIVE) {
        new_primitive_array = create_array_object(klass->mBoxingClass, array_num);
    }
    else {
        new_primitive_array = create_array_object(klass, array_num);
    }

    sCLObject* new_array_data = CLOBJECT(new_array);

    new_array_data->mFields[0].mObjectValue = new_primitive_array;

    /// boxing element ///
    int i;
    for(i=0; i<array_num; i++ ) {
        array_data = CLOBJECT(array);           // reget for GC

        CLVALUE element;
        boxing_primitive_value_to_object(array_data->mFields[i], &element, klass);

        sCLObject* new_primitive_array_data = CLOBJECT(new_primitive_array);
        new_primitive_array_data->mFields[i] = element;
    }

    (*stack_ptr)-=2;
    (*stack_ptr)->mObjectValue = new_array;
    (*stack_ptr)++;

    return TRUE;
}

} // extern C