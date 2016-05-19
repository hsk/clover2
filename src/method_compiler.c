#include "common.h"

BOOL compile_method(sCLMethod* method, sParserParam* params, int num_params, sParserInfo* info, sCompileInfo* cinfo)
{
    info->lv_table = init_var_table();

    /// self ///
    if(!(method->mFlags & METHOD_FLAGS_CLASS_METHOD)) {
        sNodeType* node_type = alloc_node_type();
        node_type->mClass = info->klass;
        node_type->mNumGenericsTypes = 0;

        if(!add_variable_to_table(info->lv_table, "self", node_type)) {
            parser_err_msg(info, "overflow the table or a variable which has the same name exists");
            return FALSE;
        }
    }

    /// add params to lv_table ///
    int i;
    for(i=0; i<num_params; i++) {
        sParserParam* param = params + i;
        if(!add_variable_to_table(info->lv_table, param->mName, param->mType)) {
            parser_err_msg(info, "overflow the table or a variable which has the same name exists");
            return FALSE;
        }
    }

    sCompileInfo cinfo2;

    sByteCode code;
    sByteCode_init(&code);

    cinfo2.code = &code;
    cinfo2.constant = &info->klass->mConst;
    cinfo2.stack_num = 0;
    cinfo2.lv_table = info->lv_table;
    cinfo2.no_output = FALSE;
    cinfo2.err_num = 0;
    cinfo2.pinfo = info;
    cinfo2.type = NULL;
    cinfo2.num_break_points = NULL;
    cinfo2.break_points = NULL;
    cinfo2.method = method;

    while(1) {
        if(*info->p == '}') {
            info->p++;
            skip_spaces_and_lf(info);
            break;
        }
        else if(*info->p == '\0') {
            parser_err_msg(info, "The block requires } character for closing block");
            info->err_num++;
            sByteCode_free(cinfo2.code);
            return TRUE;
        }
        else {
            unsigned int node = 0;
            if(!expression(&node, info)) {
                sByteCode_free(cinfo2.code);
                return FALSE;
            }

            if(info->err_num == 0) {
                if(!compile(node, &cinfo2)) {
                    sByteCode_free(cinfo2.code);
                    return FALSE;
                }

                arrange_stack(&cinfo2);
            }

            if(*info->p == ';') {
                info->p++;
                skip_spaces_and_lf(info);
            }
        }
    }

    int var_num = get_var_num(cinfo2.lv_table);
    add_code_to_method(method, cinfo2.code, var_num);
    cinfo->err_num += cinfo2.err_num;

    return TRUE;
}
