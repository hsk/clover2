#include "common.h"

static sNodeBlock* sNodeBlock_alloc()
{
    sNodeBlock* block = MCALLOC(1, sizeof(sNodeBlock));

    block->mSizeNodes = 32;
    block->mNumNodes = 0;
    block->mNodes = MCALLOC(1, sizeof(unsigned int)*block->mSizeNodes);
    block->mLVTable = NULL;
    block->mUnClosedBlock = FALSE;
    sBuf_init(&block->mSource);

    return block;
}

void sNodeBlock_free(sNodeBlock* block)
{
    if(block->mNodes) MFREE(block->mNodes);
    MFREE(block->mSource.mBuf);
    MFREE(block);
}

sNodeBlock* sNodeBlock_clone(sNodeBlock* block)
{
    sNodeBlock* result = sNodeBlock_alloc();

    result->mNumNodes = block->mNumNodes;
    result->mSizeNodes = block->mSizeNodes;
    result->mNodes = MCALLOC(1, sizeof(unsigned int)*block->mSizeNodes);

    int i;
    for(i=0; i<block->mNumNodes; i++) {
        result->mNodes[i] = block->mNodes[i];
    }

    result->mLVTable = clone_var_table(block->mLVTable);

    result->mUnClosedBlock = block->mUnClosedBlock;

    sBuf_clone(&result->mSource, &block->mSource);

    result->mSName = block->mSName;
    result->mSLine = block->mSLine;

    return result;
}

static void append_node_to_node_block(sNodeBlock* node_block, unsigned int node)
{
    if(node_block->mSizeNodes <= node_block->mNumNodes) {
        int new_size = node_block->mSizeNodes * 2;
        node_block->mNodes = MREALLOC(node_block->mNodes, sizeof(unsigned int)*new_size);
        memset(node_block->mNodes + node_block->mSizeNodes, 0, sizeof(unsigned int)*(new_size-node_block->mSizeNodes));
    }

    node_block->mNodes[node_block->mNumNodes] = node;
    node_block->mNumNodes++;
}

BOOL parse_block(ALLOC sNodeBlock** node_block, sParserInfo* info, sVarTable* new_table, BOOL block_object)
{
    //expect_next_character_with_one_forward("{", info);

    *node_block = sNodeBlock_alloc();

    sVarTable* old_vtable = info->lv_table;
    if(new_table) {
        info->lv_table = new_table;
    }
    else {
        info->lv_table = init_block_vtable(old_vtable);
    }

    (*node_block)->mSName = info->sname;
    (*node_block)->mSLine = info->sline;

    char* source_head = info->p;

    while(1) {
        if(*info->p == '}') {
            info->p++;
            skip_spaces_and_lf(info);
            break;
        }
        else if(*info->p == '\0') {
            (*node_block)->mUnClosedBlock = TRUE;

            if(!block_object) {
                set_max_block_var_num(info->lv_table, old_vtable);
            }
            (*node_block)->mLVTable = info->lv_table;

            //info->lv_table = old_vtable;   // for interpreter completion

            char* source_end = info->p;

            sBuf_append(&(*node_block)->mSource, source_head, source_end - source_head);
            sBuf_append_char(&(*node_block)->mSource, '\0');

            return TRUE;
        }

        unsigned int node = 0;

        if(!expression(&node, info)) {
            sNodeBlock_free(*node_block);

            info->lv_table = old_vtable;
            return FALSE;
        }

        if(node == 0) {
            parser_err_msg(info, "require an expression");
            info->err_num++;
            break;
        }

        append_node_to_node_block(*node_block, node);

        if(*info->p == ';') {
            info->p++;
            skip_spaces_and_lf(info);
        }

        if(*info->p == '}') {
            info->p++;
            skip_spaces_and_lf(info);
            break;
        }
        else if(*info->p == '\0') {
            parser_err_msg(info, "require } before the source end");
            info->err_num++;

            (*node_block)->mUnClosedBlock = TRUE;

            if(!block_object) {
                set_max_block_var_num(info->lv_table, old_vtable);
            }
            (*node_block)->mLVTable = info->lv_table;

            //info->lv_table = old_vtable;   // for interpreter completion

            char* source_end = info->p;

            sBuf_append(&(*node_block)->mSource, source_head, source_end - source_head);
            sBuf_append_char(&(*node_block)->mSource, '\0');

            return TRUE;
        }
    }

    char* source_end = info->p;

    sBuf_append(&(*node_block)->mSource, source_head, source_end - source_head);
    sBuf_append_char(&(*node_block)->mSource, '\0');

    if(!block_object) {
        set_max_block_var_num(info->lv_table, old_vtable);
    }
    (*node_block)->mLVTable = info->lv_table;
    info->lv_table = old_vtable;

    return TRUE;
}

BOOL compile_block(sNodeBlock* block, sCompileInfo* info)
{
    sVarTable* old_table = info->lv_table;
    info->lv_table = block->mLVTable;

    int stack_num_before = info->stack_num;
    info->stack_num = 0;

    int i;
    for(i=0; i<block->mNumNodes; i++) {
        unsigned int node = block->mNodes[i];

        info->sname = gNodes[node].mSName;
        info->sline = gNodes[node].mLine;

        append_opecode_to_code(info->code, OP_HEAD_OF_EXPRESSION, info->no_output);

        int stack_num = info->stack_num;

        if(!compile(node, info)) {
            info->lv_table = old_table;
            info->stack_num = stack_num_before;
            return FALSE;
        }

        if(info->result_type_boxing) {
            boxing_to_lapper_class(&info->type, info);
        }

        arrange_stack(info);

        info->block_last_type = info->type;

#ifdef ENABLE_INTERPRETER
        append_opecode_to_code(info->code, OP_SIGINT, info->no_output);
#endif
    }

    info->stack_num = stack_num_before;

    info->lv_table = old_table;

    return TRUE;
}

BOOL compile_block_with_result(sNodeBlock* block, sCompileInfo* info)
{
    sVarTable* old_table = info->lv_table;
    info->lv_table = block->mLVTable;

    int stack_num_before = info->stack_num;
    info->stack_num = 0;

    int i;
    for(i=0; i<block->mNumNodes; i++) {
        unsigned int node = block->mNodes[i];

        info->sname = gNodes[node].mSName;
        info->sline = gNodes[node].mLine;

        append_opecode_to_code(info->code, OP_HEAD_OF_EXPRESSION, info->no_output);

        if(!compile(node, info)) {
            info->lv_table = old_table;
            info->stack_num = stack_num_before;
            return FALSE;
        }

        if(i == block->mNumNodes-1) {
            if(info->stack_num == 0) {
                append_opecode_to_code(info->code, OP_LDCNULL, info->no_output);
                info->stack_num++;

                info->type = create_node_type_with_class_name("Null");
            }
            else if(info->stack_num < 0) {
                compile_err_msg(info, "Unexpected error. Stack pointer is invalid(stack number is %d)", info->stack_num);
                info->err_num++;
            }
            else if(info->stack_num == 1) {
            }
            else {
                int i;
                for(i=0; i<info->stack_num-1; i++) {
                    append_opecode_to_code(info->code, OP_REVERSE, info->no_output);
                    append_opecode_to_code(info->code, OP_POP, info->no_output);
                    info->stack_num--;
                }
            }
        }
        else {
            arrange_stack(info);
        }

#ifdef ENABLE_INTERPRETER
        append_opecode_to_code(info->code, OP_SIGINT, info->no_output);
#endif
    }

    info->stack_num = stack_num_before;
    info->stack_num++;
    info->lv_table = old_table;

    return TRUE;
}


