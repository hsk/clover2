#include "common.h"

static sNodeType** gNodeTypes = NULL;
static int gUsedPageNodeTypes = 0;
static int gSizePageNodeTypes = 0;
static int gUsedNodeTypes = 0;

#define NODE_TYPE_PAGE_SIZE 64

void init_node_types()
{
    const int size_page_node_types = 4;

    if(gSizePageNodeTypes == 0) {
        gNodeTypes = MCALLOC(1, sizeof(sNodeType*)*size_page_node_types);

        int i;
        for(i=0; i<size_page_node_types; i++) {
            gNodeTypes[i] = MCALLOC(1, sizeof(sNodeType)*NODE_TYPE_PAGE_SIZE);
        }

        gSizePageNodeTypes = size_page_node_types;
        gUsedPageNodeTypes = 0;
        gUsedNodeTypes = 0;
    }
}

void free_node_types()
{
    if(gSizePageNodeTypes > 0) {
        int i;
        for(i=0; i<gSizePageNodeTypes; i++) {
            MFREE(gNodeTypes[i]);
        }
        MFREE(gNodeTypes);

        gSizePageNodeTypes = 0;
        gUsedPageNodeTypes = 0;
        gUsedNodeTypes = 0;
    }
}

sNodeType* alloc_node_type()
{
    MASSERT(gNodeTypes != NULL && gSizePageNodeTypes > 0); // Is the node types initialized ?

    if(gUsedNodeTypes == NODE_TYPE_PAGE_SIZE) {
        gUsedNodeTypes = 0;
        gUsedPageNodeTypes++;

        if(gUsedPageNodeTypes == gSizePageNodeTypes) {
            int new_size = (gSizePageNodeTypes+1) * 2;
            gNodeTypes = MREALLOC(gNodeTypes, sizeof(sNodeType*)*new_size);
            memset(gNodeTypes + gSizePageNodeTypes, 0, sizeof(sNodeType*)*(new_size - gSizePageNodeTypes));

            int i;
            for(i=gSizePageNodeTypes; i<new_size; i++) {
                gNodeTypes[i] = MCALLOC(1, sizeof(sNodeType)*NODE_TYPE_PAGE_SIZE);
            }

            gSizePageNodeTypes = new_size;
        }
    }

    return &gNodeTypes[gUsedPageNodeTypes][gUsedNodeTypes++];
}

sNodeType* clone_node_type(sNodeType* node_type)
{
    sNodeType* node_type2 = alloc_node_type();

    node_type2->mClass = node_type->mClass;
    node_type2->mNumGenericsTypes = node_type->mNumGenericsTypes;

    int i;
    for(i=0; i<node_type->mNumGenericsTypes; i++) {
        node_type2->mGenericsTypes[i] = ALLOC clone_node_type(node_type->mGenericsTypes[i]);
    }

    node_type2->mArray = node_type->mArray;

    if(node_type->mBlockType) {
        node_type2->mBlockType = clone_node_block_type(node_type->mBlockType);
    }
    else {
        node_type2->mBlockType = NULL;
    }

    return node_type2;
}

sNodeType* create_node_type_with_class_pointer(sCLClass* klass)
{
    sNodeType* node_type = alloc_node_type();

    node_type->mClass = klass;
    node_type->mNumGenericsTypes = 0;
    node_type->mArray = FALSE;
    node_type->mBlockType = NULL;

    return node_type;
}

sNodeType* create_node_type_with_class_name(char* class_name)
{
    sNodeType* node_type = alloc_node_type();

    node_type->mClass = get_class_with_load(class_name);

    if(node_type->mClass == NULL) {
        node_type->mClass = load_class(class_name);
    }

    node_type->mNumGenericsTypes = 0;

    node_type->mArray = FALSE;
    node_type->mBlockType = NULL;

    return node_type;
}

sNodeType* create_node_type_with_generics_number(int generics_num)
{
    char class_name[CLASS_NAME_MAX+1];

    snprintf(class_name, CLASS_NAME_MAX, "GenericsParametorClass%d", generics_num);

    return create_node_type_with_class_name(class_name);
}

sNodeType* create_node_type_with_method_generics_number(int generics_num)
{
    char class_name[CLASS_NAME_MAX+1];

    snprintf(class_name, CLASS_NAME_MAX, "MethodGenericsParametorClass%d", generics_num);

    return create_node_type_with_class_name(class_name);
}

sNodeType* create_node_type_from_cl_type(sCLType* cl_type, sCLClass* klass)
{
    sNodeType* node_type = alloc_node_type();

    node_type->mClass = get_class_with_load(CONS_str(&klass->mConst, cl_type->mClassNameOffset));

    MASSERT(node_type->mClass != NULL);

    node_type->mNumGenericsTypes = cl_type->mNumGenericsTypes;

    int i;
    for(i=0; i<cl_type->mNumGenericsTypes; i++) {
        node_type->mGenericsTypes[i] = create_node_type_from_cl_type(cl_type->mGenericsTypes[i], klass);
    }

    node_type->mArray = cl_type->mArray;

    if(cl_type->mBlockType) {
        node_type->mBlockType = alloc_node_block_type();
        node_type->mBlockType->mNumParams = cl_type->mBlockType->mNumParams;

        int i;
        for(i=0; i<cl_type->mBlockType->mNumParams; i++) {
            node_type->mBlockType->mParams[i] = create_node_type_from_cl_type(cl_type->mBlockType->mParams[i], klass);
        }

        node_type->mBlockType->mResultType = create_node_type_from_cl_type(cl_type->mBlockType->mResultType, klass);
    }
    else {
        node_type->mBlockType = NULL;
    }

    return node_type;
}

BOOL substitution_posibility(sNodeType* left, sNodeType* right, sNodeType* left_generics_types, sNodeType* right_generics_types)
{
    sNodeType* left2;
    if(left_generics_types) {
        if(!solve_generics_types_for_node_type(left, ALLOC &left2, left_generics_types)) 
        {
            return FALSE;
        }
    }
    else {
        left2 = left;
    }

    sNodeType* right2;
    if(right_generics_types) {
        if(!solve_generics_types_for_node_type(right, ALLOC &right2, right_generics_types)) 
        {
            return FALSE;
        }
    }
    else {
        right2 = right;
    }

    sCLClass* left_class = left2->mClass;
    sCLClass* right_class = right2->mClass;

    if(left_class->mGenericsParamClassNum != -1 || right_class->mGenericsParamClassNum != -1) {
        return FALSE;
    }
    else if(type_identify_with_class_name(right2, "Null") && (!(left_class->mFlags & CLASS_FLAGS_PRIMITIVE) || left2->mArray)) 
    {
        return TRUE;
    }
    else if(type_identify_with_class_name(right2, "Anonymous") && !(left_class->mFlags & CLASS_FLAGS_PRIMITIVE)) 
    {
        return TRUE;
    }
    else if(type_identify_with_class_name(left2, "Anonymous") && !(right_class->mFlags & CLASS_FLAGS_PRIMITIVE)) 
    {
        return TRUE;
    }
    else if(left_class->mFlags & CLASS_FLAGS_INTERFACE) {
        if(right_class->mFlags & CLASS_FLAGS_INTERFACE) {
            return type_identify(left2, right2);
        }
        else {
            return check_implemented_methods_for_interface(left_class, right_class);
        }
    }
    else if(type_identify_with_class_name(left2, "lambda")) {
        if(type_identify_with_class_name(right2, "lambda")) {
            sNodeBlockType* left_block_type = left2->mBlockType;
            sNodeBlockType* right_block_type = right2->mBlockType;

            if(left_block_type && right_block_type) {
                return substitution_posibility_for_node_block_type(left_block_type, right_block_type, left_generics_types, right_generics_types);
            }
            else {
                return FALSE;
            }
        }
        else {
            return FALSE;
        }
    }
    else {
        if(left2->mClass == right2->mClass && left2->mArray == right2->mArray && left2->mNumGenericsTypes == right2->mNumGenericsTypes) {
            int i;
            for(i=0; i<left2->mNumGenericsTypes; i++) {
                if(!substitution_posibility(left2->mGenericsTypes[i], right2->mGenericsTypes[i], left_generics_types, right_generics_types))
                {
                    return FALSE;
                }
            }

            return TRUE;
        }
        else {
            return FALSE;
        }
    }
}

BOOL substitution_posibility_with_class_name(sNodeType* left, char* right_class_name)
{
    return substitution_posibility(left, create_node_type_with_class_name(right_class_name), NULL , NULL);
}

BOOL no_cast_types_for_binary_operator(sNodeType* left_type, sNodeType* right_type)
{
    return type_identify_with_class_name(left_type, "pointer") && type_identify_with_class_name(right_type, "ulong");
}

BOOL operand_posibility(sNodeType* left, sNodeType* right, char* op_string)
{
    if(type_identify_with_class_name(left, "pointer") 
        && (strcmp(op_string, "+") == 0 || strcmp(op_string, "-") == 0))
    {
        if(strcmp(op_string, "+") == 0) {
            return type_identify_with_class_name(right, "ulong");
        }
        else { // strcmp(op_string, "-") == 0
            return type_identify_with_class_name(right, "ulong") || type_identify_with_class_name(right, "pointer");
        }
    }
    else {
        return left->mClass == right->mClass;
    }
}

BOOL operand_posibility_with_class_name(sNodeType* left, char* right_class_name, char* op_string)
{
    return operand_posibility(left, create_node_type_with_class_name(right_class_name), op_string);
}

BOOL type_identify(sNodeType* left, sNodeType* right)
{
    return left->mClass == right->mClass && left->mArray == right->mArray;
}

BOOL type_identify_with_class_name(sNodeType* left, char* right_class_name)
{
    return type_identify(left, create_node_type_with_class_name(right_class_name));
}

BOOL class_identify_with_class_name(sCLClass* klass, char* class_name)
{
    sCLClass* klass2 = get_class_with_load(class_name);

    MASSERT(klass2 != NULL);

    return klass == klass2;
}

BOOL solve_generics_types_for_node_type(sNodeType* node_type, ALLOC sNodeType** result, sNodeType* generics_type)
{
    int i;
    int j;

    if(generics_type == NULL) {
        *result = clone_node_type(node_type); // no solve
    }
    else if(type_identify_with_class_name(node_type, "Self")) {
        *result = alloc_node_type();
        (*result)->mClass = generics_type->mClass;

        (*result)->mNumGenericsTypes = node_type->mNumGenericsTypes;

        for(j=0; j<node_type->mNumGenericsTypes; j++) {
            (void)solve_generics_types_for_node_type(node_type->mGenericsTypes[j], &(*result)->mGenericsTypes[j], generics_type);

            // if it can not be solved generics, no solve the generics type
        }

        (*result)->mBlockType = node_type->mBlockType;
        (*result)->mArray = node_type->mArray;
    }
    else if(generics_type && generics_type->mNumGenericsTypes > 0) {
        for(i=0; i<GENERICS_TYPES_MAX; i++) {
            if(node_type->mClass->mGenericsParamClassNum == i) {
                if(i < generics_type->mNumGenericsTypes) {
                    *result = ALLOC clone_node_type(generics_type->mGenericsTypes[i]);
                    (*result)->mArray = node_type->mArray;
                    return TRUE;
                }
                else {
                    *result = ALLOC clone_node_type(node_type); // no solve
                    return FALSE; // error
                }
            }
        }

        *result = alloc_node_type();
        (*result)->mClass = node_type->mClass;

        (*result)->mNumGenericsTypes = node_type->mNumGenericsTypes;

        for(j=0; j<node_type->mNumGenericsTypes; j++) {
            (void)solve_generics_types_for_node_type(node_type->mGenericsTypes[j], &(*result)->mGenericsTypes[j], generics_type);

            // if it can not be solved generics, no solve the generics type
        }

        (*result)->mBlockType = node_type->mBlockType;
        (*result)->mArray = node_type->mArray;
    }
    else {
        *result = clone_node_type(node_type); // no solve
    }

    return TRUE;
}

sNodeType* create_generics_types_from_generics_params(sCLClass* klass)
{
    sNodeType* result = alloc_node_type();

    result->mClass = klass;
    result->mNumGenericsTypes = klass->mNumGenerics;

    int i;
    for(i=0; i<klass->mNumGenerics; i++) {
        int offset = klass->mGenericsParamTypeOffsets[i];
        sCLClass* interface = get_class_with_load(CONS_str(&klass->mConst, offset));

        MASSERT(interface != NULL);

        result->mGenericsTypes[i] = create_node_type_with_class_pointer(interface);
    }

    return result;
}

void solve_generics_for_variable_to_class(sCLClass* klass, sCLClass** result, sCLClass* generics_class)
{
    int generics_param_number = klass->mGenericsParamClassNum;

    *result = klass;

    if(generics_param_number != -1) {
        if(generics_param_number < generics_class->mNumGenerics) {
            int offset = generics_class->mGenericsParamTypeOffsets[generics_param_number];
            sCLClass* interface = get_class_with_load(CONS_str(&generics_class->mConst, offset));

            MASSERT(klass != NULL);

            *result = interface;
        }
    }
}

void solve_generics_for_variable(sNodeType* generics_type, sNodeType** generics_type2, sCLClass* generics_class)
{
    *generics_type2 = alloc_node_type();

    sCLClass* klass = generics_type->mClass;

    int generics_param_number = klass->mGenericsParamClassNum;

    if(generics_class != NULL) {
        if(generics_param_number != -1) {
            if(generics_param_number < generics_class->mNumGenerics) {
                int offset = generics_class->mGenericsParamTypeOffsets[generics_param_number];
                sCLClass* interface = get_class_with_load(CONS_str(&generics_class->mConst, offset));

                MASSERT(interface != NULL);

                klass = interface;
            }
        }
    }

    (*generics_type2)->mClass = klass;

    int i;
    for(i=0; i<generics_type->mNumGenericsTypes; i++) {
        solve_generics_for_variable(generics_type->mGenericsTypes[i], &(*generics_type2)->mGenericsTypes[i], generics_class);
    }

    (*generics_type2)->mNumGenericsTypes = generics_type->mNumGenericsTypes;
    (*generics_type2)->mArray = generics_type->mArray;
    (*generics_type2)->mBlockType = generics_type->mBlockType;
}

BOOL is_exception_type(sNodeType* exception_type)
{
    return substitution_posibility_with_class_name(exception_type, "Exception");
}

void print_node_type(sNodeType* node_type)
{
    if(node_type->mNumGenericsTypes == 0) {
        if(node_type == NULL) {
            printf("type is NULL\n");
        }
        else if(node_type->mClass == NULL) {
            printf("class of node type is NULL\n");
        }
        else {
            printf("%s", CLASS_NAME(node_type->mClass));
        }
    }
    else {
        if(node_type == NULL) {
            printf("type is NULL\n");
        }
        else if(node_type->mClass == NULL) {
            printf("class of node type is NULL\n");
        }
        else {
            printf("%s<", CLASS_NAME(node_type->mClass));
        }

        int i;
        for(i=0; i<node_type->mNumGenericsTypes; i++) {
            print_node_type(node_type->mGenericsTypes[i]);
            if(i != node_type->mNumGenericsTypes-1) printf(",");
        }

        printf(">");
    }

    if(node_type->mArray) {
        printf("[]");
    }
}

BOOL boxing_posibility(sNodeType* left_type, sNodeType* right_type)
{
    if(left_type->mNumGenericsTypes == 0 && right_type->mNumGenericsTypes == 0) {
        sCLClass* left_class = left_type->mClass;
        sCLClass* right_class = right_type->mClass;

        if(right_class->mBoxingClass == left_class) {
            return TRUE;
        }

        if(class_identify_with_class_name(left_class, "Anonymous")) {
            return TRUE;
        }
    }

    return FALSE;
}

BOOL unboxig_posibility(sCLClass* klass)
{
    return !(klass->mFlags & CLASS_FLAGS_PRIMITIVE) && klass->mUnboxingClass != NULL;
}

void boxing_to_lapper_class(sNodeType** type_, struct sCompileInfoStruct* info)
{
    sCLClass* klass = (*type_)->mClass;

    if((*type_)->mArray) {
        sNodeType* rapper_type;
        make_boxing_type((*type_), &rapper_type);

        cast_right_type_to_left_type(rapper_type, type_, info);
    }
    else if(klass->mFlags & CLASS_FLAGS_PRIMITIVE) {
        if(klass->mBoxingClass != NULL) {
            sNodeType* boxing_type = create_node_type_with_class_pointer(klass->mBoxingClass);

            cast_right_type_to_left_type(boxing_type, type_, info);
        }
        else {
            parser_err_msg(info->pinfo, "primitive class without boxing");
            info->err_num++;
        }
    }
}

BOOL unboxing_to_primitive_type(sNodeType** left_type, struct sCompileInfoStruct* info)
{
    sCLClass* primitive_class = (*left_type)->mClass->mUnboxingClass;
    sNodeType* primitive_type = create_node_type_with_class_pointer(primitive_class);
    cast_right_type_to_left_type(primitive_type, left_type, info);

    return TRUE;
}

void make_boxing_type(sNodeType* type, sNodeType** result)
{
    (*result) = alloc_node_type();

    sCLClass* klass = type->mClass;

    if(type->mArray) {
        (*result)->mClass = get_class("Array");

        (*result)->mNumGenericsTypes = 1;

        sNodeType* node_type = create_node_type_with_class_pointer(klass);

        make_boxing_type(node_type, &(*result)->mGenericsTypes[0]);
    }
    else if((klass->mFlags & CLASS_FLAGS_PRIMITIVE) && klass->mBoxingClass) {
        (*result)->mClass = klass->mBoxingClass;

        (*result)->mNumGenericsTypes = type->mNumGenericsTypes;

        int i;
        for(i=0; i<type->mNumGenericsTypes; i++) {
            make_boxing_type(type->mGenericsTypes[i], &(*result)->mGenericsTypes[i]);
        }
    }
    else {
        (*result)->mClass = klass;

        (*result)->mNumGenericsTypes = type->mNumGenericsTypes;

        int i;
        for(i=0; i<type->mNumGenericsTypes; i++) {
            make_boxing_type(type->mGenericsTypes[i], &(*result)->mGenericsTypes[i]);
        }
    }
}
