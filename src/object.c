#include "common.h"

void object_mark_fun(CLObject self, unsigned char* mark_flg)
{
    sCLObject* object = CLOBJECT(self);
    sCLClass* klass = object->mClass;

    int i;
    for(i=0; i<klass->mNumFields; i++) {
        mark_object(object->mFields[i].mObjectValue, mark_flg);
    }
}

BOOL free_object(CLObject self)
{
    sCLObject* object = CLOBJECT(self);
    sCLClass* klass = object->mClass;

    if(!call_finalize_method_on_free_object(klass, self)) {
        return FALSE;
    }

    return TRUE;
}

static unsigned int object_size(sCLClass* klass)
{
    unsigned int size;

    size = sizeof(sCLObject) - sizeof(CLVALUE) * DUMMY_ARRAY_SIZE;
    size += (unsigned int)sizeof(CLVALUE) * klass->mNumFields;

    alignment(&size);

    return size;
}

CLObject create_object(sCLClass* klass)
{
    int size = object_size(klass);

    CLObject obj = alloc_heap_mem(size, klass, -1);

#ifdef ENABLE_JIT
    push_jit_object(obj);
#endif

    return obj;
}

char* get_class_name_from_cl_type(sCLType* cl_type, sCLClass* klass)
{
    return CONS_str(&klass->mConst, cl_type->mClassNameOffset);
}

static BOOL check_same_interface_of_two_methods(sCLMethod* method1, sCLClass* klass1, sCLMethod* method2, sCLClass* klass2)
{
    char* name1 = METHOD_NAME2(klass1, method1);
    char* name2 = METHOD_NAME2(klass2, method2);

    if(strcmp(name1, name2) != 0) {
        return FALSE;
    }

    char* result_type1 = get_class_name_from_cl_type(method1->mResultType, klass1);
    char* result_type2 = get_class_name_from_cl_type(method2->mResultType, klass2);

    if(strcmp(result_type1, result_type2) != 0) {
        return FALSE;
    }

    if(method1->mNumParams != method2->mNumParams) {
        return FALSE;
    }

    int i;
    for(i=0; i<method1->mNumParams; i++) {
        sCLParam* param1 = method1->mParams + i;
        sCLParam* param2 = method2->mParams + i;

        char* param1_type = get_class_name_from_cl_type(param1->mType, klass1);
        char* param2_type = get_class_name_from_cl_type(param2->mType, klass2);

        if(strcmp(param1_type, "Self") == 0) {
            param1_type = CLASS_NAME(klass2);
        }

        if(strcmp(param1_type, param2_type) != 0) {
            return FALSE;
        }

    }

    return TRUE;
}

static BOOL check_implemented_methods_for_interface_on_runtime(sCLClass* left_class, sCLClass* right_class)
{
    if(left_class != right_class) {
        int i;
        for(i=0; i<left_class->mNumMethods; i++) {
            sCLMethod* method = left_class->mMethods + i;

            BOOL found = FALSE;

            int j;
            for(j=0; j<right_class->mNumMethods; j++) {
                sCLMethod* method2 = right_class->mMethods + j;

                if(check_same_interface_of_two_methods(method, left_class, method2, right_class)) {
                    found = TRUE;
                }
            }

            if(!found) {
                return FALSE;
            }
        }
    }

    return TRUE;
}

BOOL object_implements_interface(CLObject object, sCLClass* interface)
{
    sCLObject* object_data = CLOBJECT(object);
    sCLClass* klass = object_data->mClass;

    return check_implemented_methods_for_interface_on_runtime(interface, klass);
}

