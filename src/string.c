#include "common.h"
#include <wchar.h>

CLObject create_string_object(char* str)
{
    /// convert str to wstr ///
    int len = strlen(str);

    wchar_t* wstr = MCALLOC(1, sizeof(wchar_t)*(len+1));

    (void)mbstowcs(wstr, str, len+1);

    int wlen = wcslen(wstr);

    /// create object ///
    sCLClass* string_class = get_class("String");
    MASSERT(string_class != NULL);

    CLObject obj = create_object(string_class);

    /// push object ///
    CLVALUE cl_value;
    cl_value.mLongValue = 0;
    cl_value.mObjectValue = obj;
    push_value_to_global_stack(cl_value);

    /// create char array ///
    sCLClass* char_class = get_class("char");
    MASSERT(char_class != NULL);

    CLObject buffer = create_array_object(char_class, wlen+1);
    sCLObject* buffer_data = CLOBJECT(buffer);

    int i;
    for(i=0; i<wlen; i++) {
        buffer_data->mFields[i].mCharValue = wstr[i];
    }
    buffer_data->mFields[i].mCharValue = '\0';

    /// entry char array to object ///
    sCLObject* obj_data = CLOBJECT(obj);
    obj_data->mFields[0].mObjectValue = buffer;
    obj_data->mFields[1].mIntValue = wlen + 1;
    obj_data->mFields[2].mIntValue = wlen;

    /// pop object ///
    pop_global_stack();

    MFREE(wstr);

    return obj;
}

CLObject create_buffer_object(char* buffer, size_t size)
{
    /// create object ///
    sCLClass* buffer_class = get_class("Buffer");
    MASSERT(buffer_class != NULL);

    CLObject obj = create_object(buffer_class);

    sCLObject* object_data = CLOBJECT(obj);

    object_data->mFields[0].mPointerValue = MCALLOC(1, size);  // bufffer
    memcpy(object_data->mFields[0].mPointerValue, buffer, size);

    object_data->mFields[1].mULongValue = size;                  // len
    object_data->mFields[2].mULongValue = size;                  // size

    return obj;
}

CLObject create_path_object(char* path)
{
    /// create object ///
    sCLClass* path_class = get_class("Path");
    MASSERT(path_class != NULL);

    CLObject str_object = create_string_object(path);

    CLVALUE cl_value;
    cl_value.mLongValue = 0;
    cl_value.mObjectValue = str_object;
    push_value_to_global_stack(cl_value);

    CLObject obj = create_object(path_class);

    sCLObject* object_data = CLOBJECT(obj);

    object_data->mFields[0].mObjectValue = str_object;

    pop_global_stack();

    return obj;
}

