#include "common.h"

#define NATIVE_METHOD_HASH_SIZE 1024

struct sNativeMethodHashItem {
    char* mPath;
    char* mFunName;
    fNativeMethod mFun;
};

static struct sNativeMethodHashItem gNativeMethodHash[NATIVE_METHOD_HASH_SIZE];

static unsigned int get_hash_key_for_native_method(char* path)
{
    unsigned int key;
    char* p;

    p = path;

    key = 0;

    while(*p) {
        key += *p++;
    }

    return key % NATIVE_METHOD_HASH_SIZE;
}

void put_fun_to_hash_for_native_method(char* path, char* fun_name, fNativeMethod fun)
{
    unsigned int key, key2;

    key = get_hash_key_for_native_method(path);

    key2 = key;

    while(1) {
        if(gNativeMethodHash[key2].mPath == NULL) {
            gNativeMethodHash[key2].mPath = MSTRDUP(path);
            gNativeMethodHash[key2].mFunName = fun_name;
            gNativeMethodHash[key2].mFun = fun;
            break;
        }
        else {
            key2++;

            if(key2 >= NATIVE_METHOD_HASH_SIZE) {
                key2 = 0;
            }
            else if(key2 == key) {
                fprintf(stderr, "overflow native methods number");
                exit(1);
            }
        }
    }
}

fNativeMethod get_native_method(char* path, char** fun_name)
{
    unsigned int key, key2;

    key = get_hash_key_for_native_method(path);

    key2 = key;

    while(1) {
        if(gNativeMethodHash[key2].mPath == NULL) {
            (*fun_name) = NULL;
            return NULL;
        }
        else {
            if(strcmp(gNativeMethodHash[key2].mPath, path) == 0) {
                (*fun_name) = gNativeMethodHash[key2].mFunName;
                return gNativeMethodHash[key2].mFun;
            }
            else {
                key2++;

                if(key2 >= NATIVE_METHOD_HASH_SIZE) {
                    key2 = 0;
                }
                else if(key2 == key) {
                    (*fun_name) = NULL;
                    return NULL;
                }
            }
        }
    }
}

struct sNativeMethodStruct {
    const char* mPath;
    char* mFunName;
    fNativeMethod mFun;
};

typedef struct sNativeMethodStruct sNativeMethod;

// manually sort is needed
static sNativeMethod gNativeMethods[] = {
    { "Clover.load(String)", "Clover_load", Clover_load },
    { "System.exit(int)", "System_exit", System_exit },
    { "System.assert(bool)", "System_assert", System_assert },
    { "System.malloc(ulong)", "System_malloc", System_malloc },
    { "System.realloc(pointer,ulong)", "System_realloc", System_realloc },
    { "System.calloc(int,ulong)", "System_calloc", System_calloc },
    { "System.free(pointer)", "System_free", System_free },
    { "System.memcpy(pointer,pointer,ulong)", "System_memcpy", System_memcpy },
    { "System.memcmp(pointer,pointer,ulong)", "System_memcmp", System_memcmp },
    { "System.strlen(pointer)", "System_strlen", System_strlen },
    { "System.strlen(byte[])", "System_strlen2", System_strlen2 },
    { "System.strcpy(pointer,pointer)", "System_strcpy", System_strcpy },
    { "System.strncpy(pointer,pointer,ulong)", "System_strncpy", System_strncpy },
    { "System.strdup(pointer)", "System_strdup", System_strdup },
    { "System.print(String)", "System_print", System_print },
    { "System.println(String)", "System_println", System_println },
    { "System.printToError(String)", "System_printToError", System_printToError },
    { "System.printlnToError(String)", "System_printlnToError", System_printlnToError },
    { "System.sleep(int)", "System_sleep", System_sleep },
    { "System.pcre_exec(regex,Buffer,int,int,PcreOVec)", "System_pcre_exec", System_pcre_exec },
    { "System.sprintf(String,Array)", "System_sprintf", System_sprintf },
    { "System.mbstowcs(pointer,pointer,ulong)", "System_mbstowcs", System_mbstowcs },
    { "System.wcstombs(pointer,char[])", "System_wcstombs", System_wcstombs },
    { "System.atoi(String)", "System_atoi", System_atoi },
    { "System.atof(String)", "System_atof", System_atof },
    { "System.strtod(String)", "System_strtod", System_strtod },
    { "System.strcmp(String,String)", "System_strcmp", System_strcmp },
    { "System.strcasecmp(String,String)", "System_strcasecmp", System_strcasecmp },
    { "System.strtol(String,int)", "System_strtol", System_strtol },
    { "System.strtoul(String,int)", "System_strtoul", System_strtoul },
    { "System.srand(uint)", "System_srand", System_srand },
    { "System.rand()", "System_rand", System_rand },
    { "System.time()", "System_time", System_time },
    { "System.open(String,int,int)", "System_open", System_open },
    { "System.close(int)", "System_close", System_close },
    { "System.read(int,Buffer,ulong)", "System_read", System_read },
    { "System.write(int,Buffer,ulong)", "System_write", System_write },
    { "System.localtime(ulong,pointer,pointer,pointer,pointer,pointer,pointer,pointer,pointer,pointer)", "System_localtime", System_localtime },
    { "System.mktime(tm)", "System_mktime", System_mktime },
    { "System.lstat(String,stat)", "System_lstat", System_lstat },
    { "System.stat(String,stat)", "System_stat", System_stat },
    { "System.realpath(String)", "System_realpath", System_realpath },
    { "System.dirname(String)", "System_dirname", System_dirname },
    { "System.basename(String)", "System_basename", System_basename },
    { "System.opendir(String)", "System_opendir", System_opendir },
    { "System.readdir(pointer)", "System_readdir", System_readdir },
    { "System.closedir(pointer)", "System_closedir", System_closedir },
    { "System.initialize_string_system()", "System_initialize_string_system", System_initialize_string_system },
    { "System.initialize_file_system()", "System_initialize_file_system", System_initialize_file_system },
    { "System.initialize_command_system()", "System_initialize_command_system", System_initialize_command_system },
    { "System.pipe(pointer,pointer)", "System_pipe", System_pipe },
    { "System.fork(lambda)", "System_fork", System_fork },
    { "System.dup2(int,int)", "System_dup2", System_dup2 },
    { "System.execvp(String,List)", "System_execvp", System_execvp },
    { "System.execv(String,List)", "System_execv", System_execv },
    { "System.waitpid(int,pointer,int)", "System_waitpid", System_waitpid },
    { "System.WIFEXITED(int)", "System_WIFEXITED", System_WIFEXITED },
    { "System.WEXITSTATUS(int)", "System_WEXITSTATUS", System_WEXITSTATUS },
    { "System.WIFSIGNALED(int)", "System_WIFSIGNALED", System_WIFSIGNALED },
    { "System.WTERMSIG(int)", "System_WTERMSIG", System_WTERMSIG },
    { "System.WCOREDUMP(int)", "System_WCOREDUMP", System_WCOREDUMP },
    { "System.WIFSTOPPED(int)", "System_WIFSTOPPED", System_WIFSTOPPED },
    { "System.WSTOPSIG(int)", "System_WSTOPSIG", System_WSTOPSIG },
    { "System.WIFCONTINUED(int)", "System_WIFCONTINUED", System_WIFCONTINUED },
    { "System.getpid()", "System_getpid", System_getpid },
    { "System.setpgid(int,int)", "System_setpgid", System_setpgid },
    { "System.kill(int,int)", "System_kill", System_kill },
    { "System.tcsetpgrp(int,int)", "System_tcsetpgrp", System_tcsetpgrp },
    { "System.tcgetattr(int,termios)", "System_tcgetattr", System_tcgetattr },
    { "System.tcsetattr(int,int,termios)", "System_tcsetattr", System_tcsetattr },
    { "System.tcflow(int,int)", "System_tcflow", System_tcflow },
    { "System.tcflush(int,int)", "System_tcflush", System_tcflush },
    { "System.tcsendbreak(int,int)", "System_tcsendbreak", System_tcsendbreak },
    { "System.tcdrain(int)", "System_tcdrain", System_tcdrain },
    { "System.cfmakeraw(termios)", "System_cfmakeraw", System_cfmakeraw },
    { "System.cfgetispeed(termios)", "System_cfgetispeed", System_cfgetispeed },
    { "System.cfgetospeed(termios)", "System_cfgetospeed", System_cfgetospeed },
    { "System.cfsetispeed(termios,int)", "System_cfsetispeed", System_cfsetispeed },
    { "System.cfsetospeed(termios,int)", "System_cfsetospeed", System_cfsetospeed },
    { "System.cfsetspeed(termios,int)", "System_cfsetspeed", System_cfsetspeed },
    { "System.chmod(String,int)", "System_chmod", System_chmod },
    { "System.lchmod(String,int)", "System_lchmod", System_lchmod },
    { "System.fchmod(int,int)", "System_fchmod", System_fchmod },
    { "System.chown(String,int,int)", "System_chown", System_chown },
    { "System.lchown(String,int,int)", "System_lchown", System_lchown },
    { "System.fchown(int,int,int)", "System_fchown", System_fchown },
    { "System.unlink(String)", "System_unlink", System_unlink },
    { "System.access(String,int)", "System_access", System_access },
    { "System.utime(String,ulong,ulong)", "System_utime", System_utime },
    { "System.fnmatch(String,String,int)", "System_fnmatch", System_fnmatch },
    { "System.link(String,String)", "System_link", System_link },
    { "System.symlink(String,String)", "System_symlink", System_symlink },
    { "System.readlink(String)", "System_readlink", System_readlink },
    { "System.rename(String,String)", "System_rename", System_rename },
    { "System.truncate(String,ulong)", "System_truncate", System_truncate },
    { "System.ftruncate(int,ulong)", "System_ftruncate", System_ftruncate },
    { "System.chdir(String)", "System_chdir", System_chdir },
    { "System.fchdir(int)", "System_fchdir", System_fchdir },
    { "System.rmdir(String)", "System_rmdir", System_rmdir },
    { "System.mkdir(String,int)", "System_mkdir", System_mkdir },
    { "System.clock_getres(int,timespec)", "System_clock_getres", System_clock_getres },
    { "System.clock_gettime(int,timespec)", "System_clock_gettime", System_clock_gettime },
    { "System.clock_settime(int,timespec)", "System_clock_settime", System_clock_settime },
    { "System.dlopen(String,int)", "System_dlopen", System_dlopen },
    { "System.dlclose(pointer)", "System_dlclose", System_dlclose },
    { "System.dlsym(pointer,String)", "System_dlsym", System_dlsym },
    { "System.system(String)", "System_system", System_system },
    { "System.getenv(String)", "System_getenv", System_getenv },
    { "System.setenv(String,String,int)", "System_setenv", System_setenv },
    { "System.unsetenv(String)", "System_unsetenv", System_unsetenv },
    { "System.put_fun_to_hash_for_native_method(String,String,pointer)", "System_put_fun_to_hash_for_native_method", System_put_fun_to_hash_for_native_method },
    { "CLParser.initialize()", "CLParser_initialize", CLParser_initialize },

    { "", "", 0 }  // sentinel
};


void native_method_init()
{
    sNativeMethod* p;

    memset(gNativeMethodHash, 0, sizeof(struct sNativeMethodHashItem)*NATIVE_METHOD_HASH_SIZE);

    p = gNativeMethods;

    while(p->mPath[0] != 0) {
        put_fun_to_hash_for_native_method((char*)p->mPath, p->mFunName, p->mFun);
        p++;
    }
}

void native_method_final()
{
    int i;
    for(i=0; i<NATIVE_METHOD_HASH_SIZE; i++) {
        if(gNativeMethodHash[i].mPath) {
            MFREE(gNativeMethodHash[i].mPath);
        }
    }
}

