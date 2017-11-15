#include "common.h"

#define LOCKFILE "./dependency_check.txt"

static BOOL dependency_flag = FALSE; /* 依存チェックモード */

void set_dependency_compile(BOOL flg)
{
  dependency_flag = flg;
}

/* ファイル名取得 */
static char* get_filename(char* filename) {
  char *fname;
  fname = strrchr(filename,'/'); /* ファイル名取得 */
  if(fname == NULL) {
    fname = strrchr(filename,'\\'); /* windows 対応 */
  }
  if (fname == NULL) {
    fname = filename; /* セパレータがなかった */
  } else {
    fname++; /* セパレータがあったら１つ戻す */
  }
  return fname;
}

/*
 * 再帰的呼び出しチェック
 */
BOOL dependency_check(char* filename)
{
	FILE *fp;	/* (1)ファイルポインタの宣言 */
	char s[256];
  char s2[256];
  filename = get_filename(filename);
  if (!dependency_flag) {
    if ((fp = fopen(LOCKFILE, "w")) == NULL) return FALSE;
  } else {
    strcpy(s2,filename);
    strcat(s2,"\n");
    if ((fp = fopen(LOCKFILE, "r")) == NULL) return FALSE;
    while (fgets(s, 256, fp) != NULL) {
      if(strcmp(s,s2)==0) {
        fclose(fp);
        return FALSE;
      }
    }
    fclose(fp);
    if ((fp = fopen(LOCKFILE, "a")) == NULL) return FALSE;
  }
  fprintf(fp,"%s\n", filename);
  fclose(fp);
	return TRUE;
}

void dependency_final()
{
    remove(LOCKFILE);
}
