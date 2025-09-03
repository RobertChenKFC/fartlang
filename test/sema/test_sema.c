#include "sema/sema.h"
#include <stdio.h>
#include <assert.h>

int GetFilenamePosition(const char *path) {
  int pos = 0;
  for (int i = 0; path[i]; ++i) {
    if (path[i] == '/')
      pos = i + 1;
  }
  return pos;
}

int main(int argc, char **argv) {
  if (argc == 1) {
    SyntaxSetupParser();
  } else {
    assert(argc == 2);
    const char *path = argv[1];
    int pos = GetFilenamePosition(path);
    bool success = path[pos] == 's';
    bool checkMain = path[pos + 1] == 'm';

    SemaCtx ctx;
    assert(SemaCheck(&ctx, path, checkMain) == success);
    SemaCtxDelete(&ctx);
  }
}

