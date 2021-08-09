#include "util/source/source.h"
#include <stdlib.h>
#include <stdarg.h>

Source *SourceFromFile(FILE *file) {
  fseek(file, 0, SEEK_END);
  int size = ftell(file);
  fseek(file, 0, SEEK_SET);

  Source *source = malloc(sizeof(Source));
  source->file = malloc(size);
  source->lines = VectorNew();
  fread(source->file, 1, size, file);

  VectorAdd(source->lines, (void*)0);
  for (int i = 1; i < size; ++i) {
    if (source->file[i - 1] == '\n')
      VectorAdd(source->lines, (void*)(long long)i);
  }
  VectorAdd(source->lines, (void*)(long long)size);
  return source;
}

void SourceDelete(Source *source) {
  free(source->file);
  VectorDelete(source->lines);
  free(source);
}

bool SourcePointIncrement(Source *source, SourcePoint *point) {
  int lineNo = point->lineNo;
  if ((int)(long long)source->lines->arr[lineNo] + (++point->charNo) ==
      (int)(long long)source->lines->arr[lineNo + 1]) {
    ++point->lineNo;
    point->charNo = 0;
    return true;
  }
  return false;
}

bool SourcePointDecrement(Source *source, SourcePoint *point) {
  int lineNo = point->lineNo;
  if ((int)(long long)source->lines->arr[lineNo] + (--point->charNo) <
      (int)(long long)source->lines->arr[lineNo]) {
    --point->lineNo;
    point->charNo = (int)(long long)source->lines->arr[lineNo] -
                    (int)(long long)source->lines->arr[lineNo - 1] - 1;
    return true;
  }
  return false;
}

unsigned char SourceGetFromPoint(Source *source, SourcePoint *point) {
  return source->file[(int)(long long)source->lines->arr[point->lineNo] +
                      point->charNo];
}

void SourceLocationPrint(Source *source, int n, ...) {
  va_list list;
  va_start(list, n);

  SourcePoint point;
  SourceLocation *loc;
  unsigned char a = '\0';
  for (int i = 0; i < n; ++i) {
    const char *color = va_arg(list, const char*);
    loc = va_arg(list, SourceLocation*);
    if (i == 0) {
      point.lineNo = loc->from.lineNo;
      point.charNo = 0;
      fprintf(stderr, "%5d | ", point.lineNo + 1);
    }
    // TODO: refactor the while bodies (all the same)
    while (point.lineNo < loc->from.lineNo ||
           (point.lineNo == loc->from.lineNo &&
            point.charNo < loc->from.charNo)) {
      a = SourceGetFromPoint(source, &point);
      fputc(a, stderr);
      if (SourcePointIncrement(source, &point))
        fprintf(stderr, "%5d | ", point.lineNo + 1);
    }
    fprintf(stderr, "%s", color);
    while (point.lineNo < loc->to.lineNo ||
           (point.lineNo == loc->to.lineNo &&
            point.charNo <= loc->to.charNo)) {
      a = SourceGetFromPoint(source, &point);
      fputc(a, stderr);
      if (SourcePointIncrement(source, &point) &&
          (i != n - 1 || point.lineNo <= loc->to.lineNo))
        fprintf(stderr, SOURCE_COLOR_RESET"%5d |%s ", point.lineNo + 1, color);
    }
    fprintf(stderr, SOURCE_COLOR_RESET);
  }
  while (point.lineNo <= loc->to.lineNo) {
    a = SourceGetFromPoint(source, &point);
    fputc(a, stderr);
    SourcePointIncrement(source, &point);
  }
  // Always end with a newline
  if (a != '\n')
    fputc('\n', stderr);

  va_end(list);
}

