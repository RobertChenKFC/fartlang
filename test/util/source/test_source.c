#include "util/source/source.h"
#include <assert.h>

int main() {
  FILE *file = fopen(__FILE__, "r");
  Source *source = SourceFromFile(file);
  fclose(file);

  // 1. Test increment source point
  SourcePoint point;
  point.lineNo = 5;
  point.charNo = 39;
  SourcePointIncrement(source, &point);
  assert(point.lineNo == 5 && point.charNo == 40);
  SourcePointIncrement(source, &point);
  assert(point.lineNo == 6 && point.charNo == 0);
  SourcePointDecrement(source, &point);
  assert(point.lineNo == 5 && point.charNo == 40);

  // 2. Print a single-line location
  SourceLocation location;
  location.from.lineNo = 5;
  location.from.charNo = 19;
  location.to.lineNo = 5;
  location.to.charNo = 32;
  SourceLocationPrint(source, 1, SOURCE_COLOR_RED, &location);

  // 3. Print a multi-line location
  location.from.lineNo = 12;
  location.from.charNo = 22;
  location.to.lineNo = 14;
  location.to.charNo = 40;
  fprintf(stderr, "\n");
  SourceLocationPrint(source, 1, SOURCE_COLOR_GREEN, &location);

  // 4. Print multiple single-line locations
  SourceLocation location2;
  SourceLocation location3;
  SourceLocation location4;
  location.from.lineNo = 3;
  location.from.charNo = 0;
  location.to.lineNo = 3;
  location.to.charNo = 2;
  location2.from.lineNo = 4;
  location2.from.charNo = 2;
  location2.to.lineNo = 4;
  location2.to.charNo = 6;
  location3.from.lineNo = 4;
  location3.from.charNo = 21;
  location3.to.lineNo = 4;
  location3.to.charNo = 28;
  location4.from.lineNo = 4;
  location4.from.charNo = 31;
  location4.to.lineNo = 4;
  location4.to.charNo = 33;
  fprintf(stderr, "\n");
  SourceLocationPrint(source, 4,
      SOURCE_COLOR_YELLOW, &location,
      SOURCE_COLOR_YELLOW, &location2,
      SOURCE_COLOR_BLUE, &location3,
      SOURCE_COLOR_PURPLE, &location4);

  // 5. Print multiple multi-line locations
  location.from.lineNo = 21;
  location.from.charNo = 2;
  location.to.lineNo = 24;
  location.to.charNo = 24;
  location2.from.lineNo = 28;
  location2.from.charNo = 2;
  location2.to.lineNo = 31;
  location2.to.charNo = 25;
  location3.from.lineNo = 57;
  location3.from.charNo = 6;
  location3.to.lineNo = 60;
  location3.to.charNo = 36;
  fprintf(stderr, "\n");
  SourceLocationPrint(source, 3,
      SOURCE_COLOR_RED, &location,
      SOURCE_COLOR_GREEN, &location2,
      SOURCE_COLOR_CYAN, &location3);

  SourceDelete(source);
}
