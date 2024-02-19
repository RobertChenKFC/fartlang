#ifndef SOURCE_H
#define SOURCE_H

#include "util/vector/vector.h"
#include <stdio.h>
#include <stdbool.h>

// Forward declarations
typedef struct Source Source;
typedef struct SourcePoint SourcePoint;
typedef struct SourceLocation SourceLocation;

// Constants
#define SOURCE_COLOR_RED    "\033[31;1m"
#define SOURCE_COLOR_GREEN  "\033[32;1m"
#define SOURCE_COLOR_YELLOW "\033[33;1m"
#define SOURCE_COLOR_BLUE   "\033[34;1m"
#define SOURCE_COLOR_PURPLE "\033[35;1m"
#define SOURCE_COLOR_CYAN   "\033[36;1m"
#define SOURCE_COLOR_RESET  "\033[0m"

// A structure that contains the entire file in memory, as well as indices to
// where each line starts
struct Source {
  unsigned char *file;
  Vector *lines;
};

// A point in the source code, indicate the position of a character in the
// source code
struct SourcePoint {
  int lineNo;
  int charNo;
};

// A pair of source points, indicating the end point positions of a segment
// in the source code
struct SourceLocation {
  SourcePoint from, to;
};

// Read content from "file" and into a source object
Source *SourceFromFile(FILE *file);
// Delete "source" created from SourceFromFile
void SourceDelete(Source *source);
// Increment "point" by one character, and find the new point according to
// "source"; returns true if and only if line number is incremented as well
bool SourcePointIncrement(Source *source, SourcePoint *point);
// Decrement "point" by one character, and find the new point according to
// "source"; returns true if and only if line number is decremented as well
bool SourcePointDecrement(Source *source, SourcePoint *point);
// Read a character from "source" at "point"
unsigned char SourceGetFromPoint(Source *source, SourcePoint *point);
// Takes "n" pairs of extra arguments, each argument contains first a source
// color, then a pointer to a SourceLocation; this function prints the "n"
// locations in different colors
void SourceLocationPrint(Source *source, int n, ...);
// Get the max of two source points (later point in the file) "p1" and "p2"
SourcePoint SourcePointMax(SourcePoint *p1, SourcePoint *p2);
// Get the min of two source points (earlier point in the file) "p1" and "p2"
SourcePoint SourcePointMin(SourcePoint *p1, SourcePoint *p2);

#endif // SOURCE_H
