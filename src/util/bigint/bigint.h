#ifndef BIGINT_H
#define BIGINT_H

#include <stdint.h>
#include <stdbool.h>

// A big integer is an array of 64-bit unsigned integers, stored least
// significant 64 bits first. The array is stored in "arr", and its size is
// stored in "size".
typedef struct {
  uint64_t *arr;
  int size;
} Bigint;

// Initialize "bigint" to allocate an array of size "size", and set its value
// to "x"
void BigintNew(Bigint *bigint, int size, uint64_t x);
// Delete the "bigint" initialized by calling BigintNew
void BigintDelete(Bigint *bigint);
// Add a Bigint "a" and an integer "b" and store the result in "c". The function
// returns true if and only if there is no overflow
bool BigintAddInt(Bigint *c, Bigint *a, uint64_t b);
// Add two Bigints "a" and "b" and store the result in "c". The function returns
// true if and only if there is no overflow
bool BigintAdd(Bigint *c, Bigint *a, Bigint *b);
// Subtract integer "b" from Bigint "a" and store the result in "c". The
// function returns true if and only if there is no underflow
bool BigintSubInt(Bigint *c, Bigint *a, uint64_t b);
// Subtract two Bigints "a" and "b" and store the result in "c". The function
// returns true if and only if there is no underflow
bool BigintSub(Bigint *c, Bigint *a, Bigint *b);
// Multiply a Bigint "a" and an integer "b" and store the result in "c". The
// function returns true if and only if there is no overflow
bool BigintMulInt(Bigint *c, Bigint *a, uint64_t b);
// Multiply two Bigints "a" and "b" and store the result in "c". The
// function returns true if and only if there is no overflow
bool BigintMul(Bigint *c, Bigint *a, Bigint *b);
// Set the "i"-th bit of Bigint "a" to "b"
void BigintSetBit(Bigint *a, int i, bool b);
// Compares two Bigints "a" and "b":
// - If "a" < "b", then returns -1
// - If "a" = "b", then returns 0
// - If "a" > "b", then returns 1
int BigintCmp(Bigint *a, Bigint *b);
// Compares a Bigint "a" to an integer "b"
int BigintCmpInt(Bigint *a, uint64_t b);
// Prints the Bigint "a" in hexadecimal
void BigintPrintHex(Bigint *a);

#endif // BIGINT_H
