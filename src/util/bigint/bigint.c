#include "util/bigint/bigint.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Copies the value from "a" to "b"; returns true if and only if the value of
// "a" fits in the memory of "b"
bool BigintCopy(Bigint *b, Bigint *a);
// Same as BigintAdd, but with an extra carry at the least significant digit
bool BigintAddCarry(Bigint *c, Bigint *a, Bigint *b, uint64_t carry);
// Multiply two 64-bit integers "a" and "b", and store the product in two 64-bit
// integers: "hi" and "lo" for the top and bottom 64 bits of the product,
// respectively
void BigintMul64(uint64_t *hi, uint64_t *lo, uint64_t a, uint64_t b);

void BigintNew(Bigint *bigint, int size, uint64_t x) {
  bigint->arr = malloc(sizeof(uint64_t) * size);
  bigint->size = size;
  memset(bigint->arr, 0, sizeof(uint64_t) * size);
  bigint->arr[0] = x;
}

void BigintDelete(Bigint *bigint) {
  free(bigint->arr);
}

bool BigintCopy(Bigint *b, Bigint *a) {
  for (int i = a->size - 1; i >= 0; --i) {
    if (a->arr[i] != 0) {
      if (i >= b->size)
        return false;
      memcpy(b->arr, a->arr, sizeof(uint64_t) * (i + 1));
      memset(b->arr + (i + 1), 0, sizeof(uint64_t) * (b->size - i - 1));
      return true;
    }
  }
  memset(b->arr, 0, sizeof(uint64_t) * b->size);
  return true;
}

bool BigintAddCarry(Bigint *c, Bigint *a, Bigint *b, uint64_t carry) {
  bool overflow = false;
  if (c != a)
    overflow = !BigintCopy(c, a);

  for (int i = 0; i < b->size; ++i) {
    if (i >= c->size && (b->arr[i] != 0 || carry != 0)) {
      overflow = true;
    } else {
      uint64_t nxtCarry = c->arr[i] > UINT64_MAX - b->arr[i] ? 1 : 0;
      c->arr[i] += b->arr[i];
      nxtCarry += c->arr[i] > UINT64_MAX - carry ? 1 : 0;
      c->arr[i] += carry;
      carry = nxtCarry;
    }
  }
  return !overflow && carry == 0;
}

bool BigintAddInt(Bigint *c, Bigint *a, uint64_t b) {
  Bigint bb;
  uint64_t arr[1];
  arr[0] = b;
  bb.arr = arr;
  bb.size = 1;
  return BigintAdd(c, a, &bb);
}

bool BigintAdd(Bigint *c, Bigint *a, Bigint *b) {
  return BigintAddCarry(c, a, b, 0);
}

bool BigintSubInt(Bigint *c, Bigint *a, uint64_t b) {
  Bigint bb;
  uint64_t arr[1];
  arr[0] = b;
  bb.arr = arr;
  bb.size = 1;
  return BigintSub(c, a, &bb);
}

bool BigintSub(Bigint *c, Bigint *a, Bigint *b) {
  Bigint nb;
  int arrSize = a->size > b->size ? a->size : b->size;
  uint64_t arr[arrSize];
  for (int i = 0; i < b->size; ++i)
    arr[i] = ~b->arr[i];
  for (int i = b->size; i < arrSize; ++i)
    arr[i] = ~(0ULL);
  nb.arr = arr;
  nb.size = arrSize;
  return !BigintAddCarry(c, a, &nb, 1);
}

#define LO32(x) ((x) & 0xffffffff)
#define HI32(x) (((x) >> 32) & 0xffffffff)
void BigintMul64(uint64_t *hi, uint64_t *lo, uint64_t a, uint64_t b) {
  uint64_t alo = LO32(a), ahi = HI32(a);
  uint64_t blo = LO32(b), bhi = HI32(b);
  *lo = alo * blo;
  *hi = ahi * bhi;

  uint64_t x = alo * bhi;
  uint64_t xlo = LO32(x) << 32;
  uint64_t xhi = HI32(x);
  uint64_t c = *lo > UINT64_MAX - xlo ? 1 : 0;
  *lo += xlo;
  *hi += xhi + c;

  x = ahi * blo;
  xlo = LO32(x) << 32;
  xhi = HI32(x);
  c = *lo > UINT64_MAX - xlo ? 1 : 0;
  *lo += xlo;
  *hi += xhi + c;
}

bool BigintMulInt(Bigint *c, Bigint *a, uint64_t b) {
  Bigint bb;
  uint64_t arr[1];
  arr[0] = b;
  bb.arr = arr;
  bb.size = 1;
  return BigintMul(c, a, &bb);
}

bool BigintMul(Bigint *c, Bigint *a, Bigint *b) {
  Bigint tmp;
  uint64_t arr[a->size];
  if (a == c) {
    memcpy(arr, a->arr, sizeof(uint64_t) * a->size);
    tmp.arr = arr;
    tmp.size = a->size;
    a = &tmp;
  }

  memset(c->arr, 0, sizeof(uint64_t) * c->size);
  bool overflow = false;
  for (int i = 0; i < a->size; ++i) {
    uint64_t carry = 0;
    for (int j = 0; j < b->size; ++j) {
      uint64_t hi, lo;
      BigintMul64(&hi, &lo, a->arr[i], b->arr[j]);
      if (i + j >= c->size && (lo != 0 || carry != 0)) {
        overflow = true;
      } else {
        uint64_t nxtCarry = c->arr[i + j] > UINT64_MAX - lo ? 1 : 0;
        c->arr[i + j] += lo;
        nxtCarry += c->arr[i + j] > UINT64_MAX - carry ? 1 : 0;
        c->arr[i + j] += carry;
        carry = nxtCarry + hi;
      }
    }
    for (int j = b->size; i + j < c->size; ++j) {
      uint64_t nxtCarry = c->arr[i + j] > UINT64_MAX - carry ? 1 : 0;
      c->arr[i + j] += carry;
      carry = nxtCarry;
    }
    if (carry != 0)
      overflow = true;
  }
  return !overflow;
}

void BigintSetBit(Bigint *a, int i, bool b) {
  int idx = i / 64;
  assert(0 <= idx && idx < a->size);
  a->arr[idx] |= 1ULL << (i % 64);
}

int BigintCmp(Bigint *a, Bigint *b) {
  int gt = 1;
  if (a->size < b->size) {
    Bigint *t = a;
    a = b;
    b = t;
    gt = -1;
  }
  for (int i = a->size - 1; i >= b->size; --i) {
    if (a->arr[i] != 0)
      return gt;
  }
  for (int i = b->size - 1; i >= 0; --i) {
    if (a->arr[i] > b->arr[i])
      return gt;
    if (a->arr[i] < b->arr[i])
      return -gt;
  }
  return 0;
}

int BigintCmpInt(Bigint *a, uint64_t b) {
  for (int i = a->size - 1; i > 0; --i) {
    if (a->arr[i] != 0)
      return 1;
  }
  if (a->arr[0] > b)
    return 1;
  if (a->arr[0] < b)
    return -1;
  return 0;
}

void BigintPrintHex(Bigint *a) {
  bool first = true;
  for (int i = a->size - 1; i >= 0; --i) {
    if (!first) {
      printf("%016llx", a->arr[i]);
    } else if (a->arr[i] != 0) {
      first = false;
      printf("0x%llx", a->arr[i]);
    }
  }
  if (first)
    printf("0x0");
  printf("\n");
}
