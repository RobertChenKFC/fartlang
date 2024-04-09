#include "util/bigint/bigint.h"
#include <assert.h>
#include <stdio.h>

void test1(void) {
  // 1. Create Bigint and print
  Bigint a;
  BigintNew(&a, 123, 0);
  BigintPrintHex(&a);
  BigintDelete(&a);
  BigintNew(&a, 456, 0xdeadbeef);
  BigintPrintHex(&a);
  BigintDelete(&a);
}

void test2(void) {
  // 2. Use Bigint to calculate the 1001th Fibonacci number
  Bigint x1, x2, x3;
  BigintNew(&x1, 12, 1);
  BigintNew(&x2, 12, 1);
  BigintNew(&x3, 12, 0);
  Bigint *a = &x1, *b = &x2, *c = &x3;
  for (int i = 3; i <= 1001; ++i) {
    assert(BigintAdd(c, a, b));
    Bigint *t = b;
    b = a;
    a = c;
    c = t;
  }
  BigintPrintHex(a);
  BigintDelete(a);
  BigintDelete(b);
  BigintDelete(c);
}

void test3(void) {
  // 3. Use Bigint to calculate the 300th factorial
  Bigint x;
  BigintNew(&x, 341, 1);
  for (int i = 2; i <= 300; ++i)
    assert(BigintMulInt(&x, &x, i));
  BigintPrintHex(&x);
  BigintDelete(&x);
}

void test4(void) {
  // 4. Some simple tests involving comparison of Bigints
  Bigint x, y;
  BigintNew(&x, 18, 1);
  for (int i = 2; i <= 29; ++i)
    assert(BigintMulInt(&x, &x, i));
  BigintNew(&y, 18, 1);
  assert(BigintMulInt(&y, &x, 30));
  assert(BigintCmp(&x, &y) == -1);
  assert(BigintSub(&y, &y, &x));
  assert(BigintMulInt(&x, &x, 29));
  assert(BigintCmp(&x, &y) == 0);
  assert(BigintAddInt(&x, &x, 1));
  assert(BigintCmp(&x, &y) == 1);
  assert(BigintSub(&x, &x, &x));
  assert(BigintCmpInt(&x, 0) == 0);
  BigintDelete(&x);
  BigintDelete(&y);
}

int main(void) {
  test1();
  test2();
  test3();
  test4();
}
