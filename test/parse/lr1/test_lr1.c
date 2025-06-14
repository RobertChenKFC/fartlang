#include "parse/lr1/lr1.h"
#include "parse/lr1/1.h"
#include "parse/lr1/2.h"
#include "parse/lr1/3.h"
#include "parse/lr1/4.h"
#include "parse/lr1/5.h"
#include "parse/lr1/6.h"
#include "parse/lr1/7.h"

int main() {
  // 1. Create LR(1) graph of simple CFG (1)
  test1();

  // 2. Create LR(1) graph of simple CFG (2)
  test2();

  // 3. Create LR(1) graph of simple CFG (3)
  test3();

  // 4. Create LR(1) graph of simple CFG with empty productions
  test4();

  // 5. Create LR(1) graph of complex CFG (1)
  test5();

  // 6. Create LR(1) graph of complex CFG (2)
  test6();

  // 7. Create LR(1) graph of simple CFG (4)
  test7();
}
