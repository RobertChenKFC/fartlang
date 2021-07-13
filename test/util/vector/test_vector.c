#include "util/vector/vector.h"
#include <assert.h>

int main() {
  // 1. Test default vector
  Vector *vec = VectorNew();
  assert(vec->size == 0);
  assert(vec->capacity == VECTOR_DEFAULT_CAPACITY);

  // 2. Test vector add (not exceed capacity)
  VectorAdd(vec, (void*)1);
  assert(vec->size == 1);
  assert(vec->arr[0] == (void*)1);
  VectorAdd(vec, (void*)2);
  assert(vec->size == 2);
  assert(vec->arr[0] == (void*)1);
  assert(vec->arr[1] == (void*)2);
  VectorAdd(vec, (void*)3);
  assert(vec->arr[0] == (void*)1);
  assert(vec->arr[1] == (void*)2);
  assert(vec->arr[2] == (void*)3);
  VectorDelete(vec);

  // 3. Test vector add (exceed capacity)
  vec = VectorNew();
  for (unsigned long long i = 0; i < VECTOR_DEFAULT_CAPACITY << 4; ++i)
    VectorAdd(vec, (void*)(i * 5 + 3));
  assert(vec->size == VECTOR_DEFAULT_CAPACITY << 4);
  for (unsigned long long i = 0; i < vec->size; ++i)
    assert(vec->arr[i] == (void*)(i * 5 + 3));
  VectorDelete(vec);
}
