#include "util/vector/vector.h"
#include <assert.h>
#include <stdlib.h>

Vector *VectorNew() {
  return VectorNewWithCapacity(VECTOR_DEFAULT_CAPACITY);
}

Vector *VectorNewWithCapacity(int capacity) {
  Vector *vec = malloc(sizeof(Vector));
  capacity = capacity == 0 ? 1 : capacity;
  vec->arr = malloc(sizeof(void*) * capacity);
  vec->size = 0;
  vec->capacity = capacity;
  return vec;
}

void VectorDelete(Vector *vec) {
  free(vec->arr);
  free(vec);
}

void VectorAdd(Vector *vec, void *elem) {
  if (vec->capacity == vec->size) {
    vec->capacity *= 2;
    vec->arr = realloc(vec->arr, sizeof(void*) * vec->capacity);
  }
  vec->arr[vec->size++] = elem;
}

void VectorReserve(Vector *vec, int capacity) {
  if (vec->capacity < capacity) {
    vec->capacity = capacity;
    vec->arr = realloc(vec->arr, sizeof(void*) * vec->capacity);
  }
}

void VectorClear(Vector *vec) {
  vec->size = 0;
}

void *VectorPop(Vector *vec) {
  assert(vec->size > 0);
  return vec->arr[--vec->size];
}
