#include "util/vector/vector.h"
#include <stdlib.h>

Vector *VectorNew() {
  Vector *vec = malloc(sizeof(Vector));
  vec->arr = malloc(sizeof(void*) * VECTOR_DEFAULT_CAPACITY);
  vec->size = 0;
  vec->capacity = VECTOR_DEFAULT_CAPACITY;
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
