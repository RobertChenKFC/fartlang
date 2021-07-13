#ifndef VECTOR_H
#define VECTOR_H

// An array-like structure that dynamically grows in size as more elements are
// add to it
#define VECTOR_DEFAULT_CAPACITY 64
typedef struct {
  void **arr;
  int size, capacity;
} Vector;

// Creates a new vector
Vector *VectorNew();
// Deletes a "vec" created by VectorNew
void VectorDelete(Vector *vec);
// Adds "elem" to the end of "vec"
void VectorAdd(Vector *vec, void *elem);

#endif // VECTOR_H
