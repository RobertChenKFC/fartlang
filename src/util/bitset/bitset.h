#ifndef BITSET_H
#define BITSET_H

#include <stdint.h>
#include <stdbool.h>

// Constants
// The size of word to use in a bitset
#define BITSET_WORD_T uint8_t
// The number of bits in each BITSET_WORD_T
#define BITSET_WORD_NBITS 8

// An array of words such that each bit of every words can be individually
// accessed
typedef struct {
  BITSET_WORD_T *words;
  int n;
} Bitset;

// Create a bit set of "n" bits full of zeros
Bitset *BitsetNew(int n);
// Delete a "b" created with BitsetNew; can be used as hash table destructors
void BitsetDelete(void *b);
// Set the "i"-th bit of "bitset" to one
void BitsetSet(Bitset *bitset, int i);
// Set the "i"-th bit of "bitset" to zero
void BitsetClear(Bitset *bitset, int i);
// Set the "i"-th bit of "bitset" to "val"; not that "val" is expected to be
// either zero or one, and passing any other value may mess up "bitset"
void BitsetSetVal(Bitset *bitset, int i, BITSET_WORD_T val);
// Check if the "i"-th bit of "bitset" is set to one or not
bool BitsetIsSet(Bitset *bitset, int i);
// A hash function for bitset "a"; used in hash tables
uint64_t BitsetHash(void *a);
// A equal function for bitsets "a" and "b"; used in hash tables
bool BitsetEqual(void *a, void *b);

#endif // BITSET_H
