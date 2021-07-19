#include "util/bitset/bitset.h"
#include <stdlib.h>

// Calculate the ceiling function of the quotient of two integers "p" and "q"
#define CEIL(p, q) ((p) / (q) + ((p) % (q) == 0 ? 0 : 1))

Bitset *BitsetNew(int n) {
  Bitset *bitset = malloc(sizeof(Bitset));
  int numWords = CEIL(n, BITSET_WORD_NBITS);
  bitset->words = calloc(numWords, sizeof(BITSET_WORD_T));
  bitset->n = n;
  return bitset;
}

void BitsetDelete(void *b) {
  Bitset *bitset = b;
  free(bitset->words);
  free(bitset);
}

void BitsetSet(Bitset *bitset, int i) {
  int wordIdx = i / BITSET_WORD_NBITS;
  int bitIdx = i % BITSET_WORD_NBITS;
  bitset->words[wordIdx] |= ((BITSET_WORD_T)1) << bitIdx;
}

void BitsetClear(Bitset *bitset, int i) {
  int wordIdx = i / BITSET_WORD_NBITS;
  int bitIdx = i % BITSET_WORD_NBITS;
  bitset->words[wordIdx] &= ~(((BITSET_WORD_T)1) << bitIdx);
}

void BitsetSetVal(Bitset *bitset, int i, BITSET_WORD_T val) {
  int wordIdx = i / BITSET_WORD_NBITS;
  int bitIdx = i % BITSET_WORD_NBITS;
  bitset->words[wordIdx] =
      bitset->words[wordIdx] & (~(((BITSET_WORD_T)1) << bitIdx))
                             | (((BITSET_WORD_T)val) << bitIdx);
}

bool BitsetIsSet(Bitset *bitset, int i) {
  int wordIdx = i / BITSET_WORD_NBITS;
  int bitIdx = i % BITSET_WORD_NBITS;
  return (bitset->words[wordIdx] >> bitIdx) & ((BITSET_WORD_T)1);
}

uint64_t BitsetHash(void *a) {
  Bitset *bitset = a;
  uint64_t hash = (uint64_t)bitset->n;
  int numWords = CEIL(bitset->n, BITSET_WORD_NBITS);
  int numWordsPerHashSize = sizeof(hash) / sizeof(BITSET_WORD_T);
  for (int i = 0; i < numWords; ++i)
    hash ^= ((uint64_t)bitset->words[i]) <<
            ((i % numWordsPerHashSize) * BITSET_WORD_NBITS);
  return hash;
}

bool BitsetEqual(void *a, void *b) {
  const Bitset *bitsetA = a, *bitsetB = b;
  if (bitsetA->n != bitsetB->n)
    return false;
  int numWords = CEIL(bitsetA->n, BITSET_WORD_NBITS);
  for (int i = 0; i < numWords; ++i) {
    if (bitsetA->words[i] != bitsetB->words[i])
      return false;
  }
  return true;
}
