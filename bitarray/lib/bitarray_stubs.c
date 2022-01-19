#include <stdbool.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "bitarray.h"

typedef int32_t word_t;

#define len(s) (caml_string_length(s) / (sizeof(word_t)))

value bitarray_and_stub(value b1, value b2, value b3) {
  for (int i = 0; i < len(b1); i++) {
    ((word_t*) b3)[i] = ((word_t*)b1)[i] & ((word_t*)b2)[i];
  }
  return Val_unit;
}

value bitarray_or_stub(value b1, value b2, value b3) {
  for (int i = 0; i < len(b1); i++) {
    ((word_t*) b3)[i] = ((word_t*)b1)[i] | ((word_t*)b2)[i];
  }
  return Val_unit;
}

value bitarray_xor_stub(value b1, value b2, value b3) {
  for (int i = 0; i < len(b1); i++) {
    ((word_t*) b3)[i] = ((word_t*)b1)[i] ^ ((word_t*)b2)[i];
  }
  return Val_unit;
}

value bitarray_any_stub(value b) {
  return Val_bool(bitarray_any((word_t*)(String_val(b)), len(b)));
}

value bitarray_not_stub(value b1, value b2) {
  for (int i = 0; i < len(b1); i++) {
    ((word_t*) b2)[i] = ~((word_t*)b1)[i];
  }
  return Val_unit;
}

intnat bitarray_hamming_weight_stub(value b) {
  return bitarray_hamming_weight((word_t*)(String_val(b)), len(b));
}

intnat bitarray_hamming_distance_stub(value b1, value b2) {
  int slen = len(b1);
  int count = 0;
  #pragma GCC unroll 8
  for (int i = 0; i < slen; i++) {
    count += __builtin_popcount(((word_t*)b1)[i] ^ ((word_t*)b2)[i]);
  }
  return count;
}

CAMLprim value bitarray_replicate_stub(value b1, intnat x, intnat y, intnat ct, intnat w, intnat h, value b2) {
  bitarray_replicate((String_val(b1)), x, y, ct, w, h, (Bytes_val(b2)), len(b1));
  return Val_unit;
}

CAMLprim intnat bitarray_hash_stub(value d, value k) {
  int *dd = (int*)d;
  int *kk = (int*)k;
  long sum = 0;
  for (int i = 0; i < len(d); i += 2) {
    sum += (long)(dd[i] + kk[i]) * (long)(dd[i + 1] + kk[i + 1]);
  }
  return sum;
}
