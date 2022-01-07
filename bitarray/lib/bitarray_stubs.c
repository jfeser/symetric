#include <stdbool.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "bitarray.h"

typedef int32_t word_t;

value bitarray_and_stub(value b1, value b2, value b3, intnat len) {
  for (int i = 0; i < len; i++) {
    ((word_t*) b3)[i] = ((word_t*)b1)[i] & ((word_t*)b2)[i];
  }
}

value bitarray_or_stub(value b1, value b2, value b3, intnat len) {
  bitarray_or((word_t*)(String_val(b1)),
              (word_t*)(String_val(b2)),
              (word_t*)(String_val(b3)),
              len);
  return Val_unit;
}

value bitarray_xor_stub(value b1, value b2, value b3, intnat len) {
  bitarray_xor((word_t*)(String_val(b1)),
               (word_t*)(String_val(b2)),
               (word_t*)(String_val(b3)),
               len);
  return Val_unit;
}

value bitarray_any_stub(value b, intnat len) {
  return Val_bool(bitarray_any((word_t*)(String_val(b)), len));
}

value bitarray_not_stub(value b1, value b2, intnat len) {
  bitarray_not((word_t*)(String_val(b1)), (word_t*)(String_val(b2)), len);
  return Val_unit;
}

intnat bitarray_hamming_weight_stub(value b, intnat len) {
  return bitarray_hamming_weight((word_t*)(String_val(b)), len);
}

intnat bitarray_hamming_distance_stub(value b1, value b2, intnat len) {
  return bitarray_hamming_distance((word_t*)(String_val(b1)),
                                   (word_t*)(String_val(b2)),
                                   len);
}

CAMLprim value bitarray_replicate_stub(value b1, intnat x, intnat y, intnat ct, intnat w,
                                       intnat h, value b2, intnat len) {
  bitarray_replicate((String_val(b1)), x, y, ct, w, h, (String_val(b2)), len);
  return Val_unit;
}

CAMLprim intnat bitarray_hash_stub(value d, value k, intnat len) {
  int *dd = (int*)d;
  int *kk = (int*)k;
  long sum = 0;
  for (int i = 0; i < len; i += 2) {
    sum += (dd[i] + kk[i]) * (dd[i + 1] + kk[i + 1]);
  }
  return sum;
  /* return bitarray_hash(String_val(d), String_val(k), len); */
}
