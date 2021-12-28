#include <stdbool.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "bitarray.h"

typedef int32_t word_t;

value bitarray_and_stub(value b1, value b2, value b3, intnat len) {
  bitarray_and((word_t*)(String_val(b1)),
               (word_t*)(String_val(b2)),
               (word_t*)(String_val(b3)),
               len);
  return Val_unit;
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

value bitarray_replicate_stub(value b1, intnat x, intnat y, intnat ct, intnat w,
                               intnat h, value b2, intnat len) {
  bitarray_replicate((word_t*)(String_val(b1)), x, y, ct, w, h,
                     (word_t*)(String_val(b2)), len);
  return Val_unit;
}
