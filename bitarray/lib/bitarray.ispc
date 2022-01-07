typedef int32 word_t;

export void bitarray_and(uniform word_t a[], uniform word_t b[], uniform word_t c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] & b[i];
  }
}

export void bitarray_or(uniform word_t a[], uniform word_t b[], uniform word_t c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] | b[i];
  }
}

export void bitarray_xor(uniform word_t a[], uniform word_t b[], uniform word_t c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] ^ b[i];
  }
}

export void bitarray_not(uniform word_t a[], uniform word_t b[], uniform int len) {
  foreach (i = 0 ... len) {
    b[i] = ~a[i];
  }
}

export uniform bool bitarray_any(uniform word_t a[], uniform int len) {
  bool part = false;
  foreach (i = 0 ... len) {
    part &= popcnt((int32)a[i]) > 0;
  }
  return any(part);
}

export uniform int bitarray_hamming_weight(uniform word_t a[], uniform int len) {
  uniform int part = 0;
  for (uniform int i = 0; i < len; i++) {
    part += popcnt(a[i]);
  }
  return part;
}

export uniform int bitarray_hamming_distance(uniform word_t a[], uniform word_t b[], uniform int len) {
  uniform int part = 0;
  for (uniform int i = 0; i < len; i++) {
    part += popcnt((a[i] ^ b[i]));
  }
  return part;
}

inline bool read_bit(const uniform word_t x[], const int b, const uniform int len) {
  int i = b / 32;
  int j = b % 32;
  if (i < 0 || i >= len) {
    return false;
  } else {
    return ((x[i] >> j) & 1);
  }
}

inline int offset(int x, int y, uniform int w, uniform int h) {
  return ((h - 1 - y) * w) + x;
}

export void bitarray_replicate(const uniform word_t a[],
                               const uniform int dx,
                               const uniform int dy,
                               const uniform int ct,
                               const uniform int w,
                               const uniform int h,
                               uniform word_t r[],
                               const uniform int len)
{
  foreach (word_idx = 0 ... len) {
    int start_bit = 32 * word_idx;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);
    word_t word = a[word_idx];
    for (uniform int b = 0; b < 32; b++) {
      bool bit = false;
      int xx = x;
      int yy = y;
      for (uniform int c = 0; c < ct && xx >= 0 && xx < w && yy >= 0 && yy < h; c++) {
        bit |= read_bit(a, offset(xx, yy, w, h), len);
        xx -= dx;
        yy -= dy;
      }
      word |= ((int)bit) << b;
      y = (x == w - 1) ? y - 1 : y;
      x = (x == w - 1) ? 0 : (x + 1);
    }
    r[word_idx] = word;
  }
}

/* export uniform int bitarray_hash(const uniform int32 data[],  */
/*                                  const uniform int32 key[], */
/*                                  const uniform int len)  */
/* { */
/*   int64 ret; */
/*   foreach (i = 0 ... len / 2) { */
/*     ret = (data[2 * i] + key[2 * i]) * (data[2 * i + 1] + key[2 * i + 1]); */
/*   } */
/*   return reduce_add(ret); */
/* } */

/* export uniform int bitarray_hash(uniform int32 data[], uniform int32 key[], uniform int len) { */
/*     int64 sum = 0; */
/*     for (uniform int i = 0; i < len; i += 2) { */
/*         int32 d0; */
/*         int32 d1; */
/*         aos_to_soa2(&data[i], &d0, &d1); */
/*         int32 k0; */
/*         int32 k1; */
/*         aos_to_soa2(&key[i], &k0, &k1); */
/*         sum += (int64)(d0 + k0) * (int64)(d1 + k1); */
/*     } */
/*     return reduce_add(sum); */
/* } */

export uniform int bitarray_hash(uniform int32 data[], uniform int32 key[], uniform int len) {
    int64 sum = 0;
    for (uniform int i = 0; i < len; i += programCount) {
        int32 d0 = data[programIndex];
        int32 d1 = (&data[i + programCount])[programIndex];
        int32 k0 = key[programIndex];
        int32 k1 = (&key[i + programCount])[programIndex];

        #if TARGET_WIDTH == 4
            const varying int vPerm = { 1, 3, 5, 7 };
        #elif TARGET_WIDTH == 8
            const varying int vPerm = { 1, 3, 5, 7, 9, 11, 13, 15 };
        #elif TARGET_WIDTH == 16
            const varying int vPerm = { 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 };
        #endif

        int32 d0x = shuffle(d0, d1, vPerm - 1);
        int32 d1x = shuffle(d0, d1, vPerm);
        int32 k0x = shuffle(k0, k1, vPerm - 1);
        int32 k1x = shuffle(k0, k1, vPerm);
        sum += (int64)(d0x * k0x) + (int64)(d1x * k1x);
    }
    return reduce_add(sum);
}
