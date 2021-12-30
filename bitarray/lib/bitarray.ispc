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
  int part = 0;
  foreach (i = 0 ... len) {
    part += popcnt((int32)a[i]);
  }
  return reduce_add(part);
}

export uniform int bitarray_hamming_distance(uniform word_t a[], uniform word_t b[], uniform int len) {
  int part = 0;
  foreach (i = 0 ... len) {
    part += popcnt((int32)(a[i] ^ b[i]));
  }
  return reduce_add(part);
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
    int end_bit = start_bit + 32 - 1;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);
    word_t word = a[word_idx];
    for (int b = start_bit; b < end_bit; b++) {
      if (x < 0 || x >= w || y < 0 || y >= h) {
        break;
      }
      bool bit = false;
      for (int c = 0; c < ct; c++) {
        bit = bit || read_bit(a, offset(x - c * dx, y - c * dy, w, h), len);
      }
      word |= ((int)bit) << (b % 32);
      y = (x == w - 1) ? y - 1 : y;
      x = (x == w - 1) ? 0 : (x + 1);
    }
    r[word_idx] = word;
  }
}

