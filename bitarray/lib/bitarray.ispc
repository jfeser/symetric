export void bitarray_and(uniform int32 a[], uniform int32 b[], uniform int32 c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] & b[i];
  }
}

export void bitarray_or(uniform int32 a[], uniform int32 b[], uniform int32 c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] | b[i];
  }
}

export void bitarray_xor(uniform int32 a[], uniform int32 b[], uniform int32 c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] ^ b[i];
  }
}

export void bitarray_not(uniform int32 a[], uniform int32 b[], uniform int len) {
  foreach (i = 0 ... len) {
    b[i] = ~a[i];
  }
}

export uniform bool bitarray_any(uniform int32 a[], uniform int len) {
  bool part = false;
  foreach (i = 0 ... len) {
    part &= popcnt(a[i]) > 0;
  }
  return any(part);
}

export uniform int bitarray_hamming_weight(uniform int32 a[], uniform int len) {
  int part = 0;
  foreach (i = 0 ... len) {
    part += popcnt(a[i]);
  }
  return reduce_add(part);
}

export uniform int bitarray_hamming_distance(uniform int32 a[], uniform int32 b[], uniform int len) {
  int part = 0;
  foreach (i = 0 ... len) {
    part += popcnt(a[i] ^ b[i]);
  }
  return reduce_add(part);
}

inline bool read_bit(uniform int32 x[], int b, uniform int len) {
  int i = b / 32;
  int j = b % 32;
  return i >= 0 && i < len && (x[i] >> j) & 1;
}

inline void update_bit(uniform int32 x[], int b, bool v) {
  x[b/32] = x[b/32] | (((int)v) << (b % 32));
}

inline int offset(int x, int y, uniform int w, uniform int h) {
  return ((h - 1 - y) * w) + x;
}

export void bitarray_replicate(uniform int32 a[], 
                               uniform int dx, uniform int dy, uniform int ct, uniform int w, uniform int h,
                               uniform int32 r[], uniform int len) {
  foreach (word = 0 ... len) {
    int start_bit = 32 * word;
    int end_bit = start_bit + 32 - 1;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);
    for (int b = start_bit; b < end_bit; b++) {
      if (x >= 0 && x < w && y >= 0 && y < h) {
        bool bit = false;
        for (int c = 0; c < ct; c++) {
          bit |= read_bit(a, offset(x - c * dx, y - c * dy, w, h), len);
        }
        update_bit(r, offset(x, y, w, h), bit);
      }
      y = (x == w - 1) ? y - 1 : y;
      x = (x == w - 1) ? 0 : (x + 1);
    }
  }
}
