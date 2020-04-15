#ifdef MSTDLIB_H
#define MSTDLIB_H

template <typename T>
struct span {
  T *ptr;
  int len;

  bool operator==(const span<T> &rhs) {
    if (len != rhs.len) {
      return false;
    } else {
      bool ret = true;
      for (int i = 0; i < len; i++) {
        ret = ret && (ptr[i] == rhs.ptr[i]);
      }
      return ret;
    }
  }
};


#endif
