#ifndef __program_config_hpp__
#define __program_config_hpp__

// Define to disable `assert`.
//#define NDEBUG 1

#define MGL_API_PROTO
#define MGL_API_FUNC
#define MGL_PROTO static
#define MGL_FUNC static

#include "erda.hpp"

inline bool equ(int x, int y) {
  return x == y;
}

inline int sub(int x, int y) {
  return x - y;
}

#endif /* __program_config_hpp__ */
