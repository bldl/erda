/*

Based on these Clang "checked arithmetic builtins":

  bool __builtin_sadd_overflow  (int x, int y, int *sum);
  bool __builtin_ssub_overflow  (int x, int y, int *diff);
  bool __builtin_smul_overflow  (int x, int y, int *prod);

These require a fairly recent version of Clang, or a very recent
version of GCC (version 5 or so).

Note that "builtin functions must be directly called," i.e., no
function pointers to them.

*/

#include "program_config.hpp"

static bool sadd(int x, int y, int* sum) {
  return __builtin_sadd_overflow(x, y, sum);
}

static bool ssub(int x, int y, int* sum) {
  return __builtin_ssub_overflow(x, y, sum);
}

static bool smul(int x, int y, int* sum) {
  return __builtin_smul_overflow(x, y, sum);
}

static Result<int> compute_with(bool (*f)(int, int, int*),
				Result<int> const& x,
				Result<int> const& y) {
  if (is_Bad(x)) {
    return Bad(Nothing<Result<int>>(), AlertName("bad-arg"));
  } else if (is_Bad(y)) {
    return Bad(Nothing<Result<int>>(), AlertName("bad-arg"));
  } else {
    int r;
    bool err = (*f)(Good_v(x), Good_v(y), &r);
    if (err) {
      return Bad(Nothing<Result<int>>(), AlertName("overflow"));
    } else if (is_data_invariant(r)) {
      return Good(r);
    } else {
      return Bad(Just(Good(r)), AlertName("data-invariant"));
    }
  }
}

Result<int> add(Result<int> const& x, Result<int> const& y) {
  return compute_with(sadd, x, y);
}

Result<int> sub(Result<int> const& x, Result<int> const& y) {
  return compute_with(ssub, x, y);
}

Result<int> mul(Result<int> const& x, Result<int> const& y) {
  return compute_with(smul, x, y);
}
