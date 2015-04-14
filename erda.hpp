#ifndef __erda_hpp__
#define __erda_hpp__

#include <assert.h>
#include <memory>

// --------------------------------------------------
// data invariant
// --------------------------------------------------

template <typename T>
bool is_data_invariant(T const& v) {
  return true;
}

// --------------------------------------------------
// option type
// --------------------------------------------------

template <typename T>
struct Maybe {
  //Maybe() : m_is_Just(false) {}
  bool m_is_Just;
  T m_v;
};

template <typename T>
Maybe<T> Just(T const& v) {
  return Maybe<T> { true, v };
}

template <typename T>
bool is_Just(Maybe<T> const& w) {
  return w.m_is_Just;
}

template <typename T>
T Just_v(Maybe<T> const& w) {
  assert(w.m_is_Just);
  return w.m_v;
}

template <typename T>
Maybe<T> Nothing() {
  Maybe<T> w;
  w.m_is_Just = false;
  return w;
}

template <typename T>
bool is_Nothing(Maybe<T> const& w) {
  return !(w.m_is_Just);
}

// --------------------------------------------------
// wrapper type
// --------------------------------------------------

template <typename T>
struct Result {
  //Result() : m_is_Good(false), m_Bad_v(nullptr) {}
  bool m_is_Good;
  T m_Good_v;
  std::shared_ptr<Result<T>> m_Bad_v; // nullable
};

template <typename T>
Result<T> Good(T const& v) {
  Result<T> w;
  w.m_is_Good = true;
  w.m_Good_v = v;
  return w;
}

template <typename T>
bool is_Good(Result<T> const& w) {
  return w.m_is_Good;
}

template <typename T>
T Good_v(Result<T> const& w) {
  assert(w.m_is_Good);
  return w.m_Good_v;
}

template <typename T>
Result<T> Bad(Maybe<Result<T>> const& mwv) {
  Result<T> w;
  w.m_is_Good = false;
  if (is_Just(mwv))
    // requires Result<T> to have a copy ctor
    w.m_Bad_v = std::make_shared<Result<T>>(Just_v(mwv));
  return w;
}

template <typename T>
bool is_Bad(Result<T> const& w) {
  return !(w.m_is_Good);
}

template <typename T>
Maybe<Result<T>> Bad_v(Result<T> const& w) {
  assert(!(w.m_is_Good));
  if (w.m_Bad_v)
    return Just<Result<T>>(*(w.m_Bad_v));
  else
    return Nothing<Result<T>>();
}

#endif /* __erda_hpp__ */
