#ifndef __erda_hpp__
#define __erda_hpp__

#include <assert.h>
#include <iostream>
#include <memory>

// --------------------------------------------------
// symbol type
// --------------------------------------------------

struct AlertName {
  AlertName() = default;
  explicit AlertName(char const* s) : m_s(s) {}
  std::string m_s;
};

inline std::ostream& operator<<(std::ostream& os, AlertName const& sym) {
  return os << sym.m_s;
}

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
  Maybe() = default;
  Maybe(Maybe<T> const&) = default;
  Maybe(Maybe<T>&&) = default;
  Maybe<T>& operator=(Maybe<T> const&) = default;
  Maybe<T>& operator=(Maybe<T>&&) = default;
  
  explicit Maybe(T const& v) : m_is_Just(true), m_v(v) {}

  bool m_is_Just = false;
  T m_v;
};

template <typename T>
Maybe<T> Just(T const& v) {
  return Maybe<T>(v);
}

template <typename T>
T Just_v(Maybe<T> const& w) {
  assert(w.m_is_Just);
  return w.m_v;
}

template <typename T>
bool is_Just(Maybe<T> const& w) {
  return w.m_is_Just;
}

template <typename T>
Maybe<T> Nothing() {
  return Maybe<T>();
}

template <typename T>
bool is_Nothing(Maybe<T> const& w) {
  return !(w.m_is_Just);
}

template <typename T>
std::ostream& operator<<(std::ostream& os, Maybe<T> const& w) {
  if (w.m_is_Just) {
    os << "Just(" << w.m_v << ")";
  } else {
    os << "Nothing()";
  }
  return os;
}

// --------------------------------------------------
// wrapper type
// --------------------------------------------------

template <typename T>
struct Result {
  enum State { BAD_NO_VAL, GOOD, BAD_GOOD_VAL, BAD_BAD_VAL };

  Result() = default;
  Result(Result<T> const&) = default;
  Result(Result<T>&&) = default;
  Result<T>& operator=(Result<T> const&) = default;
  Result<T>& operator=(Result<T>&&) = default;

  explicit Result(T const& v) : m_State(GOOD), m_Good_v(v) {}
  
  State m_State = BAD_NO_VAL;
  T m_Good_v; // for GOOD and BAD_GOOD_VAL
  std::shared_ptr<Result<T>> m_Bad_v; // for BAD_BAD_VAL
  AlertName m_name = AlertName("undefined");
};

template <typename T>
Result<T> Good(T const& v) {
  return Result<T>(v);
}

template <typename T>
bool is_Good(Result<T> const& w) {
  return w.m_State == Result<T>::GOOD;
}

template <typename T>
T Good_v(Result<T> const& w) {
  assert(is_Good(w));
  return w.m_Good_v;
}

template <typename T>
Result<T> Bad(Maybe<Result<T>> const& mwv, AlertName const& name) {
  if (is_Nothing(mwv)) {
    Result<T> w;
    w.m_State = Result<T>::BAD_NO_VAL;
    w.m_name = name;
    return w;
  } else {
    Result<T> inner_r = Just_v(mwv);
    if (is_Good(inner_r)) {
      Result<T> w;
      w.m_State = Result<T>::BAD_GOOD_VAL;
      w.m_Good_v = Good_v(inner_r);
      w.m_name = name;
      return w;
    } else {
      Result<T> w;
      w.m_State = Result<T>::BAD_BAD_VAL;
      // requires Result<T> to have a copy ctor
      w.m_Bad_v = std::make_shared<Result<T>>(inner_r);
      w.m_name = name;
      return w;
    }
  }
}

template <typename T>
bool is_Bad(Result<T> const& w) {
  return !(is_Good(w));
}

template <typename T>
Maybe<Result<T>> Bad_v(Result<T> const& w) {
  switch (w.m_State) {
  case Result<T>::BAD_GOOD_VAL:
    return Just<Result<T>>(Good(w.m_Good_v));
  case Result<T>::BAD_BAD_VAL:
    return Just<Result<T>>(*(w.m_Bad_v));
  default:
    assert(w.m_State == Result<T>::BAD_NO_VAL);
    return Nothing<Result<T>>();
  }
}

template <typename T>
AlertName Bad_name(Result<T> const& w) {
  return w.m_name;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, Result<T> const& w) {
  switch (w.m_State) {
  case Result<T>::GOOD:
    os << "Good(" << w.m_Good_v << ")"; break;
  case Result<T>::BAD_NO_VAL:
    os << "Bad(Nothing()," << w.m_name << ")"; break;
  case Result<T>::BAD_GOOD_VAL:
    os << "Bad(Just(Good(" << w.m_Good_v << "))," << w.m_name << ")"; break;
  case Result<T>::BAD_BAD_VAL:
    os << "Bad(Just(" << *(w.m_Bad_v) << ")," << w.m_name << ")"; break;
  default:
    assert(0 && "unexpected Result<T>::State");
  }
  return os;
}

#endif /* __erda_hpp__ */
