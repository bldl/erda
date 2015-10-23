#include "program.hpp"
#include <climits>
#include <iostream>

template <typename U>
static void pr(char const* n,
	       Result<U> const& r) {
  std::cout << n << " = " << r << std::endl;
}

template <typename T, typename U>
static void pr(char const* n, Result<T> const& x,
	       Result<U> const& r) {
  std::cout << n << "(" << x << ") = " << r << std::endl;
}

int main(int argc, char *argv[])
{
  pr("INT_MIN", Good(INT_MIN));
  pr("INT_MAX", Good(INT_MAX));
  
  for (int i = -1; i < 15; i++) {
    auto v = Good(i);
    pr("factorial", v, factorial(v));
  }
  
  return 0;
}
