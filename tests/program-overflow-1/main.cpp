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

template <typename T, typename U>
static void pr(char const* n, Result<T> const& x,
	       Result<T> const& y, Result<U> const& r) {
  std::cout << n << "(" << x << ", " << y << ") = " << r << std::endl;
}

int main(int argc, char *argv[])
{
  pr("INT_MIN", Good(INT_MIN));
  pr("INT_MAX", Good(INT_MAX));
  
  int r = 0;
  bool err =__builtin_sadd_overflow(4, 5, &r);
  std::cout << "sadd r =" << err << "= " << r << std::endl;
  err =__builtin_sadd_overflow(2147483640, 9, &r);
  std::cout << "sadd r =" << err << "= " << r << std::endl;

  Result<int> x;
  Result<int> y;

  x = Good(7);
  y = Good(7);
  pr("add", x, y, add(x, y));
  pr("sub", x, y, sub(x, y));

  y = Good(9);
  pr("add", x, y, add(x, y));
  
  x = Good(2147483640);
  pr("add", x, y, add(x, y));

  x = Good(INT_MAX / 2);
  pr("twice", x, twice(x));

  x = Good(INT_MAX - 20);
  pr("twice", x, twice(x));

  x = Good(2);
  pr("add_is_mul", x, x, add_is_mul(x, x));
  
  y = Good(3);
  pr("add_is_mul", x, y, add_is_mul(x, y));
  
  x = Good(2);
  y = Good(INT_MAX / 2);
  pr("add_is_mul", x, y, add_is_mul(x, y));
  y = Good(INT_MAX / 2 + 2);
  
  pr("add_is_sub", x, y, add_is_sub(x, y));
  pr("add_is_mul", x, y, add_is_mul(x, y));
  pr("add_is_sub_is_mul", x, y, add_is_sub_is_mul(x, y));
  x = y = Good(1);
  pr("add_is_sub_is_mul", x, y, add_is_sub_is_mul(x, y));
  x = y = Good(0);
  pr("add_is_sub_is_mul", x, y, add_is_sub_is_mul(x, y));
  
  return 0;
}
