#include "program.hpp"
#include <iostream>

template <typename T>
static void pr(char const* n, Result<T> const& w) {
  std::cout << n << " = " << w << std::endl;
}

int main(int argc, char *argv[])
{
  auto five = Good(5);
  pr("5", five);
  
  auto two = Good(2);
  pr("2", two);

  auto zero = Good(0);
  pr("0", zero);
  
  Result<int> three = run(five, two);
  pr("5 - 2", three);

  pr("0 - 5", run(zero, five));
  pr("5 - 0", run(five, zero));
  pr("5 - 5", run(five, five));
  
  return 0;
}
