#include "program.hpp"
#include <iostream>

template <typename T>
static void pr(char const* n, Result<T> const& w) {
  if (is_Good(w)) {
    std::cout << n << " = Good(" << Good_v(w) << ")" << std::endl;
  } else if (is_Bad(w)) {
    std::cout << n << " = Bad(....)" << std::endl;
  } else {
    assert(0 && "neither Good nor Bad");
  }
}

int main(int argc, char *argv[])
{
  auto v1 = Good(5);
  pr("v1", v1);
  
  auto v2 = Good(2);
  pr("v2", v2);

  auto zero = Good(0);
  pr("zero", zero);
  
  Result<int> w = run(v1, v2);
  pr("5 - 2", w);

  pr("5 - 0", run(v1, zero));
  pr("5 - 5", run(v1, v1));
  
  return 0;
}
