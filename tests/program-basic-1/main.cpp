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
  auto one = Good(1);
  pr("one", one);
  
  auto two = Good(2);
  pr("two", two);

  auto bad_one = Bad(Just(one));
  pr("bad_one", bad_one);
  
  Result<int> w = run(one, two);
  pr("w", w);

  return 0;
}
