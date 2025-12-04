#include "scope.h"
#include <functional>
#include <print>

template <> class hy::scope_fail<std::function<void()>>;

auto main() -> int {
  std::println("Testing start--------------");

  // Normal construction
  {
    auto f = [] { std::println("Scope exited!"); };
    hy::scope_exit<decltype(f)> exit(f);
  }

  // Defuced construction
  // {
  //   hy::scope_exit exit([] { std::println("Scope exited with deduced type!");
  //   });
  // }

  {
    hy::scope_exit<std::function<void()>> exit(
        [] { std::println("Scope exited with std::function!"); });
  }

  // Move construction
  {
    auto f = [] { std::println("Scope exited after move!"); };
    hy::scope_exit<decltype(f)> exit1(f);
    hy::scope_exit<decltype(f)> exit2(std::move(exit1));
  }

  // Release test
  {
    auto f = [] { std::println("This should not be printed!"); };
    hy::scope_exit<decltype(f)> exit(f);
    exit.release();
  }

  std::println("Testing end----------------");
  return 0;
}