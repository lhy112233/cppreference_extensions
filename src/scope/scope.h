#ifndef CPPREFERENCE_EXTENSIONS_SCOPE_H_
#define CPPREFERENCE_EXTENSIONS_SCOPE_H_
#include "impl/scope_impl.h"
#include <utility>

namespace hy {
/**
 * @brief
 *
 * @tparam EF
 */
template <typename EF> class scope_exit : private impl::scope_exit_impl<EF> {
  using impl_ = impl::scope_exit_impl<EF>;

public:
  template <typename Fn>
  explicit scope_exit(Fn &&fn) : impl_(std::forward<Fn>(fn)) {}

  scope_exit(scope_exit &&other) noexcept : impl_(std::move(other)) {}

  scope_exit(const scope_exit &) = delete;

  scope_exit &operator=(const scope_exit &) = delete;
  scope_exit &operator=(scope_exit &&) = delete;

  ~scope_exit() noexcept = default;

  void release() noexcept { impl_::release(); }
};

template <typename EF> class scope_fail : private impl::scope_fail_impl<EF> {
  using impl_ = impl::scope_fail_impl<EF>;

public:
  template <typename Fn>
  explicit scope_fail(Fn &&fn) : impl_(std::forward<Fn>(fn)) {}

  scope_fail(scope_fail &&other) noexcept : impl_(std::move(other)) {}

  scope_fail(const scope_fail &) = delete;

  scope_fail &operator=(const scope_fail &) = delete;
  scope_fail &operator=(scope_fail &&) = delete;

  ~scope_fail() noexcept = default;

  void release() noexcept { impl_::release(); }
};

template <typename EF>
class scope_success : private impl::scope_success_impl<EF> {
  using impl_ = impl::scope_success_impl<EF>;

public:
  template <typename Fn>
  explicit scope_success(Fn &&fn) : impl_(std::forward<Fn>(fn)) {}

  scope_success(scope_success &&other) noexcept : impl_(std::move(other)) {}

  scope_success(const scope_success &) = delete;

  scope_success &operator=(const scope_success &) = delete;
  scope_success &operator=(scope_success &&) = delete;

  ~scope_success() noexcept = default;

  void release() noexcept { impl_::release(); }
};

template <typename R, typename D>
class unique_resource : private impl::unique_resource_impl<R, D> {
  using impl_ = impl::unique_resource_impl<R, D>;

public:
// TODO
};

} // namespace hy

#endif // CPPREFERENCE_EXTENSIONS_SCOPE_H_