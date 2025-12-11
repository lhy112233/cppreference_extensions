#ifndef CPPREFERENCE_EXTENSIONS_SCOPE_H_
#define CPPREFERENCE_EXTENSIONS_SCOPE_H_
#include "impl/scope_impl.h"
#include <utility>

namespace hy {
/* *********************************** */
// class scope_exit
/* *********************************** */
/**
 * @brief class scope_exit
 *
 * @tparam EF
 */
template <typename EF> class scope_exit : private impl::scope_exit_impl<EF> {
  using impl_ = impl::scope_exit_impl<EF>;

public:
  /**
   * @brief Construct a new scope exit object
   *
   * @tparam Fn
   * @param fn
   */
  template <typename Fn>
  explicit scope_exit(Fn &&fn) : impl_(std::forward<Fn>(fn)) {}

  scope_exit(scope_exit &&other) noexcept : impl_(std::move(other)) {}

  scope_exit(const scope_exit &) = delete;

  scope_exit &operator=(const scope_exit &) = delete;
  scope_exit &operator=(scope_exit &&) = delete;

  ~scope_exit() noexcept = default;

  /**
   * @brief
   *
   */
  void release() noexcept { impl_::release(); }
};

template <class EF> scope_exit(EF) -> scope_exit<EF>;

/* *********************************** */
// class scope_fail
/* *********************************** */
/**
 * @brief class scope_fail
 *
 * @tparam EF
 */
template <typename EF> class scope_fail : private impl::scope_fail_impl<EF> {
  using impl_ = impl::scope_fail_impl<EF>;

public:
  /**
   * @brief Construct a new scope fail object
   *
   * @tparam Fn
   * @param fn
   */
  template <typename Fn>
  explicit scope_fail(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)
      : impl_(std::forward<Fn>(fn)) {}

  scope_fail(scope_fail &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
      : impl_(std::move(other)) {}

  scope_fail(const scope_fail &) = delete;

  scope_fail &operator=(const scope_fail &) = delete;
  scope_fail &operator=(scope_fail &&) = delete;

  ~scope_fail() noexcept = default;

  /**
   * @brief
   *
   */
  void release() noexcept { impl_::release(); }
};

template <class EF> scope_fail(EF) -> scope_fail<EF>;

/* *********************************** */
// class scope_success
/* *********************************** */
/**
 * @brief class scope_success
 *
 * @tparam EF
 */
template <typename EF>
class scope_success : private impl::scope_success_impl<EF> {
  using impl_ = impl::scope_success_impl<EF>;

public:
  /**
   * @brief Construct a new scope success object
   *
   * @tparam Fn
   * @param fn
   */
  template <typename Fn>
  explicit scope_success(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)
      : impl_(std::forward<Fn>(fn)) {}

  scope_success(scope_success &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
      : impl_(std::move(other)) {}

  scope_success(const scope_success &) = delete;

  scope_success &operator=(const scope_success &) = delete;
  scope_success &operator=(scope_success &&) = delete;

  ~scope_success() noexcept(noexcept(std::declval<EF &>()())) = default;

  /**
   * @brief
   *
   */
  void release() noexcept { impl_::release(); }
};

/* *********************************** */
// class unique_resource
/* *********************************** */
/**
 * @brief class unique_resource
 *
 * @tparam R
 * @tparam D
 */
template <typename R, typename D>
class unique_resource : private impl::unique_resource_impl<R, D> {
  using impl_ = typename impl::unique_resource_impl<R, D>;
  using RS = typename impl_::RS;

public:
  unique_resource() = default;

  /**
   * @brief Construct a new unique resource object
   *
   * @tparam RR
   * @tparam DD
   * @param r
   * @param d
   */
  template <class RR, class DD>
  unique_resource(RR &&r, DD &&d) noexcept(
      (std::is_nothrow_constructible_v<RS, RR> ||
       std::is_nothrow_constructible_v<RS, RR &>) &&
      (std::is_nothrow_constructible_v<D, DD> ||
       std::is_nothrow_constructible_v<D, DD &>))
      : impl_(std::forward<RR>(r), std::forward<DD>(d)) {}

  /**
   * @brief Construct a new unique resource object
   *
   * @param other
   */
  unique_resource(unique_resource &&other) noexcept(
      std::is_nothrow_move_constructible_v<RS> &&
      std::is_nothrow_move_constructible_v<D>)
      : impl_(std::move(other)) {}

  ~unique_resource() noexcept = default;

  unique_resource &operator=(unique_resource &&other) noexcept(
      std::is_nothrow_move_assignable_v<RS> &&
      std::is_nothrow_move_assignable_v<D>) {
    impl_::operator=(std::move(other));
    return *this;
  }

  /**
   * @brief Releases the ownership of the managed resource if any. The
   * destructor will not execute the deleter after the call, unless reset is
   * called later for managing new resource.
   * Unlike std::unique_ptr::release, this function is not required to modify
   * the stored resource handle.
   */
  void release() noexcept { impl_::release(); }

  /**
   * @brief Disposes the resource by calling the deleter with the underlying
   * resource handle if the unique_resource owns it. The unique_resource does
   * not own the resource after the call.
   *
   */
  void reset() noexcept { impl_::reset(); }

  /**
   * @brief
   *
   * @tparam RR
   * @param r
   */
  template <class RR> void reset(RR &&r) { impl_::reset(std::forward<RR>(r)); }

  /**
   * @brief
   *
   * @return const R&
   */
  auto get() const noexcept -> const R & { return impl_::get(); }

  /**
   * @brief Get the deleter object
   *
   * @return const D&
   */
  auto get_deleter() const noexcept -> const D & {
    return impl_::get_deleter();
  }

  /**
   * @brief
   *
   * @return std::add_lvalue_reference_t<std::remove_pointer_t<R>>
   */
  auto operator*() const noexcept
      -> std::add_lvalue_reference_t<std::remove_pointer_t<R>> {
    return impl_::operator*();
  }
  /**
   * @brief
   *
   * @return R
   */
  auto operator->() const noexcept -> R { return impl_::operator->(); }
};

template <class R, class D> unique_resource(R, D) -> unique_resource<R, D>;

/**
 * @brief make_unique_resource_checked
 *
 * @tparam R
 * @tparam D
 * @tparam S
 * @param r
 * @param invalid
 * @param d
 * @return unique_resource<std::decay_t<R>, std::decay_t<D>>
 */
template <class R, class D, class S = std::decay_t<R>>
auto make_unique_resource_checked(R &&r, const S &invalid, D &&d) noexcept(
    std::is_nothrow_constructible_v<std::decay_t<R>, R> &&
    std::is_nothrow_constructible_v<std::decay_t<D>, D>)
    -> unique_resource<std::decay_t<R>, std::decay_t<D>> {
  auto unique_res = unique_resource<std::decay_t<R>, std::decay_t<D>>{
      std::forward<R>(r), std::forward<D>(d)};

  if (bool(r == invalid)) {
    unique_res.release();
  }

  return unique_res;
}

} // namespace hy

#endif // CPPREFERENCE_EXTENSIONS_SCOPE_H_
