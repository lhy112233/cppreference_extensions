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
 * @brief unique_resource is universal RAII wrapper for resource handles that
 * owns and manages a resource through a handle and disposes of that resource
 * when the unique_resource is destroyed.
 *
 * @tparam R 	resource handle type
 * @tparam D  deleter type
 *
 * @requires (std::is_object_v<R> ||
 std::is_object_v<std::remove_reference_t<R>>) &&
 (std::is_move_constructible_v<std::remove_reference_t<R>> &&
 (std::is_copy_constructible_v<std::remove_reference_t<R>> ||
 std::is_nothrow_move_constructible_v<std::remove_reference_t<R> >))
 *
 */
template <typename R, typename D>
class unique_resource : private impl::unique_resource_impl<R, D> {
  using impl_ = typename impl::unique_resource_impl<R, D>;
  using RS = typename impl_::RS;

public:
  /**
   * @brief Default constructor.
   *
   * @details Value-initializes the stored resource handle and the deleter. The
   * constructed unique_resource does not own the resource.
   *
   * @requires std::is_default_constructible_v<R> &&
   * std::is_default_constructible_v<D>
   */
  unique_resource() = default;

  /**
   * @brief Constructs a unique_resource that owns a resource
   *
   * @tparam RR Resource argument type (deduced)
   * @tparam DD Deleter argument type (deduced)
   * @param r a resource handle
   * @param d a deleter to use to dispose the resource
   *
   * @throws Any exception thrown during initialization of the stored resource
   * handle or the deleter.
   *
   * @details  The stored resource handle is initialized with
                std::forward<RR>(r) if std::is_nothrow_constructible_v<RS, RR>
   is true, otherwise r. If initialization of the stored resource handle throws
   an exception, calls d(r). Then, the deleter is initialized with
   std::forward<DD>(d) if std::is_nothrow_constructible_v<D, DD> is true,
   otherwise d. If initialization of deleter throws an exception, calls d(res_).
   The constructed unique_resource owns the resource. The program is ill-formed
   if any of the expressions d(r), d(res_) and del_(res_) is ill-formed. The
   behavior is undefined if any of the expressions d(r), d(res_) and del_(res_)
   results in undefined behavior or throws an exception.
   *
   * @requires std::is_constructible_v<RS, RR> &&
  std::is_constructible_v<D, DD> &&
  (std::is_nothrow_constructible_v<RS, RR> || std::is_constructible_v<RS, RR&>)
  && (std::is_nothrow_constructible_v<D, DD> || std::is_constructible_v<D, DD&>)
   */
  template <class RR, class DD>
  unique_resource(RR &&r, DD &&d) noexcept(
      (std::is_nothrow_constructible_v<RS, RR> ||
       std::is_nothrow_constructible_v<RS, RR &>) &&
      (std::is_nothrow_constructible_v<D, DD> ||
       std::is_nothrow_constructible_v<D, DD &>))
      : impl_(std::forward<RR>(r), std::forward<DD>(d)) {}

  /**
   * @brief Move constructor.
   *
   * @param another unique_resource to acquire the ownership from
   *
   * @details The stored resource handle is initialized from the one of other,
              using std::move if std::is_nothrow_move_constructible_v<RS> is
              true. If initialization of the stored resource handle throws an
   exception, other is not modified. Then, the deleter is initialized with the
   one of other, using std::move if std::is_nothrow_move_constructible_v<D> is
   true. If initialization of the deleter throws an exception and
              std::is_nothrow_move_constructible_v<RS> is true and other owns
   the resource, calls the deleter of other with res_ to dispose the resource,
   then calls other.release(). After construction, the constructed
   unique_resource owns its resource if and only if other owned the resource
   before the construction, and other is set to not own the resource.
   *
   * @throws Any exception thrown during initialization of the stored resource
   handle or the deleter. exception description
   *
   * @note The mechanism of these constructors ensures no leaking of resources.
   */
  unique_resource(unique_resource &&other) noexcept(
      std::is_nothrow_move_constructible_v<RS> &&
      std::is_nothrow_move_constructible_v<D>)
      : impl_(std::move(other)) {}

  /**
   * @brief Destructor.
   *
   * @details Disposes the resource by calling the deleter with the underlying
   * resource handle if the unique_resource owns it, equivalent to calling
   * reset(). Then destroys the stored resource handle and the deleter.
   *
   */
  ~unique_resource() noexcept = default;

  /**
   * @brief Move assignment operator. Replaces the managed resource and the
   deleter with other's.
   *
   * @param other 	resource wrapper from which ownership will be
   transferred
   * @return *this
   *
   * @throws Any exception thrown in copy-assignment.
              noexcept specification:
              noexcept(std::is_nothrow_move_assignable_v<RS> &&
              std::is_nothrow_move_assignable_v<D>)
   *
   * @details Formally, let RS be the type of stored resource handle:
              First, calls reset() to dispose the currently owned resource, if
   any. Then assigns the stored resource handle and the deleter with other's.
   std::move is applied to the stored resource handle or the deleter of other if
              std::is_nothrow_move_assignable_v<RS> or
   std::is_nothrow_move_assignable_v<D> is true respectively. Assignment of the
   stored resource handle is executed first, unless
   std::is_nothrow_move_assignable_v<D> is false and
              std::is_nothrow_move_assignable_v<RS> is true. Finally, sets *this
   to own the resource if and only if other owned it before assignment, and
   other not to own the resource.
   *
   * @requires std::is_move_assignable_v<R> && std::is_move_assignable_v<D>
   *
   * @note If a copy of a member throws an exception, this mechanism leaves
   other intact and *this in the released state.
   */
  unique_resource &operator=(unique_resource &&other) noexcept(
      std::is_nothrow_move_assignable_v<RS> &&
      std::is_nothrow_move_assignable_v<D>) {
    impl_::operator=(std::move(other));
    return *this;
  }

  /**
   * @brief Releases the ownership of the managed resource if any.
   *
   * @details The destructor will not execute the deleter after the call, unless
   * reset is called later for managing new resource.
   *
   * @note Unlike std::unique_ptr::release, this function is not required to
   * modify the stored resource handle.
   */
  void release() noexcept { impl_::release(); }

  /**
   * @brief Disposes the resource by calling the deleter with the underlying
   * resource handle if the unique_resource owns it.
   *
   * @details The unique_resource does not own the resource after the call.
   */
  void reset() noexcept { impl_::reset(); }

  /**
   * @brief Replaces a new resource.
   *
   * @tparam RR Resource argument type (deduced)
   * @param r resource handle for a new resource to manage
   *
   * @throws Any exception thrown in assigning the stored resource handle.
   *
   * @requires  This overload participates in overload resolution only if the
   * selected assignment expression assigning the stored resource handle is
   * well-formed.
   *
   * @details Replaces the resource by calling (1) and then assigns the stored
   * resource handle with std::forward<RR>(r) if
   * std::is_nothrow_assignable_v<RS, RR> is true, otherwise std::as_const(r),
   * where RS is the type of stored resource handle. The unique_resource owns
   * the resource after the call.
   *  If copy-assignment of the store resource handle throws an exception, calls
   * del_(r), where del is the deleter object.
   *  The program is ill-formed if del_(r) is ill-formed.
   *  The behavior is undefined if del_(r) results in undefined behavior or
   * throws an exception.
   *
   * @note The mechanism ensures no leaking of resources.
   *
   */
  template <class RR> void reset(RR &&r) { impl_::reset(std::forward<RR>(r)); }

  /**
   * @brief Accesses the underlying resource handle.
   *
   * @return The underlying resource handle.
   */
  auto get() const noexcept -> const R & { return impl_::get(); }

  /**
   * @brief Accesses the deleter object which would be used for disposing the
   * managed resource.
   *
   * @return The stored deleter.
   */
  auto get_deleter() const noexcept -> const D & {
    return impl_::get_deleter();
  }

  /**
   * @brief  Access the object or function pointed by the underlying resource
   * handle which is a pointer.
   *
   * @return The object or function pointed by the underlying resource handle.
   *
   * @requires std::is_pointer_v<R> && !std::is_void_v<std::remove_pointer_t<R>>
   *
   * @details If the resource handle is not pointing to an object or a function,
   * the behavior is undefined.
   */
  auto operator*() const noexcept
      -> std::add_lvalue_reference_t<std::remove_pointer_t<R>> {
    return impl_::operator*();
  }
  /**
   * @brief Get a copy of the underlying resource handle which is a pointer.
   *
   * @return R
   *
   * @requires std::is_pointer_v<R>
   *
   * @details The return value is typically used to access the pointed object.
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
