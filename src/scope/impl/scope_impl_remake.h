#ifndef CPPREFERENCE_EXTENTIONS_SRC_SCOPE_IMPL_SCOPE_IMPL_REMAKE_H_
#define CPPREFERENCE_EXTENTIONS_SRC_SCOPE_IMPL_SCOPE_IMPL_REMAKE_H_
#include <cassert>
#include <exception>
#include <functional>
#include <type_traits>
#include <utility>

namespace hy::impl {

/**************************************/
// class BoxRollbackHelper
/**************************************/
template <typename F>
  requires(std::is_destructible_v<F> && std::is_object_v<F> &&
           requires(F f) {
             { std::invoke(f) };
           })
class BoxRollbackHelper : public F {
public:
  template <typename... Args>
    requires std::is_nothrow_constructible_v<F, Args...> &&
                 std::is_nothrow_destructible_v<F>
  BoxRollbackHelper(Args &&...args) noexcept
      : F(std::forward<Args>(args)...), released_(false) {}

  BoxRollbackHelper(BoxRollbackHelper &&other) noexcept(noexcept(F{
      std::move(other)}))
      : F(static_cast<F &&>(other)), released_(other.released_) {
    other.released_ = true;
  }

  ~BoxRollbackHelper() noexcept(
      noexcept(std::invoke(static_cast<std::remove_reference_t<F> &>(*this)))) {
    if (!released_) {
      std::invoke(static_cast<std::remove_reference_t<F> &>(*this));
    }
  }

  void release() noexcept { released_ = true; }

private:
  bool released_;
}; // class BoxRollbackHelper

template <typename F> BoxRollbackHelper(F) -> BoxRollbackHelper<F>;

/**************************************/
// class EFBox
/**************************************/
template <typename EF>
concept ScopeEFConcept =
    ((std::is_destructible_v<EF> && std::is_object_v<EF>) ||
     (std::is_lvalue_reference_v<EF> &&
      std::is_object_v<std::remove_reference_t<EF>>) ||
     (std::is_function_v<std::remove_reference_t<EF>> &&
      std::is_lvalue_reference_v<EF>)) &&
    requires(std::remove_reference_t<EF> ef) {
      { std::invoke(ef) };
    };

template <ScopeEFConcept EF> class EFBox {
public:
  template <typename Helper, typename... Args>
  EFBox(Args &&...args, BoxRollbackHelper<Helper> helper) noexcept(noexcept(EF{
      std::forward<Args>(args)...}))
      : ef_(std::forward<Args>(args)...) {
    helper.release();
  }

  operator EF &() noexcept { return ef_; }
  operator const EF &() const noexcept { return ef_; }

private:
  [[no_unique_address]] EF ef_;
}; // class EFBox

template <ScopeEFConcept EF>
class EFBox<EF &> : public std::reference_wrapper<EF> {
public:
  template <typename Helper, typename... Args>
  EFBox(Args &&...args, BoxRollbackHelper<Helper> helper) noexcept(
      noexcept(std::reference_wrapper<EF>{std::forward<Args>(args)...}))
      : std::reference_wrapper<EF>(std::forward<Args>(args)...) {
    helper.release();
  }
}; // class EFBox<EF &>

/**************************************/
// class BaseScopeImpl
/**************************************/
template <ScopeEFConcept EF> class BaseScopeImpl : public EFBox<EF> {

public:
  template <typename Fn, typename Helper>
    requires(!std::is_same_v<std::remove_cvref_t<Fn>, BaseScopeImpl<EF>>) &&
            std::is_constructible_v<EF, Fn> &&
            (!std::is_lvalue_reference_v<Fn> &&
             std::is_nothrow_constructible_v<EF, Fn>)
  explicit BaseScopeImpl(Fn &&fn,
                         Helper &&helper = BoxRollbackHelper{[&] {
                           std::invoke(fn);
                         }}) noexcept(std::is_nothrow_constructible_v<EF, Fn> ||
                                      std::is_nothrow_constructible_v<EF, Fn &>)
      : EFBox<EF>(std::forward<Fn>(fn), std::forward<Helper>(helper)) {}

  template <typename Fn, typename Helper>
    requires(!std::is_same_v<std::remove_cvref_t<Fn>, decltype(*this)>) &&
            std::is_constructible_v<EF, Fn>
  explicit BaseScopeImpl(Fn &&fn,
                         Helper &&helper = BoxRollbackHelper{[&] {
                           std::invoke(fn);
                         }}) noexcept(std::is_nothrow_constructible_v<EF, Fn> ||
                                      std::is_nothrow_constructible_v<EF, Fn &>)
      : EFBox<EF>(fn, std::forward<Helper>(helper)) {}

  BaseScopeImpl(BaseScopeImpl &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
    requires(std::is_nothrow_move_constructible_v<EF> ||
             std::is_copy_constructible_v<EF>) &&
            std::is_nothrow_move_constructible_v<EF>
      : EFBox<EF>(static_cast<EFBox<EF> &&>(other)) {}

  BaseScopeImpl(BaseScopeImpl &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
    requires(std::is_nothrow_move_constructible_v<EF> ||
             std::is_copy_constructible_v<EF>)
      : EFBox<EF>(static_cast<EFBox<EF> &>(other)) {}

  BaseScopeImpl(const BaseScopeImpl &) = delete;

  ~BaseScopeImpl() noexcept = default;

  BaseScopeImpl &operator=(const BaseScopeImpl &) = delete;
  BaseScopeImpl &operator=(BaseScopeImpl &&) = delete;
}; // class BaseScopeImpl

/**************************************/
// class scope_exit_impl
/**************************************/
template <ScopeEFConcept EF> class scope_exit_impl : private BaseScopeImpl<EF> {
  using base_ = BaseScopeImpl<EF>;

public:
  template <typename Fn>
  explicit scope_exit_impl(Fn &&fn) noexcept(
      noexcept(base_(std::forward<Fn>(fn))))
      : base_(std::forward<Fn>(fn)), released_(false) {}

  scope_exit_impl(scope_exit_impl &&other) noexcept(noexcept(base_{
      std::move(other)}))
      : base_(static_cast<base_ &&>(other)), released_(other.released_) {
    other.release();
  }

  scope_exit_impl(const scope_exit_impl &) = delete;
  scope_exit_impl &operator=(const scope_exit_impl &) = delete;
  scope_exit_impl &operator=(scope_exit_impl &&) = delete;

  ~scope_exit_impl() noexcept {
    if (!released_) {
      std::invoke(*this);
    }
  }

  void release() noexcept { released_ = true; }

private:
  bool released_;
};

/**************************************/
// class scope_fail_impl
/**************************************/
template <ScopeEFConcept EF> class scope_fail_impl : private BaseScopeImpl<EF> {
  using base_ = BaseScopeImpl<EF>;

public:
  template <typename Fn>
  explicit scope_fail_impl(Fn &&fn) noexcept(
      noexcept(base_(std::forward<Fn>(fn))))
      : base_(std::forward<Fn>(fn)),
        exception_count_(std::uncaught_exceptions()) {}

  scope_fail_impl(scope_fail_impl &&other) noexcept(noexcept(base_{
      std::move(other)}))
      : base_(static_cast<base_ &&>(other)),
        exception_count_(other.exception_count_) {
    other.release();
  }

  scope_fail_impl(const scope_fail_impl &) = delete;
  scope_fail_impl &operator=(const scope_fail_impl &) = delete;
  scope_fail_impl &operator=(scope_fail_impl &&) = delete;

  ~scope_fail_impl() noexcept {
    if (std::uncaught_exceptions() > exception_count_ &&
        exception_count_ != -1) {
      std::invoke(*this);
    }
  }

  void release() noexcept { exception_count_ = -1; }

private:
  int exception_count_;
};

/**************************************/
// class scope_success_impl
/**************************************/
template <ScopeEFConcept EF>
class scope_success_impl : private BaseScopeImpl<EF> {
  using base_ = BaseScopeImpl<EF>;

public:
  template <typename Fn>
  explicit scope_success_impl(Fn &&fn) noexcept(
      noexcept(base_(std::forward<Fn>(fn))))
      : base_(std::forward<Fn>(fn)),
        exception_count_(std::uncaught_exceptions()) {}

  scope_success_impl(scope_success_impl &&other) noexcept(noexcept(base_{
      std::move(other)}))
      : base_(static_cast<base_ &&>(other)),
        exception_count_(other.exception_count_) {
    other.release();
  }

  scope_success_impl(const scope_success_impl &) = delete;
  scope_success_impl &operator=(const scope_success_impl &) = delete;
  scope_success_impl &operator=(scope_success_impl &&) = delete;

  ~scope_success_impl() noexcept {
    if (std::uncaught_exceptions() <= exception_count_) {
      std::invoke(*this);
    }
  }

  void release() noexcept { exception_count_ = -1; }

private:
  int exception_count_;
};

/**************************************/
// class unique_resource_impl
/**************************************/
template <typename R>
concept UniqueResourceRConcept =
    (std::is_lvalue_reference_v<R> || std::is_object_v<R>) &&
    (std::is_copy_constructible_v<std::remove_reference_t<R>> ||
     std::is_nothrow_move_constructible_v<std::remove_reference_t<R>>);

template <typename D, UniqueResourceRConcept R>
concept UniqueResourceDConcept =
    (std::is_destructible_v<D> && std::is_move_constructible_v<D> &&
     std::is_object_v<D> && std::is_invocable_v<D, R>) &&
    (std::is_copy_constructible_v<D> ||
     std::is_nothrow_move_constructible_v<D>) &&
    std::is_invocable_v<D &, std::remove_reference_t<R> &>;

template <UniqueResourceRConcept R>
  requires std::is_object_v<R>
class UniqueResourceData {
public:
  using RS = R;

  template <typename... Args, typename Helper>
    requires std::is_nothrow_constructible_v<R, Args...>
  UniqueResourceData(Args &&...args, BoxRollbackHelper<Helper> helper) noexcept
      : r_(std::forward<Args>(args)...) {
    helper.release();
  }

  template <typename... Args, typename Helper>
  UniqueResourceData(Args &&...args, BoxRollbackHelper<Helper> helper)
      : r_(args...) {
    helper.release();
  }

  operator R &() noexcept { return r_; }
  operator const R &() const noexcept { return r_; }

private:
  [[no_unique_address]] R r_;
};

template <UniqueResourceRConcept R>
class UniqueResourceData<R &> : public std::reference_wrapper<R> {
public:
  using RS = std::reference_wrapper<std::remove_reference_t<R>>;
  template <typename... Args, typename Helper>
  UniqueResourceData(Args &&...args, BoxRollbackHelper<Helper> helper) noexcept(
      noexcept(std::reference_wrapper<R>{std::forward<Args>(args)...}))
      : std::reference_wrapper<R>(std::forward<Args>(args)...) {
    helper.release();
  }
};

template <UniqueResourceDConcept D> class UniqueResourceDeleter {
public:
  template <typename... Args, typename Helper>
    requires std::is_nothrow_constructible_v<D, Args...>
  UniqueResourceDeleter(Args &&...args,
                        BoxRollbackHelper<Helper> helper) noexcept
      : d_(std::forward<Args>(args)...) {
    helper.release();
  }

  template <typename... Args, typename Helper>
  UniqueResourceDeleter(Args &&...args, BoxRollbackHelper<Helper> helper)
      : d_(args...) {
    helper.release();
  }

  operator D &() noexcept { return d_; }
  operator const D &() const noexcept { return d_; }

  template <typename... Args>
  auto operator()(Args &&...args) noexcept(noexcept(
      std::invoke(static_cast<D &>(d_), std::forward<Args>(args)...))) {
    return d_(std::forward<Args>(args)...);
  }

private:
  [[no_unique_address]] D d_;
};

template <UniqueResourceRConcept R, UniqueResourceDConcept<R> D>
class unique_resource_impl : private UniqueResourceData<R>,
                             private UniqueResourceDeleter<D> {
  using resource_t = R;
  using deleter_t = D;
  using RS = typename UniqueResourceData<R>::RS;

public:
  unique_resource_impl()
    requires std::is_default_constructible_v<R> &&
                 std::is_default_constructible_v<D>
  = default;

  template <class RR, class DD>
  unique_resource_impl(RR &&r, DD &&d) noexcept(
      (std::is_nothrow_constructible_v<RS, RR> ||
       std::is_nothrow_constructible_v<RS, RR &>) &&
      (std::is_nothrow_constructible_v<D, DD> ||
       std::is_nothrow_constructible_v<D, DD &>))
    requires(std::is_constructible_v<RS, RR>) &&
                std::is_constructible_v<D, DD> &&
                (std::is_nothrow_constructible_v<RS, RR> ||
                 std::is_constructible_v<RS, RR &>) &&
                (std::is_nothrow_constructible_v<D, DD> ||
                 std::is_constructible_v<D, DD &>)
      : UniqueResourceData<R>(std::forward<RR>(r), BoxRollbackHelper{[&d, &r] {
                                std::invoke(d, r);
                              }}),
        UniqueResourceDeleter<D>(
            std::forward<DD>(d),
            [&d, this] { std::invoke(d, static_cast<RS &>(*this)); }),
        released_(false) {}

  unique_resource_impl(unique_resource_impl &&other) noexcept(
      std::is_nothrow_move_constructible_v<R> &&
      std::is_nothrow_move_constructible_v<D>)
      : UniqueResourceData<R>(static_cast<UniqueResourceData<R> &&>(other),
                              [] {}),
        UniqueResourceDeleter<D>(
            static_cast<UniqueResourceDeleter<D> &&>(other),
            [&other, this] {
              if constexpr (std::is_nothrow_move_constructible_v<R>)
                static_cast<UniqueResourceData<R> &>(other) =
                    static_cast<UniqueResourceData<R> &&>(*this);
              else
                std::invoke(static_cast<D &>(other), static_cast<RS &>(*this));
            }),
        released_(other.released_) {
    other.release();
  }

  ~unique_resource_impl() noexcept { reset(); }

  unique_resource_impl &operator=(unique_resource_impl &&other) noexcept(
      std::is_nothrow_move_assignable_v<RS> &&
      std::is_nothrow_move_assignable_v<D>) {
    if (this == &other)
      return *this;

    reset();

    if constexpr (!std::is_nothrow_move_assignable_v<D> &&
                  !std::is_nothrow_move_assignable_v<RS>) {
      if constexpr (std::is_nothrow_move_constructible_v<D>) {
        static_cast<D &>(*this) = static_cast<D &&>(other);
      } else {
        static_cast<D &>(*this) = static_cast<D &>(other);
      }

      if constexpr (std::is_nothrow_move_constructible_v<RS>) {
        static_cast<RS &>(*this) = static_cast<RS &&>(other);
      } else {
        try {
          static_cast<RS &>(*this) = static_cast<RS &>(other);
        } catch (...) {
          if constexpr (std::is_nothrow_move_constructible_v<D>)
            static_cast<D &>(other) = static_cast<D &&>(*this);
          throw;
        }
      }

    } else {
      if constexpr (std::is_nothrow_move_assignable_v<RS>)
        static_cast<RS &>(*this) = static_cast<RS &&>(other);
      else
        static_cast<RS &>(*this) = static_cast<RS &>(other);

      if constexpr (std::is_nothrow_move_assignable_v<D>)
        static_cast<D &>(*this) = static_cast<D &&>(other);
      else {
        try {
          static_cast<D &>(*this) = static_cast<D &>(other);
        } catch (...) {
          if constexpr (std::is_nothrow_move_assignable_v<RS>)
            static_cast<RS &>(other) = static_cast<RS &&>(*this);
          else
            std::invoke(static_cast<D &>(other), static_cast<RS &>(*this));
          throw;
        }
      }
    }

    released_ = other.released_;
    other.release();
    return *this;
  }

  auto release() noexcept -> void { released_ = true; }

  auto reset() noexcept -> void {
    if (!released_) {
      std::invoke(static_cast<D &>(*this), static_cast<R &>(*this));
    }
    release();
  }

  template <typename RR> auto reset(RR &&new_resource) -> void {
    reset();

    if constexpr (std::is_nothrow_assignable_v<RS, RR>) {
      static_cast<R &>(*this) = std::forward<RR>(new_resource);
    } else {
      try {
        static_cast<R &>(*this) = std::as_const(new_resource);
      } catch (...) {
        std::invoke(static_cast<D &>(*this), new_resource);
        throw;
      }
    }
  }

  auto get() const noexcept -> R & { return static_cast<R &>(*this); }
  auto get_deleter() noexcept -> D & { return static_cast<D &>(*this); }

  auto operator*() const noexcept
      -> std::add_lvalue_reference_t<std::remove_pointer_t<R>>
    requires std::is_pointer_v<R> && !std::is_void_v<std::remove_pointer_t<R>>
  {
    return *static_cast<R &>(*this);
  }
  auto operator->() const noexcept -> R
    requires std::is_pointer_v<R>
  {
    return static_cast<R &>(*this);
  }

private:
  bool released_;
};
} // namespace hy::impl

#endif // CPPREFERENCE_EXTENTIONS_SRC_SCOPE_IMPL_SCOPE_IMPL_REMAKE_H_