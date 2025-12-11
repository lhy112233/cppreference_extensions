#ifndef CPPREFERENCE_EXTENSIONS_SCOPE_IMPL_H_
#define CPPREFERENCE_EXTENSIONS_SCOPE_IMPL_H_

#include <cassert>
#include <cstddef>
#include <exception>
#include <functional>
#include <type_traits>
#include <utility>

namespace hy::impl {

/**************************************/
// class scope_exit_impl
/**************************************/
namespace {
// Type requirements
//  -EF shall be either:
//  a Destructible FunctionObject type, --1-1
//  an lvalue reference to FunctionObject,  --1-2
//  an lvalue reference to function.  --1-3
//  -Calling an lvalue of std::remove_reference_t<EF> with no argument shall be
//  well-formed.  --2
template <typename ScopeExitEF>
concept scope_exit_ef_concept =
    // 1
    requires(std::remove_reference_t<ScopeExitEF> ef) {
      { std::invoke(ef) };
    } &&
    (
        // 1-1
        (std::is_object_v<ScopeExitEF> &&
         std::is_destructible_v<ScopeExitEF>) ||
        // 1-2
        (std::is_lvalue_reference_v<ScopeExitEF> &&
         std::is_object_v<std::remove_reference_t<ScopeExitEF>> &&
         std::is_destructible_v<std::remove_reference_t<ScopeExitEF>>) ||
        // 1-3
        (std::is_function_v<ScopeExitEF> &&
         std::is_lvalue_reference_v<ScopeExitEF>));

template <typename EF, typename Fn, typename Self>
concept scope_exit_construct_overload_concept =
    !std::is_same_v<std::remove_cvref_t<Fn>, Self> &&
    std::is_constructible_v<EF, Fn>;

} // namespace

/**
 * @brief This is the implementation of scope_exit
 *
 * @tparam EF
 */
template <scope_exit_ef_concept EF> class scope_exit_impl {
  using exitfun = EF;

public:
  template <typename Fn>
    requires scope_exit_construct_overload_concept<EF, Fn, scope_exit_impl>
  explicit scope_exit_impl(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)
      : dismissed_(false) {
    if constexpr ((!std::is_lvalue_reference_v<EF>) &&
                  std::is_nothrow_constructible_v<EF, Fn>) {
      ::new (storage_) EF(std::forward<Fn>(fn));
    } else {
      try {
        ::new (storage_) EF(fn);
      } catch (...) {
        std::invoke(fn);
        throw;
      }
    }
  }

  scope_exit_impl(scope_exit_impl &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
    requires(std::is_nothrow_move_constructible_v<EF> ||
             std::is_copy_constructible_v<EF>)

      : dismissed_(other.dismissed_) {

    static_assert(!std::is_nothrow_move_constructible_v<EF> ||
                      std::is_move_constructible_v<EF>,
                  "This is Undefined Behavior!");
    static_assert(std::is_nothrow_move_constructible_v<EF> ||
                      std::is_copy_constructible_v<EF>,
                  "This is Undefined Behavior!");

    auto &other_ef = *std::launder(reinterpret_cast<exitfun *>(other.storage_));

    if constexpr (std::is_nothrow_move_constructible_v<EF>) {
      ::new (storage_) EF(std::forward<EF>(other_ef));
    } else {
      ::new (storage_) EF(other_ef);
    }

    other.release();
  }

  scope_exit_impl(const scope_exit_impl &) = delete;
  scope_exit_impl &operator=(const scope_exit_impl &) = delete;
  scope_exit_impl &operator=(scope_exit_impl &&) = delete;

  ~scope_exit_impl() noexcept {
    auto ef = std::launder(reinterpret_cast<exitfun *>(storage_));

    if (!dismissed_) {
      try {
        std::invoke(*ef);
      } catch (...) {
        assert("This is Undefined behavior!");
      }
    }

    ef->~exitfun();
  }

  void release() noexcept { dismissed_ = true; }

private:
  alignas(alignof(EF)) std::byte storage_[sizeof(EF)];
  bool dismissed_;
}; // class scope_exit_impl

/**************************************/
// class scope_fail_impl
/**************************************/
namespace {
template <typename EF>
concept scope_fail_ef_concept = scope_exit_ef_concept<EF>;

template <typename EF, typename Fn, typename Self>
concept scope_fail_construct_overload_concept =
    !std::is_same_v<std::remove_cvref_t<Fn>, Self> &&
    std::is_constructible_v<EF, Fn>;

template <typename EF>
concept scope_fail_move_construct_overload_concept =
    std::is_nothrow_move_constructible_v<EF> ||
    std::is_copy_constructible_v<EF>;
} // namespace

/**
 * @brief This is the implementation of scope_fail
 *
 * @tparam EF
 */
template <scope_fail_ef_concept EF> class scope_fail_impl {
public:
  template <typename Fn>
    requires scope_fail_construct_overload_concept<EF, Fn, scope_fail_impl<EF>>
  explicit scope_fail_impl(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)

      : exception_count_(std::uncaught_exceptions()) {

    if constexpr (!std::is_lvalue_reference_v<Fn> &&
                  std::is_nothrow_constructible_v<EF, Fn>) {
      ::new (storage_) EF(std::forward<Fn>(fn));
    } else {
      try {
        ::new (storage_) EF(fn);
      } catch (...) {
        std::invoke(fn);
        throw;
      }
    }
  }

  scope_fail_impl(EF &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
    requires scope_fail_move_construct_overload_concept<EF>

      : exception_count_(other.exception_count_) {

    static_assert(!std::is_nothrow_move_constructible_v<EF> ||
                      std::is_move_constructible_v<EF>,
                  "This is Undefined Behavior!");
    static_assert(std::is_nothrow_move_constructible_v<EF> ||
                      std::is_copy_constructible_v<EF>,
                  "This is Undefined Behavior!");

    auto &other_ef = *std::launder(reinterpret_cast<EF *>(other.storage_));

    if constexpr (std::is_nothrow_move_constructible_v<EF>) {
      ::new (storage_) EF(std::forward<EF>(other_ef));
    } else {
      ::new (storage_) EF(other_ef);
    }

    other.release();
  }

  scope_fail_impl(const scope_fail_impl &) = delete;

  ~scope_fail_impl() noexcept {
    auto ef = std::launder(reinterpret_cast<EF *>(storage_));

    if (std::uncaught_exceptions() > exception_count_ && !is_released()) {
      try {
        std::invoke(*ef);
      } catch (...) {
        assert("This is Undefined behavior!");
      }
    }

    ef->~EF();
  }

  scope_fail_impl &operator=(const scope_fail_impl &) = delete;
  scope_fail_impl &operator=(scope_fail_impl &&) = delete;

  void release() noexcept { exception_count_ = -1; }

private:
  int exception_count_;
  alignas(alignof(EF)) std::byte storage_[sizeof(EF)];

  [[nodiscard]] bool is_released() const noexcept { return exception_count_ == -1; }

}; // class scope_fail_impl

/**************************************/
// class scope_success_impl
/**************************************/
namespace {
template <typename EF>
concept scope_success_ef_concept = scope_exit_ef_concept<EF>;

template <typename EF, typename Fn, typename Self>
concept scope_success_construct_overload_concept =
    !std::is_same_v<std::remove_cvref_t<Fn>, Self> &&
    std::is_constructible_v<EF, Fn>;

template <typename EF>
concept scope_success_move_construct_overload_concept =
    std::is_nothrow_move_constructible_v<EF> ||
    std::is_copy_constructible_v<EF>;
} // namespace

/**
 * @brief This is the implementation of scope_success
 *
 * @tparam EF
 */
template <scope_success_ef_concept EF> class scope_success_impl {
public:
  template <typename Fn>
    requires scope_success_construct_overload_concept<EF, Fn,
                                                      scope_success_impl<EF>>
  explicit scope_success_impl(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)

      : exception_count_(std::uncaught_exceptions()) {

    if constexpr ((!std::is_lvalue_reference_v<EF>) &&
                  std::is_nothrow_constructible_v<EF, Fn>) {
      ::new (storage_) EF(std::forward<Fn>(fn));
    } else {
      try {
        ::new (storage_) EF(fn);
      } catch (...) {
        std::invoke(fn);
        throw;
      }
    }
  }

  scope_success_impl(EF &&other) noexcept(
      std::is_nothrow_move_constructible_v<EF> ||
      std::is_nothrow_copy_constructible_v<EF>)
    requires scope_success_move_construct_overload_concept<EF>
      : exception_count_(other.exception_count_) {
    static_assert(!std::is_nothrow_move_constructible_v<EF> ||
                      std::is_move_constructible_v<EF>,
                  "This is Undefined Behavior!");

    static_assert(std::is_nothrow_move_constructible_v<EF> ||
                      std::is_copy_constructible_v<EF>,
                  "This is Undefined Behavior!");

    auto &other_ef = *std::launder(reinterpret_cast<EF *>(other.storage_));

    if constexpr (std::is_nothrow_move_constructible_v<EF>) {
      ::new (storage_) EF(std::forward<EF>(other_ef));
    } else {
      ::new (storage_) EF(other_ef);
    }
    other.release();
  }

  scope_success_impl(const scope_success_impl &) = delete;

  scope_success_impl &operator=(const scope_success_impl &) = delete;
  scope_success_impl &operator=(scope_success_impl &&) = delete;

  ~scope_success_impl() noexcept(noexcept(std::declval<EF &>()())) {
    auto ef = std::launder(reinterpret_cast<EF *>(storage_));

    if (std::uncaught_exceptions() <= exception_count_ && !is_released()) {
      try {
        std::invoke(*ef);
      } catch (...) {
        ef->~EF();
        throw;
      }
    }

    ef->~EF();
  }

  void release() noexcept { exception_count_ = -1; }

private:
  int exception_count_;
  alignas(alignof(EF)) std::byte storage_[sizeof(EF)];

  [[nodiscard]] bool is_released() const noexcept {
    return exception_count_ == -1;
  }
}; // class scope_success_impl

/**************************************/
// class unique_resource_impl
/**************************************/
namespace {
template <typename R>
concept unique_resource_R_concept =
    (std::is_object_v<R> || (std::is_lvalue_reference_v<R> &&
                             std::is_object_v<std::remove_reference_t<R>>)) &&
    (std::is_move_constructible_v<std::remove_reference_t<R>>) &&
    (std::is_copy_constructible_v<std::remove_reference_t<R>> ||
     std::is_nothrow_move_constructible_v<std::remove_reference_t<R>>);

template <typename D, typename R>
concept unique_resource_D_concept =
    (std::is_destructible_v<D> && std::is_move_constructible_v<D> &&
     std::is_object_v<D>) &&
    (std::is_copy_constructible_v<D> ||
     std::is_nothrow_move_constructible_v<R>) &&
    (requires(D &d, std::remove_reference_t<R> &r) { d(r); });
} // namespace

template <unique_resource_R_concept R, unique_resource_D_concept<R> D>
class unique_resource_impl {
  using RS =
      std::conditional<std::is_object_v<R>, R,
                       std::reference_wrapper<std::remove_reference_t<R>>>;
  using UnrefR = std::remove_reference_t<R>;

public:
  unique_resource_impl()
    requires(std::is_default_constructible_v<R> &&
             std::is_default_constructible_v<D>)
      : engaged_(false) {
    ::new (resource_) RS();
    ::new (deleter_) D();
  };

  template <class RR, class DD>
  unique_resource_impl(RR &&r, DD &&d) noexcept(
      (

          std::is_nothrow_constructible_v<RS, RR> ||
          std::is_nothrow_constructible_v<RS, RR &>) &&
      (std::is_nothrow_constructible_v<D, DD> ||
       std::is_nothrow_constructible_v<D, DD &>))
    requires((std::is_constructible_v<RS, RR>) &&
             (std::is_constructible_v<D, DD>) &&
             (std::is_nothrow_constructible_v<RS, RR> ||
              std::is_constructible_v<RS, RR &>) &&
             (std::is_nothrow_constructible_v<D, DD> ||
              std::is_constructible_v<D, DD &>))
      : engaged_(true) {
    if constexpr (std::is_nothrow_constructible_v<RS, RR>) {
      ::new (resource_) RS(std::forward<RR>(r));
    } else {
      try {
        ::new (resource_) RS(r);
      } catch (...) {
        d(r);
        throw;
      }
    }

    if constexpr (std::is_nothrow_constructible_v<D, DD>) {
      ::new (deleter_) D(std::forward<DD>(d));
    } else {
      try {
        ::new (deleter_) D(d);
      } catch (...) {
        std::forward<DD>(d)(*get_resource_helper());
        get_resource_helper()->~RS();
        throw;
      }
    }
  }

  unique_resource_impl(unique_resource_impl &&other) noexcept(
      std::is_nothrow_move_constructible_v<RS> &&
      std::is_nothrow_move_constructible_v<D>)
      : engaged_(other.engaged_) {
    if (other.engaged_) {
      if constexpr (std::is_nothrow_move_constructible_v<RS>) {
        ::new (resource_) RS(std::move(*other.get_resource_helper()));
      } else {
        ::new (resource_) RS(*other.get_resource_helper());
      }
    }

    if constexpr (std::is_nothrow_move_constructible_v<D>) {
      ::new (deleter_) D(std::move(*other.get_deleter_helper()));
    } else {
      try {
        ::new (deleter_) D(*other.get_deleter_helper());
      } catch (...) {
        if constexpr (std::is_nothrow_move_constructible_v<RS>) {
          if (other.engaged_) {
            other.get_deleter_helper()->operator()(*get_resource_helper());
            get_resource_helper()->~RS();

            other.release();
          }
        }
        throw;
      }
    }

    other.release();
  }

  ~unique_resource_impl() {
    reset();
    get_resource_helper()->~RS();
    get_deleter_helper()->~D();
  }

  unique_resource_impl &operator=(unique_resource_impl &&other) noexcept(
      std::is_nothrow_move_assignable_v<RS> &&
      std::is_nothrow_move_assignable_v<D>) {
    static_assert((!std::is_nothrow_move_assignable_v<RS> ||
                   std::is_move_assignable_v<RS>) ||
                      std::is_copy_constructible_v<RS>,
                  "This is Undefined Behavior!");
    static_assert((!std::is_nothrow_move_assignable_v<D> ||
                   std::is_move_assignable_v<D>) ||
                      std::is_copy_constructible_v<D>,
                  "This is Undefined Behavior!");

    if (this == &other)
      return *this;

    reset();

    if constexpr (!std::is_nothrow_move_assignable_v<D> &&
                  std::is_nothrow_move_assignable_v<RS>) {
      if constexpr (std::is_nothrow_move_assignable_v<RS>) {
        *get_resource_helper() = std::move(*other.get_resource_helper());
      } else {
        *get_resource_helper() = *other.get_resource_helper();
      }

      try {
        if constexpr (std::is_nothrow_move_assignable_v<D>) {
          *get_deleter_helper() = std::move(*other.get_deleter_helper());
        } else {
          *get_deleter_helper() = *other.get_deleter_helper();
        }
      } catch (...) {
        if constexpr (std::is_nothrow_move_assignable_v<RS>) {
          *other.get_resource_helper() = std::move(*get_resource_helper());
        } else {
          other.get_deleter_helper()->operator()(*get_resource_helper());
        }
        throw;
      }

    } else {

      if constexpr (std::is_nothrow_move_assignable_v<D>) {
        *get_deleter_helper() = std::move(*other.get_deleter_helper());
      } else {
        *get_deleter_helper() = *other.get_deleter_helper();
      }

      try {
        if constexpr (std::is_nothrow_move_assignable_v<RS>) {
          *get_resource_helper() = std::move(*other.get_resource_helper());
        } else {
          *get_resource_helper() = *other.get_resource_helper();
        }
      } catch (...) {
        if constexpr (std::is_nothrow_move_assignable_v<D>) {
          *other.get_deleter_helper() = std::move(*get_deleter_helper());
        }
        throw;
      }
    }

    engaged_ = other.engaged_;
    other.release();
    return *this;
  }

  void release() noexcept { engaged_ = false; }

  void reset() noexcept {
    if (engaged_) {
      get_deleter_helper()->operator()(*get_resource_helper());

      engaged_ = false;
    }
  }

  template <typename RR> void reset(RR &&r) {
    reset();

    try {
      if constexpr (std::is_nothrow_assignable_v<RS, RR>) {
        *get_resource_helper() = std::forward<RR>(r);

      } else {
        *get_resource_helper() = r;
      }
    } catch (...) {
      get_deleter_helper()->operator()(r);
      throw;
    }

    engaged_ = true;
  }

  [[nodiscard]] auto get() const noexcept -> const R & {
    return *get_resource_helper();
  }

  [[nodiscard]] auto get_deleter() const noexcept -> const D & {
    return *get_deleter_helper();
  }

  [[nodiscard]] auto operator*() const noexcept
      -> std::add_lvalue_reference_t<std::remove_pointer_t<R>>
    requires(std::is_pointer_v<R> &&
             (!std::is_void_v<std::remove_pointer_t<R>>))
  {
    static_assert(std::is_object_v<std::remove_pointer_t<R>> ||
                      std::is_function_v<std::remove_pointer_t<R>>,
                  "This is Undefined Behavior!");

    return *get();
  }

  [[nodiscard]] auto operator->() const noexcept -> R
    requires(std::is_pointer_v<R>)
  {
    return get();
  }

private:
  alignas(alignof(RS)) std::byte resource_[sizeof(RS)];
  alignas(alignof(D)) std::byte deleter_[sizeof(D)];
  bool engaged_;

private:
  [[nodiscard]] auto get_resource_helper() noexcept -> RS * {
    return std::launder(reinterpret_cast<RS *>(resource_));
  }

  [[nodiscard]] auto get_resource_helper() const noexcept -> const RS * {
    return std::launder(reinterpret_cast<const RS *>(resource_));
  }

  [[nodiscard]] auto get_deleter_helper() noexcept -> D * {
    return std::launder(reinterpret_cast<D *>(deleter_));
  }

  [[nodiscard]] auto get_deleter_helper() const noexcept -> const D * {
    return std::launder(reinterpret_cast<const D *>(deleter_));
  }

  // std::reference_wapper can auto convert to T&
};

} // namespace hy::impl

#endif // CPPREFERENCE_EXTENSIONS_SCOPE_IMPL_H_