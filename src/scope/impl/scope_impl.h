#ifndef CPPREFERENCE_EXTENSIONS_SCOPE_IMPL_H_
#define CPPREFERENCE_EXTENSIONS_SCOPE_IMPL_H_

#include <cassert>
#include <cstddef>
#include <exception>
#include <functional>
#include <type_traits>
#include <utility>

namespace hy::impl {
namespace {

template <typename ScopeExitEF>
concept scope_exit_ef_concept =
    requires(std::remove_reference_t<ScopeExitEF> ef) {
      { std::invoke(ef) };
    };

template <typename EF, typename Fn, typename Self>
concept scope_exit_construct_overload_concept =
    !std::is_same_v<std::remove_cvref_t<Fn>, Self> &&
    std::is_constructible_v<EF, Fn> &&
    !std::is_same_v<std::remove_cvref<Self>, std::remove_cvref<Fn>>;

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
      std::is_nothrow_constructible_v<EF, Fn &>) {
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
  {
    static_assert(!std::is_nothrow_move_constructible_v<EF> ||
                      std::is_move_constructible_v<EF>,
                  "This is Undefined Behavior!");
    static_assert(std::is_nothrow_move_constructible_v<EF> ||
                      std::is_copy_constructible_v<EF>,
                  "This is Undefined Behavior!");

    auto &other_ef = *reinterpret_cast<exitfun *>(other.storage_);
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
    auto &ef = *reinterpret_cast<exitfun *>(storage_);

    if (!dismissed_) {
      try {
        std::invoke(ef);
      } catch (...) {
        assert("This is Undefined behavior!");

        ef.~exitfun();
        return;
      }
    }

    ef.~exitfun();
  }

  void release() noexcept { dismissed_ = true; }

private:
  alignas(alignof(EF)) std::byte storage_[sizeof(EF)];
  bool dismissed_;
};

/**
 * @brief scope_fail_impl
 *
 */
namespace {
template <typename EF>
concept scope_fail_ef_concept = scope_exit_ef_concept<EF>;
}

template <scope_fail_ef_concept EF> class scope_fail_impl {
public:
  template <typename Fn>
    requires(!std::is_same_v<std::remove_cvref_t<Fn>, scope_fail_impl<EF>> &&
             std::is_constructible_v<EF, Fn>)
  explicit scope_fail_impl(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)
      : dismissed_(false), exception_count_(std::uncaught_exceptions()) {
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
    requires(std::is_nothrow_move_constructible_v<EF> ||
             std::is_copy_constructible_v<EF>)
      : dismissed_(other.dismissed_), exception_count_(other.exception_count_) {
    static_assert(!std::is_nothrow_move_constructible_v<EF> ||
                      std::is_move_constructible_v<EF>,
                  "This is Undefined Behavior!");
    static_assert(std::is_nothrow_move_constructible_v<EF> ||
                      std::is_copy_constructible_v<EF>,
                  "This is Undefined Behavior!");

    if constexpr (std::is_nothrow_move_constructible_v<EF>) {
      ::new (storage_)
          EF(std::forward<EF>(reinterpret_cast<EF &>(other.storage_)));
    } else {
      ::new (storage_) EF(reinterpret_cast<EF &>(other.storage_));
    }

    other.release();
  }

  scope_fail_impl(const scope_fail_impl &) = delete;

  ~scope_fail_impl() noexcept {
    if (std::uncaught_exceptions() > exception_count_ && !dismissed_) {
      auto &ef = *reinterpret_cast<EF *>(storage_);

      try {
        std::invoke(ef);
      } catch (...) {
        assert("This is Undefined behavior!");
      }

      ef.~EF();
    }
  }

  scope_fail_impl &operator=(const scope_fail_impl &) = delete;
  scope_fail_impl &operator=(scope_fail_impl &&) = delete;

  void release() noexcept { dismissed_ = true; }

private:
  bool dismissed_;
  std::size_t exception_count_;
  alignas(alignof(EF)) std::byte storage_[sizeof(EF)];

}; // class scope_fail_impl

/**
 * @brief scope_success_impl
 *
 */

namespace {
template <typename EF>
concept scope_success_ef_concept = scope_exit_ef_concept<EF>;
}

template <scope_success_ef_concept EF> class scope_success_impl {
public:
  template <typename Fn>
    requires(!std::is_same_v<std::remove_cvref_t<Fn>, scope_success_impl<EF>> &&
             std::is_constructible_v<EF, Fn>)
  explicit scope_success_impl(Fn &&fn) noexcept(
      std::is_nothrow_constructible_v<EF, Fn> ||
      std::is_nothrow_constructible_v<EF, Fn &>)
      : dismissed_(false), exception_count_(std::uncaught_exceptions()) {
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
    requires(std::is_nothrow_move_constructible_v<EF> ||
             std::is_copy_constructible_v<EF>)
      : dismissed_(other.dismissed_), exception_count_(other.exception_count_) {
    static_assert(!std::is_nothrow_move_constructible_v<EF> ||
                      std::is_move_constructible_v<EF>,
                  "This is Undefined Behavior!");

    static_assert(std::is_nothrow_move_constructible_v<EF> ||
                      std::is_copy_constructible_v<EF>,
                  "This is Undefined Behavior!");

    if constexpr (std::is_nothrow_move_constructible_v<EF>) {
      ::new (storage_)
          EF(std::forward<EF>(reinterpret_cast<EF &>(other.storage_)));
    } else {
      ::new (storage_) EF(reinterpret_cast<EF &>(other.storage_));
    }
    other.release();
  }

  scope_success_impl(const scope_success_impl &) = delete;

  scope_success_impl &operator=(const scope_success_impl &) = delete;
  scope_success_impl &operator=(scope_success_impl &&) = delete;

  ~scope_success_impl() noexcept {
    if (std::uncaught_exceptions() < exception_count_ && !dismissed_) {
      auto &ef = *reinterpret_cast<EF *>(storage_);

      try {
        std::invoke(ef);
      } catch (...) {
        ef.~EF();
        throw;
      }

      ef.~EF();
    }
  }

  void release() noexcept { dismissed_ = true; }

private:
  bool dismissed_;
  std::size_t exception_count_;
  alignas(alignof(EF)) std::byte storage_[sizeof(EF)];

}; // class scope_success_impl

/**
 * @brief unique_resource_impl
 *
 */
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
    (requires(D d, std::remove_reference_t<R> r) { d(r); });
} // namespace

template <unique_resource_R_concept R, unique_resource_D_concept<R> D>
class unique_resource_impl : private D {
  using RS =
      std::conditional<std::is_object_v<R>, R,
                       std::reference_wrapper<std::remove_reference_t<R>>>;

public:
  unique_resource_impl()
    requires(std::is_default_constructible_v<R> &&
             std::is_default_constructible_v<D>)
      : D(), resource_(), engaged_(false) {};

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
  {
    // TODO
  }

  // unique_resource_impl(unique_resource_impl &&other) noexcept(
  //     std::is_nothrow_move_constructible_v<R1> &&
  //     std::is_nothrow_move_constructible_v<D>) {
  //   // TODO
  // }

  ~unique_resource_impl() { reset(); }

  unique_resource_impl &operator=(unique_resource_impl &&other) noexcept(
      std::is_nothrow_move_assignable_v<RS> &&
      std::is_nothrow_move_assignable_v<D>) {
    // static_assert(, "This is Undefined Behavior!");
    // TODO
    reset();
  }

  void release() noexcept { engaged_ = false; }

  void reset() noexcept {
    if (engaged_) {
      try {
        D::operator()(resource_);
      } catch (...) {
        assert("This is Undefined behavior!");
      }

      engaged_ = false;
    }
  }

  template <typename RR> void reset(RR &&r) {
    reset();

    if constexpr (std::is_nothrow_assignable_v<RS, RR>) {
      resource_ = std::forward<RR>(r);

    } else {
      try {
        resource_ = std::as_const(r);
      } catch (...) {
        D::operator()(r);
        engaged_ = false;
        throw;
      }
    }

    engaged_ = true;
  }

  const R &get() const noexcept {
    if constexpr (std::is_object_v<R>) {
      return resource_;
    } else {
      return resource_.get();
    }
  }

  const D &get_deleter() const noexcept { return *this; }

  std::add_lvalue_reference_t<std::remove_pointer_t<R>>
  operator*() const noexcept
    requires(std::is_pointer_v<R> &&
             (!std::is_void_v<std::remove_pointer_t<R>>))
  {
    static_assert(std::is_object_v<std::remove_reference_t<R>> ||
                      std::is_function_v<std::remove_reference_t<R>>,
                  "This is Undefined Behavior!");

    return *get();
  }

  R operator->() const noexcept
    requires(std::is_pointer_v<R>)
  {
    static_assert(std::is_pointer_v<R>, "This is Undefined Behavior!");

    return get();
  }

private:
  RS resource_;
  bool engaged_;
};

// template< class R, class D, class S = std::decay_t<R> >

// [[nodiscard]] inline  unique_resource<std::decay_t<R>, std::decay_t<D>>
//     make_unique_resource_checked( R&& r, const S& invalid, D&& d ) noexcept(

//     std::is_nothrow_constructible_v<std::decay_t<R>, R> &&
//     std::is_nothrow_constructible_v<std::decay_t<D>, D>
// ) {
//   // TODO
// }

} // namespace hy::impl

#endif // CPPREFERENCE_EXTENSIONS_SCOPE_IMPL_H_