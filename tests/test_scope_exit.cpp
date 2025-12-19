#include "scope.h"
#include <algorithm>
#include <functional>
#include <gtest/gtest.h>
#include <print>
#include <type_traits>

namespace {
static int no_object_test_counter = 0;
void no_object_test_function() { no_object_test_counter++; }

class Function {
public:
  void operator()() {}
};

class OnlyCopyConstruct : public Function {
public:
  OnlyCopyConstruct() = default;
  OnlyCopyConstruct(const OnlyCopyConstruct &) { no_object_test_counter++; }
  OnlyCopyConstruct(OnlyCopyConstruct &&) = delete;
  OnlyCopyConstruct &operator=(const OnlyCopyConstruct &) = delete;
  OnlyCopyConstruct &operator=(OnlyCopyConstruct &&) = delete;
};

class OnlyNothrowMoveConstruct : public Function {
public:
  OnlyNothrowMoveConstruct() = default;
  OnlyNothrowMoveConstruct(const OnlyNothrowMoveConstruct &) = delete;
  OnlyNothrowMoveConstruct(OnlyNothrowMoveConstruct &&) noexcept {
    no_object_test_counter++;
    no_object_test_counter++;
  }
  OnlyNothrowMoveConstruct &
  operator=(const OnlyNothrowMoveConstruct &) = delete;
  OnlyNothrowMoveConstruct &operator=(OnlyNothrowMoveConstruct &&) = delete;
};

class OnlyMoveAndCopyConstruct : public Function {
public:
  OnlyMoveAndCopyConstruct() = default;
  OnlyMoveAndCopyConstruct(const OnlyMoveAndCopyConstruct &) {
    no_object_test_counter++;
  }
  OnlyMoveAndCopyConstruct(OnlyMoveAndCopyConstruct &&) {
    no_object_test_counter++;
    no_object_test_counter++;
  }
  OnlyMoveAndCopyConstruct &
  operator=(const OnlyMoveAndCopyConstruct &) = delete;
  OnlyMoveAndCopyConstruct &operator=(OnlyMoveAndCopyConstruct &&) = delete;
};

class OnlyNothrowMoveAndCopyConstruct : public Function {
public:
  OnlyNothrowMoveAndCopyConstruct() = default;
  OnlyNothrowMoveAndCopyConstruct(
      const OnlyNothrowMoveAndCopyConstruct &) noexcept {
    no_object_test_counter++;
  }
  OnlyNothrowMoveAndCopyConstruct(OnlyNothrowMoveAndCopyConstruct &&) noexcept {
    no_object_test_counter++;
    no_object_test_counter++;
  }
  OnlyNothrowMoveAndCopyConstruct &
  operator=(const OnlyNothrowMoveAndCopyConstruct &) = delete;
  OnlyNothrowMoveAndCopyConstruct &
  operator=(OnlyNothrowMoveAndCopyConstruct &&) = delete;
};

class OnlyMoveConstruct : public Function {
public:
  OnlyMoveConstruct() = default;
  OnlyMoveConstruct(const OnlyMoveConstruct &) = delete;
  OnlyMoveConstruct(OnlyMoveConstruct &&) {
    no_object_test_counter++;
    no_object_test_counter++;
  }
  OnlyMoveConstruct &operator=(const OnlyMoveConstruct &) = delete;
  OnlyMoveConstruct &operator=(OnlyMoveConstruct &&) = delete;
};

} // namespace

// Compile-time checks for scope_exit
static_assert(!std::is_default_constructible_v<hy::scope_exit<void (*)()>>,
              "scope_exit should not be default constructible");
static_assert(!std::is_default_constructible_v<hy::scope_exit<decltype([] {})>>,
              "scope_exit should not be default constructible");
static_assert(!std::is_copy_assignable_v<hy::scope_exit<void (*)()>>,
              "scope_exit should not be copy assignable");
static_assert(!std::is_copy_assignable_v<hy::scope_exit<decltype([] {})>>,
              "scope_exit should not be copy assignable");
static_assert(!std::is_move_assignable_v<hy::scope_exit<void (*)()>>,
              "scope_exit should not be move assignable");
static_assert(!std::is_move_assignable_v<hy::scope_exit<decltype([] {})>>,
              "scope_exit should not be move assignable");
static_assert(!std::is_copy_constructible_v<hy::scope_exit<void (*)()>>,
              "scope_exit should be copy constructible");
static_assert(!std::is_copy_constructible_v<hy::scope_exit<decltype([] {})>>,
              "scope_exit should be move constructible");
static_assert(std::is_move_constructible_v<hy::scope_exit<void (*)()>>,
              "scope_exit should be move constructible");
static_assert(std::is_move_constructible_v<hy::scope_exit<decltype([] {})>>,
              "scope_exit should be move constructible");
// static_assert(
//     !std::is_constructible_v<hy::scope_exit<decltype(OnlyMoveConstruct{})>,
//                              hy::scope_exit<decltype(OnlyMoveConstruct{})>>,
//     "scope_exit should not be constructible with OnlyMoveConstruct");

class ScopeExitTest : public ::testing::Test {
protected:
  void SetUp() override {
    counter_ = 0;
    no_object_test_counter = 0;
  }
  void TearDown() override {
    counter_ = 0;
    no_object_test_counter = 0;
  }

public:
  int get_counter() const noexcept { return counter_; }
  void add_counter() noexcept { counter_++; }

private:
  int counter_;
};

TEST_F(ScopeExitTest, release_test) {
  {
    hy::scope_exit exit_scope([this]() { add_counter(); });
    exit_scope.release();
  }
  EXPECT_EQ(get_counter(), 0);

  {
    hy::scope_exit exit_scope(no_object_test_function);
    exit_scope.release();
  }
  EXPECT_EQ(no_object_test_counter, 0);
}

TEST_F(ScopeExitTest, invoke_test) {
  {
    hy::scope_exit exit_scope([this]() { add_counter(); });
  }
  EXPECT_EQ(get_counter(), 1);

  {
    hy::scope_exit exit_scope(no_object_test_function);
  }
  EXPECT_EQ(no_object_test_counter, 1);
}

TEST_F(ScopeExitTest, move_construct_test) {
  {
    hy::scope_exit exit_scope1([this]() { add_counter(); });
    hy::scope_exit exit_scope2(std::move(exit_scope1));
  }
  EXPECT_EQ(get_counter(), 1);

  {
    hy::scope_exit exit_scope1(no_object_test_function);
    hy::scope_exit exit_scope2(std::move(exit_scope1));
  }
  EXPECT_EQ(no_object_test_counter, 1);
}

TEST_F(ScopeExitTest, move_construct_only_copy_test) {
  {
    OnlyCopyConstruct only_copy_construct;

    hy::scope_exit exit_scope1(only_copy_construct);
    EXPECT_EQ(no_object_test_counter, 1);

    hy::scope_exit exit_scope2(std::move(exit_scope1));
    EXPECT_EQ(no_object_test_counter, 2);
  }
}

TEST_F(ScopeExitTest, move_construct_only_nothrow_move_test) {
  {
    hy::scope_exit exit_scope1(OnlyNothrowMoveConstruct{});
    EXPECT_EQ(no_object_test_counter, 2);

    hy::scope_exit exit_scope2(std::move(exit_scope1));
    EXPECT_EQ(no_object_test_counter, 4);
  }
}

TEST_F(ScopeExitTest, move_construct_only_move_and_copy_test) {
  {
    hy::scope_exit exit_scope1(OnlyMoveAndCopyConstruct{});
    EXPECT_EQ(no_object_test_counter, 1);

    hy::scope_exit exit_scope2(std::move(exit_scope1));
    EXPECT_EQ(no_object_test_counter, 2);
  }
}

TEST_F(ScopeExitTest, move_construct_only_nothrow_move_and_copy_test) {
  {
    hy::scope_exit exit_scope1(OnlyNothrowMoveAndCopyConstruct{});
    EXPECT_EQ(no_object_test_counter, 2);

    hy::scope_exit exit_scope2(std::move(exit_scope1));
    EXPECT_EQ(no_object_test_counter, 4);
  }
}

auto main(int argc, char **argv) -> int {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}