#include "scope.h"
#include <exception>
#include <gtest/gtest.h>

using namespace hy;

namespace {
static int no_object_test_counter = 0;
void no_object_test_function() { no_object_test_counter++; }

class Function {
public:
  void operator()() {}
};

} // namespace

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

// Tips:
/*TEST_F(ScopeExitTest, OnlyCopyConstruct) {
  class OnlyCopyConstruct : public Function {
    OnlyCopyConstruct() = default;
    OnlyCopyConstruct(const OnlyCopyConstruct &) {}
    OnlyCopyConstruct(OnlyCopyConstruct &&) = delete; // this is a key method
  };

  // this is could not be compile,
  // because match OnlyCopyConstruct(OnlyCopyConstruct&&) = delete;
   scope_exit a{OnlyCopyConstruct{}};

  // but this is a good assert
  static_assert(
  std::is_constructible_v<decltype(a), OnlyCopyConstruct&&>,
   "...");
} */

TEST_F(ScopeExitTest, OnlyCopyConstruct) {
  class OnlyCopyConstruct : public Function {
  public:
    OnlyCopyConstruct() { no_object_test_counter++; }
    OnlyCopyConstruct(const OnlyCopyConstruct &) {
      no_object_test_counter += 2;
    }
    OnlyCopyConstruct &operator=(const OnlyCopyConstruct &) {
      no_object_test_counter += 3;
      return *this;
    }
    ~OnlyCopyConstruct() { no_object_test_counter += 4; }

    void operator()() { no_object_test_counter += 5; }
  };

  {
    {
      // Init + copy construct -> 1 + 4 + 2
      scope_exit scope1{OnlyCopyConstruct{}};
      EXPECT_EQ(no_object_test_counter, 7);

      // copy construct -> 2
      scope_exit scope2(std::move(scope1));
      EXPECT_EQ(no_object_test_counter, 9);
    }
    // 2x ~() and operator()() -> 2x4 + 5
    EXPECT_EQ(no_object_test_counter, 22);
  }
}

/*TEST_F(ScopeExitTest, OnlyMoveConstruct) {
  class OnlyMoveConstruct : public Function {
    public:
    OnlyMoveConstruct() = default;
    OnlyMoveConstruct(OnlyMoveConstruct&&) {}
  };

  // scope_exit scope{OnlyMoveConstruct{}};  // this is bad
} */

TEST_F(ScopeExitTest, MoveAndCopyConstruct) {
  class MoveAndCopyConstruct {
  public:
    MoveAndCopyConstruct() { no_object_test_counter++; }
    MoveAndCopyConstruct(const MoveAndCopyConstruct &) {
      no_object_test_counter += 2;
    }
    MoveAndCopyConstruct(MoveAndCopyConstruct &&) {
      no_object_test_counter += 3;
    }
    ~MoveAndCopyConstruct() { no_object_test_counter += 4; }

    void operator()() { no_object_test_counter += 5; }
  };

  {
    EXPECT_EQ(no_object_test_counter, 0);

    scope_exit scope1{MoveAndCopyConstruct{}};
    // default construct + copy + ~() -> 1 + 2 + 4
    EXPECT_EQ(no_object_test_counter, 7);

    scope_exit scope2{std::move(scope1)};
    // copy -> 2
    EXPECT_EQ(no_object_test_counter, 9);
  }

  // ~() x 2 + operator()() -> 4 x 2 + 5
  EXPECT_EQ(no_object_test_counter, 22);
}

TEST_F(ScopeExitTest, NothrowMoveAndCopy) {
  class NothrowMoveAndCopy : public Function {
  public:
    NothrowMoveAndCopy() { no_object_test_counter++; }
    NothrowMoveAndCopy(const NothrowMoveAndCopy &) {
      no_object_test_counter += 2;
    }
    NothrowMoveAndCopy(NothrowMoveAndCopy &&) noexcept {
      no_object_test_counter += 3;
    }
    ~NothrowMoveAndCopy() { no_object_test_counter += 4; }

    void operator()() { no_object_test_counter += 5; }
  };

  {
    EXPECT_EQ(no_object_test_counter, 0);

    scope_exit scope1{NothrowMoveAndCopy{}};
    // default construct + move construct + ~() -> 1 + 3 + 4
    EXPECT_EQ(no_object_test_counter, 8);

    scope_exit scope2(std::move(scope1));
    // move construct -> 3
    EXPECT_EQ(no_object_test_counter, 11);
  }

  // ~() x 2 + operator()() -> 4 x2 + 5
  EXPECT_EQ(no_object_test_counter, 24);
}

TEST_F(ScopeExitTest, ConstructThrowException) {
  class ConstructThrowException : public Function {
  public:
    class SubConstructThrowException {
    public:
      SubConstructThrowException() { no_object_test_counter += 4; }
      ~SubConstructThrowException() { no_object_test_counter += 5; }

      void operator()() { no_object_test_counter += 6; }
    };

    ConstructThrowException(const SubConstructThrowException &) {
      throw std::exception{};
    }
    ConstructThrowException(const ConstructThrowException &) {
      no_object_test_counter++;
    }
    ~ConstructThrowException() { no_object_test_counter += 2; }

    void operator()() { no_object_test_counter += 3; }
  };

  {
    EXPECT_EQ(no_object_test_counter, 0);

    // Sub Decault Construct -> 4
    ConstructThrowException::SubConstructThrowException sub;
    EXPECT_EQ(no_object_test_counter, 4);

    try {
      // scope_exit<ConstructThrowException> scope1{sub};
    } catch (...) {
    }
    // Sub operator()() -> 6
    EXPECT_EQ(no_object_test_counter, 10);
  }

  // Sub ~() -> 5
  EXPECT_EQ(no_object_test_counter, 15);
}

// TEST_F(ScopeExitTest, ReferenceConstruct) {
//   EXPECT_EQ(no_object_test_counter, 0);

//   auto no_object = [](){};

//   scope_exit<decltype(no_object)&> scope{(no_object)};
// }


TEST_F(ScopeExitTest, ReferenceConstruct) {
  scope_exit<decltype(no_object_test_function)&> scope1{no_object_test_function};


}

auto main(int argc, char **argv) -> int {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}