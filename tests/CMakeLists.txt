project(Tests LANGUAGES CXX C)


set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


##################################
# GoogleTest
##################################
find_package(GTest QUIET)
if(GTest_FOUND)
    message(STATUS "Using installed GoogleTest")
    enable_testing()
else()
    message(STATUS "Fetching GoogleTest")
    include(FetchContent)
    FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)
endif()
include(GoogleTest)
link_libraries(GTest::gtest_main GTest::gtest)

##################################
# CTest
##################################
include(CTest)
enable_testing()

##################################
# Targets
##################################
# Enable verbose candidate diagnostics for C++ targets
add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-fdiagnostics-all-candidates>)


add_executable(Tests
    test_scope_exit.cpp
)

target_link_libraries(Tests PRIVATE scope)
gtest_discover_tests(Tests)