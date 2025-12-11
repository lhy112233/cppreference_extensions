project(Tests LANGUAGES CXX C)



find_package(GoogleTest QUIET)

add_executable(Tests
    test_scope_exit.cpp
)

target_link_libraries(Tests PRIVATE scope)