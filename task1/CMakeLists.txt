cmake_minimum_required(VERSION 3.14)
project(postscript)

# GoogleTest requires at least C++14
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)

FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(
  calculator_test
  calculator_test.cpp
  calculator.cpp
)
target_link_libraries(
  calculator_test
  GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(calculator_test)
