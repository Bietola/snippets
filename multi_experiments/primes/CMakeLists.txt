# init
cmake_minimum_required(VERSION 3.3.0)
project(Primes)

# packages
find_package(Threads)

# compiler flags
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# executables
add_executable(Primes src/main.cpp)

# custom include directories
include_directories(src/)

# library include directories
# NONE

# link libraries
target_link_libraries(Primes ${CMAKE_THREAD_LIBS_INIT})
