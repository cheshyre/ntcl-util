cmake_minimum_required(VERSION 3.10)

project(
    ntcl_util
    VERSION
    0.1.0
)

enable_language(Fortran)

set(NTCL_UTIL_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(NTCL_UTIL_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(src)
