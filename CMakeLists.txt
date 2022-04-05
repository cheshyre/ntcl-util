cmake_minimum_required(VERSION 3.10)

# Determine if ntcl_util is built as a subproject (using add_subdirectory)
# or if it is the main project.
if(NOT DEFINED NTCL_UTIL_MAIN_PROJECT)
  set(NTCL_UTIL_MAIN_PROJECT OFF)
  if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(NTCL_UTIL_MAIN_PROJECT ON)
    message(STATUS "CMake version: ${CMAKE_VERSION}")
  endif()
endif()

# Set the default CMAKE_BUILD_TYPE to Release.
# This should be done before the project command since the latter can set
# CMAKE_BUILD_TYPE itself (it does so for nmake).
if(NTCL_UTIL_MAIN_PROJECT AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

project(
    ntcl_util
    VERSION
    0.1.0
)

enable_language(Fortran)

if(NTCL_UTIL_MAIN_PROJECT)
  add_compile_options("-Wall" "-Wextra" "-std=f2008ts" "$<$<CONFIG:RELEASE>:-O3>")
endif()

set(NTCL_UTIL_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(NTCL_UTIL_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(src)
