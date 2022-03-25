add_library(
    ntcl_util_iterator
    integer_range.f90
    iterator_limit.f90
    iterator.f90
)
add_library(ntcl_util::iterator ALIAS ntcl_util_iterator)
target_include_directories(
    ntcl_util_iterator
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
