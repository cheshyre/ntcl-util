add_library(
    ntcl_util_iterator_test
    iterator_package_test.f90
    integer_range_test.f90
    iterator_test.f90
)
add_library(ntcl_util::iterator_test ALIAS ntcl_util_iterator_test)
target_include_directories(
    ntcl_util_iterator_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_iterator_test
    PUBLIC
    ntcl_util_api
    ntcl_util_assert
    ntcl_util_iterator
)
