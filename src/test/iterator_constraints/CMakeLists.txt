add_library(
    ntcl_util_iterator_constraints_test
    iterator_constraints_package_test.f90
)
add_library(ntcl_util::iterator_constraints_test ALIAS ntcl_util_iterator_constraints_test)
target_include_directories(
    ntcl_util_iterator_constraints_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_iterator_constraints_test
    PUBLIC
    ntcl_util_api
)
