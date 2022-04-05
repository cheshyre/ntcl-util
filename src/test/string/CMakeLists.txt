add_library(
    ntcl_util_string_test
    string_package_test.f90
    string_test.f90
)
add_library(ntcl_util::string_test ALIAS ntcl_util_interoptability_test)
target_include_directories(
    ntcl_util_string_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_string_test
    PUBLIC
    ntcl_util_api
    ntcl_util_assert
    ntcl_util_string
)
