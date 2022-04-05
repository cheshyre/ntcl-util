add_library(
    ntcl_util_domain_builder_test
    domain_builder_package_test.f90
)
add_library(ntcl_util::domain_builder_test ALIAS ntcl_util_domain_builder_test)
target_include_directories(
    ntcl_util_domain_builder_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_domain_builder_test
    PUBLIC
    ntcl_util_api
)
