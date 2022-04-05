add_library(
    ntcl_util_configuration_test
    configuration_package_test.f90
)
add_library(ntcl_util::configuration_test ALIAS ntcl_util_configuration_test)
target_include_directories(
    ntcl_util_configuration_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_configuration_test
    PUBLIC
    ntcl_util_api
)
