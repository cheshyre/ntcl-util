add_library(
    ntcl_util_property_test
    property_package_test.f90
)
add_library(ntcl_util::property_test ALIAS ntcl_util_property_test)
target_include_directories(
    ntcl_util_property_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_property_test
    PUBLIC
    ntcl_util_api
)
