add_library(
    ntcl_util_dictionary_test
    dictionary_package_test.f90
)
add_library(ntcl_util::dictionary_test ALIAS ntcl_util_dictionary_test)
target_include_directories(
    ntcl_util_dictionary_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_dictionary_test
    PUBLIC
    ntcl_util_api
)
