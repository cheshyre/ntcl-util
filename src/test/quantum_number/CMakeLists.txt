add_library(
    ntcl_util_quantum_number_test
    quantum_number_package_test.f90
)
add_library(ntcl_util::quantum_number_test ALIAS ntcl_util_quantum_number_test)
target_include_directories(
    ntcl_util_quantum_number_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_quantum_number_test
    PUBLIC
    ntcl_util_api
)
