add_library(
    ntcl_util_quantum_number
    quantum_number_collection.f90
    quantum_number_descriptor.f90
    quantum_number_lookup.f90
    quantum_number.f90
)
add_library(ntcl_util::quantum_number ALIAS ntcl_util_quantum_number)
target_include_directories(
    ntcl_util_quantum_number
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_quantum_number
    PUBLIC
    ntcl_util_string
)
