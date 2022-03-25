add_library(
    ntcl_util_measurement
    measurement_writer.f90
    measurement.f90
)
add_library(ntcl_util::measurement ALIAS ntcl_util_measurement)
target_include_directories(
    ntcl_util_measurement
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_measurement
    PUBLIC
    ntcl_util_string
)
