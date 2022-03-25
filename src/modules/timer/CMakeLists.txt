add_library(
    ntcl_util_timer
    timer.f90
)
add_library(ntcl_util::timer ALIAS ntcl_util_timer)
target_include_directories(
    ntcl_util_timer
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_timer
    PUBLIC
    ntcl_util_string
)
