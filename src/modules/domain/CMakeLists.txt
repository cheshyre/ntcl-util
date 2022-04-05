add_library(
    ntcl_util_domain
    domain.f90
    slice.f90
    tile.f90
)
add_library(ntcl_util::domain ALIAS ntcl_util_domain)
target_include_directories(
    ntcl_util_domain
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
