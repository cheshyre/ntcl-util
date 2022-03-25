add_library(
    ntcl_util_domain_builder
    balanced_slicer.f90
    slicer.f90
)
add_library(ntcl_util::domain_builder ALIAS ntcl_util_domain_builder)
target_include_directories(
    ntcl_util_domain_builder
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_domain_builder
    PUBLIC
    ntcl_util_domain
)
