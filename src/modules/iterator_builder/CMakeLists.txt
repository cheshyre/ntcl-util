add_library(
    ntcl_util_iterator_builder
    iterator_constraint_wrapper.f90
    iterator_constraint.f90
    iterator_product.f90
)
add_library(ntcl_util::iterator_builder ALIAS ntcl_util_iterator_builder)
target_include_directories(
    ntcl_util_iterator_builder
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_iterator_builder
    PUBLIC
    ntcl_util_iterator
)
