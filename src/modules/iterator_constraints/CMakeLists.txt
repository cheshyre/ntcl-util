add_library(
    ntcl_util_iterator_constraints
    pleq_constraint.f90
    plq_constraint.f90
    pneq_constraint.f90
)
add_library(ntcl_util::iterator_constraints ALIAS ntcl_util_iterator_constraints)
target_include_directories(
    ntcl_util_iterator_constraints
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_iterator_constraints
    PUBLIC
    ntcl_util_iterator_builder
)
