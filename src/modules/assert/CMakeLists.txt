add_library(
    ntcl_util_assert
    assert.f90
)
add_library(ntcl_util::assert ALIAS ntcl_util_assert)
target_include_directories(
    ntcl_util_assert
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
