add_library(
    ntcl_util_string
    priority_helper.f90
    short_string_ccompat.f90
    string_array_helper.f90
    string_converter.f90
    string.f90
)
add_library(ntcl_util::string ALIAS ntcl_util_string)
target_include_directories(
    ntcl_util_string
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
