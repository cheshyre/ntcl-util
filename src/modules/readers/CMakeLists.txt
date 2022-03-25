add_library(
    ntcl_util_readers
    integer_column_data_reader.f90
)
add_library(ntcl_util::readers ALIAS ntcl_util_readers)
target_include_directories(
    ntcl_util_readers
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_readers
    PUBLIC
    ntcl_util_string
)