add_library(
    ntcl_util_readers_test
    readers_package_test.f90
    integer_column_data_reader_test.f90
)
add_library(ntcl_util::readers_test ALIAS ntcl_util_readers_test)
target_include_directories(
    ntcl_util_readers_test
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_readers_test
    PUBLIC
    ntcl_util_api
    ntcl_util_readers
)
