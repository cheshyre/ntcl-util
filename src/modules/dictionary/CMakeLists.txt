add_library(
    ntcl_util_dictionary
    config_file_parser.f90
    dictionary_converter.f90
    dictionary.f90
)
add_library(ntcl_util::dictionary ALIAS ntcl_util_dictionary)
target_include_directories(
    ntcl_util_dictionary
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_dictionary
    PUBLIC
    ntcl_util_string
)
