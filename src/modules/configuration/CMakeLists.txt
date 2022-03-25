add_library(
    ntcl_util_configuration
    application_config.f90
    config_file_parser.f90
)
add_library(ntcl_util::configuration ALIAS ntcl_util_configuration)
target_include_directories(
    ntcl_util_configuration
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_configuration
    PUBLIC
    ntcl_util_string
    ntcl_util_dictionary
    ntcl_util_commandline
)
