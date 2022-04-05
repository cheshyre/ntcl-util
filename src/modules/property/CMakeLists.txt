add_library(
    ntcl_util_property
    property_collection.f90
    property_with_name.f90
    property.f90
)
add_library(ntcl_util::property ALIAS ntcl_util_property)
target_include_directories(
    ntcl_util_property
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(
    ntcl_util_property
    PUBLIC
    ntcl_util_string
)
