module dictionary_ccompat_module
    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer, c_loc, c_null_ptr

    use dictionary_module, only : dictionary
    use string_module, only : string
    use short_string_ccompat_module, only : short_string_ccompat, to_string

    implicit none

    private

    public :: dictionary_ccompat
    public :: create_dictionary_ccompat
    public :: cleanup_dictionary_ccompat
    public :: add_key_value_pair_to_dictionary_ccompat

    type, bind(c) :: dictionary_ccompat
        type(c_ptr) :: cptr_to_class
    end type dictionary_ccompat

    interface create_dictionary_ccompat
        module procedure create_dictionary_ccompat
    end interface create_dictionary_ccompat

    interface cleanup_dictionary_ccompat
        module procedure cleanup_dictionary_ccompat
    end interface cleanup_dictionary_ccompat

    interface add_key_value_pair_to_dictionary_ccompat
        module procedure add_key_value_pair_to_dictionary_ccompat
    end interface add_key_value_pair_to_dictionary_ccompat

contains

    subroutine create_dictionary_ccompat(dict_handle) bind(c)
        type(dictionary_ccompat), intent(out) :: dict_handle
        type(dictionary), pointer :: dict_ptr

        allocate(dict_ptr)
        call dict_ptr%clear()
        call dict_ptr%allocate_internal_structures()

        dict_handle%cptr_to_class = c_loc(dict_ptr)
    end subroutine create_dictionary_ccompat

    subroutine cleanup_dictionary_ccompat(dict_handle) bind(c)
        type(dictionary_ccompat), intent(inout) :: dict_handle
        type(dictionary), pointer :: dict_ptr

        call c_f_pointer(dict_handle%cptr_to_class, dict_ptr)

        call dict_ptr%clear()

        deallocate(dict_ptr)
        dict_handle%cptr_to_class = c_null_ptr
    end subroutine cleanup_dictionary_ccompat

    subroutine add_key_value_pair_to_dictionary_ccompat(dict_handle, key, value) bind(c)
        type(dictionary_ccompat), intent(inout) :: dict_handle
        type(short_string_ccompat), intent(in) :: key, value
        type(dictionary), pointer :: dict_ptr
        ! type(string) :: key_str, value_str


        call c_f_pointer(dict_handle%cptr_to_class, dict_ptr)

        ! key_str = to_string(key)
        ! value_str = to_string(value)
        ! call dict_ptr%set_value(key_str, value_str)
        call dict_ptr%set_value(to_string(key), to_string(value))
    end subroutine add_key_value_pair_to_dictionary_ccompat

end module dictionary_ccompat_module