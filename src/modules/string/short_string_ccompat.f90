module short_string_ccompat_module
    use, intrinsic :: iso_c_binding, only : c_char, c_int
    use :: string_module, only : string

    public :: short_string_ccompat
    public :: to_string

    ! this type provides only a simple routine to convert a short_string_ccompat
    ! to a standard string
    type, bind(c) :: short_string_ccompat
        character(kind=c_char) :: str(60) ! this should be the same as in the C implementation
        integer(c_int) :: length
    end type short_string_ccompat

    interface to_string
        module procedure short_string_ccompat_to_string
    end interface to_string
contains

    pure function short_string_ccompat_to_string(short_str) result(str)
        type(short_string_ccompat), intent(in) :: short_str
        type(string) :: str
        character(len=short_str%length) :: char_array
    
        
        str%length = short_str%length
        do i = 1,str%length
            char_array(i:i) = short_str%str(i)
        end do
        str%char_array = char_array

    end function short_string_ccompat_to_string

end module short_string_ccompat_module