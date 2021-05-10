module string_converter_module
    use, intrinsic :: iso_fortran_env, only : error_unit, real64, int64
    use :: string_module, only : string
    implicit none
    private

    public :: string_converter

    type :: string_converter
    contains
        procedure, nopass :: toint => toint
        procedure, nopass :: toint64 => toint64
        procedure, nopass :: to_logical => to_logical
        procedure, nopass :: toreal32 => toreal32
        procedure, nopass :: toreal64 => toreal64
        procedure, nopass :: to_string_array => to_string_array
        procedure, nopass :: to_int_array => to_int_array
        procedure, nopass :: fromint => fromint
    end type string_converter

    character, parameter :: default_array_delimiter = ' '
contains
    integer function toint(str)
        type(string), intent(in) :: str

        integer :: error

        read(str%char_array, *, iostat=error) toint
        if ( error /= 0) &
            error stop "string_converter::Error converting string to integer"
    end function toint

    integer(int64) function toint64(str)
        type(string), intent(in) :: str

        integer :: error

        read(str%char_array, *, iostat=error) toint64
        if ( error /= 0) &
            error stop "string_converter::Error converting string to integer64"
    end function toint64

    type(string) function fromint(input)
        integer, intent(in) :: input

        character(len=20) :: dummy

        write(dummy, *) input
        fromint = string(trim(adjustl(dummy)))
    end function fromint

    logical function to_logical(str)
        type(string), intent(in) :: str

        to_logical = .false.
        if ( str%to_lower() == "true" ) to_logical = .true.
        if ( str%to_lower()== "yes" ) to_logical = .true.
    end function to_logical

    function to_string_array(str, delimiter) result(array)
        type(string), intent(in) :: str
        character, intent(in), optional :: delimiter
        type(string), dimension(:), allocatable :: array

        character :: actual_delimiter

        actual_delimiter = default_array_delimiter
        if (present(delimiter)) actual_delimiter = delimiter

        array = str%split_and_strip(actual_delimiter)
    end function to_string_array

    function to_int_array(str, delimiter) result(array)
        type(string), intent(in) :: str
        character, intent(in), optional :: delimiter
        integer, dimension(:), allocatable :: array

        type(string), dimension(:), allocatable :: str_array
        integer :: idx

        str_array = to_string_array(str, delimiter)

        array = [(toint(str_array(idx)), idx = 1, size(str_array))]

        do idx = 1, size(str_array)
            call str_array(idx)%cleanup()
        end do
        deallocate(str_array)
    end function to_int_array

    real function toreal32(str)
        type(string), intent(in) :: str

        integer :: error

        read(str%char_array, *, iostat=error) toreal32
        if ( error /= 0) &
            error stop "string_converter::Error converting string to real32"
    end function toreal32

    real(real64) function toreal64(str)
        type(string), intent(in) :: str

        integer :: error

        read(str%char_array, *, iostat=error) toreal64
        if ( error /= 0) &
            error stop "string_converter::Error converting string to real64"
    end function toreal64
end module string_converter_module
