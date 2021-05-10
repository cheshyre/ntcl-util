module dictionary_converter_module
    use, intrinsic :: iso_fortran_env, only : int64

    use :: dictionary_module, only : dictionary
    use :: string_module, only : string
    use :: string_converter_module, only : string_converter

    implicit none
    private

    public :: dictionary_converter

    type :: dictionary_converter
        type(string_converter) :: converter
    contains
        generic :: to_string => &
                to_string_from_string, &
                to_string_from_chars
        generic :: to_string_array => &
                to_string_array_from_string, &
                to_string_array_from_chars
        generic :: to_logical => &
                to_logical_from_chars, &
                to_logical_from_string
        generic :: to_int => &
                to_int_from_chars, &
                to_int_from_string
        generic :: to_int64 => &
                to_int64_from_chars, &
                to_int64_from_string

        procedure :: to_logical_from_chars => to_logical_from_chars
        procedure :: to_logical_from_string => to_logical_from_string
        procedure :: to_int_from_chars => to_int_from_chars
        procedure :: to_int_from_string => to_int_from_string
        procedure :: to_int64_from_chars => to_int64_from_chars
        procedure :: to_int64_from_string => to_int64_from_string
        procedure :: to_string_from_string => to_string_from_string
        procedure :: to_string_from_chars => to_string_from_chars
        procedure :: to_string_array_from_string => to_string_array_from_string
        procedure :: to_string_array_from_chars => to_string_array_from_chars
    end type dictionary_converter

    logical, parameter :: default_logical = .false.
    integer(int64), parameter :: default_int64 = 0_int64
    integer, parameter :: default_int = 0
    type(string), dimension(0) :: default_string_array
contains
    logical function to_logical_from_chars(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        character(len=*), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        logical, intent(in), optional :: default_value

        to_logical_from_chars = this%to_logical(string(key), options, priorities, default_value)
    end function to_logical_from_chars

    logical function to_logical_from_string(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        type(string), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        logical, intent(in), optional :: default_value

        to_logical_from_string = default_logical
        if ( present(default_value) ) to_logical_from_string = default_value

        if ( present(options) ) then
            if ( options%has_key(key, priorities) ) &
                to_logical_from_string = this%converter%to_logical(options%get_value(key, priorities))
        end if
    end function to_logical_from_string

    integer function to_int_from_chars(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        character(len=*), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        integer, intent(in), optional :: default_value

        to_int_from_chars = this%to_int(string(key), options, priorities, default_value)
    end function to_int_from_chars

    integer function to_int_from_string(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        type(string), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        integer, intent(in), optional :: default_value

        to_int_from_string = default_int
        if ( present(default_value) ) to_int_from_string = default_value

        if ( present(options) ) then
            if ( options%has_key(key, priorities) ) &
                to_int_from_string = this%converter%toint(options%get_value(key, priorities))
        end if
    end function to_int_from_string

    integer(int64) function to_int64_from_chars(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        character(len=*), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        integer(int64), intent(in), optional :: default_value

        to_int64_from_chars = this%to_int64(string(key), options, priorities, default_value)
    end function to_int64_from_chars

    integer(int64) function to_int64_from_string(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        type(string), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        integer(int64), intent(in), optional :: default_value

        to_int64_from_string = default_int64
        if ( present(default_value) ) to_int64_from_string = default_value

        if ( present(options) ) then
            if ( options%has_key(key, priorities) ) &
                to_int64_from_string = this%converter%toint64(options%get_value(key, priorities))
        end if
    end function to_int64_from_string

    type(string) function to_string_from_string(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        type(string), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        type(string), intent(in), optional :: default_value

        to_string_from_string = ''
        if ( present(default_value) ) to_string_from_string = default_value

        if ( present(options) ) then
            if ( options%has_key(key, priorities) ) &
                to_string_from_string = options%get_value(key, priorities)
        end if
    end function to_string_from_string

    type(string) function to_string_from_chars(this, key, options, priorities, default_value)
        class(dictionary_converter), intent(in) :: this
        character(len=*), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        character(len=*), intent(in), optional :: default_value

        if ( present(default_value) ) then
            to_string_from_chars = this%to_string(string(key), options, priorities, string(default_value))
        else
            to_string_from_chars = this%to_string(string(key), options, priorities)
        end if
    end function to_string_from_chars

    function to_string_array_from_string(this, key, options, priorities, delimiter, default_value) result(array)
        class(dictionary_converter), intent(in) :: this
        type(string), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        character, intent(in), optional :: delimiter
        type(string), dimension(:), intent(in), optional :: default_value
        type(string), dimension(:), allocatable :: array

        if ( present(options) ) then
            if ( options%has_key(key, priorities) ) &
                array = this%converter%to_string_array(options%get_value(key, priorities), delimiter)
        end if

        if (.not. allocated(array) ) then
            if ( present(default_value) ) then
                array = default_value
            else
                array = default_string_array
            end if
        end if
    end function to_string_array_from_string

    function to_string_array_from_chars(this, key, options, priorities, delimiter, default_value) result(array)
        class(dictionary_converter), intent(in) :: this
        character(len=*), intent(in) :: key
        type(dictionary), intent(in), optional :: options
        type(string), dimension(:), intent(in), optional :: priorities
        character, intent(in), optional :: delimiter
        type(string), dimension(:), intent(in), optional :: default_value
        type(string), dimension(:), allocatable :: array

        array = this%to_string_array(string(key), options, priorities, delimiter, default_value)
    end function to_string_array_from_chars
end module dictionary_converter_module
