module string_array_helper
    use :: string_module, only : string

    implicit none
    private

    public :: append_to_every_string
    public :: prepend_a_string
    public :: append_a_string
    public :: prepend_strings_by_appending_to_every_string

    interface append_to_every_string
        module procedure append_to_every_string_from_string
        module procedure append_to_every_string_from_characters
    end interface append_to_every_string

    interface prepend_a_string
        module procedure prepend_a_string_from_string
        module procedure prepend_a_string_from_characters
    end interface prepend_a_string

    interface append_a_string
        module procedure append_a_string_from_string
        module procedure append_a_string_from_characters
    end interface append_a_string

    interface prepend_strings_by_appending_to_every_string
        module procedure prepend_strings_by_appending_to_every_string_string
        module procedure prepend_strings_by_appending_to_every_string_characters
    end interface prepend_strings_by_appending_to_every_string
contains
    function prepend_strings_by_appending_to_every_string_characters(strings, append) result(prepended)
        type(string), dimension(:), intent(in) :: strings
        character(len=*), intent(in) :: append
        type(string), dimension(:), allocatable :: prepended

        prepended = prepend_strings_by_appending_to_every_string(strings, string(append))
    end function prepend_strings_by_appending_to_every_string_characters

    function prepend_strings_by_appending_to_every_string_string(strings, append) result(prepended)
        type(string), dimension(:), intent(in) :: strings
        type(string), intent(in) :: append
        type(string), dimension(:), allocatable :: prepended

        prepended = [append_to_every_string(strings, append), strings]
    end function prepend_strings_by_appending_to_every_string_string

    function append_to_every_string_from_string(strings, append) result(appended)
        type(string), dimension(:), intent(in) :: strings
        type(string), intent(in) :: append
        type(string), dimension(:), allocatable :: appended

        integer :: i

        if ( size(strings) == 0 ) then
            appended = [append]
        else
            appended = [(strings(i)%cat(append), i = 1, size(strings))]
        end if
    end function append_to_every_string_from_string

    function append_to_every_string_from_characters(strings, append) result(appended)
        type(string), dimension(:), intent(in) :: strings
        character(len=*), intent(in) :: append
        type(string), dimension(:), allocatable :: appended

        appended = append_to_every_string(strings, string(append))
    end function append_to_every_string_from_characters

    function prepend_a_string_from_string(strings, prepend) result(prepended)
        type(string), dimension(:), intent(in) :: strings
        type(string), intent(in) :: prepend
        type(string), dimension(:), allocatable :: prepended

        prepended = [prepend, strings]
    end function prepend_a_string_from_string

    function prepend_a_string_from_characters(strings, prepend) result(prepended)
        type(string), dimension(:), intent(in) :: strings
        character(len=*), intent(in) :: prepend
        type(string), dimension(:), allocatable :: prepended

        prepended = prepend_a_string(strings, string(prepend))
    end function prepend_a_string_from_characters

    function append_a_string_from_string(strings, append) result(appended)
        type(string), dimension(:), intent(in) :: strings
        type(string), intent(in) :: append
        type(string), dimension(:), allocatable :: appended

        appended = [strings, append]
    end function append_a_string_from_string

    function append_a_string_from_characters(strings, append) result(appended)
        type(string), dimension(:), intent(in) :: strings
        character(len=*), intent(in) :: append
        type(string), dimension(:), allocatable :: appended

        appended = append_a_string(strings, string(append))
    end function append_a_string_from_characters
end module string_array_helper
