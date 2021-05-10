module prority_helper
    use :: string_module, only : string
    use :: string_array_helper, only : &
            append_a_string, &
            prepend_strings_by_appending_to_every_string

    implicit none
    private

    public :: add_prefix_to_priorities

    interface add_prefix_to_priorities
        module procedure add_prefix_to_priorities_from_chars
        module procedure add_prefix_to_priorities_from_string
    end interface add_prefix_to_priorities
contains
    function add_prefix_to_priorities_from_chars(prefix, priorities) result(new)
        character(len=*), intent(in) :: prefix
        type(string), dimension(:), optional :: priorities
        type(string), dimension(:), allocatable :: new

        new = add_prefix_to_priorities(string(prefix), priorities)
    end function add_prefix_to_priorities_from_chars

    function add_prefix_to_priorities_from_string(prefix, priorities) result(new)
        type(string), intent(in) :: prefix
        type(string), dimension(:), optional :: priorities
        type(string), dimension(:), allocatable :: new

        if ( present(priorities) ) then
            new = prepend_strings_by_appending_to_every_string(priorities, prefix)
            new = append_a_string(new, prefix)
        else
            new = [prefix]
        end if
    end function add_prefix_to_priorities_from_string
end module prority_helper
