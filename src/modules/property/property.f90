module property_module
    use :: string_module, only : string

    implicit none
    private

    public :: property

    type, abstract :: property
    contains
        generic :: is => is_chars, is_string
        procedure :: is_chars => is_chars
        procedure(is_string_interface), deferred :: is_string
        procedure(empty), deferred :: cleanup
    end type property

    abstract interface
        logical function is_string_interface(this, astring)
            import :: property
            import :: string

            class(property), intent(in) :: this
            type(string), intent(in) :: astring
        end function is_string_interface

        subroutine empty(this)
            import :: property

            class(property), intent(inout) :: this
        end subroutine empty
    end interface
contains
    logical function is_chars(this, chars)
        class(property), intent(in) :: this
        character(len=*), intent(in) :: chars

        is_chars = this%is(string(chars))
    end function is_chars
end module property_module
