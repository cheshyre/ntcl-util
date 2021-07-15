module selector_module
    use :: cmdline_arguments_module, only : cmdline_arguments
    use :: string_module, only : string
    implicit none
    private

    public :: selector

    type :: selector
        type(cmdline_arguments) :: cmd
        logical :: all_negated
        type(string), dimension(:), allocatable :: default_disabled
    contains
        procedure :: is_enabled => is_enabled
        procedure, private :: has_argument_that_starts_with => has_argument
        procedure, private :: negated_argument => negated_argument
        procedure, private :: is_not_negated => is_not_negated
        procedure, private :: strip_negation => strip_negation
        procedure, private :: are_all_negated => are_all_negated
        procedure, private :: is_enabled_by_default => is_enabled_by_default
        procedure, private :: get_index => get_index
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type selector

    interface selector
        module procedure constructor
    end interface selector

contains
    function constructor(default_disabled) result(this)
        type(string), dimension(:), intent(in), optional :: default_disabled
        type(selector) :: this

        call this%clear()
        this%cmd = cmdline_arguments()
        if (present(default_disabled)) this%default_disabled = default_disabled

        this%all_negated = this%are_all_negated()

    end function constructor

    logical function are_all_negated(this)
        class(selector), intent(inout) :: this

        integer :: idx
        type(string) :: arg

        are_all_negated = .true.
        do idx = 1, this%cmd%number_of_arguments
            arg = this%cmd%get_argument(idx)
            if ( .not. this%is_enabled_by_default(arg%char_array) ) cycle
            if ( .not. this%negated_argument(arg)) are_all_negated = .false.
        end do
    end function are_all_negated

    logical function is_enabled(this, char_array)
        class(selector), intent(in) :: this
        character(len=*), intent(in) :: char_array

        if ( this%has_argument_that_starts_with(char_array) ) then
            is_enabled = .true.
        else if ( this%is_enabled_by_default(char_array) ) then
            is_enabled = this%is_not_negated(char_array)
        else
            is_enabled = .false.
        end if
    end function is_enabled

    logical function negated_argument(this, arg)
        class(selector), intent(in) :: this
        type(string), intent(in) :: arg

        negated_argument = arg%char_array(1:1) == "-"
    end function negated_argument

    type(string) function strip_negation(this, arg)
        class(selector), intent(in) :: this
        type(string), intent(in) :: arg

        if ( this%negated_argument(arg) ) then
            strip_negation = arg%char_array(2:arg%length)
        else
            strip_negation = arg
        end if
    end function strip_negation

    logical function is_enabled_by_default(this, char_array)
        class(selector), intent(in) :: this
        character(len=*), intent(in) :: char_array

        integer :: idx

        is_enabled_by_default = .true.
        if (allocated(this%default_disabled)) then
            do idx = 1, size(this%default_disabled)
                if (this%default_disabled(idx) == char_array) is_enabled_by_default = .false.
            end do
        end if
    end function is_enabled_by_default

    logical function has_argument(this, char_array)
        class(selector), intent(in) :: this
        character(len=*), intent(in) :: char_array

        integer :: idx

        idx = this%get_index(char_array)
        has_argument = .false.
        if ( idx > 0 ) then
            has_argument = .not. this%negated_argument(this%cmd%get_argument(idx))
        end if
    end function has_argument

    logical function is_not_negated(this, char_array)
        class(selector), intent(in) :: this
        character(len=*), intent(in) :: char_array

        integer :: idx

        is_not_negated = this%all_negated
        idx = this%get_index(char_array)
        if ( idx > 0 ) then
            if (this%negated_argument(this%cmd%get_argument(idx))) &
                is_not_negated = .false.
        end if
    end function is_not_negated

    integer function get_index(this, char_array)
        class(selector), intent(in) :: this
        character(len=*), intent(in) :: char_array

        integer :: idx

        get_index = 0
        do idx = 1, this%cmd%number_of_arguments
            if ( this%strip_negation(this%cmd%get_argument(idx)) == char_array) get_index = idx
        end do
    end function get_index

    subroutine cleanup(this)
        class(selector), intent(inout) :: this

        integer :: idx

        call this%cmd%cleanup()
        if (allocated(this%default_disabled)) then
            do idx = 1, size(this%default_disabled)
                call this%default_disabled(idx)%cleanup()
            end do
            deallocate(this%default_disabled)
        end if
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(selector), intent(inout) :: this

        this%all_negated = .true.
    end subroutine clear
end module selector_module
