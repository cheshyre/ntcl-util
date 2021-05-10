module cmdline_arguments_module
    use, intrinsic :: iso_fortran_env, only : error_unit
    use :: string_module, only : string
    implicit none
    private

    public :: cmdline_arguments

    type :: cmdline_arguments
        integer :: number_of_arguments
        type(string), dimension(:), allocatable :: arguments
    contains
        procedure :: get_argument => get_argument
        procedure :: get_number_of_arguments => get_number_of_arguments
        procedure :: get_arguments_as_string => get_arguments_as_string
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type cmdline_arguments

    interface cmdline_arguments
        module procedure constructor
    end interface cmdline_arguments

contains
    function constructor(min_num_args, named_arguments, optional_arguments) result(this)
        integer, intent(in), optional :: min_num_args
        type(string), dimension(:), intent(in), optional :: named_arguments, optional_arguments
        type(cmdline_arguments) :: this

        character(len=100) :: dummy
        integer :: idx, minimum_number_of_arguments

        call this%clear()

        minimum_number_of_arguments = 0
        if (present(min_num_args)) minimum_number_of_arguments = min_num_args

        if ( present(min_num_args) .and. present(named_arguments) ) then
            if (min_num_args /= size(named_arguments)) then
                write(error_unit, *) "The number of arguments does not match the number of named arguments"
                stop
            end if
        end if

        if ( present(named_arguments) ) minimum_number_of_arguments = size(named_arguments)

        this%number_of_arguments = command_argument_count()
        if (this%number_of_arguments < minimum_number_of_arguments) then
            write(error_unit, *) "Not enough command line arguments."
            write(error_unit, *) "Number of given arguments: ", this%number_of_arguments
            write(error_unit, *) "Minimum number of arguments: ", minimum_number_of_arguments
            if (present(named_arguments)) &
                    write(error_unit, *) "Named arguments: ", &
                            (named_arguments(idx)%char_array//" ", idx=1, size(named_arguments)-1), &
                            named_arguments(size(named_arguments))%char_array
            if (present(optional_arguments)) &
                    write(error_unit, *) "Optional arguments: ", &
                            (optional_arguments(idx)%char_array//" ", idx=1, size(optional_arguments)-1), &
                            optional_arguments(size(optional_arguments))%char_array
            stop
        end if

        allocate(this%arguments(this%number_of_arguments))
        do idx = 1, this%number_of_arguments
            call get_command_argument(idx, dummy)
            this%arguments(idx) = trim(adjustl(dummy))
        end do
    end function constructor

    type(string) function get_arguments_as_string(this)
        class(cmdline_arguments), intent(in) :: this

        integer :: idx

        if ( this%number_of_arguments > 0 ) &
                get_arguments_as_string = this%arguments(1)

        do idx = 2, this%number_of_arguments
            get_arguments_as_string = get_arguments_as_string%cat(string(" "))
            get_arguments_as_string = get_arguments_as_string%cat(this%arguments(idx))
        end do
    end function get_arguments_as_string

    integer function get_number_of_arguments(this)
        class(cmdline_arguments), intent(in) :: this

        get_number_of_arguments = this%number_of_arguments
    end function get_number_of_arguments

    type(string) function get_argument(this, idx)
        class(cmdline_arguments), intent(in) :: this
        integer, intent(in) :: idx

        get_argument = this%arguments(idx)
    end function get_argument

    subroutine cleanup(this)
        class(cmdline_arguments), intent(inout) :: this

        integer :: idx

        if (allocated(this%arguments)) then
            do idx = 1, size(this%arguments)
                call this%arguments(idx)%cleanup()
            end do
            deallocate(this%arguments)
        end if
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(cmdline_arguments), intent(inout) :: this

        this%number_of_arguments = 0
    end subroutine clear
end module cmdline_arguments_module
