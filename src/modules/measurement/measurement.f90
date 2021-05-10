module measurement_module
    use, intrinsic :: iso_fortran_env, only : real64
    use :: string_module, only : string

    implicit none
    private

    public :: measurement

    type :: measurement
        type(string) :: id_string
        type(string) :: unit_string
        real(real64) :: val
        logical :: rate
    contains
        procedure :: reset => reset
        procedure :: add => add
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type measurement

    interface measurement
        module procedure constructor_empty
        module procedure constructor
    end interface measurement

contains
    function constructor_empty() result(this)
        type(measurement) :: this

        call this%clear()
    end function constructor_empty

    function constructor(id_string, unit_string, rate) result(this)
        type(string), intent(in) :: id_string, unit_string
        logical, intent(in), optional :: rate
        type(measurement) :: this

        this = measurement()

        this%id_string = id_string
        this%unit_string = unit_string
        if ( present(rate) ) this%rate = rate
    end function constructor

    subroutine reset(this)
        class(measurement), intent(inout) :: this

        this%val = 0.0d0
    end subroutine reset

    subroutine add(this, val)
        class(measurement), intent(inout) :: this
        real(real64), intent(in) :: val

        this%val = this%val + val
    end subroutine add

    subroutine cleanup(this)
        class(measurement), intent(inout) :: this

        call this%id_string%cleanup()
        call this%unit_string%cleanup()
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(measurement), intent(inout) :: this

        call this%reset()
        this%rate = .false.
    end subroutine clear
end module measurement_module
