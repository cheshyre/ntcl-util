module timer_module
    use, intrinsic :: iso_fortran_env, only : real64

    use :: string_module, only : string

    implicit none
    private

    public :: timer

    type :: timer
        type(string) :: timer_name
        real(real64) :: current_start, current_stop
        real(real64) :: current_time, accumulator
        logical :: running
    contains
        procedure :: startit => startit
        procedure :: stopit => stopit
        procedure :: print_current => print_current
        procedure :: print_accumulated => print_accumulated
        procedure :: get_now => get_now
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type timer

    interface timer
        module procedure constructor_empty
        module procedure constructor
    end interface timer

contains
    function constructor_empty() result(this)
        type(timer) :: this

        call this%clear()
    end function constructor_empty

    function constructor(name_of_timer) result(this)
        character(len=*), intent(in) :: name_of_timer
        type(timer) :: this

        this = timer()
        this%timer_name = name_of_timer
    end function constructor

    real(real64) function get_now(this)
        class(timer), intent(in) :: this

        integer :: cycles, rate

        call system_clock(cycles, rate)
        get_now = real(cycles, kind=real64)/real(rate, kind=real64)
    end function get_now

    subroutine startit(this)
        class(timer), intent(inout) :: this

        if (this%running) &
                error stop "timer::startit:Timer already running."

        this%running = .true.
        this%current_start = this%get_now()
    end subroutine startit

    subroutine stopit(this)
        class(timer), intent(inout) :: this

        if (.not. this%running) &
                error stop "timer::stopit:Timer is not running."

        this%running = .false.
        this%current_stop = this%get_now()
        this%current_time = this%current_stop - this%current_start
        this%accumulator = this%accumulator + this%current_time
    end subroutine stopit

    subroutine print_current(this, ounit)
        class(timer), intent(inout) :: this
        integer, intent(in) :: ounit

        write(ounit, '("Timer::",A,"::Current time (s): ",g20.8)') this%timer_name%char_array, this%current_time
    end subroutine print_current

    subroutine print_accumulated(this, ounit)
        class(timer), intent(inout) :: this
        integer, intent(in) :: ounit

        write(ounit, '("Timer::",A,"::Accumulated time (s): ",g20.8)') this%timer_name%char_array, this%accumulator
    end subroutine print_accumulated


    subroutine cleanup(this)
        class(timer), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(timer), intent(inout) :: this

        this%timer_name = ''
        this%current_start = 0.0d0
        this%current_stop = 0.0d0
        this%current_time = 0.0d0
        this%accumulator = 0.0d0
        this%running = .false.
    end subroutine clear
end module timer_module
