module measurement_writer_module
    use, intrinsic :: iso_fortran_env, only : output_unit
    use :: string_module, only  : string
    use :: measurement_module, only : measurement

    implicit none
    private

    public :: measurement_writer

    type :: measurement_writer
        integer :: aunit
        logical :: close_on_exit
    contains
        generic :: write => write_total, write_average
        procedure :: write_total => write_total
        procedure :: write_average => write_average
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type measurement_writer

    interface measurement_writer
        module procedure constructor
        module procedure constructor_from_unit
        module procedure constructor_from_filename
    end interface measurement_writer

contains
    function constructor() result(this)
        type(measurement_writer) :: this

        call this%clear()
    end function constructor

    function constructor_from_unit(aunit) result(this)
        integer, intent(in) :: aunit
        type(measurement_writer) :: this

        this = measurement_writer()
        this%aunit = aunit
    end function constructor_from_unit

    function constructor_from_filename(filename) result(this)
        type(string), intent(in) :: filename
        type(measurement_writer) :: this

        this = measurement_writer()
        open(file=filename%char_array, status="replace", newunit=this%aunit)
        this%close_on_exit = .true.
    end function constructor_from_filename

    subroutine write_total(this, ameasurement)
        class(measurement_writer), intent(in) :: this
        type(measurement), intent(in) :: ameasurement

        write(this%aunit, '(a30,f20.6,a)') ameasurement%id_string%char_array // ": ", &
                ameasurement%val, &
                " " // ameasurement%unit_string%char_array
    end subroutine write_total

    subroutine write_average(this, ameasurement, n)
        class(measurement_writer), intent(in) :: this
        type(measurement), intent(in) :: ameasurement
        integer, intent(in) :: n

        if ( .not. ameasurement%rate ) then
            write(this%aunit, '(a30,f20.6,a)') ameasurement%id_string%char_array // ": ", &
                    ameasurement%val/n, &
                    " " // ameasurement%unit_string%char_array
        else
            call this%write_total(ameasurement)
        end if
    end subroutine write_average


    subroutine cleanup(this)
        class(measurement_writer), intent(inout) :: this

        if ( this%close_on_exit ) &
                close(this%aunit)
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(measurement_writer), intent(inout) :: this

        this%close_on_exit = .false.
        this%aunit = output_unit
    end subroutine clear
end module measurement_writer_module
