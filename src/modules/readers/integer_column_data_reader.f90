module integer_column_data_reader_module
    use, intrinsic :: iso_fortran_env, only : &
            input_unit, &
            iostat_end, &
            int64

    use :: string_module, only : string

    implicit none
    private

    public :: integer_column_data_reader

    type :: integer_column_data_reader
        integer :: aunit
        logical :: close_on_exit
    contains
        procedure :: read => read
        procedure :: count_lines => count_lines
        procedure :: count_columns => count_columns
        procedure :: read_lines => read_lines
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type integer_column_data_reader

    interface integer_column_data_reader
        module procedure constructor
        module procedure constructor_from_unit
        module procedure constructor_from_filename
    end interface integer_column_data_reader

contains
    function constructor() result(this)
        type(integer_column_data_reader) :: this

        call this%clear()
    end function constructor

    function constructor_from_unit(aunit) result(this)
        integer, intent(in) :: aunit
        type(integer_column_data_reader) :: this

        this = integer_column_data_reader()
        this%aunit = aunit
    end function constructor_from_unit

    function constructor_from_filename(filename) result(this)
        type(string), intent(in) :: filename
        type(integer_column_data_reader) :: this

        this = integer_column_data_reader()
        open(file=filename%char_array, status="old", newunit=this%aunit)
        this%close_on_exit = .true.
    end function constructor_from_filename

    function read(this) result(mnk)
        class(integer_column_data_reader), intent(in) :: this
        integer(int64), dimension(:,:), allocatable :: mnk

        integer :: number_of_lines
        integer :: number_of_columns

        number_of_columns = this%count_columns()
        number_of_lines = this%count_lines()
        allocate(mnk(number_of_columns, number_of_lines))
        call this%read_lines(mnk)
    end function read

    integer function count_columns(this)
        class(integer_column_data_reader), intent(in) :: this

        integer :: iostatus, idx
        character(len=1024) :: line
        integer(int64), dimension(20) :: test_array

        rewind(this%aunit)
        read(this%aunit, *, iostat=iostatus) line
        if ( iostatus /= 0) &
                error stop "integer_column_data_reader::count_columns:Can not read file."
        rewind(this%aunit)

        do count_columns = 0, size(test_array) - 1
            read(line, *, iostat=iostatus) (test_array(idx), idx=1,count_columns+1)
            if (iostatus /= 0) exit
        end do
    end function count_columns

    integer function count_lines(this)
        class(integer_column_data_reader), intent(in) :: this

        integer :: iostatus

        count_lines = 0; iostatus = 0
        rewind(this%aunit)
        do
            read(this%aunit, *, iostat=iostatus)
            if ( iostatus == 0) then
                count_lines = count_lines + 1
            else if (iostatus == iostat_end) then ! EOF
                exit
            else
                error stop "integer_column_data_reader::count_lines:Unknown error."
            end if
        end do
        rewind(this%aunit)
    end function count_lines

    subroutine read_lines(this, mnk)
        class(integer_column_data_reader), intent(in) :: this
        integer(int64), dimension(:,:), intent(inout) :: mnk

        integer :: idx, iostatus, i

        do idx = 1, size(mnk, 2)
            read(this%aunit, *, iostat=iostatus) (mnk(i, idx), i=1, size(mnk, 1))
            if ( iostatus /= 0 ) &
                    error stop "integer_column_data_reader::read_lines:Formatting error."

        end do
    end subroutine read_lines

    subroutine cleanup(this)
        class(integer_column_data_reader), intent(inout) :: this

        if ( this%close_on_exit ) close(this%aunit)
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(integer_column_data_reader), intent(inout) :: this

        this%close_on_exit = .false.
        this%aunit = input_unit
    end subroutine clear
end module integer_column_data_reader_module
