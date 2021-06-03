module integer_column_data_reader_test_module
    use :: util_api, only : assert
    use :: integer_column_data_reader_module, only : integer_column_data_reader

    implicit none
    private

    public :: integer_column_data_reader_test

    type :: integer_column_data_reader_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type integer_column_data_reader_test

    interface integer_column_data_reader_test
        module procedure constructor
    end interface integer_column_data_reader_test
contains
    function constructor() result(this)
        type(integer_column_data_reader_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(integer_column_data_reader_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(integer_column_data_reader) :: ainteger_column_data_reader

        call assertion%equal("integer_column_data_reader::Test complete", .true.)

    end subroutine run

    subroutine cleanup(this)
        class(integer_column_data_reader_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(integer_column_data_reader_test), intent(inout) :: this
    end subroutine clear
end module integer_column_data_reader_test_module
