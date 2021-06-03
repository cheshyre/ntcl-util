! Auto-generated -- DO NOT MODIFY
module readers_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: integer_column_data_reader_test_module, only : integer_column_data_reader_test

    implicit none
    private

    public :: readers_package_test

    type :: readers_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type readers_package_test

    interface readers_package_test
        module procedure constructor
    end interface readers_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(readers_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(readers_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(integer_column_data_reader_test) :: ainteger_column_data_reader_test

        call assertion%equal("readers::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("integer_column_data_reader") ) then
            ainteger_column_data_reader_test = integer_column_data_reader_test()
            call ainteger_column_data_reader_test%run(assertion)
            call ainteger_column_data_reader_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(readers_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(readers_package_test), intent(inout) :: this
    end subroutine clear
end module readers_package_test_module
