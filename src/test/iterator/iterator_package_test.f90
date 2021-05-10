! Auto-generated -- DO NOT MODIFY
module iterator_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: integer_range_test_module, only : integer_range_test
    use :: iterator_test_module, only : iterator_test

    implicit none
    private

    public :: iterator_package_test

    type :: iterator_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type iterator_package_test

    interface iterator_package_test
        module procedure constructor
    end interface iterator_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(iterator_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(iterator_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(integer_range_test) :: ainteger_range_test
        type(iterator_test) :: aiterator_test

        call assertion%equal("iterator::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("integer_range") ) then
            ainteger_range_test = integer_range_test()
            call ainteger_range_test%run(assertion)
            call ainteger_range_test%cleanup()
        end if

        if ( &
                this%test_selector%is_enabled("iterator") ) then
            aiterator_test = iterator_test()
            call aiterator_test%run(assertion)
            call aiterator_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(iterator_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(iterator_package_test), intent(inout) :: this
    end subroutine clear
end module iterator_package_test_module
