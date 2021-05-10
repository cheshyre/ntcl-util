! Auto-generated -- DO NOT MODIFY
module iterator_builder_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: iterator_product_test_module, only : iterator_product_test

    implicit none
    private

    public :: iterator_builder_package_test

    type :: iterator_builder_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type iterator_builder_package_test

    interface iterator_builder_package_test
        module procedure constructor
    end interface iterator_builder_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(iterator_builder_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(iterator_builder_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(iterator_product_test) :: aiterator_product_test

        call assertion%equal("iterator_builder::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("iterator_product") ) then
            aiterator_product_test = iterator_product_test()
            call aiterator_product_test%run(assertion)
            call aiterator_product_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(iterator_builder_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(iterator_builder_package_test), intent(inout) :: this
    end subroutine clear
end module iterator_builder_package_test_module
