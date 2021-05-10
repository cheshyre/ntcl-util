! Auto-generated -- DO NOT MODIFY
module string_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: string_test_module, only : string_test

    implicit none
    private

    public :: string_package_test

    type :: string_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type string_package_test

    interface string_package_test
        module procedure constructor
    end interface string_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(string_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(string_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(string_test) :: astring_test

        call assertion%equal("string::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("string") ) then
            astring_test = string_test()
            call astring_test%run(assertion)
            call astring_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(string_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(string_package_test), intent(inout) :: this
    end subroutine clear
end module string_package_test_module
