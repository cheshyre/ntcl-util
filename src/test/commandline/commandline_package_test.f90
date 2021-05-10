! Auto-generated -- DO NOT MODIFY
module commandline_package_test_module
    use :: util_api, only : &
            selector, &
            assert

    use :: cmdline_parser_test_module, only : cmdline_parser_test

    implicit none
    private

    public :: commandline_package_test

    type :: commandline_package_test
        type(selector) :: test_selector
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type commandline_package_test

    interface commandline_package_test
        module procedure constructor
    end interface commandline_package_test

contains
    function constructor(aselector) result(this)
        type(selector), intent(in) :: aselector
        type(commandline_package_test) :: this

        call this%clear()

        this%test_selector = aselector
    end function constructor

    subroutine run(this, assertion)
        class(commandline_package_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(cmdline_parser_test) :: acmdline_parser_test

        call assertion%equal("commandline::Package test complete", .true.)

        if ( &
                this%test_selector%is_enabled("cmdline_parser") ) then
            acmdline_parser_test = cmdline_parser_test()
            call acmdline_parser_test%run(assertion)
            call acmdline_parser_test%cleanup()
        end if

    end subroutine run

    subroutine cleanup(this)
        class(commandline_package_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(commandline_package_test), intent(inout) :: this
    end subroutine clear
end module commandline_package_test_module
