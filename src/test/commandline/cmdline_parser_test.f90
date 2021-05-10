module cmdline_parser_test_module
    use :: assert_module, only : assert
    use :: cmdline_parser_module, only : cmdline_parser

    use :: string_module, only : string
    use :: dictionary_module, only : dictionary

    implicit none
    private

    public :: cmdline_parser_test

    type :: cmdline_parser_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type cmdline_parser_test

    interface cmdline_parser_test
        module procedure constructor
    end interface cmdline_parser_test

contains
    function constructor() result(this)
        type(cmdline_parser_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(cmdline_parser_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(cmdline_parser) :: acmdline_parser
        type(dictionary) :: original, keywords

        call assertion%equal("cmdline_parser::Test complete", .true.)

        original = dictionary(2)
        call original%set_value(string("machinefile"), string("machine.ini"))
        call original%set_value(string("runfile"), string("run.out"))

        keywords = acmdline_parser%get_keywords(string("machinefile=machine.ini runfile=run.out"))

        call assertion%equal("cmdline_parser::Correct keywords dictionary", &
            keywords == original)

        keywords = acmdline_parser%get_keywords(string("configfile machinefile=machine.ini runfile=run.out"))

        call assertion%equal("cmdline_parser::Correct keywords dictionary with a non keyword", &
            keywords == original)

        keywords = acmdline_parser%get_keywords( &
                string("configfile machinefile=machine.ini  test test2 runfile=run.out test3 test4"))

        call assertion%equal("cmdline_parser::Correct keywords dictionary with several non keywords", &
            keywords == original)

    end subroutine run

    subroutine cleanup(this)
        class(cmdline_parser_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(cmdline_parser_test), intent(inout) :: this
    end subroutine clear
end module cmdline_parser_test_module
