module string_test_module
    use :: assert_module, only : assert
    use :: string_module, only : string

    implicit none
    private

    public :: string_test

    type :: string_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type string_test

    interface string_test
        module procedure constructor
    end interface string_test
contains
    function constructor() result(this)
        type(string_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(string_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        integer :: idx, val
        type(string) :: astr1, astr2
        type(string), dimension(:), allocatable :: str_array
        logical :: res

        call assertion%equal("string::Test complete", .true.)

        astr1 = string("hello")

        res = astr1%equality_char_array("no")
        call assertion%equal("string::equality_char_array::F", .not. res)
        res = astr1%equality_char_array("hello")
        call assertion%equal("string::equality_char_array::T", res)

        call assertion%equal("string::.eq.char_array::T", astr1 .eq. "hello")

        astr2 = string("no")
        res = astr1%equality_string(astr2)
        call assertion%equal("string::equality_string::F", .not. res)
        ! is cleanup needed?
        ! call astr2%cleanup()
        astr2 = string("hello")
        res = astr1%equality_string(astr2)
        call assertion%equal("string::equality_string::T", res)

        call assertion%equal("string::.eq.string::T", astr1 .eq. astr2)

        astr1 = string("barko, borko, gabarken")
        str_array = astr1%split(",")
        res = ((str_array(1) .eq. "barko") .and. &
                (str_array(2) .eq. " borko") .and. &
                (str_array(3) .eq. " gabarken"))
        call assertion%equal("string::split", res)

        str_array = astr1%split_and_strip(",")
        res = ((str_array(1) .eq. "barko") .and. &
                (str_array(2) .eq. "borko") .and. &
                (str_array(3) .eq. "gabarken"))
        call assertion%equal("string::split_and_strip", res)

        astr1 = str_array(1)%cat(astr2)
        call assertion%equal("string::cat_string", astr1 .eq. "barkohello")

        astr1 = astr2%cat(" world")
        call assertion%equal("string::cat_char_array", astr1 .eq. "hello world")

        call assertion%equal("string::is_empty::F", .not. astr1%is_empty())

        astr1 = string("")
        call assertion%equal("string::is_empty::T", astr1%is_empty())

        astr1 = string("HELLO")
        call assertion%equal("string::to_lower", astr1%to_lower() .eq. "hello")

        astr1 = string("hello")
        call assertion%equal("string::to_upper", astr1%to_upper() .eq. "HELLO")

        astr1 = string("C(a,b,c,d) = A(a,b) * B(c,d)")
        str_array = astr1%find_substrings("(",")")
        res = ((str_array(1) .eq. "a,b,c,d") .and. &
                (str_array(2) .eq. "a,b") .and. &
                (str_array(3) .eq. "c,d"))

        call assertion%equal("string::find_substring", res)

        str_array = astr1%find_substrings_inclusive("(",")")
        res = ((str_array(1) .eq. "(a,b,c,d)") .and. &
                (str_array(2) .eq. "(a,b)") .and. &
                (str_array(3) .eq. "(c,d)"))

        call assertion%equal("string::find_substring_inclusive", res)

        astr1 = string("hello")

        res = ((astr1%char_count("h") == 1) .and. &
                (astr1%char_count("e") == 1) .and. &
                (astr1%char_count("l") == 2) .and. &
                (astr1%char_count("o") == 1) .and. &
                (astr1%char_count("p") == 0))

        call assertion%equal("string::char_count", res)

        res = ((astr1%char_remove("h") == "ello") .and. &
                (astr1%char_remove("e") == "hllo") .and. &
                (astr1%char_remove("l") == "heo") .and. &
                (astr1%char_remove("o") == "hell") .and. &
                (astr1%char_remove("p") == "hello"))

        call assertion%equal("string::char_remove", res)

        res = ((astr1%delete(1) == "ello") .and. &
                (astr1%delete(2) == "hllo") .and. &
                (astr1%delete(3) == "helo") .and. &
                (astr1%delete(5) == "hell") .and. &
                (astr1%delete(6) == "hello"))

        call assertion%equal("string::delete", res)

        res = ((astr1%get(1) == "h") .and. &
                (astr1%get(2) == "e") .and. &
                (astr1%get(3) == "l") .and. &
                (astr1%get(4) == "l") .and. &
                (astr1%get(5) == "o"))

        call assertion%equal("string::get", res)

        call assertion%equal("string::unique_chars", &
                astr1%unique_chars() == "helo")
    end subroutine run

    subroutine cleanup(this)
        class(string_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(string_test), intent(inout) :: this
    end subroutine clear
end module string_test_module
