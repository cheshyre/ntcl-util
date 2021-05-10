module assert_module
    use, intrinsic :: iso_fortran_env, only : output_unit, real64, int64, real32

    implicit none
    private

    public :: assert

    type :: assert
        integer :: total_tests = 0, passed_tests = 0, number_of_failed_tests = 0
        character(len=1024), dimension(:), allocatable :: failed_tests
        logical :: debug
    contains
        procedure :: add_failed_test => add_failed_test
        procedure :: equal => equal
        procedure :: set_debug => set_debug
        procedure :: is_equal_integer => is_equal_integer
        procedure :: is_equal_int64 => is_equal_int64
        procedure :: is_equal_real64 => is_equal_real64
        procedure :: is_equal_integer_vector => is_equal_integer_vector
        procedure :: is_equal_int64_vector => is_equal_int64_vector
        procedure :: is_equal_int64_rank2 => is_equal_int64_rank2
        procedure :: is_equal_real64_vector => is_equal_real64_vector
        procedure :: is_equal_real64_rank2 => is_equal_real64_rank2
        procedure :: is_equal_real32_vector => is_equal_real32_vector
        procedure :: is_equal_complex64_vector => is_equal_complex64_vector
        procedure :: is_equal_integer_rank2 => is_equal_integer_rank2
        procedure :: is_equal_chars=> is_equal_chars
        procedure, nopass :: write_result => write_result
        procedure :: write_summary => write_summary
        procedure :: cleanup => cleanup
        procedure :: clear => clear
        generic :: is_equal => is_equal_integer, is_equal_integer_vector, is_equal_integer_rank2, &
            is_equal_chars, is_equal_real64, is_equal_real64_vector, &
            is_equal_int64_vector, is_equal_complex64_vector, is_equal_real32_vector, &
            is_equal_int64_rank2, is_equal_int64, is_equal_real64_rank2
    end type assert

    interface assert
        module procedure constructor
    end interface assert
contains
    function constructor() result(this)
        type(assert) :: this

        call this%clear()
    end function constructor

    subroutine add_failed_test(this, testname)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname

        character(len=1024) :: dummy

        dummy = testname

        this%number_of_failed_tests = this%number_of_failed_tests + 1
        if (.not. allocated(this%failed_tests)) &
            allocate(this%failed_tests(this%number_of_failed_tests))

        if ( size(this%failed_tests) < this%number_of_failed_tests ) then
            this%failed_tests = [this%failed_tests, dummy]
        else
            this%failed_tests(this%number_of_failed_tests) = dummy
        end if
    end subroutine add_failed_test

    subroutine equal(this, testname, assertion)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        logical, intent(in) :: assertion

        character(len=:), allocatable :: testresult

        this%total_tests = this%total_tests + 1
        testresult = "FAIL"
        if ( assertion ) then
            this%passed_tests = this%passed_tests + 1
            testresult = "OK"
        else
            call this%add_failed_test(testname)
        end if
        call this%write_result(testname, testresult)
    end subroutine equal

    subroutine is_equal_integer(this, testname, int1, int2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        integer, intent(in) :: int1, int2

        logical :: assertion

        assertion = int1 == int2
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion )) &
            write(output_unit, *) "Expected: ", int1, "  Got: ", int2
    end subroutine is_equal_integer

    subroutine is_equal_int64(this, testname, int1, int2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        integer(int64), intent(in) :: int1, int2

        logical :: assertion

        assertion = int1 == int2
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion )) &
            write(output_unit, *) "Expected: ", int1, "  Got: ", int2
    end subroutine is_equal_int64

    subroutine is_equal_real64(this, testname, in1, in2, opttolerance)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        real(real64), intent(in) :: in1, in2
        real(real64), intent(in), optional :: opttolerance

        logical :: assertion
        real(real64) :: tolerance

        tolerance = 10.0d0**(-1.0_real64*precision(in1)+1)
        if ( present(opttolerance)) tolerance = opttolerance

        assertion = abs(in1 - in2) < tolerance
        call this%equal(testname, assertion )
        if ( this%debug .and. (.not. assertion )) &
            write(output_unit, *) "Expected: ", in1, "  Got: ", in2, "with difference: ", abs(in1 - in2)
    end subroutine is_equal_real64

    subroutine is_equal_real64_vector(this, testname, vec1, vec2, opttolerance)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        real(real64), dimension(:), intent(in) :: vec1, vec2
        real(real64), intent(in), optional :: opttolerance

        logical :: assertion
        real(real64) :: tolerance

        tolerance = 10.0d0**(-1.0_real64*precision(vec1(1))+1)
        if (present(opttolerance)) tolerance = opttolerance

        assertion = size(vec1) == size(vec2)
        if ( assertion) then
            assertion = all(abs(vec1 - vec2) < tolerance)
        end if
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) then
            if ( size(vec1) == size(vec2) ) then
                write(output_unit, *) "Number of errouneous elements: ", &
                    count(abs(vec1 - vec2) > tolerance)
                write(output_unit, *) "Maximum error: ", &
                    maxval(abs(vec1 - vec2))
            else
                write(output_unit, *) "Expected vector of size: ", size(vec1), "  Got: ", size(vec2)
            end if
        end if
    end subroutine is_equal_real64_vector

    subroutine is_equal_real32_vector(this, testname, vec1, vec2, opttolerance)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        real(real32), dimension(:), intent(in) :: vec1, vec2
        real(real32), intent(in), optional :: opttolerance

        logical :: assertion
        real(real32) :: tolerance

        tolerance = 10.0**(-1.0_real32*precision(vec1(1))+1)
        if (present(opttolerance)) tolerance = opttolerance

        assertion = size(vec1) == size(vec2)
        if ( assertion) then
            assertion = all(abs(vec1 - vec2) < tolerance)
        end if
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) then
            if ( size(vec1) == size(vec2) ) then
                write(output_unit, *) "Number of errouneous elements: ", &
                    count(abs(vec1 - vec2) > tolerance)
                write(output_unit, *) "Maximum error: ", &
                    maxval(abs(vec1 - vec2))
            else
                write(output_unit, *) "Expected vector of size: ", size(vec1), "  Got: ", size(vec2)
            end if
        end if
    end subroutine is_equal_real32_vector

    subroutine is_equal_complex64_vector(this, testname, vec1, vec2, opttolerance)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        complex(real64), dimension(:), intent(in) :: vec1, vec2
        real(real64), intent(in), optional :: opttolerance

        logical :: assertion
        real(real64) :: tolerance

        tolerance = 10.0d0**(-1.0_real64*precision(vec1(1))+1)
        if (present(opttolerance)) tolerance = opttolerance

        assertion = size(vec1) == size(vec2)
        if ( assertion) then
            assertion = all(abs(vec1 - vec2) < tolerance)
        end if
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) then
            if ( size(vec1) == size(vec2) ) then
                write(output_unit, *) "Number of errouneous elements: ", &
                    count(abs(vec1 - vec2) > tolerance)
                write(output_unit, *) "Maximum error: ", &
                    maxval(abs(vec1 - vec2))
            else
                write(output_unit, *) "Expected vector of size: ", size(vec1), "  Got: ", size(vec2)
            end if
        end if
    end subroutine is_equal_complex64_vector

    subroutine is_equal_integer_vector(this, testname, vec1, vec2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        integer, dimension(:), intent(in) :: vec1, vec2

        logical :: assertion

        assertion = size(vec1) == size(vec2) .and. all(vec1 == vec2)
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) then
            write(output_unit, *) "Expected vector of size: ", size(vec1), "  Got: ", size(vec2)
            write(output_unit, *) "Number of errouneous elements: ", &
                count((vec1 - vec2) /= 0)
        end if
    end subroutine is_equal_integer_vector

    subroutine is_equal_int64_vector(this, testname, vec1, vec2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        integer(int64), dimension(:), intent(in) :: vec1, vec2

        logical :: assertion

        assertion = size(vec1) == size(vec2)
        if (assertion) then
            assertion = all(vec1 == vec2)
        end if
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) then
            write(output_unit, *) "Expected vector of size: ", size(vec1), "  Got: ", size(vec2)
            if ( size(vec1) == size(vec2) ) then
                write(output_unit, *) "Number of errouneous elements: ", &
                    count(vec1 /= vec2)
            end if
        end if
    end subroutine is_equal_int64_vector

    subroutine is_equal_int64_rank2(this, testname, in1, in2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        integer(int64), dimension(:,:), intent(in) :: in1, in2

        integer :: size_of_in1, size_of_in2
        logical :: assertion

        assertion = all(shape(in1) == shape(in2))
        if ( .not. assertion ) then
            call this%equal(testname, .false.)
            if ( this%debug) write(output_unit, *) "Expected matrix of shape: ", shape(in1), "  Got: ", shape(in2)
            return
        end if

        size_of_in1 = size(in1)
        size_of_in2 = size(in2)

        call this%is_equal(testname, reshape(in1, [size_of_in1]), reshape(in2, [size_of_in2]))
    end subroutine is_equal_int64_rank2

    subroutine is_equal_real64_rank2(this, testname, in1, in2, opttolerance)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        real(real64), dimension(:,:), intent(in) :: in1, in2
        real(real64), intent(in), optional :: opttolerance

        integer :: size_of_in1, size_of_in2

        if ( .not. all(shape(in1) == shape(in2)) ) then
            call this%equal(testname, .false.)
            if (this%debug) write(output_unit, *) "Expected matrix of shape: ", shape(in1), "  Got: ", shape(in2)
            return
        end if

        size_of_in1 = size(in1)
        size_of_in2 = size(in2)

        call this%is_equal(testname, reshape(in1, [size_of_in1]), reshape(in2, [size_of_in2]), &
            opttolerance)
    end subroutine is_equal_real64_rank2

    subroutine is_equal_integer_rank2(this, testname, in1, in2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        integer, dimension(:,:), intent(in) :: in1, in2

        logical :: assertion

        assertion = all(shape(in1) == shape(in2)) .and. all(in1 == in2)
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) &
            write(output_unit, *) "Expected matrix of shape: ", shape(in1), "  Got: ", shape(in2)
    end subroutine is_equal_integer_rank2

    subroutine is_equal_chars(this, testname, chars1, chars2)
        class(assert), intent(inout) :: this
        character(len=*), intent(in) :: testname
        character(len=*), intent(in) :: chars1, chars2

        logical :: assertion

        assertion = chars1 == chars2
        call this%equal(testname, assertion)
        if ( this%debug .and. (.not. assertion ) ) &
            write(output_unit, *) "Expected: ", chars1, "  Got: ", chars2
    end subroutine is_equal_chars

    subroutine write_result(testname, testresult)
        character(len=*), intent(in) :: testname, testresult

        write(output_unit, *) trim(adjustl(testname)), ": ", trim(adjustl(testresult))
    end subroutine write_result

    subroutine write_summary(this)
        class(assert), intent(in) :: this

        integer :: idx, maxlength
        character(len=20) :: width, width_space

        maxlength = 0
        do idx = 1, this%number_of_failed_tests
            maxlength = max(maxlength, len(trim(adjustl(this%failed_tests(idx)))))
        end do
        maxlength = maxlength + 1
        write(output_unit, *) "================= SUMMARY ================"
        write(output_unit, *) "Total number of tests run   : ", this%total_tests
        write(output_unit, *) "Total number of tests passed: ", this%passed_tests
        write(output_unit, *) "Total number of tests failed: ", this%total_tests - this%passed_tests
        write(output_unit, *) "========== List of failed tests =========="
        do idx = 1, this%number_of_failed_tests
            write(width, '(i3)') len(trim(adjustl(this%failed_tests(idx))))
            write(width_space, '(i3)') maxlength - len(trim(adjustl(this%failed_tests(idx))))
            write(output_unit, '(1x,a'//trim(adjustl(width)) // &
                    ','//trim(adjustl(width_space))//'x,a)') &
                    trim(adjustl(this%failed_tests(idx))), ": FAIL"
        end do
    end subroutine write_summary

    subroutine set_debug(this, val)
        class(assert), intent(inout) :: this
        logical, intent(in) :: val

        this%debug = val
    end subroutine set_debug

    subroutine cleanup(this)
        class(assert), intent(inout) :: this

        if ( allocated(this%failed_tests) ) deallocate(this%failed_tests)
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(assert), intent(inout) :: this

        this%total_tests = 0
        this%passed_tests = 0
        this%number_of_failed_tests = 0
        this%debug = .false.
    end subroutine clear
end module assert_module
