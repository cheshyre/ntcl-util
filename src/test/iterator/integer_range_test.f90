module integer_range_test_module
    use :: assert_module, only : assert
    use :: integer_range_module, only : integer_range

    implicit none
    private

    public :: integer_range_test

    type :: integer_range_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
    end type integer_range_test

    integer, dimension(10) :: rng = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    integer, dimension(11) :: rng2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
contains
    subroutine run(this, assertion)
        class(integer_range_test), intent(in) :: this
        type(assert) :: assertion

        type(integer_range) :: iterator

        iterator = integer_range(1, 10)
        call assertion%is_equal("integer_range::get_size1", 10, iterator%get_size())
        call assertion%equal("integer_range::get_range1", all(iterator%get_range() == rng))
        iterator = integer_range(2, 10)
        call assertion%is_equal("integer_range::get_size2", 9, iterator%get_size())
        call assertion%equal("integer_range::get_range2", all(iterator%get_range() == rng(2:)))

        iterator = integer_range(1, 10, 2)
        call assertion%is_equal("integer_range::get_strided_size1", 5, iterator%get_size())
        call assertion%equal("integer_range::get_strided_range1", all(iterator%get_range() == rng(::2)))
        iterator = integer_range(2, 10, 2)
        call assertion%is_equal("integer_range::get_strided_size2", 5, iterator%get_size())
        call assertion%equal("integer_range::get_strided_range2", all(iterator%get_range() == rng(2::2)))

        iterator = integer_range(1, 11, 2)
        call assertion%is_equal("integer_range::get_strided_size3", 6, iterator%get_size())
        call assertion%equal("integer_range::get_strided_range3", all(iterator%get_range() == rng2(::2)))
        iterator = integer_range(2, 11, 2)
        call assertion%is_equal("integer_range::get_strided_size4", 5, iterator%get_size())
        call assertion%equal("integer_range::get_strided_range4", all(iterator%get_range() == rng2(2::2)))
        iterator = integer_range(1, 11, 3)
        call assertion%is_equal("integer_range::get_strided_size5", 4, iterator%get_size())
        call assertion%equal("integer_range::get_strided_range5", all(iterator%get_range() == rng2(::3)))
        iterator = integer_range(2, 11, 3)
        call assertion%is_equal("integer_range::get_strided_size6", 4, iterator%get_size())
        call assertion%equal("integer_range::get_strided_range6", all(iterator%get_range() == rng2(2::3)))
    end subroutine run

    subroutine cleanup(this)
        class(integer_range_test), intent(inout) :: this

    end subroutine cleanup
end module integer_range_test_module

