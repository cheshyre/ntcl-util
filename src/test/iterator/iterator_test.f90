module iterator_test_module
    use :: assert_module, only : assert
    use :: iterator_module, only : iterator
    implicit none
    private

    public :: iterator_test

    type :: iterator_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type iterator_test

    interface iterator_test
        module procedure constructor
    end interface iterator_test

    integer, dimension(10) :: rng = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    integer, dimension(11) :: rng2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
contains
    function constructor() result(this)
        type(iterator_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(iterator_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        type(iterator) :: aniterator
        integer, dimension(11,2) :: expected

        aniterator = iterator(1, 10)
        call assertion%is_equal("iterator::get_size from 1 to 10", 10, aniterator%get_size())
        call assertion%is_equal("iterator::get_items from 1 to 10", rng, aniterator%get_items(1))
        call assertion%is_equal("iterator::max_rank for 1 to 10", 1, aniterator%get_max_rank())

        aniterator = iterator(2, 10)
        call assertion%is_equal("iterator::get_size from 2 to 10", 9, aniterator%get_size())
        call assertion%is_equal("iterator::get_items from 2 to 10", rng(2:), aniterator%get_items(1))

        aniterator = iterator(1, 10, 2)
        call assertion%is_equal("iterator::get_strided_size from 1 to 10 with steps of 2", &
                5, aniterator%get_size())
        call assertion%is_equal("iterator::get_strided_items from 1 to 10 with steps of 2", &
                rng(::2), aniterator%get_items(1))

        aniterator = iterator(2, 10, 2)
        call assertion%is_equal("iterator::get_strided_size from 2 to 10 with steps of 2", &
            5, aniterator%get_size())
        call assertion%is_equal("iterator::get_strided_items from 2 to 10 with steps of 2", &
            rng(2::2), aniterator%get_items(1))

        aniterator = iterator(1, 11, 2)
        call assertion%is_equal("iterator::get_strided_size from 1 to 11 with steps of 2", &
            6, aniterator%get_size())
        call assertion%is_equal("iterator::get_strided_items from 1 to 11 with steps of 2", &
            rng2(::2), aniterator%get_items(1))

        aniterator = iterator(2, 11, 2)
        call assertion%is_equal("iterator::get_strided_size from 2 to 11 with steps of 2", &
            5, aniterator%get_size())
        call assertion%is_equal("iterator::get_strided_items from 2 to 11 with steps of 2", &
            rng2(2::2), aniterator%get_items(1))

        aniterator = iterator(1, 11, 3)
        call assertion%is_equal("iterator::get_strided_size from 1 to 11 with steps of 3", &
            4, aniterator%get_size())
        call assertion%is_equal("iterator::get_strided_items from 1 to 11 with steps of 3", &
            rng2(::3), aniterator%get_items(1))

        aniterator = iterator(2, 11, 3)
        call assertion%is_equal("iterator::get_strided_size from 2 to 11 with steps of 3", &
            4, aniterator%get_size())
        call assertion%is_equal("iterator::get_strided_items from 2 to 11 with steps of 3", &
            rng2(2::3), aniterator%get_items(1))

        expected(:,1) = rng2
        expected(:,2) = 4*rng2

        aniterator = iterator(expected)
        call assertion%is_equal("iterator::from data::get_size rank 2 iterator ", 11, aniterator%get_size())
        call assertion%is_equal("iterator::from data::get_items from rank 1", expected(:,1), aniterator%get_items(1))
        call assertion%is_equal("iterator::from data::get_items from rank 2", expected(:,2), aniterator%get_items(2))
        call assertion%is_equal("iterator::max rank for rank 2 iterator", 2, aniterator%get_max_rank())

    end subroutine run

    subroutine cleanup(this)
        class(iterator_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(iterator_test), intent(inout) :: this
    end subroutine clear
end module iterator_test_module
