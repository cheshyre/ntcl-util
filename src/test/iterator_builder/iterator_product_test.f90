module iterator_product_test_module
    use :: assert_module, only : assert
    use :: iterator_product_module, only : iterator_product
    use :: iterator_module, only : iterator
    use :: iterator_constraint_wrapper_module, only : iterator_constraint_wrapper
    use :: pleq_constraint_module, only : pleq_constraint
    implicit none
    private

    public :: iterator_product_test

    type :: iterator_product_test
    contains
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type iterator_product_test

    interface iterator_product_test
        module procedure constructor
    end interface iterator_product_test

contains
    function constructor() result(this)
        type(iterator_product_test) :: this

        call this%clear()
    end function constructor

    subroutine run(this, assertion)
        class(iterator_product_test), intent(in) :: this
        type(assert), intent(inout) :: assertion

        integer, dimension(:,:), allocatable :: combined, res
        type(iterator) :: aniterator
        type(iterator), dimension(:), allocatable :: iterators
        type(iterator_constraint_wrapper), dimension(1) :: constraints
        integer :: rank, number_of_states

        combined = get_combined([iterator(1,3)])
        allocate(res(3,1))
        res(:,1) = [1,2,3]
        call assertion%is_equal("iterator_product::rank1_size1", 3, size(combined))
        call assertion%equal("iterator_product::rank1_combined1", all(combined == res) )
        deallocate(res)

        combined = get_combined([iterator(2,3)])
        allocate(res(2,1))
        res(:,1) = [2,3]
        call assertion%is_equal("iterator_product::rank1_size2", 2, size(combined))
        call assertion%equal("iterator_product::rank1_combined2", all(combined == res) )
        deallocate(res)

        combined = get_combined([iterator(1,2), iterator(3,4)])
        allocate(res(4,2))
        res(:,1) = [1, 1, 2, 2]
        res(:,2) = [3, 4, 3, 4]
        call assertion%is_equal("iterator_product::rank2", 2, size(combined, 2))
        call assertion%is_equal("iterator_product::rank2_size1", 4, size(combined, 1))
        call assertion%equal("iterator_product::rank2_combined1", all(combined == res) )
        deallocate(res)

        combined = get_combined_column_major([iterator(1,2), iterator(3,4)])
        allocate(res(4,2))
        res(:,1) = [1, 2, 1, 2]
        res(:,2) = [3, 3, 4, 4]
        call assertion%is_equal("iterator_product::rank2 column major", 2, size(combined, 2))
        call assertion%is_equal("iterator_product::rank2_size1 column major", 4, size(combined, 1))
        call assertion%equal("iterator_product::rank2_combined1 column major", all(combined == res) )
        deallocate(res)

        number_of_states = 12
        aniterator = iterator(1, number_of_states)
        constraints = [ iterator_constraint_wrapper(pleq_constraint()) ]

        rank = 1
        iterators = [aniterator]
        combined = get_combined(iterators)
        call assertion%is_equal("iterator_product::p1::number_of_configs",  &
            size(combined, 1), number_of_states**rank)
        combined = get_combined(iterators, constraints)
        call assertion%is_equal("iterator_product::p1_pleq::number_of_configs",  &
            size(combined, 1), get_triangular_number(number_of_states,rank))

        rank = 2
        iterators = [aniterator, aniterator]

        combined = get_combined(iterators)
        call assertion%is_equal("iterator_product::p2::number_of_configs",  &
            size(combined, 1), number_of_states**rank)
        combined = get_combined(iterators, constraints)
        call assertion%is_equal("iterator_product::p2_pleq::number_of_configs",  &
            size(combined, 1), get_triangular_number(number_of_states,rank))

        rank = 3
        iterators = [aniterator, aniterator, aniterator]

        combined = get_combined(iterators)
        call assertion%is_equal("iterator_product::p3::number_of_configs",  &
            size(combined, 1), number_of_states**rank)
        combined = get_combined(iterators, constraints)
        call assertion%is_equal("iterator_product::p3_pleq::number_of_configs",  &
            size(combined, 1), get_triangular_number(number_of_states,rank))

        rank = 4
        iterators = [aniterator, aniterator, aniterator, aniterator]

        combined = get_combined(iterators)
        call assertion%is_equal("iterator_product::p4::number_of_configs",  &
            size(combined, 1), number_of_states**rank)
        combined = get_combined(iterators, constraints)
        call assertion%is_equal("iterator_product::p4_pleq::number_of_configs",  &
            size(combined, 1), get_triangular_number(number_of_states,rank))

        rank = 5
        iterators = [aniterator, aniterator, aniterator, aniterator, aniterator]

        combined = get_combined(iterators)
        call assertion%is_equal("iterator_product::p5::number_of_configs",  &
            size(combined, 1), number_of_states**rank)
        combined = get_combined(iterators, constraints)
        call assertion%is_equal("iterator_product::p5_pleq::number_of_configs",  &
            size(combined, 1), get_triangular_number(number_of_states,rank))
    end subroutine run

    subroutine cleanup(this)
        class(iterator_product_test), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(iterator_product_test), intent(inout) :: this
    end subroutine clear

    function get_combined(iterators, constraints) result(combined)
        type(iterator), dimension(:) :: iterators
        type(iterator_constraint_wrapper), dimension(:), intent(in), optional :: constraints
        integer, dimension(:,:), allocatable :: combined

        type(iterator_product) :: iproduct
        type(iterator) :: aniterator

        iproduct = iterator_product(constraints)
        aniterator = iproduct%get_product_row_major(iterators)
        combined = aniterator%get_all_items()
    end function get_combined

    function get_combined_column_major(iterators, constraints) result(combined)
        type(iterator), dimension(:) :: iterators
        type(iterator_constraint_wrapper), dimension(:), intent(in), optional :: constraints
        integer, dimension(:,:), allocatable :: combined

        type(iterator_product) :: iproduct
        type(iterator) :: aniterator

        iproduct = iterator_product(constraints)
        aniterator = iproduct%get_product_column_major(iterators)
        combined = aniterator%get_all_items()
    end function get_combined_column_major

    function get_triangular_number(nstates, nranks) result(triangular_number)
        integer, intent(in) :: nstates, nranks
        integer :: triangular_number

        integer :: idx

        triangular_number = nstates
        do idx = 1, nranks-1
            triangular_number = triangular_number*(nstates+idx)
        end do

        do idx = nranks, 2, -1
            triangular_number = triangular_number/idx
        end do
    end function get_triangular_number

end module iterator_product_test_module
