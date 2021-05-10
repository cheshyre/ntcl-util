module iterator_product_module
    use :: iterator_module, only : iterator
    use :: iterator_constraint_wrapper_module, only : iterator_constraint_wrapper
    implicit none
    private

    public :: iterator_product

    type :: iterator_product
        integer :: number_of_constraints
        type(iterator_constraint_wrapper), dimension(:), allocatable :: constraints
    contains
        procedure :: get_product_column_major => get_product_column_major
        procedure :: get_product_row_major => get_product_row_major
        procedure, private :: constrain => constrain_configurations
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type iterator_product

    interface iterator_product
        module procedure constructor_with_constraints
    end interface iterator_product

contains
    function constructor_with_constraints(constraints) result(this)
        type(iterator_constraint_wrapper), dimension(:), intent(in), optional :: constraints
        type(iterator_product) :: this

        call this%clear()
        if (present(constraints)) then
            this%constraints = constraints
            this%number_of_constraints = size(constraints)
        end if
    end function constructor_with_constraints

    function get_product_row_major(this, iterators) result(aniterator)
        class(iterator_product), intent(in) :: this
        type(iterator), dimension(:), intent(in) :: iterators
        type(iterator) :: aniterator

        integer, dimension(:,:), allocatable :: previous_indices, configs
        integer :: number_of_iterators
        integer :: idx, rank, number_of_previous_configs, previous_idx
        integer :: config, rank_idx, number_of_current_configs, size_of_this_rank
        integer, dimension(:), allocatable :: valid_for_this_rank

        number_of_iterators = size(iterators)
        number_of_current_configs = 1
        allocate(configs(number_of_current_configs, 0))
        previous_indices = configs
        do rank = 1, number_of_iterators
            size_of_this_rank = iterators(rank)%get_size()
            valid_for_this_rank = iterators(rank)%get_items(1)
            number_of_previous_configs = number_of_current_configs
            number_of_current_configs = number_of_previous_configs*size_of_this_rank

            deallocate(configs)
            allocate(configs(number_of_current_configs, rank))
            do previous_idx = 1, number_of_previous_configs
                do idx = 1, size_of_this_rank
                    config = (previous_idx - 1)*size_of_this_rank + idx
                    do rank_idx = 1, rank - 1
                        configs(config, rank_idx) = previous_indices(previous_idx, rank_idx)
                    end do
                    configs(config, rank) = valid_for_this_rank(idx)
                end do
            end do
            previous_indices = configs
        end do
        deallocate(previous_indices)

        aniterator = iterator(this%constrain(configs))
    end function get_product_row_major

    function get_product_column_major(this, iterators) result(aniterator)
        class(iterator_product), intent(in) :: this
        type(iterator), dimension(:), intent(in) :: iterators
        type(iterator) :: aniterator

        integer, dimension(:,:), allocatable :: previous_indices, configs
        integer :: number_of_iterators
        integer :: idx, rank, number_of_previous_configs, previous_idx
        integer :: config, rank_idx, number_of_current_configs, size_of_this_rank
        integer, dimension(:), allocatable :: valid_for_this_rank

        number_of_iterators = size(iterators)
        number_of_current_configs = 1
        allocate(configs(number_of_current_configs, 0))
        previous_indices = configs
        do rank = 1, number_of_iterators
            size_of_this_rank = iterators(rank)%get_size()
            valid_for_this_rank = iterators(rank)%get_items(1)
            number_of_previous_configs = number_of_current_configs
            number_of_current_configs = number_of_previous_configs*size_of_this_rank

            deallocate(configs)
            allocate(configs(number_of_current_configs, rank))
            do idx = 1, size_of_this_rank
                do previous_idx = 1, number_of_previous_configs
                    config = (idx -1)*number_of_previous_configs + previous_idx
                    configs(config, :rank-1) = previous_indices(previous_idx, :)
                    configs(config, rank) = valid_for_this_rank(idx)
                end do
            end do
            previous_indices = configs
        end do
        deallocate(previous_indices)

        aniterator = iterator(this%constrain(configs))
    end function get_product_column_major

    function constrain_configurations(this, all_configs) result(configs)
        class(iterator_product), intent(in) :: this
        integer, dimension(:,:), intent(in) :: all_configs
        integer, dimension(:,:), allocatable :: configs

        integer :: nconfs, ndim, idx
        logical, dimension(:), allocatable :: mask
        integer, dimension(:), allocatable :: valid

        nconfs = size(all_configs, 1)
        ndim = size(all_configs, 2)

        allocate(mask(nconfs))
        mask = .true.

        do idx = 1, this%number_of_constraints
            mask = mask .and. this%constraints(idx)%is_valid(all_configs)
        end do

        valid = pack([(int(idx), idx=1, nconfs)], mask)
        nconfs = size(valid)

        allocate(configs(nconfs, ndim))
        configs(:,:) = all_configs(valid, :)
        deallocate(mask, valid)
    end function constrain_configurations

    subroutine cleanup(this)
        class(iterator_product), intent(inout) :: this

        integer :: idx

        if (allocated(this%constraints)) then
            do idx = 1, size(this%constraints)
                call this%constraints(idx)%cleanup()
            end do
            deallocate(this%constraints)
        end if
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(iterator_product), intent(inout) :: this

        this%number_of_constraints = 0
    end subroutine clear
end module iterator_product_module
