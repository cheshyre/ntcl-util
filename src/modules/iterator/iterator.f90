module iterator_module
    use :: integer_range_module, only : integer_range
    use :: iterator_limit_module, only : iterator_limit
    implicit none
    private

    public :: iterator

    type :: iterator
        integer, dimension(:,:), allocatable :: internal_storage
    contains
        procedure :: get_size => get_size
        procedure :: get_items => get_items
        procedure :: get_all_items => get_all_items
        procedure :: get_max_rank => get_max_rank
        procedure :: is_equal => is_equal
        procedure :: is_not_equal => is_not_equal
        generic :: operator(.eq.) => is_equal
        generic :: operator(.ne.) => is_not_equal
        procedure :: cleanup => cleanup
    end type iterator

    interface iterator
        module procedure constructor
        module procedure constructor_from_data
        module procedure constructor_from_range
        module procedure constructor_from_vector
        module procedure constructor_from_limit
    end interface iterator

contains
    function constructor_from_range(rng) result(this)
        type(integer_range), intent(in) :: rng
        type(iterator) :: this

        this = iterator(reshape(rng%get_range(), [rng%get_size(), 1]))
    end function constructor_from_range

    function constructor(first, last, stride) result(this)
        integer, intent(in) :: first, last
        integer, intent(in), optional :: stride
        type(iterator) :: this

        type(integer_range) :: rng

        if (present(stride)) then
            rng = integer_range(first, last, stride)
        else
            rng = integer_range(first, last)
        end if
        this = iterator(rng)
    end function constructor

    function constructor_from_data(iterator_data, mask) result(this)
        integer, dimension(:,:), intent(in) :: iterator_data
        logical, dimension(:), intent(in), optional :: mask
        type(iterator) :: this

        integer :: idx

        if ( present(mask) ) then
            if (size(mask) /= size(iterator_data, 1)) stop
            this%internal_storage = iterator_data(pack([(idx, idx = 1, size(mask))], mask), :)
        else
            this%internal_storage = iterator_data
        end if
    end function constructor_from_data

    function constructor_from_vector(vector) result(this)
        integer, dimension(:), intent(in) :: vector
        type(iterator) :: this

        this = iterator(reshape(vector, [size(vector), 1]))
    end function constructor_from_vector

    function constructor_from_limit(ilim) result(this)
        type(iterator_limit), intent(in) :: ilim
        type(iterator) :: this

        this = iterator(ilim%first, ilim%last, ilim%stride)
    end function constructor_from_limit

    logical function is_equal(this, other)
        class(iterator), intent(in) :: this, other

        is_equal = allocated(this%internal_storage) .eqv. allocated(other%internal_storage)
        if (allocated(this%internal_storage) .and. allocated(other%internal_storage)) then
            is_equal = is_equal .and. &
                all(shape(this%internal_storage) == shape(other%internal_storage))
            if ( is_equal) is_equal = is_equal .and. &
                all(this%internal_storage == other%internal_storage)
        end if
    end function is_equal

    logical function is_not_equal(this, other)
        class(iterator), intent(in) :: this, other

        is_not_equal = .not. this == other
    end function is_not_equal

    pure integer function get_size(this)
        class(iterator), intent(in) :: this

        get_size = 0
        if (allocated(this%internal_storage)) get_size = size(this%internal_storage, 1)
    end function get_size

    pure integer function get_max_rank(this)
        class(iterator), intent(in) :: this

        get_max_rank = 0
        if (allocated(this%internal_storage)) get_max_rank = size(this%internal_storage, 2)
    end function get_max_rank

    pure function get_items(this, rank) result(items)
        class(iterator), intent(in) :: this
        integer, intent(in) :: rank
        integer, dimension(:), allocatable :: items

        allocate(items(this%get_size()))
        if (rank > 0 .and. rank <= this%get_max_rank() ) then
            items = this%internal_storage(:,rank)
        else
            items = 0
        end if
    end function get_items

    pure function get_all_items(this) result(items)
        class(iterator), intent(in) :: this
        integer, dimension(:,:), allocatable :: items

        items = this%internal_storage
    end function get_all_items

    subroutine cleanup(this)
        class(iterator), intent(inout) :: this

        if (allocated(this%internal_storage)) deallocate(this%internal_storage)
    end subroutine cleanup
end module iterator_module
