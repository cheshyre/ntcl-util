module integer_range_module
    implicit none
    private

    public :: integer_range

    type :: integer_range
        integer :: first, last, stride
        integer :: number_of_items
    contains
        procedure :: get_size => get_size
        procedure :: get_range => get_range
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type integer_range

    interface integer_range
        module procedure constructor
        module procedure constructor_with_stride
    end interface integer_range

contains
    pure function constructor(first, last) result(this)
        integer, intent(in) :: first, last
        type(integer_range) :: this

        call this%clear()
        this%number_of_items = last - first + 1
        this%first = first
        this%last = last
    end function constructor

    pure function constructor_with_stride(first, last, stride) result(this)
        integer, intent(in) :: first, last, stride
        type(integer_range) :: this

        call this%clear()
        this%number_of_items = (last - first)/stride + 1
        this%first = first
        this%last = last
        this%stride = stride
    end function constructor_with_stride

    pure function get_size(this) result(nitems)
        class(integer_range), intent(in) :: this
        integer :: nitems

        nitems = this%number_of_items
    end function get_size

    pure function get_range(this) result(rng)
        class(integer_range), intent(in) :: this
        integer, dimension(:), allocatable :: rng

        integer :: idx, val
        
        allocate(rng(this%number_of_items))

        idx = 0
        do val = this%first, this%last, this%stride
            idx = idx + 1
            rng(idx) = val
        end do
    end function get_range

    pure subroutine cleanup(this)
        class(integer_range), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    pure subroutine clear(this)
        class(integer_range), intent(inout) :: this

        this%number_of_items = 0
        this%first = 0
        this%last = 0
        this%stride = 1
    end subroutine clear
end module integer_range_module
