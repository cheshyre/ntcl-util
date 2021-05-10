module domain_module
    use :: iso_fortran_env, only : int64
    implicit none
    private

    public :: domain

    type :: domain
        integer :: number_of_dimensions
        integer(int64), dimension(:), allocatable :: all_first, all_last, all_offsets, all_sizes
    contains
        procedure :: is_equal => is_equal
        generic :: operator(==) => is_equal
        procedure :: clear => clear
        procedure :: cleanup => cleanup
    end type domain

    interface domain
        module procedure constructor_int64
        module procedure constructor_int32
    end interface domain
contains
    pure function constructor_int64(all_first, all_last) result(this)
        integer(int64), dimension(:), intent(in) :: all_first, all_last
        type(domain) :: this

        call this%clear()
        if (size(all_first) /= size(all_last)) return

        this%number_of_dimensions = size(all_first)

        this%all_first = all_first
        this%all_last = all_last
        this%all_offsets = this%all_first -1
        this%all_sizes = this%all_last - this%all_first + 1
        where ( this%all_sizes < 0) this%all_sizes = 0_int64
    end function constructor_int64

    pure function constructor_int32(all_first, all_last) result(this)
        integer, dimension(:), intent(in) :: all_first, all_last
        type(domain) :: this

        this = domain(int(all_first, int64), int(all_last, int64))
    end function constructor_int32

    pure function is_equal(this, other) result(res)
        class(domain), intent(in) :: this, other
        logical :: res

        res = &
            allocated(this%all_first) .and. allocated(other%all_first) .and. &
            allocated(this%all_last) .and. allocated(other%all_last) .and. &
            allocated(this%all_offsets) .and. allocated(other%all_offsets) .and. &
            allocated(this%all_sizes) .and. allocated(other%all_sizes)
        if (.not. res) return

        if (this%number_of_dimensions /= other%number_of_dimensions ) res = .false.
        if (size(this%all_first) == size(other%all_first)) then
            res = res .and. all(this%all_first == other%all_first)
        else
            res = .false.
        end if

        if (size(this%all_last) == size(other%all_last)) then
            res = res .and. all(this%all_last == other%all_last)
        else
            res = .false.
        end if

        if (size(this%all_offsets) == size(other%all_offsets)) then
            res = res .and. all(this%all_offsets == other%all_offsets)
        else
            res = .false.
        end if

        if (size(this%all_sizes) == size(other%all_sizes)) then
            res = res .and. all(this%all_sizes == other%all_sizes)
        else
            res = .false.
        end if
    end function is_equal

    pure subroutine cleanup(this)
        class(domain), intent(inout) :: this

        if (allocated(this%all_first)) deallocate(this%all_first)
        if (allocated(this%all_last)) deallocate(this%all_last)
        if (allocated(this%all_offsets)) deallocate(this%all_offsets)
        if (allocated(this%all_sizes)) deallocate(this%all_sizes)

        call this%clear()
    end subroutine cleanup
        
    pure subroutine clear(this)
        class(domain), intent(inout) :: this

        this%number_of_dimensions = 0
    end subroutine clear
end module domain_module
