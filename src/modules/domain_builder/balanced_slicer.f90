module balanced_slicer_module
    use, intrinsic :: iso_fortran_env, only : int64
    use :: slice_module, only : slice
    use :: slicer_module, only : slicer
    implicit none
    private

    public :: balanced_slicer

    type, extends(slicer) :: balanced_slicer
    contains
        procedure, nopass :: get_slices_by_sizes => get_slices_by_sizes
        procedure, nopass :: get_slices_by_number => get_slices_by_number
    end type balanced_slicer
contains
    function get_slices_by_sizes(total_size, size_pr_slice) result(slices)
        integer(int64), intent(in) :: total_size, size_pr_slice
        type(slice), dimension(:), allocatable :: slices

        slices = get_slices(total_size, total_size/size_pr_slice, size_pr_slice)
    end function get_slices_by_sizes

    function get_slices_by_number(total_size, number_of_slices) result(slices)
        integer(int64), intent(in) :: total_size, number_of_slices
        type(slice), dimension(:), allocatable :: slices

        slices = get_slices(total_size, number_of_slices, total_size/number_of_slices)
    end function get_slices_by_number

    function get_slices(total_size, number_of_slices, size_pr_slice) result(slices)
        integer(int64), intent(in) :: number_of_slices, total_size, size_pr_slice
        type(slice), dimension(:), allocatable :: slices

        integer(int64) :: remainder, first, last, idx

        remainder = mod(total_size, number_of_slices)

        allocate(slices(number_of_slices))

        last = 0
        do idx = 1, number_of_slices
            first = last + 1
            last = last + size_pr_slice
            if ( idx <= remainder ) last = last + 1
            slices(idx) = slice(first, last)
        end do
    end function get_slices
end module balanced_slicer_module
