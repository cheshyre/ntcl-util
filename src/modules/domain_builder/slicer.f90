module slicer_module
    use, intrinsic :: iso_fortran_env, only : int64
    use :: slice_module, only : slice

    implicit none
    private

    public :: slicer

    type, abstract :: slicer
    contains
        procedure(get_by_sizes_interface), nopass, deferred :: get_slices_by_sizes
        procedure(get_by_number_interface), nopass, deferred :: get_slices_by_number
    end type slicer

    abstract interface
        function get_by_sizes_interface(total_size, size_pr_slice) result(slices)
            import :: int64
            import :: slice

            integer(int64), intent(in) :: total_size, size_pr_slice
            type(slice), dimension(:), allocatable :: slices
        end function get_by_sizes_interface

        function get_by_number_interface(total_size, number_of_slices) result(slices)
            import :: int64
            import :: slice

            integer(int64), intent(in) :: total_size, number_of_slices
            type(slice), dimension(:), allocatable :: slices
        end function get_by_number_interface
    end interface
end module slicer_module
