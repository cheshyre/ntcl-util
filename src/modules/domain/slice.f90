module slice_module
    use, intrinsic :: iso_fortran_env, only : int64
    use :: domain_module, only : domain
    implicit none
    private

    public :: slice

    type, extends(domain) :: slice
        integer(int64) :: first, last, offset, nsize
    end type slice

    interface slice
        module procedure constructor_int64
        module procedure constructor_int32
    end interface slice

contains
    pure function constructor_int64(first, last) result(this)
        integer(int64), intent(in) :: first, last
        type(slice) :: this

        call this%clear()
        this%domain = domain([first], [last])
        this%first = this%all_first(1)
        this%last = this%all_last(1)
        this%offset = this%all_offsets(1)
        this%nsize = this%all_sizes(1)
    end function constructor_int64

    pure function constructor_int32(first, last) result(this)
        integer, intent(in) :: first, last
        type(slice) :: this

        this = slice(int(first, int64), int(last, int64))
    end function constructor_int32
end module slice_module
