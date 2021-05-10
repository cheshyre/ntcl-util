module tile_module
    use, intrinsic :: iso_fortran_env, only : int64, int32
    use :: domain_module, only : domain
    implicit none
    private

    public :: tile

    type, extends(domain) :: tile
        integer(int64) :: row_start
        integer(int64) :: row_end
        integer(int64) :: row_offset
        integer(int64) :: row_size
        integer(int64) :: col_start
        integer(int64) :: col_end
        integer(int64) :: col_offset
        integer(int64) :: col_size
    end type tile

    interface tile
        module procedure constructor_int32
        module procedure constructor_int64
        module procedure constructor_2int64_2int32
        module procedure constructor_2int64_2int32_v2
    end interface tile
contains
    pure function constructor_int64(rstart, rend, cstart, cend) result(atile)
        integer(int64), intent(in) :: rstart
        integer(int64), intent(in) :: rend
        integer(int64), intent(in) :: cstart
        integer(int64), intent(in) :: cend
        type(tile) :: atile

        call atile%clear()
        atile%domain = domain([rstart, cstart], [rend, cend])

        atile%row_start = rstart
        atile%row_end = rend
        atile%col_start = cstart
        atile%col_end = cend

        atile%row_offset = atile%all_offsets(1)
        atile%col_offset = atile%all_offsets(2)

        atile%row_size = atile%all_sizes(1)
        atile%col_size = atile%all_sizes(2)
    end function constructor_int64

    pure function constructor_int32(rstart, rend, cstart, cend) result(atile)
        integer(int32), intent(in) :: rstart
        integer(int32), intent(in) :: rend
        integer(int32), intent(in) :: cstart
        integer(int32), intent(in) :: cend
        type(tile) :: atile

        atile = constructor_int64(int(rstart, int64), int(rend, int64), int(cstart, int64), int(cend, int64))
    end function constructor_int32

    pure function constructor_2int64_2int32(rstart, rend, cstart, cend) result(atile)
        integer(int64), intent(in) :: rstart
        integer(int64), intent(in) :: rend
        integer(int32), intent(in) :: cstart
        integer(int32), intent(in) :: cend
        type(tile) :: atile

        atile = constructor_int64(int(rstart, int64), int(rend, int64), int(cstart, int64), int(cend, int64))
    end function constructor_2int64_2int32

    pure function constructor_2int64_2int32_v2(rstart, rend, cstart, cend) result(atile)
        integer(int32), intent(in) :: rstart
        integer(int64), intent(in) :: rend
        integer(int32), intent(in) :: cstart
        integer(int64), intent(in) :: cend
        type(tile) :: atile

        atile = constructor_int64(int(rstart, int64), int(rend, int64), int(cstart, int64), int(cend, int64))
    end function constructor_2int64_2int32_v2
end module tile_module
