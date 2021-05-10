module fortran_c_intrinsics
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int

    implicit none

    interface
        type(c_ptr) function increment_pointer(ptr, number_of_bytes) result(resptr) bind(c, name="increment_pointer")
            import :: c_ptr, c_size_t
            type(c_ptr), value :: ptr
            integer(c_size_t), value :: number_of_bytes
        end function increment_pointer

        type(c_ptr) function memcpy(src, dst, number_of_bytes) bind(c, name="memcpy")
            import:: c_ptr, c_size_t
            type(c_ptr), value :: src, dst
            integer(c_size_t), value :: number_of_bytes
        end function memcpy

        type(c_ptr) function memset(src, val, number_of_bytes) bind(c, name="memset")
            import:: c_ptr, c_size_t, c_int
            type(c_ptr), value :: src
            integer(c_int), value :: val
            integer(c_size_t), value :: number_of_bytes
        end function memset
    end interface
end module fortran_c_intrinsics
