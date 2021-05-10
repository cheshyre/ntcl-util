module iterator_constraint_module
    implicit none
    private

    public :: iterator_constraint

    type, abstract :: iterator_constraint
    contains
        procedure(is_valid_interface), deferred :: is_valid
    end type iterator_constraint

    abstract interface
        function is_valid_interface(this, configs) result(mask)
            import :: iterator_constraint
            class(iterator_constraint), intent(in) :: this
            integer, dimension(:,:), intent(in) :: configs
            logical, dimension(:), allocatable :: mask
        end function is_valid_interface
    end interface
end module iterator_constraint_module
