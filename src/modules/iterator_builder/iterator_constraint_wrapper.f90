module iterator_constraint_wrapper_module
    use :: iterator_constraint_module, only : iterator_constraint
    implicit none
    private

    public :: iterator_constraint_wrapper

    type, extends(iterator_constraint) :: iterator_constraint_wrapper
        class(iterator_constraint), allocatable :: constraint
    contains
        procedure :: is_valid => is_valid
        procedure :: cleanup => cleanup
    end type iterator_constraint_wrapper

    interface iterator_constraint_wrapper
        module procedure constructor
    end interface iterator_constraint_wrapper
contains
    function constructor(constraint) result(this)
        class(iterator_constraint), intent(in) :: constraint
        type(iterator_constraint_wrapper) :: this

        allocate(this%constraint, source=constraint)
    end function constructor

    function is_valid(this, configs) result(mask)
        class(iterator_constraint_wrapper), intent(in) :: this
        integer, dimension(:,:), intent(in) :: configs
        logical, dimension(:), allocatable :: mask

        mask = this%constraint%is_valid(configs)
    end function is_valid

    subroutine cleanup(this)
        class(iterator_constraint_wrapper), intent(inout) :: this

        if (allocated(this%constraint)) then
            deallocate(this%constraint)
        end if
    end subroutine cleanup
end module iterator_constraint_wrapper_module
