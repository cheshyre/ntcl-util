module pleq_constraint_module
    use :: iterator_constraint_module, only : iterator_constraint
    implicit none
    private

    public :: pleq_constraint

    type, extends(iterator_constraint) :: pleq_constraint
    contains
        procedure :: is_valid => is_valid
    end type pleq_constraint
contains
    function is_valid(this, configs) result(mask)
        class(pleq_constraint), intent(in) :: this
        integer, dimension(:,:), intent(in) :: configs
        logical, dimension(:), allocatable :: mask

        integer :: idx

        allocate(mask(size(configs, 1)))
        mask = .true.
        do idx = 1, size(configs, 2) - 1
            mask = mask .and. configs(:,idx) <= configs(:,idx+1)
        end do
    end function is_valid
end module pleq_constraint_module
