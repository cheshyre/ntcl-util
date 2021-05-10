module pneq_constraint_module
    use :: iterator_constraint_module, only : iterator_constraint
    implicit none
    private

    public :: pneq_constraint

    type, extends(iterator_constraint) :: pneq_constraint
    contains
        procedure :: is_valid => is_valid
    end type pneq_constraint
contains
    function is_valid(this, configs) result(mask)
        class(pneq_constraint), intent(in) :: this
        integer, dimension(:,:), intent(in) :: configs
        logical, dimension(:), allocatable :: mask

        integer :: a, b, nparticles

        nparticles = size(configs, 2)

        allocate(mask(size(configs, 1)))
        mask = .true.
        do a = 1, nparticles - 1
            do b = a + 1, nparticles
                mask = mask .and. configs(:,a) /= configs(:,b)
            end do
        end do
    end function is_valid
end module pneq_constraint_module
