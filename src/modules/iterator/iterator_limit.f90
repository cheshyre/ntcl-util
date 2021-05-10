module iterator_limit_module
    implicit none
    private

    public :: iterator_limit

    type :: iterator_limit
        integer :: first, last, stride
    contains
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type iterator_limit

    interface iterator_limit
        module procedure constructor
    end interface iterator_limit

contains
    function constructor(first, last, stride) result(this)
        integer, intent(in) :: first, last
        integer, intent(in), optional :: stride
        type(iterator_limit) :: this

        call this%clear()
        this%first = first
        this%last = last
        if (present(stride)) this%stride = stride
    end function constructor

    subroutine cleanup(this)
        class(iterator_limit), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(iterator_limit), intent(inout) :: this

        this%first = 0
        this%last = 0
        this%stride = 1
    end subroutine clear
end module iterator_limit_module
