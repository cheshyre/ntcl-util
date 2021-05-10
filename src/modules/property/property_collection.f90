module property_collection_module
    use :: property_module, only : property
    use :: string_module, only : string

    implicit none
    private

    public :: property_collection

    type, extends(property) :: property_collection
        class(property), dimension(:), allocatable :: properties
    contains
        procedure :: is_string => is_string
        procedure :: cleanup => cleanup
    end type property_collection

    interface property_collection
        module procedure constructor
    end interface property_collection

contains
    function constructor(properties) result(this)
        class(property), dimension(:), intent(in) :: properties   
        type(property_collection) :: this

        this%properties = properties
    end function constructor

    logical function is_string(this, astring)
        class(property_collection), intent(in) :: this
        type(string), intent(in) :: astring

        integer :: idx

        is_string = .false.

        if ( allocated(this%properties) ) then
            do idx = 1, size(this%properties)
                is_string = is_string .or. this%properties(idx)%is(astring)
            end do
        end if
    end function is_string

    subroutine cleanup(this)
        class(property_collection), intent(inout) :: this

        integer :: idx

        if ( allocated(this%properties) ) then
            do idx = 1, size(this%properties)
                call this%properties(idx)%cleanup()
            end do
            deallocate(this%properties)
        end if
    end subroutine cleanup
end module property_collection_module
