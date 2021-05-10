module quantum_number_lookup_module
    use :: quantum_number_descriptor_module, only : quantum_number_descriptor
    implicit none
    private

    public :: quantum_number_lookup

    type :: quantum_number_lookup
        integer :: number_of_items
        type(quantum_number_descriptor) :: descriptor
        integer, dimension(:), allocatable :: lookup
    contains
        procedure :: is_half_integer => is_half_integer
        procedure :: get_size => get_size
        procedure :: is_equal => is_equal
        generic :: operator(.eq.) => is_equal
        procedure :: is_not_equal => is_not_equal
        generic :: operator(.ne.) => is_not_equal
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type quantum_number_lookup

    interface quantum_number_lookup
        module procedure constructor
    end interface quantum_number_lookup

contains
    pure function constructor(descriptor, lookup) result(this)
        type(quantum_number_descriptor), intent(in) :: descriptor
        integer, dimension(:), intent(in) :: lookup
        type(quantum_number_lookup) :: this

        call this%clear()
        this%descriptor = descriptor
        this%lookup = lookup
        this%number_of_items = size(lookup)
    end function constructor

    pure logical function is_equal(this, other)
        class(quantum_number_lookup), intent(in) :: this, other

        is_equal = this%number_of_items == other%number_of_items .and. &
            this%descriptor == other%descriptor .and. &
            (allocated(this%lookup) .eqv. allocated(other%lookup))
        if ( allocated(this%lookup) .and. allocated(other%lookup)) &
            is_equal = is_equal .and. &
                size(this%lookup) == size(other%lookup) .and. &
                all(this%lookup == other%lookup)
    end function is_equal

    pure logical function is_not_equal(this, other)
        class(quantum_number_lookup), intent(in) :: this, other

        is_not_equal = .not. this == other
    end function is_not_equal

    pure logical function is_half_integer(this)
        class(quantum_number_lookup), intent(in) :: this

        is_half_integer = this%descriptor%half_integer
    end function is_half_integer

    pure function get_size(this) result(number_of_items)
        class(quantum_number_lookup), intent(in) :: this
        integer :: number_of_items

        number_of_items = this%number_of_items
    end function get_size

    pure subroutine cleanup(this)
        class(quantum_number_lookup), intent(inout) :: this

        if (allocated(this%lookup)) deallocate(this%lookup)
        call this%clear()
    end subroutine cleanup

    pure subroutine clear(this)
        class(quantum_number_lookup), intent(inout) :: this

        call this%descriptor%clear()
        this%number_of_items = 0
    end subroutine clear
end module quantum_number_lookup_module
