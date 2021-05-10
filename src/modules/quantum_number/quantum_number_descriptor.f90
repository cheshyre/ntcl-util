module quantum_number_descriptor_module
    use :: string_module, only : string
    implicit none
    private

    public :: quantum_number_descriptor

    type :: quantum_number_descriptor
        type(string) :: identifier
        logical :: half_integer
    contains
        procedure :: equal => equal
        procedure :: clear => clear
        generic :: operator(==) => equal
        procedure :: is_string => is_string
        procedure :: is_chars => is_chars
        generic :: is => is_string, is_chars
    end type quantum_number_descriptor

    interface quantum_number_descriptor
        module procedure constructor
    end interface quantum_number_descriptor

contains
    pure function constructor(identifier, half_integer) result(this)
        character(len=*), intent(in) :: identifier
        logical, intent(in), optional :: half_integer
        type(quantum_number_descriptor) :: this

        call this%clear()
        this%identifier = identifier
        if (present(half_integer)) this%half_integer = half_integer
    end function constructor

    pure logical function equal(this, other)
        class(quantum_number_descriptor), intent(in) :: this
        type(quantum_number_descriptor), intent(in) :: other

        equal = this%identifier == other%identifier .and. (this%half_integer .eqv. other%half_integer)
    end function equal

    pure logical function is_string(this, str)
        class(quantum_number_descriptor), intent(in) :: this
        type(string), intent(in) :: str

        is_string = this%identifier == str
    end function is_string

    pure logical function is_chars(this, chars)
        class(quantum_number_descriptor), intent(in) :: this
        character(len=*), intent(in) :: chars

        is_chars = this%is(string(chars))
    end function is_chars

    pure subroutine clear(this)
        class(quantum_number_descriptor), intent(inout) :: this

        this%identifier = ''
        this%half_integer = .false.
    end subroutine clear
end module quantum_number_descriptor_module
