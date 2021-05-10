module quantum_number_module
    use :: quantum_number_descriptor_module, only : quantum_number_descriptor
    implicit none
    private

    public :: quantum_number

    type :: quantum_number
        type(quantum_number_descriptor) :: descriptor
        integer :: qn_value
    contains
        procedure :: equal => equal
        procedure :: not_equal => not_equal
        procedure :: get_descriptor => get_descriptor
        procedure :: is_descr => is_descr
        procedure :: is_chars => is_chars
        procedure :: get_value => get_value
        procedure :: get_value_as_string => get_value_as_string
        procedure :: get_string => get_string
        procedure :: as_infix => as_infix
        procedure :: is_set => is_set
        procedure :: clear => clear
        generic :: operator(.eq.) => equal
        generic :: operator(.ne.) => not_equal
        generic :: is => is_descr, is_chars
        procedure :: is_compatible_two => is_compatible_two
        generic :: is_compatible => is_compatible_two
    end type quantum_number

    interface quantum_number
        module procedure constructor
        module procedure constructor_blank
        module procedure constructor_easy
    end interface quantum_number

    integer, parameter :: NOT_SET = -999
contains
    pure function constructor(descriptor, val) result(this)
        type(quantum_number_descriptor), intent(in) :: descriptor
        integer, intent(in), optional :: val
        type(quantum_number) :: this

        this = quantum_number()
        this%descriptor = descriptor
        this%qn_value = NOT_SET
        if (present(val)) this%qn_value = val
    end function constructor

    pure function constructor_easy(qname, val) result(this)
        character(len=*), intent(in) :: qname
        integer, intent(in), optional :: val
        type(quantum_number) :: this

        this = quantum_number(quantum_number_descriptor(qname), val)
    end function constructor_easy

    pure function constructor_blank() result(this)
        type(quantum_number) :: this

        call this%clear()
    end function constructor_blank

    elemental logical function equal(this, other)
        class(quantum_number), intent(in) :: this, other

        equal = this%qn_value == other%qn_value .and. this%descriptor == other%descriptor
    end function equal

    elemental logical function not_equal(this, other)
        class(quantum_number), intent(in) :: this, other

        not_equal = .not. this == other
    end function not_equal

    function get_string(this) result(chars)
        class(quantum_number), intent(in) :: this
        character(len=:), allocatable :: chars

        chars = this%descriptor%identifier%as_char_array()//"="//trim(this%get_value_as_string())
    end function get_string

    function as_infix(this) result(chars)
        class(quantum_number), intent(in) :: this
        character(len=:), allocatable :: chars

        chars = this%descriptor%identifier%as_char_array()//trim(this%get_value_as_string())
    end function as_infix

    function get_value_as_string(this) result(chars)
        class(quantum_number), intent(in) :: this
        character(len=:), allocatable :: chars

        character(len=4) :: dummy
        integer :: mylength

        if (this%qn_value == 0) then
            mylength = 1
        else
            mylength = floor(log10(real(abs(this%qn_value)))) + 1
        end if
        if ( this%qn_value < 0 ) mylength = mylength + 1

        if ( mylength == 1 ) then
            write(dummy, '(i1)') this%qn_value
        else if (mylength == 2 ) then
            write(dummy, '(i2)') this%qn_value
        else if (mylength == 3 ) then
            write(dummy, '(i3)') this%qn_value
        else if (mylength == 4 ) then
            write(dummy, '(i4)') this%qn_value
        else
            stop 'quantum_number::get_value_as_string::Not enough space.'
        end if

        chars = dummy
    end function get_value_as_string

    pure function get_descriptor(this) result(descriptor)
        class(quantum_number), intent(in) :: this
        type(quantum_number_descriptor) :: descriptor

        descriptor = this%descriptor
    end function get_descriptor

    pure logical function is_descr(this, descriptor)
        class(quantum_number), intent(in) :: this
        type(quantum_number_descriptor), intent(in) :: descriptor

        is_descr = this%descriptor%is(descriptor%identifier)
    end function is_descr

    pure logical function is_chars(this, chars)
        class(quantum_number), intent(in) :: this
        character(len=*), intent(in) :: chars

        is_chars = this%descriptor%is(chars)
    end function is_chars

    pure integer function get_value(this)
        class(quantum_number), intent(in) :: this

        get_value = this%qn_value
    end function get_value

    elemental logical function is_set(this)
        class(quantum_number), intent(in) :: this
        is_set = this%qn_value /= NOT_SET
    end function is_set

    logical function is_compatible_two(this, a, b)
        class(quantum_number), intent(in) :: this, a, b

        integer :: factor, afactor, bfactor
        is_compatible_two = .true.
        if ( .not. this%is_set() ) return

        factor = 2; afactor = 2; bfactor = 2
        if ( this%descriptor%half_integer ) factor = 1
        if ( a%descriptor%half_integer ) afactor = 1
        if ( b%descriptor%half_integer ) bfactor = 1
        is_compatible_two = a%is_set() .and. b%is_set() .and. &
                factor*this%qn_value == afactor*a%qn_value + bfactor*b%qn_value
    end function is_compatible_two

    pure subroutine clear(this)
        class(quantum_number), intent(inout) :: this

        call this%descriptor%clear()
        this%qn_value = NOT_SET
    end subroutine clear
end module quantum_number_module
