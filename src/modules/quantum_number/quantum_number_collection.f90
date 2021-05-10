module quantum_number_collection_module
    use :: quantum_number_module, only : quantum_number
    use :: quantum_number_descriptor_module, only : quantum_number_descriptor
    implicit none
    private

    public :: quantum_number_collection

    type :: quantum_number_collection
        type(quantum_number), dimension(:), allocatable :: quantum_numbers
    contains
        generic :: get_quantum_number => &
                get_quantum_number_chars, &
                get_quantum_number_descr
        generic :: operator(.eq.) => equal
        generic :: operator(.ne.) => not_equal
        procedure :: equal => equal
        procedure :: not_equal => not_equal
        procedure :: get_quantum_number_chars => get_quantum_number_chars
        procedure :: get_quantum_number_descr => get_quantum_number_descr
        procedure :: is_compatible => is_compatible
        procedure :: as_chars => as_chars
        procedure :: as_infix => as_infix
        procedure :: get_valid => get_valid
        procedure :: is_set => is_set
        procedure :: get_all_descriptors => get_all_descriptors
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type quantum_number_collection

    interface quantum_number_collection
        module procedure constructor_empty
        module procedure constructor
    end interface quantum_number_collection

contains
    function constructor_empty() result(this)
        type(quantum_number_collection) :: this

        call this%clear()
    end function constructor_empty

    function constructor(qnums) result(this)
        type(quantum_number), dimension(:), intent(in) :: qnums
        type(quantum_number_collection) :: this

        this = quantum_number_collection()
        this%quantum_numbers = qnums
    end function constructor

    pure function get_all_descriptors(this) result(array)
        class(quantum_number_collection), intent(in) :: this
        type(quantum_number_descriptor), dimension(:), allocatable :: array

        integer :: idx

        if ( allocated(this%quantum_numbers) ) then
            array = [(this%quantum_numbers(idx)%descriptor, idx = 1, size(this%quantum_numbers))]
        else
            allocate(array(0))
        end if
    end function get_all_descriptors

    pure type(quantum_number) function get_quantum_number_chars(this, chars)
        class(quantum_number_collection), intent(in) :: this
        character(len=*), intent(in) :: chars

        type(quantum_number_descriptor) :: descr
        integer :: idx

        call get_quantum_number_chars%clear()
        if (.not. allocated(this%quantum_numbers)) return

        do idx = 1, size(this%quantum_numbers)
            descr = this%quantum_numbers(idx)%get_descriptor()
            if (descr%is(chars)) then
                get_quantum_number_chars = this%quantum_numbers(idx)
                return
            endif
        end do
    end function get_quantum_number_chars

    logical function is_set(this, str)
        class(quantum_number_collection), intent(in) :: this
        character(len=*), intent(in) :: str

        type(quantum_number) :: qn

        qn = this%get_quantum_number(str)
        is_set = qn%is_set()
    end function is_set

    pure type(quantum_number) function get_quantum_number_descr(this, descr)
        class(quantum_number_collection), intent(in) :: this
        type(quantum_number_descriptor), intent(in) :: descr

        integer :: idx

        call get_quantum_number_descr%clear()
        if (.not. allocated(this%quantum_numbers)) return

        do idx = 1, size(this%quantum_numbers)
            if ( descr == this%quantum_numbers(idx)%get_descriptor() ) then
                get_quantum_number_descr = this%quantum_numbers(idx)
                return
            endif
        end do
    end function get_quantum_number_descr

    pure logical function equal(this, other)
        class(quantum_number_collection), intent(in) :: this, other

        equal = allocated(this%quantum_numbers) .eqv. allocated(other%quantum_numbers)
        if (allocated(this%quantum_numbers) .and. allocated(other%quantum_numbers) ) then
            equal = equal .and. size(this%quantum_numbers) == size(other%quantum_numbers)
            if ( equal ) &
                equal = equal .and. all(this%quantum_numbers == other%quantum_numbers)
        end if
    end function equal

    pure logical function not_equal(this, other)
        class(quantum_number_collection), intent(in) :: this, other

        not_equal = .not. this == other
    end function not_equal

    pure function get_valid(this) result(valid)
        class(quantum_number_collection), intent(in) :: this
        type(quantum_number), dimension(:), allocatable :: valid

        integer :: idx, counter

        allocate(valid(count(this%quantum_numbers(:)%is_set())))
        counter = 0
        do idx =1, size(this%quantum_numbers)
            if (this%quantum_numbers(idx)%is_set() ) then
                counter = counter + 1
                valid(counter) = this%quantum_numbers(idx)
            end if
        end do
    end function get_valid
            
    function as_chars(this) result(chars)
        class(quantum_number_collection), intent(in) :: this
        character(len=:), allocatable :: chars

        integer :: idx
        logical :: have_symmetry

        have_symmetry = .false.
        if (.not. allocated(this%quantum_numbers)) then
            chars = "no symmetry"
            return
        end if

        if (size(this%quantum_numbers) == 0) then
            chars = "no symmetry"
            return
        end if

        do idx = 1, size(this%quantum_numbers)-1
            associate ( qn => this%quantum_numbers(idx) )
                have_symmetry = have_symmetry .or. qn%is_set()
                if (qn%is_set() ) chars = chars//qn%get_string()//', '
            end associate
        end do
        associate ( qn => this%quantum_numbers(size(this%quantum_numbers)) )
            have_symmetry = have_symmetry .or. qn%is_set()
            if (qn%is_set() ) chars = chars//qn%get_string()
        end associate
        if (.not. have_symmetry) then
            chars = "no symmetry"
            return
        end if
    end function as_chars

    function as_infix(this) result(chars)
        class(quantum_number_collection), intent(in) :: this
        character(len=:), allocatable :: chars

        integer :: idx
        logical :: first

        chars = ''
        if (.not. allocated(this%quantum_numbers)) return
        if (size(this%quantum_numbers) == 0) return

        first = .true.
        do idx = 1, size(this%quantum_numbers)
            associate ( qn => this%quantum_numbers(idx) )
                if (qn%is_set()) then
                    if ( first ) then
                        chars = qn%as_infix()
                        first = .false.
                    else
                        chars = chars//"_"//qn%as_infix()
                    end if
                end if
            end associate
        end do
    end function as_infix

    logical function is_compatible(this, a, b)
        class(quantum_number_collection), intent(in) :: this, a, b

        integer :: idx

        is_compatible = .true.
        do idx = 1, size(this%quantum_numbers)
            associate( qn => this%quantum_numbers(idx) )
                is_compatible = is_compatible .and. &
                        qn%is_compatible(a%get_quantum_number(qn%get_descriptor()), &
                                b%get_quantum_number(qn%get_descriptor()) )
            end associate
        end do

    end function is_compatible

    subroutine cleanup(this)
        class(quantum_number_collection), intent(inout) :: this

        if (allocated(this%quantum_numbers)) deallocate(this%quantum_numbers)
        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(quantum_number_collection), intent(inout) :: this
    end subroutine clear
end module quantum_number_collection_module
