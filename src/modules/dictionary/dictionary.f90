module dictionary_module
    use, intrinsic :: iso_fortran_env, only : error_unit
    use :: string_module, only : string
    implicit none
    private

    public :: dictionary

    integer, parameter :: default_max_length = 100, theoretical_max_length = 1000000

    type :: dictionary
        integer :: max_length, allocated_length, next_available_position, number_of_pairs
        type(string), dimension(:), allocatable :: keys, values
    contains
        generic :: operator (.eq.) => is_equal
        generic :: operator (.ne.) => is_not_equal
        generic :: has_key => &
                has_key_chars, &
                has_key_bare

        procedure :: is_not_equal => is_not_equal
        procedure :: is_equal => is_equal
        procedure :: has_key_bare => has_key_bare
        procedure :: has_key_chars => has_key_chars
        procedure :: get_value_from_string_key => get_value_from_string_key
        procedure :: get_value_from_character_key => get_value_from_character_key
        generic :: get_value => &
                get_value_from_string_key, &
                get_value_from_character_key
        procedure :: get_value_as_chars_from_string_key => get_value_as_chars_from_string_key
        procedure :: get_value_as_chars_from_character_key => get_value_as_chars_from_character_key
        generic :: get_value_as_chars => &
                get_value_as_chars_from_string_key, &
                get_value_as_chars_from_character_key
        generic :: get_value_from_prioritized_prefixes => &
            get_value_from_prioritized_prefixes_char, &
            get_value_from_prioritized_prefixes_string
        procedure :: has_key_from_prioritized_prefixes => has_key_from_prioritized_prefixes
        procedure :: get_value_from_prioritized_prefixes_char => get_value_from_prioritized_prefixes_char
        procedure :: get_value_from_prioritized_prefixes_string => get_value_from_prioritized_prefixes_string
        procedure :: get_size => get_size
        generic :: set_value => &
                set_value_from_strings, &
                set_value_from_chars, &
                set_value_from_chars_and_string
        procedure :: set_value_from_strings => set_value_from_strings
        procedure :: set_value_from_chars => set_value_from_chars
        procedure :: set_value_from_chars_and_string => set_value_from_chars_and_string
        procedure :: write_to_unit => write_to_unit
        procedure :: allocate_internal_structures => allocate_internal_structures
        procedure :: find_index_from_key => find_index_from_key
        procedure :: get_keys => get_keys
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type dictionary

    interface dictionary
        module procedure constructor
    end interface dictionary

contains
    function constructor(max_length) result(this)
        integer, intent(in), optional :: max_length
        type(dictionary) :: this

        call this%clear()
        call this%allocate_internal_structures()
    end function constructor

    logical function is_equal(this, other)
        class(dictionary), intent(in) :: this, other

        integer :: idx

        is_equal = this%next_available_position == other%next_available_position .and. &
                ( allocated(this%keys) .eqv. allocated(other%keys)) .and. &
                ( allocated(this%values) .eqv. allocated(other%values))
        if ( is_equal .and. allocated(this%keys) .and. allocated(this%values) ) &
                is_equal = &
                all([(this%keys(idx) == other%keys(idx), idx = 1, this%next_available_position -1)]) .and. &
                all([(this%values(idx) == other%values(idx), idx = 1, this%next_available_position -1)])
    end function is_equal

    logical function is_not_equal(this, other)
        class(dictionary), intent(in) :: this, other

        is_not_equal = .not. this == other
    end function is_not_equal

    logical function has_key_bare(this, key, prefixes)
        class(dictionary), intent(in) :: this
        type(string), intent(in) :: key
        type(string), dimension(:), intent(in), optional :: prefixes

        if ( present(prefixes) ) then
            has_key_bare = this%has_key_from_prioritized_prefixes(key, prefixes)
        else
            has_key_bare = this%find_index_from_key(key) > 0
        end if
    end function has_key_bare

    logical function has_key_chars(this, key, prefixes)
        class(dictionary), intent(in) :: this
        character(len=*), intent(in) :: key
        type(string), dimension(:), intent(in), optional :: prefixes

        has_key_chars = this%has_key(string(key), prefixes)
    end function has_key_chars

    logical function has_key_from_prioritized_prefixes(this, key, prefixes)
        class(dictionary), intent(in) :: this
        type(string), intent(in) :: key
        type(string), dimension(:), intent(in) :: prefixes

        integer :: idx
        type(string) :: longkey

        has_key_from_prioritized_prefixes = .true.
        do idx = 1, size(prefixes)
            longkey = prefixes(idx)%cat(key)
            if (this%has_key(longkey)) return
        end do
        has_key_from_prioritized_prefixes = this%has_key(key)
    end function has_key_from_prioritized_prefixes

    subroutine write_to_unit(this, lun)
        class(dictionary), intent(in) :: this
        integer, intent(in) :: lun

        integer :: idx

        write(lun, *) "Number of dictionary items: ", this%number_of_pairs
        do idx = 1, this%number_of_pairs
            write(lun, *) this%keys(idx)%char_array//" = "//this%values(idx)%char_array
        end do
    end subroutine write_to_unit

    function get_value_from_string_key(this, key, prefixes) result(val)
        class(dictionary), intent(in) :: this
        type(string), intent(in) :: key
        type(string), dimension(:), intent(in), optional :: prefixes
        type(string) :: val

        integer :: idx

        if ( present(prefixes) ) then
            val = this%get_value_from_prioritized_prefixes(prefixes, key)
            return
        end if

        val = string('')

        idx = this%find_index_from_key(key)
        if ( idx < 1) then
            write(error_unit, *) "Can't find key: "//key%char_array
            return
        end if
        val = this%values(idx)
    end function get_value_from_string_key

    function get_value_from_character_key(this, key, prefixes) result(val)
        class(dictionary), intent(in) :: this
        character(len=*), intent(in) :: key
        type(string), dimension(:), intent(in), optional :: prefixes
        type(string) :: val

        type(string) :: skey

        skey = string(key)
        val = this%get_value(skey, prefixes)
    end function get_value_from_character_key

    function get_value_as_chars_from_string_key(this, key) result(val)
        class(dictionary), intent(in) :: this
        type(string), intent(in) :: key
        character(len=:), allocatable :: val

        integer :: idx

        val = ''

        idx = this%find_index_from_key(key)
        if ( idx < 1) then
            write(error_unit, *) "Can't find key: "//key%char_array
            return
        end if
        val = this%values(idx)%char_array
    end function get_value_as_chars_from_string_key

    function get_value_as_chars_from_character_key(this, key) result(val)
        class(dictionary), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: val

        type(string) :: skey

        skey = string(key)
        val = this%get_value_as_chars(skey)
    end function get_value_as_chars_from_character_key

    function get_value_from_prioritized_prefixes_char(this, prefixes, key) result(val)
        class(dictionary), intent(in) :: this
        type(string), dimension(:), intent(in) :: prefixes
        character(len=*), intent(in) :: key
        type(string) :: val

        type(string) :: skey

        skey = string(key)
        val = this%get_value_from_prioritized_prefixes(prefixes, skey)
    end function get_value_from_prioritized_prefixes_char

    function get_value_from_prioritized_prefixes_string(this, prefixes, key) result(val)
        class(dictionary), intent(in) :: this
        type(string), dimension(:), intent(in) :: prefixes
        type(string), intent(in) :: key
        type(string) :: val

        integer :: idx
        type(string) :: longkey

        do idx = 1, size(prefixes)
            longkey = prefixes(idx)%cat(key)
            if (this%has_key(longkey)) then
                val = this%get_value(longkey)
                return
            end if
        end do
        val = this%get_value(key)
    end function get_value_from_prioritized_prefixes_string

    subroutine set_value_from_strings(this, key, val)
        class(dictionary), intent(inout) :: this
        type(string), intent(in) :: key, val

        integer :: idx

        idx = this%find_index_from_key(key)
        if ( idx < 1 ) then
            if ( this%next_available_position < this%allocated_length ) then
                idx = this%next_available_position
                this%next_available_position = this%next_available_position + 1
                this%number_of_pairs = this%number_of_pairs + 1
            else
                write(error_unit, *) "dictionary::set_value:Out of bounds"
                stop
            end if
        end if
        this%keys(idx) = key
        this%values(idx) = val
    end subroutine set_value_from_strings

    subroutine set_value_from_chars(this, key, val)
        class(dictionary), intent(inout) :: this
        character(len=*), intent(in) :: key, val

        call this%set_value(string(key), string(val))
    end subroutine set_value_from_chars

    subroutine set_value_from_chars_and_string(this, key, val)
        class(dictionary), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(string), intent(in) :: val

        call this%set_value(string(key), val)
    end subroutine set_value_from_chars_and_string

    integer function get_size(this)
        class(dictionary), intent(in) :: this

        get_size = this%number_of_pairs
    end function get_size

    integer function find_index_from_key(this, key)
        class(dictionary), intent(in) :: this
        type(string), intent(in) :: key

        integer :: idx

        find_index_from_key = -1
        do idx = 1, this%next_available_position - 1
            if ( this%keys(idx) == key ) then
                find_index_from_key = idx
                exit
            end if
        end do
    end function find_index_from_key

    subroutine allocate_internal_structures(this)
        class(dictionary), intent(inout) :: this

        if (allocated(this%keys)) deallocate(this%keys)
        if (allocated(this%values)) deallocate(this%values)

        if ( this%max_length < 1 .or. this%max_length > theoretical_max_length ) then
            write(error_unit, *) "dictionary::allocate_internal_structures::Illegal size: ", this%max_length
            stop
        end if

        allocate(this%keys(this%max_length))
        allocate(this%values(this%max_length))

        this%allocated_length = this%max_length
        this%next_available_position = 1
    end subroutine allocate_internal_structures

    function get_keys(this) result(keys)
        class(dictionary), intent(in) :: this
        type(string), dimension(:), allocatable :: keys

        keys = this%keys(1:this%number_of_pairs)
    end function get_keys

    subroutine cleanup(this)
        class(dictionary), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(dictionary), intent(inout) :: this

        this%max_length = default_max_length
        this%allocated_length = 0
        this%next_available_position = 0
        this%number_of_pairs = 0
    end subroutine clear
end module dictionary_module
