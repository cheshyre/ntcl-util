module string_module
    implicit none
    private

    public :: string
    public :: split_string_word

    type :: string
        character(len=:), allocatable :: char_array
        integer :: length
    contains
        generic, public :: init => init_char_array, init_string
        generic, public :: assignment (=) => assign_char_array
        generic, public :: operator (.eq.) => equality_string, equality_char_array
        generic, public :: operator (.ne.) => not_equal_string, not_equal_char_array
        generic, public :: cat => cat_char_array, cat_string

        procedure, public :: cleanup => cleanup
        procedure, public :: as_char_array => as_char_array
        procedure, public :: split => split
        procedure, public :: split_and_strip => split_and_strip
        procedure, public :: find_substrings => find_substrings
        procedure, public :: find_substrings_inclusive => &
                find_substrings_inclusive
        procedure, public :: cat_string
        procedure, public :: cat_char_array
        procedure, public :: delete => delete
        procedure, public :: get => get
        procedure, public :: unique_chars => unique_chars
        procedure, public :: strip => strip
        procedure, public :: char_count => char_count
        procedure, public :: char_remove => char_remove
        procedure, public :: is_empty => is_empty
        procedure, public :: to_lower => to_lower
        procedure, public :: to_upper => to_upper

        ! private routines that are called through generic interface
        procedure :: init_char_array
        procedure :: init_string
        procedure :: assign_string
        procedure :: assign_char_array
        procedure :: equality_string
        procedure :: equality_char_array
        procedure :: not_equal_string
        procedure :: not_equal_char_array
    end type string

    integer, parameter :: upper_lower_difference = ichar('A') - ichar('a')

    interface string
        module procedure string_factory
    end interface string
contains
    pure function string_factory(char_array, strip) result(astring)
        character(len=*), intent(in) :: char_array
        logical, intent(in), optional :: strip
        type(string) :: astring

        logical :: strip_input

        strip_input = .false.
        if (present(strip)) strip_input = strip
        if (strip_input) then
            astring = trim(adjustl(char_array))
        else
            astring = char_array
        end if
    end function string_factory

    pure subroutine init_char_array(this, char_array)
        class(string), intent(inout) :: this
        character(len=*), intent(in) :: char_array

        integer :: length

        call this%cleanup()

        length = len(char_array)
        this%char_array = char_array
        this%length = length
    end subroutine init_char_array

    logical function is_empty(this)
        class(string), intent(in) :: this

        is_empty = this%length == 0
    end function is_empty

    function as_char_array(this) result(char_array)
        class(string), intent(in) :: this
        character(len=:), allocatable :: char_array

        char_array = this%char_array
    end function as_char_array


    pure subroutine init_string(this, str)
        class(string), intent(inout) :: this
        type(string), intent(in) :: str

        if (allocated(str%char_array)) then
            call init_char_array(this, str%char_array)
        else
            call this%cleanup()
        endif
    end subroutine init_string

    type(string) function to_lower(this)
        class(string), intent(in) :: this

        integer :: idx
        ! workaround for gfortran bug (br 63494)
        character(len=:), allocatable :: text
        character :: ch

        text = this%char_array
        do idx = 1, len(text)
            ch = text(idx:idx)
            if ( ch >= 'A' .and. ch <= 'Z' ) &
                ch = char(ichar(ch) - upper_lower_difference)
            text(idx:idx) = ch
        end do
        to_lower = text
    end function to_lower

    type(string) function to_upper(this)
        class(string), intent(in) :: this

        integer :: idx
        ! workaround for gfortran bug (br 63494)
        character(len=:), allocatable :: text
        character :: ch

        text = this%char_array
        do idx = 1, len(text)
            ch = text(idx:idx)
            if ( ch >= 'a' .and. ch <= 'z' ) &
                ch = char(ichar(ch) + upper_lower_difference)
            text(idx:idx) = ch
        end do
        to_upper = text
    end function to_upper

    function strip(this) result(stripped)
        class(string), intent(in) :: this
        type(string) :: stripped

        stripped = trim(adjustl(this%char_array))
    end function strip

    function cat_char_array(this, other) result(res)
        class(string) :: this
        character(len=*), intent(in) :: other
        type(string) :: res

        if (allocated(this%char_array)) then
            res = this%char_array//other
        else
            res = other
        endif
    end function cat_char_array

    function cat_string(this, other) result(res)
        class(string) :: this
        type(string), intent(in) :: other
        type(string) :: res

        if (allocated(this%char_array) .and. allocated(other%char_array)) then
            res = this%char_array//other%char_array
        else if (allocated(this%char_array)) then
            res = this%char_array
        else if (allocated(other%char_array)) then
            res = other%char_array
        else
            call res%cleanup()
        endif
    end function cat_string

    pure subroutine assign_string(this, other)
        class(string), intent(inout) :: this
        type(string), intent(in) :: other

        call this%init_string(other)
    end subroutine assign_string

    pure elemental subroutine assign_char_array(this, other)
        class(string), intent(inout) :: this
        character(len=*), intent(in) :: other

        call this%init_char_array(other)
    end subroutine assign_char_array

    pure elemental function equality_string(this, other) result(res)
        class(string), intent(in) :: this
        type(string), intent(in) :: other
        logical :: res

        res = .true.
        if (allocated(this%char_array) .and. allocated(other%char_array)) then
            if (this%char_array /= other%char_array) res = .false.
        else
            if (allocated(this%char_array) .or. allocated(other%char_array)) res = .false.
        endif
    end function equality_string

    pure elemental function not_equal_string(this, other) result(res)
        class(string), intent(in) :: this
        type(string), intent(in) :: other
        logical :: res

        res = .not. this == other
    end function not_equal_string

    pure function equality_char_array(this, other) result(res)
        class(string), intent(in) :: this
        character(len=*), intent(in) :: other
        logical :: res

        res = .true.
        ! return true with or without leading spaces
        if (allocated(this%char_array) ) then
            if (this%char_array /= adjustl(trim(other))) res = .false.
            if (this%char_array == trim(other)) res = .true.
        else
            if (len(trim(adjustl(other))) /= 0) res = .false.
        endif
    end function equality_char_array

    pure function not_equal_char_array(this, other) result(res)
        class(string), intent(in) :: this
        character(len=*), intent(in) :: other
        logical :: res

        res = .not. this == other
    end function not_equal_char_array

    pure subroutine cleanup(this)
        class(string), intent(inout) :: this

        if (allocated(this%char_array)) deallocate(this%char_array)
        this%length = 0
    end subroutine cleanup

    function split(this, delimiter) result(word)
        class(string), intent(in) :: this
        character, intent(in) :: delimiter
        type(string), dimension(:), allocatable :: word
        integer :: pos1, pos2, n, max_length
        character(len=:), allocatable :: text

        max_length = 0
        pos1 = 1
        n = 0

        if (.not. allocated(this%char_array)) then
            allocate(word(0))
            return
        end if

        ! workaround for gfortran bug (br 63494)
        text = this%char_array

        ! count and determine maximum length
        do
            pos2 = index(text(pos1:), delimiter)
            if (pos2 == 0) then
                pos2 = len(text) + 1
                n = n + 1
                if (max_length < pos2-pos1) max_length = pos2-pos1
                exit
            end if
            n = n + 1
            if (max_length < pos2-pos1) max_length = pos2-pos1
            pos1 = pos2+pos1
        end do

        allocate(word(n))

        pos1 = 1
        n = 0
        do
            pos2 = index(text(pos1:), delimiter)
            if (pos2 == 0) then
                n = n + 1
                word(n) = text(pos1:)
                exit
            end if
            n = n + 1
            word(n) = text(pos1:pos1+pos2-2)
            pos1 = pos2+pos1
        end do
    end function split

    function find_substrings(this, delimiter1, delimiter2) result(words)
        class(string), intent(in) :: this
        character, intent(in) :: delimiter1, delimiter2
        type(string), dimension(:), allocatable :: words
        integer :: pos1, pos2, pos3, n, max_length
        character(len=:), allocatable :: text, text2

        max_length = 0
        pos1 = 1
        pos2 = 0
        pos3 = 0
        n = 0

        if (.not. allocated(this%char_array)) then
            allocate(words(0))
            return
        end if

        ! workaround for gfortran bug (br 63494)
        text = this%char_array
        text2 = this%char_array

        ! count and determine maximum length
        do
            pos2 = index(text(pos1:), delimiter1)
            pos3 = index(text(pos1:), delimiter2)
            ! no instance of delimiter1 or delimiter2
            if (pos2 == 0 .or. pos3 == 0) then
                exit
            end if

            n = n + 1
            pos1 = pos3+pos1
        end do

        allocate(words(n))

        pos1 = 1
        pos2 = 0
        pos3 = 0
        n = 0
        do
            pos2 = index(text(pos1:), delimiter1)
            pos3 = index(text(pos1:), delimiter2)
            if (pos2 == 0 .or. pos3 == 0) then
                exit
            end if
            n = n + 1
            words(n) = text(pos1+pos2:pos1+pos3-2)
            pos1 = pos3+pos1
        end do
    end function find_substrings

    function find_substrings_inclusive(this, delimiter1, delimiter2) result(words)
        class(string), intent(in) :: this
        character, intent(in) :: delimiter1, delimiter2
        type(string), dimension(:), allocatable :: words
        integer :: pos1, pos2, pos3, n, max_length
        character(len=:), allocatable :: text

        max_length = 0
        pos1 = 1
        pos2 = 0
        pos3 = 0
        n = 0

        if (.not. allocated(this%char_array)) then
            allocate(words(0))
            return
        end if

        ! workaround for gfortran bug (br 63494)
        text = this%char_array

        ! count and determine maximum length
        do
            pos2 = index(text(pos1:), delimiter1)
            pos3 = index(text(pos1:), delimiter2)
            ! no instance of delimiter1 or delimiter2
            if (pos2 == 0 .or. pos3 == 0) then
                exit
            end if

            n = n + 1
            pos1 = pos3+pos1
        end do

        allocate(words(n))

        pos1 = 1
        pos2 = 0
        pos3 = 0
        n = 0
        do
            pos2 = index(text(pos1:), delimiter1)
            pos3 = index(text(pos1:), delimiter2)
            if (pos2 == 0 .or. pos3 == 0) then
                exit
            end if
            n = n + 1
            words(n) = text(pos1+pos2-1:pos1+pos3-1)
            pos1 = pos3+pos1
        end do
    end function find_substrings_inclusive

    function char_count(this, letter) result(num)
        class(string), intent(in) :: this
        character, intent(in) :: letter
        integer :: num
        integer :: pos1, pos2
        character(len=:), allocatable :: text

        num = 0
        pos1 = 1
        pos2 = 0
        if (.not. allocated(this%char_array)) then
            return
        end if

        ! workaround for gfortran bug (br 63494)
        text = this%char_array

        do
            pos2 = index(text(pos1:), letter)
            ! no instance of delimiter1 or delimiter2
            if (pos2 == 0) then
                exit
            end if

            num = num + 1
            pos1 = pos2+pos1
        end do
    end function char_count

    function char_remove(this, letter) result(word)
        class(string), intent(in) :: this
        character, intent(in) :: letter
        type(string) :: word
        character(len=:), allocatable :: text
        integer :: idx

        if (.not. allocated(this%char_array)) then
            return
        end if

        ! workaround for gfortran bug (br 63494)
        text = this%char_array

        do
            idx = index(text, letter)
            ! no instance of delimiter1 or delimiter2
            if (idx == 0) then
                exit
            end if

            text = text(:idx-1)//text(idx+1:)
        end do
        word = text(:)
    end function char_remove

    function unique_chars(this) result(word)
        class(string), intent(in) :: this
        type(string) :: word
        integer :: i, idx

        if (.not. allocated(this%char_array)) then
            return
        end if

        word = string("")

        do i = 1, this%length
            idx = index(word%as_char_array(), this%get(i))
            if (idx == 0) then
                word = word%cat( this%get(i) )
            end if
        end do
    end function unique_chars

    ! what behavior should idx > size have?
    function delete(this, idx) result(word)
        class(string), intent(in) :: this
        integer, intent(in) :: idx
        type(string) :: word
        character(len=:), allocatable :: text

        if (.not. allocated(this%char_array)) then
            return
        end if

        text = this%as_char_array()
        if ( idx > this%length ) then
            word = text(:)
            return
        end if
        word = text(:idx-1)//text(idx+1:)
    end function delete

    function get(this, idx) result(word)
        class(string), intent(in) :: this
        integer, intent(in) :: idx
        character :: word
        character(len=:), allocatable :: text

        if (.not. allocated(this%char_array)) then
            return
        end if

        text = this%as_char_array()
        if ( idx > this%length ) then
            error stop "string::get:: idx > length"
        end if
        word = text(idx:idx)
    end function get

    function split_and_strip(this, delimiter) result(words)
        class(string), intent(in) :: this
        character, intent(in) :: delimiter
        type(string), dimension(:), allocatable :: words

        integer :: idx

        words = this%split(delimiter)
        do idx = 1, size(words)
            words(idx) = words(idx)%strip()
        end do
    end function split_and_strip

    subroutine split_string_word(str, delimiter, word)
        character(len=*) :: str
        character :: delimiter
        character(len=100), allocatable, intent(inout) :: word(:)
        integer :: pos1, pos2, n, max_length

        max_length = 0
        pos1 = 1
        n = 0

        ! count and determine maximum length
        do
            pos2 = index(str(pos1:), delimiter)
            if (pos2 == 0) then
                pos2 = len(str) + 1
                n = n + 1
                if (max_length < pos2-pos1) max_length = pos2-pos1
                exit
            end if
            n = n + 1
            if (max_length < pos2-pos1) max_length = pos2-pos1
            pos1 = pos2+pos1
        end do

        allocate(word(n))

        pos1 = 1
        n = 0
        do
            pos2 = index(str(pos1:), delimiter)
            if (pos2 == 0) then
                n = n + 1
                word(n) = str(pos1:)
                exit
            end if
            n = n + 1
            word(n) = str(pos1:pos1+pos2-2)
            pos1 = pos2+pos1
        end do
    end subroutine split_string_word
end module string_module
