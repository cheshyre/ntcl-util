module config_file_parser_module
    use, intrinsic :: iso_fortran_env, only : error_unit
    use :: string_module, only : string
    use :: dictionary_module, only : dictionary

    implicit none
    private

    public :: config_file_parser

    type :: config_file_parser
        type(string), dimension(:), allocatable :: comment_strings
        character(1) :: separation_character
    contains
        generic :: parse => parse_from_filename_as_string, &
                parse_from_filename_as_characters
        procedure :: read_lines => read_lines
        procedure :: strip_comments => strip_comments
        procedure :: create_dictionary => create_dictionary
        procedure :: count_number_of_lines => count_number_of_lines
        procedure :: parse_from_filename_as_string => parse_from_filename_as_string
        procedure :: parse_from_filename_as_characters => parse_from_filename_as_characters
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type config_file_parser

    interface config_file_parser
        module procedure constructor
    end interface config_file_parser

    character(len=1), dimension(2), parameter :: default_comment_characters = ['#', "!"]
    character(len=1), parameter :: default_separation_character = "="
    integer, parameter :: max_line_length = 1024
contains
    function constructor(comment_string, separation_character) result(this)
        type(string), intent(in), optional :: comment_string
        character(len=1), intent(in), optional :: separation_character
        type(config_file_parser) :: this

        integer :: i

        call this%clear()
        if (present(comment_string)) then
            allocate(this%comment_strings(size(default_comment_characters) + 1))
            do i = 1, size(default_comment_characters)
                this%comment_strings(i+1) = default_comment_characters(i)
            end do
            this%comment_strings(1) = comment_string
        else
            allocate(this%comment_strings(size(default_comment_characters)))
            do i = 1, size(default_comment_characters)
                this%comment_strings(i) = default_comment_characters(i)
            end do
        end if
        if ( present(separation_character) ) this%separation_character = separation_character
    end function constructor

    function parse_from_filename_as_string(this, filename) result(config)
        class(config_file_parser), intent(in) :: this
        type(string), intent(in) :: filename
        type(dictionary) :: config

        type(string), dimension(:), allocatable :: lines
        integer :: lun, idx
        logical :: exists

        inquire(file=filename%char_array, exist=exists)
        if ( exists ) then
            open(newunit=lun, file=filename%char_array, status='old')
            lines = this%read_lines(lun)
            close(lun)
        else
            lines = [(string(""), idx = 1, 0)]
        end if

        lines = this%strip_comments(lines)
        config = this%create_dictionary(lines)
    end function parse_from_filename_as_string

    function parse_from_filename_as_characters(this, filename) result(config)
        class(config_file_parser), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(dictionary) :: config

        config = this%parse(string(filename))
    end function parse_from_filename_as_characters

    function read_lines(this, lun) result(lines)
        class(config_file_parser), intent(in) :: this
        integer, intent(in) :: lun
        type(string), dimension(:), allocatable :: lines

        integer ::  iostatus, idx, number_of_lines
        character(len=max_line_length) :: line

        number_of_lines = this%count_number_of_lines(lun)
        allocate(lines(number_of_lines))
        do idx = 1, number_of_lines
            read(lun, '(a)', iostat=iostatus) line
            if ( iostatus /= 0 ) then
                write(error_unit, *) "config_file_parser::read_lines:Error in reading file."
                stop
            end if
            lines(idx) = string(trim(adjustl(line)))
        end do
    end function read_lines

    function strip_comments(this, inlines) result(lines)
        class(config_file_parser), intent(in) :: this
        type(string), dimension(:), intent(in) :: inlines
        type(string), dimension(:), allocatable :: lines

        integer :: idx, last, length_of_line, comment, start_of_comment

        lines = inlines
        do idx = 1, size(lines)
            last = len(lines(idx)%char_array)
            length_of_line = last
            do comment = 1, size(this%comment_strings)
                start_of_comment = index(inlines(idx)%char_array, this%comment_strings(comment)%char_array)
                if (start_of_comment > 0) last = min(last, start_of_comment - 1)
            end do
            if ( last <= 0 ) then
                lines(idx) = string("")
            else if ( last < length_of_line ) then
                lines(idx) = string(lines(idx)%char_array(1:last), strip=.true.)
            end if
        end do
        lines = pack(lines, [(.not. lines(idx) == "", idx = 1, size(lines))])
    end function strip_comments

    function create_dictionary(this, lines) result(dict)
        class(config_file_parser), intent(in) :: this
        type(string), dimension(:), intent(in) :: lines
        type(dictionary) :: dict

        integer :: idx
        type(string), dimension(:), allocatable :: words

        dict = dictionary()
        do idx = 1, size(lines)
            words = lines(idx)%split(this%separation_character)
            if (size(words) /= 2) then
                write(error_unit, *) "config_file_parser::create_dictionary:Not a valid config pair: ", &
                        lines(idx)%char_array
                stop
            end if
            call dict%set_value(words(1)%strip(), words(2)%strip())
        end do
    end function create_dictionary
        
    integer function count_number_of_lines(this, id)
        class(config_file_parser), intent(in) :: this
        integer, intent(in) :: id

        integer :: iostatus, counter

        iostatus = 0; counter = 0
        do while (.true.)
            read(id, *, iostat=iostatus)
            if ( iostatus == 0 ) then
                counter = counter + 1
            else if ( is_iostat_end(iostatus) ) then
                exit
            else
                write(error_unit, *) "config_file_parser::count_number_of_lines::Unexpected error."
                stop
            end if
        end do
        count_number_of_lines = counter
        rewind(id)
    end function count_number_of_lines

    subroutine cleanup(this)
        class(config_file_parser), intent(inout) :: this

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(config_file_parser), intent(inout) :: this
        
        this%separation_character = default_separation_character
    end subroutine clear
end module config_file_parser_module
