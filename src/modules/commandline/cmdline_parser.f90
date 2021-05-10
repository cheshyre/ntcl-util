module cmdline_parser_module
    use :: dictionary_module, only : dictionary
    use :: string_module, only : string

    implicit none
    private

    public :: cmdline_parser

    type :: cmdline_parser
    contains
        procedure, nopass :: get_keywords => get_keywords
    end type cmdline_parser
contains
    type(dictionary) function get_keywords(cmdline)
        type(string), intent(in) :: cmdline

        integer :: number_of_words, idx
        type(string), dimension(:), allocatable :: words, keywords

        words = cmdline%split(" ")
        number_of_words = size(words)

        get_keywords = dictionary(number_of_words)
        do idx = 1, number_of_words
            if ( is_a_pair(words(idx)) ) then
                keywords = words(idx)%split("=")
                call get_keywords%set_value(keywords(1), keywords(2))
            end if
        end do
    end function get_keywords

    logical function is_a_pair(word)
        type(string), intent(in) :: word

        type(string), dimension(:), allocatable :: words

        words = word%split("=")
        is_a_pair = size(words) == 2
    end function is_a_pair
end module cmdline_parser_module
