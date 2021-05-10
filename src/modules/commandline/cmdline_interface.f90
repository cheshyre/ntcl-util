module cmdline_interface
    use :: cmdline_parser_module, only : cmdline_parser
    use :: cmdline_arguments_module, only : cmdline_arguments
    use :: dictionary_module, only : dictionary

    implicit none
    private

    public :: get_key_value_pairs_from_command_line
contains
    type(dictionary) function get_key_value_pairs_from_command_line()
        type(cmdline_arguments) :: cmd
        type(cmdline_parser) :: parser

        cmd = cmdline_arguments()
        get_key_value_pairs_from_command_line = &
                parser%get_keywords(cmd%get_arguments_as_string())
    end function get_key_value_pairs_from_command_line
end module cmdline_interface
