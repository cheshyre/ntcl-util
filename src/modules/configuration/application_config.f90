module application_config_module
    use, intrinsic :: iso_fortran_env, only : error_unit

    use :: string_module, only : string
    use :: dictionary_module, only : dictionary
    use :: cmdline_arguments_module, only : cmdline_arguments
    use :: cmdline_parser_module, only : cmdline_parser
    use :: config_file_parser_module, only : config_file_parser
    use :: string_converter_module, only : string_converter

    implicit none
    private

    public :: application_config

    type :: application_config
        type(dictionary) :: config, machine, full_config
        type(string) :: output_filename
        logical :: has_runfile, dump_config
    contains
        procedure :: read_commandline => read_commandline
        procedure :: process_cmd_options => process_cmd_options
        procedure :: process_config => process_config
        procedure :: config_has_required_options => config_has_required_options
        procedure :: get_output_filename => get_output_filename
        procedure :: write_to_file => write_to_file
        procedure :: get_config => get_config
        procedure :: dump => dump
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type application_config

    interface application_config
        module procedure constructor
    end interface application_config

contains
    function constructor() result(this)
        type(application_config) :: this

        call this%clear()
    end function constructor

    subroutine read_commandline(this)
        class(application_config), intent(inout) :: this

        type(cmdline_arguments) :: cmd
        type(cmdline_parser) :: parser
        type(config_file_parser) :: config_parser
        type(dictionary) :: cmd_keywords

        cmd = cmdline_arguments()
        config_parser = config_file_parser()

        cmd_keywords = parser%get_keywords(cmd%get_arguments_as_string())
        if ( cmd_keywords%has_key("configfile")) then
            this%config = config_parser%parse(cmd_keywords%get_value("configfile"))
        else
            this%config = dictionary()
        end if

        if ( cmd_keywords%has_key("machinefile")) then
            this%machine = config_parser%parse(cmd_keywords%get_value("machinefile"))
        else
            this%machine = dictionary()
        end if

        call this%process_cmd_options(cmd_keywords)
        call this%process_config()
    end subroutine read_commandline

    subroutine process_cmd_options(this, cmd)
        class(application_config), intent(inout) :: this
        type(dictionary), intent(in) :: cmd

        type(string), dimension(:), allocatable :: keys
        integer :: idx
        type(string) :: val

        this%full_config = this%machine
        keys = this%config%get_keys()
        do idx = 1, size(keys)
            associate ( key => keys(idx) )
                val = this%config%get_value(key)
                call this%full_config%set_value(key, val)
            end associate
        end do

        keys = cmd%get_keys()
        do idx = 1, size(keys)
            associate ( key => keys(idx) )
                val = cmd%get_value(key)
                if ( this%machine%has_key(key) ) then
                    call this%machine%set_value(key, val)
                else if (this%config%has_key(key) .or. &
                        ( key /= string("configfile") .and. &
                        key /= string("machinefile") ) ) then
                    call this%config%set_value(key, val)
                end if
                call this%full_config%set_value(key, val)
            end associate
        end do
    end subroutine process_cmd_options

    subroutine process_config(this)
        class(application_config), intent(inout) :: this

        type(string_converter) :: converter
        type(string) :: dummy

        if ( this%config%has_key("runfile") ) then
            this%has_runfile = .true.
            this%output_filename =  this%config%get_value("runfile")
        end if

        if ( this%config%has_key("dump_config") ) &
                this%dump_config = converter%to_logical(this%config%get_value("dump_config"))
    end subroutine process_config

    type(dictionary) function get_config(this)
        class(application_config), intent(in) :: this

        get_config = this%full_config
    end function get_config

    logical function config_has_required_options(this, required, priorities)
        class(application_config), intent(in) :: this
        type(string), dimension(:) :: required
        type(string), dimension(:), optional :: priorities

        integer :: idx

        config_has_required_options = .true.

        do idx = 1, size(required)
            if ( .not. this%config%has_key(required(idx), priorities) ) then
                write(error_unit, *) "application_config::config_has_required_options:"// &
                        "Missing config option: "//required(idx)%char_array
                config_has_required_options = .false.
            end if
        end do
    end function config_has_required_options

    logical function write_to_file(this)
        class(application_config), intent(in) :: this

        write_to_file = this%has_runfile
    end function write_to_file

    type(string) function get_output_filename(this)
        class(application_config), intent(in) :: this

        if ( .not. this%has_runfile ) &
                error stop "application_config::get_output_filename:No filename in config."

        get_output_filename = this%output_filename
    end function get_output_filename

    subroutine dump(this, ounit)
        class(application_config), intent(in) :: this
        integer, intent(in) :: ounit

        if ( .not. this%dump_config ) return

        call this%config%write_to_unit(ounit)
        call this%machine%write_to_unit(ounit)
    end subroutine dump

    subroutine cleanup(this)
        class(application_config), intent(inout) :: this

        integer :: idx

        call this%output_filename%cleanup()
        call this%config%cleanup()
        call this%machine%cleanup()
        call this%full_config%cleanup()

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(application_config), intent(inout) :: this

        this%has_runfile = .false.
        this%dump_config = .false.
    end subroutine clear
end module application_config_module
