module property_with_name_module
    use :: property_module, only : property
    use :: string_module, only : string

    implicit none
    private

    public :: property_with_name

    type, extends(property) :: property_with_name
        type(string), private :: pname
    contains
        procedure :: is_string => is_string
        procedure :: cleanup => cleanup
    end type property_with_name

    interface property_with_name
        module procedure constructor
        module procedure constructor_from_chars
    end interface property_with_name

contains
    function constructor(pname) result(this)
        type(string), intent(in) :: pname
        type(property_with_name) :: this

        this%pname = pname%to_lower()
    end function constructor

    function constructor_from_chars(pname) result(this)
        character(len=*), intent(in) :: pname
        type(property_with_name) :: this

        this = property_with_name(string(pname))
    end function constructor_from_chars

    logical function is_string(this, astring)
        class(property_with_name), intent(in) :: this
        type(string), intent(in) :: astring

        is_string = this%pname == astring%to_lower()
    end function is_string

    subroutine cleanup(this)
        class(property_with_name), intent(inout) :: this

        call this%pname%cleanup()
    end subroutine cleanup
end module property_with_name_module
