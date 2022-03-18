module flags

    implicit none
    private

    type, public :: flag
        integer            :: value 
    contains
        private
        procedure, pass(a) :: assign_flagflag
        procedure, pass(a) :: assign_flagint
        procedure, pass(a) :: assign_intflag
        procedure, pass(a) :: and_flagflag
        procedure, pass(a) :: and_flagint
        procedure, pass(a) :: and_intflag
        procedure, pass(a) :: or_flagflag
        procedure, pass(a) :: or_flagint
        procedure, pass(a) :: or_intflag
        procedure, pass(a) :: equal_flagflag
        procedure, pass(a) :: equal_flagint
        procedure, pass(a) :: equal_intflag
        procedure, pass(a) :: nequal_flagflag
        procedure, pass(a) :: nequal_flagint
        procedure, pass(a) :: nequal_intflag
        procedure, pass(a) :: testFlag_flag
        procedure, pass(a) :: testFlag_int
        generic, public    :: assignment(=) => assign_flagflag, assign_intflag, assign_flagint
        generic, public    :: operator(.and.) => and_flagflag,and_flagint, and_intflag
        generic, public    :: operator(.or.) => or_flagflag, or_flagint, or_intflag
        generic, public    :: operator(==) => equal_flagflag, equal_flagint, equal_intflag
        generic, public    :: operator(/=) => nequal_flagflag, nequal_flagint, nequal_intflag
        generic, public    :: testFlag => testFlag_flag, testFlag_int
    end type


contains
    subroutine assign_flagflag(a,b)
        class(flag), intent(out)         :: a
        class(flag), intent(in)          :: b
        a%value = b%value
    end subroutine

    subroutine assign_flagint(a,b)
        class(flag), intent(out)         :: a
        integer, intent(in)              :: b
        a%value = b
    end subroutine

    subroutine assign_intflag(b,a)
        integer, intent(out)             :: b
        class(flag), intent(in)          :: a
        b =  a%value
    end subroutine
   
    type(flag) function and_flagflag(a,b) result(and)
        class(flag), intent(in)          :: a
        class(flag), intent(in)          :: b
        and%value = iand(a%value,b%value)
    end function

    type(flag) function and_flagint(a,b) result(and)
        class(flag), intent(in)          :: a
        integer, intent(in)              :: b
        and%value = iand(a%value,b)
    end function

    type(flag) function and_intflag(b,a) result(and)
        integer, intent(in)              :: b
        class(flag), intent(in)          :: a
        and%value = iand(a%value,b)
    end function

    type(flag) function or_flagflag(a,b) result(or)
        class(flag), intent(in)          :: a
        class(flag), intent(in)          :: b
        or%value = ior(a%value,b%value)
    end function

    type(flag) function or_flagint(a,b) result(or)
        class(flag), intent(in)          :: a
        integer, intent(in)              :: b
        or%value = ior(a%value,b)
    end function

    type(flag) function or_intflag(b,a) result(or)
        integer, intent(in)              :: b
        class(flag), intent(in)          :: a
        or%value = ior(a%value,b)
    end function

    logical function equal_flagflag(a,b) result(equal)
        class(flag), intent(in)          :: a
        class(flag), intent(in)          :: b
        equal = (a%value == b%value)
    end function

    logical function equal_flagint(a,b) result(equal)
        class(flag), intent(in)          :: a
        integer, intent(in)              :: b
        equal = (a%value == b)
    end function

    logical function equal_intflag(b,a) result(equal)
        integer, intent(in)              :: b
        class(flag), intent(in)          :: a
        equal = (b == a%value)
    end function

    logical function nequal_flagflag(a,b) result(nequal)
        class(flag), intent(in)          :: a
        class(flag), intent(in)          :: b
        nequal = (.not. a == b)
    end function

    logical function nequal_flagint(a,b) result(nequal)
        class(flag), intent(in)          :: a
        integer, intent(in)              :: b
        nequal = (.not. a == b)
    end function

    logical function nequal_intflag(b,a) result(nequal)
        integer, intent(in)              :: b
        class(flag), intent(in)          :: a
        nequal = (.not. a == b)
    end function

    logical function testFlag_flag(a,b)
        class(flag), intent(in)          :: a
        class(flag), intent(in)          :: b
        if ((a .and. b) == b .and. (b%value /=0 .or. a == b)) then
            testFlag_flag = .true.
        else
            testFlag_flag = .false.
        end if
    end function

    logical function testFlag_int(a,b)
        class(flag), intent(in)          :: a
        integer, intent(in)              :: b
        if (iand(a%value,b) == b .and. (b /= 0 .or. a%value == b)) then
            testFlag_int = .true.
        else
            testFlag_int = .false.
        end if
    end function
end module flags
