module messagehandler
    use, intrinsic :: iso_fortran_env, only: output_unit
    use kinds
    use flags

    implicit none
    private

    type, extends(flag), public :: msgflag
        character(len=15)   :: name
     contains
         procedure, pass(a) :: assign_flagflag => msgassign_flagflag
    end type msgflag

    type(msgflag), public, parameter        :: ErrorMessage   = msgflag(b'1',"ErrorMessage")
    type(msgflag), public, parameter        :: WarningMessage = msgflag(b'10',"WarningMessage")
    type(msgflag), public, parameter        :: DebugMessage   = msgflag(b'100',"DebugMessage")
    type(msgflag), public, parameter        :: InfoMessage    = msgflag(b'1000',"InfoMessage")

    interface message_report
        module procedure message_report_screen
        module procedure message_report_file
    end interface message_report

    interface itoa
        module procedure itoa_int16
        module procedure itoa_int32
        module procedure itoa_int64
    end interface itoa

    interface rtoa
        module procedure rtoa_real32
        module procedure rtoa_real64
    end interface rtoa

    public :: message_report, itoa, rtoa

contains

    subroutine msgassign_flagflag(a,b)
        class(msgflag), intent(out)         :: a
        class(flag), intent(in)             :: b
        a%value = b%value
        select type (b)
        type is (msgflag)
            a%name = b%name
        end select
    end subroutine

    subroutine message_report_screen(msgtype, message, filename, line)
        implicit none
        type(msgflag), intent(in)                :: msgtype
        character(len=*), intent(in)             :: message
        character(len=*), intent(in), optional   :: filename
        integer, intent(in), optional            :: line

        if (present(filename) .and. present(line)) then
            call message_report_file(output_unit, msgtype, message, filename, line)
        else
            call message_report_file(output_unit, msgtype, message)
        end if
        return
    end subroutine message_report_screen

    subroutine message_report_file(fid, msgtype, message, filename, line)
        implicit none
        integer, intent(in)                      :: fid
        type(msgflag), intent(in)                :: msgtype
        character(len=*), intent(in)             :: message
        character(len=*), intent(in), optional   :: filename
        integer, intent(in), optional            :: line

        character(len=:), allocatable            :: pos
        
        if (present(filename) .and. present(line)) then
            pos = '('//trim(filename) //':'// itoa(line)//')'
        end if

        if (msgtype == ErrorMessage) then
            write(fid,'(a, a,": ", a)'), achar(27)//'[31mERROR'//achar(27)//'[0m' , pos, message
        elseif(msgtype == WarningMessage) then
            write(fid,'(a, a,": ", a)'), char(27)//'[95mWarning'//achar(27)//'[0m', pos, message
        elseif(msgtype == DebugMessage) then
            write(fid,'(a, a,": ", a)'), 'Debug', pos, message
        elseif(msgtype == InfoMessage) then
            write(fid,'(a)') message
        else
            return
        end if

        return
    end subroutine message_report_file

    function itoa_int16(i) result(res)
        implicit none
        integer(kind=int16),intent(in)      :: i
        character(len=:),allocatable        :: res

        character(len=digits(i))            :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
        return
    end function

    function itoa_int32(i) result(res)
        implicit none
        integer(kind=int32),intent(in)      :: i
        character(len=:),allocatable        :: res

        character(len=digits(i))            :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
        return
    end function

    function itoa_int64(i) result(res)
        implicit none
        integer(kind=int64),intent(in)      :: i
        character(len=:),allocatable        :: res

        character(len=digits(i))            :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
        return
    end function

    function rtoa_real32(x) result(res)
        implicit none
        real(kind=real32),intent(in)        :: x
        character(len=:),allocatable        :: res
        
        character(len=12)                   :: tmp

        write(tmp, '(ES12.5E2)') x
        res = trim(adjustl(tmp))
        return
    end function

    function rtoa_real64(x) result(res)
        implicit none
        real(kind=real64),intent(in)        :: x
        character(len=:),allocatable        :: res

        character(len=12)                   :: tmp

        write(tmp, '(ES12.5E2)') x
        res = trim(adjustl(tmp))
        return
    end function

end module messagehandler
