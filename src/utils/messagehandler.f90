!
! This file is part of the ode_solver library distribution (https://github.com/feodorp/ode_solver)
! Copyright (C) 2022 Feodor Pisnitchenko
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
module messagehandler
    use, intrinsic :: iso_fortran_env, only: output_unit
    use kinds

    implicit none
    private

    type :: mtype
        integer             :: value
        character(len=16)   :: header
    contains
        procedure, pass(a)  :: eq_mtype
        generic, public     :: operator(==) => eq_mtype
    end type

    type(mtype), public, parameter :: ErrorMessage   = mtype(int(b'1'),achar(27)//'[31mERROR'//achar(27)//'[0m')
    type(mtype), public, parameter :: WarningMessage = mtype(int(b'10'),achar(27)//'[95mWarning'//achar(27)//'[0m')
    type(mtype), public, parameter :: DebugMessage   = mtype(int(b'100'),'Debug')
    type(mtype), public, parameter :: InfoMessage    = mtype(int(b'1000'),'')

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

    logical pure function eq_mtype(a,b) result(equal)
        class(mtype), intent(in)                 :: a
        class(mtype), intent(in)                 :: b
        equal = (a%value == b%value)
    end function


    subroutine message_report_screen(msgtype, message, filename, line)
        implicit none
        type(mtype), intent(in)                  :: msgtype
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
        type(mtype), intent(in)                  :: msgtype
        character(len=*), intent(in)             :: message
        character(len=*), intent(in), optional   :: filename
        integer, intent(in), optional            :: line

        character(len=:), allocatable            :: pos

        if (present(filename) .and. present(line)) then
            pos = '('//trim(filename) //':'// itoa(line)//'): '
        else
            pos = ''
        end if

        write(fid,'(a)') trim(msgtype%header)//pos//message
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
