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
module odecontrol
    use kinds
    use messagehandler
    use odemessages

    implicit none
    private

    real(kind=rp), parameter, public        :: relerr_min = 10.0_rp**int(-(precision(1.0_rp)+1)*3/4)
    real(kind=rp), parameter, public        :: eps = epsilon(1.0_rp)

    ! Step adjustment
    integer, parameter, public :: &
        STEPADJUST_ERRNULL        = -2, &
        STEPADJUST_DOWN           = -1, &
        STEPADJUST_NONE           = 0, &
        STEPADJUST_UP             = 1

    type, public :: ode_step_control
        real(kind=rp)                       :: relerr  = relerr_min
        real(kind=rp)                       :: abserr  = eps
        real(kind=rp)                       :: coef_y  = 0.5_rp
        real(kind=rp)                       :: coef_yp = 0.5_rp
    contains
        procedure, pass                     :: set => ode_step_control_set
        procedure, pass                     :: stepadjust => ode_step_control_stepadjust
        procedure, pass                     :: errorlevel => ode_step_control_errorlevel
    end type ode_step_control

contains

    logical function ode_step_control_set(this, relerr, abserr, coef_y, coef_yp) result(flg)
        implicit none
        class(ode_step_control), intent(inout) :: this
        real(kind=rp), intent(in), optional :: relerr
        real(kind=rp), intent(in), optional :: abserr
        real(kind=rp), intent(in), optional :: coef_y
        real(kind=rp), intent(in), optional :: coef_yp

        if ( present(relerr) ) then
            if (relerr < 0.0_rp) then
                call message_report(ErrorMessage, MESSAGE_NEGATIVERELERROR, __FILE__, __LINE__)
                flg = .false.
                return
            else
                this%relerr  = relerr
            end if
        end if

        if ( present(abserr) ) then
            if (abserr < 0.0_rp) then
                call message_report(ErrorMessage, MESSAGE_NEGATIVEABSERROR, __FILE__, __LINE__)
                flg = .false.
                return
            else
                this%abserr  = abserr
            end if
        end if

        if ( present(coef_y) ) then
            if (coef_y < 0.0_rp) then
                call message_report(ErrorMessage, MESSAGE_NEGATIVECOEFY, __FILE__, __LINE__)
                flg = .false.
                return
            else
                this%coef_y  = coef_y
            end if
        end if

        if ( present(coef_yp) ) then
            if (coef_yp < 0.0_rp) then
                call message_report(ErrorMessage, MESSAGE_NEGATIVECOEFYP, __FILE__, __LINE__)
                flg = .false.
                return
            else
                this%coef_yp = coef_yp
            end if
        end if

        flg = .true.
        return
    end function ode_step_control_set

    integer function ode_step_control_stepadjust(this,y,yp,yerr,h_old,h_new) result(flg)
        implicit none
        class(ode_step_control), intent(in)  :: this
        real(kind=rp), intent(in)           :: y(:)
        real(kind=rp), intent(in)           :: yp(:)
        real(kind=rp), intent(in)           :: yerr(:)
        real(kind=rp), intent(in)           :: h_old
        real(kind=rp), intent(out)          :: h_new

        real(kind=rp)                       :: errtol
        real(kind=rp)                       :: esttol
        integer                             :: i

        esttol = 0.0_rp
        do i=1, size(y,1)
            errtol = this%errorlevel(y(i),yp(i),h_old)
            if (errtol > 0.0_rp ) then
                esttol = max(esttol,abs(yerr(i)/errtol))
            else
                call message_report(ErrorMessage, MESSAGE_ESTERRORVANISH, __FILE__, __LINE__)
                flg = STEPADJUST_ERRNULL
                return
            end if
        end do
        if (esttol > 1.1_rp) then
            if (esttol >= 410.0625_rp) then ! 410.0625 = (0.9/0.2)^4 decrease limited to 1/5
                h_new = 0.2_rp*h_old
            else
                h_new = 0.9_rp*h_old/esttol**0.25_rp
            end if
            flg = STEPADJUST_DOWN
        else if (esttol < 0.5_rp) then
            if ( esttol <= 1.889568e-4_rp ) then ! 1.889568e-4 = (0.9/5)^5 increase limited to 5.0
                h_new = 5.0_rp*h_old
            else
                h_new = 0.9_rp*h_old/esttol**0.2_rp
            end if
            flg = STEPADJUST_UP
        else
            h_new = h_old
            flg = STEPADJUST_NONE
        end if
        return
    end function ode_step_control_stepadjust

    elemental real(kind=rp) function ode_step_control_errorlevel(this,y,yp,h) result(errtol)
        implicit none
        class(ode_step_control), intent(in) :: this
        real(kind=rp), intent(in)           :: y
        real(kind=rp), intent(in)           :: yp
        real(kind=rp), intent(in)           :: h

        errtol = this%relerr*(this%coef_y*abs(y)+this%coef_yp*abs(h*yp))+this%abserr
        return
    end function ode_step_control_errorlevel


end module odecontrol
