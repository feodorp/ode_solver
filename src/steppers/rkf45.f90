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
! Reference:
! Hairer, Norsett, Wanner: Solving Ordinary Differential Equations, Nonstiff Problems. vol I,
! Chapter II, Section II.5 - "Embedded Formulas of Order 5"
!
module rkf45
    use kinds
    use messagehandler
    use odesystem, only : ode_system
    use odestep_proto, only : ode_step
    use odemessages

    implicit none
    private

    integer, parameter                      :: rkf45_order = 5
    logical, parameter                      :: rkf45_dense = .false.
    character(len=*), parameter             :: rkf45_name = 'Runge-Kutta-Fehlberg (order 4/5)'

    type, extends(ode_step), public :: rkf45_step
        private
        real(kind=rp), allocatable          :: yt(:)
        real(kind=rp), allocatable          :: f1(:)
        real(kind=rp), allocatable          :: f2(:)
        real(kind=rp), allocatable          :: f3(:)
        real(kind=rp), allocatable          :: f4(:)
        real(kind=rp), allocatable          :: f5(:)
    contains
        procedure, public, pass             :: alloc => rkf45_step_alloc
        procedure, public, pass             :: apply => rkf45_step_apply
        procedure, public, nopass           :: name => rkf45_step_name
        procedure, public, nopass           :: order => rkf45_step_order
        procedure, public, nopass           :: dense_output => rkf45_step_dense
        final                               :: rkf45_step_finalize
    end type rkf45_step

    ! Runge-Kutta-Fehlberg coefficients
    !
    real(kind=rp), parameter           :: c(5)  = [1.0_rp/4.0_rp, &
                                                   3.0_rp/8.0_rp, &
                                                   12.0_rp/13.0_rp, &
                                                   1.0_rp, &
                                                   1.0_rp/2.0_rp]

    real(kind=rp), parameter           :: a1(1) = [1.0_rp/4.0_rp]
    real(kind=rp), parameter           :: a2(2) = [3.0_rp/32.0_rp, &
                                                   9.0_rp/32.0_rp]

    real(kind=rp), parameter           :: a3(3) = [1932.0_rp/2197.0_rp, &
                                                  -7200.0_rp/2197.0_rp, &
                                                   7296.0_rp/2197.0_rp]

    real(kind=rp), parameter           :: a4(4) = [8341.0_rp/4104.0_rp, &
                                                  -32832.0_rp/4104.0_rp,&
                                                   29440.0_rp/4104.0_rp,&
                                                  -845.0_rp/4104.0_rp]

    real(kind=rp), parameter           :: a5(5) = [-6080.0_rp/20520.0_rp, &
                                                    41040.0_rp/20520.0_rp,&
                                                   -28352.0_rp/20520.0_rp,&
                                                    9295.0_rp/20520.0_rp, &
                                                   -5643.0_rp/20520.0_rp]
    ! coefficients for fifth-order solution
    real(kind=rp), parameter           :: b1 =  16.0_rp/135.0_rp, &
                                          b3 =  6656.0_rp/12825.0_rp, &
                                          b4 =  28561.0_rp/56430.0_rp, &
                                          b5 = -9.0_rp/50.0_rp, &
                                          b6 =  2.0_rp/55.0_rp

    ! e(i) = b(i) - b'(i), where b' are coefficients for forth-order solution
    real(kind=rp), parameter           :: e1 =  1.0_rp/360.0_rp, &
                                          e3 = -128.0_rp/4275.0_rp, &
                                          e4 = -2197.0_rp/75240.0_rp, &
                                          e5 =  1.0_rp/50.0_rp, &
                                          e6 =  2.0_rp/55.0_rp

contains

    subroutine rkf45_step_alloc(this,neqn,stat)
        implicit none
        class(rkf45_step), intent(inout)    :: this
        integer, intent(in)                 :: neqn
        logical, intent(out)                :: stat

        integer                             :: istat

        allocate(this%yt(neqn), &
                 this%f1(neqn), &
                 this%f2(neqn), &
                 this%f3(neqn), &
                 this%f4(neqn), &
                 this%f5(neqn), stat=istat)

        if (istat /= 0) then
            if (allocated(this%yt)) deallocate(this%yt)
            if (allocated(this%f1)) deallocate(this%f1)
            if (allocated(this%f2)) deallocate(this%f2)
            if (allocated(this%f3)) deallocate(this%f3)
            if (allocated(this%f4)) deallocate(this%f4)
            if (allocated(this%f5)) deallocate(this%f5)
            call message_report(ErrorMessage, MESSAGE_STEPALLOCERROR, __FILE__, __LINE__)
            stat = .false.
        else
            stat = .true.
        end if
    end subroutine rkf45_step_alloc

    pure subroutine rkf45_step_apply(this,system,t,h,y,yp,y_out,yerr,stat)
        implicit none
        class(rkf45_step), intent(inout)    :: this
        class(ode_system), intent(in)       :: system
        real(kind=rp), intent(in)           :: t
        real(kind=rp), intent(in)           :: h
        real(kind=rp), intent(in)           :: y(:)
        real(kind=rp), intent(in)           :: yp(:)
        real(kind=rp), intent(out)          :: y_out(:)
        real(kind=rp), intent(out)          :: yerr(:)
        logical, intent(out)                :: stat

        integer                             :: n
        n = system%dim()
        stat = .true.

        associate(yt => this%yt, &
                  f1 => this%f1, &
                  f2 => this%f2, &
                  f3 => this%f3, &
                  f4 => this%f4, &
                  f5 => this%f5)

        yt(1:n) = y(1:n) + h * a1(1) * yp(1:n)
        call system%feval(t+c(1)*h, yt(1:n), f1(1:n), stat)
        if (.not. stat) then
            ! call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
            return
        end if

        yt(1:n) = y(1:n) + h * (a2(1) * yp(1:n) + a2(2) * f1(1:n))
        call system%feval(t+c(2)*h, yt(1:n), f2(1:n), stat)
        if (.not. stat) then
            ! call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
            return
        end if

        yt(1:n) = y(1:n) + h * (a3(1) * yp(1:n) + (a3(2) * f1(1:n) + &
                  a3(3) * f2(1:n)))
        call system%feval(t+c(3)*h, yt(1:n), f3(1:n), stat)
        if (.not. stat) then
            ! call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
            return
        end if

        yt(1:n) = y(1:n) + h * (( a4(1) * yp(1:n) + a4(4) * f3(1:n)) + &
                  (a4(3) * f2(1:n) + a4(2) * f1(1:n)))
        call system%feval(t+h, yt, f4, stat) ! c(4) = 1
        if (.not. stat) then
            ! call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
            return
        end if

        yt = y(1:n) + h * (( a5(1)* yp(1:n) + (a5(4) * f3(1:n) + &
             a5(5) * f4(1:n))) + (a5(2) * f1(1:n) + a5(3) * f2(1:n)))
        call system%feval(t+c(5)*h, yt, f5, stat)
        if (.not. stat) then
            ! call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
            return
        end if

        !  compute the approximate solution at t+h.
        y_out(1:n) = y(1:n) + h * (( b1 * yp(1:n) + (b4 * f3(1:n) + &
                     b5 * f4(1:n))) + (b3 * f2(1:n) + b6 * f5(1:n)))

        ! compute error
        yerr(1:n) = h * ((e1 * yp(1:n) + (e4 * f3(1:n) + e5 * f4(1:n))) + &
                    (e3 * f2(1:n) + e6 * f5(1:n) ))

        end associate
    end subroutine rkf45_step_apply

    pure function rkf45_step_name() result(r)
        implicit none
        character(len=:), allocatable       :: r
        r = rkf45_name
    end function rkf45_step_name

    integer pure function rkf45_step_order() result(r)
        implicit none
        r = rkf45_order
    end function rkf45_step_order

    logical pure function rkf45_step_dense() result(r)
        implicit none
        r = rkf45_dense
    end function rkf45_step_dense

    subroutine rkf45_step_finalize(this)
        implicit none
        type(rkf45_step)                    :: this

        deallocate(this%yt, &
                   this%f1, &
                   this%f2, &
                   this%f3, &
                   this%f4, &
                   this%f5)
    end subroutine rkf45_step_finalize

end module rkf45
