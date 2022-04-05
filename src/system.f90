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
module odesystem
    use kinds
    use messagehandler

    implicit none
    private

    public :: ode_system

!
! ODE system:
!   y' = f(t,y)
!
    type, abstract :: ode_system
    contains
        procedure(odesystem_user_dim), public, deferred, pass     :: dim
        procedure(odesystem_user_feval), public, deferred, pass   :: feval
        ! procedure, public, pass :: jaceval => odesystem_user_nojaceval
    end type ode_system

    abstract interface
        pure subroutine odesystem_user_feval(this, t, y, yp, stat)
            import                          :: ode_system, rp
            class(ode_system), intent(in)   :: this
            real(kind=rp), intent(in)       :: t
            real(kind=rp), intent(in)       :: y(:)
            real(kind=rp), intent(out)      :: yp(:)
            logical, intent(out)            :: stat
        end subroutine odesystem_user_feval
        ! subroutine odesystem_user_jaceval(this, t, y, dfdt, dfdy, stat)
        !     import                          :: ode_system, rp
        !     class(ode_system), intent(in)   :: this
        !     real(kind=rp), intent(in)       :: t
        !     real(kind=rp), intent(in)       :: y(:)
        !     real(kind=rp), intent(out)      :: dfdt(:)
        !     real(kind=rp), intent(out)      :: dfdy(:,:)
        !     logical, intent(out), optional  :: stat
        ! end subroutine odesystem_user_jaceval
        integer pure function odesystem_user_dim(this)
            import                          :: ode_system
            class(ode_system), intent(in)   :: this
        end function odesystem_user_dim
    end interface

contains

    pure subroutine odesystem_user_nojaceval(this, t, y, dfdt, dfdy, stat)
        implicit none
        class(ode_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: dfdt(:)
        real(kind=rp), intent(out)      :: dfdy(:,:)
        logical, intent(out)            :: stat

        ! call message_report(ErrorMessage,'Jacobian evaluation function must be provided by&
        !                   & user in extanded ode_system derived type!')
        stat = .false.
    end subroutine odesystem_user_nojaceval
end module odesystem
