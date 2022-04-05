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
module odestep_proto
    use kinds
    use odesystem, only: ode_system

    implicit none
    private

    public :: ode_step

    type, abstract :: ode_step
    contains
        private
        procedure(odestep_alloc), public, deferred, pass :: alloc
        procedure(odestep_apply), public, deferred, pass :: apply
        procedure(odestep_name), public, deferred, nopass :: name
        procedure(odestep_order), public, deferred, nopass :: order
        procedure(odestep_dense), public, deferred, nopass :: dense_output
    end type

    abstract interface
        subroutine odestep_alloc(this,neqn,stat)
            import                          :: ode_step
            class(ode_step), intent(inout)  :: this
            integer, intent(in)             :: neqn
            logical, intent(out)            :: stat
        end subroutine odestep_alloc
        pure subroutine odestep_apply(this,system,t,h,y,yp,y_out,yerr,stat)
            import                          :: ode_step, ode_system, rp
            class(ode_step), intent(inout)  :: this
            class(ode_system), intent(in)   :: system
            real(kind=rp), intent(in)       :: t
            real(kind=rp), intent(in)       :: h
            real(kind=rp), intent(in)       :: y(:)
            real(kind=rp), intent(in)       :: yp(:)
            real(kind=rp), intent(out)      :: y_out(:)
            real(kind=rp), intent(out)      :: yerr(:)
            logical, intent(out)            :: stat
        end subroutine odestep_apply
        pure function odestep_name() result(name)
            implicit none
            character(len=:), allocatable   :: name
        end function odestep_name
        integer pure function odestep_order()
        end function odestep_order
        logical pure function odestep_dense()
        end function odestep_dense
    end interface
end module odestep_proto
