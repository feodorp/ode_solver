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
module odemessages
    implicit none
    private

    ! Report message flags (need real messages here!)
    character(len=*), parameter, public :: &
      MESSAGE_CANTINIT          = 'MESSAGE_CANTINIT', &
      MESSAGE_NEQNERROR         = 'MESSAGE_NEQNERROR', &
      MESSAGE_NEGATIVERELERROR  = 'MESSAGE_NEGATIVERELERROR', &
      MESSAGE_NEGATIVEABSERROR  = 'MESSAGE_NEGATIVEABSERROR', &
      MESSAGE_NEGATIVECOEFY     = 'MESSAGE_NEGATIVECOEFY', &
      MESSAGE_NEGATIVECOEFYP    = 'MESSAGE_NEGATIVECOEFYP', &
      MESSAGE_CONTROLINITERROR  = 'MESSAGE_CONTROLINITERROR', &
      MESSAGE_WRONGOPTIONSTYPE  = 'MESSAGE_WRONGOPTIONSTYPE', &
      MESSAGE_INITVALSIZEERROR  = 'MESSAGE_INITVALSIZEERROR', &
      MESSAGE_RELERRTOOSMALL    = 'MESSAGE_RELERRTOOSMALL', &
      MESSAGE_SOLVEFLAGPROB     = 'MESSAGE_SOLVEFLAGPROB', &
      MESSAGE_ESTERRORVANISH    = 'MESSAGE_ESTERRORVANISH', &
      MESSAGE_STEPALLOCERROR    = 'MESSAGE_STEPALLOCERROR', &
      MESSAGE_STATEALLOCERROR   = 'MESSAGE_STATEALLOCERROR', &
      MESSAGE_SOLUTIONVANISH    = 'MESSAGE_SOLUTIONVANISH', &
      MESSAGE_TIMESTEPERROR     = 'MESSAGE_TIMESTEPERROR', &
      MESSAGE_MAXSTEPSEXTRAPOL  = 'MESSAGE_MAXSTEPSEXTRAPOL', &
      MESSAGE_STEPAPPLYERROR    = 'Step evaluation failed due to a user-supplied system evaluation error.', &
      MESSAGE_USERFEVALERROR    = 'Evaluation of user-supplied ODE system failed.', &
      MESSAGE_USERJACEVALERROR  = 'Evaluation of user-supplied Jacobian of ODE system failed.'
end module odemessages
