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
module odesolver

    use kinds
    use messagehandler
    use odestep_proto, only : ode_step
    use odesteps
    use odecontrol
    use odesystem
    use odemessages

    implicit none
    private

    ! Print Level flags
    integer, parameter, public :: &
      PRINTLVL_OFF              = 0, &
      PRINTLVL_ERROR            = 1, &
      PRINTLVL_WARNING          = 2, &
      PRINTLVL_INFO             = 3, &
      PRINTLVL_DEBUG            = 4

    ! Solver state flags
    integer, parameter, public :: &
      STATE_ERROR               = -1, &
      STATE_NONINIT             = 0, &
      STATE_INIT                = 1, &
      STATE_CONTINUE            = 2

    type, public :: ode_solver
        private
        class(ode_system), pointer          :: system => null()
        class(ode_step_control), pointer    :: control => null()
        class(ode_step), pointer            :: step => null()
        logical                             :: control_owner = .false.
        logical                             :: step_owner = .false.
        integer                             :: stateflag = STATE_NONINIT
        integer                             :: maxsteps  = 0
        integer                             :: nsteps = 0
        integer                             :: nfailed_steps = 0
        integer                             :: neqn
        real(kind=rp)                       :: h
        real(kind=rp)                       :: t
        real(kind=rp), allocatable          :: y(:)
        real(kind=rp), allocatable          :: y_out(:)
        real(kind=rp), allocatable          :: yp(:)
        real(kind=rp), allocatable          :: yp_out(:)
        real(kind=rp), allocatable          :: yerr(:)
    contains
        private
        procedure, pass, public             :: init => ode_solver_init
        ! procedure, pass, public             :: reset => ode_solver_reset
        procedure, pass, public             :: solve => ode_solver_solve
        procedure, pass, public             :: report => ode_solver_report
        procedure, pass, public             :: get_t => ode_solver_gett
        procedure, pass, public             :: get_h => ode_solver_geth
        procedure, pass, public             :: get_y => ode_solver_gety
        procedure, pass, public             :: get_yerr => ode_solver_getyerr
        procedure, pass, public             :: get_yp => ode_solver_getyp
        procedure, pass, public             :: get_nsteps => ode_solver_getnsteps
        procedure, pass, public             :: get_nfailedsteps => ode_solver_getnfailedsteps
        final                               :: ode_solver_finalize
    end type ode_solver

contains

    subroutine ode_solver_init(this, system, t0, yinit, h0, maxsteps, control, step)
!
!   solver initialization:
!       system   - ODE system defined by user
!       t0       - initial time
!       yinit    - initial condition
!       h0       - initial step size (optional)
!       maxsteps - number of maximum steps
!       control  - user defined control options for solver (optional)
!       step     - user defined stepper for solver (optional)
!
        implicit none
        class(ode_solver), intent(inout)            :: this
        class(ode_system), intent(in), target       :: system
        real(kind=rp), intent(in)                   :: t0
        real(kind=rp), intent(in)                   :: yinit(:)
        real(kind=rp), intent(in), optional         :: h0
        integer, intent(in), optional               :: maxsteps
        class(ode_step_control), intent(in), target, optional :: control
        class(ode_step), intent(in), target, optional         :: step

        logical                                     :: flg
        integer                                     :: istat

        ! Check if solver wasn't initialazed previously
        if (this%stateflag /= STATE_NONINIT) then
            call message_report(ErrorMessage, MESSAGE_CANTINIT, __FILE__, __LINE__)
            return
        end if

        ! Set ODE system
        this%neqn = system%dim()
        this%system => system

        ! Check if user supplied ODE system have one or more equations
        if (this%neqn < 1) then
            call message_report(ErrorMessage, MESSAGE_NEQNERROR, __FILE__, __LINE__)
            this%stateflag = STATE_ERROR
            return
        end if

        ! Check if initial value vector yinit have at least the same size of the ODE system
        if (this%neqn /= size(yinit,1)) then
            call message_report(ErrorMessage, MESSAGE_INITVALSIZEERROR, __FILE__, __LINE__)
            this%stateflag = STATE_ERROR
            return
        end if

        ! Set maximum number of step evaluations
        if (present(maxsteps)) this%maxsteps = maxsteps

        ! If present, point to user control options, otherwise allocate the default.
        if (present(control)) then
            this%control => control
        else
            allocate(ode_step_control :: this%control)
            this%control_owner = .true.
        end if

        ! If present, point to user defined stepper, otherwise allocate the default.
        if (present(step)) then
            this%step => step
        else
            allocate(default_step :: this%step)
            this%step_owner = .true.
        end if
        call this%step%alloc(this%neqn, flg)
        if (.not. flg) then
            this%stateflag = STATE_ERROR
            return
        end if

        ! Allocate variables
        allocate(this%y(this%neqn), &
                 this%y_out(this%neqn), &
                 this%yp(this%neqn), &
                 this%yp_out(this%neqn), &
                 this%yerr(this%neqn), stat=istat)

        if (istat /= 0) then
            if (allocated(this%y)) deallocate(this%y)
            if (allocated(this%y_out)) deallocate(this%y_out)
            if (allocated(this%yp)) deallocate(this%yp)
            if (allocated(this%yp_out)) deallocate(this%yp_out)
            if (allocated(this%yerr)) deallocate(this%yerr)
            this%stateflag = STATE_ERROR
            return
        end if

        ! Set initial value
        this%t = t0
        this%y(1:this%neqn) = yinit(1:this%neqn)

        ! Get RHS
        call this%system%feval(this%t, this%y, this%yp, flg)
        if (.not. flg) then
            call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
            this%stateflag = STATE_ERROR
            return
        end if

        ! Estimate initial step size if not supplied
        if (present(h0)) then
            this%h = h0
        else
            this%h = 0.0_rp
            block
                real(kind=rp)               :: errtol(this%neqn)
                errtol = this%control%errorlevel(this%y,this%yp,this%h)
                if (any(this%yp(1:this%neqn) /= 0.0_rp .and. errtol(1:this%neqn) /= 0.0_rp)) then
                    this%h = maxval(abs(this%yp(1:this%neqn))/errtol(1:this%neqn), &
                                    mask=(errtol(1:this%neqn) /= 0.0_rp))
                end if
            end block
            if (this%h > 0.0_rp) this%h = this%h**(-1.0_rp/real(this%step%order(),rp))
            this%h = max(this%h,26.0_rp*eps*max(abs(t0),1.0_rp))
        end if

        ! Set solver flag as initiated
        this%stateflag = STATE_INIT
    end subroutine ode_solver_init

    subroutine ode_solver_solve(this, tout)
        implicit none
        class(ode_solver), intent(inout)    :: this
        real(kind=rp), intent(in)           :: tout

        real(kind=rp)                       :: dt
        real(kind=rp)                       :: h_new
        integer                             :: stepadjust_stat
        logical                             :: failed_step, last_step
        logical                             :: flg

        ! Check flag
        if (this%stateflag /= STATE_INIT .and. this%stateflag /= STATE_CONTINUE) then
            call message_report(ErrorMessage, MESSAGE_SOLVEFLAGPROB, __FILE__, __LINE__)
            return
        end if

        dt = tout - this%t

!        ! If tout is close to t, just estrapolate
!        if (abs(dt) <= 26.0_rp*eps*abs(this%t)) then
!            this%t = tout
!            this%y = this%y +dt*this%yp
!            call this%system%feval(this%t,this%y,this%yp)
!            this%stateflag = STATE_CONTINUE
!            return
!        end if

        ! Adjust time step
        if (this%stateflag == STATE_INIT) then
            this%h = max(this%h,26.0_rp*eps*abs(dt))
        end if

        ! Set last step indicator
        last_step = .false.

        ! Set time step direction
        this%h = sign(this%h, dt)

        ! Begin steps
        do
            failed_step = .false.

            ! Adjust the stepsize if necessary to hit the output point
            if (2.0_rp*abs(this%h) > abs(dt)) then
                if (abs(this%h) >= abs(dt) ) then
                    last_step = .true.
                    this%h = dt
                else
                    this%h = 0.5_rp*dt
                end if
            end if

            ! Try to advance the integration from t to t+h
 step_eval: do
                ! Check if maximum number of steps was extrapolated
                if (this%maxsteps > 0 .and. this%nsteps > this%maxsteps) then
                    call message_report(WarningMessage, MESSAGE_MAXSTEPSEXTRAPOL,__FILE__,__LINE__)
                    this%stateflag = STATE_ERROR
                    return
                end if

                ! apply step
                call this%step%apply(this%system,this%t,this%h,this%y,this%yp,this%y_out,this%yerr,flg)
                if (.not. flg) then
                    call message_report(ErrorMessage, MESSAGE_STEPAPPLYERROR, __FILE__, __LINE__)
                    this%stateflag = STATE_ERROR
                    return
                end if
                ! get yp at t+h
                call this%system%feval(this%t+this%h,this%y_out,this%yp_out,flg)
                if (.not. flg) then
                    call message_report(ErrorMessage, MESSAGE_USERFEVALERROR, __FILE__, __LINE__)
                    this%stateflag = STATE_ERROR
                    return
                end if
                ! adjust step size
                stepadjust_stat = this%control%stepadjust(this%y_out,this%yp_out,this%yerr,this%h,h_new)
                ! if get error
                if (stepadjust_stat == STEPADJUST_ERRNULL) then
                    call message_report(ErrorMessage, MESSAGE_SOLUTIONVANISH, __FILE__, __LINE__)
                    this%stateflag = STATE_ERROR
                    return
                end if
                ! if step size was reduced: try again, if possible
                if (stepadjust_stat == STEPADJUST_DOWN) then
                    failed_step = .true.
                    last_step = .false.
                    if (abs(h_new) < abs(this%h) .and. abs((this%t+h_new)-this%t) > 0.0_rp) then
                        this%nfailed_steps = this%nfailed_steps + 1
                        this%h = h_new
                    else
                        call message_report(ErrorMessage, MESSAGE_TIMESTEPERROR,__FILE__, __LINE__)
                        this%stateflag = STATE_ERROR
                        return
                    end if
                ! step accepted
                else
                    this%t = this%t + this%h
                    this%y = this%y_out
                    this%yp = this%yp_out
                    this%nsteps = this%nsteps + 1
                    dt = tout - this%t
                    ! if during current step failure occured, preserve step size
                    if (.not.failed_step) this%h = h_new
                    exit step_eval
                end if
            end do step_eval
            if (last_step) exit
        end do

        this%stateflag = STATE_CONTINUE
    end subroutine ode_solver_solve

    subroutine ode_solver_report(this)
        implicit none
        class(ode_solver)                 :: this

        character(len=:), allocatable       :: msg

        msg = this%step%name()//':'
        call message_report(InfoMessage,msg)
        if (this%stateflag == STATE_CONTINUE) then
            msg = '  Problem Status:  ' // 'Solved'
        else
            msg = '  Problem Status:  ' // 'NOT Solved'
        end if
        call message_report(InfoMessage,msg)
        msg = '  ODE system size: ' // itoa(this%neqn)
        call message_report(InfoMessage,msg)
        msg = '  Relative error:  ' // rtoa(this%control%relerr)
        call message_report(InfoMessage,msg)
        msg = '  Absolute error:  ' // rtoa(this%control%abserr)
        call message_report(InfoMessage,msg)
        msg = '  Number of steps: ' // itoa(this%nsteps)
        call message_report(InfoMessage,msg)
        msg = '  Failed steps:    ' // itoa(this%nfailed_steps)
        call message_report(InfoMessage,msg)
    end subroutine ode_solver_report

    pure function ode_solver_gett(this) result(t)
        implicit none
        class(ode_solver), intent(in)       :: this
        real(kind=rp)                       :: t

        t = this%t
    end function ode_solver_gett

    pure function ode_solver_geth(this) result(h)
        implicit none
        class(ode_solver), intent(in)       :: this
        real(kind=rp)                       :: h

        h = this%h
    end function ode_solver_geth

    pure function ode_solver_gety(this) result(y)
        implicit none
        class(ode_solver), intent(in)       :: this
        real(kind=rp)                       :: y(this%neqn)

        y = this%y
    end function ode_solver_gety

    pure function ode_solver_getyerr(this) result(yerr)
        implicit none
        class(ode_solver), intent(in)       :: this
        real(kind=rp)                       :: yerr(this%neqn)

        yerr = this%yerr
    end function ode_solver_getyerr

    pure function ode_solver_getyp(this) result(yp)
        implicit none
        class(ode_solver), intent(in)       :: this
        real(kind=rp)                       :: yp(this%neqn)

        yp = this%yp
    end function ode_solver_getyp

    pure function ode_solver_getnsteps(this) result(step)
        implicit none
        class(ode_solver), intent(in)       :: this
        integer                             :: step

        step = this%nsteps
    end function ode_solver_getnsteps

    pure function ode_solver_getnfailedsteps(this) result(failedsteps)
        implicit none
        class(ode_solver), intent(in)       :: this
        integer                             :: failedsteps

        failedsteps = this%nfailed_steps
    end function ode_solver_getnfailedsteps

    subroutine ode_solver_finalize(this)
        implicit none
        type(ode_solver)                    :: this

        deallocate(this%y, &
                   this%y_out, &
                   this%yp, &
                   this%yp_out, &
                   this%yerr, &
                   this%control, &
                   this%step)
    end subroutine ode_solver_finalize

end module odesolver
