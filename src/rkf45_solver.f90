module odesolver

    use kinds
    use messagehandler

    implicit none
    private

    ! Print Level flags
    integer, parameter, public         :: PRINTLVL_OFF              = 0
    integer, parameter, public         :: PRINTLVL_ERROR            = 1
    integer, parameter, public         :: PRINTLVL_WARNING          = 2
    integer, parameter, public         :: PRINTLVL_INFO             = 3
    integer, parameter, public         :: PRINTLVL_DEBUG            = 4

    ! global print level variable
    integer, public                    :: ode_printlvl    = PRINTLVL_WARNING

    ! Step adjustment
    integer, parameter                 :: STEPADJUST_ERRNULL        = -2
    integer, parameter                 :: STEPADJUST_DOWN           = -1
    integer, parameter                 :: STEPADJUST_NONE           = 0
    integer, parameter                 :: STEPADJUST_UP             = 1

    ! Solver state flags
    integer, parameter                 :: STATE_ERROR               = -1
    integer, parameter                 :: STATE_NONINIT             = 0
    integer, parameter                 :: STATE_INIT                = 1
    integer, parameter                 :: STATE_CONTINUE            = 2

    ! Report message flags (need real messages here!)
    character(len=*), parameter        :: MESSAGE_CANTINIT          = 'MESSAGE_CANTINIT'
    character(len=*), parameter        :: MESSAGE_NEQNERROR         = 'MESSAGE_NEQNERROR'
    character(len=*), parameter        :: MESSAGE_NEGATIVERELERROR  = 'MESSAGE_NEGATIVERELERROR'
    character(len=*), parameter        :: MESSAGE_NEGATIVEABSERROR  = 'MESSAGE_NEGATIVEABSERROR'
    character(len=*), parameter        :: MESSAGE_NEGATIVECOEFY     = 'MESSAGE_NEGATIVECOEFY'
    character(len=*), parameter        :: MESSAGE_NEGATIVECOEFYP    = 'MESSAGE_NEGATIVECOEFYP'
    character(len=*), parameter        :: MESSAGE_CONTROLINITERROR  = 'MESSAGE_CONTROLINITERROR'
    character(len=*), parameter        :: MESSAGE_WRONGOPTIONSTYPE  = 'MESSAGE_WRONGOPTIONSTYPE'
    character(len=*), parameter        :: MESSAGE_INITVALSIZEERROR  = 'MESSAGE_INITVALSIZEERROR'
    character(len=*), parameter        :: MESSAGE_RELERRTOOSMALL    = 'MESSAGE_RELERRTOOSMALL'
    character(len=*), parameter        :: MESSAGE_SOLVEFLAGPROB     = 'MESSAGE_SOLVEFLAGPROB'
    character(len=*), parameter        :: MESSAGE_ESTERRORVANISH    = 'MESSAGE_ESTERRORVANISH'
    character(len=*), parameter        :: MESSAGE_STEPALLOCERROR    = 'MESSAGE_STEPALLOCERROR'
    character(len=*), parameter        :: MESSAGE_STATEALLOCERROR   = 'MESSAGE_STATEALLOCERROR'
    character(len=*), parameter        :: MESSAGE_SOLUTIONVANISH    = 'MESSAGE_SOLUTIONVANISH'
    character(len=*), parameter        :: MESSAGE_TIMESTEPERROR     = 'MESSAGE_TIMESTEPERROR'
    character(len=*), parameter        :: MESSAGE_MAXSTEPSEXTRAPOL  = 'MESSAGE_MAXSTEPSEXTRAPOL'

    ! Runge-Kutta-Fehlberg coefficients
    real(kind=rp), parameter           :: ah(5) = [1.0_rp/4.0_rp, &
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

    real(kind=rp), parameter           :: c1 =  16.0_rp/135.0_rp, &
                                          c3 =  6656.0_rp/12825.0_rp, &
                                          c4 =  28561.0_rp/56430.0_rp, &
                                          c5 = -9.0_rp/50.0_rp, &
                                          c6 =  2.0_rp/55.0_rp

    real(kind=rp), parameter           :: er1 =  1.0_rp/360.0_rp, &
                                          er3 = -128.0_rp/4275.0_rp, &
                                          er4 = -2197.0_rp/75240.0_rp, &
                                          er5 =  1.0_rp/50.0_rp, &
                                          er6 =  2.0_rp/55.0_rp


    real(kind=rp), parameter           :: relerr_min = 10.0_rp**int(-(precision(1.0_rp)+1)*3/4)
    real(kind=rp), parameter           :: eps = epsilon(1.0_rp)

    type, abstract, public :: ode_system
        integer                               :: neqn
    contains
        procedure(user_feval), deferred, pass :: feval
    end type ode_system

    abstract interface
        subroutine user_feval(this, t, y, yp)
            use kinds
            import                          :: ode_system
            class(ode_system), intent(in)   :: this
            real(kind=rp), intent(in)       :: t
            real(kind=rp), intent(in)       :: y(:)
            real(kind=rp), intent(out)      :: yp(:)
        end subroutine user_feval
    end interface

    type, public :: rkf45_control
        private
        real(kind=rp)                       :: relerr  = relerr_min
        real(kind=rp)                       :: abserr  = eps
        real(kind=rp)                       :: coef_y  = 0.5_rp
        real(kind=rp)                       :: coef_yp = 0.5_rp
    contains
        private
        procedure, pass, public             :: set => rkf45_control_set
        procedure, pass                     :: stepadjust => rkf45_control_stepadjust
        procedure, pass                     :: errorlevel => rkf45_control_errorlevel
    end type rkf45_control

    type :: rkf45_step
        private
        real(kind=rp), allocatable          :: yt(:)
        real(kind=rp), allocatable          :: f1(:)
        real(kind=rp), allocatable          :: f2(:)
        real(kind=rp), allocatable          :: f3(:)
        real(kind=rp), allocatable          :: f4(:)
        real(kind=rp), allocatable          :: f5(:)
    contains
        private
        procedure, pass                     :: alloc => rkf45_step_alloc
        procedure, pass                     :: apply => rkf45_step_apply
        final                               :: rkf45_step_finalize
    end type rkf45_step

    type, public :: rkf45_solver
        private
        integer                             :: stateflag = STATE_NONINIT
        integer                             :: maxsteps  = 0
        integer                             :: nstep = 0
        integer                             :: nfailed_steps = 0
        integer                             :: neqn
        real(kind=rp)                       :: h
        real(kind=rp)                       :: t
        real(kind=rp), allocatable          :: y(:)
        real(kind=rp), allocatable          :: y_out(:)
        real(kind=rp), allocatable          :: yp(:)
        real(kind=rp), allocatable          :: yp_out(:)
        real(kind=rp), allocatable          :: yerr(:)
        class(ode_system), pointer          :: system
        class(rkf45_control), allocatable   :: control
        class(rkf45_step), allocatable      :: step
    contains
        private
        procedure, pass, public             :: init => rkf45_init
        procedure, pass, public             :: solve => rkf45_solve
        procedure, pass, public             :: report => rkf45_report
        procedure, pass, public             :: gett => rkf45_gett
        procedure, pass, public             :: geth => rkf45_geth
        procedure, pass, public             :: gety => rkf45_gety
        procedure, pass, public             :: getyerr => rkf45_getyerr
        procedure, pass, public             :: getyp => rkf45_getyp
        procedure, pass, public             :: getstep => rkf45_getstep
        procedure, pass, public             :: getfailedsteps => rkf45_getfailedsteps
        final                               :: rkf45_solver_finalize
    end type rkf45_solver

contains

    ! rkf45_control procedures
    logical function rkf45_control_set(this, relerr, abserr, coef_y, coef_yp) result(flag)
        implicit none
        class(rkf45_control), intent(inout) :: this
        real(kind=rp), intent(in), optional :: relerr
        real(kind=rp), intent(in), optional :: abserr
        real(kind=rp), intent(in), optional :: coef_y
        real(kind=rp), intent(in), optional :: coef_yp


        if ( present(relerr) ) then
            if (relerr < 0.0_rp) then
                call message(ErrorMessage, MESSAGE_NEGATIVERELERROR, __FILE__, __LINE__)
                flag = .false.
                return
            else
                this%relerr  = relerr
            end if
        end if

        if ( present(abserr) ) then
            if (abserr < 0.0_rp) then
                call message(ErrorMessage, MESSAGE_NEGATIVEABSERROR, __FILE__, __LINE__)
                flag = .false.
                return
            else
                this%abserr  = abserr
            end if
        end if

        if ( present(coef_y) ) then
            if (coef_y < 0.0_rp) then
                call message(ErrorMessage, MESSAGE_NEGATIVECOEFY, __FILE__, __LINE__)
                flag = .false.
                return
            else
                this%coef_y  = coef_y
            end if
        end if

        if ( present(coef_yp) ) then
            if (coef_yp < 0.0_rp) then
                call message(ErrorMessage, MESSAGE_NEGATIVECOEFYP, __FILE__, __LINE__)
                flag = .false.
                return
            else
                this%coef_yp = coef_yp
            end if
        end if

        flag = .true.
        return
    end function rkf45_control_set

    integer function rkf45_control_stepadjust(this,y,yp,yerr,h_old,h_new) result(flag)
        implicit none
        class(rkf45_control), intent(in)    :: this
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
                call message(ErrorMessage, MESSAGE_ESTERRORVANISH, __FILE__, __LINE__)
                flag = STEPADJUST_ERRNULL
                return
            end if
        end do
        if (esttol > 1.1_rp) then
            if (esttol >= 410.0625_rp) then ! 410.0625 = (0.9/0.2)^4 decrease limited to 1/5
                h_new = 0.2_rp*h_old
            else
                h_new = 0.9_rp*h_old/esttol**0.25_rp
            end if
            flag = STEPADJUST_DOWN
        else if (esttol < 0.5_rp) then
            if ( esttol <= 1.889568e-4_rp ) then ! 1.889568e-4 = (0.9/5)^5 increase limited to 5.0
                h_new = 5.0_rp*h_old
            else
                h_new = 0.9_rp*h_old/esttol**0.2_rp
            end if
            flag = STEPADJUST_UP
        else
            h_new = h_old
            flag = STEPADJUST_NONE
        end if
        return
    end function rkf45_control_stepadjust

    elemental real(kind=rp) function rkf45_control_errorlevel(this,y,yp,h) result(errtol)
        implicit none
        class(rkf45_control), intent(in)    :: this
        real(kind=rp), intent(in)           :: y
        real(kind=rp), intent(in)           :: yp
        real(kind=rp), intent(in)           :: h

        errtol = this%relerr*(this%coef_y*abs(y)+this%coef_yp*abs(h*yp))+this%abserr
        return
    end function rkf45_control_errorlevel


    ! rkf45_step  procedures
    logical function rkf45_step_alloc(this,neqn) result(flag)
        implicit none
        class(rkf45_step), intent(inout)    :: this
        real(kind=rp), allocatable          :: r(:) ! size n, residual array (r(x))
        integer, intent(in)                 :: neqn

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
            call message(ErrorMessage, MESSAGE_STEPALLOCERROR, __FILE__, __LINE__)
            flag = .false.
        else
            flag = .true.
        end if
        return
    end function rkf45_step_alloc

    subroutine rkf45_step_apply(step,system,t,h,y,yp,y_out,yerr)
        implicit none
        class(rkf45_step), intent(inout)    :: step
        class(ode_system), intent(in)       :: system
        real(kind=rp), intent(in)           :: t
        real(kind=rp), intent(in)           :: h
        real(kind=rp), intent(in)           :: y(:)
        real(kind=rp), intent(in)           :: yp(:)
        real(kind=rp), allocatable          :: r(:) ! size n, residual array (r(x))
        real(kind=rp), intent(out)          :: y_out(:)
        real(kind=rp), intent(out)          :: yerr(:)

        associate(yt => step%yt, &
                  f1 => step%f1, &
                  f2 => step%f2, &
                  f3 => step%f3, &
                  f4 => step%f4, &
                  f5 => step%f5)

        yt = y + h * a1(1) * yp
        call system%feval(t+ah(1)*h,yt,f1 )

        yt = y + h * (a2(1) * yp + a2(2) * f1)
        call system%feval(t+ah(2)*h,yt,f2)

        yt = y + h * (a3(1) * yp + (a3(2) * f1 + a3(3) * f2))
        call system%feval(t+ah(3)*h,yt,f3)

        yt = y + h * (( a4(1) * yp + a4(4) * f3) + (a4(3) * f2 + a4(2) * f1))
        call system%feval(t+h,yt,f4) ! ah(4) = 1

        yt = y + h * (( a5(1)* yp + (a5(4) * f3 + a5(5)* f4)) + (a5(2) * f1 + a5(3) * f2))
        call system%feval(t+ah(5)*h, yt, f5)

        !  compute the approximate solution at t+h.
        y_out = y + h * (( c1 * yp + (c4 * f3 + c5 * f4)) + (c3 * f2 + c6 * f5))

        ! compute error
        yerr = h * ( (er1 * yp + (er4 * f3 + er5 * f4)) + (er3 * f2 + er6 * f5 ))

        end associate
        return
    end subroutine rkf45_step_apply

    subroutine rkf45_step_finalize(this)
        implicit none
        type(rkf45_step)                    :: this

        deallocate(this%yt, &
                   this%f1, &
                   this%f2, &
                   this%f3, &
                   this%f4, &
                   this%f5)
        return
    end subroutine rkf45_step_finalize


    ! rkf45_solver  procedures
    subroutine rkf45_init(this, system, t0, yinit, h0, maxsteps, control)
!
!   RKF45 initialization:
!       system  - ODE system defined by user
!       t0      - initial time
!       yinit   - initial condition
!       h0      - initial step size (optional)
!       control - user defined control options for solver (optional)
!
        implicit none
        class(rkf45_solver)                         :: this
        class(ode_system), intent(in), target       :: system
        real(kind=rp), intent(in)                   :: t0
        real(kind=rp), intent(in)                   :: yinit(:)
        real(kind=rp), intent(in), optional         :: h0
        integer, intent(in), optional               :: maxsteps
        class(rkf45_control), intent(in), optional  :: control

        integer                                     :: istat

        ! Check if solver wasn't initialazed previously
        if (this%stateflag /= STATE_NONINIT) then
            call message(ErrorMessage, MESSAGE_CANTINIT, __FILE__, __LINE__)
            return
        end if

        ! Set ODE system
        this%system => system
        this%neqn = this%system%neqn

        ! Check if user supplied ODE system have one or more equations
        if (this%neqn < 1) then
            call message(ErrorMessage, MESSAGE_NEQNERROR, __FILE__, __LINE__)
            this%stateflag = STATE_ERROR
            return
        end if

        if (present(maxsteps)) this%maxsteps = maxsteps

        ! If present, define control options
        if (present(control)) then
            allocate(this%control,source=control)
        else
            allocate(rkf45_control :: this%control)
        end if

        ! Check if initial value vector yinit have the same size of the ODE system
        if (this%neqn /= size(yinit,1)) then
            call message(ErrorMessage, MESSAGE_INITVALSIZEERROR, __FILE__, __LINE__)
            this%stateflag = STATE_ERROR
            return
        end if

        ! Initiate step
        allocate(rkf45_step :: this%step)
        if (.not.this%step%alloc(this%neqn)) then
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
        this%y = yinit

        ! Get RHS
        call this%system%feval(this%t, this%y, this%yp)

        ! Estimate initial step size if not supplied
        if (present(h0)) then
            this%h = h0
        else
            this%h = 0.0_rp
            block
                real(kind=rp)               :: errtol(this%neqn)
                errtol = this%control%errorlevel(this%y,this%yp,this%h)
                if (any(this%yp /= 0.0_rp .and. errtol /= 0.0_rp)) then
                    this%h = minval(errtol/abs(this%yp), mask=(this%yp /= 0.0_rp .and. errtol /= 0.0_rp))
                end if
            end block
            if (this%h > 0.0_rp) this%h = this%h**0.2_rp
            this%h = max(this%h,26.0_rp*eps*max(abs(t0),1.0_rp))
        end if

        ! Set solver flag as initiated
        this%stateflag = STATE_INIT

        return
    end subroutine rkf45_init

    subroutine rkf45_solve(this, tout)
        implicit none
        class(rkf45_solver), intent(inout)  :: this
        real(kind=rp), intent(in)           :: tout

        real(kind=rp)                       :: dt
        real(kind=rp)                       :: h_new
        integer                             :: stepadjust_stat
        logical                             :: failed_step, last_step

        ! Check flag
        if (this%stateflag /= STATE_INIT .and. this%stateflag /= STATE_CONTINUE) then
            call message(ErrorMessage, MESSAGE_SOLVEFLAGPROB, __FILE__, __LINE__)
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
                if (this%maxsteps > 0 .and. this%nstep > this%maxsteps) then
                    call message(WarningMessage, MESSAGE_MAXSTEPSEXTRAPOL,__FILE__,__LINE__)
                    this%stateflag = STATE_ERROR
                    return
                end if

                ! apply step
                call this%step%apply(this%system,this%t,this%h,this%y,this%yp,this%y_out,this%yerr)
                ! get yp at t+h
                call this%system%feval(this%t+this%h,this%y_out,this%yp_out)
                ! adjust step size
                stepadjust_stat = this%control%stepadjust(this%y_out,this%yp_out,this%yerr,this%h,h_new)
                ! if get error
                if (stepadjust_stat == STEPADJUST_ERRNULL) then
                    call message(ErrorMessage, MESSAGE_SOLUTIONVANISH, __FILE__, __LINE__)
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
                        call message(ErrorMessage, MESSAGE_TIMESTEPERROR,__FILE__, __LINE__)
                        this%stateflag = STATE_ERROR
                        return
                    end if
                ! step accepted
                else
                    this%t = this%t + this%h
                    this%y = this%y_out
                    this%yp = this%yp_out
                    this%nstep = this%nstep + 1
                    dt = tout - this%t
                    ! if during current step failure occured, preserve step size
                    if (.not.failed_step) this%h = h_new
                    exit step_eval
                end if
            end do step_eval
            if (last_step) exit
        end do

        this%stateflag = STATE_CONTINUE
        return
    end subroutine rkf45_solve

    subroutine rkf45_report(this)
        implicit none
        class(rkf45_solver)                 :: this

        character(len=:), allocatable       :: message

        message = 'Runge-Kutta-Fehlberg solver (order 4/5)'
        call message_report(InfoMessage,message)
        if (this%stateflag == STATE_CONTINUE) then
            message = '  Problem Status:  ' // 'Solved'
        else
            message = '  Problem Status:  ' // 'NOT Solved'
        end if
        call message_report(InfoMessage,message)
        message = '  ODE system size: ' // itoa(this%neqn)
        call message_report(InfoMessage,message)
        message = '  Relative error:  ' // rtoa(this%control%relerr)
        call message_report(InfoMessage,message)
        message = '  Absolute error:  ' // rtoa(this%control%abserr)
        call message_report(InfoMessage,message)
        message = '  Number of steps: ' // itoa(this%nstep)
        call message_report(InfoMessage,message)
        message = '  Failed steps:    ' // itoa(this%nfailed_steps)
        call message_report(InfoMessage,message)
    end subroutine rkf45_report

    pure function rkf45_gett(this) result(t)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        real(kind=rp)                       :: t

        t = this%t
        return
    end function rkf45_gett

    pure function rkf45_geth(this) result(h)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        real(kind=rp)                       :: h

        h = this%h
        return
    end function rkf45_geth

    pure function rkf45_gety(this) result(y)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        real(kind=rp)                       :: y(this%neqn)

        y = this%y
        return
    end function rkf45_gety

    pure function rkf45_getyerr(this) result(yerr)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        real(kind=rp)                       :: yerr(this%neqn)

        yerr = this%yerr
        return
    end function rkf45_getyerr

    pure function rkf45_getyp(this) result(yp)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        real(kind=rp)                       :: yp(this%neqn)

        yp = this%yp
        return
    end function rkf45_getyp

    pure function rkf45_getstep(this) result(step)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        integer                             :: step

        step = this%nstep
        return
    end function rkf45_getstep

    pure function rkf45_getfailedsteps(this) result(failedsteps)
        implicit none
        class(rkf45_solver), intent(in)     :: this
        integer                             :: failedsteps

        failedsteps = this%nfailed_steps
        return
    end function rkf45_getfailedsteps

    subroutine rkf45_solver_finalize(this)
        implicit none
        type(rkf45_solver)                  :: this

        deallocate(this%y, &
                   this%y_out, &
                   this%yp, &
                   this%yp_out, &
                   this%yerr, &
                   this%control, &
                   this%step)
        return
    end subroutine rkf45_solver_finalize

    subroutine message(error_flag, error_msg, filename, line)
        implicit none
        type(msgflag), intent(in)                :: error_flag
        character(len=*), intent(in)             :: error_msg
        character(len=*), intent(in)             :: filename
        integer, intent(in)                      :: line

        call message_report(error_flag,error_msg,filename,line)

    end subroutine message

end module odesolver
