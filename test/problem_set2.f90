module problem_set

    use kinds
    use messagehandler
    use odesolver

    implicit none

    character(len=*), parameter        :: p01_title = 'Problem p01, Enright and Pryce #A1, exponential decay.'
    character(len=*), parameter        :: p02_title = 'Problem p02, Enright and Pryce #A2.'
    character(len=*), parameter        :: p03_title = 'Problem p03, Enright and Pryce #A3.'
    character(len=*), parameter        :: p04_title = 'Problem p04, Enright and Pryce #A4.'
    character(len=*), parameter        :: p05_title = 'Problem p05, Enright and Pryce #A5.'
    character(len=*), parameter        :: plei_title = 'Pleiades problem.'

    type, public, extends(ode_system) :: test_ode_system
        character(len=:), allocatable  :: title
        real(kind=rp)                  :: tinit
        real(kind=rp)                  :: tend
        real(kind=rp), allocatable     :: yinit(:)
        real(kind=rp), allocatable     :: yend(:)
        procedure(user_feval), pass, pointer :: test_feval => null()
        procedure(user_ysol), pass, pointer  :: ysol => null()
    contains
        procedure, pass                :: feval
    end type test_ode_system
        
    abstract interface
        subroutine user_feval(this, t, y, yp)
            use kinds
            import                               :: test_ode_system
            class(test_ode_system), intent(in)   :: this
            real(kind=rp), intent(in)            :: t
            real(kind=rp), intent(in)            :: y(:)
            real(kind=rp), intent(out)           :: yp(:)
        end subroutine user_feval
        subroutine user_ysol(this, t, y)
            use kinds
            import                               :: test_ode_system
            class(test_ode_system), intent(in)   :: this
            real(kind=rp), intent(in)            :: t
            real(kind=rp), intent(out)           :: y(:)
        end subroutine user_ysol
    end interface

    interface test_ode_system
        module procedure test_ode_system_construct
    end interface test_ode_system

    procedure(user_feval), pointer  :: p01_feval_ptr => p01_feval
    procedure(user_ysol), pointer  :: p01_ysol_ptr => p01_ysol

    type(test_ode_system), parameter   :: p01_system = test_ode_system(p_neqn  = 1, &
                                                            p_title = p01_title, &
                                                            p_tinit = 0.0_rp, &
                                                            p_tend  = 20.0_rp, &
                                                            p_yinit = [1.0_rp], &
                                                            p_yend  = [2.061153353012535e-09_rp], &
                                                            p_feval = p01_feval_ptr, &
                                                            p_ysol  = p01_ysol_ptr)
contains

    type(test_ode_system) function test_ode_system_construct(p_neqn, p_title, p_tinit, p_tend, p_yinit, p_yend, p_feval, p_ysol) result(T)
        implicit none
        integer, intent(in)                      ::p_neqn
        character(len=*), intent(in)             ::p_title
        real(kind=rp), intent(in)                ::p_tinit
        real(kind=rp), intent(in)                ::p_tend
        real(kind=rp), intent(in)                ::p_yinit(:)
        real(kind=rp), intent(in)                ::p_yend(:)
        procedure(user_feval), pointer, intent(in)        ::p_feval
        procedure(user_ysol), pointer, intent(in), optional    :: p_ysol

        if (size(p_yinit,1) /= p_neqn .or. size(p_yend) /= p_neqn) then
            call message_report(ErrorMessage,"Size of initial (end) array is not equal to the sistem size.",__FILE__,__LINE__)
            return
        end if
        T%neqn = p_neqn
        T%tinit = p_tinit
        T%tend  = p_tend
        allocate(T%title, source=p_title)
        allocate(T%yinit, source=p_yinit)
        allocate(T%yend, source=p_yend)
        T%test_feval => p_feval
        if (present(p_ysol)) T%ysol => p_ysol
        return
    end function test_ode_system_construct

    subroutine feval(this, t, y, yp)
        implicit none
        class(test_ode_system), intent(in)       :: this
        real(kind=rp), intent(in)                :: t
        real(kind=rp), intent(in)                :: y(:)
        real(kind=rp), intent(out)               :: yp(:)
        call this%test_feval(t,y,yp)
        return
    end subroutine feval

    subroutine p01_feval(this, t, y, yp)
        implicit none
        class(test_ode_system), intent(in)  :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        yp = -y
        return
    end subroutine p01_feval

    subroutine p01_ysol(this, t, y)
        implicit none
        class(test_ode_system), intent(in)  :: this
        real(kind=rp), intent(in)      :: t
        real(kind=rp), intent(out)     :: y(:)
        y = exp(-t)
        return
    end subroutine p01_ysol

end module problem_set
