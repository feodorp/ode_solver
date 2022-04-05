module trivial_test_problem
    use kinds
    use ode

    implicit none

    type, extends(ode_system) :: my_problem
        integer                        :: neqs = 1
    contains
        procedure, pass                :: dim => my_dim
        procedure, pass                :: feval => my_feval
    end type my_problem
contains
    pure integer function my_dim(this) result(n)
        implicit none
        class(my_problem), intent(in)   :: this

        n = this%neqs
    end function my_dim

    pure subroutine my_feval(this, t, y, yp, stat)
        implicit none
        class(my_problem), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat

        ! y' = -y
        yp(:) = -y(:)
        stat = .true.
    end subroutine my_feval
end module trivial_test_problem


program trivial_test
    use kinds
    use odesolver
    use trivial_test_problem

    implicit none

    type(my_problem)                        :: prob
    type(ode_solver)                        :: solver
    type(default_step)                        :: step
    type(ode_step_control)                  :: control
    real(kind=rp)                           :: t0, tf, y0(1)

    ! Initial value problem is given by:
    ! y'=-y
    ! y(0) = 1.0
    t0 = 0.0_rp
    y0 = 1.0_rp
    tf = 1.0_rp
    call solver%init(prob,t0,y0, control=control, step=step) ! init problem
    call solver%solve(tf) ! solve for t = 10.0
    call solver%report()  ! print final report
end

