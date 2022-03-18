module trivial_test_problem
    use kinds
    use odesolver

    implicit none

    type, extends(ode_system) :: my_problem
        integer                        :: numeqs
    contains
        procedure, pass                :: neqn => my_neqn
        procedure, pass                :: feval => my_feval
    end type my_problem
contains
    pure integer function my_neqn(this)
        implicit none
        class(my_problem), intent(in)   :: this

        neqn = this%numeqs
    end function my_neqn

    subroutine my_feval(this, t, y, yp)
        implicit none
        class(my_problem), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)

        ! y' = -y
        yp = -y
        return
    end subroutine my_feval
end module trivial_test_problem
    

program trivial_test
    use kinds
    use odesolver
    use trivial_test_problem

    implicit none

    type(my_problem)                   :: prob
    type(rkf45_solver)                 :: solver

    prob%numeqs = 1

    ! Initial value problem is given by:
    ! y'=-y
    ! y(0) = 1.0 
    
    call solver%init(prob,0.0,1.0) ! init problem
    call solver%solve(10.0) ! solve for t = 10.0
    call solver%report() ! print final report
end
    
