module trivial_test_problem
    use kinds
    use odesolver

    implicit none

    type, extends(ode_system) :: my_problem
        integer                        :: numeqs
        real(kind=rp)                  :: Rocketmass=100.0_rp,&
                                          Fuelmass=30.0_rp,&
                                          Impulse=300.0_rp,&
                                          Burnrate=1.0_rp,&
                                          Initgravity=9.81,&
                                          Endthrust=Fuelmass/Burnrate,&
                                          Velocity=Initgravity*impulse,&
                                          Initthrust=Burnrate*Velocity,&
                                          Surfacearea=0.1_rp,&



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
        yp(1) = y(2)
        yp(2) = accel(this,t,y)
        return
    end subroutine my_feval

    real(kind=rp) function accel(this,t,y) result(a)
        implicit none
        class(my_problem)               :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(2)

        real(kind=rp)                   :: trust

        if()
        a =
    end function accel
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

