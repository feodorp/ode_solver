program ode_test
   use kinds
   use odesolver
   use problem_set

   implicit none

   type(test_problems), allocatable     :: problems(:)
   class(test_ode_system), pointer      :: prob

   real(kind=rp)                        :: dt
   integer                              :: n = 10 
   integer                              :: k, i

   call allocate_test_problems(problems)

   do k=1, size(problems,1)
   prob => problems(k)%problem

   block
   type(rkf45_solver)                   :: solver
   real(kind=rp), allocatable           :: y(:)

   allocate(y(prob%neqn()))

   call solver%init(prob,prob%tinit(),prob%yinit())

   write(*,'(a)') new_line('a')//new_line('a')
   write(*,'(a90)') repeat('-',90)
   write(*,'(20x, a)') prob%gettitle()
   write(*,'(a90)') repeat('-',90)
   write(*,'(2x,a4,7a12)') "step","t", "h","||y||","||yerr||","||y'||","||y_e||","||y-y_e||"
   write(*,'(2x,i4,2F12.6,3ES12.4E2)',advance='no') solver%getstep(), solver%gett(), solver%geth(), norm2(solver%gety()), 0.0_rp, norm2(solver%getyp())
   if (prob%analiticsol()) then
       y = prob%ysol(solver%gett())
       write(*,'(2ES12.4E2)') norm2(y), norm2(solver%gety()-y)
   else
       write(*,*)
   end if

   dt = (prob%tend()-prob%tinit())/real(n,kind=rp)
        
   do i=1,n-1
       call solver%solve(prob%tinit()+real(i,kind=rp)*dt)
       write(*,'(2x,i4,2F12.6,3ES12.4E2)',advance='no') solver%getstep(), solver%gett(), solver%geth(), norm2(solver%gety()), norm2(solver%getyerr()), norm2(solver%getyp())
       if (prob%analiticsol()) then
           y = prob%ysol(solver%gett())
           write(*,'(2ES12.4E2)') norm2(y), norm2(solver%gety()-y)
       else
           write(*,*)
       end if
   end do
   call solver%solve(prob%tinit()+real(n,kind=rp)*dt)
   write(*,'(2x,i4,2F12.6,5ES12.4E2)') solver%getstep(), solver%gett(), solver%geth(),& 
                                       norm2(solver%gety()), norm2(solver%getyerr()), &
                                       norm2(solver%getyp()), norm2(prob%yend()), &
                                       norm2(solver%gety()-prob%yend())
   write(*,'(a90)') repeat('-',90)
   call solver%report()
   end block

   end do
   call deallocate_test_problems(problems)
end
