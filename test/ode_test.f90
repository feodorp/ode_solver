program ode_test
   use kinds
   use ode
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
           type(ode_solver)                 :: solver
           real(kind=rp), allocatable       :: y(:)

           allocate(y(prob%dim()))

           call solver%init(prob,prob%tinit(),prob%yinit())

           write(*,'(a)') new_line('a')//new_line('a')
           write(*,'(a90)') repeat('-',90)
           write(*,'(20x, a)') prob%gettitle()
           write(*,'(a90)') repeat('-',90)
           write(*,'(2x,a4,7a12)') "step","t", "h","||y||","||yerr||","||y'||","||y_e||","||y-y_e||"
           write(*,'(2x,i4,2F12.6,3ES12.4E2)',advance='no') solver%get_nsteps(), solver%get_t(), solver%get_h(), &
               norm2(solver%get_y()), 0.0_rp, norm2(solver%get_yp())
           if (prob%analiticsol()) then
               y = prob%ysol(solver%get_t())
               write(*,'(2ES12.4E2)') norm2(y), norm2(solver%get_y()-y)
           else
               write(*,*)
           end if

           dt = (prob%tend()-prob%tinit())/real(n,kind=rp)

           do i=1,n-1
               call solver%solve(prob%tinit()+real(i,kind=rp)*dt)
               write(*,'(2x,i4,2F12.6,3ES12.4E2)',advance='no') solver%get_nsteps(), solver%get_t(), solver%get_h(), &
                   norm2(solver%get_y()), norm2(solver%get_yerr()), norm2(solver%get_yp())
               if (prob%analiticsol()) then
                   y = prob%ysol(solver%get_t())
                   write(*,'(2ES12.4E2)') norm2(y), norm2(solver%get_y()-y)
               else
                   write(*,*)
               end if
           end do
           call solver%solve(prob%tinit()+real(n,kind=rp)*dt)
           write(*,'(2x,i4,2F12.6,5ES12.4E2)') solver%get_nsteps(), solver%get_t(), solver%get_h(),&
               norm2(solver%get_y()), norm2(solver%get_yerr()), &
               norm2(solver%get_yp()), norm2(prob%yend()), &
               norm2(solver%get_y()-prob%yend())
           write(*,'(a90)') repeat('-',90)
           call solver%report()
       end block

   end do
   call deallocate_test_problems(problems)
end
