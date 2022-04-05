module problem_set

    use kinds
    use ode

    implicit none

    character(len=*), parameter        :: p01_title = 'Problem p01, Enright and Pryce #A1, exponential decay.'
    character(len=*), parameter        :: p02_title = 'Problem p02, Enright and Pryce #A2.'
    character(len=*), parameter        :: p03_title = 'Problem p03, Enright and Pryce #A3.'
    character(len=*), parameter        :: p04_title = 'Problem p04, Enright and Pryce #A4.'
    character(len=*), parameter        :: p05_title = 'Problem p05, Enright and Pryce #A5.'
    character(len=*), parameter        :: plei_title = 'Pleiades problem.'

    type, abstract, extends(ode_system) :: test_ode_system
    contains
        procedure(user_title), deferred, pass    :: gettitle
        procedure(user_yinit), deferred, pass    :: yinit
        procedure(user_yend), deferred, pass     :: yend
        procedure(user_tinit), deferred, pass    :: tinit
        procedure(user_tend), deferred, pass     :: tend
        procedure(user_ysol), deferred, pass     :: ysol
        procedure(user_anltc), deferred, pass    :: analiticsol
    end type test_ode_system

    abstract interface
        pure function user_title(this) result(title)
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
            character(len=:), allocatable       :: title
        end function user_title
        pure function user_yinit(this) result(yinit)
            use kinds
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
            real(kind=rp), allocatable          :: yinit(:)
        end function user_yinit
        pure function user_yend(this) result(yend)
            use kinds
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
            real(kind=rp), allocatable          :: yend(:)
        end function user_yend
        pure real(kind=rp) function user_tinit(this) result(tinit)
            use kinds
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
        end function user_tinit
        pure real(kind=rp) function user_tend(this) result(tend)
            use kinds
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
        end function user_tend
        pure function user_ysol(this,t) result(ysol)
            use kinds
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
            real(kind=rp), intent(in)           :: t
            real(kind=rp), allocatable          :: ysol(:)
        end function user_ysol
        pure logical function user_anltc(this) result(analiticsol)
            use kinds
            import                              :: test_ode_system
            class(test_ode_system), intent(in)  :: this
        end function user_anltc
    end interface

    type, public :: test_problems
        class(test_ode_system), pointer     :: problem
    end type

    type, public, extends(test_ode_system) :: p01_system
        character(len=len(p01_title))  :: title = p01_title
        integer                        :: neqn = 1
        real(kind=rp), private         :: t_init = 0.0_rp
        real(kind=rp), private         :: t_end  = 20.0_rp
        logical                        :: analitic_sol = .true.
    contains
        procedure, pass                :: dim => p01_dim
        procedure, pass                :: feval => p01_feval
        procedure, pass                :: gettitle => p01_gettitle
        procedure, pass                :: yinit => p01_yinit
        procedure, pass                :: yend => p01_yend
        procedure, pass                :: tinit => p01_tinit
        procedure, pass                :: tend  => p01_tend
        procedure, pass                :: ysol => p01_ysol
        procedure, pass                :: analiticsol => p01_anltc
    end type p01_system

    type, public, extends(test_ode_system) :: p02_system
        character(len=len(p02_title))  :: title = p02_title
        integer                        :: neqn = 1
        real(kind=rp), private         :: t_init = 0.0_rp
        real(kind=rp), private         :: t_end  = 20.0_rp
        logical                        :: analitic_sol = .true.
    contains
        procedure, pass                :: dim => p02_dim
        procedure, pass                :: feval => p02_feval
        procedure, pass                :: gettitle => p02_gettitle
        procedure, pass                :: yinit => p02_yinit
        procedure, pass                :: yend => p02_yend
        procedure, pass                :: tinit => p02_tinit
        procedure, pass                :: tend  => p02_tend
        procedure, pass                :: ysol => p02_ysol
        procedure, pass                :: analiticsol => p02_anltc
    end type p02_system

    type, public, extends(test_ode_system) :: p03_system
        character(len=len(p03_title))  :: title = p03_title
        integer                        :: neqn = 1
        real(kind=rp), private         :: t_init = 0.0_rp
        real(kind=rp), private         :: t_end  = 20.0_rp
        logical                        :: analitic_sol = .true.
    contains
        procedure, pass                :: dim => p03_dim
        procedure, pass                :: feval => p03_feval
        procedure, pass                :: gettitle => p03_gettitle
        procedure, pass                :: yinit => p03_yinit
        procedure, pass                :: yend => p03_yend
        procedure, pass                :: tinit => p03_tinit
        procedure, pass                :: tend  => p03_tend
        procedure, pass                :: ysol => p03_ysol
        procedure, pass                :: analiticsol => p03_anltc
    end type p03_system

    type, public, extends(test_ode_system) :: p04_system
        character(len=len(p04_title))  :: title = p04_title
        integer                        :: neqn = 1
        real(kind=rp), private         :: t_init = 0.0_rp
        real(kind=rp), private         :: t_end  = 20.0_rp
        logical                        :: analitic_sol = .true.
    contains
        procedure, pass                :: dim => p04_dim
        procedure, pass                :: feval => p04_feval
        procedure, pass                :: gettitle => p04_gettitle
        procedure, pass                :: yinit => p04_yinit
        procedure, pass                :: yend => p04_yend
        procedure, pass                :: tinit => p04_tinit
        procedure, pass                :: tend  => p04_tend
        procedure, pass                :: ysol => p04_ysol
        procedure, pass                :: analiticsol => p04_anltc
    end type p04_system

    type, public, extends(test_ode_system) :: p05_system
        character(len=len(p05_title))  :: title = p05_title
        integer                        :: neqn = 1
        real(kind=rp), private         :: t_init = 0.0_rp
        real(kind=rp), private         :: t_end  = 20.0_rp
        logical                        :: analitic_sol = .false.
    contains
        procedure, pass                :: dim => p05_dim
        procedure, pass                :: feval => p05_feval
        procedure, pass                :: gettitle => p05_gettitle
        procedure, pass                :: yinit => p05_yinit
        procedure, pass                :: yend => p05_yend
        procedure, pass                :: tinit => p05_tinit
        procedure, pass                :: tend  => p05_tend
        procedure, pass                :: ysol => p05_ysol
        procedure, pass                :: analiticsol => p05_anltc
    end type p05_system

    type, public, extends(test_ode_system) :: plei_system
        character(len=len(plei_title)) :: title = plei_title
        integer                        :: neqn = 28
        real(kind=rp), private         :: t_init = 0.0_rp
        real(kind=rp), private         :: t_end  = 3.0_rp
        logical                        :: analitic_sol = .false.
    contains
        procedure, pass                :: dim => plei_dim
        procedure, pass                :: feval => plei_feval
        procedure, pass                :: gettitle => plei_gettitle
        procedure, pass                :: yinit => plei_yinit
        procedure, pass                :: yend => plei_yend
        procedure, pass                :: tinit => plei_tinit
        procedure, pass                :: tend  => plei_tend
        procedure, pass                :: ysol => plei_ysol
        procedure, pass                :: analiticsol => plei_anltc
    end type plei_system

contains

    subroutine allocate_test_problems(problems)
        implicit none
        type(test_problems), allocatable    :: problems(:)

        if (allocated(problems)) deallocate(problems)

        allocate(problems(6))
        allocate(p01_system :: problems(1)%problem)
        allocate(p02_system :: problems(2)%problem)
        allocate(p03_system :: problems(3)%problem)
        allocate(p04_system :: problems(4)%problem)
        allocate(p05_system :: problems(5)%problem)
        allocate(plei_system :: problems(6)%problem)
        return
    end subroutine allocate_test_problems

    subroutine deallocate_test_problems(problems)
        implicit none
        type(test_problems), allocatable    :: problems(:)
        integer                             :: i
        if (.not.allocated(problems)) return

        do i = 1, size(problems,1)
            if (associated(problems(i)%problem)) deallocate(problems(i)%problem)
        end do
        deallocate(problems)
        return
    end subroutine deallocate_test_problems


    ! P01 Problem
    integer pure function p01_dim(this) result(n)
        implicit none
        class(p01_system), intent(in)   :: this
        n = this%neqn
    end function p01_dim

    pure subroutine p01_feval(this, t, y, yp, stat)
        implicit none
        class(p01_system), intent(in)  :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat
        yp = -y
        stat = .true.
        return
    end subroutine p01_feval

    pure function p01_gettitle(this) result(title)
        implicit none
        class(p01_system), intent(in)  :: this
        character(len=:), allocatable  :: title
        title = this%title
        return
    end function p01_gettitle

    pure function p01_yinit(this) result(yinit)
        implicit none
        class(p01_system), intent(in)  :: this
        real(kind=rp), allocatable     :: yinit(:)
        allocate(yinit(1))
        yinit = 1.0_rp
        return
    end function p01_yinit

    pure function p01_yend(this) result(yend)
        implicit none
        class(p01_system), intent(in)  :: this
        real(kind=rp), allocatable     :: yend(:)
        allocate(yend(1))
        yend(1) = 2.061153353012535e-09_rp
        return
    end function p01_yend

    pure real(kind=rp) function p01_tinit(this) result(tinit)
        implicit none
        class(p01_system), intent(in)  :: this
        tinit = this%t_init
    end function p01_tinit

    pure real(kind=rp) function p01_tend(this) result(tend)
        implicit none
        class(p01_system), intent(in)  :: this
        tend = this%t_end
    end function p01_tend

    pure function p01_ysol(this,t) result(ysol)
        implicit none
        class(p01_system), intent(in)  :: this
        real(kind=rp), intent(in)      :: t
        real(kind=rp), allocatable     :: ysol(:)
        allocate(ysol(1))
        ysol = exp(-t)
        return
    end function p01_ysol

    pure logical function p01_anltc(this) result(analiticsol)
        implicit none
        class(p01_system), intent(in)  :: this
        analiticsol = this%analitic_sol
    end function p01_anltc

    ! P02 Problem
    integer pure function p02_dim(this) result(n)
        implicit none
        class(p02_system), intent(in)   :: this
        n = this%neqn
    end function p02_dim

    pure subroutine p02_feval(this, t, y, yp, stat)
        implicit none
        class(p02_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat
        yp =  - 0.5_rp*y**3
        stat = .true.
        return
    end subroutine p02_feval

    pure function p02_gettitle(this) result(title)
        implicit none
        class(p02_system), intent(in)  :: this
        character(len=:), allocatable  :: title
        title = this%title
        return
    end function p02_gettitle

    pure function p02_yinit(this) result(yinit)
        implicit none
        class(p02_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yinit(:)
        allocate(yinit(1))
        yinit(1) = 1.0_rp
        return
    end function p02_yinit

    pure function p02_yend(this) result(yend)
        implicit none
        class(p02_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yend(:)
        allocate(yend(1))
        yend(1) = 0.2182178902359887_rp
        return
    end function p02_yend

    pure real(kind=rp) function p02_tinit(this) result(tinit)
        implicit none
        class(p02_system), intent(in)   :: this
        tinit = this%t_init
    end function p02_tinit

    pure real(kind=rp) function p02_tend(this) result(tend)
        implicit none
        class(p02_system), intent(in)   :: this
        tend = this%t_end
    end function p02_tend

    pure function p02_ysol(this,t) result(ysol)
        implicit none
        class(p02_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), allocatable      :: ysol(:)
        allocate(ysol(1))
        ysol = 1.0_rp/sqrt(t+1.0_rp)
        return
    end function p02_ysol

    pure logical function p02_anltc(this) result(analiticsol)
        implicit none
        class(p02_system), intent(in)  :: this
        analiticsol = this%analitic_sol
    end function p02_anltc

    ! P03 Problem
    integer pure function p03_dim(this) result(n)
        implicit none
        class(p03_system), intent(in)   :: this
        n = this%neqn
    end function p03_dim

    pure subroutine p03_feval(this, t, y, yp, stat)
        implicit none
        class(p03_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat
        yp =  y*cos(t)
        stat = .true.
        return
    end subroutine p03_feval

    pure function p03_gettitle(this) result(title)
        implicit none
        class(p03_system), intent(in)  :: this
        character(len=:), allocatable  :: title
        title = this%title
        return
    end function p03_gettitle

    pure function p03_yinit(this) result(yinit)
        implicit none
        class(p03_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yinit(:)
        allocate(yinit(1))
        yinit = 1.0_rp
        return
    end function p03_yinit

    pure function p03_yend(this) result(yend)
        implicit none
        class(p03_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yend(:)
        allocate(yend(1))
        yend = 2.491650271850414_rp
        return
    end function p03_yend

    pure real(kind=rp) function p03_tinit(this) result(tinit)
        implicit none
        class(p03_system), intent(in)   :: this
        tinit = this%t_init
    end function p03_tinit

    pure real(kind=rp) function p03_tend(this) result(tend)
        implicit none
        class(p03_system), intent(in)   :: this
        tend = this%t_end
    end function p03_tend

    pure function p03_ysol(this,t) result(ysol)
        implicit none
        class(p03_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), allocatable      :: ysol(:)
        allocate(ysol(1))
        ysol = exp(sin(t))
        return
    end function p03_ysol

    pure logical function p03_anltc(this) result(analiticsol)
        implicit none
        class(p03_system), intent(in)  :: this
        analiticsol = this%analitic_sol
    end function p03_anltc

    ! P04 Problem
    integer pure function p04_dim(this) result(n)
        implicit none
        class(p04_system), intent(in)   :: this
        n = this%neqn
    end function p04_dim

    pure subroutine p04_feval(this, t, y, yp, stat)
        implicit none
        class(p04_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat
        yp =  y*(20.0_rp-y)/80.0_rp
        stat = .true.
        return
    end subroutine p04_feval

    pure function p04_gettitle(this) result(title)
        implicit none
        class(p04_system), intent(in)  :: this
        character(len=:), allocatable  :: title
        title = this%title
        return
    end function p04_gettitle

    pure function p04_yinit(this) result(yinit)
        implicit none
        class(p04_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yinit(:)
        allocate(yinit(1))
        yinit = 1.0_rp
        return
    end function p04_yinit

    pure function p04_yend(this) result(yend)
        implicit none
        class(p04_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yend(:)
        allocate(yend(1))
        yend = 17.73016648131483_rp
        return
    end function p04_yend

    pure real(kind=rp) function p04_tinit(this) result(tinit)
        implicit none
        class(p04_system), intent(in)   :: this
        tinit = this%t_init
    end function p04_tinit

    pure real(kind=rp) function p04_tend(this) result(tend)
        implicit none
        class(p04_system), intent(in)   :: this
        tend = this%t_end
    end function p04_tend

    pure function p04_ysol(this,t) result(ysol)
        implicit none
        class(p04_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), allocatable      :: ysol(:)
        allocate(ysol(1))
        ysol = 20.0_rp / (1.0_rp+19.0_rp*exp(-0.25*t))
        return
    end function p04_ysol

    pure logical function p04_anltc(this) result(analiticsol)
        implicit none
        class(p04_system), intent(in)  :: this
        analiticsol = this%analitic_sol
    end function p04_anltc

    ! P05 Problem
    integer pure function p05_dim(this) result(n)
        implicit none
        class(p05_system), intent(in)   :: this
        n = this%neqn
    end function p05_dim

    pure subroutine p05_feval(this, t, y, yp, stat)
        implicit none
        class(p05_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat
        yp =  (y-t)/(y+t)
        stat = .true.
        return
    end subroutine p05_feval

    pure function p05_gettitle(this) result(title)
        implicit none
        class(p05_system), intent(in)  :: this
        character(len=:), allocatable  :: title
        title = this%title
        return
    end function p05_gettitle

    pure function p05_yinit(this) result(yinit)
        implicit none
        class(p05_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yinit(:)
        allocate(yinit(1))
        yinit = 4.0_rp
        return
    end function p05_yinit

    pure function p05_yend(this) result(yend)
        implicit none
        class(p05_system), intent(in)   :: this
        real(kind=rp), allocatable      :: yend(:)
        allocate(yend(1))
        yend = -0.788782668896419_rp
        return
    end function p05_yend

    pure real(kind=rp) function p05_tinit(this) result(tinit)
        implicit none
        class(p05_system), intent(in)   :: this
        tinit = this%t_init
    end function p05_tinit

    pure real(kind=rp) function p05_tend(this) result(tend)
        implicit none
        class(p05_system), intent(in)   :: this
        tend = this%t_end
    end function p05_tend

    pure function p05_ysol(this,t) result(ysol)
        implicit none
        class(p05_system), intent(in)   :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), allocatable      :: ysol(:)
        return
    end function p05_ysol

    pure logical function p05_anltc(this) result(analiticsol)
        implicit none
        class(p05_system), intent(in)  :: this
        analiticsol = this%analitic_sol
    end function p05_anltc

    ! Pleiades problem
    integer pure function plei_dim(this) result(n)
        implicit none
        class(plei_system), intent(in)   :: this
        n = this%neqn
    end function plei_dim

    pure subroutine plei_feval(this, t, y, yp, stat)
        implicit none
        class(plei_system), intent(in)  :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), intent(in)       :: y(:)
        real(kind=rp), intent(out)      :: yp(:)
        logical, intent(out)            :: stat

        integer                         :: i, j
        real(kind=rp)                   :: sumx, sumy, rij, rij32
        do i=1,7
            sumx = 0.0_rp
            sumy = 0.0_rp
            do j=1,7
                if (j /= i) then
                    rij = (y(i)-y(j))**2+(y(i+7)-y(j+7))**2
                    rij32 = sqrt(rij)*rij
                    sumx = sumx+real(j,kind=rp)*(y(j)-y(i))/rij32
                    sumy = sumy+real(j,kind=rp)*(y(j+7)-y(i+7))/rij32
                end if
            end do
            yp(i+14) = sumx
            yp(i+21) = sumy
        end do
        yp(1:14) = y(15:28)
        stat = .true.
        return
    end subroutine plei_feval

    pure function plei_gettitle(this) result(title)
        implicit none
        class(plei_system), intent(in) :: this
        character(len=:), allocatable  :: title
        title = this%title
        return
    end function plei_gettitle

    pure function plei_yinit(this) result(yinit)
        implicit none
        class(plei_system), intent(in)  :: this
        real(kind=rp), allocatable      :: yinit(:)

        allocate(yinit(28))
        yinit = [3.0_rp ,&
                 3.0_rp ,&
                -1.0_rp ,&
                -3.0_rp ,&
                 2.0_rp ,&
                -2.0_rp ,&
                 2.0_rp ,&
                 3.0_rp ,&
                -3.0_rp ,&
                 2.0_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                -4.0_rp ,&
                 4.0_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                 1.75_rp,&
                -1.5_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                 0.0_rp ,&
                -1.25_rp,&
                 1.0_rp ,&
                 0.0_rp ,&
                 0.0_rp]

        return
    end function plei_yinit

    pure function plei_yend(this) result(yend)
        implicit none
        class(plei_system), intent(in)  :: this
        real(kind=rp), allocatable      :: yend(:)

        allocate(yend(28))
        yend = [0.3706139143970502e+000_rp,&
                0.3237284092057233e+001_rp,&
               -0.3222559032418324e+001_rp,&
                0.6597091455775310e+000_rp,&
                0.3425581707156584e+000_rp,&
                0.1562172101400631e+001_rp,&
               -0.7003092922212495e+000_rp,&
               -0.3943437585517392e+001_rp,&
               -0.3271380973972550e+001_rp,&
                0.5225081843456543e+001_rp,&
               -0.2590612434977470e+001_rp,&
                0.1198213693392275e+001_rp,&
               -0.2429682344935824e+000_rp,&
                0.1091449240428980e+001_rp,&
                0.3417003806314313e+001_rp,&
                0.1354584501625501e+001_rp,&
               -0.2590065597810775e+001_rp,&
                0.2025053734714242e+001_rp,&
               -0.1155815100160448e+001_rp,&
               -0.8072988170223021e+000_rp,&
                0.5952396354208710e+000_rp,&
               -0.3741244961234010e+001_rp,&
                0.3773459685750630e+000_rp,&
                0.9386858869551073e+000_rp,&
                0.3667922227200571e+000_rp,&
               -0.3474046353808490e+000_rp,&
                0.2344915448180937e+001_rp,&
               -0.1947020434263292e+001_rp]
        return
    end function plei_yend

    pure real(kind=rp) function plei_tinit(this) result(tinit)
        implicit none
        class(plei_system), intent(in)  :: this
        tinit = this%t_init
    end function plei_tinit

    pure real(kind=rp) function plei_tend(this) result(tend)
        implicit none
        class(plei_system), intent(in)  :: this
        tend = this%t_end
    end function plei_tend

    pure function plei_ysol(this,t) result(ysol)
        implicit none
        class(plei_system), intent(in)  :: this
        real(kind=rp), intent(in)       :: t
        real(kind=rp), allocatable      :: ysol(:)
        return
    end function plei_ysol

    pure logical function plei_anltc(this) result(analiticsol)
        implicit none
        class(plei_system), intent(in)  :: this
        analiticsol = this%analitic_sol
    end function plei_anltc

end module problem_set
