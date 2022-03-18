include_directories(${ODESOLVERINCLUDE})

set(SRC
    kinds.f90
    flags.f90
    messagehandler.f90
    rkf45_solver.f90)

add_library(odesolver ${SRC})

