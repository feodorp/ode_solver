include_directories(${ODESOLVERINCLUDE})

set(SRC
    kinds.f90
    utils/messagehandler.f90
    messages.f90
    step_control.f90
    system.f90
    step_proto.f90
    steppers/rkf45.f90
    steps.f90
    solver.f90
    ode.f90)

add_library(odesolver ${SRC})

