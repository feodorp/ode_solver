include_directories(${ODESOLVERINCLUDE})

set(SRC
    problem_set2.f90)

add_library(odetestprob ${SRC})

add_executable(ode_test ode_test.f90)

target_link_libraries(ode_test odesolver odetestprob)

add_test(ode_test "./ode_test")

