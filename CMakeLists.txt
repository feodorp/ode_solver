cmake_minimum_required(VERSION 2.9)


set(CMAKE_Fortran_COMPILER_NAMES gfortran ifort)
enable_language(Fortran)

project(ode_solver)
set(VERSION 1.0)


# compiler options
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # gfortran
    set(common "-Wall -Wextra -Wimplicit-interface -fPIC -ffree-line-length-none")
    set(CMAKE_Fortran_FLAGS_RELEASE "${common} -cpp -O3 -flto -march=native -ffast-math -funroll-loops")
    set(CMAKE_Fortran_FLAGS_DEBUG "${common} -cpp -g -fcheck=all -fbacktrace")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    # ifort
    set(common "-warn all")
    set(CMAKE_Fortran_FLAGS_RELEASE "${common} -fpp -ipo -xSSSE3 -O3")
    set(CMAKE_Fortran_FLAGS_DEBUG "${common} -fpp -check all -g")
endif ()

# Make sure that CMAKE_BUILD_TYPE is either Debug or Release:
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release
        CACHE STRING "Build type (Debug, Release)" FORCE)
endif ()
if (NOT (CMAKE_BUILD_TYPE STREQUAL "Debug" OR
         CMAKE_BUILD_TYPE STREQUAL "Release"))
    message("${CMAKE_BUILD_TYPE}")
    message(FATAL_ERROR "CMAKE_BUILD_TYPE must be one of: Debug, Release (current value: '${CMAKE_BUILD_TYPE}')")
endif ()

# Define some directories
set(ODESOLVERINCLUDE ${CMAKE_BINARY_DIR}/include)
set(ODESOLVERLIB ${CMAKE_BINARY_DIR}/lib)

# .mod files placed in the include folder
set(CMAKE_Fortran_MODULE_DIRECTORY ${ODESOLVERINCLUDE})

enable_testing()

add_subdirectory(src)
add_subdirectory(test)
