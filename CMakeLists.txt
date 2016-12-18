project (fpgf)

cmake_minimum_required(VERSION 3.3)
enable_language (Fortran)
if( NOT CMAKE_BUILD_TYPE )
    set( CMAKE_BUILD_TYPE Release )
endif()
message(STATUS "Build type: " ${CMAKE_BUILD_TYPE})
set(CMAKE_Fortran_FLAGS_RELEASE
    "-cpp -fopenmp -O2 -march=native")
set(CMAKE_Fortran_FLAGS_DEBUG
    "-cpp -g -Wall -Wextra -Warray-temporaries -Wconversion -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})
add_subdirectory(src)
add_subdirectory(examples)
