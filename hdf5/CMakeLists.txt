cmake_minimum_required(VERSION 3.13)

project(Training_HDF5 Fortran)

set ( CMAKE_CONFIGURATION_TYPES "Debug" "Release" "CodeCoverage" "Fast" )

set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}" )

if(NOT CMAKE_BUILD_TYPE)
  set( CMAKE_BUILD_TYPE "Release" )
  message(STATUS "No CMAKE_BUILD_TYPE selected, default to ${CMAKE_BUILD_TYPE}")
endif()

option( OpenMP "Enable OpenMP" OFF )
if( OpenMP )
  find_package( OpenMP REQUIRED )
endif()

if( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )

  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wimplicit-procedure -Wunused-parameter -std=f2018" )
  set( CMAKE_Fortran_FLAGS_FAST "-Ofast -flto" )
  set( CMAKE_Fortran_FLAGS_RELEASE "-O3 -flto" )
  set( CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fcheck=all -ffpe-trap=zero,overflow,invalid -fbacktrace" )
  set ( CMAKE_Fortran_FLAGS_CODECOVERAGE "--coverage -g -O0" )

elseif( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )

  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -stand f18 -standard-semantics -assume nostd_mod_proc_name" )
  set( CMAKE_Fortran_FLAGS_FAST "-Ofast -ipo" )
  set( CMAKE_Fortran_FLAGS_RELEASE "-O3 -ipo" )
  set( CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -check all -fpe-all=0 -traceback" )
  set ( CMAKE_Fortran_FLAGS_CODECOVERAGE "-prof-gen:srcpos -prof-dir=${CMAKE_BINARY_DIR}/prof/ -g -O0" )

elseif( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Flang" )

  set( CMAKE_Fortran_FLAGS_FAST "${CMAKE_Fortran_FLAGS} -Ofast" )
  set( CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -O3" )
  set( CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -O0 -g -Wall" )

elseif( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "PGI" )

  set( CMAKE_Fortran_FLAGS_FAST "${CMAKE_Fortran_FLAGS} -Mallocatable=03 -fast" )
  set( CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -Mallocatable=03 -O3" )
  set( CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -Minform=inform -Mallocatable=03 -Mstandard -O0 -g -C" )

endif()

find_package( HDF5 REQUIRED COMPONENTS Fortran )

add_executable( extract_hgt_gedi "extract_hgt_gedi.f90" )
target_link_libraries( extract_hgt_gedi ${HDF5_LIBRARIES} )
target_include_directories( extract_hgt_gedi PRIVATE ${HDF5_INCLUDE_DIRS}/shared )
