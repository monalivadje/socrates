SOCRATES (Suite Of Community RAdiative Transfer codes based on Edwards and Slingo)
-----------------------------------------------------------------------

Contents:


1) What's included?
2) Compiling the source code externally
3) Compilation of scripts in bin
4) Compilation of scripts in pyhton
5) Tested compilers


1. What's included? -


bin/ contains the wrapper code in Fortran 95 (.f90).

make/ contains the Makefile which then accesses the various Mk_*
files.

python/ contains python code which is interface to fortran using cffi and 
also .c header file



2. Compiling the source code externally -

For external users it should only be necessary to edit the file
make/Mk_cmd to allow compilation of the code on your system. FORTCOMP
and LINK can be changed to your local Fortran compiler. To use the netCDF
routines you must also change INCCDF_PATH and LIBCDF_PATH to point to
your local netCDF installation.

The following commands can then be run to build the suite and setup
your path to the executables and man pages:

./build_code

. ./set_rad_env



3. Compilation of scripts in bin -

There are a wrapper code in bin/ which are written 
in fortran and are compiled by.

gfortran -c -fPIC runes_driver.f90 -o runes_driver.o
gfortran -c -fPIC runes_driverwrap.f90 -o runes_driverwrap.o
ar -rcs libicore.a runes_driver.o runes_driverwrap.o




4. Compilation of scripts in python -


There are a python code in python/ which is interface to 
fortran using cffi and are compiled by.

python3 runes_driverpy.py

python3 runes_driverCffi.py



5. Tested compilers -


The full suite has been tested with the following compilers:

Intel ifort 17.0.7

GCC gfortran 8.1.0

To use these compilers within the Met Office run, respectively:
./build_code ifort

./build_code gfortran
