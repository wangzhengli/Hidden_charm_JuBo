# define the compilers for current computer

# ubuntu with ifort
#LIB = -Lminuit -lminuit
##Lib = -lpacklib
#FC = ifort
#F90 = ifort
##FFLAGS = -O3 -xssse3 -fast-transcendentals -no-prec-div -no-prec-sqrt
##FFLAGS = -O3 -xssse3 -ipo -fast-transcendentals -no-prec-div -no-prec-sqrt
#FFLAGS = -no-ipo -no-vec
#ARCH = ar
#ARCHFLAGS = cr
#RANLIB = ranlib

# ubuntu with mpif90
#LIB =
#FC = mpif90
#F90 = mpif90
#FFLAGS = -O3 -ffree-line-length-none -mcmodel=large
#ARCH = ar
#ARCHFLAGS = cr
#RANLIB = ranlib

#-march=native -ffast-math -ffree-form -ftree-vectorize -funroll-loops

# JUROPA
#LIB =
#FC = mpif90
#F90 = mpif90
#FFLAGS = -O2 -no-ipo -no-vec
#ARCH     = ar
#ARCHFLAGS= cr
#RANLIB   = ranlib

# JUMP
#LIB =
#FC = mpxlf_r -qfixed=72
#F90 = mpxlf_r -qfree=f90
#FFLAGS = -q64 -O3 -qipa=level=1  -qstrict
#ARCH     = ar
#ARCHFLAGS= -c -r -X64
#RANLIB   = ranlib

# JUGENE
#FC = mpixlf95_r -qfixed=72
#F90 = mpixlf95_r -qfree=f90
#FFLAGS = -O3 -qhot -qstrict -qarch=450 -qtune=450
#ARCH     = ar
#ARCHFLAGS= cr
#RANLIB   = ranlib
#LIB = -L/bgsys/local/lib -llapack -lesslbg

# single node
#FC = xlf95_r -qfixed=72
#F90 = xlf95_r -qfree=f90
#FFLAGS = -O4 -qhot -qipa=level=1  -qstrict # -qarch=450 -qtune=450
#ARCH     = ar
#ARCHFLAGS= cr
#RANLIB   = ranlib
#LIB =

# Local with gfortran (not parallel)
#LIB = -Lminuit -lminuit
#Lib = -lpacklib
FC = gfortran
F90 = gfortran
#FC = ifort
#F90 = ifort
#FFLAGS = -O3 -xssse3 -fast-transcendentals -no-prec-div -no-prec-sqrt
#FFLAGS = -O3 -xssse3 -ipo -fast-transcendentals -no-prec-div -no-prec-sqrt
#FFLAGS = -no-ipo -no-vec
#FFLAGS = -ffixed-line-length-none
FFLAGS = -O3 -mcmodel=large -ffree-line-length-none
#FFLAGS = -ffree-line-length-none
ARCH = ar
ARCHFLAGS = cr
RANLIB = ranlib
