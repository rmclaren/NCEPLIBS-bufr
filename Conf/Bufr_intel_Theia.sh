# *** for Theia (intel) ***
 WORKDIR=/scratch3/NCEPDEV/nwprod/IPS
 MODULEPATH=/apps/lmod/lmod/modulefiles/Core
 module use -a $WORKDIR/modulefiles/core_third
 module load EnvVars/1.0.2
 module load ips/18.0.1.163
 module load dev/bufr/11.3.0

 export CC=icc
 export FC=ifort
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -traceback -O0"
 export CFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export FFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export FPPCPP="-cpp"
 export FREEFORM="-free"
 export MPICFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export MPIFFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CFLAGSDEFS="-DUNDERSCORE"
 export CF77INTSIZE="-DF77_INTSIZE_8"
 export FFLAGSDEFS=""

 export USECC="YES"
 export USEFC="YES"
 export DEPS=""
