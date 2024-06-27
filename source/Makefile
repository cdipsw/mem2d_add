#Makefile for netcdf4
PROG = mem2d_add

.KEEP_STATE:

NC_MODS = /usr/include
NC_LIBS = /usr/lib
CDIP_MODS = ../f90_modules
LIBS = -L${NC_LIBS} -L/usr/lib/x86_64-linux-gnu -Wl,-rpath,${NC_LIBS} ${CDIP_MODS}/libproc_gf.a -lcurl -lnetcdff -lnetcdf

FFLAGS = ${EXTRAFLAGS} -ffixed-line-length-0 -c -g -m64 -fPIC -I${NC_MODS} -I${CDIP_MODS} -I/usr/include/x86_64-linux-gnu/curl -fbounds-check
FLAGS = ${EXTRAFLAGS} -ffixed-line-length-0 -g -m64 -fPIC -fbounds-check

OBJ = 	${PROG}.o

${PROG}:	${OBJ}
	gfortran ${FLAGS} -o ${PROG} ${OBJ} ${LIBS} -I${NC_MODS} -I${CDIP_MODS}

.f.o:
	gfortran ${FFLAGS} $<

clean:
	rm ${OBJ}
