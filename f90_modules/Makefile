#/project/wvutil/f90_modules/Makefile
#Makefile for modules and processing library:libproc.a

#.KEEP_STATE:

NC_DIR = /usr/include
NC_MOD = ${NC_DIR}/netcdf.mod
LIB_NAME = libproc_gf.a
CURL_DIR = /usr/lib/x86_64-linux-gnu

FFLAGS = ${EXTRAFLAGS} -ffixed-line-length-0 -c -g -m64 -fPIC -mcmodel=medium

MODULES = 	file_ops.mod \
		unit_conversions.mod \
		strings.mod \
		dates.mod \
		json_parser.mod \
		wmo_utils.mod \
		locations.mod \
		ascii_tags.mod \
		proc_streams.mod \
		archive_info.mod \
		gudb_lookup_utils.mod \
		gudb_objects.mod \
		deploy_info.mod \
		gudb_deploy_info.mod \
		error_utils.mod \
		misc_utils.mod \
		lookup_utils.mod \
		datawell_vectors.mod \
		datawell_utils.mod \
		spectral.mod \
		mem_ds.mod \
		xml_elements.mod \
		xml_wave_data.mod \
		mop_utils.mod \
		netcdf_utils.mod \
		metadata_utils.mod \
		gudb_metadata_utils.mod \
		ioc_flags.mod \
		wavecdf5_data.mod \
		wavecdf5_utils.mod \
		wc5_mem_utils.mod \


OBJ =	file_ops.o \
	unit_conversions.o \
	strings.o \
	dates.o \
	json_parser.o \
	wmo_utils.o \
	locations.o \
	ascii_tags.o \
	proc_streams.o \
	archive_info.o \
	gudb_lookup_utils.o \
	gudb_objects.o \
	deploy_info.o \
	gudb_deploy_info.o \
	error_utils.o \
	misc_utils.o \
	lookup_utils.o \
	datawell_vectors.o \
	datawell_utils.o \
	spectral.o \
	mem_ds.o \
	xml_elements.o \
	xml_wave_data.o \
	mop_utils.o \
	netcdf_utils.o \
	metadata_utils.o \
	gudb_metadata_utils.o \
	ioc_flags.o \
	wavecdf5_data.o \
	wavecdf5_utils.o \
	wc5_mem_utils.o \
	gudb_rest_request_.o

%.o:	%.mod

.f.o:
	gfortran ${FFLAGS} $<

netcdf_utils.o: ./netcdf_utils.f
	gfortran ${FFLAGS} -I${NC_DIR} ./netcdf_utils.f

ioc_flags.o: ./ioc_flags.f
	gfortran ${FFLAGS} -I${NC_DIR} ./ioc_flags.f

wavecdf5_data.o: ./wavecdf5_data.f
	gfortran ${FFLAGS} -I${NC_DIR} ./wavecdf5_data.f

gudb_rest_request_.o:	gudb_rest_request_.c
	gcc ${FFLAGS} -c -I${CURL_DIR} gudb_rest_request_.c

all:	${OBJ}
	ar r ${LIB_NAME} ${OBJ} ${MODULES} ${NC_MOD}

clean:
	rm ${OBJ} ${MODULES} ${LIB_NAME}
