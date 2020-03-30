#
# this Makefile should be OK for tcl/tk 8.5, python 2.7 using gcc/gfortran
# (tested on Ubuntu 12.04)
#
CCOMP = gcc

FCOMP = gfortran

MPIFC = mpif90

FFLAGS = -O2

CFLAGS = -O2

all:	f_python f_tclsh f_wish f_demo libompstubs.a

demo:	f_demo

libompstubs.a: openmp_stubs_c.c openmp_stubs_f.F90
	$(CCOMP) -c -fPIC openmp_stubs_c.c
	$(FCOMP) -c -fPIC openmp_stubs_f.F90
	ar rcv libompstubs.a openmp_stubs_c.o openmp_stubs_f.o

f_demo:	mydemo.c Fortran_to_c_main.F90
	$(CCOMP) -c -Dmain=MY_C_MAIN mydemo.c
	$(FCOMP) -o f_demo Fortran_to_c_main.F90 mydemo.o
	rm -f mydemo.o

f_python : pythonAppInit.c Fortran_to_c_main.F90
	$(CCOMP) -c -Dmain=MY_C_MAIN pythonAppInit.c
	$(FCOMP) -o f_python Fortran_to_c_main.F90 pythonAppInit.o -lpython2.7
	rm -f pythonAppInit.o

f_tclsh : tclAppInit.c Fortran_to_c_main.F90
	$(CCOMP) -c -Dmain=MY_C_MAIN tclAppInit.c -I/usr/include/tcl8.5
	$(FCOMP) -o f_tclsh Fortran_to_c_main.F90 tclAppInit.o -ltcl8.5
	rm -f tclAppInit.o

f_wish : tkAppInit.c Fortran_to_c_main.F90
	$(CCOMP) -c -Dmain=MY_C_MAIN tkAppInit.c -I/usr/include/tcl8.5
	$(FCOMP) -o f_wish Fortran_to_c_main.F90 tkAppInit.o -ltk8.5  -ltcl8.5
	rm -f tkAppInit.o

trace_test_mpi: time_trace_c.c time_trace.F90 time_trace.h
	$(CCOMP) $(CFLAGS) -I. -c -DSELF_TEST time_trace_c.c
	$(MPIFC) $(FFLAGS) -DSELF_TEST time_trace.F90 time_trace_c.o -o trace_test_mpi

trace_test_f: time_trace_c.c time_trace.F90 time_trace.h
	$(CCOMP) $(CFLAGS) -I. -c -DSELF_TEST time_trace_c.c
	$(FCOMP) $(FFLAGS) -I. -DSELF_TEST -DNO_MPI time_trace.F90 time_trace_c.o -o trace_test_f

trace_test_c: time_trace_c.c time_trace.h
	$(CCOMP) $(CFLAGS) -I. -DSELF_TEST -DC_SELF_TEST time_trace_c.c -o trace_test_c

libtimetrace.a: time_trace_c.c time_trace.h time_trace.F90
	$(CCOMP) $(CFLAGS) -I. -c time_trace_c.c
	$(FCOMP) $(FFLAGS) -I. -c time_trace.F90
	ar rcv libtimetrace.a time_trace_c.o time_trace.o

clean:	
	rm -f *.o *.mod a.out f_python f_tclsh f_wish f_demo *~  *.s
	rm -f libompstubs.a time_list_0*.txt time_list_0*.dat trace_test_c trace_test_f trace_test_mpi
