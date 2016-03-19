#
# this Makefile should be OK for tcl/tk 8.5, python 2.7 using gcc/gfortran
# (tested on Ubuntu 12.04)
#
CC = gcc

FC = gfortran

all:	f_python f_tclsh f_wish f_demo

demo:	f_demo

f_demo:	mydemo.c Fortran_to_c_main.F90
	$(CC) -c -Dmain=MY_C_MAIN mydemo.c
	$(FC) -o f_demo Fortran_to_c_main.F90 mydemo.o
	rm -f mydemo.o

f_python : pythonAppInit.c Fortran_to_c_main.F90
	$(CC) -c -Dmain=MY_C_MAIN pythonAppInit.c
	$(FC) -o f_python Fortran_to_c_main.F90 pythonAppInit.o -lpython2.7
	rm -f pythonAppInit.o

f_tclsh : tclAppInit.c Fortran_to_c_main.F90
	$(CC) -c -Dmain=MY_C_MAIN tclAppInit.c -I/usr/include/tcl8.5
	$(FC) -o f_tclsh Fortran_to_c_main.F90 tclAppInit.o -ltcl8.5
	rm -f tclAppInit.o

f_wish : tkAppInit.c Fortran_to_c_main.F90
	$(CC) -c -Dmain=MY_C_MAIN tkAppInit.c -I/usr/include/tcl8.5
	$(FC) -o f_wish Fortran_to_c_main.F90 tkAppInit.o -ltk8.5  -ltcl8.5
	rm -f tkAppInit.o

clean:	
	rm -f *.o f_python f_tclsh f_wish f_demo *~
