#!/bin/bash
usage(){
  cat <<EOT
  usage : ${0} [--python|--tcl|--wish] [-h|--help|--demos]
          ${0} name|name.c options for Fortran compiler (s.f90)

  examples:
          ${0} mydemo.c -o mypython -lpython2.7
          (Ubuntu 14.04)
          compile mydemo.c, produce executable called mypython, use libpython2.7.so

          ${0} mydemo.c -o mywish -L/usr/lib/x86_64-linux-gnu -ltcl8.5 -ltk8.5
          (Ubuntu 14.04)
          compile mydemo.c, produce executable called mywish, use libtcl8.5.so and libtk8.5.so
EOT
}

demos(){
echo "================================ python ================================"
makepython | tee mypython.c
echo "================================   tcl  ================================"
maketclsh  | tee mytcl.c
echo "================================  wish  ================================"
makewish   | tee mywish.c
echo "================================        ================================"
cat << EODOC
!****P* FMAIN/f_main_to_c_main
! AUTHOR
!   M.Valin Recherche en Prevision Numerique 2015/2016
! SYNOPSIS
  fake fortran main program for use with C programs that call fortran routines that
  might require fortran runtime library initialization

  tested with gfortran, Intel fortran, Portland group fortran

  with Ubuntu 14.04
  ${0} --python    # create mypython
  ${0} --tcl       # create mytclsh
  ${0} --wish      # create mywish
!******
EODOC
}

makepython(){
cat <<EOT
main(int argc, char** argv)
{
  Py_Initialize();
  Py_Main(argc, argv);
  Py_Finalize();
}
EOT
}

makewish(){
cat <<EOT
#include <tcl.h>
#include <tk.h>
int main(
    int argc,                   /* Number of command-line arguments. */
    char **argv)                /* Values of command-line arguments. */
{
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;                   /* Needed only to prevent compiler warning. */
}

int Tcl_AppInit(interp)
    Tcl_Interp *interp;         /* Interpreter for application. */
{
    if (Tcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
    return TCL_OK;
}
EOT
}

maketclsh(){
cat <<EOT
#include <tcl.h>
int main(
    int argc,                   /* Number of command-line arguments. */
    char **argv)                /* Values of command-line arguments. */
{
    Tcl_Main(argc, argv, Tcl_AppInit);
    return 0;                   /* Needed only to prevent compiler warning. */
}
int Tcl_AppInit(interp)
    Tcl_Interp *interp;         /* Interpreter for application. */
{
    if (Tcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_SetVar(interp, "tcl_rcFileName", "~/tclshrc.tcl", TCL_GLOBAL_ONLY);
    return TCL_OK;
}
EOT
}

[[ "$1" == -*pyth* ]]  && makepython >mypython.c   && \
                          TO_REMOVE="mypython.c mypython.o" && \
                          set -- mypython.c -o mypython -lpython2.7
[[ "$1" == -*tcl* ]]   && maketclsh >mytclsh.c    && \
                          TO_REMOVE="mytclsh.c mytclsh.o"       && \
                          C_EXTRA="-I/usr/include/tcl8.5" &&\
                          set -- mytclsh.c -o mytclsh -ltcl8.5
[[ "$1" == -*wish* ]]  && makewish >mywish.c     && \
                          TO_REMOVE="mywish.c mywish.o"     && \
                          C_EXTRA="-I/usr/include/tcl8.5" &&\
                          set -- mywish.c -o mywish -ltcl8.5 -ltk8.5
[[ "$1" == -*dem* ]]   && demos  && exit 0
[[ "$1" == -*h* ]]     && usage  && exit 0
set -x
if [[ "$1" == *.c ]] ; then                 # compile the C main
  s.cc -c ${C_EXTRA} -Dmain=MY_C_MAIN ${1}
  Object=${1%.c}.o
  Name=MY_C_MAIN
else                                        # already compiled, main has name "Name"
  Object=""
  Name="$1"
fi
shift
cat <<EOC >program.f90
program fcmain
  use ISO_C_BINDING
  implicit none

  integer(C_INT) :: nargs
  integer :: i, length, status
  character(len=4096) :: argument
  character(len=1), dimension(:), pointer :: arg1
  type(C_PTR), dimension(:), pointer :: argv
  type(C_PTR) :: argtab
  interface
    function c_main(nargs,argv) result(status) BIND(C,name='${Name}')
    import
    implicit none
    integer, intent(IN), value :: nargs
    type(C_PTR), intent(IN), value :: argv
    integer :: status
    end function c_main
  end interface

  nargs = command_argument_count()
  allocate(argv(0:nargs+1))
  argv = C_NULL_PTR
  do i=0,nargs
    call get_command_argument(i,argument,length,status)
    allocate(arg1(length+1))
    arg1 = transfer(trim(argument)//achar(0),arg1,length+1)
    argv(i) = C_LOC(arg1(1))
  enddo
  argv(nargs+1) = C_NULL_PTR
  argtab = C_LOC(argv(0))
  status = c_main(nargs+1,argtab)
  stop
end
EOC
s.f90 program.f90 ${Object} "$@"
rm -f program.f90 ${TO_REMOVE}
