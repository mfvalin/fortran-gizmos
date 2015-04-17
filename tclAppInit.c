/*
 * modified/stripped version of tclAppInit.c --
 * simplified version to be called by a Fortran main
 *
 *	Provides a default version of the main program and Tcl_AppInit
 *	procedure for tclsh and other Tcl-based applications (without Tk).
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1998-1999 Scriptics Corporation.
 *
 * See the file "license.terms" for information on usage and redistribution of
 * this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#undef BUILD_tcl
#undef STATIC_BUILD
#include "tcl.h"

extern int Tcl_AppInit(Tcl_Interp *);
extern int main(int, char **);

int main(
    int argc,			/* Number of command-line arguments. */
    char *argv[])		/* Values of command-line arguments. */
{
    Tcl_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}
int Tcl_AppInit(Tcl_Interp *interp)		/* Interpreter for application. */
{
    if ((Tcl_Init)(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    (Tcl_ObjSetVar2)(interp, Tcl_NewStringObj("tcl_rcFileName", -1), NULL,
	    Tcl_NewStringObj("~/.tclshrc", -1), TCL_GLOBAL_ONLY);

    return TCL_OK;
}
