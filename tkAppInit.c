/*
 * modified/stripped version of tkAppInit.c --
 * simplified version to be called by a Fortran main
 *
 *	Provides a default version of the main program and Tcl_AppInit
 *	procedure for wish and other Tk-based applications.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1998-1999 Scriptics Corporation.
 *
 * See the file "license.terms" for information on usage and redistribution of
 * this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#undef BUILD_tk
#undef STATIC_BUILD
#include "tk.h"

extern int Tcl_AppInit(Tcl_Interp *);
extern int main(int, char **);

int main(
    int argc,			/* Number of command-line arguments. */
    char **argv)		/* Values of command-line arguments. */
{
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}
int Tcl_AppInit(Tcl_Interp *interp)		/* Interpreter for application. */
{
    if ((Tcl_Init)(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

    Tcl_ObjSetVar2(interp, Tcl_NewStringObj("tcl_rcFileName", -1), NULL,
	    Tcl_NewStringObj("~/.wishrc", -1), TCL_GLOBAL_ONLY);
    return TCL_OK;
}
