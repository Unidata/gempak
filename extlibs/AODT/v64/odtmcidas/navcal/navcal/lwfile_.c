/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 *
 *** $Id: lwfile_.c,v 1.2 2000/08/18 22:42:01 gad Exp $ ***
*/

/* GAD stub for windco cal routines 7/6/00  */

#include <stdio.h>
#include "mcidas.h"

/*
*$ Name:
*$      lwfile - Checks to see that a file exists.
*$
*$ Interface:
*$      integer function
*$      lwfile(character*(*) c)
*$
*$ Input:
*$      c      - Name of the file being investigated.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       1     - The file exists.
*$       0     - The file does not exist.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      file
*/

Fint
lwfile_(const char *file, FsLen i)
{
/*	printf("\nCall to lwfile to check existence of %s",file); */
	return 1;
}
