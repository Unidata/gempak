/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: ischar_.c,v 1.1 2000/07/12 13:12:24 gad Exp $ ****/
/* GAD moved over from mcidas for windco cal 7/6/00 */

/*
*$ Name:
*$      ischar  - Checks whether the 4 characters in the input field
*$                can be printed as ascii characters.
*$
*$ Interface:
*$      subroutine
*$      ischar(character*4 value)
*$
*$ Input:
*$      value   - Field containing four characters to be tested.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      1       - The values can be printed.
*$      0       - The values can not be printed.
*$
*$ Remarks:
*$      Important use info, algorithm, etc.
*$
*$ Categories:
*$      utility
*/

#include <ctype.h>

#include "mcidas.h"

/* Returns 1 if all four characters are ASCII printable, 0 otherwise */

Fint4
ischar_(void *value)
{
	const int *val;
	int i;

	val=(const int *)value;
	for (i = 0 ; i < 4; i++) {
		if (isprint(val[i]) == 0) {
			return(0);
		}
	}
	return(1);
}
