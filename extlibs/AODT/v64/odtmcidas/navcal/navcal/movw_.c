/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: movw_.c,v 1.1 2000/07/12 13:12:26 gad Exp $ ****/
/* GAD moved over from mcidas for windco cal 7/6/00 */

#include <string.h>

#include "mcidas.h"
/*
*$ Name:
*$      movw    - Moves 4 byte words from one array to another array.
*$
*$ Interface:
*$      subroutine
*$      movw(integer num, integer inbuf(*), integer outbuf(*))
*$
*$ Input:
*$      num     - Number of words to move.
*$      inbuf   - Input array.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      outbuf  - Output array.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      If the number of bytes to move is less than 1 then no
*$      words are transferred.
*$
*$ Categories:
*$      utility
*/

void
movw_(Fint *num, void *inbuf, void *outbuf)
{
	int i;

	i = 4 * (*num);
	memcpy(outbuf, inbuf, 4 * (*num));
}
