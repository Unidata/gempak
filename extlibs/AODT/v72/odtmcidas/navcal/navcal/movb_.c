/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: movb_.c,v 1.1 2000/06/26 15:51:48 daves Exp $ ****/

#include <string.h>

#include "mcidas.h"
/*
*$ Name:
*$      movb      - Moves bytes from one array to another array with a
*$                  destination byte offset.
*$
*$ Interface:
*$      subroutine
*$      movb(integer num, integer inbuffer(*), integer outbuffer(*),
*$           integer offset)
*$
*$ Input:
*$      num       - Number of bytes to move.
*$      inbuffer  - Input array.
*$      offset    - Byte offset in output buffer, zero based.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      outbuffer  - Output array.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      If the number of bytes to move is less than 1 no bytes are
*$      transferred.
*$
*$ Categories:
*$      utility
*/

void
movb_(Fint *num, void *inbuffer, void *outbuffer, Fint *offset)
{
	Fint            off = *offset;
	unsigned char  *outbuf = outbuffer;


	memcpy(&outbuf[off], inbuffer, *num);
}
