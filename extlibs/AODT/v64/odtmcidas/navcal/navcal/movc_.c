/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: movc_.c,v 1.1 2000/06/26 15:51:48 daves Exp $ ****/

#include <string.h>

# include "mcidas.h"

/*
*$ Name:
*$      movc    - Moves bytes from one array to another.
*$
*$ Interface:
*$      subroutine
*$      movc(integer num, integer inbuf(*), integer soff, integer outbuf(*),
*$           integer doff)
*$
*$ Input:
*$      num     - Number of bytes to move.
*$      inbuf   - Input array.
*$      soff    - Byte offset in input buffer; zero based.
*$      doff    - Byte offset in output buffer; zero based.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      outbuf  - Output array.
*$
*$ Return values:
*$       none
*$
*$ Remarks:
*$      If the number of bytes to move is less than 1 no bytes are
*$      transferred.
*$
*$ Categories:
*$      utility
*/

void
movc_(Fint4 *num, unsigned char inbuf[], Fint4 *soff, unsigned char outbuf[], Fint4 *doff)
{
     memcpy( &outbuf[*doff] , &inbuf[*soff], *num);
}

