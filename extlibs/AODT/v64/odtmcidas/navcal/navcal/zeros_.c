/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */
/**** $Id: zeros_.c,v 1.11 1997/10/10 20:19:07 dglo Exp $ ****/

/*
*$ Name:
*$      zeros    - Places zeroes in a byte array.
*$
*$ Interface:
*$      subroutine
*$      zeros(integer buf(*), integer bytes))
*$
*$ Input:
*$      bytes    - The number of bytes that are zero-filled.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      buf      - Array that is zero filled.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      utility
*/

#include <string.h>
#include "mcidas.h"

void
zeros_(void *buf, Fint4 *nbytes)
{
        static unsigned char zip = 0;
        memset(buf, zip, *nbytes);
}

