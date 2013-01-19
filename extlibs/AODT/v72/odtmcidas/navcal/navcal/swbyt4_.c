/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: swbyt4_.c,v 1.1 2000/07/12 13:12:27 gad Exp $ ****/
/* GAD moved over from mcidas for windco cal */

#include "mcidas.h"

/*
*$ Name:
*$      swbyt4   - Re-orders bytes if internal representation is not
*$                 big-endian.
*$
*$ Interface:
*$      subroutine
*$      swbyt4(integer buf(*), integer n)
*$
*$ Input:
*$      n        - Number of 4 byte swaps to be made if order is not
*$                 big-endian.
*$
*$ Input and Output:
*$      buf      - Array containing bytes to be swapped.
*$
*$ Output:
*$      none
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

void
swbyt4_(void *buf, Fint *n)
{
	/* determine byte order from first principles */
	union
	{
		char            bytes[sizeof(Mcint4)];
		Mcint4          word;
	}               q;

	q.word = 1;
	if (q.bytes[3] == 1)
		return;

	/* swap bytes */
	fbyte4_(buf, n);
}
