/************************************************************************
 * BRIDGE.H								*
 *									*
 * This file contains parameters defined for use in the decoder bridge	*
 * routines.								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/95						*
 * A. Hardy/GSC		 1/01	Added proto_bridge.h			*
 * S. Jacobs/NCEP	12/02	Increased DCMXBF from 16kb to 100kb	*
 * B. Hebbard/NCEP       9/21   Increased DCMXBF from 100kB to 5MB      *
 ***********************************************************************/

#define DCDFTM  (   600 )	/* Default timeout */

#define DCMXLN  (   133 )	/* Max length for a character string */

#define DCMXBF  ( 5000000 )	/* Max length of the data buffer */

#define EOD     (  0xfe )	/* End of data marker */

/*---------------------------------------------------------------------*/

/* Error list */

#define EBNMEM  (    -4 )	/* No memory for malloc */

#define EBREAD  (    -5 )	/* Error reading into buffer */

#define EBTMOUT (    -6 )	/* Time out has occurred */

#define EBFULL  (    -7 )	/* The buffer is full */

#define EBEMPT  (    -8 )	/* The buffer is empty */

#define EBEND   (    -9 )	/* End of data input */

/*---------------------------------------------------------------------*/
#include "proto_bridge.h"
