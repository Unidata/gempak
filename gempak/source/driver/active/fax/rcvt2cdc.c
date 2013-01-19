#include "faxcmn.h"

int rcvt2cdc ( char chasc )
/************************************************************************
 * rcvt2cdc 								*
 *									*
 * This function converts an ASCII character to the CDC format		*
 * for inclusion in the header of 6-bit fax products.			*
 *									*
 * The Extended CDC Display codes are as follows:			*
 *									*
 *   ASCII Code	   CDC Code		ASCII Code   CDC Code		*
 *      A  a		01		   S  s		13		*
 *	B  b		02		   T  t		14		*
 * 	C  c		03		   U  u		15		*
 * 	D  d		04		   V  v		16 		*
 *	E  e		05		   W  w		17		*
 *	F  f		06		   X  x		18		*
 *	G  g		07		   Y  y		19		*
 * 	H  h		08		   Z  z		1A		*
 * 	I  i		09		   0		1B		*
 *	J  j		0A		   1		1C		*
 * 	K  k		0B		   2		1D		*
 *	L  l		0C		   3		1E		*
 * 	M  m		0D		   4		1F		*
 *	N  n		0E		   5		20		*
 *	O  o		0F		   6		21		*
 *	P  p		10		   7		22		*
 *	Q  q		11		   8		23		*
 *	R  r		12		   9		24		*
 *					Blank		2D		*
 *									*
 * int rcvt2cdc ( chasc )						*
 * 									*
 * Input parameters:							*
 *	chasc		char		Ascii code to be converted	*
 *									*
 * Output parameters:							*
 *	rcvt2cdc	int		Extended CDC Display code	*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created.				*
 * S. Jacobs/NCEP	 7/97	Updated header; Added faxcmn.h		*
 * S. Jacobs/NCEP	 7/79	Updated header; Reformatted if check	*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 ***********************************************************************/
{

	char	cdc_ch;

/*---------------------------------------------------------------------*/

	if  ( ( chasc >= 'A' ) && ( chasc <= 'Z' ) )  {
	    cdc_ch = chasc - 'A' + 1;
	}

	else if ( ( chasc >= 'a' ) && ( chasc <= 'z' ) )  {
	    cdc_ch = chasc - 'a' + 1;
	}

	else if ( ( chasc >= '0' ) && ( chasc <= '9' ) )  {
	    cdc_ch = chasc - '0' + 0x1b;
	}

	else  {
	    cdc_ch = 0x2d;
	}

	return ( cdc_ch );
        
}
