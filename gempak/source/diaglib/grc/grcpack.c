#include "geminc.h"
#include "gemprm.h"

void grc_pack  ( const char *gpack, int *ipktyp, int *nbits, int *iret )
/************************************************************************
 * GR_PACK								*
 *									*
 * This subroutine decodes the user input for grid packing into the	*
 * packing type and number of bits / precision.  The valid packing	*
 * types are GRIB, DEC, DIF and NONE.  If the packing type is DEC,	*
 * NBITS is the precision; otherwise, NBITS is the number of bits.	*
 *									*
 * GRIB packing is the default with 16 bits.				*
 *									*
 * Default values:							*
 *		Packing type	Number of bits				*
 *		    GRIB	      16				*
 *		    DEC		       2 (decimal places)		*
 *		    DIF 	      16				*
 *		    NONE	      32				*
 *									*
 * GR_PACK  ( GPACK, IPKTYP, NBITS, IRET )				*
 *									*
 * Input parameters:							*
 *	GPACK		CHAR*		Packing type / number of bits	*
 *									*
 * Output parameters:							*
 *	IPKTYP		INTEGER		GEMPAK packing type		*
 *	NBITS		INTEGER		Number of bits			*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 3/89						*
 * S. Jacobs/EAI	 3/93		Specify ranges for nbits for 	*
 *					  different pack types		*
 * D.W.Plummer/NCEP      2/06           Translated from FORTRAN         *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_pack ( (char *)gpack, ipktyp, nbits, iret, strlen(gpack) );

	return;
}
