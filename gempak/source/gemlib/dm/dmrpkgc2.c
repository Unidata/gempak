#include "geminc.h"
#include "gemprm.h"

void dm_rpkgc2 ( int *iflno, int *iiword, int *lendat, 
	        int *iarray, float *rarray, int *kxky, 
		float *rdata, int *iret )
/************************************************************************
 * dm_rpkgc2                                                            *
 *                                                                      *
 * This is a subfunction to the DM library function DM_RPKG which reads	*
 * packed gridded data from a file and unpacks the data according to	*
 * the GRIB2 packing type.						*
 * This functionality has been isolated and written in 'C'		*
 * to allow for the dynamic allocation of buffer memory.		*
 *                                                                      *
 * dm_rpkgc2 ( iflno, iiword, lendat, iarray, rarray, kxky, 	 	*
 * 	      rdata, iret )						*
 *									*
 * Input parameters:                                                    *
 *      *iflno          int             File number (from FORTRAN)	*
 *      *iiword         int             Starting word			*
 *      *lendat         int             Number of words			*
 *      *iarray         int             int type packing information	*
 *      *rarray         float           float type packing information	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *kxky           int             Number of grid points unpacked	*
 *      *rdata          float           Grid data			*
 *      *iret           int             Return code                     *
 *                                      = 0 - normal                    *
 *                                      = -31 - unknown packing type	*
 *                                      = -35 - problem w/ mem alloc	*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      9/06   Created                                 *
 * S. Chiswell/Unidata	12/06	Modified from DM_RPKGC			*
 ************************************************************************/
{
int	*ksgrid;
/*---------------------------------------------------------------------*/

    *iret = 0;

    G_MALLOC ( ksgrid, int, *lendat, "Error allocating ksgrid in dm_rpkgc2" );

    if ( ksgrid == (int *)NULL )  {

	*iret = -35;

    }
    else  {

        /*
         * Read the packed data from the file.
         */
        dm_rint  ( iflno, iiword, lendat, ksgrid, iret );

        /*
         * Unpack the data
         */
	dp_ugb2  ( ksgrid, iarray, rarray, kxky, rdata, iret );

        G_FREE ( ksgrid, int );

    }

}
