#include "geminc.h"
#include "gemprm.h"

void dm_rpkgc ( int *iflno, int *iiword, int *lendat, 
	        int *ipktyp, int *kxky, int *nbits, 
		float *ref, float *scale, int *miss, 
		float *difmin, int *kx, float *rdata, 
		int *iret )
/************************************************************************
 * dm_rpkgc                                                             *
 *                                                                      *
 * This is a subfunction to the DM library function DM_RPKG which reads	*
 * packed gridded data from a file and unpacks the data according to	*
 * the packing type.							*
 * This functionality has been isolated and written in 'C'		*
 * to allow for the dynamic allocation of buffer memory.		*
 *                                                                      *
 * dm_rpkgc ( iflno, iiword, lendat, ipktyp, kxky, nbits, ref, scale, 	*
 * 	      miss, difmin, kx, rdata, iret )				*
 *									*
 * Input parameters:                                                    *
 *      *iflno          int             File number (from FORTRAN)	*
 *      *iiword         int             Starting word			*
 *      *nword          int             Number of words			*
 *      *ipktyp         int             Packing type			*
 *      *kxky           int             Number of grid points		*
 *      *nbits          int             Number of bits			*
 *      *ref            float           Reference minimum value of grid	*
 *      *scale          float           Scaling factor			*
 *      *miss           int             Missing data flag		*
 *      *difmin         float           Minimum value of differences	*
 *      *kx             int             Number of points in x direction	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *rdata          float           Grid data			*
 *      *iret           int             Return code                     *
 *                                      = 0 - normal                    *
 *                                      = -31 - unknown packing type	*
 *                                      = -35 - problem w/ mem alloc	*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      9/06   Created                                 *
 ************************************************************************/
{
int	*ksgrid;
/*---------------------------------------------------------------------*/

    *iret = 0;

    G_MALLOC ( ksgrid, int, *lendat, "Error allocating ksgrid in dm_rpkgc" );

    if ( ksgrid == (int *)NULL )  {

	*iret = -35;

    }
    else  {

        /*
         * Read the packed data from the file.
         */
        dm_rint  ( iflno, iiword, lendat, ksgrid, iret );

        /*
         * Unpack the data according to type.
         */
        if  ( *ipktyp == MDGGRB )  {
	    dp_ugrb  ( ksgrid, kxky, nbits, ref, scale, miss, rdata, iret );
        }
        else if  ( *ipktyp == MDGNMC )  {
    	    dp_unmc  ( ksgrid, kxky, nbits, ref, scale, rdata, iret );
        }
        else if  ( *ipktyp == MDGDIF )  {
	    dp_udif  ( ksgrid, kxky, nbits, ref, difmin, scale, 
		       miss, kx, rdata, iret );
        }
        else  {
	    *iret = -31;
        }

        G_FREE ( ksgrid, int );

    }

}
