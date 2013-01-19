#include "geminc.h"
#include "gemprm.h"

static	int	*_ksGrid;

/************************************************************************
 * dm_wdtrc                                                             *
 *                                                                      *
 * This module contains two functions in support of the DM library	*
 * function DM_WDTR. It is written in 'C' to allow dynamic allocation	*
 * of a temporary buffer.						*
 * The first function, dm_pkgdc, packs the data into an integer array	*
 * (the temp buffer). The buffer, local global variable '_ksGrid', is 	*
 * allocated in this function.						*
 * The second function, dm_wpkgc, writes the data to the file. The 	*
 * buffer, local global variable '_ksGrid', is freed at the end of this	*
 * function.								*
 ************************************************************************/

void dm_pkgdc ( float *rdata, int *nword, int *kword, int *ipktyp,
		int *nbits, int *misflg, int *kxky, int *kx,
		float *ref, float *scale, float *difmin, int *iret )
/************************************************************************
 * dm_pkgdc                                                             *
 *                                                                      *
 * This is a subfunction to the DM library function DM_WDTR.		*
 * It packs the data into an integer array stored locally until it 	*
 * is written by the sister routine 'dm_wpkgc'.				*
 * This functionality has been isolated and written in 'C'		*
 * to allow for the dynamic allocation of buffer memory.		*
 *                                                                      *
 * dm_pkgdc ( rdata, kword, ipktyp, nbits, misflg, kxky, kx,		*
 * 	      ref, scale, difmin, iret )				*
 *									*
 * Input parameters:                                                    *
 *      *rdata          float           Grid data			*
 *      *nword          int             Number of words	of gridded data	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *kword          int             Number of words	to write	*
 *      *ipktyp         int             Packing type			*
 *      *nbits          int             Number of bits			*
 *      *miss           int             Missing data flag		*
 *      *kxky           int             Number of grid points		*
 *      *kx             int             Number of points in x direction	*
 *      *ref            float           Reference minimum value of grid	*
 *      *scale          float           Scaling factor			*
 *      *difmin         float           Minimum value of differences	*
 *      *iret           int             Return code                     *
 *                                      = 0 - normal                    *
 *                                      = -31 - unknown packing type	*
 *                                      = -35 - problems w/ mem alloc	*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      9/06   Created                                 *
 ************************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( _ksGrid == (int *)NULL )  {
        G_MALLOC ( _ksGrid, int, *nword, 
		"Error allocating _ksGrid in dm_wdtrc" );
    }
    else  {
        G_REALLOC ( _ksGrid, int, *nword, 
		"Error re-allocating _ksGrid in dm_wdtrc" );
    }

    if ( _ksGrid == (int *)NULL )  {
	*iret = -35;
    }
    else  {
        dm_pkgd  ( rdata, _ksGrid, kword, ipktyp, nbits, misflg, kxky, 
		   kx, ref, scale, difmin, iret );
    }

}

void dm_wpkgc ( int *iflno, float *rdata, int *isword, int *kword, 
	 	int *ipktyp, int *nbits, int *misflg, int *kxky, int *kx,
		float *ref, float *scale, float *difmin, int *iret )
/************************************************************************
 * dm_wpkgc                                                             *
 *                                                                      *
 * This is a subfunction to the DM library function DM_WDTR.		*
 * It writes the packed grid stored locally in the variable '_ksGrid'	*
 * by sister routine 'dm_pkgdc'.					*
 * This functionality has been isolated and written in 'C'		*
 * to allow for the dynamic allocation of buffer memory.		*
 *                                                                      *
 * dm_wpkgc ( iflno, rdata, isword, kword, ipktyp, nbits, misflg, kxky,	*
 * 	      kx, ref, scale, difmin, iret )				*
 *									*
 *									*
 * Input parameters:                                                    *
 *      *iflno		int		File number (from FORTRAN)	*
 *      *rdata          float           Grid data			*
 *      *isword         int             Starting word			*
 *      *kword          int             Number of words	to write	*
 *      *ipktyp         int             Packing type			*
 *      *nbits          int             Number of bits			*
 *      *miss           int             Missing data flag		*
 *      *kxky           int             Number of grid points		*
 *      *kx             int             Number of points in x direction	*
 *      *ref            float           Reference minimum value of grid	*
 *      *scale          float           Scaling factor			*
 *      *difmin         float           Minimum value of differences	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                      = 0 - normal                    *
 *                                      = -31 - unknown packing type	*
 *                                      = -35 - problems w/ mem alloc	*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      9/06   Created                                 *
 ************************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( _ksGrid == (int *)NULL )  {
	*iret = -35;
    }
    else  {
        dm_wpkg  ( iflno, rdata, _ksGrid, isword, kword, ipktyp, nbits, 
		   misflg, kxky, kx, ref, scale, difmin, iret );
	G_FREE ( _ksGrid, int );
    }

}
