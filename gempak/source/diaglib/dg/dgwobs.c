#include "dg.h"

void dg_wobs ( const int *iflno, const char *gdtime1, const char *gdtime2,
               const int *level1, const int *level2, const int *ivcord,
	       float *grid1, float *grid2, int *wcmp, int *wmks,
	       char *wparm, int *igx, int *igy, int *iret )
/************************************************************************
 * dg_wobs								*
 *									*
 * This subroutine retrieves the observed wind components from a grid	*
 * file.  The grid file is searched for the following parameter names:	*
 *									*
 *              'UWND'  and  'VWND'					*
 *              'UKNT'  and  'VKNT'					*
 *              'DRCT'  and  'SPED'					*
 *              'DRCT'  and  'SKNT'					*
 *									*
 * dg_wobs ( iflno, gdtime1, gdtime2, level1, level2, ivcord, grid1,	*
 *           grid2, wcmp, wmks, wparm, igx, igy, iret )			*
 *									*
 * Input parameters:							*
 *	*iflno		const int	Grid file number		*
 *	*gdtime1	const char	Grid time			*
 *	*gdtime2	const char	Grid time			*
 *	*level1		const int	Grid level			*
 *	*level2		const int	Grid level			*
 *	*ivcord		const int	Grid vertical coordinate	*
 *									*
 * Output parameters:							*
 *	*grid1		float		First grid			*
 *	*grid2		float		Second grid			*
 *	*wcmp		int		Component type 			*
 *					  true  = u,v			*
 *					  false = speed,direction	*
 *	*wmks		int		MKS units flag			*
 *	*wparm		char		Wind components concatenated	*
 *	*igx		int		Number of points in x dir	*
 *	*igy		int		Number of points in y dir	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					  -3 = wind unavailable		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 1/04	Modified from GR_WOBS     		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char c1[4][5] = { "UWND", "UKNT", "DRCT", "DRCT" };
    char c2[4][5] = { "VWND", "VKNT", "SPED", "SKNT" };
    int jghdr[LLGDHD];
    int knt, igx1, igy1, ier1, ier2;
/*----------------------------------------------------------------------*/
    *iret = -3;

    /*
     * Check to find which fields are present in grid file.
     */
    knt = 0;
    ier1 = 0;
    ier2 = 0;
    while ( ( *iret != 0 ) && ( knt < 4 ) ) {
	/*
	 * Check whether two required components are in file.
	 */
	if ( _nfile.nucode == G_TRUE ) {
	    dgc_nrdt ( iflno, gdtime1, gdtime2, level1, level2, ivcord,
	        c1[knt], grid1, &igx1, &igy1, jghdr, &ier1 );
	    if ( ier1 == 0 ) {
	        dgc_nrdt ( iflno, gdtime1, gdtime2, level1, level2,
		          ivcord, c2[knt], grid2, igx, igy, jghdr, &ier2 );
	    }
	    /*
	     * If both grids found, check for size.
	     */
	    if ( ( ier1 == 0 ) && ( ier2 == 0 ) &&
	         ( igx1 == *igx ) && ( igy1 == *igy ) ) {
		*iret = 0;
	    } else {
		knt++;
	    }
	} else {
#if 0
	    dg_rdat ( iflno, gdtime1, gdtime2, level1, level2, ivcord,
	        c1[knt], grid1, &igx1, &igy1, jghdr, &ier1 );
	    if ( ier1 == 0 )
	        dg_rdat ( iflno, gdtime1, gdtime2, level1, level2, 
		          ivcord, c2[knt], grid2, igx, igy, jghdr, &ier2 );
	    /*
	     * If both grids found, check for size.
	     */
	    if ( ( ier1 == 0 ) && ( ier2 == 0 ) &&
	         ( igx1 == *igx ) && ( igy1 == *igy ) ) {
		*iret = 0;
	    } else {
		knt++;
	    }
#endif
	}
    }

    /*
     * If grids were found, set flags properly.
     */
    if ( *iret == 0 ) {
	if ( knt == 0 ) {
	    *wcmp = G_TRUE;
	    *wmks = G_TRUE;
	} else if ( knt == 1 ) {
	    *wcmp = G_TRUE;
	    *wmks = G_FALSE;
	} else if ( knt == 2 ) {
	    *wcmp = G_FALSE;
	    *wmks = G_TRUE;
	} else if ( knt == 3 ) {
	    *wcmp = G_FALSE;
	    *wmks = G_FALSE;
	}

	/*
	 * Set wparm.
	 */
	strcpy ( wparm, c1[knt] );
	strcat ( wparm, ";" );
	strcat ( wparm, c2[knt] );
    }

    return;
}
