#define SFVINIT
#include "sfvcmn.h"

void sfvvgf ( char vgfile[], int *lenv, int *nparm, int iploc[], 
				int icolr[], int *ngrps, int *iret )
/************************************************************************
 * sfvvgf								*
 *									*
 * This function opens and scans a VG file for all data in STNMDL	*
 * groups. The data is stored in a global structure.			*
 *									*
 * sfvvgf ( vgfile, lenv, nparm, iploc, icolr, ngrps, iret )		*
 *									*
 * Input parameters:							*
 *	vgfile []	char		Vector graphics file name	*
 *	*lenv		int		Length of VG file name		*
 *	*nparm		int		Number of parameters		*
 *	iploc []	int		Data position in output array	*
 *	icolr []	int		Color list			*
 *									*
 * Output parameters:							*
 *	*ngrps		int		Number of groups found		*
 *	*iret		int		Return code			*
 *					  -3 = cannot open VG file	*
 *					   0 = normal			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/99						*
 * S. Jacobs/NCEP	 3/99	Added string length to call		*
 * A. Hardy/GSC		 1/01   Removed cast from FILE pointer          *
 * S. Jacobs/NCEP	 3/02	Fixed array index for station data	*
 * T. Piper/SAIC	 4/02	Added cfl_clos				*
 ***********************************************************************/
{

	int		ipos, ieof, ier, i, j, lent, 
			kcolr, ignum, found, misflg, kgnum;
	long		ifsiz;
	float		val;
	char		path[133];

	FILE		*filptr;

	VG_DBStruct	el;

/*---------------------------------------------------------------------*/

	*iret = 0;

	vgfile[*lenv] = CHNULL;

	cfl_inqr ( vgfile, NULL, &ifsiz, path, &ier );
	cvg_open ( vgfile, 0, &filptr, &ier );
	if  ( ier != 0 )  {
	    *iret = -3;
	    return;
	}

	for ( i = 0; i < LLSTFL; i++ ) {
	    stns[i].stid[0] = CHNULL;
	    for ( j = 0; j < MMPARM; j++ ) {
		stns[i].rdata[j] = RMISSD;
	    }
	}


	/*
	 * Find the smallest group number in the STNMDL group.
	 */
	ipos  = 0;
	kgnum = INT_MAX;
	ieof  = G_FALSE;
	while  ( ! ieof )  {
	    cvg_rdhdr ( vgfile, filptr, ipos, ifsiz, &el, &ieof, &ier );
	    if  ( ier != 0 )  ieof = G_TRUE;

	    if  ( ! ieof )  {
		if  ( el.hdr.grptyp == STNMDL )  {
		    kgnum = G_MIN ( kgnum, el.hdr.grpnum );
		}
	    }
	    ipos += el.hdr.recsz;
	}

	/*
	 * Process the elements.
	 */
	ipos = 0;

	ieof = G_FALSE;
	while  ( ! ieof )  {

	    cvg_rdhdr ( vgfile, filptr, ipos, ifsiz, &el, &ieof, &ier );

	    if  ( ier != 0 )  ieof = G_TRUE;

	    if  ( ! ieof )  {

		if  ( el.hdr.grptyp == STNMDL &&
		      el.hdr.vg_type == SPTX_ELM )  {

		    /*
		     * Use the group number and the smallest group
		     * number to compute an array index.
		     */
		    ignum = el.hdr.grpnum - kgnum + 1;
		    kcolr = el.hdr.maj_col;

		    found = G_FALSE;
		    i = 0;
		    while  ( i < *nparm && ! found ) {

			if  ( kcolr == icolr[i] )  {

			    found = G_TRUE;

			    cvg_rdele ( &el, ipos, el.hdr.recsz,
					filptr, &ier );

			    if  ( iploc[i] == -1 || iploc[i] == -2 )  {
				cst_lstr ( el.elem.spt.text,
					   &lent, &ier );
				cst_ncpy ( stns[ignum].stid,
					   el.elem.spt.text,
					   lent, &ier );
			    }
			    else {
				cst_crnm ( el.elem.spt.text,
					   &val, &ier );
				stns[ignum].rdata[i] = val;
			    }

			}

			i++;

		    }
		}
	    }

	    ipos += el.hdr.recsz;

	}

	misflg = G_TRUE;
	*ngrps = LLSTFL;
	while ( misflg && *ngrps >= 0 )  {
	    if  ( stns[*ngrps].stid[0] != CHNULL )  {
		misflg = G_FALSE;
	    }
	    else {
		(*ngrps)--;
	    }
	}
	cfl_clos ( filptr, &ier );
}
