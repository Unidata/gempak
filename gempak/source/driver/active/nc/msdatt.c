#include "nccmn.h"

void msdatt ( int *iunit, char *filnam, int *lenf, int *itype, 
              float *xsize, float *ysize, int *ileft, int *ibot, 
	      int *iright, int *itop, int *iret )
/************************************************************************
 * msdatt								*
 *									*
 * This subroutine is called to initialize the metafile attributes.	*
 *									*
 * msdatt  ( iunit, filnam, lenf, itype, xsize, ysize,			*
 *           ileft, ibot, iright, itop, iret )                          *
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*filnam		char		Output metafile name		*
 *	*lenf		int		Length of file name		*
 *	*itype		int		Device color type		*
 *	*xsize		float		X size in pixels		*
 *	*ysize		float		Y size in pixels		*
 *									*
 * Output parameters:							*
 *      *ileft          int             Left device coordinate          *
 *      *ibot           int             Bottom device coordinate        *
 *      *iright         int             Right device coordinate         *
 *      *itop           int             Top device coordinate           *
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 2/96						*
 * S. Jacobs/NCEP	 5/96	Added new global variables for queries	*
 * S. Jacobs/NCEP	 2/97	Fixed check for same file name		*
 * R. Tian/SAIC          4/02   Added init of ileft, ibot, iright, itop *
 * M. Li/SAIC		 7/02	Added G_NINT				*
 ***********************************************************************/
{
	int	ier;
	char	tmpfil[MAX_FNLEN];
        int     tmpxz, tmpyz;
	long	flen;
	int	nbin;
	FILE	*fp;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	If the input length is greater than the maximum allowed,
 *	return an error.
 */
	if  ( ( *lenf >= MAX_FNLEN ) || ( *lenf <= 0 ) )  {
	    *iret = G_NOMETA;
	    return;
	}

/*
 *	Copy the file name to a temporary variable and add a NULL.
 */
	strncpy ( tmpfil, filnam, (size_t)*lenf );
	tmpfil[*lenf] = CHNULL;

/*
 *	Compare the input file name to the current file name.  If the
 *	names are different, close the old file.  Otherwise, do nothing.
 */
	if  ( strcmp ( curfil, tmpfil ) != 0 )  {
 	    mclose ( &ier );

/*
 *	    Set the current file name.
 */
	    strcpy ( curfil, tmpfil );
	    *iret = G_NEWWIN;

/*
 *	    Set the global output and color scheme types.
 */
    	    kctype = *itype;
	    kunit  = *iunit;

/*
 *          Check for the existence of the metafile.
 */
	    cfl_inqr(curfil, NULL, &flen, tmpfil, &ier);
            if ( ( ier == 0 ) && ( strcmp ( curfil, "Nmeta" ) != 0 ) ) {
/*
 *          	Get frame size from the existing file
 */
		if ( ( fp = cfl_ropn(curfil, NULL, &ier) ) == NULL ) {
            	    *iret = G_NOMETA;
            	    return;
        	}
		
	        cfl_read(fp, sizeof(nc_file_header), (unsigned char *)&meta_head,
		         &nbin, &ier);
        	if ( ier ) {
            	    *iret = G_NOMETA;
		    cfl_clos(fp, &ier);
            	    return;
        	}
		cfl_clos(fp, &ier);

            	if ( meta_head.version == 2 ) {
/*
 *    	            Set the global CGM frame size
 */
        	    fxsize = meta_head.fxsize;
        	    fysize = meta_head.fysize;

/*
 *      	    ileft  = left device coordinate
 *      	    ibot   = bottom device coordinate
 *      	    iright = right device coordinate
 *      	    itop   = top device coordinate
 */
        	    *ileft  = 0;
        	    *ibot   = 0;
        	    *iright = (int)meta_head.fxsize;
        	    *itop   = (int)meta_head.fysize;
		}

            } else {
/*
 *              The valid range of xsize and ysize is 0.1 ~ 1.0.  If neither
 *              value is 1.0, then set the larger value to be 1.0, and set the
 *              smaller value to be the ratio of the smaller to the larger.
 */
        	tmpxz = G_NINT(*xsize * (float)XY_SCALE);
        	tmpyz = G_NINT(*ysize * (float)XY_SCALE);
        	tmpxz = (tmpxz < 0 || tmpxz > XY_SCALE) ? XY_SCALE : tmpxz;
        	tmpyz = (tmpyz < 0 || tmpyz > XY_SCALE) ? XY_SCALE : tmpyz;
        	if(tmpxz != XY_SCALE && tmpyz != XY_SCALE) {
            	    if(tmpxz > tmpyz) {
                    	tmpyz = G_NINT((((float)tmpyz)/(float)tmpxz)*(float)XY_SCALE);
                    	tmpxz = XY_SCALE;
            	    } else if(tmpxz < tmpyz) {
                    	tmpxz = G_NINT((((float)tmpxz)/(float)tmpyz)*(float)XY_SCALE);
                    	tmpyz = XY_SCALE;
            	    } else {
                    	tmpxz = XY_SCALE;
                    	tmpyz = XY_SCALE;
            	    }
        	}
        	tmpxz = (tmpxz < G_NINT(0.1F*(float)XY_SCALE)) ? G_NINT(0.1F*(float)XY_SCALE) 
						      : tmpxz;
        	tmpyz = (tmpyz < G_NINT(0.1F*(float)XY_SCALE)) ? G_NINT(0.1F*(float)XY_SCALE) 
						      : tmpyz;

/*
 *    	        Set the global CGM frame size
 */
        	fxsize = (unsigned short)tmpxz;
        	fysize = (unsigned short)tmpyz;;

/*
 *      	ileft  = left device coordinate
 *      	ibot   = bottom device coordinate
 *      	iright = right device coordinate
 *      	itop   = top device coordinate
 */
        	*ileft  = 0;
        	*ibot   = 0;
        	*iright = tmpxz;
        	*itop   = tmpyz;
	    }
	}
}
