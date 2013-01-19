#define GLOBAL

#include "nccmn.h"

void minita ( int *iunit, char *filnam, int *lenf, int *itype, 
  	      float *xsize, float *ysize, int *ileft, int *ibot, 
	      int *iright, int *itop, int *iret )
/************************************************************************
 * minita								*
 *									*
 * This subroutine is called to initialize the metafile.		*
 *									*
 * minita  ( iunit, filnam, lenf, itype, xsize, ysize, 			*
 *	     ileft, ibot, iright, itop, iret )				*
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
 * S. Jacobs/NCEP	 3/96	MINITA based on MINITD			*
 * S. Jacobs/NCEP	 4/96	Added color table initialization	*
 * S. Jacobs/NCEP	 5/96	Added new global variables for queries	*
 * S. Jacobs/NCEP	 7/98	Added init of txszx and txszy		*
 * R. Tian/SAIC		 4/02	Added init of ileft, ibot, iright, itop *
 * M. Li/SAIC		 7/02	Added G_NINT				*
 ***********************************************************************/
{
	int	ier;
	char	tmpfil[MAX_FNLEN];
	int	tmpxz, tmpyz;
	long	flen;
	int	nbin;
        FILE    *fp;

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
 *	Set the global output and color scheme types.
 */
	kctype = *itype;
	kunit  = *iunit;

/*
 *	Set txszx, txszy (hardware text sizes) here if hardware
 *	text is used.
 *	txszx = bscalc * 7
 *	txszy = bscalc * 9
 */
	txszx = 350.0F;
	txszy = 450.0F;

/*
 *	Copy the file name to a temporary variable and add a NULL.
 */
	strncpy ( tmpfil, filnam, (size_t)*lenf );
	tmpfil[*lenf] = CHNULL;

/*
 *	If the new file name is not empty, set the current file name.
 */
	if  ( tmpfil[0] != CHNULL )  {
	    strcpy ( curfil, tmpfil );
	    *iret = G_NEWWIN;
	}

/*
 *      Check for the existence of the metafile.
 */
	cfl_inqr(curfil, NULL, &flen, tmpfil, &ier);
	if ( (ier == 0) && ( strcmp ( curfil, "Nmeta" ) != 0 ) ) {
/*
 *          Get frame size from the existing file
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
 *              Set the global CGM frame size
 */
                fxsize = meta_head.fxsize;
                fysize = meta_head.fysize;

/*
 *              ileft  = left device coordinate
 *              ibot   = bottom device coordinate
 *              iright = right device coordinate
 *              itop   = top device coordinate
 */
                *ileft  = 0;
                *ibot   = 0;
                *iright = (int)meta_head.fxsize;
                *itop   = (int)meta_head.fysize;
            }
	} else {
/*
 *	    The valid range of xsize and ysize is 0.1 ~ 1.0.  If neither
 *	    value is 1.0, then set the larger value to be 1.0, and set the 
 *	    smaller value to be the ratio of the smaller to the larger.
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
	    tmpxz = (tmpxz < G_NINT(0.1F*(float)XY_SCALE)) ? G_NINT(0.1F*(float)XY_SCALE) : tmpxz;
	    tmpyz = (tmpyz < G_NINT(0.1F*(float)XY_SCALE)) ? G_NINT(0.1F*(float)XY_SCALE) : tmpyz;

/*
 *	    Set the global CGM frame size
 */
	    fxsize = (unsigned short)tmpxz;
	    fysize = (unsigned short)tmpyz;;

/*
 *          ileft  = left device coordinate
 *          ibot   = bottom device coordinate
 *          iright = right device coordinate
 *          itop   = top device coordinate
 */
            *ileft  = 0;
            *ibot   = 0;
            *iright = tmpxz;
            *itop   = tmpyz;
	}

/*
 *	Set the open file flag to FALSE.
 */
	opnfil = G_FALSE;

/*
 *	Initialize the color tables in case the user queries the
 *	color components.
 */
	cctabl( NULL, &ier );
	if  ( ier != G_NORMAL )  return;

	csctbl ( NULL, &ier );

}
