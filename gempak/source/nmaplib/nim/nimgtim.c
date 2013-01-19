#include "nimcmn.h"

void nim_gtim ( int index, dttms_t endtim, int mrange, int intrvl,
		Boolean bflag, Boolean bauto, dttms_t basetm,
		int *ntime, char ***timarr, int *iret )
/************************************************************************
 * nim_gtim								*
 *									*
 * This routine returns an array of times for the requested image data.	*
 *									*
 * nim_gtim ( index, endtim, mrange, intrvl, ntime, timarr, iret )	*
 *									*
 * Input parameters:							*
 *	index		int		Index to attribute arrays	*
 *	endtim		dttms_t		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	bflag		Boolean		Reference time flag		*
 *	bauto		Boolean		Auto update flag		*
 *	basetm		dttms_t		Base time for image data	*
 *									*
 * Output parameters:							*
 *	*ntime		int		Number of times in array	*
 *	***timarr	char		Array of times			*
 *	*iret		int		Return code			*
 *                                        -1 = invalid index            *
 *                                        -2 = invalid time range       *
 *                                        -3 = no files found           *
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXIMG -> MAXTMPLT			*
 * R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
 * T. Lee/SAIC		 8/03	Added time interval in calling sequence	*
 * T. Lee/SAIC		01/04	Auto update with set interval		*
 * T. Piper/SAIC	04/07	Incorporated nim_tlst functionality 	*
 ***********************************************************************/
{

    int ii;
    char **namarr=NULL, path[LLPATH];

/*---------------------------------------------------------------------*/
/*
 *  Initialize output parameters.
 */
    *ntime = 0;
    *iret = 0;
/*
 *  Check for a valid index value.
 */
    if  ( index >=  0 &&  index <= MAXTMPLT ) {

/*
 *  Check for a valid maximum number of times.
 */
        if ( mrange > 0 ) {

/*
 *  Get all file names for the given image type.
 */
	    nim_flnm ( image[index].type, image[index].info,
			basetm, endtim, mrange, intrvl, bflag,
			bauto, path, &namarr, timarr, ntime, iret );
/*
 *  This function does NOT require the names array.
 */
	    for (ii=0; ii < *ntime; ii++) {
                G_FREE(namarr[ii], char);
            }
            if ( namarr != NULL ) G_FREE(namarr, char*);

	    if ( *iret != G_NORMAL ) {
                *iret = -3;
	    }
        }
        else {
            *iret = -2;
        }
    }
    else {
        *iret = -1;
    }
}
