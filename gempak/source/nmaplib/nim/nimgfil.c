#include "nimcmn.h"

void nim_gfil ( int index, dttms_t dattim, dttms_t endtim, 
	int mrange, int intrvl, int match, int minute, char *filnam, 
	int *iret )
/************************************************************************
 * nim_gfil								*
 *									*
 * This routine returns a file name that matches the given time for	*
 * the requested image data.						*
 *									*
 * nim_gfil ( index, dattim, endtim, mrange, intrvl, match, minute, 	*
 *		filnam, iret )						*
 *									*
 * Input parameters:							*
 *	index		int		Index to attribute arrays	*
 *	dattim		dttms_t		Date/time for the data		*
 *	endtim		dttms_t		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	match		int		Flag for time match scheme	*
 *	minute		int		Number of min diff for match	*
 *									*
 * Output parameters:							*
 *	*filnam		char		File name matched to date/time	*
 *	*iret		int		Return code			*
 *                                        +1 = no match for time        *
 *					  -1 = invalid index		*
 *                                        -2 = invalid time range       *
 *                                        -3 = no files found           *
 *									*
 **									*
 * S. Jacobs/NCEP	10/99	Created					*
 * S. Law/GSC		06/00	MAXIMG -> MAXTMPLT			*
 * T. Lee/SAIC		08/03	added time interval to calling sequence	*
 * T. Piper/SAIC	04/07	Incorporated nim_gfln functionality	*
 ***********************************************************************/
{
	int	ii, ier, ipos, nfound, ntimes;
        char    **namarr=NULL, **timarr=NULL;
        char    basetm[DTTMS_SIZE], path[LLPATH];
 	Boolean bauto, bflag;
/*---------------------------------------------------------------------*/
/*
 *  Initialize output parameters.
 */
    filnam[0] = '\0';
    *iret = 0;

/*
 *  Check for a valid index value.
 */
    if  ( index >= 0 &&  index <= MAXTMPLT ) {

/*
 *  Check for a valid maximum number of times.
 */
	if ( mrange > 0 ) {

	    bflag = FALSE;
	    bauto = FALSE;
	    basetm[0] = '\0';

/*
 *  Get all file names for the given image type.
 */
            nim_flnm ( image[index].type, image[index].info,
			basetm, endtim, mrange, intrvl, bflag,
			bauto, path, &namarr, &timarr, &nfound, iret );
            if ( *iret == G_NORMAL ) {
                if ( nfound > 0 ) {

/*
 *  Find the proper data date/time to plot.
 */
                    ntimes = nfound;  /*  Do not allow nfound to be changed!  */
                    cti_mtch ( match, (char*)dattim, (const char **)timarr,
					ntimes, minute, &ipos, &ier );
                    if  ( ipos >= 0 ) {

/*
 *  Set the output file name to the matched name.
 */
                        strcpy(filnam, path);
                        strcat(filnam, "/");
                        strcat(filnam, namarr[ipos]);
                    }
                    else {
                        *iret = +1;
                    }
                }
                else {
                    *iret = -3;
                }
            }
	    for (ii=0; ii < nfound; ii++) {
                G_FREE(namarr[ii], char);
                G_FREE(timarr[ii], char);
            }
            if ( namarr != NULL ) G_FREE(namarr, char*);
            if ( timarr != NULL ) G_FREE(timarr, char*);

        }
        else {
            *iret = -2;
        }
    }
    else {
	*iret = -1;
    }
}
