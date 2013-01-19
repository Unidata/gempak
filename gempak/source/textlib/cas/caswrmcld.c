#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrmcld ( FILE *ifpout, mcloud_t *ptr, int nummcld, int *iret )
/************************************************************************
 * cas_wrmcld								*
 *                                                                      *
 * This function prints the information for the "MCLOUD" data to an     *
 * ASCII file.								*
 *                                                                      *
 * cas_wrmcld ( ifpout, ptr, nummcld, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr		mcloud_t	Pointer to MCLOUD linked list   *
 *	nummcld		int		Number of mcloud groups		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           08/04   Created					*
 * M. Li/SAIC		08/04	Modified output format			*
 ***********************************************************************/
{
    int		ii, ier;
    char	tmpln[256], tmpst[10];
    Boolean	fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    fini    = False;

   /* 
    * Write out each linked list's structure for group type 'MCLOUD'. 
    */

    fprintf(ifpout, "MCLOUD \n");
    fprintf(ifpout, "%d \n", nummcld);

    while ( !fini ) {

       /*
	* Lat/lon.
	*/
        fprintf(ifpout, "%d \n", ptr -> npt );
        for ( ii = 0; ii < ptr -> npt; ii++ ) {
            fprintf (ifpout, "%5.1f      %6.1f\n", 
	                                   ptr->lat[ii], ptr->lon[ii] );
        }

       /*
	* Non-Cb cloud distribution.
	*/
	fprintf(ifpout, "%d \n", ptr -> ncld );
	if ( ptr -> ncld > 0 ) {
	    sprintf ( tmpln, "%d      ", ptr -> ncdis[0] );
	    for ( ii = 1; ii < ptr -> ncld; ii++ ) {
		cst_inch ( ptr -> ncdis[ii], tmpst, &ier );
		strcat ( tmpln, tmpst );
		strcat ( tmpln, "      " );
	    }

	    fprintf (ifpout, "%s \n", tmpln );
	}

       /*
        * Non-Cb cloud type.
        */
        fprintf(ifpout, "%d \n", ptr -> ntyp );
        if ( ptr -> ntyp > 0 ) {
            sprintf ( tmpln, "%d      ",ptr -> nctyp[0] );
            for ( ii = 1; ii < ptr -> ntyp; ii++ ) {
		cst_inch ( ptr -> nctyp[ii], tmpst, &ier );
                strcat ( tmpln, tmpst );
                strcat ( tmpln, "      " );
            }

            fprintf (ifpout, "%s \n", tmpln );
        }

       /*
	* Turbulence.
	*/
	fprintf(ifpout, "%d \n", ptr -> turb );
	if ( ptr -> turb == 1 ) {
	    fprintf(ifpout, "%10.1f   %10.1f \n", 
			ptr -> tbase, ptr -> ttop );
	    fprintf(ifpout, "%d \n", ptr -> tdeg );
 	}

       /*
        * Icing.
        */
        fprintf(ifpout, "%d \n", ptr -> icing);
        if ( ptr -> icing == 1 ) {
            fprintf(ifpout, "%10.1f   %10.1f \n", 
                        ptr -> icbase, ptr -> ictop );
	    fprintf(ifpout, "%d \n", ptr -> dic );
        }

       /*
        * Cb.
        */
        fprintf(ifpout, "%d \n", ptr -> fcb );
        if ( ptr -> fcb == 1 ) {
            fprintf(ifpout, "%10.1f   %10.1f \n", 
                        ptr -> cbbase, ptr -> cbtop );
	    fprintf(ifpout, "%d      %d \n",                    
                        ptr -> cbdis, ptr -> cbtyp );
        }


        if ( ptr -> next == NULL ) {
	    fini = True;
	}
	else {
	    ptr = ptr -> next;
	}
    }
}
