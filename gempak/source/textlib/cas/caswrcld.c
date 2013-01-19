#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrcld ( FILE *ifpout, cloud_t *ptr, int numclds, int *iret )
/************************************************************************
 * cas_wrcld								*
 *                                                                      *
 * This function prints the information associated with group type      *
 * "CLOUD" to an ASCII file.						*
 *                                                                      *
 * cas_wrcld ( ifpout, ptr, numclds, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr		cloud_t		Pointer to CLOUD linked list    *
 *	numclds		int		Number of cloud groups		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 ***********************************************************************/
{
    int		ij;
    Boolean	fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    fini    = False;


   /* 
    * Write out each linked list's structure for group type 'CLOUD'. 
    */

    fprintf(ifpout, "CLOUD \n");
    fprintf(ifpout, "%d \n", numclds);

    while ( !fini ) {

        fprintf(ifpout, "%10.1f   %10.1f\n", ptr -> level1, ptr -> level2 );

        fprintf(ifpout, "%d \n",ptr -> npt );
        for ( ij = 0; ij < ptr -> npt;ij++ ) {
            fprintf (ifpout, "%5.1f      %6.1f\n", 
	                                   ptr->lat[ij], ptr->lon[ij] );
        }
        fprintf(ifpout, "%d        %d\n", ptr -> clddist, ptr -> cldtyp);

        if ( ptr -> next == NULL ) {
	    fini = True;
	}
	else {
	    ptr = ptr -> next;
	}
    }
}
