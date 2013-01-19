#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrjets ( FILE *ifpout, jets_t *ptr, int numjet, char *chlvl, 
		  int *iret )
/************************************************************************
 * cas_wrjets								*
 *                                                                      *
 * This function prints the information associated with group type      *
 * "JETS" to an ASCII file.						*
 *                                                                      *
 * cas_wrjets ( ifpout, ptr, numjet, chlvl, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr		jets_t		Pointer to JETS linked list     *
 *	numjet		int		Number of jet groups		*
 *	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * M. Li/SAIC		04/04	Added flight level deltas		*
 * M. Li/SAIC           09/04   Added chlvl                             *
 ***********************************************************************/
{
    int		ij; 
    Boolean	fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    fini    = False;


   /* 
    * Write out each linked list's structure for group type 'JETS'. 
    */

    if ( strcmp ( chlvl, "SWH" ) == 0 ) {
        fprintf(ifpout, "JET\n");
    }
    else {
        fprintf(ifpout, "MJET\n");
    }
    fprintf(ifpout, "%d \n", numjet);

    while ( !fini ) {

        fprintf(ifpout, "%d\n", ptr -> npt);

        for ( ij = 0; ij < ptr -> npt;ij++ ) {
            fprintf (ifpout, "%5.1f  %6.1f  %10.1f  %10.1f  %10.1f  %10.1f\n", 
	                ptr->lat[ij], ptr->lon[ij], ptr->level[ij], 
			ptr->speed[ij], ptr->levabv[ij], ptr->levblw[ij] );
        }

        if ( ptr -> next == NULL ) {
	    fini = True;
	}
	else {
	    ptr = ptr -> next;
	}
    }
}
