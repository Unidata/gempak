#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrfrt ( FILE *ifpout, front_t *ptr, int numfrt, char *chlvl,
		 int *iret )
/************************************************************************
 * cas_wrfrt								*
 *                                                                      *
 * This function prints the information associated with group type      *
 * "FRONT" to an ASCII file.						*
 *                                                                      *
 * cas_wrfrt ( ifpout, ptr, numfrt, chlvl, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr		front_t		Pointer to FRONT linked list    *
 *	numfrt		int		Number of front groups		*
 *      *chlvl          char            Chart level                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * M. Li/SAIC           09/04   Added chlvl                             *
 ***********************************************************************/
{
    int		ij; 
    Boolean	fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    fini    = False;


   /* 
    * Write out each linked list's structure for group type 'FRONT'. 
    */

    if ( strcmp ( chlvl, "SWH" ) == 0 ) {
        fprintf(ifpout, "FRONT\n");
    }
    else {
        fprintf(ifpout, "MFRONT\n");
    }

    fprintf(ifpout, "%d \n", numfrt);

    while ( !fini ) {

        fprintf(ifpout, "%d\n", ptr -> ftype);
        fprintf(ifpout, "%d\n", ptr -> npt);

        for ( ij = 0; ij < ptr -> npt;ij++ ) {
            fprintf (ifpout, "%5.1f  %6.1f  %10.1f  %10.1f\n", 
	                       ptr->lat[ij], ptr->lon[ij],
			       ptr -> fntdir[ij], ptr -> fntspd[ij]);
        }

        if ( ptr -> next == NULL ) {
	    fini = True;
	}
	else {
	    ptr = ptr -> next;
	}
    }
}
