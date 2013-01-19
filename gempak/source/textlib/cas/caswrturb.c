#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrturb ( FILE *ifpout, turb_t *ptr, int numturb, char *chlvl, 
		  int *iret )
/************************************************************************
 * cas_wrturb								*
 *                                                                      *
 * This function prints the information associated with group type      *
 * "TURB" to an ASCII file.						*
 *                                                                      *
 * cas_wrturb ( ifpout, ptr, numturb, chlvl, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr		turb_t		Pointer to TURB linked list     *
 *	numturb		int		Number of turbulence groups	*
 *	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * M. Li/SAIC		09/04	Added chlvl				*
 ***********************************************************************/
{
    int		ij;
    Boolean	fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    fini    = False;


   /* 
    * Write out each linked list's structure for group type 'TURB'. 
    */

    if ( strcmp ( chlvl, "SWH" ) == 0 ) {
        fprintf(ifpout, "TURB \n");
    }
    else {
	fprintf(ifpout, "MTURB \n");
    }
    fprintf(ifpout, "%d \n", numturb);

    while ( !fini ) {

        fprintf(ifpout, "%10.1f   %10.1f\n", ptr -> level1, 
	                                                ptr -> level2 );

        fprintf(ifpout, "%d \n",ptr -> npt );
        for ( ij = 0; ij < ptr -> npt;ij++ ) {
            fprintf (ifpout, "%5.1f      %6.1f\n", 
	                                   ptr->lat[ij], ptr->lon[ij] );
        }
        fprintf(ifpout, "%d\n", ptr -> tdeg ); 
	                                                 

        if ( ptr -> next == NULL ) {
	    fini = True;
	}
	else {
	    ptr = ptr -> next;
	}
    }
}
