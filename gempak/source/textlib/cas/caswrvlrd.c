#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrvlrd ( FILE *ifpout, volrad_t *ptr, int numb, char *symtyp, int *iret )
/************************************************************************
 * cas_wrvlrd								*
 *                                                                      *
 * This function prints the information associated with group type      *
 * "LABEL" for volcanoes and radiation to an ASCII file.		*
 *                                                                      *
 * cas_wrvlrd ( ifpout, ptr, numb, symtyp, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr            volrad_t	Pointer to LABEL linked list    *
 *	numb		int		Number of symbol groups		*
 *	*symtyp		char		Type of symbol			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 ***********************************************************************/
{
    Boolean	fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    fini    = False;

   /* 
    * Write out each linked list's structure for group type 'LABEL'. 
    */

    if ( numb > 0 ) {
        fprintf ( ifpout, "%s\n", symtyp);
        fprintf ( ifpout, "%d \n", numb);

        while ( !fini ) {

            fprintf ( ifpout, "%s \n", ptr -> name );
            fprintf ( ifpout, "%5.1f      %6.1f\n", ptr->lat, ptr->lon );
            fprintf ( ifpout, "%d   %d   %d   %d   %d \n", 
	                       ptr -> year, ptr -> month, ptr -> day, 
		               ptr -> hour, ptr -> minute );
           /*
            * Advance to next link list.
            */

            if ( ptr -> next == NULL ) {
	        fini = True;
	    }
	    else {
	        ptr = ptr -> next;
	    }
	}
    }
}
