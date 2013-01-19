#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrstm ( FILE *ifpout, storm_t *ptr, int numb, char *symtyp, int *iret )
/************************************************************************
 * cas_wrstm								*
 *                                                                      *
 * This function prints the information associated with group type      *
 * "LABEL" to an ASCII file.						*
 *                                                                      *
 * cas_wrstm ( ifpout, ptr, numb, symtyp, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr		storm_t		Pointer to LABEL linked list    *
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
    * Write out the linked list's structure for group type 'LABEL'. 
    */

    fprintf ( ifpout, "%s\n", symtyp);
    fprintf ( ifpout, "%d \n", numb);

    while ( !fini ) {

        fprintf ( ifpout, "%s \n", ptr -> name );
        fprintf ( ifpout, "%5.1f      %6.1f\n", ptr->lat, ptr->lon );
	if ( strcmp ( symtyp, "STORM" ) == 0 ) {
            fprintf ( ifpout, "%d \n", ptr -> stmtyp );
	}

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
