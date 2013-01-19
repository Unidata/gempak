#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define  SPOT   -9999999
#define  LOW    3
#define  HIGH   2

void cas_wrtrop ( FILE *ifpout, trop_t *ptr, int numbx, trophi_t *ptrh, 
		int numhi, troplo_t *ptrl, int numlo, char *chlvl, int *iret )
/************************************************************************
 * cas_wrtrop								*
 *                                                                      *
 * This function prints the information associated with spot, high and  *
 * low tropopause values to an ASCII file.				*
 *                                                                      *
 * cas_wrtrop ( ifpout, ptr, numbx, ptrh, numhi, ptrl, numlo, chlvl,	*
 *		 iret )    						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *ptr	        trop_t	 	Pointer to spot trop linked list*
 *	numbx		int		Number of spot trops. 		*
 *      *ptrh	        trophi_t	Pointer to high trop linked list*
 *	numhi		int		Number of high trops. 		*
 *      *ptrl		troplo_t	Pointer to low trop linked list *
 *	numlo		int		Number of low trops. 		*
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
    int		total;
    Boolean     fini;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    total   = 0;
    fini    = False;


    if ( numbx > 0 ){
	total++;
    }
    if ( numhi > 0 ){
	total++;
    }
    if ( numlo > 0 ){
	total++;
    }

   /* 
    * Write out each linked list's structure for tropopauses.
    */

   /*
    * Write out spot tropopauses first, if any.
    */
    if ( strcmp ( chlvl, "SWH" ) == 0 ) {
        fprintf(ifpout, "TROP\n");
    }
    else {
        fprintf(ifpout, "MTROP\n");
    }
    fprintf(ifpout, "%d \n", total);

    if ( numbx > 0 ) {
        fprintf(ifpout, "%d \n", SPOT);
        fprintf(ifpout, "%d \n", numbx );
        while ( !fini ) {
            fprintf (ifpout, "%5.1f   %6.1f   %10.1f\n", 
	                     ptr->lat, ptr->lon, ptr -> level); 

            if ( ptr -> next == NULL ) {
	        fini = True;
	    }
	    else {
	        ptr = ptr -> next;
	    }
        }
	fini = False;
    }

   /*
    * Write out low tropopauses next, if any.
    */

    if ( numlo > 0 ) {
        fprintf(ifpout, "%d \n", LOW);
        fprintf(ifpout, "%d \n", numlo );
        while ( !fini ) {
            fprintf (ifpout, "%5.1f   %6.1f   %10.1f\n", 
	                     ptrl->lat, ptrl->lon, ptrl -> level); 

            if ( ptrl -> next == NULL ) {
	        fini = True;
	    }
	    else {
	        ptrl = ptrl -> next;
	    }
        }
	fini = False;
    }

   /*
    * Write out high tropopauses last, if any.
    */

    if ( numhi > 0 ) {
        fprintf(ifpout, "%d \n", HIGH);
        fprintf(ifpout, "%d \n", numhi );
        while ( !fini ) {
            fprintf (ifpout, "%5.1f   %6.1f   %10.1f\n", 
	                     ptrh->lat, ptrh->lon, ptrh -> level); 

            if ( ptrh -> next == NULL ) {
	        fini = True;
	    }
	    else {
	        ptrh = ptrh -> next;
	    }
        }
    }
}
