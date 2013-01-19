#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define   MAXBRK  2

void cas_rdstm ( FILE *ifpout, int *numele, storm_t **ptrs, int *memalc,
                 int *iret )
/************************************************************************
 * cas_rdstm								*
 *                                                                      *
 * This function reads the storm information from an ASCII High Level 	*
 * Significant Weather file.						*
 *                                                                      *
 * cas_rdstm ( ifpout, numele, ptrs, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of storm groups  *
 *      **ptrs			storm_t		Pointer to storm struct *
 *      *memalc			int		Memory allocation flag  * 
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        02/02   Created					*
 * M. Li/SAIC           09/04   Corrected mistaken comments             *
 ***********************************************************************/
{
    int		ii, jj, ier, ierr;
    int         blen, num, numstr;
    char 	buff[256], **aryptr;

    Boolean	done;
    storm_t     *stm, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    *numele = 0;
    ierr    = 0;
    *memalc = 0;
    done    = False;

   /*
    * Allocate memory for storm strings.
    */

    aryptr = (char **) malloc(sizeof(char *) * MAXBRK);
    for ( ii = 0; ii < MAXBRK; ii++ ) {
        aryptr[ii] = (char *) malloc(MAXCH);
    }


    while ( !done ) {
        cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
        cst_rmbl (  buff, buff, &blen, &ier );
        if ( ierr != 0 ) {
            *iret = ierr;
	    return;
        }

       /*
        * Look for storm element section.
	*/

	if (strcmp ( buff, "STORM") == 0 ) {
	    done = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    *numele = num;

           /*
            * Found the storm section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( num > 0 ) {
                    new = ( storm_t *) malloc (sizeof ( storm_t ) );
                    *memalc = 1;
                    new -> next = NULL;
                    if ( jj != 0 ) {
                        stm -> next = new;
                        stm = new;
                    }
                    else {
                        stm = new;
                        head = stm;
                    }
                }

	       /*
	        * Read the name of the storm.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
		strcpy ( stm -> name, buff );

	       /*
	        * Read the latitude and longitude point.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_rxbl ( buff, buff, &blen, &ierr );
		cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		cst_crnm ( aryptr[0], &stm->lat, &ier);
		cst_crnm ( aryptr[1], &stm->lon, &ier);

	       /*
	        * Read the storm type.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &stm -> stmtyp, &ier);
	    }
	}
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *ptrs = head;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
