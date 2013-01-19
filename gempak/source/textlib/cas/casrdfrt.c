#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define   MAXBRK   4

void cas_rdfrt ( FILE *ifpout, int *numele, front_t **ptrf, int *memalc, 
                 int *iret )
/************************************************************************
 * cas_rdfrt								*
 *                                                                      *
 * This function reads the FRONT information from an ASCII High Level 	*
 * or Mid level Significant Weather file.				*
 *                                                                      *
 * cas_rdfrt ( ifpout, numele, ptrf, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of front groups  *
 *      **ptrf			front_t		Pointer to front struct *
 *      *memalc			int		Memory allocation flag  *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        01/02   Created					*
 * M. Li/SAIC		09/04	Modified to read Mid level front info 	*
 ***********************************************************************/
{
    int		ii, jj, ij, ier, ierr;
    int         blen, num, numstr, llnum;
    char 	buff[256], **aryptr;

    Boolean	done;
    front_t     *frt, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ierr    = 0;
    *numele = 0;
    *memalc = 0;
    done    = False;

   /*
    * Allocate memory for front strings.
    */

    aryptr = (char **) malloc(sizeof(char *) * MAXBRK);
    for ( ii = 0; ii < MAXBRK; ii++ ) {
        aryptr[ii] = (char *) malloc(128);
    }


    while ( !done ) {
        cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
        cst_rmbl (  buff, buff, &blen, &ier );
        if ( ierr != 0 ) {
            *iret = ierr;
	    return;
        }

       /*
        * Look for front element section.
	*/

	if (strstr ( buff, "FRONT") != (char *)NULL ) {
	    done = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    *numele = num;

           /*
            * Found the front section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( num > 0 ) {
                    new = ( front_t *) malloc (sizeof ( front_t) );
                    *memalc = 1;
                    new -> next = NULL;
                    if ( jj != 0 ) {
                        frt -> next = new;
                        frt = new;
                    }
                    else {
                        frt = new;
                        head = frt;
                    }
                }

	       /*
	        * Read the type of front.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &frt -> ftype, &ier);

	       /*
	        * Read the number of latitude and longitude points
		* and then the points, front speed and front direction.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &llnum, &ier);
		frt -> npt = llnum;

		for ( ij = 0; ij < llnum; ij++){
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
		    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		    cst_crnm ( aryptr[0], &frt->lat[ij], &ier);
		    cst_crnm ( aryptr[1], &frt->lon[ij], &ier);
		    cst_crnm ( aryptr[2], &frt->fntdir[ij], &ier);
		    cst_crnm ( aryptr[3], &frt->fntspd[ij], &ier);
		}
	    }
	}
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *ptrf = head;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
