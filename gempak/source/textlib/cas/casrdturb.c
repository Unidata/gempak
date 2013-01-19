#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define   MAXBRK   2

void cas_rdturb ( FILE *ifpout, int *numele, turb_t **ptrb, int *memalc,
                  int *iret )
/************************************************************************
 * cas_rdturb								*
 *                                                                      *
 * This function reads the TURB information from an ASCII High Level 	*
 * or Mid level Significant Weather file.				*
 *                                                                      *
 * cas_rdturb ( ifpout, numele, ptrb, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of TURB groups   *
 *      **ptrb			turb_t		Pointer to turb struct  *
 *      *memalc			int		Memory allocation flag  *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC         2/02   Created					*
 * M. Li/SAIC		 9/04	Modified to read Mid level TURB info	*
 ***********************************************************************/
{
    int		ii, jj, ij, ier, ierr;
    int         blen, num, numstr, llnum;
    char 	buff[256], **aryptr;

    Boolean	done;
    turb_t     *turb, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ierr    = 0;
    *numele = 0;
    *memalc = 0;
    done    = False;

   /*
    * Allocate memory for TURB strings.
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
        * Look for turbulence element section.
	*/

	if (strstr ( buff, "TURB") != (char *)NULL ) {
	    done = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    *numele = num;

           /*
            * Found the turbulence section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( num > 0 ) {
                    new = ( turb_t *) malloc (sizeof ( turb_t ) );
                    *memalc = 1;
                    new -> next = NULL;
                    if ( jj != 0 ) {
                        turb -> next = new;
                        turb = new;
                    }
                    else {
                        turb = new;
                        head = turb;
                    }
                }

	       /*
	        * Read the base and top of cat area ( meters ).
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_rxbl ( buff, buff, &blen, &ierr );
		cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		cst_crnm ( aryptr[0], &turb -> level1, &ier);
		cst_crnm ( aryptr[1], &turb -> level2, &ier);

	       /*
	        * Read the number of latitude and longitude points
		* and then the points.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &llnum, &ier);
		turb -> npt = llnum;

		for ( ij = 0; ij < llnum; ij++){
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
		    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		    cst_crnm ( aryptr[0], &turb->lat[ij], &ier);
		    cst_crnm ( aryptr[1], &turb->lon[ij], &ier);
		}

	       /*
	        * Read the degree of turbulence.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &turb->tdeg, &ier);
	    }
	}
    }
   /*
    * Set the return pointer to begining of link list location.
    */

     *ptrb = head;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
