#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define   MAXBRK   2

void cas_rdcld ( FILE *ifpout, int *numele, cloud_t **ptrc, int *memalc,
                 int *iret )
/************************************************************************
 * cas_rdcld								*
 *                                                                      *
 * This function reads the CLOUD information from an ASCII High Level 	*
 * Significant Weather file.						*
 *                                                                      *
 * cas_rdcld ( ifpout, numele, ptrc, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of cloud groups  *
 *      **ptrc			cloud_t		Pointer to cloud struct *
 *      *memalc			int		Memory allocation flag  *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        01/02   Created					*
 * M. Li/SAIC		09/04	Corrected mistaken comments		*
 ***********************************************************************/
{
    int		ii, jj, ij, ier, ierr;
    int         blen, num, numstr, llnum;
    char 	buff[256], **aryptr;

    Boolean	done;
    cloud_t     *cld, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ierr    = 0;
    *numele = 0;
    *memalc = 0;
    done    = False;

   /*
    * Allocate memory for cloud strings.
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
        * Look for cloud element section.
	*/

	if (strcmp ( buff, "CLOUD") == 0 ) {
	    done = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    *numele = num;

           /*
            * Found the cloud section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( num > 0 ) {
                    new = ( cloud_t *) malloc (sizeof ( cloud_t ) );
                    *memalc = 1;
                    new -> next = NULL;
                    if ( jj != 0 ) {
                        cld -> next = new;
                        cld = new;
                    }
                    else {
                        cld = new;
                        head = cld;
                    }
                }

	       /*
	        * Read the base and top of cloud area ( meters ).
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_rxbl ( buff, buff, &blen, &ierr );
		cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		cst_crnm ( aryptr[0], &cld -> level1, &ier);
		cst_crnm ( aryptr[1], &cld -> level2, &ier);

	       /*
	        * Read the number of latitude and longitude points
		* and then the points.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &llnum, &ier);
		cld -> npt = llnum;

		for ( ij = 0; ij < llnum; ij++){
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
		    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		    cst_crnm ( aryptr[0], &cld->lat[ij], &ier);
		    cst_crnm ( aryptr[1], &cld->lon[ij], &ier);
		}

	       /*
	        * Read the cloud distribution and type.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_rxbl ( buff, buff, &blen, &ierr );
		cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
	        cst_numb (aryptr[0], &cld->clddist, &ier);
	        cst_numb (aryptr[1], &cld->cldtyp, &ier);
	    }
	}
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *ptrc = head;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
