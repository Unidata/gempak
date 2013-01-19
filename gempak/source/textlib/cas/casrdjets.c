#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define  MAXBRK   6

void cas_rdjets ( FILE *ifpout, int *numele, jets_t **ptrj, int *memalc,
                  int *iret )
/************************************************************************
 * cas_rdjets								*
 *                                                                      *
 * This function reads the JETS information from an ASCII High Level 	*
 * or Mid level Significant Weather file.				*
 *                                                                      *
 * cas_rdjets ( ifpout, numele, ptrj, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of jets groups   *
 *      **ptrj			jets_t		Pointer to jets struct 	*
 *      *memalc			int		Memory allocation flag  *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        01/02   Created					*
 * M. Li/SAIC		04/04	Added flight level deltas		*
 * M. Li/SAIC		09/04	Modified to read Mid level jet info	*
 ***********************************************************************/
{
    int		ii, jj, ij, ier, ierr;
    int         blen, num, numstr, llnum;
    char 	buff[256], **aryptr;

    Boolean	done;
    jets_t     *jets, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ierr    = 0;
    *numele = 0;
    *memalc = 0;
    done    = False;

   /*
    * Allocate memory for jet strings.
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
        * Look for jet element section.
	*/

	if (strstr ( buff, "JET") != (char *)NULL ) {
	    done = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    *numele = num;

           /*
            * Found the jets section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( num > 0 ) {
                    new = ( jets_t *) malloc (sizeof ( jets_t) );
                    *memalc = 1;
                    new -> next = NULL;
                    if ( jj != 0 ) {
                        jets -> next = new;
                        jets = new;
                    }
                    else {
                        jets = new;
                        head = jets;
                    }
                }

	       /*
	        * Read the number of latitude and longitude points
		* and then the points, level and speed.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &llnum, &ier);
		jets -> npt = llnum;

		for ( ij = 0; ij < llnum; ij++){
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
		    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		    cst_crnm ( aryptr[0], &jets->lat[ij], &ier);
		    cst_crnm ( aryptr[1], &jets->lon[ij], &ier);
		    cst_crnm ( aryptr[2], &jets->level[ij], &ier);
		    cst_crnm ( aryptr[3], &jets->speed[ij], &ier);
		    if ( numstr == 4 ) {
			jets->levabv[ij] = SIGRLMS;
			jets->levblw[ij] = SIGRLMS;
		    }
		    else {
		    	cst_crnm ( aryptr[4], &jets->levabv[ij], &ier);
		    	cst_crnm ( aryptr[5], &jets->levblw[ij], &ier);
		   }
		}
	    }
	}
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *ptrj = head;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
