#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define   MAXBRK   2

void cas_rdmcld ( FILE *ifpout, int *numele, mcloud_t **ptrm, int *memalc,
                 int *iret )
/************************************************************************
 * cas_rdmcld								*
 *                                                                      *
 * This function reads the MCLOUD information from an ASCII Mid Level 	*
 * Significant Weather file.						*
 *                                                                      *
 * cas_rdmcld ( ifpout, numele, ptrm, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of cloud groups  *
 *      **ptrm			mcloud_t	Pointer to mcloud struct*
 *      *memalc			int		Memory allocation flag  *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		09/04	Created                     		*
 ***********************************************************************/
{
    int		ii, jj, ij, ier, ierr;
    int         blen, num, numstr, llnum;
    int		ncnum, ntnum, tbnum, icnum, cbnum;
    char 	buff[256], **aryptr;

    Boolean	done;
    mcloud_t     *cld, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ierr    = 0;
    *numele = 0;
    *memalc = 0;
    done    = False;

   /*
    * Allocate memory for mcloud strings.
    */

    aryptr = (char **) malloc(sizeof(char *) * 9);
    for ( ii = 0; ii < 9; ii++ ) {
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
        * Look for mcloud element section.
	*/

	if (strcmp ( buff, "MCLOUD") == 0 ) {
	    done = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    *numele = num;

           /*
            * Found the mcloud section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( num > 0 ) {
                    new = ( mcloud_t *) malloc (sizeof ( mcloud_t ) );
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
		* Read non-Cb cloud distribution.
		*/

		cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_numb (buff, &ncnum, &ier);
                cld -> ncld = ncnum;

		if ( ncnum > 0 ) {
		    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
                    cst_clst ( buff, ' '," ", ncnum, MAXCH, aryptr, &numstr, &ier );

		    for ( ij = 0; ij < ncnum; ij++ ) {
			cst_numb ( aryptr[ij], &(cld -> ncdis[ij]), &ier );
		    }

		}

	       /*
                * Read non-Cb cloud type.
                */

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_numb (buff, &ntnum, &ier);
                cld -> ntyp = ntnum;

                if ( ntnum > 0 ) {
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
                    cst_clst ( buff, ' '," ", ntnum, MAXCH, aryptr, &numstr, &ier );

                    for ( ij = 0; ij < ntnum; ij++ ) {
                        cst_numb ( aryptr[ij], &(cld -> nctyp[ij]), &ier );
                    }

                }

	       /*
		* Read the number, base/top, and degree of turbulence.
		*/

		cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_numb (buff, &tbnum, &ier);
                cld -> turb = tbnum;

		if ( tbnum == 1 ) {
		    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
                    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		    cst_crnm (aryptr[0], &(cld -> tbase), &ier);
                    cst_crnm (aryptr[1], &(cld -> ttop), &ier);

		    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_numb (buff, &(cld -> tdeg), &ier);
		}

               /*
                * Read the number, base/top, and degree of icing.
                */

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_numb (buff, &icnum, &ier);
                cld -> icing = icnum;

                if ( icnum == 1 ) {
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
                    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
                    cst_crnm (aryptr[0], &(cld -> icbase), &ier);
                    cst_crnm (aryptr[1], &(cld -> ictop), &ier);

                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_numb (buff, &(cld -> dic), &ier);
                }

               /*
                * Read Cb number, base/top, distribution and type.
                */

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_numb (buff, &cbnum, &ier);
                cld -> fcb = cbnum;

                if ( cbnum == 1 ) {
                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
                    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
                    cst_crnm (aryptr[0], &(cld -> cbbase), &ier);
                    cst_crnm (aryptr[1], &(cld -> cbtop), &ier);

		    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
                    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		    cst_numb (aryptr[0], &(cld -> cbdis), &ier);
                    cst_numb (aryptr[1], &(cld -> cbtyp), &ier);
                }

	    }
	}
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *ptrm = head;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < 9; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
