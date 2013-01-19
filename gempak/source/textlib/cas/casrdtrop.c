#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define    MAXBRK   3

void cas_rdtrop ( FILE *ifpout, int *numele, trop_t **ptrr, int *membx,
                  int *rnum, troplo_t **ptrl, int *memlo, int *lnum,
		  trophi_t **ptrh, int *memhi, int *hnum, int *iret )
/************************************************************************
 * cas_rdtrop								*
 *                                                                      *
 * This function reads the TROPOPAUSE information from an ASCII High 	*
 * Level or Mid level Significant Weather file.				*
 *                                                                      *
 * cas_rdtrop ( ifpout, numele, ptrr, membx, rnum, ptrl, memlo, lnum,   *
 *              ptrh, memhi, hnum, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 	FILE		ASCII file name			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele		int		Number of TURB groups   	*
 *      **ptrr		trop_t		Pointer to regular trop struct  *
 *      *membx		int		Reg trop Memory allocation flag *
 *      *rnum		int		Number of reg. trop. elements   *
 *      **ptrl		troplo_t	Pointer to low trop struct  	*
 *      *memlo		int		Low trop Memory allocation flag *
 *      *lnum		int		Number of low trop. elements    *
 *      **ptrh		trophi_t	Pointer to high trop struct     *
 *      *memhi		int		Hi trop Memory allocation flag  *
 *      *hnum		int		Number of hi. trop. elements    *
 *      *iret           int     	Return code             	*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        02/02   Created					*
 * M. Li/SAIC		09/04	Modifed to read Mid level TROP info	*
 ***********************************************************************/
{
    int		ii, jj, type, ier, ierr;
    int         blen, num, numstr, ibox;
    char 	buff[256], **aryptr;

    Boolean	done;
    trop_t      *trop, *head, *new;
    trophi_t    *trophi, *headh, *newh; 
    troplo_t    *troplo, *headl, *newl;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    *numele = 0;
    ierr    = 0;
    done    = False;

   /*
    * Allocate memory for TROP strings.
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
        * Look for topopause element section.
	*/

	if (strstr ( buff, "TROP") != (char *) NULL ) {
	    done = True;
	   /*
	    * Read number of tropopause types.
	    */
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &ibox, &ier);

	    for ( jj = 0; jj < ibox; jj++ ) {
		num = 0;

	       /*
	        * Read type of tropopause.
	        */
                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &type, &ier);

               /*
                * Found the turbulence section, begin reading each group.
	        */

	       /*
	        * Read number of tropopause values.
	        */
                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	        cst_numb (buff, &num, &ier);
	        *numele = *numele + num;

	        for ( ii = 0; ii < num; ii++ ) { 
	           /*
	            * Read the latitudes, longitudes and height values.
		    */

                    cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                    cst_rxbl ( buff, buff, &blen, &ierr );
		    cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );

	           /*
	            * Set up link list for regular trop. values.
		    */

                    if ( ( type == SIGIMSS ) && ( num > 0 ) ) {
                        new = ( trop_t *) malloc (sizeof ( trop_t ) );
                        *membx = 1;
                        new->next = NULL;
                        if ( ii != 0 ) {
                         trop -> next = new;
                         trop = new;
                        }
                        else {
                            trop = new;
                            head = trop;
                        }
			++*rnum;
		        cst_crnm ( aryptr[0], &trop -> lat, &ier);
		        cst_crnm ( aryptr[1], &trop -> lon, &ier);
		        cst_crnm ( aryptr[2], &trop -> level, &ier);
                    }

	           /*
	            * Set up link list for high trop. values.
		    */

		    else if ( ( type == 2 ) && ( num > 0 ) ) {
                        newh = (trophi_t *)malloc(sizeof(trophi_t));
                        *memhi = 1;
                        newh -> next = NULL;
                        if ( ii != 0 ) {
                            trophi -> next = newh;
                            trophi = newh;
                        }
                        else {
                            trophi = newh;
                            headh = trophi;
                        }
			++*hnum;
		        cst_crnm ( aryptr[0], &trophi -> lat, &ier);
		        cst_crnm ( aryptr[1], &trophi -> lon, &ier);
		        cst_crnm ( aryptr[2], &trophi -> level, &ier);
		    }

	           /*
	            * Set up link list for low trop. values.
    		    */

		    else if ( ( type == 3 ) && ( num > 0 ) ) {
                        newl = (troplo_t *)malloc(sizeof(troplo_t));
                        *memlo = 1;
                        newl -> next = NULL;
                        if ( ii != 0 ) {
                            troplo -> next = newl;
                            troplo = newl;
                        }
                        else {
                            troplo = newl;
                            headl = troplo;
                        }
			++*lnum;
		        cst_crnm ( aryptr[0], &troplo -> lat, &ier);
		        cst_crnm ( aryptr[1], &troplo -> lon, &ier);
		        cst_crnm ( aryptr[2], &troplo -> level, &ier);
		    }
		}
	    }
	}
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *ptrr = head;
    *ptrl = headl;
    *ptrh = headh;

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
