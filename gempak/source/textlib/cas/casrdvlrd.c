#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

#define  MAXBRK   5

void cas_rdvlrd ( FILE *ifpout, int *numele, volrad_t **ptrv, 
                  int *memalc, int *iret )
/************************************************************************
 * cas_rdvlrd								*
 *                                                                      *
 * This function reads the volcano and radiation information from an 	*
 * ASCII High Level Significant Weather file.	To distinguish between  *
 * volcanos and radiation, a '_' is appended to the name for radiation  *
 * types.								*
 *                                                                      *
 * cas_rdvlrd ( ifpout, numele, ptrv, memalc, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*numele			int		Number of vol.rad groups*
 *      **ptrv			volrad_t	Pointer to volrad struct*
 *      *memalc			int		Memory allocation flag  *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        02/02   Created					*
 * A. Hardy/SAIC        03/02   Fix problem of overwriting linked list  *
 ***********************************************************************/
{
    int		ii, jj, ier, ierr, iend, istart;
    int         blen, num, numstr, nc, icnt;
    char 	buff[256], **aryptr, tmptyp[256], tmpnme[40];

    Boolean	done, found;
    volrad_t    *vol, *head, *new;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ierr    = 0;
    icnt    = 0;
    istart  = 0;
    *numele = 0;
    *memalc = 0;
    done    = False;
    found   = False;
    vol = head = NULL;

   /*
    * Allocate memory for strings.
    */

    aryptr = (char **) malloc(sizeof(char *) * MAXBRK);
    for ( ii = 0; ii < MAXBRK; ii++ ) {
        aryptr[ii] = (char *) malloc(MAXCH);
    }

   /*
    * Look for volcano element section.
    */

    while ( !done ) {
        cfl_rdln ( ifpout, sizeof(buff), buff, &iend );
        cst_rmbl (  buff, tmptyp, &blen, &ier );
	if ( ( strcmp ( tmptyp, "VOLCANO") == 0 ) ||
	     ( strcmp ( tmptyp, "RADIATION") == 0 ) ) {

            found   = True;
            cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
	    cst_numb (buff, &num, &ier);
	    icnt = icnt + num; 

           /*
            * Found the volcano section, begin reading each group.
	    */

	    for ( jj = 0; jj < num; jj++ ) {

	       /*
	        * Set up link list.
		*/

                if ( icnt > 0 ) {
                    new = ( volrad_t *) malloc (sizeof ( volrad_t ) );
                    *memalc = 1;
                    new -> next = NULL;
                    if ( istart > 0 ) {
                        vol -> next = new;
                        vol = new;
                    }
                    else {
                        vol = new;
                        head = vol;
                    }
		    istart = icnt;
                }

	       /*
	        * Store the name of the volcano or radiation event.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
		cst_lstr ( buff, &nc, &ier);
		cst_ncpy (tmpnme, buff, nc, &ier );
		cst_lstr ( tmptyp, &nc, &ier);
		if ( strcmp ( tmptyp, "RADIATION") == 0 ) {
		    strcat ( tmpnme, "_" );
		}
		strcpy ( vol -> name, tmpnme);

	       /*
	        * Read the latitude and longitude point.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_rxbl ( buff, buff, &blen, &ierr );
		cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
		cst_crnm ( aryptr[0], &vol->lat, &ier);
		cst_crnm ( aryptr[1], &vol->lon, &ier);

	       /*
	        * Read the volcano eruption time.
		*/

                cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
                cst_rxbl ( buff, buff, &blen, &ierr );
		cst_clst ( buff, ' '," ", MAXBRK, MAXCH, aryptr, &numstr, &ier );
	        cst_numb (aryptr[0], &vol->year, &ier);
	        cst_numb (aryptr[1], &vol->month, &ier);
	        cst_numb (aryptr[2], &vol->day, &ier);
	        cst_numb (aryptr[3], &vol->hour, &ier);
	        cst_numb (aryptr[4], &vol->minute, &ier);
	    }
	}
        if ( iend != 0 ) {
	    done = True;
        }
    }

   /*
    * Set the return pointer to begining of link list location.
    */

    *numele = icnt;
    *ptrv = head;
    if ( !found ) {
	*iret = 4;
    }

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < MAXBRK; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
