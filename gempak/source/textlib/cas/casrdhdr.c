#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

# define HDLIN     4

void cas_rdhdr ( FILE *ifpout, int itime[], int jtime[], int *idcent,
		 float *chbase, float *chtop, int *iret )
/************************************************************************
 * cas_rdhdr								*
 *                                                                      *
 * This function reads the header information for an ASCII High Level 	*
 * Significant Weather file.						*
 *                                                                      *
 * cas_rdhdr ( ifpout, itime, jtime, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifpout 		FILE		ASCII file name		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	itime[]			int		Issue time array	*
 * 	jtime[]			int		Valid time array	*
 *	*idcent			int		Originating center id	*
 *	*chbase			float		Base level of chart	*
 *	*chtop			float		Top level of chart	*
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        01/02   Created					*
 * M. Li/SAIC		09/04	Added idcent, chbase and chtop		*
 * M. Li/SAIC		03/05	Expanded the check for idcent		*
 ***********************************************************************/
{
    int		ii, jj, ier, ierr;
    int         numstr, len;
    char 	buff[256], **aryptr;

/*---------------------------------------------------------------------*/
    *iret = 0;
    ierr  = 0;

   /*
    * Allocate memory for date/time strings.
    */

    aryptr = (char **) malloc(sizeof(char *) * 5);
    for ( ii = 0; ii < 5; ii++ ) {
        aryptr[ii] = (char *) malloc(MAXCH);
    }

   /*
    * Store the issue time and the valid time arrays.
    */

    for ( ii = 0 ; ii < HDLIN; ii++ ) {
        cfl_rdln ( ifpout, sizeof(buff), buff, &ierr );
        if ( ierr != 0 ) {
            *iret = ierr;
	    return;
        }

	cst_rxbl ( buff, buff, &len, &ier );
	if ( ii == 0 ) {
	    cst_numb ( buff, idcent, &ier );
	    if ( *idcent != 7 && *idcent != 74 && *idcent != 93) 
		*idcent = IMISSD;
	}
	else if ( ii == 1 ) {
	    cst_clst ( buff, ' '," ", 5, MAXCH, aryptr, &numstr, &ier );
	    
            for ( jj = 0 ; jj < 5; jj++ ) {
	        cst_numb ( aryptr[jj], &itime[jj], &ier );
	    }
	}
	else if ( ii == 2 ) {
	    cst_clst ( buff, ' '," ", 5, MAXCH, aryptr, &numstr, &ier );
	    
            for ( jj = 0 ; jj < 5; jj++ ) {
	        cst_numb ( aryptr[jj], &jtime[jj], &ier );
	    }
	}
	else if ( ii == 3 ) {
	    cst_clst ( buff, ' '," ", 2, MAXCH, aryptr, &numstr, &ier );
	    cst_crnm ( aryptr[0], chbase, &ier );
	    cst_crnm ( aryptr[1], chtop,  &ier );
	}
    }

   /*
    * Free allocated memory.
    */

    for ( ii = 0; ii < 5; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
}
