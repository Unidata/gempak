#include "geminc.h"
#include "gemprm.h"
#define TPCGLOBAL
#include "ghcmn.h"


void gh_rtbl ( int *iret )
/************************************************************************
 * gh_rtbl								*
 *									*
 * This subroutine opens and reads the color table used by GPTPC.	*
 *									*
 * gh_rtbl ( iret )  							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		 6/01   					*
 * A. Hardy/GSC		 7/01	Use cfl_tinq to calc. table byte size   *
 * D. Kidwell/NCEP	 4/02	Added null at end of table              *
 ***********************************************************************/
{
    FILE	    *fp;
    int		    ier, nbin;
    long            nbytes;
    char            fnm[LLPATH], flnm[LLPATH];
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

   /*
    *  Open the table. If not found return an error.
    */

    strcpy ( fnm, "ghcolr.tbl" );
    fp = cfl_tbop(fnm, "hcnadv", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

   /*
    *   Find the size of the table file.
    */


    cfl_tinq ( fnm, "hcnadv", &nbytes, flnm, &ier);
    if ( ier != 0 ) {
        cfl_iret ( errno, iret, &ier );
        cfl_clos ( fp, &ier );
	return;
    }

   /*
    *   Read the table file.
    */

    table = (char *) malloc( (int) (nbytes + 1) );

    cfl_read ( fp, (int)nbytes, (unsigned char *)table, &nbin, iret );
    table [nbytes] = CHNULL;

   /*
    *   Close the table file.
    */

    cfl_clos ( fp, &ier );
}
