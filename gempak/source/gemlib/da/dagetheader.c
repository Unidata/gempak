#include "da.h"

void da_getheader ( int *iflno, char *hdrtyp, char *cycle, char* forecast, int *nval, int *values,
		    int *iret )
/************************************************************************
 * da_getheader								*
 *									*
 * This function will return the row and column header data from a	*
 * non-GEMPAK data source.						*
 *									*
 * da_getheader ( iflno, hdrtyp, nval, values, iret )			*
 *									*
 * Input parameters:							*
 *	iflno		int*		GEMPAK file number		*
 *	hdrtyp		char*		Type of header - ROW or COL	*
 *									*
 * Output parameters:							*
 *	nval		int*		Number of header groups		*
 *	values		int*		Array of header values		*
 * 	iret		int*		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    int		ii, nhead, gflnum, ier;
    char	pyfile[MXFLSZ], pymeth[MXFLSZ], dbkey[MXFLSZ];
/*---------------------------------------------------------------------*/
    *iret = 0;

    gflnum = *iflno - 1;

    /* Set the function variables for either ROW or COL searches */
    if ( strcmp(hdrtyp, "ROW") == 0 ) {
	strcpy ( pyfile, common[gflnum].pyfile_row );
	strcpy ( pymeth, common[gflnum].pymeth_row );
	strcpy ( dbkey, common[gflnum].dbkey_row );
	nhead = common[gflnum].numrows + 1;
    }
    else if ( strcmp(hdrtyp, "COL") == 0 ) {
	strcpy ( pyfile, common[gflnum].pyfile_col );
	strcpy ( pymeth, common[gflnum].pymeth_col );
	strcpy ( dbkey, common[gflnum].dbkey_col );
	nhead = common[gflnum].numcols + 1;
    }
    else {
	*iret = -9;
	return;
    }

    /* Set the arguments for input to the request script */
    datype = DAINT;
    danarg = 5;
    daargs = (char **) malloc ( danarg * sizeof(char *) );
    for ( ii = 0; ii < danarg; ii++ ) {
	daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
    }
    ii = 0;
    strcpy ( daargs[ii], common[gflnum].dbserver );	ii++;
    strcpy ( daargs[ii], common[gflnum].dbtable );	ii++;
    strcpy ( daargs[ii], dbkey );		ii++;
    strcpy ( daargs[ii], cycle );		ii++;
    strcpy ( daargs[ii], forecast ); 		ii++;

    /* Run the Python script to make the actual request */
    da_runpy ( pyfile, pymeth, &ier );

    /* Assign the output array of integers and the number of headers */
    for ( ii = 0; ii < danumi; ii++ ) {
	values[ii] = daouti[ii];
    }
    *nval = danumi / nhead;

    /* Free all allocated memory */
    for ( ii = 0; ii < danarg; ii++ ) {
	free ( daargs[ii] );
    }
    free ( daargs );
    free ( daouti );
}
