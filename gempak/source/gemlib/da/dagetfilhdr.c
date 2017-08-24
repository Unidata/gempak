#include "da.h"

void da_getfilhdr ( int *iflno, char *fhdnam, int *mxword,
			float *rheadr, int *nword, int *iret )
/************************************************************************
 * da_getfilhdr								*
 *									*
 * This function reads the real values for the given file header from	*
 * a non-GEMPAK data source.						*
 *									*
 * da_getfilhdr ( iflno, fhdnam, mxword, rheadr, nword, iret )		*
 *									*
 * Input parameters:							*
 *	iflno		int*		GEMPAK file number		*
 *	fhdnam		char*		Name of the file header section	*
 *	mxword		int*		Maximum words to return		*
 *									*
 * Output parameters:							*
 *	rheadr		float*		File header			*
 *	nword		int*		Header length			*
 *	iret		int*		Return code			*
 **									*
 * S. Jacobs/NCEP	 6/13	Created					*
 * S. Jacobs/NCEP	12/13	Save NX and NY for later use		*
 ************************************************************************/
{
    int		ii, jj, gflnum, ier;
    char	arrlen[12], model[MXFLSZ];
    char	pyfile[MXFLSZ], pymeth[MXFLSZ];
/*---------------------------------------------------------------------*/
    *iret = 0;

    gflnum = *iflno - 1;

    for ( jj = 0; jj < common[gflnum].numfhdrs; jj++ ) {
	if ( strcmp ( common[gflnum].fhdrs[jj].name, fhdnam ) == 0 ) {
	    strcpy ( model, common[gflnum].fhdrs[jj].dbkey );
	    strcpy ( pyfile, common[gflnum].fhdrs[jj].pyfile );
	    strcpy ( pymeth, common[gflnum].fhdrs[jj].pymeth );
	    cst_inch ( common[gflnum].fhdrs[jj].length, arrlen, &ier );
	}
    }

    /* Set the arguments for input to the request script */
    danarg = 4;
    daargs = (char **) malloc ( danarg * sizeof(char *) );
    for ( ii = 0; ii < danarg; ii++ ) {
	daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
    }
    ii = 0;
    strcpy ( daargs[ii], common[gflnum].dbserver );	ii++;
    strcpy ( daargs[ii], common[gflnum].dbtable );	ii++;
    strcpy ( daargs[ii], model );		ii++;
    strcpy ( daargs[ii], arrlen );		ii++;

    /* Get the header data */
    /* Run the Python script to make the actual request */
    datype = DAFLOAT;
    da_runpy ( pyfile, pymeth, &ier );

    /* Assign the output array of floats */
    for ( ii = 0; ii < danumf; ii++ ) {
	rheadr[ii] = daoutf[ii];
    }
    *nword = danumf;

    /* If this is a grid navigation, save the number of points */
    if ( strcmp ( fhdnam, "NAVB" ) == 0 ) {
	dacmn.nx = rheadr[4];
	dacmn.ny = rheadr[5];
    }

    /* Free all allocated memory */
    for ( ii = 0 ; ii < danarg; ii++ ) {
	free ( daargs[ii] );
    }
    free ( daargs );
    free ( daoutf );

}
