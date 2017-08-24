#include "da.h"

void da_getdtrgd ( char *cycle, char* forecast,
		    char *level1, char *level2,
		    char *gvcord, char *gparm, int *iflno,
		    int *idthdr, float *rdata, int *nval, int *iret )
/************************************************************************
 * da_getdtrgd								*
 *									*
 * This function reads real grid data from a non-GEMPAK data	source for the	*
 * given station and time.						*
 *									*
 * da_getdtrgd ( cycle, forecast, level1, level2, gvcord, gparm,	*
 *		 idthdr, rdata, nval, iret )				*
 *									*
 * Input parameters:							*
 *	cycle		char*		Cycle date/time			*
 *	forecast	char*		Forecast time in seconds	*
 *	level1		char*		Level 1				*
 *	level2		char*		Level 2				*
 *	gvcord		char*		Vertical coordinate		*
 *	gparm		char*		Grid parameter			*
 *	iflno		int*		GEMPAK file number		*
 *									*
 * Output parameters:							*
 *	idthdr		int*		Data header array		*
 *	rdata		float*		Array of real data		*
 *	nval		int*		Length of data array		*
 *	iret		int*		Return code			*
 **									*
 * S. Jacobs/NCEP	 6/13	Created					*
 * S. Jacobs/NCEP	12/13	Added NX and NY to the script call	*
 ************************************************************************/
{
    int		ii, gflnum, ier;
    char	pyfile[MXFLSZ], pymeth[MXFLSZ];
/*---------------------------------------------------------------------*/
    *iret = 0;

    gflnum = *iflno - 1;

    /* Set the arguments for input to the request script */
    danarg = 11;
    daargs = (char **) malloc ( danarg * sizeof(char *) );
    for ( ii = 0; ii < danarg; ii++ ) {
	daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
    }
    ii = 0;
    strcpy ( daargs[ii], common[gflnum].dbserver );	ii++;
    strcpy ( daargs[ii], common[gflnum].dbtable );	ii++;
    strcpy ( daargs[ii], common[gflnum].dbkey_row );	ii++; /*model alias*/
    strcpy ( daargs[ii], cycle );		ii++;
    strcpy ( daargs[ii], forecast ); 		ii++;
    strcpy ( daargs[ii], level1 ); 		ii++;
    strcpy ( daargs[ii], level2 ); 		ii++;
    strcpy ( daargs[ii], gvcord ); 		ii++;
    strcpy ( daargs[ii], gparm ); 		ii++;
    sprintf ( daargs[ii], "%d", dacmn.nx ); 	ii++;
    sprintf ( daargs[ii], "%d", dacmn.ny ); 	ii++;

    /* Get the data header */
    /* Run the Python script to make the actual request */
    datype = DAINT;
    strcpy ( pyfile, common[gflnum].parts[0].pyfile );
    strcpy ( pymeth, common[gflnum].parts[0].pymethhdr );
    da_runpy ( pyfile, pymeth, &ier );

    /* Assign the output array of integers */
    for ( ii = 0; ii < danumi; ii++ ) {
	idthdr[ii] = daouti[ii];
    }
    free ( daouti );

    /* Get the data */
    /* Run the Python script to make the actual request */
    datype = DAFLOAT;
    strcpy ( pyfile, common[gflnum].parts[0].pyfile );
    strcpy ( pymeth, common[gflnum].parts[0].pymethdata );
    da_runpy ( pyfile, pymeth, &ier );

    /* Assign the output array of floats */
    for ( ii = 0; ii < danumf; ii++ ) {
	rdata[ii] = daoutf[ii];
    }
    *nval = danumf;

    /* Free all allocated memory */
    for ( ii = 0 ; ii < danarg; ii++ ) {
	free ( daargs[ii] );
    }
    free ( daargs );
    free ( daoutf );

}
