#include "da.h"

void da_getdtc ( char *station, char *time, int *partnum, int *iflno,
			int *idthdr, char *cdata, int *nchr, int *iret )
/************************************************************************
 * da_getdtc								*
 *									*
 * This function reads character data from a non-GEMPAK data source for the	*
 * given station and time.						*
 *									*
 * da_getdtc ( station, time, idthdr, cdata, nchr, iret )		*
 *									*
 * Input parameters:							*
 *	station		char*		Station ID for request		*
 *	time		char*		Date/Time for request		*
 *	partnum		int*		Part number (Fortran counting)	*
 *	iflno		int*		GEMPAK file number		*
 *									*
 * Output parameters:							*
 *	idthdr		int*		Data header array		*
 *	cdata		char*		Character string		*
 *	nchr		int*		Length of string		*
 *	iret		int*		Return code			*
 **									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    int		ii, jj, gflnum, ier;
    char	pyfile[MXFLSZ], pymeth[MXFLSZ];
    char	parms[1000], part[12];
/*---------------------------------------------------------------------*/
    *iret = 0;

    gflnum = *iflno - 1;

    /* Set the part number as a string */
    if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTAA") == 0 ) {
	strcpy ( part, "2020" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"PPBB") == 0 ) {
	strcpy ( part, "2021" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTBB") == 0 ) {
	strcpy ( part, "2022" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTCC") == 0 ) {
	strcpy ( part, "2030" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"PPDD") == 0 ) {
	strcpy ( part, "2031" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTDD") == 0 ) {
	strcpy ( part, "2032" );
    }
    else {
	strcpy ( part, "0" );
    }

    /* Set the list of parameter names */
    strcpy ( parms, common[gflnum].parts[*partnum-1].parms[0].key );
    jj = 1;
    while ( jj < common[gflnum].parts[*partnum-1].numparms ) {
	strcat ( parms, "," );
	strcat ( parms, common[gflnum].parts[*partnum-1].parms[jj].key );
	jj++;
    }

    /* Set the arguments for input to the request script */
    danarg = 6;
    daargs = (char **) malloc ( danarg * sizeof(char *) );
    for ( ii = 0; ii < danarg; ii++ ) {
	daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
    }
    ii = 0;
    strcpy ( daargs[ii], common[gflnum].dbserver );	ii++;
    strcpy ( daargs[ii], common[gflnum].dbtable );	ii++;
    strcpy ( daargs[ii], station );		ii++;
    strcpy ( daargs[ii], time ); 		ii++;
    strcpy ( daargs[ii], parms ); 		ii++;
    strcpy ( daargs[ii], part ); 		ii++;

    /* Get the data header */
    /* Run the Python script to make the actual request */
    datype = DAINT;
    strcpy ( pyfile, common[gflnum].parts[*partnum-1].pyfile );
    strcpy ( pymeth, common[gflnum].parts[*partnum-1].pymethhdr );
    da_runpy ( pyfile, pymeth, &ier );

    /* Assign the output array of integers */
    for ( ii = 0; ii < danumi; ii++ ) {
	idthdr[ii] = daouti[ii];
    }
    free ( daouti );

    /* Get the data */
    /* Run the Python script to make the actual request */
    datype = DACHAR;
    strcpy ( pyfile, common[gflnum].parts[*partnum-1].pyfile );
    strcpy ( pymeth, common[gflnum].parts[*partnum-1].pymethdata );
    da_runpy ( pyfile, pymeth, &ier );

    /* Assign the output string */
    strcpy ( cdata, daoutc );
    *nchr = strlen ( cdata );

    /* Free all allocated memory */
    for ( ii = 0 ; ii < danarg; ii++ ) {
	free ( daargs[ii] );
    }
    free ( daargs );
    free ( daoutc );

}
