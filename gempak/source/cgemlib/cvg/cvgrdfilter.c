#define	FILTER_STORAGE

#include "cvgcmn.h"

#define	FILTER_TBL	"filter.tbl"

static int	filterLoaded = False;

/*
 *  Private functions
 */
static void _fillStrArray ( int max_strarr, int max_string,
              char origstr[], int *nstrarr, char strarr[][DSPLY_FILTER_SZ] );

static void _vrfyTimes ( char *instr, int *iret );


void cvg_rdfilter ( int *iret )
/************************************************************************
 * cvg_rdfilter								*
 *									*
 * This function reads in all valid entries in the filter table.	*
 *									*
 * cvg_rdfilter ( filter, iret )					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = Normal  			*
 *					 -1 = unable to open table file	*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/06   initial coding         			*
 ***********************************************************************/
{
    int		ier;
    char	buff[256], filtype[12], str[256], filstr[8];
    FILE	*fp;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    /*
     *  Read the table only once.
     */
    if ( filterLoaded )  return;
    
    
    /*
     *  Initialize.
     */
    nTblFilter = 0;
            
    
    /*
     *  Set the flag to True.
     */
    filterLoaded = True;


    /*
     *  Open the filter table. If not found, return an error.
     */
    fp = cfl_tbop ( FILTER_TBL, "pgen", &ier );
    if ( fp == NULL || ier != 0 ) {
        printf ( "not loaded ....\n" );
        *iret = -1;
        return;
    }
    
    /*
     *  Scan table line-by-line and load valid entries.
     */
    while ( !feof(fp) ) {

	cfl_trln ( fp, sizeof(buff), buff, &ier );

	if ( ier == 0 ) {

	    sscanf ( buff, "%s %s", filstr, filtype );

	    if ( strcmp ( filstr, "FILTER" ) != 0 ) {
		continue;	/* Skip invalid lines */
	    }
            
	    if ( ( strcmp ( filtype, "TIME") == 0 ) && 
	         ( nTblFilter < MAX_FILTER_NUM ) ) {
		sscanf ( buff, "%*s %*s %s", str );
		_fillStrArray ( MAX_FILTER_NUM, DSPLY_FILTER_SZ, str, 
				&nTblFilter, tblFilter );
	    }	    	    	    
	}
    }

    cfl_clos ( fp, &ier );

}

/****************************************************************************/

static void _fillStrArray ( int max_strarr, int max_string,
              char origstr[], int *nstrarr, char strarr[][DSPLY_FILTER_SZ] )
/************************************************************************
 * _fillStrArray							*
 *									*
 * This function copies a string of multiple entries delimited by a ';'	*
 * to an array of strings. The duplicate entries will count only once.	*
 *									*
 * static void _fillStrArray (max_strarr, max_string, origstr,		*
 *						nstrarr, strarr )	*
 *									*
 * Input parameters:							*
 *	max_strarr	int	maximum array size			*
 *	max_string	int	maximum string length			*
 *	origstr[]	char	the original string			*
 *									*
 * Output parameters:							*
 *	*nstrarr		  int	current number in array		*
 *	strarr[][DSPLY_FILTER_SZ] char	the array of strings		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/06	move from nmap_pgfilter.c		*
 ***********************************************************************/
{
    int		last, ier, ii;
    char	*ptr;
    Boolean	entry_exist;
/*---------------------------------------------------------------------*/

    ptr = strtok ( origstr, ";" );
    last = max_string - 1;
    
    while ( (ptr != (char *)NULL) && (*nstrarr < max_strarr) ) {
	
	strncpy ( strarr[*nstrarr], ptr, (size_t)last );
	strarr[*nstrarr][last] = '\0';

        _vrfyTimes ( strarr[*nstrarr], &ier );

	ptr = strtok ( NULL, ";" );	
	
	/*
	 *  Check if the entry already exists.
	 */ 
        entry_exist = False;
	if ( ier >= 0 ) {	
	    for ( ii = 0; ii < *nstrarr; ii++ ) {
	        if ( strcmp ( strarr[*nstrarr], strarr[ii]) == 0 ) {
		    entry_exist = True;
		    break;
	        }
	    }
	    	    
	    if ( !entry_exist ) (*nstrarr)++;
        }
    }    
}

/*=====================================================================*/

static void _vrfyTimes ( char *instr, int *iret )
/************************************************************************
 * _vrfyTimes								*
 *									*
 * Validates a time filter string. Valid entries include "AIRM", "OTLK",*
 * and time strings in forms of "HH+", and "HH-HH".  Note that '+', '-',*
 * and the "HH" after the '-' are optional; e.g, "1", "2+", "2-", and 	*
 * "2-3" are valid entries ( "2-" will be truncated as "2" ). 		*
 *									*
 * static void _vrfyTimes ( instr, iret )				*
 *									*
 * Input/Output parameters:						*
 *	*instr		char	string to be verified. It will be cut	*
 *				off at the proper position after check.	*
 *									*
 * Output parameters:							*
 *	*iret 		int	return code				*
 *					 0 - Normal			*
 *					-1 - Invalid entry		*
 *					 1 - Partially valid entry	*
 *					     cut off at postion "pos"	*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/06	move from nmap_pgfilter.c and allow	*
 *				"AIRM", "OTLK", & '+'			*
 ***********************************************************************/
{
    int 	ii, dashPos, plusPos, len, ier;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    /*
     *  Null string or string starting with non-digit char are invalid 
     *  except "AIRMET" and "OTLK".
     */
    if ( !instr ) {
        *iret = -1;    
	return;    
    } 
    
    if ( !isdigit(instr[0]) )  {
        if ( !strcmp( instr, "AIRM" ) == 0 &&
	     !strcmp( instr, "OTLK" ) == 0 ) {	
	    *iret = -1;
            instr[0] = '\0';
	}
	
	return;
    }
    

    /*
     *  One '+' is acceptable as the 2nd or 3rd char.
     */
    cst_nocc ( instr, '+', 1, TRUE, &plusPos, &ier );
    if ( ier == 0 ) {
	if ( plusPos > 2 ) {
	    instr[ plusPos ] = '\0';		
	}
	else { 
	    if ( plusPos == 1 || 
	        ( plusPos == 2 && isdigit( instr[1] ) ) ) { 
	        instr[ plusPos + 1 ] = '\0';	
	    }
	    else {
	        *iret = -1;
                instr[0] = '\0';
	    }
	    
	    return;
	}    
    }
    

    /*
     *  One '-' is acceptable as the 2nd, 3rd, or 4th char. 
     */
    cst_nocc ( instr, '-', 1, TRUE, &dashPos, &ier );
    
    len = strlen ( instr );
    if ( dashPos == 0 || dashPos > 3 ) {
        for ( ii = 0; ii < 3; ii++ ) {
	    if ( !isdigit(instr[ii]) ) break;	
	}
    }
    else {	/* dashPos == 1, 2, or 3 */
        for ( ii = dashPos+1; ii < G_MIN(len,dashPos+4); ii++ ) {            
	    if ( !isdigit(instr[ii]) ) {
                break;	
	    }
	}
	
	if ( ii == (dashPos+1) ) ii--;
	
    }	
               
    instr[ii] = '\0';
    if ( ii != len ) {	
        *iret = 1;	/* indicate cutoff at position "ii" */
    }
           
}

/*=====================================================================*/
