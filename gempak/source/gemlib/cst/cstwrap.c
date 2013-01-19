#include "geminc.h"
#include "gemprm.h"

void cst_wrap ( char *str, const char *seps, const int *ilen, 
	const char *eol, const char *newLineStr, char *outstr, int *iret )
/************************************************************************
 * cst_wrap								*
 *									*
 * This routine will wrap long strings at ilen.  The input string and	*
 * the output string may be the same pointer.				*
 *									*
 * The end-of-line string is assumed to contain control characters such	*
 * as '\n'.  If the string begins with a "\", it is furthur assumed	*
 * that the string consists of literal control characters, eg. "\r\n",	*
 * and will be parsed and processed accordingly.			*
 *									*
 * The newLineStr defines the indent for lines after the first line. 	*
 * The first line is the FIRST string longer than "ilen" and without an	*
 * EOL sequence.  If newLineStr IS not NULL,  it is inserted at the	*
 * beginning of the second and 	subsequent lines.   			*
 *									*
 * Note that the pre-existence of EOL in the string is now recognized	*
 * and taken into consideration.					*
 *									*
 * CAUTION:  If ilen is less than any word in the string, then that 	*
 *	word will be broken up and the output string will be larger 	*
 *	than the input string in order to insert the needed return.	*
 *									*
 * It is the caller's responsibility to make sure that outstr has 	*
 * enough space to hold the orginal string and all possible inserts.	*
 * 									*
 * cst_wrap ( str, seps, ilen, eol, outstr, iret )			*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *	*seps		char		Seperator(s) for wrapping	*
 *	*ilen		int		Length of each row		*
 *	*eol		char		End-of-line string		*
 *	*newLineStr	char		Indent string for lines after 	*
 *						first line		*
 * Output parameters:							*
 *	*outstr		char		Wrapped string			*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * T. Piper/GSC		12/99	Initial coding				*
 * D.W.Plummer/NCEP	 2/00	Generalized for given EOL characters	*
 * D.W.Plummer/NCEP	 4/01	Added checks for existing EOL chars	*
 * T. Piper/SAIC	1/06	Added sep argument and logic		*
 * J. Wu/SAIC		04/06	Indent second & subsequent lines 	*
 * D.W.Plummer/NCEP	11/06	Added mulitple separator capability	*
***********************************************************************/
{
int 	ii, ibeg, iend, indx, iptr, eollen, seclen, nsep, ier;
int	iptr_tmp, iptr_chk;
char	*cptr, **sep, *seps_tmp, *result;
size_t	*lensep, lenstr;
/*---------------------------------------------------------------------*/
    
    if ( str != outstr )  strcpy(outstr, str);
   
    eollen = strlen ( eol );
    if ( eollen > *ilen || eollen == 0 ) {
	*iret = -1;
	return;
    }
    
    *iret = 0;

    /*
     * Determine the number of separators to check for
     */
    cptr = strchr(seps,'|');
    nsep = 1;
    while ( cptr != (char *)NULL )  {
	nsep += 1;
	cptr = strchr(cptr+1,'|');
    }

    /*
     * Allocate appropriate memory for the separators
     */
    G_MALLOC ( lensep, size_t, nsep, "Error allocating lensep" );
    G_MALLOC ( sep, char *, nsep, "Error allocating sep" );

    G_MALLOC ( seps_tmp, char, strlen(seps)+1, "Error allocating seps_tmp" );
    strcpy ( seps_tmp, seps );
    G_MALLOC ( result, char, strlen(seps)+1, "Error allocating result" );

    /*
     * Parse out the separators
     */
    cptr = seps_tmp;
    for ( ii = 0; ii < nsep; ii++ )  {
        cptr = cst_split( cptr, '|', sizeof(result), result, &ier );
	lensep[ii] = strlen(result);
	G_MALLOC ( sep[ii], char, lensep[ii]+1, "Error allocating sep[ii]" );
	strcpy ( sep[ii], result );
    }

    G_FREE ( seps_tmp, char );
    G_FREE ( result, char );
        
/*
 *  Search for first section of string longer than ilen and
 *  without an EOL sequence.
 */
    ibeg = 0;
    cptr = strstr(&(outstr[ibeg]), eol);
    while ( cptr != (char *)NULL && (int)(cptr-(&(outstr[ibeg]))) <= *ilen ) {
	ibeg = ibeg + (int)(cptr-(&(outstr[ibeg]))) + eollen;
	cptr = strstr(&(outstr[ibeg]), eol);
    }

    
    iptr = ibeg + *ilen;
    lenstr = strlen(outstr);
        
    while ( (size_t)iptr < lenstr ) {
	/*
	 * Find the most appropriate separator to use
	 */
	iptr_tmp = iptr;
	iptr = 0;
	for ( ii = 0; ii < nsep; ii++ )  {
	    iend = iptr_tmp - *ilen;
	    indx = iptr_tmp - lensep[ii] + 1;
	    iptr_chk = iptr_tmp;
	    while ( (iptr_chk > iend) ) {
	        if ( (int)(indx + lensep[ii]) <= ibeg+*ilen ) {
	            if ( strncmp(&outstr[indx], sep[ii], lensep[ii]) == 0 ) {
		        break;
		    }
	        }	
	        iptr_chk--;
	        indx--;
	    }
	    iptr = G_MAX ( iptr, iptr_chk );
	}
        	
	if ( iptr == iend ) {
	/*  
	 *  no separator (sep) found; word greater than ilen.
	 */
	    iptr = iptr + *ilen;
	    lenstr = lenstr + eollen;
	/*  
	 *  shift string over by eollen characters 
	 *  to insert eol string  
	 */
	    for (ii = lenstr; ii >= iptr; ii--) {
		outstr[ii] = outstr[ii-eollen];
	    }
	    for (ii = 0; ii < eollen; ii++) {
		outstr[iptr+ii] = eol[ii];
	    }
	}	
	else {
	/*  
	 *  separator (sep) found; insert EOL.
	 */
	    if ( outstr[iptr] == ' ' ) {
		if ( eollen == 1 )  {
		    outstr[iptr] = eol[0];
		}
	    	else {
		    lenstr = lenstr + eollen - 1;
		    for (ii = lenstr; ii >= iptr+eollen; ii--) {
		        outstr[ii] = outstr[ii-eollen+1];
                    }
		}
	    }
	    else {
		lenstr = lenstr + eollen;
		for (ii = lenstr; ii >= iptr+eollen; ii--) {
		    outstr[ii] = outstr[ii-eollen];
	        }
		iptr++;
	    }
	/*
	 *  add/insert EOL.
	 */
	    for (ii = 0; ii < eollen; ii++){
		outstr[iptr+ii] = eol[ii];
	    }
        }
  	
	iptr = iptr + eollen;
	
    /*
     *  shift the outstr and insert indentation string for next line
     */
	if ( newLineStr != (char *)NULL ) {
	    seclen = strlen ( newLineStr );	    
	    if ( seclen > 0 && seclen <= *ilen ) {		
		lenstr = lenstr + seclen;
	        for ( ii = lenstr; ii >= iptr + seclen; ii-- ) {
		    outstr[ii] = outstr[ii-seclen];
	        }

		for ( ii = 0; ii < seclen; ii++ ){
                    outstr[iptr+ii] = newLineStr[ii];
	        }	       
	    }
	}

	iptr = iptr + *ilen;
	      
    /*
     *  Search for next section of string longer than ilen and
     *  without an EOL sequence.
     */
	ibeg = iptr - *ilen;
	cptr = strstr(&(outstr[ibeg]), eol);
	while ( cptr != (char *)NULL && (int)(cptr-(&(outstr[ibeg]))) <= *ilen ) {
	    ibeg = ibeg + (int)(cptr-(&(outstr[ibeg]))) + eollen;
	    cptr = strstr(&(outstr[ibeg]), eol);
	}

	iptr = ibeg + *ilen;
    }    

    G_FREE ( lensep, size_t );
    for ( ii = 0; ii < nsep; ii++ )  {
	G_FREE ( sep[ii], char );
    }
    G_FREE ( sep, char * );

}
