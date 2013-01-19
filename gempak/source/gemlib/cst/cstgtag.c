#include "geminc.h"
#include "gemprm.h"

void cst_gtag ( const char *tag, const char *str, const char *def,
						char *value, int *iret )
/************************************************************************
 * cst_gtag								*
 *									*
 * This subroutine gets a tag value from a string containing many tags.	*
 * In the variable str, tags must be prefixed and suffixed by < and >.	*
 * For instance, "<FIPS>30051<STATE>MT<TIME_ZONE>M<FE_AREA>nc" or	*
 * "<loop0:source0>    CAT_IMG|IMAGE/SAT/GOES-E/NAtl_16km/IR|10|0"	*
 * If the tag value contains a "<", it must be preceeded by a backslash	*
 * '\\' so as not to confuse it with the start of another tag.		*
 * The predeeding backslash will be stripped off prior to returning.	*
 * Embedded linefeeds and carriage returns are acceptable as value field*
 * terminators.	If the tag name is not found, the default value is 	*
 * returned and the return code is set to 3.				*
 *									*
 * Note that tag may or may not contain the '<' and '>' starting and    *
 * ending chars.  If it does not have them this routine will append     *
 * them.								*
 * 									*
 *									*
 * cst_gtag ( tag, str, def, value, iret )				*
 *									*
 * Input parameters:							*
 *	*tag		char		Tag name			*
 *	*str		char		String to search for tag name	*
 *	*def		char		Returned value if tag not found	*
 *									*
 * Output parameters:							*
 *	*value		char		Returned value of tag		*
 *	*iret		int		Return code			*
 *					  0 = normal, value found	*
 *					  3 = default returned		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 5/01						*
 * D.W.Plummer/NCEP	 6/01	Add positive return code; add MXTAGN	*
 * D.W.Plummer/NCEP	 5/03	Add "\<" check and replacement		*
 * E. Safford/SAIC	10/04	allow tag to contain '<' '>' delimiters *
 ***********************************************************************/
{
int	ier;
size_t	nc;
char	*cptr;
char	*stagp, *etagp;
char	*svalp, *evalp, *qvalp;
char	*tag_start = { "<" };
char	*tag_end   = { ">" };
int	back_slash = { '\\' };
char	bsts[1+(sizeof(tag_start)/sizeof(char))+1];
char	fulltag[MXTAGN+sizeof(tag_start)+sizeof(tag_end)+1];
int	end;
/*---------------------------------------------------------------------*/

    *iret = 0;

    bsts[0] = (char)back_slash; bsts[1] = '\0';
    strcat ( bsts, tag_start );

    /*
     * Formulate the full tag string.
     */
    fulltag[0] = '\0';

    /*
     *  Check for '<' at beginning of tag.  Add it if it isn't there.
     */
    if( tag[0] != '<' ) {
        strcpy(fulltag,tag_start); 
    }
    strcat(fulltag,tag); 
 
    /*
     *  Check for '>' at end of tag.  Add it if it isn't there.
     */
    end = strlen( tag );
    if( tag[ end-1 ] != '>' ) {
        strcat(fulltag,tag_end);
    }

    /*
     * Load output value string with default value.
     */
    strcpy ( value, def );

    /*
     * Search for tag name and return it's value, if found.
     * A fruitless search returns the default value.
     */
    stagp = strstr ( str, fulltag );

    if ( stagp != (char *)NULL )  {

	etagp = strstr ( stagp, tag_end );

	if ( etagp != (char *)NULL )  {

	    svalp = etagp + 1;

	    evalp = strchr ( svalp, CHNULL );

	    qvalp = strstr ( svalp, tag_start );

	    /*
	     * This section finds the next tag not preceded by
	     * a backslash (or finds the end of string).
	     */
	    while ( qvalp != (char *)NULL && *(qvalp-1) == back_slash )  {
		qvalp++;
	        qvalp = strstr ( qvalp, tag_start );
	    }

	    if ( qvalp != (char *)NULL && qvalp < evalp )  evalp=qvalp;
	    qvalp = strchr ( svalp, CHLF );
	    if ( qvalp != (char *)NULL && qvalp < evalp )  evalp=qvalp;
	    qvalp = strchr ( svalp, CHCR );
	    if ( qvalp != (char *)NULL && qvalp < evalp )  evalp=qvalp;

	    nc = (size_t)(evalp-svalp);
	    strncpy ( value, svalp, nc );
	    value[nc] = '\0';

	    /*
	     * Replace all backslash-tag_start combinations with tag_start.
	     */
	    cptr = strstr(value,bsts);
	    while ( cptr != (char *)NULL )  {
	        cst_rpst ( cptr, bsts, tag_start, cptr, &ier );
	        cptr = strstr(cptr,bsts);
	    }

 	    return;

	}

    }

    *iret = 3;

}
