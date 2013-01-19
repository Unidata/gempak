#include "geminc.h"
#include "gemprm.h"

#define	DEFAULT_TEST_STRING	"CST_GTAG DEFAULT"

void cst_stag ( const char *tag, const char *tagvalue, int *slen, char *str, int *iret )
/************************************************************************
 * cst_stag								*
 *									*
 * This subroutine sets a tag value within a string. If the tag name	*
 * already exists in the string, the value will be replaced; if the	*
 * tag name does not exist it will be added to the end of the string.	*
 * Embedded linefeeds and carriage returns are acceptable as value field*
 * terminators. The tagvalue valiable will be scanned for "<" and 	*
 * replaced with "\<" so as not to confuse it with the start of another *
 * tag. (The predeeding backslash will be stripped off by cst_gtag.)	*
 *									*
 * cst_stag ( tag, tagvalue, slen, str, iret )				*
 *									*
 * Input parameters:							*
 *	*tag		char		Tag name			*
 *	*tagvalue	void		Value of tag			*
 *	*slen		int		Max length of str		*
 *									*
 * Input and Output parameters:						*
 *	*str		char		String containing <tag>tagvalues*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = unable to add another	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 5/03						*
 * E. Safford/SAIC	10/04	allow tag to contain '<' '>' delimiters	*
 ***********************************************************************/
{
int	ier;
char	*cptr;
char	*tag_start = { "<" };
char	*tag_end   = { ">" };
int     back_slash = { '\\' };
char    bsts[1+(sizeof(tag_start)/sizeof(char))+1];
char	oldsubstr[MXTAGN+sizeof(tag_start)+sizeof(tag_end)+256+1];
char	newsubstr[MXTAGN+sizeof(tag_start)+sizeof(tag_end)+256+1];
char	oldvalue[256];

int	end;
/*---------------------------------------------------------------------*/

    *iret = 0;

    bsts[0] = (char)back_slash; bsts[1] = '\0';
    strcat ( bsts, tag_start );

    /*
     * First replace any tag_start in tagvalue with backslash-tag_start.
     */
    cptr = strstr(tagvalue, tag_start);
    while ( cptr != (char *)NULL )  {
	cst_rpst ( cptr, tag_start, bsts, cptr, &ier );
	cptr += 2;
	cptr = strstr(cptr, tag_start);
    }

    cst_gtag ( tag, str, DEFAULT_TEST_STRING, oldvalue, &ier );

    newsubstr[0] = '\0';

    /*
     *  Check for '<' at beginning of tag.  Add it if it isn't there.
     */
    if( tag[0] != '<' ) {
        strcat( newsubstr, tag_start );
    }

    strcat( newsubstr, tag );
   
    /*
     *  Check for '>' at end of tag.  Add it if it isn't there.
     */
    end = strlen(tag);
    if( tag[end-1] != '>' ) {
        strcat( newsubstr, tag_end );
    }

    strcat( newsubstr, tagvalue );

    if ( strcmp(oldvalue,DEFAULT_TEST_STRING) == 0 )  {

	/*
	 * Tag does not already exist.
	 */
	if ( strlen(tag_start) + strlen(tag) + strlen(tag_end) + 
		strlen(tagvalue) < (size_t)*slen )  {

            strcat ( str, newsubstr );

	}
	else  {
	    *iret = -1;
	    return;
	}

    }
    else  {

	/*
	 * Tag already exists; replace it, if possible.
	 */
	oldsubstr[0] = '\0';
	strcat( oldsubstr, tag_start );
	strcat( oldsubstr, tag );
	strcat( oldsubstr, tag_end );
	strcat( oldsubstr, oldvalue );
	cst_rpst ( str, oldsubstr, newsubstr, str, &ier );

    }

}
