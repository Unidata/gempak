#include "geminc.h"
#include "gemprm.h"

#define	DEFAULT_TEST_STRING	"CST_GTAG DEFAULT"

void cst_rmtag ( const char *tag, char *str, int *iret )
/************************************************************************
 * cst_rmtag								*
 *									*
 * This subroutine removes a tag and its value from the str.  The tag   *
 * may or may not contain the starting '<' and ending '>'; they will be *
 * added if they are missing.						*
 * 									*
 * The str will be searched for the tag.  If it is found, both it and   *
 * the associated valued will be removed from str and the remaining     *
 * contents of str will be shifted to the left where tag and its value  *
 * were.  The iret will contain 0 if the tag was removed, -1 if it      *
 * wasn't found.							*
 *									*
 * cst_rmtag ( tag, str, iret )						*
 *									*
 * Input parameters:							*
 *	*tag		const char	Tag name			*
 *	*slen		int		Max length of str		*
 *									*
 * Input and Output parameters:						*
 *	*str		char		String containing <tag>tagvalues*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = unable to remove tag	*
 *					 -2 = 0 length input str	*
 **									*
 * Log:									*
 * E. Safford/SAIC	10/04	initial coding                         	*
 * E. Safford/SAIC	11/04	handle case of tag and no value		*
 * E. Safford/SAIC	04/07	fix valgrind source/dest warning	*
 * E. Safford/SAIC	05/07	fix memory leak                 	*
 ***********************************************************************/
{
char	*tag_start = { "<" };
char	*tag_end   = { ">" };

char	rmTag[ MXTAGN ], *rmStart = NULL, *rmEnd, *localStr;
int	end, ii, len;
Boolean	endFound = False;
/*---------------------------------------------------------------------*/

    *iret 	= 0;
    rmTag[0] 	= '\0';

    len = strlen( str );
    if( len <= 0 ) {
        *iret = -2;
        return;
    }
    
    G_MALLOC( localStr, char, len + 1, "cst_rmTag: localStr" );


    if( tag[0] != '<' ) {
        strcat( rmTag, tag_start ); 
    }
    strcat( rmTag, tag );

    end = strlen( tag );
    if( tag[ end-1 ] != '>' ) {
        strcat( rmTag, tag_end );
    }


    /*
     *  Find rmTag in str.  If it isn't found, return an error.
     */
    rmStart = strstr( str, rmTag );
    
    if( rmStart == NULL ) {
        *iret = -1;
        G_FREE( localStr, char );
	return;
    }

    /*
     *  Find the end of the value for rmTag.
     */
    endFound 	= False;
    ii 	     	= strlen( rmTag ) -1;
    rmEnd    	= NULL;

    while( !endFound ) {
	ii++;
        if( rmStart[ii] == '\\' ) {
            if( rmStart[ii+1] == '<' ) {
	        ii++;
            }
        }
	else if ( rmStart[ii] == '<' || rmStart[ii] == '\0' ) {
	    endFound = True;
	    rmEnd = &rmStart[ii];
        }
    }

    if( endFound ) {
        strcpy( localStr, rmEnd );
        strcpy ( rmStart, localStr ); 
    }
    else {
        *iret = -1;
    }

    G_FREE( localStr, char );
}
