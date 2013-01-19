#include "geminc.h"
#include "gemprm.h"

#define FROM_LINE	0
#define BOUNDED_BY_LINE	1

/*
 *  Private functions
 */
static char *clo_getNextVor      ( char *fromline, char *nextVor );


void clo_cleanFmLine( const char *fromLine, int lineType,
                        char *cleanFromLine, int *iret )
/************************************************************************
 * clo_cleanFmLine                                                	*
 *                                                                      *
 * This routine removes duplicate VOR relative points from the input    *
 * fromLine.  It copies all unique VOR-relative points into the         *
 * cleanFromLine string.  The search for duplicate points is not        *
 * sophisticated -- it assumes all duplicates will be adjacent to each  *
 * other in the fromLine string.                                        *
 *                                                                      *
 * Note that a valid fromLine has the first and last points repeated.   *
 * This closes the figure but is not considered a duplicate point.      *
 *                                                                      *
 *                                                                      *
 * void clo_cleanFmLine ( fromLine, lineType, cleanFromLine, iret )	*       
 *                                                                      *
 * Input parameters:                                                    *
 *      *fromLine       const char      input from line                 *
 *      lineType        int             FROM_LINE or BOUNDED_BY_LINE    *
 *                                                                      *
 * Output Parameters:                                                   *
 *      *cleanFromLIne  char            output from line                *
 *      *iret           int             return code                     *
 *                                         0 = normal                   *
 *                                         1 = removed dup point(s)     *
 *                                        -1 = less than 3 pts found    *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      04/05   Initial coding                          *
 * B. Yin/SAIC          10/05   Added new input parapmeter lineType     *
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06   moved from af_cleanFromLine     	*
 ***********************************************************************/
{
    char        lastVor[256];
    char        *fromLinePtr, *localFromLine;
    char        vors[25][256];
    int         numVors, ii, lastWrite;
/*---------------------------------------------------------------------*/

    *iret = 0;
    strcpy( cleanFromLine, "" );

    localFromLine = ( char * )
    malloc( sizeof( char ) * ( strlen( fromLine ) + 1 ));

    strcpy( localFromLine, fromLine );
    fromLinePtr = localFromLine;
    numVors = 0;

    while( fromLinePtr != NULL && numVors < 25 ) {
        fromLinePtr = clo_getNextVor( fromLinePtr, lastVor );
	strcpy( vors[ numVors ], lastVor );
	numVors++;
    }

    free( localFromLine );
    if ( numVors < 2 ) {
        *iret = -1;
    }
    else {
        strcpy( cleanFromLine, vors[ 0 ] );
        lastWrite = 0;

        for( ii=1; ii < numVors; ii++ ) {
            if( strcmp( vors[ lastWrite ], vors[ ii ] ) != 0 ) {

                if ( lineType == FROM_LINE ) {
	            strcat( cleanFromLine, " TO " );
                }
                else if ( lineType == BOUNDED_BY_LINE ) {
                    strcat( cleanFromLine, "-" );
		}

		strcat( cleanFromLine, vors[ ii ] );
		lastWrite = ii;
	    }
	    else {
	        *iret = 1;
	    }
	}
    }
}

/*=====================================================================*/

static char *clo_getNextVor( char *fromLine, char *vor )
/************************************************************************
 * clo_getNextVor                                                 	*
 *                                                                      *
 * This routine finds the first VOR-relative point string in the        *
 * fromline string.  The sequence that separates each VOR-relative      *
 * point in the from line is the " TO " string.                         *
 *                                                                      *
 * static char *clo_getNextVor ( fromLine, vor )                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *fromLine       const char      input from line                 *
 *                                                                      *
 * Output parameters:                                                   *
 *      *vor            *char           next VOR point string           *
 *                                                                      *
 * Return Value:                                                        *
 *                      *char           pointer to start of the next    *
 *                                        VOR string or NULL            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      04/05   Initial coding                          *
 * B. Yin/SAIC          10/05   check "-" for LLWS                      *
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06   moved from af_getNextVor     		*
 ***********************************************************************/
{
    char        *headPtr = NULL, *ptr = NULL;
    int         separatorLen = 0;
/*---------------------------------------------------------------------*/

    strcpy( vor, "" );

    if( fromLine != NULL ) {

        headPtr = fromLine;
        ptr = strstr( fromLine, " TO " );

        if ( ptr == NULL ) {
            ptr = strstr( fromLine, "-" );
            separatorLen = 1;
        }
        else {
            separatorLen = 4;
        }

        if( ptr != NULL ) {
            while( ( headPtr != ptr ) && ( strcmp( headPtr, "") != 0 ) ) {
	        strncat( vor, headPtr, 1 );
	        strcat( vor, "\0" );
	        headPtr++;
	    }
	    /*
	     *  Move headPtr from the start of the " TO " string to the 
	     *  beginning of the next VOR-relative point.
	     */
	    headPtr += separatorLen;
        }
        else {
            /*
             *  If ptr is NULL then there are no other " TO " strings
             *  in the fromLine.  The fromLine contains only one VOR
             *  relative string.  So copy the entire fromLine into vor.
             */
            strcpy( vor, fromLine );
            headPtr = NULL;
        }
    }

    return(  headPtr );
}

/*=====================================================================*/
