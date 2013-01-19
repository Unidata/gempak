#include "geminc.h"
#include "gemprm.h"

#include "vgstruct.h"

#define	MAX_CHARS	(65)	/* max # of characters on each formatted line */
#define NO_POINT_REORDER (-1 )
#define FZLVL_INDENT	 ( 7 )


int cgr_canBeFormatted ( int npts, float *xlat, float *ylon, char *prefix )
/************************************************************************
 * cgr_canBeFormatted							*
 *                                                                      *
 * This function determines whether a sequence of polygon points can	*
 * be formatted on three lines of text of MAX_CHARS characters each.	*
 * The representation is that of a prefixed 'FROM' or 'BOUNDED BY' line *
 * where the lat,lon points are converted to VOR proximity, 		*
 * i.e., '20SSE XYZ'.							*
 *                                                                      *
 * Note that the prefixed string must not have a trailing blank.	*
 *                                                                      *
 * cgr_canBeFormatted ( npts, xlat, ylon, prefix )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      npts		int             Number of points in polygon	*
 *      *xlat           float           Latitude                        *
 *      *ylon           float           Longitude                       *
 *      *prefix         char            Prefix string			*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *      canBeFormatted  int             G_TRUE or G_FALSE		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	09/06   Created                                 *
 * J. Wu/SAIC		10/06   Called cgr_cleanFmLine to make it 	*
 *				consistent with af_fmt2xml		*
 * D.W.Plummer/NCEP	11/06	Call cst_wrap to parallel actual format	*
 * E. Safford/SAIC	12/06	correctly format FZLVL prefix		*
 * B. Yin/SAIC		03/08	Added check for OPEN FZLVL		*
 ***********************************************************************/
{
    int			fromType, coordFlag, lineType, line, nret, ier;
    char		cloFmLine[STD_STRLEN * 2];
    char		FROMline[STD_STRLEN * 2];
    char		FROMlineOut[STD_STRLEN * 2];
    char		*cptr, sep[8];
    char		localPrefix[ STD_STRLEN ];

    Boolean		fzlvl;
/*---------------------------------------------------------------------*/

    fromType = SIGTYP_AREA;

    /*
     *  Build a clean FROM/BOUNDED BY line from the points.
     *  The default values are for a FROM line.
     */
    coordFlag = 5;
    lineType  = 0;
    strcpy( localPrefix, prefix );
    fzlvl = False;

    if ( strcasecmp( prefix,"BOUNDED BY" ) == 0 )  {
        coordFlag = 4;
        lineType  = 1;		/* BOUNDED BY line */
        strcpy( localPrefix, prefix );
    }
    else if( strcasecmp( prefix, "CLOSED FZLVL" ) == 0 ) {
        fromType = NO_POINT_REORDER;		/* closed FZLVL contour */
	coordFlag = 5;
	lineType  = 1; 
	strcpy(localPrefix, "       BOUNDED BY" );
	fzlvl = True;
    }
    else if( strcasecmp( prefix, "OPEN FZLVL" ) == 0 ) {
        fromType = NO_POINT_REORDER;		
	coordFlag = 5;
	lineType  = 1; 
	strcpy(localPrefix, "       ALG" );
	fzlvl = True;
    }

    strcpy ( cloFmLine, localPrefix );
    strcat ( cloFmLine, " " );

    clo_from ( GFA_ELM, fromType, npts, coordFlag, xlat, ylon, 
	    (int)sizeof(cloFmLine), &(cloFmLine[strlen(cloFmLine)]), &ier );

    clo_cleanFmLine( cloFmLine, lineType, FROMline, &ier );

    strcat ( FROMline, "\n" );


    if ( strchr(FROMline,'-') != (char *)NULL )  {
	strcpy ( sep, " |-" );
    }
    else  {
	strcpy ( sep, " " );
    }

    line = MAX_CHARS;
    if( fzlvl ) {
        line = line - FZLVL_INDENT;
    }
   
    cst_wrap ( FROMline, sep, &line, "\n", (char *)NULL, 
	       FROMlineOut, &ier );

    /*
     * FROMlineOut should have no more than 4 "\n" newlines to be able to
     * format on 3 lines (we added one on the end).
     */
    cptr = strstr ( FROMlineOut, "\n" );
    nret = 0;
    while ( cptr != (char *)NULL )  {
	nret += 1;
	cptr += 1;
	cptr = strstr ( cptr, "\n" );
    }

    if ( nret <= 3 )  {
	return ( G_TRUE );
    }
    else  {
	return ( G_FALSE );
    }
    
}
