#include "geminc.h"
#include "gemprm.h"

void ctb_prmt ( char *type, int *num, char alias[][73], 
			char vcord[][5], int level[], int *iret )
/************************************************************************
 * ctb_prmt								*
 *									*
 * This subroutine retrieves a list of GEMPAK parameter aliases of the	*
 * given data type.							*
 *									*
 * ctb_prmt ( type,  num, alias, vcord, level, iret )			*
 *									*
 * Input parameters:							*
 *	*type		char		Data type			*
 *									*
 * Output parameters:							*
 *	*num		int		Number of aliases in the list	*
 *	alias [NUM][73]	char		GEMPAK parameter aliases	*
 *	vcord [NUM][5]	char		Vertical coordinate system	*
 *	level [NUM]	int		Vertical levels			*
 *	*iret		int		Return code			*
 *					  As for cfl_ropn		*
 **									*
 * Log:									*
 * G. Krueger/EAI	 6/97						*
 ***********************************************************************/
{
	FILE	*ftbl;
	int	nflds, ier;
	char	buffer[133], rdtype[9], intype[9];

/*---------------------------------------------------------------------*/
	*iret = 0;

	*num = 0;

	cst_lcuc ( type, intype, &ier );
/*
 *	Open the table.
 */
	ftbl = cfl_tbop ( "prmlst.tbl", "config", iret );
	if ( *iret != 0 ) return;
/*
 *	Read in the next record, and process valid table entries.
 */
	while ( fgets ( buffer, 132, ftbl ) != NULL ) {
/*
 *	    Check for a ^ marker.
 */
	    if ( buffer[0] == '^' ) {
/*
 *		Parse the string by | separator.
 */
		nflds = sscanf ( buffer,
				 "^ %[^\n |] | %[^\n |] | %[^\n |] | %d",
				 alias[*num], rdtype, vcord[*num],
				 &level[*num] );
		if ( nflds > 1 ) {
		    if ( nflds < 4 ) {
			vcord[*num][0] = CHNULL;
			level[*num] = IMISSD;
		    }
/*
 *		    Check 2nd item against input type.  If a match,
 *		    increment NUM.
 */
		    if ( strcmp (rdtype, intype) == 0 ) (*num)++;
		}
	    }
	}

	return;
}
