#include "ngdcmn.h"

void ngd_satt ( int iindex, char alias[], int isbcat, 
		char cycle[], char rstfil[], int *jindex, int *iret )
/************************************************************************
 * ngd_satt								*
 *									*
 * This routine sets the attributes for the GRID data.  The input index	*
 * tells which set of attributes to set. If the value is < 0, the next	*
 * available index is used.						*
 *									*
 * ngd_satt ( iindex, alias, isbcat, cycle, rstfil, jindex, iret )	*
 *									*
 * Input parameters:							*
 *	iindex		int		Input index to arrays		*
 *					   < 0 = Choose next available	*
 *	alias[]		char		Alias for GRID data		*
 *	isbcat		int		Data subcategory number		*
 *	cycle[]		char		Cycle time for the data		*
 *	rstfil[]	char		Restore file (full pathname)	*
 *									*
 * Output parameters:							*
 *	*jindex		int		Output index to arrays		*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Law/GSC		06/00	MAXGRID -> MAXTMPLT			*
 * T. Piper/SAIC        06/03   Allowed for removing an index           *
 ***********************************************************************/
{

	int		knt, found, ier;
/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Start with the index provided.
 */
	*jindex = iindex;

/*
 *	If the index is less than 0, pick the first available index.
 */
	if  ( *jindex < 0 )  {

/*
 *	    Set the first available value for the index.
 */
	    knt = 0;
	    while ( (knt < MAXTMPLT) && (grddt[knt].alias[0] != CHNULL) ) {
		knt++;
	    }

	    *jindex = knt;
	}

/*
 *	If the index is bigger than the maximum value allowed,
 *	return with an error.
 */
	if  ( *jindex >= MAXTMPLT )  {
	    *iret = -1;
	    return;
	}

/*
 *	Set the structure values to the input values.
 */
	cst_lcuc ( alias, grddt[*jindex].alias, &ier );
	grddt[*jindex].isbcat = isbcat;

	if  ( strcmp ( cycle, "NONE" ) == 0 )  {
	    strcpy ( grddt[*jindex].cycle,  "*" );
	}
	else {
	    strcpy ( grddt[*jindex].cycle,  cycle );
	}

	strcpy ( grddt[*jindex].rstfil, rstfil );

/*
 *	Check for the index already in the list of indices.
 */
	found = G_FALSE;
	for ( knt = 0; knt < MAXTMPLT; knt++ ) {
	    if  ( indgrd[knt] == *jindex )  {
		found = G_TRUE;
		break;
	    }
	}

/*
 *	If this is a new index, set the index value in the
 *	global array and increase the count of indices set.
 *      If it is null remove index.
 */
	if  ( ! found ) {
	    indgrd[*jindex] = *jindex;
	}
        else {
            if ( grddt[*jindex].alias[0] == CHNULL ) {
                indgrd[*jindex] = -1;
            }
	}
}
