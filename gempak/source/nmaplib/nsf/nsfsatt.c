#include "nsfcmn.h"

void nsf_satt ( int iindex, char alias[], int isbcat, 
		char cycle[], char parms[], char color[], 
		char filter[], char txtatt[], int *jindex, int *iret )
/************************************************************************
 * nsf_satt								*
 *									*
 * This routine sets the attributes for the SFC data. The input index	*
 * tells which set of attributes to set. If the value is < 0, the next	*
 * available index is used.						*
 *									*
 * nsf_satt ( iindex, alias, isbcat, cycle, parms, color, filter,	*
 *	      txtatt, jindex, iret )					*
 *									*
 * Input parameters:							*
 *	iindex		int		Input index to arrays		*
 *					   < 0 = Choose next available	*
 *	alias[]		char		Alias for SFC data		*
 *	isbcat		int		Data subcategory number		*
 *	cycle[]		char		Cycle time for the data		*
 *	parms[]		char		Parameter list			*
 *	color[]		char		Color list			*
 *	filter[]	char		Filter value			*
 *	txtatt[]	char		Text attributes			*
 *									*
 * Output parameters:							*
 *	*jindex		int		Output index to arrays		*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSFC -> MAXTMPLT			*
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
	    while ( (knt < MAXTMPLT) && (sfcdt[knt].alias[0] != CHNULL) ) {
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
	cst_lcuc ( alias, sfcdt[*jindex].alias, &ier );
	sfcdt[*jindex].isbcat = isbcat;

	if  ( strcmp ( cycle, "NONE" ) == 0 )  {
	    strcpy ( sfcdt[*jindex].cycle,  " " );
	}
	else {
	    strcpy ( sfcdt[*jindex].cycle,  cycle );
	}

	strcpy ( sfcdt[*jindex].parms,  parms );
	strcpy ( sfcdt[*jindex].color,  color );
	strcpy ( sfcdt[*jindex].filter, filter );
	strcpy ( sfcdt[*jindex].txtatt, txtatt );

/*
 *	Check for the index already in the list of indices.
 */
	found = G_FALSE;
	for ( knt = 0; knt < MAXTMPLT; knt++ ) {
	    if  ( indsfc[knt] == *jindex )  {
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
	    indsfc[*jindex] = *jindex;
	}
        else {
            if (sfcdt[*jindex].alias[0] == CHNULL  ) {
                indsfc[*jindex] = -1;
            }
        }
}
