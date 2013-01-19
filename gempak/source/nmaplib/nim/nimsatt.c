#include "nimcmn.h"

void nim_satt ( int iindex, char imtype[], char iminfo[], 
				char imlutf[], int *jindex, int *iret )
/************************************************************************
 * nim_satt								*
 *									*
 * This routine sets the attributes for the image data. The input index	*
 * tells which set of attributes to set. If the value is < 0, the next	*
 * available index is used. The type and info describe the data and	*
 * its directory. The LUT file is used to set the image colors. The	*
 * index to the attributes is returned.					*
 *									*
 * nim_satt ( iindex, imtype, iminfo, imlutf, jindex, iret )		*
 *									*
 * Input parameters:							*
 *	iindex		int		Input index to arrays		*
 *					   < 0 = Choose next available	*
 *	imtype[]	char		Type of image			*
 *	iminfo[]	char		Directory information		*
 *	imlutf[]	char		Color look up table		*
 *									*
 * Output parameters:							*
 *	*jindex		int		Output index to arrays		*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXIMG -> MAXTMPLT			*
 * T. Piper/SAIC	06/03	Allowed for removing an index		*
 ***********************************************************************/
{

	int		knt, found;
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
	    while ( (knt < MAXTMPLT) && (image[knt].type[0] != CHNULL) ) {
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
	strcpy ( image[*jindex].type, imtype );
	strcpy ( image[*jindex].info, iminfo );
	strcpy ( image[*jindex].lutf, imlutf );

/*
 *	Check for the index already in the list of indices.
 */
	found = G_FALSE;
	for ( knt = 0; knt < MAXTMPLT; knt++ ) {
	    if  ( indimg[knt] == *jindex )  {
		found = G_TRUE;
		break;
	    }
	}

/*
 *	If this is a new index, set the index value in the
 *	global array and increase the count of indices set.
 *	If it is null remove index.
 */
	if  ( ! found ) {
	    indimg[*jindex] = *jindex;
	}
	else {
	    if ( image[*jindex].type[0] == CHNULL ) {
		indimg[*jindex] = -1;
	    }
	}		
}
