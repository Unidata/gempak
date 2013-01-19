#include "nimcmn.h"

void nim_qatt ( int index, char imtype[], char iminfo[], 
					char imlutf[], int *iret )
/************************************************************************
 * nim_qatt								*
 *									*
 * This routine queries the attributes for the image data. The index	*
 * tells which set of attributes to query. The type and info describe	*
 * the data and	its directory. The LUT file is used to set the image	*
 * colors. 								*
 *									*
 * nim_qatt ( index, imtype, iminfo, imlutf, iret )			*
 *									*
 * Input parameters:							*
 *	index		int		Input index to arrays		*
 *									*
 * Output parameters:							*
 *	imtype[]	char		Type of image			*
 *	iminfo[]	char		Directory information		*
 *	imlutf[]	char		Color look up table		*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXIMG -> MAXTMPLT			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Check for a valid index value.
 */
	if  ( index < 0 || index >= MAXTMPLT )  {
	    *iret = -1;
	    return;
	}

/*
 *	Set the output values to the stored values.
 */
	strcpy ( imtype, image[index].type );
	strcpy ( iminfo, image[index].info );
	strcpy ( imlutf, image[index].lutf );

}
