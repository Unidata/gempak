#include "geminc.h"
#include "gemprm.h"

void grc_levl  ( const char *glevel, int *level1, int *level2, int *iret )
/************************************************************************
 * grc_levl								*
 *									*
 * This subroutine changes the user input for grid level into two	*
 * integers which represent the layer requested.  If no value or	*
 * invalid values are entered, the output level is set to -1.		*
 * LIST is no longer an option in this subroutine.			*
 *									*
 * grc_levl  ( glevel, level1, level2, iret )				*
 *									*
 * Input parameters:							*
 *	glevel		char 		Grid level input		*
 *									*
 * Output parameters:							*
 *	level1		int		First level of layer		*
 *	level2		int		Second level of layer		*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 *					 -2 = invalid input level	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/85						*
 * M. desJardins/GSFC	 9/88	GEMPAK4					*
 * D.W.Plummer/NCEP      1/05   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_levl ( (char *)glevel, level1, level2, iret, strlen(glevel) );

	return;
}
