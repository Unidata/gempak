#include "gbcmn.h"

void gb_gspk ( unsigned char *ptarray, int indxs, int length, int *xgrid )
/************************************************************************
 * gb_gspk								*
 *                                                                      *
 * This routine will read Grid data using Simple Packing.		*
 *                                                                      *
 * gb_gspk ( ptarray, indxs, length, xgrid )				*
 *									*
 * Input parameters:                                                    *
 *      *ptarray	unsigned char	Data buffer                     *
 *	indxs		int		Index into data buffer		*
 *	length		int		Length of the data section	*
 *                                                                      *
 * Output parameters:							*
 *	*xgrid		int		Array of grid data		*
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI          02/93                                           *
 * S. Jacobs/EAI        10/93	Copied from GBUTIL              	*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP	 3/96	GB_BTOI calling sequence		*
 * S. Jacobs/NCEP	 8/98	Changed xgrid from long int to int	*
 * T. Piper/GSC		11/98	Updated prolog				*
 ***********************************************************************/
{ 
	register int	j;
	int		i, counter, size, rmder, indx;
	unsigned char	buffer[4];

/*---------------------------------------------------------------------*/
	indx = indxs;

	counter = 0;
	rmder   = ( length - 11 ) % sizeof (int);
	size    = ( length - 11 ) / sizeof (int);
	if ( size == 0 ) {
	    return;
	}
	else {
	    for ( j = 0; j < size; j++ ) {
	    	xgrid[counter] = (int) gb_btoi(ptarray, indx, 4, FALSE);
		indx = indx + 4;
		counter++;
	    }
	} 
	if ( rmder > 0 ) {
		buffer[3]=buffer[2]=buffer[1]=buffer[0] = 0;
		for ( i = 0; i < rmder ; i++ ) {
			buffer[i] = ptarray[indx + i];
		}
		xgrid[counter] = (int) gb_btoi(buffer, 0, 4, FALSE);
	}

}
