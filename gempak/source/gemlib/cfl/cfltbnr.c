#include "geminc.h"
#include "gemprm.h"

void cfl_tbnr ( FILE *fp, int *nr, int *iret )
/************************************************************************
 * cfl_tbnr								*
 *									*
 * This function determines the number of viable records in a table	*
 * file.  The file must be rewound to perform the count; it is also	*
 * rewound prior to returning.						*
 *									*
 * cfl_tbnr ( fp, nr, iret )						*
 *									*
 * Input parameters:							*
 *	*fp		FILE		Table file pointer		*
 *									*
 * Output parameters:							*
 *	*nr		int		Number of viable records	*
 *	*iret		int		Return code			*
 *					  -1 = file not opened		*
 **									*
 * D.W.Plummer/NCEP	 3/98						*
 * D.W.Plummer/NCEP	 4/98	Removed working buffer from calling	*
 *				sequence				*
 ***********************************************************************/
{
int	ier;
char	buff[256];
/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( fp == NULL )  {
	*iret = -1;
	return;
    }

    rewind ( fp );

    *nr = 0;

    cfl_trln(fp, sizeof(buff), buff, &ier);

    while ( !feof(fp) ) {

	(*nr)++;

        cfl_trln(fp, sizeof(buff), buff, &ier);

    }

    rewind ( fp );

}
