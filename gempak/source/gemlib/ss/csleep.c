#include "geminc.h"
#include "gemprm.h"

void csleep  ( float *nsec, int *iret )
/************************************************************************
 * csleep								*
 *									*
 * This subroutine is used by UNIX systems to wait for a variable	*
 * number of seconds.							*
 *									*
 * csleep ( nsec, iret )						*
 *									*
 * Input parameters:							*
 *	*nsec		float		Number of seconds		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  G_NORMAL = normal return	*
 **									*
 * Log:									*
 * M. desJardins/NMC	 3/91						*
 * S. Jacobs/EAI	 8/93	Use unsigned int in SLEEP		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * T. Piper/SAIC	10/04	Changed nsec to float			*
 ***********************************************************************/
{
	double ip, fp;
	unsigned int usec;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	fp = modf( (double)*nsec, &ip );
	usec = (unsigned int)(fp * 1000000.0);

	sleep ( (unsigned int)ip );
	usleep ( usec );

}
