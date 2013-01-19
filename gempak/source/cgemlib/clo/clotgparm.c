#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_tgparm ( char *name, int icol, int maxret, char *sep, int *nret, 
						char *strout, int *iret )
/************************************************************************
 * clo_tgparm								*
 *									*
 * This function returns the value of a particular column based on the	*
 * name and hotlist value(s).  If the hotlist contains more than one	*
 * then the values will be concatenated using sep.			*
 *									*
 * clo_tgparm ( name, icol, maxret, sep, nret, strout, iret )		*
 *									*
 * Input parameters:							*
 *	*name		char	Bound name				*
 *	icol		int	Column to return			*
 *	maxret		int	Maximum number to return		*
 *	*sep		char	Separator if maxret > 1			*
 *									*
 * Output parameters:							*
 *	*nret		int	# of strings returned			*
 *	*strout		char	Information				*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 8/00	Created					*
 ***********************************************************************/
{
int	ii, ih, which;
char	ctmp[32];
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( name );

    *nret = 0;
    strout[0] = '\0';

    ii = 0;
    while ( ii < nhot  &&  ii < maxret )  {

	ih = hotlist[ii];

	if ( ii > 0 )  strcat ( strout, sep );

	switch ( icol )  {

	    case	1:
		strcat ( strout, clo.loc[which].stn.station[ih].id );
	        break;

	    case	2:
		sprintf( ctmp, "%d", clo.loc[which].stn.station[ih].nm );
		strcat ( strout, ctmp );
	        break;

	    case	3:
		strcat ( strout, clo.loc[which].stn.station[ih].desc );
	        break;

	    case	4:
		strcat ( strout, clo.loc[which].stn.station[ih].state );
	        break;

	    case	5:
		strcat ( strout, clo.loc[which].stn.station[ih].cntry );
	        break;

	    case	6:
		sprintf( ctmp, "%7.2f", clo.loc[which].stn.station[ih].lat );
		strcat ( strout, ctmp );
	        break;

	    case	7:
		sprintf( ctmp, "%7.2f", clo.loc[which].stn.station[ih].lon );
		strcat ( strout, ctmp );
	        break;

	    case	8:
		sprintf( ctmp, "%d", clo.loc[which].stn.station[ih].elv );
		strcat ( strout, ctmp );
	        break;

	    case	9:
		sprintf( ctmp, "%d", clo.loc[which].stn.station[ih].pri );
		strcat ( strout, ctmp );
	        break;

	    case	10:
		strcat ( strout, clo.loc[which].stn.station[ih].col10 );
	        break;

	}

	(*nret)++;

	ii++;

    }

}
