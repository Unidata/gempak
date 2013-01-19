#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t		clo;

void clo_tgst ( char *name, int maxlen, int *nst, char *states, int *iret )
/************************************************************************
 * clo_tgst								*
 *									*
 * This function returns all parameter information currently indexed    *
 * by the county hotlist.						*
 *									*
 * clo_tgst  ( name, maxlen, nst, states, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char	Location name				*
 *	maxlen		int	Maximum allowed in the returned arrays	*
 *									*
 * Output parameters:							*
 *	*nst		int	# of indices in hotlist			*
 *	*states		char	States array				*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/00	Created					*
 * D.W.Plummer/NCEP	 8/00	Updated for revised CLO library		*
 ***********************************************************************/
{
int	i, which, ier;
FILE    *fp;
char    fnm[132], buff[128];
char	cdum[40];
char	st[4], *stnst;
/*---------------------------------------------------------------------*/
    *iret = 0;

    *nst = 0;
    states[0] = '\0';

    which = clo_which ( name );
    if ( which < 0 )  {
        *iret = -2;
        return;
    }

    switch ( clo.loc[which].format )  {

        case 0:		/*  Standard station table format        */

	    for ( i = 0; i < nhot; i++ )  {

	      stnst = clo.loc[which].stn.station[hotlist[i]].state;

              if ( (int)strlen(states)+3 < maxlen )  {

		if ( i > 0 )  strcat ( states, ";" );
		strcat ( states, stnst );
    	        (*nst)++;

	      }

            }

	    break;

        case 1:		/*  Extract via bounds structure */

	    strcpy(fnm, clo.loc[which].bnd.filename);

	    fp = (FILE *)cfl_tbop(fnm, "bounds", &ier);
            if ( fp == NULL  ||  ier != 0 )  {
                *iret = -1;
                return;
            }

	    for ( i = 0; i < nhot; i++ )  {

              if ( (int)strlen(states)+3 < maxlen )  {

		cfl_seek ( fp, hotstrec[i], 0, &ier );
		cfl_trln( fp, sizeof(buff), buff, &ier );
	        sscanf( buff, "%s %s %s %s", cdum, cdum, cdum, st );

		if ( i > 0 )  strcat ( states, ";" );
		strcat ( states, st );

    	        (*nst)++;

              }

            }

	    cfl_clos ( fp, &ier );

	    break;

    }

}
