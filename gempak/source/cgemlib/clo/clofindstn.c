#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

#define	EXACT	1
#define	FIRST	2
#define	INDEX	3

extern	CLO_t	clo;

void clo_findstn ( char *locnam, char *xid, char *xstate, int srchtyp, 
				int maxlen, int *nret, char *info, int *iret )
/************************************************************************
 * clo_findstn								*
 *									*
 * This function finds station information based on a station name.	*
 * A state PO abbreviation may also be given to	to narrow the choices.  *
 *									*
 * The returned info string contains the station information as a       *
 * string in the <TAG>value format.  Valid TAGs are:                    *
 * "STID", "STNM", "NAME", "ST", "CO", "LAT", "LON", "ELV", "PRI" and   *
 * "COL10". If the number of station information found is greater than  *
 * one, they are separated by ';'.                                      *
 *									*
 * clo_findstn (locnam, xid, xstate, srchtyp, maxlen, nret, info, iret) *
 *									*
 * Input parameters:							*
 *      *locnam         char       	Data location name		*
 *	*xid		char		Station name or substring	*
 *	*xstate		char		State name (optional)		*
 *	srchtyp		int		Search type			*
 *					= 1 - EXACT			*
 *					= 2 - FIRST			*
 *					= 3 - INDEX			*
 *	maxlen		int		Max length of info string	*
 *									*
 * Output parameters:							*
 *	*nret		int		Number of stations returned	*
 *	*info		char		String w/ station information	*
 *	*iret		int		Return code			*
 *					= 0 - normal			*
 *					= >0 - >maxret stations availbl	*
 *					= -2 - unable to match station	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/99	Created					*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * D.W.Plummer/NCEP	 4/99	Added MARINE and COASTAL types		*
 * D.W.Plummer/NCEP	 4/99	fix minor bug in binary srch algorithm	*
 * S. Jacobs/NCEP	11/99	Removed unused variables		*
 * D.W.Plummer/NCEP	 8/00	changes for modified CLO library	*
 * R.Tian/SAIC           7/03   Changed output format, tstr size, prolog*
 ***********************************************************************/
{
int	i, which, ihi, ilo, stptr, len, iptr, ipt1, ier;
char	tstr[128];
char	qid[64], qstate[16];

int	done, chkst, eqid;

Stn_t	*stn;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *nret = 0;

    /*
     *  Sort locnam stations by first column (station id)
     */
    clo_sortstn( locnam, STN_ID, &ier );

    which = clo_which ( locnam );

    stn = &(clo.loc[which].stn);

    /*
     *  Change lower case to upper case; also insert underscores
     */
    cst_lcuc( xid, qid, iret );
    for ( i = 0 ; i < (int)strlen(qid) ; i++ )
        if ( qid[i] == ' ' )  qid[i] = '_';
    cst_lcuc( xstate, qstate, iret );

    chkst = strlen( qstate ) != (size_t)0;

    if ( qid[0] == stn->station[0].id[0] || srchtyp == INDEX )  {
	stptr = 0;
    }
    else  {
    
        ihi = stn->nstn - 1;
        ilo = 0;
        stptr = (ihi+ilo) / 2;
        done = 0;
	iptr = stptr;
        while ( done == 0 && ihi > ilo )  {

	    if ( stn->station[iptr].id[0] == qid[0] )  {
	        ipt1 = stptr - 1;
		while ( stn->station[iptr].id[0] == stn->station[ipt1].id[0] ) {
		    stptr--;
		    iptr = stptr;
		    ipt1 = stptr - 1;
		}
	        done = 1;
	    }
	    else  {
	        if ( qid[0] < stn->station[iptr].id[0] )  ihi = stptr-1;
	        if ( qid[0] > stn->station[iptr].id[0] )  ilo = stptr+1;
    	        stptr = (ihi+ilo) / 2;
		iptr = stptr;
	    }
        }
    }

    info[0] = '\0';

    iptr = stptr;
    done = 0;
    while ( done == 0 )  {

	/*
	 *  Determine qualification based on search type
	 */
	switch ( srchtyp )  {

	    case FIRST:

		eqid = strncmp( stn->station[iptr].id, qid, strlen(qid) );

		break;

	    case INDEX:

		if ( strstr( stn->station[iptr].id, qid ) != (char *)NULL )  {
		        eqid = 0;
		}
		else  {
		        eqid = 1;
		}

		break;

	    case EXACT:
	    default:

		eqid = strcmp( stn->station[iptr].id, qid );

		break;

	}

	if ( ( eqid == 0 && !chkst )  ||
	     ( eqid == 0 && chkst && 
		strcmp( stn->station[iptr].state, qstate ) == 0 ) )  {

            sprintf( tstr, "<STID>%s<STNM>%d<NAME>%s<ST>%s<CO>%s"
			   "<LAT>%6.2f<LON>%6.2f<ELV>%d<PRI>%d<COL10>%s",
             	stn->station[iptr].id,
            	stn->station[iptr].nm,
            	stn->station[iptr].desc,
            	stn->station[iptr].state,
            	stn->station[iptr].cntry,
            	stn->station[iptr].lat,
            	stn->station[iptr].lon,
            	stn->station[iptr].elv,
            	stn->station[iptr].pri,
            	stn->station[iptr].col10 );

	    len = (int)(strlen(info) + strlen(tstr) + 2);
	    if ( len < maxlen )  {
		    /*
		     *  Save information into info string
		     */
		    if ( *nret > 0 )  {
		        strcat( info, ";" );
		    }

		    strcat( info, tstr );
	
		    (*nret)++;
		    
	    }
	    else  {
		    /*
		     *  Length exceeded; keep track of total number
		     */
		    if ( *iret == 0 )  {
			*iret = *nret + 1;
		    }
		    else  {
			(*iret)++;
		    }
	    }

	}

        stptr++;
        iptr = stptr;

	/*
	 *  Determine qualification based on search type
	 */
	switch ( srchtyp )  {

	    case INDEX:

		if ( stptr == stn->nstn )  done = 1;

		break;

	    case EXACT:
	    case FIRST:
	    default:

		if ( stptr == stn->nstn )  done = 1;

		if ( stptr < stn->nstn-1 )  {
		    if ( stn->station[iptr].id[0] != 
			 stn->station[stptr-1].id[0] ) 
			done = 1;
		}

		break;

	}

    }

    if ( *nret == 0 )  *iret = -2;

}
