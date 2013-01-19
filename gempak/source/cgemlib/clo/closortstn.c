#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

static	int	_icol = -1;


void clo_sortstn ( char *locnam, int icol, int *iret )
/************************************************************************
 * clo_sortstn								*
 *									*
 * This function sorts station structures based on a column number.	*
 *									*
 * clo_sortstn (locnam, icol, iret) 					*
 *									*
 * Input parameters:							*
 *      *locnam         char       	Data location name		*
 *	icol		int		Column number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					= 0 - normal			*
 *					= >0 - >maxret stations availbl	*
 *					= -2 - unable to match station	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 ***********************************************************************/
{
int	ii, which;
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( locnam );

    if ( which == whichStn && icol == sortStn )  return;

    /*
     *  Sort the structure using icol as the determinant.
     *  Added (int(*)(const void*, const void*)) cast to satify qsort
     */

    _icol = icol;

    qsort( clo.loc[which].stn.station, (size_t)clo.loc[which].stn.nstn, 
	   sizeof(struct sinfo_t), (int(*)(const void*, const void*))stn_sort );

    /*
     *  Set up the lat,lon arrays for global CLO use.
     */

    npStn = clo.loc[which].stn.nstn;

    for ( ii = 0; ii < npStn; ii++ )  {
        latStn[ii] = clo.loc[which].stn.station[ii].lat;
        lonStn[ii] = clo.loc[which].stn.station[ii].lon;
    }

    whichStn = which;
    sortStn = icol;

}

/*=====================================================================*/

int stn_sort ( struct sinfo_t *stn1, struct sinfo_t *stn2 )
/************************************************************************
 * stn_sort                                                     	*
 *                                                                      *
 * This function compares strings within a stn structure.		*
 *                                                                      *
 * int stn_sort  ( stn1, stn2 )                                    	*
 *                                                                      *
 * Input parameters:                                                    *
 * *stn1	struct sinfo_t		struct sinfo_t element       	*
 * *stn2	struct sinfo_t		struct sinfo_t element       	*
 *                                                                      *
 * Output parameters:                                                   *
 * stn_sort	int          		Return code (ala strcmp)        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      3/99                                           *
 * T. Piper/GSC		 3/99	Corrected prolog			*
 ***********************************************************************/
{
    int ival;
/*---------------------------------------------------------------------*/

    switch ( _icol )  {

	case	STN_ID:		/* COLUMN 1 -- station id		*/

    	    ival = strcmp( stn1->id, stn2->id );

	    break;

	case	STN_NM:		/* COLUMN 2 -- station number		*/

    	    ival = ( stn1->nm < stn2->nm ) ? -1 : 1 ;

	    break;

	case	STN_DESC:	/* COLUMN 3 -- station descriptor	*/

    	    ival = strcmp( stn1->desc, stn2->desc );

	    break;

	case	STN_ST:		/* COLUMN 4 -- station state PO abbrv.	*/
				/* ties broken by column 3 descriptor	*/

    	    ival = strcmp( stn1->state, stn2->state );

	    if ( ival != 0 )  break;

	    ival = strcmp( stn1->desc, stn2->desc );

	    break;

	case	STN_LAT:	/* COLUMN 6 -- stn lat (south->north)	*/

    	    ival = ( stn1->lat < stn2->lat ) ? -1 : 1 ;

	    break;

	case	STN_LON:	/* COLUMN 7 -- stn long (east->west)	*/

    	    ival = ( stn1->lon > stn2->lon ) ? -1 : 1 ;

	    break;

	case	STN_ELV:	/* COLUMN 8 -- station elev (lo->hi)	*/

    	    ival = ( stn1->elv < stn2->elv ) ? -1 : 1 ;

	    break;

	case	STN_PRI:	/* COLUMN 9 -- station prior (lo->hi)	*/

    	    ival = ( stn1->lon < stn2->lon ) ? -1 : 1 ;

	    break;

	case	STN_COL10:	/* COLUMN 10 -- column 10 ( string)  	*/

    	    ival = strcmp( stn1->col10, stn2->col10 );

	    break;

	default:		/* DEFAULT -- do nothing		*/

	    break;

    }

    return (ival);
}
