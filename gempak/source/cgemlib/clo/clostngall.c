#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_stngall ( char *name, int maxstn, int *nstn, float *lat, 
			float *lon, char desc[][32], char state[][3], 
			char stnid[][9], int *stnnm, char country[][3],
			int *elv, int *pri, char c10[][20], 
			int *iret )
/************************************************************************
 * clo_stngall								*
 *									*
 * This function returns all parameter information currently indexed	*
 * by the station hotlist.  Note the appropriate lengths for strings.	*
 *									*
 * clo_stngall ( name, maxstn, nstn, lat, lon, desc, state, stnid,	*
 *			stnnm, country, elv, pri, c10, iret )		*
 *									*
 * Input parameters:							*
 *	*name		char	Name of 				*
 *	maxstn		int	Maximum allowed in the returned arrays	*
 *									*
 * Output parameters:							*
 *	*nstn		int	# of indices in hotlist			*
 *	*lat		float	Latitude array				*
 *	*lon		float	Longitude array				*
 *	desc[][32]	char	Station name or descriptor		*
 *	state[][3]	char	State					*
 *	stnid[][9]	char	Station ID				*
 *	*stnnm		int	Station number				*
 *	country[][3]	char	Country 				*
 *	*elv		int	Elevation in meters			*
 *	*pri		int	Priority				*
 *	c10[][20]	char	Column 10				*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 07/00	Created					*
 * B. Yin/SAIC		 09/05	Added country, pri and elv in the output*
 ***********************************************************************/
{
int	ii, which;
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( name );

    for ( ii = 0; ii < nhot; ii++ )  {

	if ( ii <= maxstn )  {

	    lat[ii] = clo.loc[which].stn.station[hotlist[ii]].lat;
	    lon[ii] = clo.loc[which].stn.station[hotlist[ii]].lon;
	    strcpy( desc[ii], clo.loc[which].stn.station[hotlist[ii]].desc );
	    strcpy( state[ii], clo.loc[which].stn.station[hotlist[ii]].state );
	    strcpy( stnid[ii], clo.loc[which].stn.station[hotlist[ii]].id );
	    stnnm[ii] = clo.loc[which].stn.station[hotlist[ii]].nm;
	    strcpy( c10[ii], clo.loc[which].stn.station[hotlist[ii]].col10 );
	    strcpy( country[ii], clo.loc[which].stn.station[hotlist[ii]].cntry );
	    elv[ii] = clo.loc[which].stn.station[hotlist[ii]].elv;
	    pri[ii] = clo.loc[which].stn.station[hotlist[ii]].pri;

	}

    }
    *nstn = G_MIN( nhot, maxstn );

}
