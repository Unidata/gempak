#include "geminc.h"
#include "gemprm.h"

#include "rsl.h"

#ifdef UNDERSCORE
#define im_rnex2   im_rnex2_
#endif

void	im_rnex2 ( char *infile, char *failid, char *sweepnum, char *radarfield,
		int *kx, int *ky, int *isorc, int *itype,
                int *idate, int *itime, float *range, float *belev, int *sweep_num,
		float *clat_dms, float *clon_dms, float *xmin, float *xmax, 
		float *ymin, float *ymax, float *vel_res, int *iret )
/************************************************************************
 * im_rcdf ( imgfil, kx, ky, isorc, itype, idate, itime, range,         *
 *        clat_dms, clon_dms, xmin, xmax, ymin, ymax, iret )     	*
 *                                                                      *
 * Input parameters:                                                    *
 *      imgfil[]        char            Name of image file              *
 *      sweepnum[]      char            sweep number              	*
 *      radarfield[]    char            radar field name                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *kx             int             x dimension of image            *
 *      *ky             int             y dimension of image            *
 *      *isorc          int                                             *
 *      *itype          int             type of channel                 *
 *      *idate          int             date                            *
 *      *itime          int             time                            *
 *      *range          float           maximum radar range             *
 *      *belev          float           mean beam elevation for sweep   *
 *      *sweep_num      int             sweep number			*
 *      *clat_dms       float           central latitude (dms)          *
 *      *clon_dms       float           central longitude (dms)         *
 *      *xmin           float           x - minimum for image           *
 *      *xmax           float           x - maximum for image           *
 *      *ymin           float           y - minimum for image           *
 *      *ymax           float           y - maximum for image           *
 *      *iret           int             return value                    *
 ***********************************************************************/
{
Radar *radar;
Sweep *sweep;
/*Volume *v;*/
Ray *ray;

char callid[5], cparm[8];

char    defdir[12];
long	lofset = 20;
FILE 	*fp;
int	nbin, ier, num;
int VINDEX=DZ_INDEX;
float sweep_angles[1], limit;

/*
 * see if we can get the station ID from bytes 21-24 
 */

defdir[0] = CHNULL;
fp = cfl_ropn ( infile, defdir, &ier );
if  ( ier != 0 )  {
    *iret = -1;
    return;
    }
else {
    cfl_seek ( fp, lofset, SEEK_SET, &ier );
    if  ( ier != 0 )  {
        *iret = -1;
        cfl_clos ( fp, &ier );
        return;
        }
    }

cfl_read ( fp, 4, (unsigned char *)callid, &nbin, &ier );
cfl_clos ( fp, &ier );

if ( ( nbin < 4 ) || ( callid[0] == 0)  || ( callid[0] == ' ' ) )
    {
    if( failid[0] != ' ' )
	{
	strncpy(callid, failid, 4);
        callid[4] = '\0';
        /*printf("Can't read station ID from file, using %s\n", callid);*/
        }
    else
        {
        sprintf(callid,"KFTG\0"); /* give something to be able to read the data */
        printf("Can't read station ID from file, %%SITE%% not found in template\n");
        }
    }
else
    callid[4] = '\0';

*iret = 0;

*isorc = 7;

RSL_select_fields(radarfield, NULL);

cst_lcuc(radarfield, cparm, &ier);
if ( cparm[0] == 'D' )
   VINDEX = DZ_INDEX;
else if ( cparm[0] == 'S' )
   VINDEX = SW_INDEX;
else if ( cparm[0] == 'V' )
   VINDEX = VR_INDEX;


*itype = 225 + VINDEX;

/*RSL_read_these_sweeps(sweepnum, NULL);*/
RSL_read_these_sweeps("all", NULL);

/*RSL_radar_verbose_on();*/

radar = RSL_wsr88d_to_radar(infile, callid);

/*RSL_radar_verbose_off();*/

if ( radar == NULL ) 
    {
    *iret = -1;
    return;
    }

cst_rlst ( sweepnum, ' ' , 0.5, 1, sweep_angles, &num, &ier);
limit = 20.0;
sweep = RSL_get_closest_sweep(radar->v[VINDEX], sweep_angles[0], limit);
if ( sweep == NULL )
    {
    printf("oops, sweep %f +/- %f is null\n",sweep_angles[0],limit);
    RSL_free_radar(radar);
    *iret = -1;
    return;
    }

*belev = sweep->h.elev;
*sweep_num = sweep->h.sweep_num;

*idate = radar->h.year * 10000 + radar->h.month * 100 + radar->h.day;
*itime = radar->h.hour * 10000 + radar->h.minute * 100 + (int)radar->h.sec;

*clat_dms = radar->h.latd + ( radar->h.latm / 60. ) + ( radar->h.lats / 3600.);
*clon_dms = radar->h.lond + ( radar->h.lonm / 60. ) + ( radar->h.lons / 3600.);

ray = RSL_get_first_ray_of_sweep(sweep);
if ( ray == NULL )
   {
   printf("oops, ray is null\n");
   RSL_free_radar(radar);
   *iret = -1;
   return;
   }

/*	
printf("gate size %d range bin1 %d lat %f lon %f nbins %d mo %d day %d year %d hour %d min %d\n",ray->h.gate_size,ray->h.range_bin1,ray->h.lat,ray->h.lon,ray->h.nbins,ray->h.month,ray->h.day,ray->h.year,ray->h.hour,ray->h.minute);
printf("look unambig %f %d velres %f \n",ray->h.unam_rng,ray->h.range_bin1,ray->h.vel_res);
*/


if (( VINDEX == VR_INDEX )||( VINDEX == SW_INDEX ))
    *vel_res = ray->h.vel_res;
else
    *vel_res = RMISSD;

*kx = ( 2.0 * ray->h.range_bin1 / ray->h.gate_size ) + ( 2.0 * ray->h.nbins );
*ky = *kx;
*range = ray->h.range_bin1 / 1000.0 + ( ray->h.nbins * ray->h.gate_size / 1000.0);

RSL_free_radar(radar);

*xmin = 1;
*xmax = (float)(*kx);
*ymin = 1;
*ymax = (float)(*ky);


return;
}
