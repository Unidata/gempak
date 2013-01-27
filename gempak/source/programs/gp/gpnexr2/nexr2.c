#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <limits.h>

#include "rsl.h"

#ifdef UNDERSCORE
#define nexr2   nexr2_
#endif

void    nexr2 (char *infile)
{
Radar *radar;
Sweep *sweep;
Ray   *ray;

int xdim, ydim, i;
float range;
float x, fval;
int minval, maxval, intval;

unsigned char *radData;

char callid[]="KFTG";

printf("I am here %s \n",infile);

RSL_select_fields("dz", NULL); 
RSL_read_these_sweeps("0", NULL);

radar = RSL_anyformat_to_radar(infile, callid);
printf("check here %s %s %f %f\n",radar->h.name,radar->h.radar_name,
	radar->h.latd + ( radar->h.latm / 60. ) + ( radar->h.lats / 3600.) ,
	radar->h.lond + ( radar->h.lonm / 60. ) + ( radar->h.lons / 3600.) );

for (i=0; i<MAX_RADAR_VOLUMES; i++) {
        sweep = RSL_get_first_sweep_of_volume(radar->v[i]);
        ray   = RSL_get_first_ray_of_volume(radar->v[i]);

        if (sweep) {

                xdim = 900;
                ydim = 900;
                range = 400.0;

		fval = 32.;
		RSL_add_dbz_offset_to_sweep(sweep, fval);
                radData = RSL_sweep_to_cart(sweep, xdim, xdim, range);

		minval = INT_MAX;
		maxval = INT_MIN;
		for(i=0;i<(xdim*ydim);i++)
		   if(radData[i] != 0 )
		      {
		      if ( radData[i] > 127 )
		         intval = (int)radData[i] - 256;
		      else
		         intval = (int)radData[i];

		      printf("check here %d %u %d\n",radData[i],radData[i],intval);

		      if ( intval > maxval ) maxval = intval;
		      if ( intval < minval ) minval = intval;
		      }

		printf("[ min %d  max %d ] ",minval, maxval);
		free(radData);
                }
        }

	if ( ray ) {

		for (i=0; i<ray->h.nbins; i++) {
   			x = ray->h.f(ray->range[i]);
   			/* Do something with this floating point value 'x'. */
   			printf("BIN %d value is %f %d\n", i, x, ray->range[i]);
		}
	}

RSL_free_radar(radar);

}
