#include "geminc.h"
#include "gemprm.h"

#define KM_DEG	111.111

extern float radar_clat, radar_clon;

void xytolatlon ( float x, float y, float *lat, float *lon)
{
float dlat, dlon, avg_lat;

/* x 4 unites in 1/4 KM */
dlat = ( y / 4.) / KM_DEG;
*lat = radar_clat + dlat;
avg_lat = (*lat + radar_clat ) / 2.0;
dlon = ( x / 4.) / (KM_DEG * cos(avg_lat*DTR) );
*lon = radar_clon + dlon;
}

void radar_ij ( float *i, float *j )
{
int np=1, ier;
float xin[1], yin[1], xout[1], yout[1];
static char sysg[]="G", sysm[]="M";

xin[0]=radar_clat;
yin[0]=radar_clon;
gtrans ( sysm, sysg, &np, xin, yin, xout, yout, &ier,
	strlen(sysm), strlen(sysg) );
if ( ier == 0 )
   {
   *i = xout[0];
   *j = yout[0];
   }
else
   {
   printf("look ier %d\n",ier);
   *i = RMISSD;
   *j = RMISSD;
   }
}
