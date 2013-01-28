#include "geminc.h"
#include "gemprm.h"

#include "rsl.h"

#ifdef UNDERSCORE
#define rnex2   rnex2_
#define set_gproj set_gproj_
#define get_cxpts get_cxpts_
#endif

void	rnex2 ( char *infile, char *radarfield, 
		float *fvals, int *kx, int *kz, float *dhorz,
		float *dvert, float *clat, float *clon, 
		int *interp, char *time, int *iret )
/************************************************************************
 * im_rcdf ( imgfil, kx, ky, isorc, itype, idate, itime, range,         *
 *        clat, clon, xmin, xmax, ymin, ymax, iret )                    *
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
 *      *clat           float           central latitude                *
 *      *clon           float           central longitude               *
 *      *xmin           float           x - minimum for image           *
 *      *xmax           float           x - maximum for image           *
 *      *ymin           float           y - minimum for image           *
 *      *ymax           float           y - maximum for image           *
 *      *iret           int             return value                    *
 ***********************************************************************/
{
Radar *radar=NULL;
Volume *v=NULL;

char callid[5], cparm[8], tmpl[MXFLSZ];
static char lev2tmp[]="NEXRII";
char *cpos, *basnam, *dirnam;

char    defdir[12], rfld[132], newfil[512];
long	lofset = 20, flen;
FILE 	*fp;
int	i, j, nbin, ipos, ier;
int VINDEX=0;

float dx, dy, dz;
int nx, ny, nz, x, y, z;
float grnd_r=920;  /* not used by rsl at this time */
int radar_x, radar_y, radar_z=0;
int nsweeps;
float min_tilt, max_tilt,delang=1.0;

float rgx[2], rgy[2], rlt[2], rln[2];

float 		(*f)(Range x);
Range 		(*invf)(float x);

/***********************************************************************/

*iret = 0;
dx = dy = *dhorz;
dz = *dvert;
nx = ny = *kx;
nz = *kz;
radar_x = radar_y = nx / 2;

cst_lcuc(radarfield, cparm, &ier);
if ( cparm[0] == 'D' )
	VINDEX = DZ_INDEX;
else if ( cparm[0] == 'S' )
	VINDEX = SW_INDEX;
else if ( cparm[0] == 'V' )
	VINDEX = VR_INDEX;


    /*
     * see if we can get the station ID from bytes 21-24 
     */

    defdir[0] = CHNULL;

    fp = cfl_ropn ( infile, defdir, &ier );
    if  ( ier != 0 )  {
        printf("oops look file %s %d\n",infile,ier);
        *iret = -1;
        return;
        }
    else {
        cfl_seek ( fp, lofset, SEEK_SET, &ier );
        if  ( ier != 0 )  {
             printf("oops look lofset %d\n",lofset);
            *iret = -1;
            cfl_clos ( fp, &ier );
            return;
            }
        }

    cfl_read ( fp, 4, (unsigned char *)callid, &nbin, &ier );
    cfl_clos ( fp, &ier );

    if ( ( nbin < 4 ) || ( callid[0] == 0) || ( callid[0] == ' ' ) )
        { /* see if Radar ID can be extracted from file name (as a failsafe) */
	basnam = (char *)malloc(strlen(infile)+1);
	dirnam = (char *)malloc(strlen(infile)+1);
	cfl_path ( infile, dirnam, basnam, &ier);
        ctb_dttmpl ( lev2tmp, tmpl, &ier );
	if ( ier == 0 )
           {
	   cst_rpst ( tmpl, "%SITE%", "****", tmpl, &ier);
           if ( ( ( cpos = strstr(tmpl,"****") ) != NULL ) &&
		( strlen(tmpl) == strlen(basnam) ) )
              {
	      ipos = cpos - tmpl;
	      strncpy ( callid, basnam + ipos, 4);
	      callid[4] = '\0';
              }
	   else
	      {
	      printf("file template %s does not provide %%SITE%%\n",tmpl);
	      sprintf ( callid, "KFTG\0");
	      }
           }
	else
           {
	   printf("could not find template %s, required for station id\n",
		lev2tmp);
	   sprintf ( callid, "KFTG\0");
           }
	free(basnam);
	free(dirnam);
        }
    else
        callid[4] = '\0';


    radarfield[3] = '\0';
    RSL_select_fields(radarfield, NULL);

    /*RSL_radar_verbose_on();*/

    cfl_inqr ( infile, defdir, &flen, newfil, &ier );
    radar = RSL_wsr88d_to_radar(newfil, callid);

    /*RSL_radar_verbose_off();*/

    if ( radar == NULL ) 
        {
        *iret = -1;
        return;
        }

    v = radar->v[VINDEX];
    sprintf(time,"%02d%02d%02d/%02d%02d",
	radar->h.year%100,
	radar->h.month,
	radar->h.day,
	radar->h.hour,
	radar->h.minute);

    if((VINDEX == VR_INDEX)||(VINDEX == SW_INDEX))
	rebin_volume(v);


*clat = radar->h.latd + ( radar->h.latm / 60. ) + ( radar->h.lats / 3600.);
*clon = radar->h.lond + ( radar->h.lonm / 60. ) + ( radar->h.lons / 3600.);
set_gproj ( clat, clon, kx, dhorz, &ier);
if ( ier != 0 )
   {
   *iret = -4;
   return;
   }

get_cxpts ( rgx, rgy, rlt, rln, &ier ); 
if ( ier != 0 )
   {
   *iret = -4;
   return;
   }

f = v->h.f;
invf = v->h.invf;

nsweeps = v->h.nsweeps;
max_tilt = v->sweep[nsweeps-1]->h.elev + delang;
min_tilt = v->sweep[0]->h.elev - delang;

for(j=0; j < nsweeps; j++)
   {
   printf("%d    elev %f\n",j,v->sweep[j]->h.elev);
   }

for (j=0; j<nz; j++)
    for (i=0; i<nx; i++)
	{
	float rvalue, rng, slantr, azm, gx, gy, elev, h, limit=5.0;
        x = rgx[0] + ( rgx[1] - rgx[0] ) * ((float)i / (float)nx ); 
        y = rgy[0] + ( rgy[1] - rgy[0] ) * ((float)i / (float)ny ); 

	gx = x - radar_x;
	gy = y - radar_y;
	RSL_find_rng_azm(&rng, &azm, (float)gx, (float)gy);
	h = (float) j * dz;
	RSL_get_slantr_and_elev(rng, h, &slantr, &elev);

	if ((x >= 0)&&(y >= 0)&&(x <= nx - 1 )&&(y <= ny - 1))
           {
	   if(! *interp)
	      rvalue = RSL_get_value(v, elev, azm, slantr);
	   else if ( (elev >= min_tilt)&&(elev <= max_tilt))
	      {
	      rvalue = RSL_get_linear_value(v, slantr, azm, elev, limit);
	      }
	   else
	      rvalue = BADVAL;
	   if((rvalue >= BADVAL)&&(*interp)&&
	      (elev >= min_tilt)&&(elev <= max_tilt)) {
	      rvalue = RSL_get_value(v, elev, azm, slantr);
	      if ( rvalue > BADVAL ) rvalue = BADVAL;
	      }
	   }
	else
	   {
	   printf("outside %d %d   %d %d\n",i,j,x,y);
	   rvalue = BADVAL;
           }
        if((rvalue != BADVAL)&&(rvalue != (BADVAL-1)))  /* BADVAL-1 is Range Folded */
	   {
	   fvals[j * nx + i] = rvalue;
           if ( fvals[j * nx + i] > BADVAL ) {
	      printf("look value %f   %f   %f\n",rvalue,fvals[j * nx + i],elev);
	      fvals[j * nx + i] = RMISSD;
              }
           }
        else
	   fvals[j * nx + i] = RMISSD;
	}

RSL_free_radar(radar);

return;
}
