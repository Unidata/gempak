#include "geminc.h"
#include "gemprm.h"

#include "imgdef.h"

#include "rsl.h"

void nex2_rebin_sweep(Sweep *s );
void nex2_rebin_ray(Ray *r );

void crnex2 ( char filnam[], int *iret )
/************************************************************************
 * crnex2								*
 *									*
 * This subroutine reads the image data from an AWIPS GINI file.	*
 * The full image is placed in memory pointed to by imgData.		*
 *									*
 * crgini ( filnam, iret )						*
 *									*
 * Input parameters:							*
 *	filnam[]	char		Name of image file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 *					G_NIMGFL = cannot open/read img	*
 **									*
 * Log:									*
 ***********************************************************************/
{

	Radar 	*radar;
	Sweep	*sweep=NULL;
	Volume	*volume;

	char	defdir[12], callid[5];

	int	i, ier, nbin, xdim, ydim;
	float	range, fval;
	FILE 	*fp;
	long	lofset = 20;
	int VINDEX=0;

/*---------------------------------------------------------------------*/

	/*
	 * see if we can get the station ID from bytes 21-24
	 */

	defdir[0] = CHNULL;
	fp = cfl_ropn ( filnam, defdir, &ier );
	if  ( ier != 0 )  {
	    *iret = G_NIMGFL;
            return;
	    }
	else {
	    cfl_seek ( fp, lofset, SEEK_SET, &ier );
	    if  ( ier != 0 )  {
		*iret = G_NIMGFL;
		cfl_clos ( fp, &ier );
		return;
		}
	    }

	cfl_read ( fp, 4, (unsigned char *)callid, &nbin, &ier );
	cfl_clos ( fp, &ier );

	if ( ( nbin < 4 ) || ( callid[0] == 0) || ( callid[0] == ' ' ) )
    	    {
	    /* doesn't matter, since we already did the projection in IM_ */
    	    /*printf("can't read station ID from file\n");*/
    	    sprintf(callid,"KFTG\0");
    	    }
	else
    	    callid[4] = '\0';

	
	*iret = G_NORMAL;

        /*RSL_radar_verbose_on();*/
	RSL_select_fields(cmstyp, NULL);

	RSL_read_these_sweeps("all", NULL);

	radar = RSL_wsr88d_to_radar(filnam, callid);
	/*RSL_radar_verbose_off();*/
	if ( radar == NULL )  {
	    *iret = G_NIMGFL;
	    return;
	}

	if ( ( cmstyp[0] == 'D' ) || ( cmstyp[0] == 'd' ) )
	   VINDEX = DZ_INDEX;
	else if ( ( cmstyp[0] == 'V' ) || ( cmstyp[0] == 'v' ) )
	   VINDEX = VR_INDEX;
	else if ( ( cmstyp[0] == 'S' ) || ( cmstyp[0] == 's' ) )
	   VINDEX = SW_INDEX;

        volume = radar->v[VINDEX];
	/*volume = RSL_sort_sweeps_in_volume(radar->v[VINDEX]);*/
	if(volume != NULL) 
	   {
	   i = 0;
	   while ( ( sweep == NULL ) && ( i < volume->h.nsweeps ) )
	      {
	      if (volume->sweep[i]->h.sweep_num == imnchl )
	         sweep = volume->sweep[i];
              else
	         i++;
	      }
	   }


	if ( sweep ) {
	    xdim = imnpix - 1;
	    ydim = imnlin - 1;
	    range = ((float)xdim * rmxres ) / 2.0;

	    switch(VINDEX) {
		case DZ_INDEX:
			fval = 32.;
	                RSL_add_dbz_offset_to_sweep(sweep, fval);
			break;
		case VR_INDEX:
	    		nex2_rebin_sweep(sweep);
	    		/*RSL_rebin_velocity_sweep(sweep);*/
			break;
		case SW_INDEX:
	    		nex2_rebin_sweep(sweep);
			break;
		default:
			fval = 0.;
		}
	    imgData = RSL_sweep_to_cart(sweep, imnpix, imnpix, range);
	    /*for(i=0;i<xdim*ydim;i++)
		if(imgData[i] != 0)
			printf("[%d %d] ",i,imgData[i]);
	    printf("\n");*/
	}
	else {
	    *iret = G_NIMGFL;
	}

	RSL_free_radar(radar);


}





void nex2_rebin_sweep (Sweep *s)
{
  int i;
  if (s == NULL) return;
  for (i=0; i<s->h.nrays; i++)
        nex2_rebin_ray(s->ray[i]);
}

void nex2_rebin_ray(Ray *r)
{
  int i;
  float nyquist, val, width;
  int nscale; 
  float (*f)(Range x);
  Range (*invf)(float x);

  if (r == NULL) return;

  if ( r->h.vel_res == .5 )
     width = 63.5;
  else
     width = 127.0;

  nyquist = r->h.nyq_vel;
  if (nyquist == 0.0) {
        fprintf(stderr, "nex2_rebin_ray: nyquist == 0.0\n");
        fprintf(stderr, "nex2_RSL_rebin_ray: Unable to rebin.\n");
        return;
  }
  f = r->h.f;
  invf = r->h.invf;
  for (i=0; i<r->h.nbins; i++) {
        val = f(r->range[i]);
        if ( val == RFVAL ) {
           val = (float)immxpx;
           }
        else if ( val == APFLAG ) {
           val = (float)immxpx;
           }
        else if ( val == NOECHO ) {
           val = (float)immnpx;
           }
        else if (val != BADVAL) {
           if (fabs(val) > nyquist) 
                val = (float)immxpx;
           else 
              {
/*
           Okay, we want to shift the data to positive values
           then we re-scale them by the number of color bins/nyquist
*/
	      nscale = (immxpx - immnpx)  / 2;
              val = (int)( (val * nscale )/width + 1.0 + nscale);
              }
           }
        else 
           val = immnpx;

        if ( val == immnpx+1 )
           printf("look min pix %f %f\n",val,f(r->range[i]));
        r->range[i] = invf(val);
  }
}
