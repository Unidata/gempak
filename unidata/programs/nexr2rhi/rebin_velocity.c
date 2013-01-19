#include "geminc.h"
#include "gemprm.h"

#include "rsl.h"

void rebin_volume (Volume *v);
void rebin_sweep (Sweep *s);
void rebin_ray(Ray *r);


void rebin_volume (Volume *v)
{
  int i;
  if (v == NULL) return;
  for (i=0; i<v->h.nsweeps; i++)
        rebin_sweep(v->sweep[i]);
}


void rebin_sweep (Sweep *s)
{
  int i;
  if (s == NULL) return;
  for (i=0; i<s->h.nrays; i++)
        rebin_ray(s->ray[i]);
}

void rebin_ray(Ray *r)
{
  int i;
  float nyquist, val /*, width */;
  float (*f)(Range x);
  Range (*invf)(float x);

  if (r == NULL) return;

  /*if ( r->h.vel_res == .5 )
     width = 63.5;
  else
     width = 127.0;*/

  nyquist = r->h.nyq_vel;
  if (nyquist == 0.0) {
        fprintf(stderr, "rebin_ray: nyquist == 0.0\n");
        fprintf(stderr, "RSL_rebin_ray: Unable to rebin.\n");
        return;
  }
  f = r->h.f;
  invf = r->h.invf;
  for (i=0; i<r->h.nbins; i++) {
        val = f(r->range[i]);
	if (val == BADVAL-1) val = BADVAL;
	/*if(val != BADVAL)
           printf("look val %f\n",val);*/

        r->range[i] = invf(val);
  }
}
