/*
 * Ingest NEXRAD (wsr88d) data and output images representing
 * all field types found.
 *
 * This example is the most minimum of coding that you need to do
 * to achieve good results from using the RSL code.
 *
 * This is short and sweet to demonstrate the simplicity of use for
 * the RSL.
 *
 */

#define USE_RSL_VARS
#include "rsl.h"

void main(int argc, char **argv)
{
  Radar *radar;
  Sweep *sweep;
  Ray   *ray;
  int   i;
  char fname[100];

  if (argc < 2 || argc > 3) {
	fprintf(stderr, "Usage: %s infile [callid_or_firstfile]\n", argv[0]);
	exit(-1);
  }

  RSL_radar_verbose_on(); /* Not needed; it bides the time. */
  RSL_select_fields("all", NULL);
  radar = RSL_anyformat_to_radar(argv[1], argv[2]);

  if (radar == NULL) exit(-1);

  if (0) {
	RSL_write_radar(radar, "rsl.rsl");
	exit(0);
  }

  {
	char time_string[100];
	sprintf(time_string,"%2.2d%2.2d%2.2d_%2.2d%2.2d", 
		    radar->h.month, radar->h.day, radar->h.year-1900, 
		    radar->h.hour, radar->h.minute);
  }

  for (i=0; i<MAX_RADAR_VOLUMES; i++) {
	sweep = RSL_get_first_sweep_of_volume(radar->v[i]);
	ray   = RSL_get_first_ray_of_volume(radar->v[i]);

	if (sweep) {
	  if (i == SW_INDEX)
		RSL_load_sw_color_table();
	  else if (i == VR_INDEX || i == VE_INDEX) {
		RSL_load_vel_color_table();
		RSL_rebin_velocity_volume(radar->v[i]);
	  } else
		RSL_load_refl_color_table();


#undef DO_SWEEP
#define DO_SWEEP
#ifdef DO_SWEEP
	  sprintf(fname, "%s_sweep.ppm", RSL_ftype[i]);
	  RSL_sweep_to_ppm(sweep, fname, 400, 400, 200.0);
	  fprintf(stderr, "Wrote %s\n", fname);
#else	
	  sprintf(fname, "%s_sweep", RSL_ftype[i]);
	  RSL_volume_to_gif(radar->v[i], fname, 400, 400, 200.0);
#endif
	}
  }
  exit(0);

}



