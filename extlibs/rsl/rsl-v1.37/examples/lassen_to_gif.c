/*
 * lassen_to_gif [file]
 *
 * This program can read the file from stdin.
 */

#include "rsl.h"

void main(int argc, char **argv)
{
  Radar *radar;

  RSL_radar_verbose_on();
  radar = RSL_lassen_to_radar(argv[1]);
  if (radar == NULL) exit(-1);

  RSL_load_refl_color_table();
  RSL_volume_to_gif(radar->v[DZ_INDEX], "dz_sweep", 400, 400, 200.0);

  RSL_load_vel_color_table();
  RSL_rebin_velocity_volume(radar->v[VR_INDEX]); /* Modifies v[i]. */
  RSL_volume_to_gif(radar->v[VR_INDEX], "vr_sweep", 400, 400, 200.0);

  RSL_load_sw_color_table();
  RSL_volume_to_gif(radar->v[SW_INDEX], "sw_sweep", 400, 400, 200.0);

  exit(0);

}



