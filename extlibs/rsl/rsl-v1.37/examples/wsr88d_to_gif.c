/*
 * Ingest NEXRAD (wsr88d) data and output 3 gif images representing
 * Reflectivity, Velocity and Spectrum width.
 *
 * This example is the most minimum of coding that you need to do
 * to achieve good results from using the RSL code.
 *
 * This is short and sweet to demonstrate the simplicity of use for
 * the RSL.
 *
 * CAN READ STDIN.
 *
 * wsr88d_to_gif < file
 * wsr88d_to_gif file [tape_header_file]
 */

#include "rsl.h"

void main(int argc, char **argv)
{
  Radar *radar;

/*
 * Pass bitwise or of DZ_MASK, VR_MASK, SW_MASK
 */
  RSL_radar_verbose_on(); /* Not needed; on a slow network it bides the time. */
  radar = RSL_wsr88d_to_radar(argv[1], argv[2]);
  if (radar == NULL) exit(-1);

/*  RSL_sort_radar(radar); */
  
/***********************************************************************/
/*                                                                     */
/*            You now have a pointer to Radar.                         */
/*            Now use *radar all you like.                             */
/*                                                                     */
/***********************************************************************/

/* Use radar->v[DZ_INDEX] for REFELECTIVITY
 *     radar->v[VR_INDEX] for VELOCITY
 *     radar->v[SW_INDEX] for SPECTRUM_WIDTH
 */

  RSL_load_refl_color_table();
  RSL_volume_to_gif(radar->v[DZ_INDEX], "dz_sweep", 400, 400, 200.0);

  RSL_load_vel_color_table();
  RSL_rebin_velocity_volume(radar->v[VR_INDEX]); /* Modifies v[i]. */
  RSL_volume_to_gif(radar->v[VR_INDEX], "vr_sweep", 400, 400, 200.0);

  RSL_load_sw_color_table();
  RSL_volume_to_gif(radar->v[SW_INDEX], "sw_sweep", 400, 400, 200.0);

  exit(0);

}



