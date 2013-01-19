/*
 * Test reading and writing of UF files by using the histogram function.
 *
 * 1. Read WSR88D file.
 * 2. Print histogram of DZ volume.
 * 3. Output Radar to UF.
 * 3. Free Radar structure.
 * 4. Read UF into Radar.
 * 5. Print histogram of DZ volume.
 *
 * The two outputted histograms should be identical.
 *
 */



#include <stdio.h>
#ifdef sgi
#include <getopt.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "rsl.h"


usage()
{
  fprintf(stderr,"Usage: wsr_hist_uf_test infile [-s site_id]\n");
  exit(-1);
}

process_args(int argc, char **argv, char **in_file, char **site)
{
  int c;
  
  while ((c = getopt(argc, argv, "s:")) != -1)
	switch (c) {
	case 's': *site = strdup(optarg); break;
	case '?': usage(argv); break;
	default:  break;
	}
  if (argc - optind == 1) *in_file = strdup(argv[optind]);
  else usage();
}


main(int argc, char **argv)
{
  char *infile;
  char *site = NULL;

  Radar *radar;
  Histogram *histogram = NULL;

  process_args(argc, argv, &infile, &site);
  RSL_radar_verbose_on();

  if ((radar = RSL_anyformat_to_radar(infile, site)) == NULL) {
	/* RSL_wsr88d_to_radar writes an error message to stdout. */
	exit(-1);
  }

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
  printf("Radar date: %2.2d/%2.2d/%2.2d\n", radar->h.month, radar->h.day, radar->h.year);
  printf("Radar time: %2.2d:%2.2d:%f\n", radar->h.hour, radar->h.minute, radar->h.sec);


  RSL_radar_to_uf(radar, "uf_file.uf");
  histogram = RSL_get_histogram_from_volume(radar->v[DZ_INDEX],
										 histogram, -30, 70, 0, 200);
  RSL_print_histogram(histogram, 0, 200, "hist_wsr88d_to_radar.dat");
  RSL_free_radar(radar);

  RSL_radar_verbose_on();
  printf("RSL_uf_to_radar\n");
  radar = RSL_uf_to_radar("uf_file.uf");
  histogram = NULL;  /* There should be a free here. */
  histogram = RSL_get_histogram_from_volume(radar->v[DZ_INDEX],
										 histogram, -30, 70, 0, 200);
  RSL_print_histogram(histogram, 0, 200, "hist_uf_to_radar.dat");

  exit(0);

}
