/*
 * For RSL version 0.28 and higher.
 *
 * v1.0 Began 2/16/94 by John Merritt.
 *
 * Demonstrates reading NEXRAD files and loading the Radar structure.
 */



#include <stdio.h>
#ifdef sgi
#include <getopt.h>
#endif
#include <stdlib.h>
#include <string.h>

#include "rsl.h"

usage()
{
  fprintf(stderr,"Usage: bscan infile [callid]\n");
  exit(-1);
}

process_args(int argc, char **argv,
			 char **in_file, char **callid)
{
 
  if (argc < 2) usage();
  else if (argc == 2) *in_file = strdup(argv[1]);
  else if (argc == 3) {
	*in_file = strdup(argv[1]);
	*callid  = strdup(argv[2]);
  } else {
	usage();
  }
}


main(int argc, char **argv)
{
  char *infile, *callid;

  Radar *radar;
  Volume *cappi_vol;
  char *index_str[] = {"DZ", "VR", "SW"};

  int i;

/* 1. Process the arguments. */
  callid = NULL;
  process_args(argc, argv, &infile, &callid); /* malloc for in/outfile */


/*
 * Pass bitwise or of DZ_MASK, VR_MASK, SW_MASK
 */
  i = DZ_INDEX;

  RSL_radar_verbose_on();
  if ((radar = RSL_anyformat_to_radar(infile, callid)) == NULL)	exit(-1);

  printf("Radar date: %2.2d/%2.2d/%2.2d\n", radar->h.month, radar->h.day, radar->h.year);
  printf("Radar time: %2.2d:%2.2d:%f\n", radar->h.hour, radar->h.minute, radar->h.sec);

  printf("Radar file: %s\n", infile);
  printf("Radar site: %c%c%c%c\n",
		 radar->h.name[0],
		 radar->h.name[1],
		 radar->h.name[2],
		 radar->h.name[3]);
  printf("Radar date: %2.2d/%2.2d/%2.2d\n", radar->h.month, radar->h.day, radar->h.year);
  printf("Radar time: %2.2d:%2.2d:%f\n", radar->h.hour, radar->h.minute, radar->h.sec);


  if (i == DZ_INDEX) RSL_load_refl_color_table();
  if (i == VR_INDEX) RSL_load_vel_color_table();
  if (i == SW_INDEX) RSL_load_sw_color_table();
  if (i == VR_INDEX) RSL_rebin_velocity_volume(radar->v[i]); /* Modifies v[i]. */

  printf("Generating bscan ppm images of %s\n", index_str[i]);
  RSL_bscan_volume((Volume *) radar->v[i], "bscan.ppm");
  printf("----> BSCAN complete.\n");

  exit(0);

}
