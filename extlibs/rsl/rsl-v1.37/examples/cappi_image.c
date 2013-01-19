/*
 * For RSL version 0.28 and higher.
 *
 * v1.0 Began 2/16/94 by John Merritt.
 *
 * Demonstrates reading NEXRAD files and loading the Radar structure.
 */

#include <stdio.h>
#include <string.h>
#include "rsl.h"

usage(int argc, char **argv)
{
  fprintf(stderr,"Usage: %s infile [firstfile | callid]\n", argv[0]);
  exit(-1);
}

process_args(int argc, char **argv,
			 char **in_file, char **out_file)
{
 
  if (argc == 2) {
	*in_file = strdup(argv[1]);
	*out_file = NULL;
  } else if (argc == 3) {
	*in_file = strdup(argv[1]);
	*out_file = strdup(argv[2]);
  } else {
	usage(argc, argv);
  }
}


main(int argc, char **argv)
{
  char *infile, *firstfile;
  char outfile[100];

  Radar *radar;
  Cappi *cappi;

  int i, j;

/* 1. Process the arguments. */
  process_args(argc, argv, &infile, &firstfile); /* malloc for in/outfile */


  i = DZ_INDEX;

  RSL_radar_verbose_on();
  if ((radar = RSL_anyformat_to_radar(infile, firstfile)) == NULL) exit(-1);

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

  for (j=1; j<=5; j++) {
	cappi = RSL_cappi_at_h( radar -> v[i], (float)j/2.0, 200.0);
	sprintf(outfile, "cappi_%2.2d.gif", j);
	printf("Writing %s.\n", outfile);
	RSL_sweep_to_gif(cappi->sweep, outfile, 400, 400, 200.0);
	RSL_free_cappi(cappi);
  }


  exit(0);

}
