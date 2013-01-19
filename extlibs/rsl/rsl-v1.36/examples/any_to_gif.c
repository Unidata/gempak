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

/* Example use for getopt; argument processing */
/* ------
 * Remember that to pass back strings you must pass the
 *    address of the pointer to the string; argument is char **.
 *    (Note: arrays of characters; the name is char **)
 *    Ex.
 *    char *file
 *    process_args(... &file);
 *    DECLARE as: process_args(... char **file)
 *       *file = strdup(argv[optind]);
 *
 *    char infile[80];
 *    process_args(... infile ...);
 *    DECLARE as: process_args(... char *infile ...)
 *       strcpy(infile, argv[optind]);
 */
#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
int usage(char **argv)
{
  fprintf(stderr, "Usage: %s (v1.20) [-v] [-V] [-x n] [-y n] [-r x] infile [callid_or_firstfile]\n\n", argv[0]);
  fprintf(stderr, "Where: -v  = verbose print.  Default = no printing.\n");
  fprintf(stderr, "       -V  = Output entire volume.  Default = output first sweep only.\n");
  fprintf(stderr, "       -x n = Image dimension, image is n x n pixels. Default = 400x400 pixels.\n");
  fprintf(stderr, "       -r x = Range of radar data. Default = 400.0 km.\n");
  fprintf(stderr, "       -b x = Make black for dbz below x. Default = 0 (no action).\n");
  exit(-1);
}
process_args(int argc, char **argv,
			 char **in_file, char **callid, int *verbose, int *wholevolume,
			 int *xdim, float *range, int *dbz_black)
{
  int c;
  
  while ((c = getopt(argc, argv, "x:r:b:vV")) != -1)
	switch (c) {
	case 'v': *verbose = 1;  break;
	case 'V': *wholevolume = 1;  break;
	case 'x': *xdim = atoi(optarg);  break;
	case 'r': *range = atof(optarg);  break;
	case 'b': *dbz_black = atoi(optarg);  break;
	case '?': usage(argv); break;
	default:  break;
	}

/* must have 2 files listed */
  if (argc - optind != 1 && argc - optind != 2) usage(argv);

/* Can use strdup, if desired */
/* strdup allocates memory */
/* in_file = strdup(argv[optind]); */
  *in_file = strdup(argv[optind]);
  if (argc - optind == 2) *callid = strdup(argv[optind+1]);
  else *callid = NULL;

}


void main(int argc, char **argv)
{
  Radar *radar;
  Sweep *sweep;
  Ray   *ray;
  int   i, j;
  char fname[1000];
  char *infile, *callid;
  char time_string[100];
  int verbose;
  int wholevolume;
  int xdim;
  float range;
  int dbz_black;

  verbose = 0;
  wholevolume = 0;
  xdim  = 400;
  range = 400.0;
  dbz_black = 0;
  process_args(argc, argv, &infile, &callid, &verbose, &wholevolume,
			   &xdim, &range, &dbz_black);

  if (verbose)
	RSL_radar_verbose_on(); /* Not needed; it bides the time. */
  RSL_select_fields("all", NULL);
  RSL_read_these_sweeps("all", NULL);
  radar = RSL_anyformat_to_radar(infile, callid);

  if (radar == NULL) exit(-1);

  if (0) {
	RSL_write_radar(radar, "rsl.rsl");
	exit(0);
  }

  sprintf(time_string,"%2.2d%2.2d%2.2d_%2.2d%2.2d", 
		  radar->h.month, radar->h.day, radar->h.year-1900, 
		  radar->h.hour, radar->h.minute);

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

	  {
		char r[256], g[256], b[256];
		int nc;
		int i;
		RSL_get_color_table(RSL_RED_TABLE, r, &nc);
		RSL_get_color_table(RSL_GREEN_TABLE, g, &nc);
		RSL_get_color_table(RSL_BLUE_TABLE, b, &nc);
		for (i=0; i<dbz_black; i++) {
		  r[i]=(char)0;
		  g[i]=(char)0;
		  b[i]=(char)0;
		}
		RSL_set_color_table(RSL_RED_TABLE, r, nc);
		RSL_set_color_table(RSL_GREEN_TABLE, g, nc);
		RSL_set_color_table(RSL_BLUE_TABLE, b, nc);
	  }
	  memcpy(fname,radar->h.name, sizeof(radar->h.name));
	  for (j=0; j<sizeof(radar->h.name); j++) 
		if (fname[j] == '\0' || fname[j] == ' ') break;
	  if (j==sizeof(radar->h.name)) j--;

	  if (! wholevolume) {
		sprintf(&fname[j], "_%s_%s.gif", RSL_ftype[i],  time_string);
		/*	  printf("FNAME = <%s>\n", fname); */
		RSL_sweep_to_gif(sweep, fname, xdim, xdim, range);
		printf("%s\n", fname);
	  } else {
		sprintf(&fname[j], "_%s_%s", RSL_ftype[i],  time_string);
		/* RSL_volume_to_gif outputs to stdout the filenames produced. */
		RSL_volume_to_gif(radar->v[i], fname, xdim, xdim, range);
	  }
	}
  }
  exit(0);

}



