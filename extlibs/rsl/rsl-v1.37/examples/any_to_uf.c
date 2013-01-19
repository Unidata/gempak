#include <stdio.h>
#include "rsl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void usage(char **argv)
{
  fprintf(stderr, "Usage: %s [-f firstfile] infile outfile\n", argv[0]);
  exit(-1);
}

process_args(int argc, char **argv,
			 char **in_file, char **out_file, char **first_file)
{
  int c;
  
  while ((c = getopt(argc, argv, "f:")) != -1)
	switch (c) {
	case 'f': *first_file = strdup(optarg); break;
	case '?': usage(argv); break;
	default:  break;
	}

/* must have 2 files listed */
  if (argc - optind != 2) usage(argv);

/* Can use strdup, if desired */
/* strdup allocates memory */
/* in_file = strdup(argv[optind]); */
  *in_file = strdup(argv[optind]);
  *out_file = strdup(argv[optind+1]);
}


main (int argc, char **argv)
{

  Radar *radar;
  char *infile, *outfile, *first_file;

  first_file = NULL;
  process_args(argc, argv, &infile, &outfile, &first_file);
  RSL_radar_verbose_on();
  radar = RSL_anyformat_to_radar(infile, first_file);
  RSL_radar_to_uf(radar, outfile);

}

  
