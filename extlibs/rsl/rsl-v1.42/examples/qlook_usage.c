#define TRUE 1
#define FALSE 0

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

void process_args(int argc, char **argv, char *in_file, int *verbose,
                  char *site_id, char *tape_id,
                  int *qc_reflectivity, int *total_reflectivity,
                  int *differential_reflectivity, 
                  int *velocity, int *spectral_width,
                  int *make_gif, int *make_pgm, int *make_bscan, int *make_uf,
                  int *num_sweeps, float *dbz_offset,
                  int *xdim, int *ydim, float *range,
                  float *gate_size_adjustment, int *print_azim,
                  char *gifdir, char *pgmdir, char *ufdir)
{
    extern char   *optarg;
    extern int    optind, optopt;
    char c;

    while ((c = getopt(argc, argv, "vgpus:t:n:x:y:r:o:a:ADCQTWVG:P:U:")) != -1) {

      switch(c) {
/*
  RSL Verbose flag
*/
      case 'v': *verbose = TRUE; break;

/* 
  s: First file or call sign 
*/
      case 's': strcpy(site_id, optarg); break;
      case 't': strcpy(tape_id, optarg); break;

/*
   x: x dimension
   y: y dimension
   r: max range
   z: zoom factor (km/pixel)
*/
      case 'x': *xdim  = atoi(optarg); break;
      case 'y': *ydim  = atoi(optarg); break;
      case 'r': *range = atof(optarg); break;
      case 'a': *gate_size_adjustment = atof(optarg); break;
      
/*  dBZ Offset
*/
      case 'o': *dbz_offset = atof(optarg); break;
/* 
   T: Total reflectivity
   Q: Do qc'd reflectivity
   V: Do radial velocity
   W: Do spectral width
*/
      case 'Q': *qc_reflectivity = TRUE; break;
      case 'T': *total_reflectivity = TRUE; break;
      case 'V': *velocity        = TRUE; break;
      case 'W': *spectral_width  = TRUE; break;
      case 'A': *print_azim = TRUE; break;
      case 'D': *differential_reflectivity  = TRUE; break;

/*
   g: Make gif images
   p: Make pgm images
   u: Make uf files
*/
      case 'g': *make_gif = TRUE; break;
      case 'p': *make_pgm = TRUE; break;
      case 'u': *make_uf  = TRUE; break;

/*
   G: gif directory
   P: pgm directory
   U: uf directory
*/
      case 'G': strcpy(gifdir, optarg); break;
      case 'P': strcpy(pgmdir, optarg); break;
      case 'U': strcpy(ufdir,  optarg); break;

/* 
   num_sweeps: Number of sweeps to make images of 
*/
      case 'n': *num_sweeps = atoi(optarg); break;

/*
  Deal with bad input
*/
      case '?': fprintf(stderr, "ERROR: option -%c is undefined\n", optopt);
        goto Usage;
      case ':': fprintf(stderr, "ERROR: option -%c requires an argument\n",optopt);
        goto Usage;
      default: break;
      }
    }

/*
   Must have at the least a file listed on the command lines, everything
   can be defaulted.
 */

    if (argc - optind != 1) {
Usage:
        fprintf(stderr,"ERROR:::\n");
        fprintf(stderr,"%s [options] input_file:",argv[0]);
        fprintf(stderr,"\n[options]: ");
        fprintf(stderr,"\n\t[-v verbose_flag?] ");
        fprintf(stderr,"\n\t[-s First file or call sign?] ");
        fprintf(stderr,"\n\t[-t Tape ID] ");
        fprintf(stderr,"\n\t[-u Make UF file]");
        fprintf(stderr,"\n\t[-g Make GIF images?]");
        fprintf(stderr,"\n\t[-p Make PGM images?]");
        fprintf(stderr,"\n\t[-U Directory for UF output files]");
        fprintf(stderr,"\n\t[-G Directory for GIF output files]");
        fprintf(stderr,"\n\t[-P Directory for PGM output files]");
        fprintf(stderr,"\n\t[-x X dimension]");
        fprintf(stderr,"\n\t[-y Y dimension]");
        fprintf(stderr,"\n\t[-r max range]");
        fprintf(stderr,"\n\t[-n Number of sweeps to make images]");
        fprintf(stderr,"\n\t[-Q Do qc reflectivity]");
        fprintf(stderr,"\n\t[-T Do total reflectivity]");
        fprintf(stderr,"\n\t[-V Do velocity]");
        fprintf(stderr,"\n\t[-W Do spectral_width]");
        fprintf(stderr,"\n\t[-D Do differential reflectivity");
        fprintf(stderr,"\n\t[-o Apply dBZ offset");
        fprintf(stderr,":::\n");
        exit(-1);
    }
    
    strcpy(in_file, argv[optind]);

}




















