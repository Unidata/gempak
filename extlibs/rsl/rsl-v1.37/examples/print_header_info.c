
#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>

#include "rsl.h"

int qprint_ray;

usage(char **argv)
{
  fprintf(stderr,"Usage: %s [-v] [-f nexrad_id] [infile]\n", argv[0]);
  exit(-1);
}

process_args(int argc, char **argv,
			 char **in_file, int *qprint_ray,
			 char **site)
{
  
  int c;
  
  while ((c = getopt(argc, argv, "vf:")) != -1)
	switch (c) {
	case 'v': *qprint_ray = 1;  break;
	case 'f': *site = strdup(optarg); break;
	case '?': usage(argv); break;
	default:  break;
	}
  
  if ((argc-optind) == 0) {
	*in_file = NULL;
  } else if ((argc-optind) == 1) {
	*in_file = strdup(argv[optind]);
  } else {
	usage(argv);
  }
}

void print_ray_values(Ray *ray)
{
  int i,j;

  if (ray == NULL) {
	printf("Ray = NULL\n");
	return;
  }

  for (i=0; i<ray->h.nbins; i+=10) {
	for (j=i; j<(i+10)&&j<ray->h.nbins; j++)
	  printf("%10.2f", ray->h.f(ray->range[j]));
	printf("\n");
  }
  return;
}

void print_header_for_ray(Ray *ray)
{
  if (ray==NULL) {printf("NULL."); return;}

printf("   month %d\n",   ray->h.month); /* Time for this ray; month (1-12). */
printf("     day %d\n",   ray->h.day);   /* Time for this ray; day (1-31).   */
printf("    year %d\n",   ray->h.year);  /* Time for this ray; year (eg. 1993). */
printf("    hour %d\n",   ray->h.hour);  /* Date for this ray; hour (0-23). */
printf("  minute %d\n",   ray->h.minute);/* Date for this ray; minute (0-59).*/
printf("     sec %f\n", ray->h.sec);   /* Date for this ray; second + fraction of second. */
printf("    unam %f\n", ray->h.unam_rng);  /* Unambiguous range. (KM). */
printf(" azimuth %f\n", ray->h.azimuth);   /* Azimuth angle. (degrees). 0=North, 90=east, -90=west. */
printf(" ray_num %d\n",   ray->h.ray_num);   /* Ray no. within elevation scan. */
printf("    elev %f\n", ray->h.elev);       /* Elevation angle. (degrees). */
printf("elev_num %d\n",   ray->h.elev_num);   /* Elevation no. within volume scan. */
  
printf(" range_bin1 %d\n",   ray->h.range_bin1); /* Range to first gate.(meters) */
printf("  gate_size %d\n",   ray->h.gate_size);  /* Data gate size (meters)*/
  
printf("    vel_res %f\n",  ray->h.vel_res);    /* Doppler velocity resolution */
printf(" sweep_rate %f\n", ray->h.sweep_rate);   /* Sweep rate. Full sweeps/min. */
  
printf("        prf %f\n", ray->h.prf);          /* Pulse repitition frequency, in Hz. */
printf("  azim_rate %f\n", ray->h.azim_rate);
printf("  fix_angle %f\n", ray->h.fix_angle);
printf("pulse_count %f\n", ray->h.pulse_count);
printf("pulse_width %f\n", ray->h.pulse_width); /* Pulse width (micro-sec). */
printf(" beam_width %f\n", ray->h.beam_width);  /* Beamwidth in degrees. */
printf("  frequency %f\n", ray->h.frequency);   /* Bandwidth MHz. */
printf(" wavelength %f\n", ray->h.wavelength);  /* Wavelength. Meters. */
printf("    nyq_vel %f\n", ray->h.nyq_vel);    /* Nyquist velocity. m/s */
printf("   latitude %f\n", ray->h.lat);    /* Latitude (decimal) */
printf("  longitude %f\n", ray->h.lon);    /* Longitude (decimal) */
printf("      nbins %d\n", ray->h.nbins);               /* Number of array elements for 'Range'. */

if (!qprint_ray) return;

print_ray_values(ray);

}

void print_header_for_sweep(Sweep *s)
{
  int i;

  if (s==NULL) return;
  for (i=0; i<s->h.nrays; i++) {
	printf("ray[%d] ",i );
	print_header_for_ray(s->ray[i]);
	printf("\n");
  }

}

void print_header_for_volume(Volume *v)
{
  int i;
  if (v==NULL) return;
  for (i=0; i<v->h.nsweeps; i++) {
	printf("-------- Sweep %d ---------\n", i);
	print_header_for_sweep(v->sweep[i]);
  }
}


main(int argc, char **argv)
{
  char *infile;

  Radar *radar;
  char *site = NULL;
  int i;

  qprint_ray = 0; /* Global flag for printing ray values. */
  process_args(argc, argv, &infile, &qprint_ray, &site); /* malloc for in/outfile */

  RSL_radar_verbose_on();
  RSL_read_these_sweeps("all", NULL);

  radar = RSL_anyformat_to_radar(infile, site);

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


printf("Radar sec       : %f\n", radar->h.sec); /* Second plus fractional part. */
printf("Radar radar_type: %s\n", radar->h.radar_type);
                       /* Type of radar.  Use for QC-ing the data.
	                    * Supported types are:
                        * "wsr88d", "lassen", "uf",
                        * "nsig", "mcgill",
	                    * "kwajalein", "rsl", "toga".
                        * Set by appropriate ingest routine.
                        */
printf("Radar nvolumes  : %d\n", radar->h.nvolumes);
printf("Radar number    : %d\n", radar->h.number);
printf("Radar name      : %s\n", radar->h.name);
printf("Radar radar_name: %s\n", radar->h.radar_name);
printf("Radar city      : %s\n",  radar->h.city);
printf("Radar state     : %s\n", radar->h.state);
printf("Radar latd: %d\n", radar->h.latd);
printf("Radar latm: %d\n", radar->h.latm);
printf("Radar lats: %d\n", radar->h.lats);
printf("Radar lond: %d\n", radar->h.lond);
printf("Radar lonm: %d\n", radar->h.lonm);
printf("Radar lons: %d\n", radar->h.lons);
printf("Radar height: %d\n", radar->h.height);
printf("Radar spulse: %d\n", radar->h.spulse);
printf("Radar lpulse: %d\n", radar->h.lpulse);
printf("Radar lpulse: %d\n", radar->h.lpulse);


for (i=0; i<radar->h.nvolumes; i++) {

  if (radar->v[i]) {
	printf("PRINT_HEADER_FOR_VOLUME, %d, ... nsweeps = %d\n", i, radar->v[i]->h.nsweeps);
	print_header_for_volume(radar->v[i]);
  }
}
exit(0);

}
