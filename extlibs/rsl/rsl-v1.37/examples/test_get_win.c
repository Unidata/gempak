#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>

#include "rsl.h"

Sweep *load_sweep(Sweep *s)
{
  int i, j;
  Ray *ray;
  
  if (!s) {
	printf("sweep is NULL-can't load\n");
	return NULL;
  }
  
  printf("loading sweep with %d rays\n", s->h.nrays);
  for (i = 0; i < s->h.nrays; i++) {
	ray = s->ray[i];
	/* set range */
	for (j = 0; j < ray->h.nbins; j++) {

	  ray->range[j] = ray->h.invf(40.0);
	}
  }

  return s;
}


main (int argc, char **argv)
{

  Radar *new_radar, *tmp_radar, *radar;
  Sweep *s, *new_sweep;
  float min_range, max_range, low_azim, hi_azim;
  int j, i;
  char type;
  Volume *new_volume, *v;

/*
  RSL_radar_verbose_on();
*/
  if (argc < 8) {
	fprintf(stderr, "%s type(r|v|s) min_range max_range low_azim hi_azim ref_uf_file out_file\n", argv[0]);
	exit(-1);
  }

  i = 1;
  type = argv[i++][0];
  min_range = (float) atoi(argv[i++]);
  max_range = (float) atoi(argv[i++]);
  low_azim = (float) atoi(argv[i++]);
  hi_azim = (float) atoi(argv[i++]);

  radar = RSL_uf_to_radar(argv[i++]);
  if (!radar) exit(-1);

  RSL_load_refl_color_table();

  switch(type) {
  case 'r': 
  case 'v':
	v = RSL_copy_volume(radar->v[DZ_INDEX]);
	if (!v) exit(-1);
	if ((v = RSL_clear_volume(v)) == NULL) exit(-1);

	printf("volume's nsweeps - %d\n", v->h.nsweeps);
	for (j = 0; j < v->h.nsweeps; j++) {
	  printf("loading sweep %d\n", j);
	  v->sweep[j] = load_sweep(v->sweep[j]);

	}
	/* test get*from radar */
	if (type == 'r') {
	  if ((tmp_radar = RSL_new_radar(radar->h.nvolumes)) == NULL) exit (-1);
	  tmp_radar->h = radar->h;
	  tmp_radar->v[DZ_INDEX] = v;
	  new_radar = RSL_get_window_from_radar(tmp_radar,min_range, max_range, 
											low_azim, hi_azim); 
	  if (new_radar == NULL) {
		printf("null new radar\n");
		exit(-1);
	  }
	  RSL_volume_to_gif(new_radar->v[DZ_INDEX], argv[i], 500, 500, max_range);
	  RSL_free_radar(tmp_radar);
	  RSL_free_radar(new_radar);
	}
	else {
	  new_volume = RSL_get_window_from_volume(v, min_range, max_range, low_azim,
											hi_azim);
	  if (new_volume != NULL)
		RSL_volume_to_gif(new_volume, argv[i], 500, 500, max_range);
/*
	RSL_bscan_volume(new_volume);
*/
	  RSL_free_volume(new_volume);
	}
	break;
  case 's':
	s = RSL_copy_sweep(radar->v[DZ_INDEX]->sweep[0]);
	s = RSL_clear_sweep(s);
	s = load_sweep(s);
	if (!s) {
	  printf("null sweep\n");
	  exit(-1);
	}
	new_sweep = RSL_get_window_from_sweep(s, min_range, max_range, 
											low_azim, hi_azim  );

	RSL_sweep_to_gif(new_sweep, argv[i], 500, 500, max_range);
/*
	RSL_bscan_sweep(new_sweep);
*/
	RSL_free_sweep(new_sweep);

	break;
  default:
	break;
  }
  RSL_free_radar(radar);
  printf("done\n");
  exit (0);

}

