#include <stdio.h>
#include "rsl.h"

/*
 * Cannot compile if the hash table is static in volume.c.  For
 * testing we make it globally known.
 */

typedef struct {
  Sweep *s_addr;
  Hash_table *hash;
} Sweep_list;

extern int RSL_max_sweeps; /* Initial allocation for sweep_list.
							* RSL_new_sweep will allocate the space first
							* time around.
							*/
extern int RSL_nsweep_addr; /* A count of sweeps in the table. */
extern Sweep_list *RSL_sweep_list;
extern int RSL_nextents;


void print_link_list(Azimuth_hash *list)
{
  if (list == NULL) {
	printf("\n");
	return;
  }
  printf("ray# %d azim %f |", list->ray->h.ray_num, list->ray->h.azimuth);
  print_link_list(list->next);
}
  

void print_hash_table (Sweep *s)
{
  int i;
  int sweep_index;
  Azimuth_hash *index;
  float azim;
  float res;

  if (s == NULL) return;
  sweep_index = SWEEP_INDEX(s);
  res = 360.0/RSL_sweep_list[sweep_index].hash->nindexes;
  printf("Azimuth resolution = %f for %d bins.\n", res, RSL_sweep_list[sweep_index].hash->nindexes);
  for (i=0; i<RSL_sweep_list[sweep_index].hash->nindexes; i++) {
	index = RSL_sweep_list[sweep_index].hash->indexes[i];
	azim = i/res;
	printf("RSL_sweep_list[%d].hash->indexes[%d] = ", sweep_index, i);
	
	if (index == NULL) 
	  printf("IS NULL\n");
	else 
	  print_link_list(index);
  }
}

Sweep * get_sector(Sweep *s, float lo_azimuth, float hi_azimuth)
{
  int   i, j;
  Sweep *new_sweep;
  Ray   *saved_ray;

  if (s == NULL) return NULL;

  if ((new_sweep = RSL_new_sweep(s->h.nrays)) == NULL)
	return NULL;
  new_sweep->h = s->h;

  for (i = 0,j = 0; i < s->h.nrays; i++) {
	if (s->ray[i] == NULL) continue;
	if (s->ray[i]->h.azimuth >= lo_azimuth && 
		s->ray[i]->h.azimuth < hi_azimuth) {


	  new_sweep->ray[j] =RSL_copy_ray(s->ray[i]);

	  j++;
	}
  }

  return new_sweep;

}


void main(int argc, char **argv)
{
  Radar *radar;
  Sweep *sector;

  if (argc != 3) {fprintf(stderr, "Usage: %s infile callid_or_firstfile\n", argv[0]); exit(-1);}
  RSL_radar_verbose_on();
  radar = RSL_wsr88d_to_radar(argv[1], argv[2]);
  if (radar == NULL) exit(-1);

  RSL_load_refl_color_table();

  sector = get_sector(radar->v[DZ_INDEX]->sweep[0], 0.0, 90.0);

/*  sector = RSL_copy_sweep(radar->v[DZ_INDEX]->sweep[0]);
*/
  
  print_hash_table(sector);

  RSL_sweep_to_gif(sector, "dz_sector.gif", 400, 400, 200.0);

  exit(0);

}
