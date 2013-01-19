/*
 * This app tests what happens when a sweep has just one azimuth bin for
 * all the rays.  I will first read real data then permute the sweeps.
 * Then, I'll check how the hash table were built.  Based on print_hash_table.
 */

#include "rsl.h"
#include <stdlib.h>
void print_link_list(Azimuth_hash *list)
{
  if (list == NULL) {
	printf("\n");
	return;
  }
  printf("\n            ray# %d azim %f, hi# %d lo# %d", list->ray->h.ray_num, list->ray->h.azimuth, list->ray_high->ray->h.ray_num, list->ray_low->ray->h.ray_num);
  print_link_list(list->next);
}
  

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

void poke_about_sweep(Sweep *s)
{
  /* This routine demonstrates that the azimuth we want is the azimuth
   * we get.
   */
  float azim, res;
  Ray *ray;

  ray = RSL_get_first_ray_of_sweep(s);
  res = 360.0/s->h.nrays;
  for (azim=0; azim<360; azim+=res) {
	ray = RSL_get_ray_from_sweep(s, azim);
	if (ray)
	  printf("Azimuth %f matched in ray # %d, h.azimuth= %f, diff=%f\n",
			 azim, ray->h.ray_num, ray->h.azimuth,
			 ray->h.azimuth-azim);
	else
	  printf("Azimuth %f NOT FOUND within 1/2 beamwidth; 1/2beam=%f\n",
			 azim, s->h.horz_half_bw);
  }
}

float random_azim(void)
{
  double drand;
  drand = drand48()*1;
  return (float)drand;
}

Sweep *permute_sweep(Sweep *sweep)
{
  int i;
  Ray *ray;
  if (sweep == NULL) return NULL;
  for (i=0; i<sweep->h.nrays; i++) {
	ray = sweep->ray[i];
	if (ray == NULL) continue;
	ray->h.azimuth = random_azim();
  }
  return sweep;
}
Volume *permute_volume(Volume *volume)
{
  int i;
  if (volume == NULL) return NULL;
  for (i=0; i<volume->h.nsweeps; i++)
	volume->sweep[i] = permute_sweep(volume->sweep[i]);
  return volume;
}
Radar *permute_radar(Radar *radar)
{
  int i;
  if (radar == NULL) return NULL;
  for (i=0; i<radar->h.nvolumes; i++)
	radar->v[i] = permute_volume(radar->v[i]);
  printf("Radar permuted.  Now, redo the ray_indexes.\n");
  return radar;
}

void chase_hi_links(Sweep *sweep)
{
  int i;
  Azimuth_hash *table;
  Hash_table *hash_table;
  float last_azimuth;
  float azimuth;

  if (sweep == NULL) return;
  hash_table = hash_table_for_sweep(sweep);
  table = hash_table->indexes[0];

  printf("Printing HI links.  This has better be a sorted output.\n");
  printf("ELEVATION angle = %f\n", sweep->h.elev);
  for (i=0, last_azimuth=-1;i<sweep->h.nrays; i++) {
	if (table == NULL) continue;
	printf("   ray# %3d azim %8.6f hi# %3d lo# %3d\n",
		   table->ray->h.ray_num,
		   table->ray->h.azimuth,
		   table->ray_high->ray->h.ray_num,
		   table->ray_low->ray->h.ray_num);
	azimuth = table->ray->h.azimuth;
	if (azimuth < last_azimuth) printf("AZIMUTH OUT OF ORDER\n");
	last_azimuth = azimuth;
	table = table->ray_high;
  }
}

void main(int argc, char **argv)
{
  Radar *radar;
  Sweep *sweep;
  int i;

  if (argc < 2 || argc > 3) {fprintf(stderr, "Usage: %s infile [callid_or_firstfile]\n", argv[0]); exit(-1);}
  RSL_radar_verbose_on();
  radar = RSL_anyformat_to_radar(argv[1], argv[2]);
  if (radar == NULL) {
	printf("RADAR IS NULL.\n");
	exit(-1);
  }

  printf("permute_radar\n");
  radar = permute_radar(radar);

  for (i=0; i<radar->v[DZ_INDEX]->h.nsweeps; i++) {
	sweep = radar->v[DZ_INDEX]->sweep[i];
	chase_hi_links(sweep);
  }
/*
  print_hash_table(sweep);

  poke_about_sweep(sweep);
*/
  exit(0);

}



