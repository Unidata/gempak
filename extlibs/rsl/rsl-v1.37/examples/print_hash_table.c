/*
 * Ingest NEXRAD (wsr88d) data and print the azimuth hash table created.
 *
 * This example is the most minimum of coding that you need to do
 * to achieve good results from using the RSL code.
 *
 * This is short and sweet to demonstrate the simplicity of use for
 * the RSL.
 *
 */

#include "rsl.h"

void print_link_list(Azimuth_hash *list)
{
  if (list == NULL) {
	printf("\n");
	return;
  }
  printf("ray# %d azim %f, hi# %d lo# %d|", list->ray->h.ray_num, list->ray->h.azimuth, list->ray_high->ray->h.ray_num, list->ray_low->ray->h.ray_num);
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


void main(int argc, char **argv)
{
  Radar *radar;
  Sweep *sweep;

  if (argc != 3) {fprintf(stderr, "Usage: %s infile callid_or_firstfile\n", argv[0]); exit(-1);}
  RSL_radar_verbose_on();
  radar = RSL_wsr88d_to_radar(argv[1], argv[2]);
  if (radar == NULL) exit(-1);
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

  sweep = RSL_get_first_sweep_of_volume(radar->v[DZ_INDEX]);
  print_hash_table(sweep);

  poke_about_sweep(sweep);

  exit(0);

}



