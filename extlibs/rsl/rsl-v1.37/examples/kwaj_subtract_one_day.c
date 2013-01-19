#include "rsl.h"

/**********************************************************************/
/*                                                                    */
/*                           usage                                    */
/*                                                                    */
/**********************************************************************/
void usage(char **argv)
{
  fprintf(stderr, "Usage: %s in out.uf\n", argv[0]);
  fprintf(stderr, "\n");
  fprintf(stderr, "Subtract one day from all time fields in all headers,\n");
  fprintf(stderr, "all the way down to the ray.  Output to uf.\n");
  return;
}

/**********************************************************************/
/*                                                                    */
/*                        subtract_one_day_ray                        */
/*                                                                    */
/**********************************************************************/
#include<time.h>
void    *subtract_one_day(int month, int day, int year,
						  int *m, int *d, int *y)
{
  /* Connocialize and subtract. */
  struct tm *t;
  time_t the_time;

  t = (struct tm *)calloc(1, sizeof(struct tm));
  t->tm_mon  = month-1;   /* 0 - 11 */
  t->tm_mday = day-1;       /* 1 - 31 */  /* And, subtract one day. */
  t->tm_year = year-1900; /* since 1900 */
  the_time = mktime(t);
  t = localtime(&the_time);
  *m = t->tm_mon+1;
  *d = t->tm_mday;
  *y = t->tm_year+1900;
  return;
}

/**********************************************************************/
/*                                                                    */
/*                        subtract_one_day_ray                        */
/*                                                                    */
/**********************************************************************/
Ray    *subtract_one_day_ray(Ray *x)
{
  if (x == NULL) return x;
  subtract_one_day(x->h.month, x->h.day, x->h.year,
				   &x->h.month, &x->h.day, &x->h.year);
  return x;
}
/**********************************************************************/
/*                                                                    */
/*                        subtract_one_day_sweep                      */
/*                                                                    */
/**********************************************************************/
Sweep  *subtract_one_day_sweep(Sweep *x)
{
  int i;

  if (x == NULL) return x;
  for(i=0; i<x->h.nrays; i++)
	x->ray[i] = subtract_one_day_ray(x->ray[i]);
  return x;
}
/**********************************************************************/
/*                                                                    */
/*                        subtract_one_day_volume                     */
/*                                                                    */
/**********************************************************************/
Volume *subtract_one_day_volume(Volume *x)
{
  int i;

  if (x == NULL) return x;
  for(i=0; i<x->h.nsweeps; i++)
	x->sweep[i] = subtract_one_day_sweep(x->sweep[i]);
  return x;
}
/**********************************************************************/
/*                                                                    */
/*                        subtract_one_day_radar                      */
/*                                                                    */
/**********************************************************************/
Radar  *subtract_one_day_radar(Radar *x)
{
  int i;

  if (x == NULL) return x;
  for(i=0; i<x->h.nvolumes; i++)
	x->v[i] = subtract_one_day_volume(x->v[i]);
  return x;
}
  


/**********************************************************************/
/*                                                                    */
/*                             m a i n                                */
/*                                                                    */
/**********************************************************************/
int main(int argc, char **argv)
{

  Radar *radar;

  if (argc != 3) {
	usage(argv);
	exit(-1);
  }

  radar = RSL_anyformat_to_radar(argv[1]);
  radar = subtract_one_day_radar(radar);
  subtract_one_day(radar->h.month, radar->h.day, radar->h.year,
				   &radar->h.month, &radar->h.day, &radar->h.year);
  RSL_radar_to_uf_gzip(radar, argv[2]);

  exit(0);
}
