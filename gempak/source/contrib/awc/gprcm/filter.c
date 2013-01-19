#include "geminc.h"
#include "gemprm.h"

#include "rcm.h"

/************************************************************************
 * filter.c                                                             *
 *                                                                      *
 * Subroutines to filter plottable data using "N" coordinates.		*
 *                                                                      *
 ***********************************************************************/


typedef struct filter_struct {
	float x, y;
	float bnd[4];
	void *ds;
	struct filter_struct *next;
	} filter_struct;

filter_struct *head = NULL;

int search_filter(float x, float y, float rh, float rw, float ffactor)
/*
 * Internal routine. Determine if the data point is within plotted data boundaries.
 */
{
float dx, dy;
filter_struct *p;

dx = rw * ffactor / 2.;
dy = rh * ffactor / 2.;

p = head;
while(p != NULL)
   {
   if(((x+dx) > p->bnd[0])&&((x-dx) < p->bnd[1])&&((y-dy) < p->bnd[2]) && ((y+dy) > p->bnd[3]))
      return(0);
   p = p->next; 
   }
return(1);
}
/*=====================================================================*/

void insert_filter(float x, float y, float l1, float l2, float l3, float l4, void *ds)
/*
 * Internal routine. Insert plottable data into linked list.
 */
{
filter_struct *p;

p = (filter_struct*)malloc(sizeof(filter_struct));
p->x = x;
p->y = y;
p->bnd[0] = l1;
p->bnd[1] = l2;
p->bnd[2] = l3;
p->bnd[3] = l4;
p->ds = ds;

p->next = head;
head = p;
}
/*=====================================================================*/

void	n_filter(int npts, float *nx, float *ny, float rh, float rw, float ffactor, void *ds)
/*
 * "N" coordinate filter routine. If no plotable data already occupies this space,
 * insert the location and data pointer into linked list.
 */
{
float left, right, top, bottom;
float x, y;
int i;

for(i=0;i<npts;i++)
   {
   x = nx[i]; y = ny[i];
   if(search_filter(x,y,rh,rw,ffactor))
      {
      left = x - (ffactor * rw)/2.;
      right = x + (ffactor * rw)/2.;
      top = y + (ffactor * rh )/2.;
      bottom = y - (ffactor * rh )/2.;

      insert_filter(x, y, left, right, top, bottom, ds);
      }

   }
}
/*=====================================================================*/

void	filter_init()
/*
 * Initialize the linked list of plotable data.
 */
{
filter_struct *p;

while (head != NULL)
   {
   p = head;
   head = head->next;
   free(p);
   }
}
/*=====================================================================*/

filter_struct *hp;
void filter_set_head()
/*
 * Initialize the retrieval pointer to top of linked list.
 */
{
hp = head;
}
/*=====================================================================*/

int	filter_retrieve(void **ds, float *x, float *y)
/*
 * Retrieve the next plottable data from the linked list.
 */
{
if(hp == NULL) return(0);

*ds = hp->ds;
*x = hp->x;
*y = hp->y;
hp = hp->next;
return(1);
}
/*=====================================================================*/
