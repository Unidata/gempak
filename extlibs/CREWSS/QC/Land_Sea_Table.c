#include "gui.h"


int Land_Ship( float lon, float lat, int reptype, int *Land_Sea_Table, int *Dims);
void Get_Land_Sea_Table (char *Land_Sea_Filename, int **Land_Sea_Table, 
								int *Land_Sea_dims);


void Get_Land_Sea_Table (char *Land_Sea_Filename, int **Land_Sea_Table,
								int *Land_Sea_dims)
{

FILE *Grid_File;
int nrows, ncols, r, c;

/*
 * read in the land-sea table
 */

   Open_Grid_File( &Grid_File, Land_Sea_Filename, Land_Sea_dims );
   nrows=Land_Sea_dims[0];
   ncols=Land_Sea_dims[1];

   if ((*Land_Sea_Table = (int *) calloc(ncols*nrows, sizeof(int))) == NULL) {
      fprintf(stderr,"Memory problems.  Cannot create array of size %d int point values.\n", ncols*nrows);
      exit(1);
   }

   for (r=0; r<nrows; r++) {
      for (c=0; c<ncols; c++)
         fscanf( Grid_File, "%i", (*Land_Sea_Table + (r*ncols) + c));
   }

}

int Land_Ship(
float lon,
float lat,
int reptype,
int *Land_Sea_Table,
int *Dims)

/************************************************************************
 *                                                                      *
 * Land_Ship                                                            *
 *   Get the corresponding land/sea value for the given lat/lon.	*
 *   If the value is not 1 (sea) than return 1, else return 0;          *
 *                                                                      *
 ***********************************************************************/
{
  int    Grid_x, Grid_y, offset;

/*---------------------------------------------------------------------*/


/* if the report type is not a ship, then just return 0 */

   if (reptype != 522 && reptype != 523) 
      return 0;
/*
 * if ConvertLon, then longitude is expressed -180 to 180.
 * so convert to 0 to 360
 */

    Grid_y = (int) (90 - lat +.5); /* round */
    Grid_x = (int) (lon +.5);


    offset = (Dims[1]*Grid_y) + Grid_x;

    if (Land_Sea_Table[offset] == 1) 
      return 1;
    else
      return 0;

}


