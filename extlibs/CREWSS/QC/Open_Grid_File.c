#include "gui.h"

/* =====================================================================/
 *  Open_Grid_File 
 *
 *  Read in grid values into a one-dimensional array.
 *  The array is arranged row by row.
 *
 *  Open Grid_Filename, and read first record:
 *  #columns #rows 
 *
 *  Allocate an array of floating point values #cols x #rows.
 *  and point Grid to it.  Put dimensions in Grid_Dims:
 *  Grid_Dims[0]=#rows, Grid_Dims[1]=#columns.
 *
 * The file is read into the grid to end up with the
 * this grid:
 *
 *   0          180            360
 *   |--------------------------| 90
 *   |            |             |
 *   |            |             |
 *   |            |             |
 *   |            |             |
 *   |---------equator----------| 0
 *   |            |             |
 *   |            |             |
 *   |            |             |
 *   |            |             |
 *   |--------------------------| -90
 * 
 *
 * 
 * **********************************************************************/


int  Open_Grid_File( FILE **Grid_File, char *Grid_Filename, int *Grid_Dims)
{

char line[80];

/*
 * Open grid file and get dimensions
 */

   *Grid_File = fopen(Grid_Filename,"r");
   if ( *Grid_File == NULL) {
      PopupErrorMsg("No model files available for requested cycle.");
      return 0;
   }

   if (fgets (line, 80, *Grid_File) == NULL) {
      PopupErrorMsg("Error reading grid file.");
      return 0; 
   }

   sscanf(line, "%i %i",&Grid_Dims[1] ,&Grid_Dims[0]);
   return 1;

}
