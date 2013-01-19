#include "gui.h"


/* =====================================================================*/
/* Read in the SLP, SST, Temp, UWind and VWind grids                    */
/* =====================================================================*/

   int  Get_Grids( char *SLP_filename,
                   char *SST_filename,
                   char *Temp_filename,
                   char *UWind_filename,
                   char *VWind_filename)

{

   int ii;
   FILE *Grid_File;
 

/* Read in the SLP fields. convert from hundredths of mb to mb.
 */

   if (!Open_Grid_File( &Grid_File, SLP_filename, SLP_dims)) return 0;
   if (!Read_Grid_File( &Grid_File, &SLP_Grid, SLP_dims)) return 0;

   SLPmin = 9999.; SLPmax = -999;  
   for (ii=0; ii<SLP_dims[0]*SLP_dims[1]; ii++) {
      SLP_Grid[ii] = SLP_Grid[ii]/100.;
      if (SLP_Grid[ii]<SLPmin) SLPmin = SLP_Grid[ii];
      if (SLP_Grid[ii]>SLPmax) SLPmax = SLP_Grid[ii];
   }


/*
 * read in the SST fields, convert from degrees Kelvin to Celcius    
 */

   if (!Open_Grid_File( &Grid_File, SST_filename, SST_dims)) return 0;
   if (!Read_Grid_File( &Grid_File, &SST_Grid, SST_dims)) return 0;
   SSTmin = 9999.; SSTmax = -999. ;
   for (ii=0; ii<SST_dims[0]*SST_dims[1]; ii++) {
      SST_Grid[ii] = SST_Grid[ii]-273.15;
      if (SST_Grid[ii]<SSTmin) SSTmin = SST_Grid[ii];
      if (SST_Grid[ii]>SSTmax) SSTmax = SST_Grid[ii];
   }


/*
 * read in the Temp fields, convert from degrees Kelvin to Celcius    
 */

   if (!Open_Grid_File( &Grid_File, Temp_filename, Temp_dims )) return 0;
   if (!Read_Grid_File( &Grid_File, &Temp_Grid, Temp_dims )) return 0;
   Tempmin = 9999.; Tempmax = -999; 
   for (ii=0; ii<Temp_dims[0]*Temp_dims[1]; ii++) {
      Temp_Grid[ii] = Temp_Grid[ii]-273.15;
      if (Temp_Grid[ii]<Tempmin) Tempmin = Temp_Grid[ii];
      if (Temp_Grid[ii]>Tempmax) Tempmax = Temp_Grid[ii];
   }

/*
 * read in the U and V component wind fields
 */

   if (!Open_Grid_File( &Grid_File, UWind_filename, Wind_dims )) return 0;
   if (!Read_Grid_File( &Grid_File, &UWind_Grid, Wind_dims )) return 0;

   if (!Open_Grid_File( &Grid_File, VWind_filename, Wind_dims )) return 0;
   if (!Read_Grid_File( &Grid_File, &VWind_Grid, Wind_dims )) return 0;

   return 1;
}
