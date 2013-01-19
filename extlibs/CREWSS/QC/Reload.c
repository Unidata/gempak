#include "gui.h"


void Reload (char *Date, char *Cycle )
/************************************************************************
 * Reload                                                               *
 *                                                                      *
 * This function frees (if allocated) and reallocates the arrays for    *
 * obs and grids, and reads in the obs and grids for the specified      *
 * date and cycle.                                                      *
 *                                                                      *
 ************************************************************************/
{

char  Date_str[10];


   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */

/*
 * Set the Reload button to red, change label to "Reading..." and clear the map to 
 * let the user know something is happening.
 */

   XtVaSetValues(MenuButtons[11],  XmNforeground, Red.pixel, XmNlabelString, XmStringCreateLocalized("Reading..."), NULL);
   XmUpdateDisplay( ObsWindow );
   NhlClearWorkstation(Wkid);
   ClearFields();

/* free the obs arrays */

   free(Lat);
   free(Lon);
   free(Suspect);
   free(Callsigns);
   free(Db_Index);
   free(Callsign_Index);
   free(X);
   free(Y);
   free(Priority);
   free(SubsetLat);
   free(SubsetLon);
   free(SubsetIndex);
   free(Viewed);
   free(Bookmark_Viewed);

   strcpy(Date_str,Date);
   strcat(Date_str, "_");
   strcat(Date_str, Cycle);

/* Create the filename strings */

   MakeFilenameStrings(Date_str);


/* Initialize some global variables */

   Initialize();

/*
 * read in grid files
 */

  Get_Grids( SLP_Filename, SST_Filename, Temp_Filename, UWind_Filename, VWind_Filename);

/*
 * Read in Observations
 */

   if (Read_Db_Files()) {

/* recreate obs processing arrays */

      MakeObsArrays(NumObs);


/* Plot the obs on the world map */

      WorldMapCb(NULL, NULL, NULL);   
}

/* Set the Reload button back to black */

   XtVaSetValues(MenuButtons[11],  XmNforeground, Black.pixel, XmNlabelString, XmStringCreateLocalized("Date/Cycle"), NULL);
}


void MakeObsArrays(size_t NObs )
{

   size_t ii;

/* Allocate all the data and observation processing arrays */


   if ((X = (float*) calloc( NObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }
   if ((Y = (float*) calloc( NObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }


   if ((Priority = (int*) calloc( NObs, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   for (ii=0;ii<NObs;ii++) {
      Priority[ii] = -1;
   }

   if ((SubsetLat = (float*) calloc( NObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }
   if ((SubsetLon = (float*) calloc( NObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   if ((SubsetIndex = (int*) calloc( NObs, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   if ((Viewed = (int*) calloc( NumObs*2, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   if ((Bookmark_Viewed = (int*) calloc( NumObs*2, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

}

/*=============================================================================*/

void MakeFilenameStrings( char *Date_str) {

   strcpy(Dated_QC_Directory, QC_Directory);
   strcat(Dated_QC_Directory, "/");
   strcat(Dated_QC_Directory, Date_str);
   /* printf("Dated directory is %s\n", Dated_QC_Directory); */

   strcpy(Flag_Filename, Dated_QC_Directory);
   strcat(Flag_Filename, "/Flags_");
   strcat(Flag_Filename, Date_str);

   strcpy(SLP_Filename, Dated_QC_Directory);
   strcat(SLP_Filename, "/grids/SLP_Grid");

   strcpy(SST_Filename, Dated_QC_Directory);
   strcat(SST_Filename, "/grids/SST_Grid");

   strcpy(Temp_Filename, Dated_QC_Directory);
   strcat(Temp_Filename, "/grids/Temp_Grid");

   strcpy(UWind_Filename, Dated_QC_Directory);
   strcat(UWind_Filename, "/grids/UWind_Grid");

   strcpy(VWind_Filename, Dated_QC_Directory);
   strcat(VWind_Filename, "/grids/VWind_Grid");

}

