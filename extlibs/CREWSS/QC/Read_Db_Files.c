#include "gui.h"

/****************************************************************
 *  Read in latest obs database.  Store the lat & lon values.	*
 *								* 
 ***************************************************************/

int Read_Db_Files (void)

{
   FILE *Temp_File;
   struct dirent *this_qc_file;
   DIR  *qcdir;
   char *Temp_Filename;
   int ncall, nthiscall, nobs;
   int NumCallsigns;
   int NumShips;

/*-------------------------------------------------------------*/

   if ((Temp_Filename = (char *)  calloc( strlen(Dated_QC_Directory)+13, 1)) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

/* Open the nobs file for # obs in the db files */

   strcpy(Temp_Filename, Dated_QC_Directory);
   strcat(Temp_Filename, "/nobs");

   if ((Temp_File = fopen(Temp_Filename,"r")) == NULL) {
      PopupErrorMsg("No data available for the requested cycle");
      return 0;
   }
   fscanf(Temp_File,"%i", &NumObs);
   printf("Num obs expected in db files: %d\n", NumObs);
   fclose(Temp_File);


   /* Count number of callsigns in the Dated QC Directory */

   NumCallsigns = 0;
   qcdir = opendir(Dated_QC_Directory);
   if (qcdir == NULL) {
      PopupErrorMsg( "Error opening dated data directory");
      return 0;
   } else {
      while ((this_qc_file = readdir( qcdir )) != NULL) {
         if ( (strcmp(this_qc_file->d_name, "nobs") != 0) &&
            (strcmp(this_qc_file->d_name, "Flags") != 0)) {
            strcpy(Db_Filename, Dated_QC_Directory);
            strcat(Db_Filename, "/");
            strcat(Db_Filename, this_qc_file->d_name);
            if (!  IsDir(Db_Filename)) NumCallsigns++;
         }
      }
      closedir(qcdir);
   }
 
   printf("number of callsigns: %i\n", NumCallsigns);


/* Open the ShipNames file */
   Open_ShipNames_File( &ShipNames_File, &NumShips);

/* Allocate Lat, Lon, and Suspect arrays. */

   if ((Lat = (float*) calloc( NumObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }
   if ((Lon = (float*) calloc( NumObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   if ((Suspect = (int*) calloc( NumObs, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

    if ((Callsigns = (char *) calloc( NumCallsigns, 9)) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

    if ((ShipNames_Index = (int *) calloc( NumCallsigns, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }


   if ((Db_Index = (int *) calloc( NumObs, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }


   if ((Callsign_Index = (int *) calloc( NumObs, sizeof(int))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   printf("Reading in observations...\n");
   ncall = -1;
   nobs = -1;
   qcdir = opendir(Dated_QC_Directory);
   if (qcdir == NULL) {
      PopupErrorMsg( "Error opening dated data directory");
      return 0;
   } else {


/* For each file (callsign), save the callsign in the callsign array.
 * Find the shipname for the callsign, and save index into the shipname file
 * in the ShipNames_Index array.  
 *
 * For each record in each file
 *   - Add the lat & lon to the Lat and Lon arrays.
 *   - Add the suspect value to the Suspect array.
 *   - save the index into the current file in the Db_Index array.
 *   - save the index for the current callsign (current filename) in the Callsign_Index array.
 *   - count as a Suspect, Nonsuspect, or QCed obs.
 */
 


      while ((this_qc_file = readdir( qcdir )) != NULL) {
         if ((strcmp(this_qc_file->d_name, ".") != 0) &&
             (strcmp(this_qc_file->d_name, "..") && 
             (strcmp(this_qc_file->d_name, "nobs") != 0) &&
             (strcmp(this_qc_file->d_name, "Flags") != 0) &&
             (strcmp(this_qc_file->d_name, "grids") != 0) &&
             (strlen(this_qc_file->d_name) <= (size_t)8))) {       /* if over 8 chars, it's not a callsign */
             strcpy(Db_Filename, Dated_QC_Directory);
             strcat(Db_Filename, "/");
             strcat(Db_Filename, this_qc_file->d_name);
             ncall++;
             strcpy((Callsigns + ncall*9), this_qc_file->d_name );

             *(ShipNames_Index + ncall) = Search_For_Ship_Name(ShipNames_File, this_qc_file->d_name, 0, NumShips-1) ;
             /* printf("Ship name index for %s is %d\n", this_qc_file->d_name, *(ShipNames_Index + ncall)); */

             nthiscall = 0;
             Db_File = NULL;
             while( ReadWriteObs( &Db_File, Db_Filename, nthiscall, 1,
                   obsdate, obstime, callsign, &lat, &lon,
                   &slp,  &airtemp, &windspd, &winddir, &sst,
                   &dewp, &presschg3hr, &cloud, &vis,  &reptype, &wx, 
                   &ice, &waveper, &model_slp,
                   &model_airtemp, &model_windspd, &model_winddir,
                   &model_sst, edit_callsign, Edited_Value, QC_Flags,
                   &duplicate, &suspect_slp,  &suspect_airtemp, &suspect_windspd,
                   &suspect_winddir, &suspect_sst, &suspect_dup, &suspect_loc, &suspect) != -1) {
                nobs++;


                if (!null_data(Edited_Value[1])) {
                   *(Lat + nobs) = Edited_Value[1];
                } else
                   *(Lat + nobs) = lat;

                if (!null_data(Edited_Value[2])) {
                   *(Lon + nobs) = Edited_Value[2];
                } else
                   *(Lon + nobs) = lon;

                *(Suspect + nobs) = suspect;
                *(Callsign_Index + nobs) = ncall;  /* keep index into Callsigns array */
                *(Db_Index + nobs) = nthiscall;


               /* Add obs to one of three lists: Suspect, NonSuspect, or QCed */
               switch (suspect) {
                  case 0: NumNonsuspect++; break;
                  case 1: NumSuspect++; break;
                  case 2: NumQCed++; break;
               }  


                nthiscall++;
             } 
             fclose(Db_File); 
         }
      }
      closedir(qcdir);
      ncall++;

   }

   NumObs = nobs+1;
   printf("Read %d obs\n", nobs+1);
/*
   for (i=0; i<10; i++)
      printf("obs %i is in callsign index %i which is file %s and is at (%f,%f)\n", i, Callsign_Index[i], (Callsigns + (Callsign_Index[i]*9)), *(Lat + i), *(Lon + i));

*/

   return 1;
   
}

