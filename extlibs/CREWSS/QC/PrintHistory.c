#include "gui.h"


int PrintHistory( char *callsign)
/************************************************************************
 * PrintHistory                                                         *
 *                                                                      *
 * This function is the callback for the history button.  It accesses   *
 * all obs for the current callsign, and prints the current suspect     *
 * field and the predicted model value.  It calculates the average      *
 * difference between the observed and predicted value.                 *
 * Return the number of obs printed.                                    *
 **									*
 * Log:									*
 *									*
 * T. Piper/SAIC	01/06	Removed duplicate fclose(History_File)	*
 ***********************************************************************/
{
   FILE *History_File;
   int  nbytes, textpos, recpos, Nobs, n, heading_length;
   int  nslp, nspd, ndir, nsst, ntemp;
   int  nslpgross, nspdgross, ndirgross, nsstgross, ntempgross;
   char record[270], heading[250], historyrec[250];

   /* Create local version of record parameters */

   char  rec_obsdate[9], rec_obstime[5];
   float rec_lat, rec_lon, rec_sst, rec_slp, rec_airtemp, rec_dewp, rec_presschg3hr;
   float rec_windspd, rec_winddir;
   int   rec_cloud, rec_vis, rec_ice, rec_waveper; 
   int   rec_wx;
   int   rec_reptype;
   float rec_model_slp, rec_model_sst, rec_model_airtemp;  
   float rec_model_winddir, rec_model_windspd;
   int   rec_QC_Flags[8];                            
   int   rec_duplicate;                             
   int   rec_suspect_dup;                          
   int   rec_suspect_slp, rec_suspect_sst;            
   int   rec_suspect_airtemp, rec_suspect_windspd;   
   int   rec_suspect_winddir;
   int   rec_suspect_loc;       
   int   rec_suspect;       
   float out_lat, out_lon;
   float rec_Edited_Value[8];    
   char  rec_edit_callsign[9];
   int   Moved;
   char  HistoryField[60];
   char  prev_obsdate[9], prev_obstime[5];
   float prev_lat, prev_lon, speed;
   float total_slp_diff, total_sst_diff, total_airtemp_diff, total_windspd_diff, total_winddir_diff;
   float total_slp_gross_diff, total_sst_gross_diff, total_airtemp_gross_diff, total_windspd_gross_diff, total_winddir_gross_diff;

/*---------------------------------------------------------------------------------*/

   if (strcmp(callsign,"SHIP") == 0) {
      XmTextInsert( HistoryText, 0, "                   History not allowed for callsign SHIP");
   }  else {

      textpos=0;

      /* Write headings for the fields to be displayed (current rec's suspect field(s) & model vals   */

      recpos = sprintf(heading, " obsdate  obstime    lat       lon      speed ");   

      nbytes = sprintf(&heading[recpos], "  slp   mod_slp  diff  ");   
      recpos = recpos + nbytes;

      if (suspect_airtemp) {
         nbytes = sprintf(&heading[recpos], "  airtemp mod_temp diff  " );   
         recpos = recpos + nbytes;
      }
       
      nbytes = sprintf(&heading[recpos], "  windspd mod_spd  diff   winddir mod_dir  diff  " );    
      recpos = recpos + nbytes;

      if (suspect_sst) {
         nbytes = sprintf(&heading[recpos], "   sst  mod_sst  diff   " );
         recpos = recpos + nbytes;
      }

      nbytes = sprintf(&heading[recpos], "Slp Tmp Wsp Wdr Sst Dup");  
      recpos = recpos + nbytes;

      strcat(heading, "\n\n"); recpos = recpos + 2;
      XmTextInsert( HistoryText, textpos, heading);
      textpos = textpos + recpos;
      heading_length = recpos;


      /* Read in each rec, print out chosen fields. Generate statistics. */

      Nobs=0;
      total_slp_diff = 0.;
      total_sst_diff = 0.;
      total_airtemp_diff = 0.;
      total_windspd_diff = 0.;
      total_winddir_diff = 0.;
      total_slp_diff = 0; 
      total_sst_diff = 0; 
      total_airtemp_diff = 0; 
      total_windspd_diff = 0; 
      total_winddir_diff = 0;
      total_slp_gross_diff = 0; 
      total_sst_gross_diff = 0; 
      total_airtemp_gross_diff = 0; 
      total_windspd_gross_diff = 0; 
      total_winddir_gross_diff = 0;



      History_File = fopen(History_Filename, "r");

      nslp = 0;
      ndir = 0;
      nspd = 0;
      nsst = 0;
      ntemp = 0;
      nslpgross = 0;
      nspdgross = 0;
      ndirgross = 0;
      ntempgross = 0;
      nsstgross = 0;
      while ( fgets( record, sizeof(record)+1, History_File) != NULL) {
         n = ReadRec( record, rec_obsdate, rec_obstime, callsign, &rec_lat, &rec_lon,
                      &rec_slp,  &rec_airtemp, &rec_windspd, &rec_winddir, &rec_sst,
                      &rec_dewp, &rec_presschg3hr, &rec_cloud, &rec_vis,  &rec_reptype, &rec_wx, 
                      &rec_ice, &rec_waveper, &rec_model_slp,
                      &rec_model_airtemp, &rec_model_windspd, &rec_model_winddir,
                      &rec_model_sst, rec_edit_callsign, rec_Edited_Value, rec_QC_Flags,
                      &rec_duplicate, &rec_suspect_slp,  &rec_suspect_airtemp, &rec_suspect_windspd,
                      &rec_suspect_winddir, &rec_suspect_sst, &rec_suspect_dup, &rec_suspect_loc, &rec_suspect);
          if (n>0) {

             Nobs++;

             recpos = sprintf(historyrec, "%s   %s  ", rec_obsdate, rec_obstime);

             Moved = Edited_Loc( &out_lat, &out_lon, rec_lat, rec_lon, rec_Edited_Value[1], rec_Edited_Value[2]);

             if (out_lat > 0) {
                nbytes = sprintf(&historyrec[recpos], "%7.2f N ", out_lat); 
             } else {
                nbytes = sprintf(&historyrec[recpos], "%7.2f S ", -out_lat);
             }
             recpos = recpos + nbytes;


             if (out_lon > 180) {
                nbytes = sprintf(&historyrec[recpos], "%7.2f W  ", 360-out_lon); 
             } else {
                nbytes = sprintf(&historyrec[recpos], "%7.2f E  ", out_lon); 
             }
             recpos = recpos + nbytes;

             if (Nobs != 1) {
                speed = Speed(prev_lat, prev_lon, out_lat, out_lon,
                        prev_obsdate, prev_obstime, rec_obsdate, rec_obstime);
             } else {
                speed = -99.;
             }
             prev_lat = out_lat;
             prev_lon = out_lon;
             strcpy(prev_obsdate,rec_obsdate);
             strcpy(prev_obstime,rec_obstime);

             if (speed >= 0.) 
                nbytes = sprintf(&historyrec[recpos], "%7.1f  ", speed);
             else
                nbytes = sprintf(&historyrec[recpos], "         ");
             recpos = recpos + nbytes;

       
             MakeHistoryField(rec_slp, rec_model_slp, 10., Moved, &total_slp_diff,  
                              &nslp, &total_slp_gross_diff, &nslpgross, HistoryField);
             nbytes = sprintf(&historyrec[recpos], "%s  ", HistoryField); 
             recpos = recpos + nbytes;

             if (suspect_airtemp) {
                MakeHistoryField(rec_airtemp, rec_model_airtemp, 15., Moved, &total_airtemp_diff, 
                                 &ntemp, &total_airtemp_gross_diff,  &ntempgross, HistoryField);
                nbytes = sprintf(&historyrec[recpos], "%s  ", HistoryField);
                recpos = recpos + nbytes;
             }

             MakeHistoryFieldWind(rec_windspd, rec_winddir, rec_model_windspd, rec_model_winddir,
                              Moved, &total_windspd_diff, &total_winddir_diff, 
                              &nspd, &ndir, &total_windspd_gross_diff, &total_winddir_gross_diff, 
                              &nspdgross, &ndirgross, HistoryField);
             nbytes = sprintf(&historyrec[recpos], "%s  ", HistoryField);
             recpos = recpos + nbytes;

             if (suspect_sst) {
                MakeHistoryField(rec_sst, rec_model_sst, 15., Moved, &total_sst_diff, 
                                 &nsst, &total_sst_gross_diff, &nsstgross, HistoryField);
                nbytes = sprintf(&historyrec[recpos], "%s  ", HistoryField);
                recpos = recpos + nbytes;
             }


             nbytes = sprintf(&historyrec[recpos], "%d   %d   %d   %d   %d  %d", rec_QC_Flags[3], 
                      rec_QC_Flags[4], rec_QC_Flags[5], rec_QC_Flags[6], rec_QC_Flags[7], rec_duplicate);
             recpos = recpos + nbytes;

             strcat(historyrec, "\n"); recpos++;
             XmTextInsert( HistoryText, textpos, historyrec); 
             textpos = textpos + recpos;
          }
       }
       fclose(History_File);

       if (Nobs > 10) {          /* if more than 15 obs, but heading on bottom, too */
          strcpy(historyrec, "\n"); 
          XmTextInsert( HistoryText, textpos, historyrec);
          textpos++;

          XmTextInsert( HistoryText, textpos, heading);
          textpos = textpos + heading_length;
       }

       strcpy(historyrec, "\n"); 
       XmTextInsert( HistoryText, textpos, historyrec);
       textpos++;

       if (nslp > 0) {
          recpos = sprintf(historyrec, "mean diff slp     = %6.2f  #obs = %3d    ", total_slp_diff/nslp, nslp );
          XmTextInsert( HistoryText, textpos, historyrec);
          textpos = textpos + recpos;
       }

       if (nslpgross > 0) {
          recpos = sprintf(historyrec, "mean diff slp      with gross= %6.2f  #obs = %3d\n", 
          (total_slp_diff+total_slp_gross_diff)/(nslp+nslpgross), nslp+nslpgross );
          XmTextInsert( HistoryText, textpos, historyrec);
          textpos = textpos + recpos;
       } else {
          XmTextInsert( HistoryText, textpos, "\n");
          textpos++;
       }


       if (suspect_airtemp && ntemp > 0) {
          recpos = sprintf(historyrec, "mean diff airtemp = %6.2f  #obs = %3d    ", total_airtemp_diff/ntemp, ntemp );
          XmTextInsert( HistoryText, textpos, historyrec);
          textpos = textpos + recpos;
       }

       if (ntempgross > 0) {
          recpos = sprintf(historyrec, "mean diff airtemp  with gross= %6.2f  #obs = %3d \n",
          (total_airtemp_diff+total_airtemp_gross_diff)/(ntemp+ntempgross), ntemp+ntempgross );
          XmTextInsert( HistoryText, textpos, historyrec);
          textpos = textpos + recpos;
       } else {
          XmTextInsert( HistoryText, textpos, "\n");
          textpos++;
       }

       if (suspect_sst && nsst > 0) {
          recpos = sprintf(historyrec, "mean diff sst     = %6.2f  #obs = %3d    ", total_sst_diff/nsst, nsst );
          textpos = textpos + recpos;
          XmTextInsert( HistoryText, textpos, historyrec);
        }
       if (nsstgross > 0) {
          recpos = sprintf(historyrec, "mean diff sst  with gross= %6.2f  #obs = %3d \n",
          (total_sst_diff+total_sst_gross_diff)/(nsst+nsstgross), nsst+nsstgross );
          XmTextInsert( HistoryText, textpos, historyrec);
          textpos = textpos + recpos;
       } else {
          XmTextInsert( HistoryText, textpos, "\n");
          textpos++;
       }


       if ( nspd > 0)  {
          recpos = sprintf(historyrec, "mean diff windspd = %6.2f  #obs = %3d    ", 
                   total_windspd_diff/nspd, nspd);
          textpos = textpos + recpos;
          XmTextInsert( HistoryText, textpos, historyrec);

          if (nspdgross > 0) {
             recpos = sprintf(historyrec, "mean diff windspd  with gross= %6.2f  #obs = %3d \n",
             (total_windspd_diff+total_windspd_gross_diff)/(nspd+nspdgross), nspd+nspdgross );
             XmTextInsert( HistoryText, textpos, historyrec);
             textpos = textpos + recpos;
          } else {
             XmTextInsert( HistoryText, textpos, "\n");
             textpos++;
          }

          recpos = sprintf(historyrec, "mean diff winddir = %6.2f  #obs = %3d    ", 
                   total_winddir_diff/ndir, ndir);
          textpos = textpos + recpos;
          XmTextInsert( HistoryText, textpos, historyrec);

          if (ndirgross > 0) {
             recpos = sprintf(historyrec, "mean diff winddir  with gross= %6.2f  #obs = %3d \n",
             (total_winddir_diff+total_winddir_gross_diff)/(ndir+ndirgross), ndir+ndirgross );
             XmTextInsert( HistoryText, textpos, historyrec);
             textpos = textpos + recpos;
          } else {
             XmTextInsert( HistoryText, textpos, "\n");
             textpos++;
          }
       }

       recpos = sprintf(historyrec, "\n1 = accept 2 = reject 3 = changed \n");
       textpos = textpos + recpos;
       XmTextInsert( HistoryText, textpos, historyrec);

       return Nobs;

    }
    return(0);
}

/*==============================================================================*/

void MakeHistoryField( float data_val, float model_val, float gross_error, int moved, 
                       float *diff_total, int *nvals, float *diff_gross_total, int *ngross, 
                       char *HistoryField ) 
{
    int nodata;
    char data[7], model[7], diff[7];

    nodata = null_data(data_val);
    if (!nodata) sprintf(data,"%6.1f", data_val);    else strcpy(data, "      ");
    if (!moved)  sprintf(model, "%6.1f", model_val); else strcpy(model, "      ");
    if (!nodata && !moved) {
       sprintf(diff, "%6.1f", data_val-model_val);
       if (f_abs(data_val-model_val) <= gross_error) {
          *diff_total = *diff_total + data_val-model_val;
          (*nvals)++;
       } else {
          *diff_gross_total = *diff_gross_total + data_val-model_val;
          (*ngross)++;
       }
    } else 
       strcpy(diff,"      ");
    sprintf(HistoryField, "%s %s %s  ", data, model, diff);

}

/*===============================================================================*/
void MakeHistoryFieldWind( float windspd_val, float winddir_val, 
                           float model_spd_val, float model_dir_val, 
                           int moved, float *diff_spd_total, float *diff_dir_total, 
                           int *nspd, int *ndir,float *diff_spd_gross_total, 
                           float *diff_dir_gross_total, int *nspdgross, int *ndirgross, 
                           char *HistoryField )
{
    int nospd, nodir;
    float dirdif, spddif;
    char windspd[7], winddir[7], model_spd[7], model_dir[7], spd_diff[7], dir_diff[7];

    nospd= null_data(windspd_val);
    nodir= null_data(winddir_val);

    if (!nospd) 
       sprintf(windspd,"%6.1f", windspd_val);
    else
       strcpy(windspd, "      ");

    if (!nodir)
       sprintf(winddir,"%6.1f", winddir_val);
    else
       strcpy(winddir,"      ");

    if (!moved)  {
       sprintf(model_spd, "%6.1f", model_spd_val); 
       sprintf(model_dir, "%6.1f", model_dir_val); 
    } else {
       strcpy(model_spd,"      ");
       strcpy(model_dir,"      ");
    }
    if (!nodir && !moved) {

       dirdif = f_abs(winddir_val-model_dir_val);
       if (dirdif > 180.) dirdif = 360. - dirdif;
       sprintf(dir_diff, "%6.1f", dirdif);
       if (dirdif <= 100.)  {
          *diff_dir_total = *diff_dir_total + dirdif;
          (*ndir)++;
       } else {
          *diff_dir_gross_total = *diff_dir_gross_total + dirdif;
          (*ndirgross)++;
       }
    } else
       strcpy(dir_diff,"      ");
      
    if (!nospd && !moved) {
       spddif = f_abs(windspd_val-model_spd_val);
       sprintf(spd_diff, "%6.1f", windspd_val-model_spd_val);
       if (spddif <= 30.) {
          *diff_spd_total = *diff_spd_total + windspd_val-model_spd_val;
          (*nspd)++;
       } else {
          *diff_spd_gross_total = *diff_spd_gross_total + windspd_val-model_spd_val;
          (*nspdgross)++;
       }
    } else   
       strcpy(spd_diff,"      ");
     

    sprintf(HistoryField, "%s  %s  %s  %s  %s  %s", windspd, model_spd, spd_diff, winddir, model_dir, dir_diff);

  /* try removing all four spaces from end? */
}
