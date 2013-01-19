#include "gui.h"


void Plot_Cruise( int, int, float *, float *, int *, size_t, float, float, float, 
									float );

void CruisePlot( void )

{

   FILE  *History_File;          
   int   n;
   float minlon, maxlon, minlat, maxlat;
   float out_lat, out_lon;
   int   year, month, day, hour, min;

   /* Create local version of record parameters */

   char  rec_obsdate[9], rec_obstime[5];
   float rec_lat, rec_lon, rec_sst, rec_slp, rec_airtemp, rec_dewp, rec_presschg3hr;
   float rec_windspd, rec_winddir;
   int   rec_cloud, rec_vis;
   int   rec_wx, rec_ice, rec_waveper;
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
   float rec_Edited_Value[8];
   char  rec_edit_callsign[9];
   char  rec_callsign[9];

   char  record[260];

/*-------------------------------------------------------------*/

   minlat =  180.0F;
   maxlat = -180.0F;
   minlon =  360.0F;
   maxlon =    0.0F;

   AutoZoomed = False;
   Zoomed = False;
  

   if ( (strcmp(callsign,"SHIP")!=0) &&
        ( (Current_Pri_Obs  >= 0 && Current_Pri_Obs < (int)NumObs) ||
          (Current_View_Obs >= 0) ) ){
  
      /* Collect the locations of the points.        */

      History_File = NULL;
      History_File = fopen( History_Filename, "r");

/*
 *    Read the history file, and fill up the Subset arrays to sent to Plot_Cruise.
 *    If buoy or CMAN, don't fill in date/time field (using SubsetIndex for this, handy integer
 *    array not in use right now)
 */
      n = 0;
      while ( fgets( record, sizeof(record)+1, History_File) != NULL) {
         ReadRec( record, rec_obsdate, rec_obstime, rec_callsign, &rec_lat, &rec_lon,
             &rec_slp,  &rec_airtemp, &rec_windspd, &rec_winddir, &rec_sst,
             &rec_dewp, &rec_presschg3hr, &rec_cloud, &rec_vis,  &rec_reptype, &rec_wx, 
             &rec_ice, &rec_waveper, &rec_model_slp,
             &rec_model_airtemp, &rec_model_windspd, &rec_model_winddir,
             &rec_model_sst, rec_edit_callsign, rec_Edited_Value, rec_QC_Flags,
             &rec_duplicate, &rec_suspect_slp,  &rec_suspect_airtemp, &rec_suspect_windspd,
             &rec_suspect_winddir, &rec_suspect_sst, &rec_suspect_dup, &rec_suspect_loc, &rec_suspect);
         getc(History_File); /*skip CR */

         Edited_Loc( &out_lat, &out_lon, rec_lat, rec_lon, rec_Edited_Value[1], rec_Edited_Value[2]);
         SubsetLon[n]=out_lon;  
         SubsetLat[n]=out_lat;  

         if ((rec_reptype==531) || (rec_reptype==532)){  /* buoy or CMAN */
            SubsetIndex[n] = -99;
            break;
         } else {
            sscanf(rec_obsdate,"%4d%2d%2d", &year, &month, &day);
            sscanf(rec_obstime, "%2d%2d", &hour, &min); 
            SubsetIndex[n] = day*100+hour; /* SubsetIndex is free right now, so use it for temp space */
         }
         n++;
         if (out_lon < minlon) minlon = out_lon;
         if (out_lon > maxlon) maxlon = out_lon;
         if (out_lat < minlat) minlat = out_lat;
         if (out_lat > maxlat) maxlat = out_lat;
       }
      fclose(History_File);

      if (n > 0) {


         Plot_Cruise( GKSWkid, 2, SubsetLon, SubsetLat, SubsetIndex, n,
                      minlon, minlat, maxlon, maxlat);
      }

      printf("after Plot_Cruise: callsign=%s", callsign);
   }
}   

/*=====================================================================*/

void Plot_Cruise( int GKSwid, int Color, float *flon, float *flat, 
		  int *Datime, size_t NumObs, float minlon, float minlat, 
		  float maxlon, float maxlat)
{

   float  *TempX, *TempY;
   float  out_lat, out_lon;
   float  leftlon, rightlon, toplat, bottomlat;
   int    lt, ln;
   float  x, y, max, char_ht;
   char   label[6];
   Gpoint text_pos;
   Gvec   char_up_right_vec, char_low_right_vec, char_right_vec;
   int    day, hour;
   size_t ii, latdivisions, londivisions;
/*---------------------------------------------------------------------*/

   char_up_right_vec.delta_x = -1.;
   char_up_right_vec.delta_y = 1.;

   char_low_right_vec.delta_x = 1.;
   char_low_right_vec.delta_y = 1.;

   char_right_vec.delta_x = 0.;
   char_right_vec.delta_y = 1.;

/* make temp arrays */

   if ((TempX = (float *) calloc( NumObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }

   if ((TempY = (float *) calloc( NumObs, sizeof(float))) == NULL) {
      fprintf(stderr,"memory problems\n");
      exit(1);
   }


/*
 * Compute right, left, top, and bottom.
 *
 * If the cruise crosses 0 longitude, convert to -180 -> 180,
 * Recompute left and right long limits
 *
 * Expand the box out 5 degrees in each direction
 *
 */

   /* printf("Plot_Cruise: in max/mins: (lon/lat): (%f,%f) (%f,%f)\n",
         maxlon, maxlat, minlon, minlat); */
   NhlClearWorkstation(Wkid);
   NhlRLClear(setrlist);
   toplat = maxlat; bottomlat = minlat;
   if ( fabs(maxlon - minlon) > 180.)  {   
      leftlon=0; rightlon=0;
      for (ii=0; ii<NumObs; ii++) {
         if (SubsetLon[ii] >180) SubsetLon[ii] = SubsetLon[ii] - 360;
         if (SubsetLon[ii]<leftlon) leftlon = SubsetLon[ii];
         if (SubsetLon[ii]>rightlon) rightlon = SubsetLon[ii];
      }
      leftlon = leftlon - 5;
      rightlon = rightlon + 5;
      if (leftlon < -180) leftlon = -180.;
      if (rightlon > 180) rightlon = 180;

      NhlRLSetFloat(setrlist,NhlNmpCenterLonF, 0.); 
   } else {
      leftlon = minlon;
      rightlon = maxlon;
      leftlon = leftlon - 5;
      rightlon = rightlon + 5;
      if (leftlon < 0) leftlon = 0.;
      if (rightlon > 359) rightlon = 359;

      NhlRLSetFloat(setrlist,NhlNmpCenterLonF, 180.); 
   }


   toplat = toplat + 5.; bottomlat = bottomlat - 5.;
   if (toplat > 180)  toplat = 180; 
   if (bottomlat < -180) bottomlat = -180; 


   /* printf("Plot_Cruise: after expanding box 5 degrees: left-top right-bottom: (%f,%f) (%f,%f)\n",
          leftlon, toplat, rightlon, bottomlat); */

   latdivisions =  abs(toplat - bottomlat);
   londivisions = abs(rightlon - leftlon);

   /* printf("londivisions = %d latdivisions = %d\n", londivisions ,latdivisions); */

/* Remove any contours and barbs, and reset the boundries of the map */

   if (Contour_SLP_On) {
      NhlRemoveOverlay(Mapid,Contour_SLP_id,-1);
      NhlDestroy(Contour_SLP_id);
   }
   if (Contour_SST_On) {
      NhlRemoveOverlay(Mapid,Contour_SST_id,-1);
      NhlDestroy(Contour_SST_id);
   }
   if (Contour_Temp_On) {
      NhlRemoveOverlay(Mapid,Contour_Temp_id,-1);
      NhlDestroy(Contour_Temp_id);
   }
   if (Wind_Vectors_On) {
      NhlRemoveOverlay(Mapid,Wind_id,-1);
      NhlDestroy(Wind_id);
   }

   NhlRLSetFloat(setrlist,NhlNvpYF,.75);
   NhlRLSetFloat(setrlist,NhlNvpXF,0.);
   NhlRLSetString(setrlist,NhlNmpLimitMode,"Corners");
   NhlRLSetString(setrlist,NhlNmpProjection, "CylindricalEquidistant");
   NhlRLSetFloat(setrlist,NhlNmpLeftCornerLatF,toplat);
   NhlRLSetFloat(setrlist,NhlNmpRightCornerLatF,bottomlat);
   NhlRLSetFloat(setrlist,NhlNmpLeftCornerLonF,leftlon);
   NhlRLSetFloat(setrlist,NhlNmpRightCornerLonF,rightlon);
   if (latdivisions>londivisions)
      NhlRLSetFloat(setrlist,NhlNvpHeightF,.5);
   else
      NhlRLSetFloat(setrlist,NhlNvpWidthF, 1.);
   NhlSetValues(Mapid, setrlist);
   NhlDraw(Mapid);  


/* activate the gks workstation, set marker type, size, and color  */

   gactivate_ws(GKSwid);
   gset_marker_size(.5);
   gset_marker_type(1);
   gset_marker_colr_ind(Color);
   gset_line_colr_ind(Color);


/* draw the lat values along left axis and lon across the bottom */

   gset_text_colr_ind(1); 
   max = (float) (latdivisions > londivisions) ? latdivisions : londivisions;
   char_ht=max/9.;   
   gset_char_ht(char_ht);   
   gset_char_up_vec( &char_right_vec );
   ln = (int) leftlon; 
   lt = (int) bottomlat+1;
   for (ii=1; ii<latdivisions; ii=ii+2,lt=lt+2) {
     sprintf(label, "%d", lt);
     label[4]='\0';
     c_maptrn(  lt, ln, &text_pos.x, &text_pos.y);
     gtext( &text_pos, label); 
   }

   ln = leftlon+1;
   lt = bottomlat;
   for (ii=1; ii<londivisions; ii=ii+2,ln=ln+2) {
     sprintf(label, "%d", ln);
     label[4]='\0';
     c_maptrn(  lt+.4, ln-.5, &text_pos.x, &text_pos.y); /* the .5 is to center the label. */
     gtext( &text_pos, label);
   }
 
   for (ii=0; ii<NumObs; ii++) {
       c_maptrn( flat[ii], flon[ii], &TempX[ii], &TempY[ii]);
   }

   c_points(TempX, TempY, NumObs, -4, 1);   

   if (Datime[0] != -99) {
      gset_text_colr_ind(1);
      for (ii=0; ii<NumObs-1; ii++) {
          day = Datime[ii]/100;
          hour = Datime[ii] - day*100;
          sprintf(label, "%d/%2.2d",day, hour );
          if ( (flon[ii] - flon[ii+1]) > 0.) {    /* next obs is to left so print slant up right */
             gset_char_up_vec( &char_up_right_vec );
             c_maptrn( flat[ii]+.3, flon[ii], &text_pos.x, &text_pos.y);
          } else {
             gset_char_up_vec( &char_low_right_vec );  /* next obs is to right, print slanted low right  */
             c_maptrn( flat[ii]-.3, flon[ii], &text_pos.x, &text_pos.y);   
          }
          gtext( &text_pos, label);
       }

      day = Datime[NumObs-1]/100;
      hour = Datime[NumObs-1] - day*100;
      sprintf(label, "%d/%2.2d",day, hour );
      if ((flon[NumObs-1] - flon[NumObs-2]) < 0.) {
         gset_char_up_vec( &char_up_right_vec );
         c_maptrn( flat[NumObs-1]+.3, flon[NumObs-1], &text_pos.x, &text_pos.y);
      } else {
         gset_char_up_vec( &char_low_right_vec );
            c_maptrn( flat[NumObs-1]-.3, flon[NumObs-1], &text_pos.x, &text_pos.y); 
      }
      gtext( &text_pos, label);
   }


   Edited_Loc( &out_lat, &out_lon, lat, lon, Edited_Value[1], Edited_Value[2]);
   c_maptrn( out_lat, out_lon, &x, &y);
   ii=1;
   gset_marker_colr_ind(Current_Obs_Color);
   c_points( &x, &y, ii, -3, 0);      /* put a yellow astrisk at the current obs */
   c_ngpict(GKSwid, 0);
   gdeactivate_ws(GKSwid);
   free(TempX);
   free(TempY);
  
}
