#include "gui.h"

/* ARGSUSED */
void ContourSLPCb(Widget wid, XtPointer client, XtPointer call) 
/************************************************************************
 *                                                                      *
 * ContourSLPCb                                                         *
 *   Toggle on/off SLP  contour                                         *
 *                                                                      *
 ***********************************************************************/
{

  if (Contour_SLP_On) {                      /* on, so toggle it off  */
     Contour_SLP_On = False;
     NhlClearWorkstation(Wkid);
     NhlRemoveOverlay(Mapid,Contour_SLP_id,-1); 
     NhlDestroy(Contour_SLP_id);
     Replot(0);
  } else {                                   /* off, so toggle it on   */  
     Contour_SLP_On = True;
     Draw_Contour( SLP_Grid,SLP_dims, &Contour_SLP_id, SLP_Contour_Color, 4.);
     NhlDraw(Mapid);
  }

}

/*======================================================================*/
/* ARGSUSED */
void ContourSSTCb(Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 *                                                                      *
 * ContourSSTCb                                                         *
 *   Toggle on/off SST  contour                                         *
 *                                                                      *
 ***********************************************************************/
{
  if (Contour_SST_On) {                      /* on, so toggle it off  */
     Contour_SST_On = False;
     NhlClearWorkstation(Wkid);
     NhlRemoveOverlay(Mapid,Contour_SST_id,-1);
     NhlDestroy(Contour_SST_id);
     Replot(0);
  } else {                                   /* off, so toggle it on   */
     Contour_SST_On = True;
     Draw_Contour( SST_Grid,SST_dims, &Contour_SST_id, SST_Contour_Color, 3.);
     NhlDraw(Mapid);
  }

}

/*======================================================================*/
/* ARGSUSED */
void ContourTempCb(Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 *                                                                      *
 * ContourTempCb                                                        *
 *   Toggle on/off Temp contour                                         *
 *                                                                      *
 ***********************************************************************/
{
  if (Contour_Temp_On) {                      /* on, so toggle it off  */
     Contour_Temp_On = False;
     NhlClearWorkstation(Wkid);
     NhlRemoveOverlay(Mapid,Contour_Temp_id,-1);
     NhlDestroy(Contour_Temp_id);
     Replot(0);
  } else {                                   /* off, so toggle it on   */
     Contour_Temp_On = True;
     Draw_Contour( Temp_Grid,Temp_dims, &Contour_Temp_id, Temp_Contour_Color, 3.);
     NhlDraw(Mapid);
  }

}

/*======================================================================*/

void Draw_Contour(
float *Grid,
int   *Dims,
int   *Contourid,
int   Color,
float Level)
/************************************************************************
 *                                                                      *
 * Draw_Contour                                                         *
 *   Draw contour on map.                                               *
 *                                                                      *
 * Input Parameters:                                                    *
 *                                                                      *
 *   Grid - grid to contour                                             *
 *   Dims - dimension of grid                                           *
 *   Color - line color                                                 *
 *   Level - intervals if map is not zoomed, use 1 if zoomed.           *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *  Contourid - id for resulting ContourPlot object.                    *
 *                                                                      *
 ***********************************************************************/
{
   int SubDims[2], lon1, lat1, lon2, lat2;
   int Gridid;
   int i, j, lt, ln, poslon;

if (Zoomed) {

   /* printf("Map boundries: %f %f to %f %f\n", MapBnd.lon[0], MapBnd.lat[0], MapBnd.lon[1],MapBnd.lat[1]); */

   lon1 = (int) (MapBnd.lon[0]-1.); lat1 = (int) (MapBnd.lat[0]+1.);
   lon2 = (int) (MapBnd.lon[1] +1.); lat2 = (int) (MapBnd.lat[1]-1.);

   SubDims[0] = abs(lat2 - lat1) + 1;               /* # rows */
   if (lon2 < lon1)                                 /* crosses 0 lon */
       SubDims[1] = lon2 + 360 - lon1;              /* # cols */
   else
      SubDims[1] = abs(lon2 - lon1) + 1;            /* # cols */

      i=0;
      for (lt=(90-lat1); lt<=(90-lat2); lt++) {       /* grid rows 0->180 represents lat -90 to 90  */
         ln=lon1;
         for (j=0; j<SubDims[1]; j++) {               /* #cols */
            poslon = ln;
            if (poslon < 0) poslon = poslon + 360; /*  is lon ever < 0 here? */
            SubGrid[i] = Grid[(lt*360)+poslon];
            i++;
            ln++;
         }
      }

   NhlRLClear(setrlist);
   NhlRLSetMDFloatArray(setrlist,NhlNsfDataArray,SubGrid,2,SubDims);
   NhlRLSetInteger(setrlist,NhlNsfXCStartV,lon1);
   NhlRLSetInteger(setrlist,NhlNsfXCEndV,lon2);
   NhlRLSetInteger(setrlist,NhlNsfYCStartV,lat1);
   NhlRLSetInteger(setrlist,NhlNsfYCEndV,lat2);
   NhlCreate(&Gridid,"grid",NhlscalarFieldClass,appid, setrlist);
 
 
} else {
   NhlRLClear(setrlist);
   NhlRLSetMDFloatArray(setrlist,NhlNsfDataArray,Grid,2,Dims);
   NhlRLSetInteger(setrlist,NhlNsfXCStartV,0);
   NhlRLSetInteger(setrlist,NhlNsfXCEndV,360);
   NhlRLSetInteger(setrlist,NhlNsfYCStartV,90);
   NhlRLSetInteger(setrlist,NhlNsfYCEndV,-90);
   NhlCreate(&Gridid,"grid",NhlscalarFieldClass,appid, setrlist);
}

/*
 * Create a ContourPlot object, supplying the ScalarField object as data
 */

   NhlRLClear(setrlist);
   NhlRLSetInteger(setrlist,NhlNcnScalarFieldData,Gridid);
   NhlRLSetInteger(setrlist,NhlNcnLineColor, Color);
   NhlRLSetString(setrlist,NhlNcnLineLabelsOn, "False");
   NhlRLSetString(setrlist,NhlNcnHighLabelsOn, "False");
   NhlRLSetString(setrlist,NhlNcnLowLabelsOn, "False");
   NhlRLSetString(setrlist,NhlNcnInfoLabelOn, "True");  

   NhlRLSetInteger(setrlist,NhlNcnInfoLabelFontHeightF, .005);
   NhlRLSetInteger(setrlist,NhlNcnInfoLabelFontColor, Color);
   NhlRLSetString(setrlist,NhlNcnInfoLabelPerimOn, "False");
  
   NhlRLSetFloat(setrlist,NhlNcnLevelSpacingF, Level);
   NhlCreate(Contourid,"ContourPlot",NhlcontourPlotClass,Wkid,setrlist);

   NhlAddOverlay(Mapid,*Contourid,-1);

}
