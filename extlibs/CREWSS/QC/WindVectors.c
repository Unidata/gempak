#include "gui.h"

/* ARGSUSED */
void WindVectorsCb(Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 *                                                                      *
 * WindVectorsCb                                                        *
 *   Toggle the wind vector overlay on the map.                         *
 *                                                                      *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

  if (Wind_Vectors_On) {                             /* on, so toggle it off  */
       Wind_Vectors_On = False;
       NhlClearWorkstation(Wkid);
       NhlRemoveOverlay(Mapid,Wind_id,-1);
       NhlDestroy(Wind_id);
       Replot(0);
  } else {                                   /* off, so toggle it on   */
       Wind_Vectors_On = True;
       Draw_Vectors( UWind_Grid, VWind_Grid, &Wind_id);

   }
}

/*=====================================================================*/

void Draw_Vectors( float *UGrid, float *VGrid, int *Vectorid)
/************************************************************************
 *                                                                      *
 * Draw_Vectors                                                         *
 *                                                                      *
 * Input:                                                               *
 *                                                                      *
 *   UGrid - grid for u component                                       *
 *   VGrid - grid for v component                                       *
 *                                                                      *
 * Output parameters:                                                   *
 *   Contourid - id for resulting ContourPlot object.                   *
 *                                                                      *
 ***********************************************************************/
{
   int SubDims[2], lon1, lat1, lon2, lat2;
   int Gridid;
   int i, j, lt, ln, poslon;
   float Stride;

/*---------------------------------------------------------------------*/

   if (Zoomed) {

      /* printf("In Draw_Vectors--Map boundries: %f %f to %f %f\n", MapBnd.lat[0], MapBnd.lon[0], MapBnd.lat[1],MapBnd.lon[1]);  */
   
      lon1 = (int) (MapBnd.lon[0]-1.); lat1 = (int) (MapBnd.lat[0]+1.);
      lon2 = (int) (MapBnd.lon[1] +1.); lat2 = (int) (MapBnd.lat[1]-1.);


      SubDims[0] = abs(lat2 - lat1) + 1;               /* # rows */
      if (lon2 < lon1)                                 /* crosses 0 lon */
          SubDims[1] = lon2 + 360 - lon1;              /* # cols */
      else
         SubDims[1] = abs(lon2 - lon1) + 1;            /* # cols */

      /* extract subgrid of wind */
      /* convert from m/sec to knots */


      i=0;
      for (lt=(90-lat1); lt<=(90-lat2); lt++) {       /* grid rows 0->180 represents lat -90 to 90  */
         ln=lon1; 
         for (j=0; j<SubDims[1]; j++) {               /* #cols */
            poslon = ln;
            if (poslon < 0) poslon = poslon + 360; /*  is lon ever < 0 here? */
            SubGrid[i] = UGrid[(lt*360)+poslon]/.5148;
            SubGrid2[i] = VGrid[(lt*360)+poslon]/.5148;
            i++;
            ln++;
         }
      }

      NhlRLSetMDFloatArray(setrlist,NhlNvfUDataArray,SubGrid,2,SubDims);
      NhlRLSetMDFloatArray(setrlist,NhlNvfVDataArray,SubGrid2,2,SubDims);
      NhlRLSetInteger(setrlist,NhlNvfXCStartV,lon1);
      NhlRLSetInteger(setrlist,NhlNvfXCEndV,lon2);
      NhlRLSetInteger(setrlist,NhlNvfYCStartV,lat1);
      NhlRLSetInteger(setrlist,NhlNvfYCEndV,lat2);
      Stride = 1.;

   } else {
      NhlRLSetMDFloatArray(setrlist,NhlNvfUDataArray, UWind_Grid,2,Wind_dims);
      NhlRLSetMDFloatArray(setrlist,NhlNvfVDataArray, VWind_Grid,2,Wind_dims);
      NhlRLSetInteger(setrlist,NhlNvfXCStartV,0);
      NhlRLSetInteger(setrlist,NhlNvfXCEndV,360);
      NhlRLSetInteger(setrlist,NhlNvfYCStartV,90);
      NhlRLSetInteger(setrlist,NhlNvfYCEndV,-90);
      Stride = 10.;
   }

   NhlRLSetInteger(setrlist,NhlNvfXCStride, Stride);
   NhlRLSetInteger(setrlist,NhlNvfYCStride, Stride);
   NhlRLSetFloat(setrlist,NhlNvfMissingUValueV, -99.);
   NhlRLSetFloat(setrlist,NhlNvfMissingVValueV, -99.);

   NhlCreate(&Gridid,"UVgrid",NhlvectorFieldClass,appid, setrlist);

/*
 * Create a VectorPlot object, supplying the VectorField object as data
 */

   NhlRLClear(setrlist);
   NhlRLSetInteger(setrlist,NhlNvcVectorFieldData,Gridid);
   if (Zoomed) {
      NhlRLSetFloat(setrlist,NhlNvcRefLengthF, .02); 
      NhlRLSetFloat(setrlist,NhlNvcRefMagnitudeF, 15.);
   }
   NhlRLSetString(setrlist,NhlNvcRefAnnoSide, "Right");
   NhlRLSetString(setrlist,NhlNvcRefAnnoPerimOn, "Off");
   NhlRLSetFloat(setrlist,NhlNvcRefAnnoArrowMinOffsetF, .1);
   NhlRLSetFloat(setrlist,NhlNvcRefAnnoArrowSpaceF, .1);
   NhlRLSetFloat(setrlist,NhlNvcRefAnnoAngleF, 90.); 
   NhlRLSetFloat(setrlist,NhlNvcRefAnnoArrowAngleF, 90.);
   NhlRLSetString(setrlist,NhlNvcMonoLineArrowColor, "True");
   NhlRLSetInteger(setrlist,NhlNvcLineArrowColor, 1);      /* white arrows */

   NhlCreate(Vectorid,"VectorPlot",NhlvectorPlotClass,Wkid,setrlist);

   NhlAddOverlay(Mapid,*Vectorid,-1);
   NhlDraw(Mapid); 

}
