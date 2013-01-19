#include "gui.h"


/*ARGSUSED*/
void DisplaySLPValCb( Widget widget, int tag, XtPointer cb_data)
/************************************************************************
 *                                                                      *
 * DisplaySLPValCb                                                      *
 *   Display the model SST  value                                       *
 *                                                                      *
 ***********************************************************************/
{

DisplayModelVal( 0, "slp", SLP_Grid, SLP_Grid, SLP_dims );

}

/*======================================================================*/
/*ARGSUSED*/
void DisplaySSTValCb( Widget widget, int tag, XtPointer cb_data)
/************************************************************************
 *                                                                      *
 * DisplaySSTValCb                                                      *
 *   Display the model SST  value                                       *
 *                                                                      *
 ***********************************************************************/
{

DisplayModelVal( 0, "sst", SST_Grid, SST_Grid, SST_dims );

}

/*======================================================================*/
/*ARGSUSED*/
void DisplayTempValCb( Widget widget, int tag, XtPointer cb_data)
/************************************************************************
 *                                                                      *
 * DisplayTempValCb                                                     *
 *   Display the model temp value                                       *
 *                                                                      *
 ***********************************************************************/
{

DisplayModelVal( 0, "air temp", Temp_Grid, Temp_Grid, Temp_dims );

}

/*======================================================================*/
/*ARGSUSED*/
void DisplayWindSpdValCb( Widget widget, int tag, XtPointer cb_data)
/************************************************************************
 *                                                                      *
 * DisplayWindSpdValCb                                                  *
 *   Display the model wind speed value                                 *
 *                                                                      *
 ***********************************************************************/
{

DisplayModelVal( 1, "wind speed", UWind_Grid, VWind_Grid, Wind_dims );

}

/*======================================================================*/

void DisplayModelVal( int Is_Wind, char *Param, float *Grid1, 
						float *Grid2, int *Dims)
/************************************************************************
 *                                                                      *
 * DisplayModelVal                                                      *
 *   accept a mouse click on the map, retrieve the model value for      *
 *   that point, and return the value.                                  *
 *                                                                      *
 * If Is_Wind is false, access the corresponding value from Grid1, and  *
 *                      ignore Grid2.                                   *
 * If Is_Wind is true,  then Grid1 is U and Grid2 is V.  Convert the    *
 *                      U/V values to speed and direction, and display. *
 *                                                                      *
 *                                                                      *
 ***********************************************************************/
{

  Widget ModelValButton;
  int    x, y;
  float  x_ndc, y_ndc;
  XEvent Event;
  float  model_value;
  int    i, ret;
  char   stringval[40];
  float  u_value, v_value;

/*---------------------------------------------------------------------*/
  ModelValButton = MenuButtons[8];   /* the button for "Model Value" popup menu */

/*
 * change Model Value button color to red
 */

   XtVaSetValues( ModelValButton, XmNforeground, Red.pixel, NULL);
   XmUpdateDisplay( ModelValButton );

/*
 *  When button is pressed, display the model value label.
 */

   Event.type = 0;
   while (Event.type != ButtonPress) 
      XtAppNextEvent(app, &Event); 

   XtManageChild(ModelValueLabel);


/*  Until button is released, keep updating the value of the label */

   while (Event.type != ButtonRelease) { 
     XtAppNextEvent(app, &Event);

     if (Event.type == MotionNotify) {

/*
 *      Access the mouse button click location
 */

        x = Event.xbutton.x;
        y = Event.xbutton.y;

/*      Convert points from window coordinates to NDC,(0 -> 1) relative to this window */

        x_ndc = (float) x / (float) WindowWidth;
        y_ndc = (float) (WindowWidth - y) / (float) WindowWidth;

/*      Convert to Lat/Lon */

        i = 1;
        NhlNDCToData( Mapid, &x_ndc, &y_ndc, i, &lon, &lat,  NULL, NULL, &ret, &missing);
        if (ret != 0) {
           printf("Mapped to an out-of-range value?? x_ndc, y_ndc %f %f", x_ndc, y_ndc);
       } else {

/*
 *         Get the value from the grid and display. 
 *         Lon from NCAR is pos/neg, so pass "1" to GetModelValue to convert
 */


           if (!Is_Wind) {
              GetModelValue( lon, lat, Grid1, Dims, 1, &model_value );  
              sprintf( stringval, "%s at (%5.2f,%5.2f): %5.1f", Param, lat, lon, model_value);
           } else {
              GetModelValue( lon, lat, Grid1, Dims, 1, &u_value ); 
              GetModelValue( lon, lat, Grid2, Dims, 1, &v_value );
              Vector2SD( u_value, v_value, &winddir, &windspd);
              windspd = windspd/.5148; 
              sprintf( stringval, "wind speed at (%5.2f,%5.2f): %5.1f", lat, lon, windspd);
           }

           XtVaSetValues( ModelValueLabel,   XmNlabelString, XmStringCreateLocalized(stringval), NULL);
           XmUpdateDisplay(ObsWindow);
         }
      }

   };

/* Remove label from screen */

   XtUnmanageChild(ModelValueLabel);

  
/* Restore the button color to inactive_color */

   XtVaSetValues(ModelValButton, XmNforeground, Black.pixel, NULL);

}

/*======================================================================*/

void Vector2SD (float u, float v, float *dir, float *spd)
{
float spdtst = .0000000001;
float dchalf = 180.;
float rtod = 57.2957795;

*spd = sqrt( (u*u) + (v*v) );

   if (*spd < spdtst) {
      *dir = 0.;
   } else {
      *dir = (atan2(u,v) * rtod) + dchalf + .001;
   }
}
