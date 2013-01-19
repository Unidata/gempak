#include "geminc.h"
#include "cpgcmn.h"


#define OK 1
#define CANCEL 2

void show_text ( int pitch );
void setup_font_gc ( char *font_name );
void create_font ( char *fname, int pitch );


extern XmStringCharSet char_set;

extern Widget toplevel;
extern Widget draw_area;
extern Widget fontsel_dlg;
extern Widget fontsel_item;
extern Widget fontbox_item;
extern Widget fontsave_dlg;
extern GC font_gc;  /* graphics context for fonts processing */
extern GC bit_gc;   /* graphics context for bitplane */
extern GC dump_gc;   /* graphics context for dumping pixel-bitplane */
extern int active_fnt_sz;


extern char *fonts[];


extern Pixmap pmap;

XmFontList fontlist2;    /* font list for drawing area string */


/************************************************************************
 * VFONTS								*
 *									*
 * This file contains functions related to creating bitmap fonts for	*
 * NAWIPS raster based products.					*
 *									*
 * The following functions are included in this file:			*
 * show_text	-	displays the text strings for a font in an	*
 *			X-windows screen				*
 * setup_font_gc  - 	Sets up a graphics context containing the font	*
 *			that is to be displayed				*
 * fontsel_dlg_cb - 	Callback function for font selection dialog box	*
 * create_font	- 	Handles file storage of created font		*
 * fontsave_cb	-	Callback function for font saving function	*
 * fontsz_CB 	-	Callback function for font size selection 	*
 *			dialog box.					*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	7/96	Created					*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/


void show_text ( int pitch )
/************************************************************************
 * show_text								*
 * 									*
 * Using the requested pitch, this function displays the printable 	*
 * characters for the font established in the font_gc on an X-window	*
 * drawable.								*
 *									*
 * show_text ( pitch )							*
 * 									*
 * Input parameters:							*
 *  pitch	int	point size of the font				*
 * 									*
 **									*
 * Log:									*
 * T. Piper/SAIC	03/05	Replaced XCreateGC with XtGetGC		*
 ***********************************************************************/
{

    int xpos;
    int ypos;
    Cardinal ii;
    XGCValues vals;
    Arg al[10];
    Pixel fg;
    Pixel bg;
    char text_str[126];
    
    xpos = 100;
    ypos = 100;

/*---------------------------------------------------------------------*/
/* set up foreground and background colors */
    ii = 0;
    XtSetArg(al[ii], XmNforeground, &fg); ii++;
    XtSetArg(al[ii], XmNbackground, &bg); ii++;
    XtGetValues(draw_area, al, ii);

    vals.foreground = fg;
    vals.background = bg;

    vals.function  = GXcopy;
    vals.plane_mask = 0x01;
    

/* set up the graphics contexts */
    dump_gc = XtGetGC(draw_area,
               GCForeground |  GCBackground | GCFunction | GCPlaneMask,
                                  &vals );

    pmap = XCreatePixmap(XtDisplay(draw_area), 
		RootWindowOfScreen(XtScreen(toplevel)),
		1000, 1000, get_depth(draw_area));

    XClearArea(XtDisplay(draw_area), XtWindow(draw_area), 0, 0, 0, 0, 
                             False);
    XCopyArea(XtDisplay(draw_area), XtWindow(draw_area), pmap,
		dump_gc, 0,0, 1000, 1000, 0, 0);   

    for (ii=32;ii<126;ii++)
    {
      text_str[0] = (char)ii;
      text_str[1] = '\0';
      XDrawString(XtDisplay(draw_area), pmap, font_gc, 
				xpos, ypos,text_str,
                                 (int)strlen(text_str));
      xpos+= pitch;

      if ((ii == 62) || (ii==92) )
      {
          ypos += 50;
	  xpos = 100;
      }
    }

    XCopyArea(XtDisplay(draw_area), pmap,XtWindow(draw_area),
		dump_gc, 0,0, 1000, 1000, 0, 0);   
}

/*=====================================================================*/

void setup_font_gc ( char *font_name )
/************************************************************************
 * setup_font_gc							*
 * 									*
 * Sets up a graphics context containing the passed in font		*
 * 									*
 * setup_font_gc ( font_name )						*
 *									*
 * Input parameters:							*
 *	font_name	char *	name of font to establish in the GC	*
 * 									*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	03/05	Added GCFont to XtGetGC; removed 	*
 * 				XSetFont; replaced XmFontListCreate	*
 * 				with XmFontListEntryCreate		*
 ***********************************************************************/
{
     Pixel fg;    /* foreground and background */
     Pixel bg;
     XGCValues vals;
     Arg al[10];
     XmFontListEntry entry;
     XFontStruct *font;

     Cardinal ii = 0;

/*---------------------------------------------------------------------*/
/* load a font */
     font = XLoadQueryFont(XtDisplay(draw_area), font_name);
     entry = XmFontListEntryCreate("tag2", XmFONT_IS_FONT, (XtPointer)font);
     fontlist2 = XmFontListAppendEntry(NULL, entry);
     XmFontListEntryFree(&entry);
     XmFontListFree(fontlist2);

/* set up foreground and background colors */
     XtSetArg(al[ii], XmNforeground, &fg); ii++;
     XtSetArg(al[ii], XmNbackground, &bg); ii++;

     XtGetValues(draw_area, al, ii);

     vals.foreground = fg;
     vals.background = bg;
     vals.font=font->fid;
     font_gc=XtGetGC(draw_area, GCForeground | GCBackground | GCFont, &vals);

}

/*======================================================================*/

void fontsel_dlg_CB ( Widget w, long clnt, 
			XmSelectionBoxCallbackStruct *call )
/************************************************************************
 * fontsel_dlg_CB							*
 * 									*
 * Callback function for font selection dialog box (the font list)	*
 * 									*
 * fontsel_dlg_CB ( w, clnt, call )					*	
 *									*
 * Input parameters:							*
 * w		Widget		Widget that caused the callback		*
 * clnt		long		related information			*
 * *call	XmSelectionBoxCallbackStruct	Calling information	*
 * 									*
 **									*
 ***********************************************************************/
{
    char *s;

/*---------------------------------------------------------------------*/

    switch (clnt)
    {
       case OK:
         XmStringGetLtoR(call->value, char_set, &s);
         setup_font_gc(s);

         printf("load prompt-> %s \n", s);
         show_text(active_fnt_sz);

         XtFree(s);
         break;
       case CANCEL:
        
         break;
    }
    XtUnmanageChild(w);
}

/*======================================================================*/

void create_font ( char *fname, int pitch )
/************************************************************************
 * create_font								*
 *  Creates a bitmap file containing the picture of each character in 	*
 * 									*
 * create_font ( fname, pitch )						*
 * 									*
 * Input parameters:							*
 *	fname	char *		Name of font being created		*
 *	pitch	int		Size of font being created		*
 * 									*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	02/05	Replaced XCreateGC with XtGetGC		*
 ***********************************************************************/
{

    Pixmap char_bitmap;
    XGCValues vals;
    Arg al[10];
    FILE *rfp;
    FILE *wfp;
    FILE *pfp;
    Cardinal i;
    Pixel fg;
    Pixel bg;
    int xpos = 99;
    int ypos = 101;

    int istat;

    int hot_x = -1;
    int hot_y = -1;

    char temp_name[120];
    char buff[120];
    char pipedin[120];

/*---------------------------------------------------------------------*/

    ypos -= pitch;

/* set up foreground and background colors */
    i = 0;
    XtSetArg(al[i], XmNforeground, &fg); i++;
    XtSetArg(al[i], XmNbackground, &bg); i++;

    XtGetValues(draw_area, al, i);

    vals.foreground = fg;
    vals.background = bg;

    vals.function  = GXcopy;
    vals.plane_mask = 0x01;
    
/* set up the graphics contexts */
    dump_gc = XtGetGC(draw_area, GCForeground |  GCBackground | 
			GCFunction | GCPlaneMask, &vals );


    bit_gc = XtGetGC(draw_area,
               GCForeground |  GCBackground | GCFunction | GCPlaneMask,
                                  &vals );


    char_bitmap = XCreatePixmap(XtDisplay(draw_area), XtWindow(draw_area),
		(Cardinal)pitch, (Cardinal)pitch, get_depth(draw_area));

/* for each character in the font */
    for (i=32;i<126;i++) 
    {
      XCopyPlane(XtDisplay(draw_area), pmap, char_bitmap, 
		bit_gc, xpos, ypos, (Cardinal)pitch, (Cardinal)pitch, 0, 0, 0x01);

      XCopyPlane(XtDisplay(draw_area), char_bitmap,XtWindow(draw_area),
		bit_gc, 0,0, (Cardinal)pitch, (Cardinal)pitch, xpos, ypos+500, 0x01);   

/* create a file name to store to */
      sprintf(temp_name, "%s%i", fname, i);
      istat = XWriteBitmapFile( XtDisplay(draw_area), temp_name, char_bitmap,
				(Cardinal)pitch, (Cardinal)pitch, hot_x, hot_y);

      
      if (istat != BitmapSuccess)
      {
        printf("Unable to store bitmap in %s: Error %i \n", temp_name, istat);
      }
      xpos+= pitch;

      if ((i == 62) || (i==92) )
      {
          ypos += 50;
	  xpos = 99;
      }

    }

/* merge the individual files to one big bitmap structure */
/* create a file name to store to */
    sprintf(temp_name, "%s.fnt", fname);

    wfp = fopen(temp_name, "w");
    if (wfp)
    {
/* write out all the new data */
        for (i=32;i<126;i++)
        {

/* create a file name to read from */
            sprintf(temp_name, "%s%i", fname, i);
            rfp = fopen(temp_name, "r");
            if (rfp)
            { 
                while ((fgets(buff, sizeof(buff), rfp)) != NULL)
                    fputs(buff, wfp); 

                fclose(rfp);
            }

/* delete all of the temporary bitmap files */
            sprintf(buff, "rm %s", temp_name);
            if ( (pfp = popen(buff, "r")) != NULL)
            {
              fgets(pipedin, sizeof(pipedin), pfp);
              pclose(pfp);
            }
        }
        fclose(wfp);
    }
}

/*======================================================================*/

void fontsave_cb ( Widget w, long clnt, 
				XmSelectionBoxCallbackStruct *call )
/************************************************************************
 * fontsave_cb								*
 * 									*
 * This is the callback function for the font saving dialog box		*
 * 									*
 * fontsave_cb ( w, clnt, call )					*
 *									*
 * Input parameters:							*
 * w		Widget		Widget that caused the callback		*
 * clnt		long		related information			*
 * *call	XmSelectionBoxCallbackStruct	Calling information	*
 * 									*
 *									*
 **									*
 ***********************************************************************/
{
    char *s;
    switch (clnt)
    {
       case OK:
         XmStringGetLtoR(call->value, char_set, &s);

         printf("save to file -> %s \n", s);
         create_font(s, active_fnt_sz);

         XtFree(s);
         break;
       case CANCEL:
        
         break;
    }
    XtUnmanageChild(w);
}

/*======================================================================*/

void fontsz_CB ( Widget w, long clnt, 
				XmSelectionBoxCallbackStruct *call )
/************************************************************************
 * fontsz_CB								*
 *	This is the callback function for the font size dialog box.	*
 * 									*
 * fontsz_CB ( w, clnt, call )						*
 * 									*
 * Input parameters:							*
 * w		Widget		Widget that caused the callback		*
 * clnt		long		related information			*
 * *call	XmSelectionBoxCallbackStruct	Calling information	*
 * 									*
 *									*
 **									*
 ***********************************************************************/
{
    char szfonts[50][50];
    Widget list;
    int list_cnt;
    XmString s;
    char *xst;
    switch (clnt)
    {
       case OK:
          XmStringGetLtoR(call->value, char_set, &xst);

/* Add items to selection box list. */
          list=XmSelectionBoxGetChild(fontsel_dlg, XmDIALOG_LIST);
          XmListDeleteAllItems(list);
          for (list_cnt=0; list_cnt<12; list_cnt++)
          {
              sprintf(szfonts[list_cnt], "*%s%s*", fonts[list_cnt], xst);
	      active_fnt_sz = atoi(xst);


              s=XmStringCreate(szfonts[list_cnt],char_set);
              XmListAddItem(list,s,0);
              XmStringFree(s);
          }

          XtFree(xst);

          XtManageChild(fontsel_dlg);
         break;
       case CANCEL:
        
         break;
    }
    XtUnmanageChild(w);
}
