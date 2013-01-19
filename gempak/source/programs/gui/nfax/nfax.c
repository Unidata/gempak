#include "geminc.h"
#include "gemprm.h"
#include "scnfll.h"
#include "cpgcmn.h"
#include "Nxm.h"

/************************************************************************
 * nfax.c								*
 *									*
 * This is the main routine for the NFAX application.  This module	*
 * contains only low level X manipulation fuctions.			*
 *									*
 * CONTENTS:								*
 *   get_depth()	- returns the window depth of X display 	*
 *   setup_gc() 	- Sets up a graphics context			*
 *   draw_expose()	- Exposure callback function for drawing area	*
 *   make_menu()	- Creates the X windows menu			*
 *   make_menu_item()	- Creates one cell on the X windows menu	*
 *   creat_menu()	- Creates all menus				*
 *   set_scroll_sz()	- Sets range of scroll bars			*
 *   main()		-						*
 *									*
 **									*
 * Log: 								*
 *	E. Wehner/EAi	 6/96	Created 				*
 *	E. Wehner/EAi	11/96	Cleanup menu				*
 *	E. Wehner/EAi	 3/97	Added help				*
 *	S. Wang/GSC	 3/97	change help file table name		*
 *	G. Krueger/EAI	10/97	Add NxmVers_showTitle & NxmRes_check;	*
 *				Improve headers 			*
 *	G. Krueger/EAI	11/97	Renamed NxmHelp functions		*
 * T. Piper/GSC		10/99	Fixed compiler warnings, mostly added	*
 *				XtPointer cast in XtAddCallback routines*
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 * J. Wu/GSC		05/01	free XmStrings				*
 * E. Safford/SAIC	10/01	make help window 80 columns wide 	*
 * T. Piper/SAIC	10/01	Changed viewfax to nfax in help files	*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 * T. Piper/SAIC	02/04	Changed name and location of help index	*
 ***********************************************************************/

#define MAXARGS 10   /* Maximum # of arguments in arg lists */
#define NUMBUTTONS 10 /* number of buttons down the left edge */

#define OK 1
#define CANCEL 2

#define RESOURCE_FILE "Pdisp"

void create_menus ( Widget menu_bar );
void draw_expose ( Widget, XtPointer, XtPointer );
Widget make_menu ( char *menu_name, Widget menu_bar, int help_button );
Widget make_menu_item ( char *item_name, XtPointer clnt, Widget menu );
void setup_gc ( void );
void vfx_cmd ( char *fname, char *subset, int pgsz, int idx_dump, int *istat );
void vfx_gui ( int argc, char *argv[] );


XtAppContext context;

XmStringCharSet char_set=XmFONTLIST_DEFAULT_TAG;

Widget toplevel;
Widget menu_bar;
Widget popmen;
Widget cad_form;
Widget file_menu;
Widget edit_menu;
Widget settings_menu;
Widget help_menu;
Widget help_item;
Widget print_item;
Widget quit_item;
Widget sampling_item;
Widget fax_open_item;
Widget clrscrn_item;
Widget howto_item;
Widget draw_area;
Widget draw_frame;   /* frame around the drawing area */
Widget exit_dlg;    /* exit box dialog */
Widget raster_item;
Widget rastop_dlg;
Widget size_prompt_dlg;
Widget wheel_dlg;
Widget list;
Widget vert_scroll;
Widget horiz_scroll;
Widget scroll_win;
Widget busy;
Widget sample_1;
Widget sample_2;
Widget sample_3;
Widget sample_4;
Widget node_box;
Widget node1;
Widget node2;
Widget node3;
Widget node4;
Widget size_box;
Widget size1;
Widget size2;
Widget prod_name;
Widget prod_extract;
Widget prod_quit;
Widget prod_ok;
Widget prod_label;
Widget prod_descr;
Widget prod_descr_label;
Widget status_box;
Widget fontsel_dlg;
Widget fontsel_item;
Widget fontbox_item;
Widget fontsave_dlg;
Widget fontsave_item;

XmFontList fontlist;

GC gc;	    /* graphics context declaration */
GC gc_or;   /* graphics context for OR pixels */
GC gc_not;
GC font_gc;  /* graphics context for fonts processing */
GC bit_gc;   /* graphics context for bitplane */
GC dump_gc;   /* graphics context for dumping pixel-bitplane */

char *fonts[] = {
    "courier-medium-r-normal--",
    "helvetica-medium-r-normal--",
    "times-medium-r-normal--",
    "courier-medium-o-normal--",
    "helvetica-medium-o-normal--",
    "times-medium-i-normal--",
    "courier-bold-r-normal--",
    "helvetica-bold-r-normal--",
    "times-bold-r-normal--",
    "courier-bold-o-normal--",
    "helvetica-bold-o-normal--",
    "times-bold-i-normal--" };

char * font_name;

int active_fnt_sz = 0;
int active_node = 0;
int printing = 0;	/* whether we are printing or not */

Dimension sh;	/* string height */

ConvertRec crec[MAX_CUTS];

char raster_fname[120];
int raster_up;
int samp_factor;
int xsize, ysize;
Cardinal bitmapX;
Cardinal bitmapY;
int xorg, yorg;

PrdRec prec;	/* standard product generation record */

Pixmap pmap;


/* ----------------------------------
function: get_depth()
Description: Gets the depth of the display containing the specified
widget....
---------------------------------------- */
unsigned int get_depth ( Widget w )
{
     Window r;
     int x, y;
     unsigned int wd, ht, bw, depth;
     XGetGeometry(XtDisplay(w), RootWindowOfScreen(XtScreen(toplevel)),
		      &r, &x, &y, &wd, &ht, &bw, &depth);
     return depth;
}

/* -------------------------------------
function: setup_gc ()
Description:  Sets up the graphics context for the drawing area
---------------------------------------- */
void setup_gc ( void )
{
     Pixel fg;	/* foreground and background */
     Pixel bg;
     XGCValues vals;
     Arg al[MAXARGS];
     XmFontListEntry entry;
     XFontStruct *font = NULL;
     char *namestring = NULL;

     Cardinal ii = 0;

/* load a font */
     namestring = "*times*-24-*";
     font = XLoadQueryFont(XtDisplay(draw_area), namestring);
     entry = XmFontListEntryCreate("tag1", XmFONT_IS_FONT, (XtPointer)font);
     fontlist = XmFontListAppendEntry(NULL, entry);
     XmFontListEntryFree(&entry);
     XmFontListFree(fontlist);

/* set up foreground and background colors */
     XtSetArg(al[ii], XmNforeground, &fg); ii++;
     XtSetArg(al[ii], XmNbackground, &bg); ii++;
     XtGetValues(draw_area, al, ii);

     vals.foreground = fg;
     vals.background = bg;
     vals.font=font->fid;
     gc=XtGetGC(draw_area, GCForeground | GCBackground, &vals);


/* set up the gc for OR functions.... */
     vals.foreground = fg | bg;
     vals.function = GXor;
     vals.font = font->fid;
     gc_or=XtGetGC(draw_area, GCForeground | GCBackground | GCFunction, &vals);

/* set up the off pixel graphics context */
     vals.foreground = bg;
     vals.background = bg;
     gc_not = XtGetGC(draw_area, GCForeground | GCBackground, &vals);
}


/*=====================================================================*/
/* ARGSUSED */
void draw_expose ( Widget w, XtPointer clnt, XtPointer call )
/* --------------------------------------
function: draw_expose
Description:  This is a callback that is activated on expose in
the drawing area......
---------------------------------------- */
{
    Arg al[10];
    Cardinal ac;
/*---------------------------------------------------------------------*/

    ac	= 0;
    XtSetArg(al[ac], XmNheight, &bitmapY); ac++;
    XtGetValues(toplevel, al, ac);

    ac	= 0;
    XtSetArg(al[ac], XmNwidth, &bitmapX); ac++;
    XtGetValues(toplevel, al, ac);


    if (raster_up)
    {
	vshowmap( bitmapX, bitmapY, xorg, yorg, gc, &pmap, draw_area);

    }
}

/*=====================================================================*/

Widget make_menu ( char *menu_name, Widget menu_bar, int help_button )
{
    Widget menu;
    Widget cascade;
    Cardinal i = 0;
    Arg al[MAXARGS];
    XmString xmstr;
/*---------------------------------------------------------------------*/
        
    menu = XmCreatePulldownMenu(menu_bar, menu_name, NULL, 0);

    XtSetArg(al[i], XmNsubMenuId, menu); i++;
    xmstr = XmStringCreateLtoR(menu_name, XmFONTLIST_DEFAULT_TAG);
    XtSetArg(al[i], XmNlabelString, xmstr); 
    i++;
    
    cascade = XmCreateCascadeButton(menu_bar, menu_name, al, i);
    XtManageChild(cascade);

/* if this is the help button, hardwire it to the RH edge of menubar */
    if (help_button)
    {
	i = 0;
	XtSetArg(al[i], XmNmenuHelpWidget, cascade);i++;
	XtSetValues(menu_bar, al, i);
    }
    XmStringFree( xmstr );
    return menu;
}

/*=====================================================================*/

Widget make_menu_item ( char *item_name, XtPointer clnt, Widget menu )
{
    Widget item;
    Cardinal i = 0;
    Arg al[MAXARGS];
    XmString xmstr;
/*---------------------------------------------------------------------*/
    
    xmstr = XmStringCreateLtoR(item_name, char_set);
    XtSetArg(al[i], XmNlabelString, xmstr); 
    i++;
    
    item = XmCreatePushButton(menu, item_name, al, i);
    XtManageChild(item);
    XtAddCallback(item, XmNactivateCallback, 
			(XtCallbackProc)vmenuCB, clnt);
    XtSetSensitive(item, TRUE);

    XmStringFree( xmstr );

    return item;
}

/*=====================================================================*/

void create_menus ( Widget menu_bar )
/************************************************************************
 * Function: create_menus
 *
 * Creates menu widgets by request.
 *
 * NOTE:  The font creation menu choices are removed preventing the
 * activation of dialog boxes related to font creation.  To activate 
 * those menu choices uncomment those options and recompile.
 ***********************************************************************/
{
/* this menu bar has file, edit utilities and help */
/* FILE */
    file_menu = make_menu("File", menu_bar, 0);
    fax_open_item = make_menu_item("Load Product", "Fax_Open", file_menu);
    print_item = make_menu_item("Print Product", "Print", file_menu);
    quit_item = make_menu_item("Quit", "Quit", file_menu);
/*  fontsave_item = make_menu_item("Save font", "Savefont", file_menu); */


/* EDIT */
    edit_menu = make_menu("Edit", menu_bar, 0);
    clrscrn_item = make_menu_item("Clear Screen", "ClearScrn", edit_menu);
/*     fontsel_item = make_menu_item("Load Font", "Fontsel", edit_menu); */

/* SETTINGS */
    settings_menu = make_menu("Settings", menu_bar, 0);
    sampling_item = make_menu("Sampling", settings_menu, 0);

/* sub sampling sub menu */
    sample_1 = make_menu_item("Sample factor 1:1", "sample1", sampling_item);
    sample_2 = make_menu_item("Sample factor 1:2", "sample2", sampling_item);
    sample_3 = make_menu_item("Sample factor 1:3", "sample3", sampling_item);
    sample_4 = make_menu_item("Sample factor 1:4", "sample4", sampling_item);
}

/*=====================================================================*/

void set_scroll_sz ( Cardinal x, Cardinal y )
{
    Arg a1[10];
    Cardinal ac;

/*---------------------------------------------------------------------*/
/* reset scroll bars */
    ac = 0;
    XtSetArg(a1[ac], XmNminimum, 0); ac++;
    XtSetArg(a1[ac], XmNmaximum, x); ac++;
    XtSetValues(horiz_scroll, a1, ac);

    ac = 0;
    XtSetArg(a1[ac], XmNminimum, 0); ac++;
    XtSetArg(a1[ac], XmNmaximum, y); ac++;
    XtSetValues(vert_scroll, a1, ac);
}

/*============================================================================*/

int main ( int argc, char *argv[] )
{
    int i;
    int gui;		/* whether to use a gui interface */
    int cmd;		/* whether to execute the command interface */
    int idx;		/* whether to dump the index */
    int istat;
    char subset[8];
    char fname[120];
    int  pg_sz = 11;   /* default page size is 8.5 X 11.0 */

/*---------------------------------------------------------------------*/

    i =  0;
    gui  = FALSE;
    idx = FALSE;
    cmd  = TRUE;
    if (argc == 1) gui = TRUE;

    while (i<argc)
    {
	if ( (strcmp("-s",argv[i]) == 0))  /* looking for subset number */
	{

	  strncpy(subset, argv[i+1], (sizeof(subset)-1));
	    i++;
	}
	else
	{
	  if ( (strcmp("-f",argv[i]) == 0))  /* looking for file name */
	  {

	    strncpy(fname, argv[i+1], (sizeof(fname)-1));
	    i++;
	  }
	  else
	  {
	      if ( (strcmp("-p", argv[i]) == 0)) /* page size */
	      {
		 pg_sz = atoi(argv[i+1]);
		 i++;
	      }
	      else
	      {
		  if ( (strcmp("-gui", argv[i]) == 0) ) /* GUI interface */
		  {
		      gui = TRUE;
		  }
		  else
		  {
		      if ( (strcmp("-h", argv[i]) == 0) ) /* help message */
		      {
			printf("Usage:	 nfax [-h] [-s subset -f fname] \n");
			printf("		 [-p size] [-gui] [-idump] \n");
			printf(" Where: -h = Print this help message \n");
			printf("	-s = Subset of fax cut to extract \n");
			printf("	-f = File name containing subset \n");
			printf("	-p = Page size ( > 11 = 11x17 ) \n");
			printf("	-gui = Bring up in GUI mode \n");
			printf("	-idump = Dump index of fax cuts \n");
			cmd = FALSE;
		      }
		      else
		      {
			if ( (strcmp("-idump", argv[i]) == 0) ) /* indx dmp */
			{
			    idx = TRUE;
			}
		      }
		  }
	      }
	  }
	}
	i++;
    }

    if (gui)
    {
	vfx_gui(argc, argv);
    }
    else
    {
	if (cmd)
	{
	    vfx_cmd(fname, subset, pg_sz, idx,	&istat);
	}

    }
    return(0);
}

/*===========================================================================*/

void vfx_cmd ( char *fname, char *subset, int pgsz, int idx_dump, int *istat )
{
    char  descr[120];
    char  raster_fname[120];
    char  new_name[120];
    char  prt_name[40];
    int ixlen;
    int iylen;
    int bpp;
/*---------------------------------------------------------------------*/

    if (idx_dump)
    {
	printf("\n\nThe following cuts are present in %s \n", fname);
	cpg_shoct( fname, descr, istat);

    }
    else
    {
	cpg_sixrd(fname, subset, descr, &ixlen, &iylen, &bpp, istat);

	printf("Extracting %s \n", descr);

	sprintf(raster_fname, "%s/%s.ras", getenv("FAX_TEMP"), subset);

	pg_print( raster_fname, ixlen, iylen, samp_factor, prt_name, pgsz,
		new_name, istat);

	printf("Postscript output in %s \n", new_name);
    }
}

/*=============================================================================*/

void vfx_gui ( int argc, char *argv[] )
{

    Arg a1[10];
    Cardinal ac;
    XmString xmstr;
/*---------------------------------------------------------------------*/
    
    prec.pagesz = 11;  /* default page size is 8.5 x 11 */
    raster_up = 0;   /* set to say raster is not up */
    samp_factor = 1;  /* set initial sampling factor to 1:1 */

/* create the top level shell */
    toplevel = XtAppInitialize( &context, RESOURCE_FILE, NULL, 0, &argc, argv,
		   NULL, NULL, 0);

/*
 * check resource file
 */
    NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL);

/*
 * display version in title string
 */
    NxmVers_showTitle(toplevel);

    ac = 0;
    cad_form  = XmCreateForm(toplevel, "cad_form", a1,ac);
    XtManageChild(cad_form);

/* create the menubar first */
    ac = 0;
    menu_bar = XmCreateMenuBar(cad_form, "menu_bar", a1, ac);
    XtManageChild(menu_bar);

/* attach the menubar to the form */
    ac=0;
    XtSetArg(a1[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(a1[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(a1[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetValues(menu_bar, a1, ac);

/* create the "raster file open" dialog box... */
    ac = 0;
    rastop_dlg = XmCreateFileSelectionDialog(cad_form,
	"rastop_dlg", a1,ac);
    XtAddCallback(rastop_dlg, XmNokCallback, (XtCallbackProc)vraster_openCB, (XtPointer)OK);
    XtAddCallback(rastop_dlg, XmNcancelCallback, (XtCallbackProc)vraster_openCB, (XtPointer)CANCEL);
    XtUnmanageChild(XmSelectionBoxGetChild(rastop_dlg,
		XmDIALOG_HELP_BUTTON));

/* create the wheel selection dialog box */
    ac = 0;
    XtSetArg(a1[ac], XmNautoUnmanage, False); ac++;
    wheel_dlg = XmCreateFormDialog(cad_form, "wheel_dlg", a1, ac);
    XtUnmanageChild(wheel_dlg);

/* create the option menu button on the dialog */
    node_box = XmCreateRadioBox(wheel_dlg, "node_box", NULL, 0);
    XtManageChild(node_box);

    node1 = XmCreateToggleButton(node_box, "node1", NULL,0);
    XtManageChild(node1);
    XtAddCallback(node1, XmNvalueChangedCallback, (XtCallbackProc)vnodeCB, (XtPointer)1);
    node2 = XmCreateToggleButton(node_box, "node2", NULL,0);
    XtManageChild(node2);
    XtAddCallback(node2, XmNvalueChangedCallback, (XtCallbackProc)vnodeCB, (XtPointer)2);
    node3 = XmCreateToggleButton(node_box, "node3", NULL,0);
    XtManageChild(node3);
    XtAddCallback(node3, XmNvalueChangedCallback, (XtCallbackProc)vnodeCB, (XtPointer)3);
    node4 = XmCreateToggleButton(node_box, "node4", NULL,0);
    XtManageChild(node4);
    XtAddCallback(node4, XmNvalueChangedCallback, (XtCallbackProc)vnodeCB, (XtPointer)4);
    prod_name = XmCreateText(wheel_dlg, "prod_name", NULL, 0);
    XtManageChild(prod_name);

    size_box = XmCreateRadioBox(wheel_dlg, "size_box", NULL, 0);
    XtUnmanageChild(size_box);

    size1 = XmCreateToggleButton(size_box, "size1", NULL, 0);
    XtUnmanageChild(size1);
    XtAddCallback(size1, XmNvalueChangedCallback, (XtCallbackProc)vsizeCB, (XtPointer)11);
    size2 = XmCreateToggleButton(size_box, "size2", NULL, 0);
    XtAddCallback(size2, XmNvalueChangedCallback, (XtCallbackProc)vsizeCB, (XtPointer)17);
    XtUnmanageChild(size2);


    prod_extract = XmCreatePushButton(wheel_dlg, "prod_extract", NULL, 0);
    XtManageChild(prod_extract);
    XtAddCallback(prod_extract, XmNactivateCallback, (XtCallbackProc)vwheelCB, (XtPointer)1);

    prod_ok = XmCreatePushButton(wheel_dlg, "prod_ok", NULL, 0);
    XtManageChild(prod_ok);
    XtAddCallback(prod_ok, XmNactivateCallback, (XtCallbackProc)vwheelCB, (XtPointer)2);


    prod_quit = XmCreatePushButton(wheel_dlg, "prod_quit", NULL, 0);
    XtManageChild(prod_quit);
    XtAddCallback(prod_quit, XmNactivateCallback, (XtCallbackProc)vwheelCB, (XtPointer)3);

    prod_label = XmCreateLabel(wheel_dlg, "prod_label", NULL, 0);
    XtManageChild(prod_label);

    prod_descr_label = XmCreateLabel(wheel_dlg, "prod_descr_label", NULL, 0);
    XtUnmanageChild(prod_descr_label);

    prod_descr = XmCreateText(wheel_dlg, "prod_descr", NULL, 0);
    XtUnmanageChild(prod_descr);

    status_box = XmCreateText(wheel_dlg, "status_box", NULL, 0);
    XtManageChild(status_box);

    ac = 0;
    XtSetArg(a1[ac], XmNscrollingPolicy, XmAPPLICATION_DEFINED); ac++;
    XtSetArg(a1[ac], XmNscrollBarDisplayPolicy, XmSTATIC); ac++;
    XtSetArg(a1[ac], XmNscrollBarPlacement, XmBOTTOM_RIGHT); ac++;
    scroll_win = XmCreateScrolledWindow(cad_form, "scroll_win", a1, ac);
    XtManageChild(scroll_win);

    XtSetArg(a1[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(a1[ac], XmNtopWidget, menu_bar); ac++;
    XtSetArg(a1[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(a1[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(a1[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetValues(scroll_win, a1, ac);

/* create scroll bars */
    ac = 0;
    XtSetArg(a1[ac], XmNminimum, 0); ac++;
    XtSetArg(a1[ac], XmNmaximum, MAX_X); ac++;
    XtSetArg(a1[ac], XmNorientation, XmHORIZONTAL); ac++;
    horiz_scroll = XmCreateScrollBar(scroll_win, "horiz_scroll", a1, ac);
    XtManageChild(horiz_scroll);
    XtAddCallback(horiz_scroll, XmNvalueChangedCallback, (XtCallbackProc)vhoriz_scrollCB, 
					(XtPointer)1);


    ac = 0;
    XtSetArg(a1[ac], XmNminimum, 0); ac++;
    XtSetArg(a1[ac], XmNmaximum, MAX_Y); ac++;
    vert_scroll = XmCreateScrollBar(scroll_win, "vert_scroll", a1, ac);
    XtManageChild(vert_scroll);
    XtAddCallback(vert_scroll, XmNvalueChangedCallback, (XtCallbackProc)vvert_scrollCB, 
					(XtPointer)1);


/* create a drawing area */
    ac = 0;

    draw_area = XmCreateDrawingArea(scroll_win, "draw_area", a1, ac);
    XtManageChild(draw_area);
    XtAddCallback(draw_area, XmNexposeCallback, draw_expose, NULL);


    XmScrolledWindowSetAreas(scroll_win, horiz_scroll, vert_scroll, draw_area);

/* create a popup menu for convenience */
    ac = 0;
    popmen = XmCreatePopupMenu(draw_area, "popmen", a1, ac);
    create_menus(popmen);

/* quit dialog */
    ac = 0;
    xmstr = XmStringCreateLtoR("Really want to QUIT?", char_set);
    XtSetArg(a1[ac], XmNmessageString, xmstr);
    ac++;
    
    exit_dlg = (Widget)XmCreateQuestionDialog(cad_form, "exit_dlg", a1,ac);
    
    XmStringFree( xmstr );
    XtAddCallback(exit_dlg, XmNokCallback, (XtCallbackProc)vexitCB, (XtPointer)OK);
    XtAddCallback(exit_dlg, XmNcancelCallback, (XtCallbackProc)vexitCB, (XtPointer)CANCEL);

/* working dialog */
    ac = 0;
    xmstr = XmStringCreateLtoR("Loading...", char_set);
    XtSetArg(a1[ac], XmNmessageString, xmstr);
    ac++;
    
    busy = XmCreateWorkingDialog(cad_form, "busy", a1, ac);
    
    XmStringFree( xmstr );
    XtUnmanageChild(XmMessageBoxGetChild(busy, XmDIALOG_HELP_BUTTON));

/* create the "save font" dialog box... */
    ac = 0;
    fontsave_dlg = XmCreateFileSelectionDialog(cad_form,
	"fontsave_dlg", a1,ac);
    XtAddCallback(fontsave_dlg, XmNokCallback, (XtCallbackProc)fontsave_cb, (XtPointer)OK);
    XtAddCallback(fontsave_dlg, XmNcancelCallback, (XtCallbackProc)fontsave_cb, (XtPointer)CANCEL);
    XtUnmanageChild(XmSelectionBoxGetChild(fontsave_dlg,
		XmDIALOG_HELP_BUTTON));

/* create the size prompt dialog used in creating fonts */
    ac = 0;
    xmstr = XmStringCreateLtoR("Enter size of font to create:", char_set);
    XtSetArg(a1[ac], XmNselectionLabelString, xmstr); 
    ac++;
    
    size_prompt_dlg = XmCreatePromptDialog(cad_form, "size_prompt_dlg",
			a1, ac);
    
    XmStringFree( xmstr );
    XtAddCallback(size_prompt_dlg, XmNokCallback, (XtCallbackProc)fontsz_CB, (XtPointer)OK);
    XtAddCallback(size_prompt_dlg, XmNcancelCallback, (XtCallbackProc)fontsz_CB, (XtPointer)CANCEL);
    XtUnmanageChild(XmSelectionBoxGetChild(size_prompt_dlg,
			XmDIALOG_HELP_BUTTON));

/* create the font selection dialog box.... */
    ac = 0;
    XtSetArg(a1[ac], XmNmustMatch, TRUE); ac++;
    xmstr = XmStringCreateLtoR("Pick a font to create", char_set);    
    XtSetArg(a1[ac], XmNselectionLabelString, xmstr); 
    ac++;
    
    fontsel_dlg = XmCreateSelectionDialog(cad_form, "fontsel_dlg",a1,ac);
    
    XmStringFree( xmstr );
    XtAddCallback(fontsel_dlg, XmNokCallback, (XtCallbackProc)fontsel_dlg_CB, (XtPointer)OK);
    XtAddCallback(fontsel_dlg, XmNcancelCallback, (XtCallbackProc)fontsel_dlg_CB, (XtPointer)CANCEL);
    XtUnmanageChild(XmSelectionBoxGetChild(fontsel_dlg, XmDIALOG_HELP_BUTTON));

/* create the selection box widget */
    ac = 0;
    XtSetArg(a1[ac],XmNmustMatch,False); ac++;
    xmstr = XmStringCreateLtoR("Pick a font.", char_set);    
    XtSetArg(a1[ac],XmNselectionLabelString, xmstr);	
    ac++;
    
    fontsel_dlg=XmCreateSelectionDialog(cad_form,"fontsel_dlg",a1,ac);
    
    XmStringFree( xmstr );    
    XtAddCallback(fontsel_dlg, XmNokCallback, (XtCallbackProc)fontsel_dlg_CB, (XtPointer)OK);
    XtAddCallback(fontsel_dlg, XmNcancelCallback, (XtCallbackProc)fontsel_dlg_CB, (XtPointer)CANCEL);
    XtUnmanageChild(XmSelectionBoxGetChild(fontsel_dlg, XmDIALOG_HELP_BUTTON));

    XtSetSensitive(node3, False);
    XtSetSensitive(node4, False);

/* set up the graphics context */
    setup_gc();

    xorg = 0;
    yorg = 0;

    bitmapX = 600;
    bitmapY = 600;

/* create the menubarstuff */
    create_menus(menu_bar);

    NxmHelp_create(cad_form, "HelpDialog", "Help",
			"$GEMHLP/hlp/nfaxIndex.hlp", 20, 80);

/*
 * create help button
 */
    help_item = XmCreateCascadeButton(menu_bar, "Help", NULL, 0);
    XtVaSetValues(menu_bar, XmNmenuHelpWidget, help_item, NULL);
    XtAddCallback(help_item, XmNactivateCallback, 
			(XtCallbackProc)NxmHelp_helpBtnCb, (XtPointer)1);
    XtManageChild(help_item);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(context);
}
