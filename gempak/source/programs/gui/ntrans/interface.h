/******************************************************************************
 * interface.h: Header file for metafile interface  which                     *
 *              are part of the NMC Metafile Translator System                *
 *                            (motif/X-windows)                               *
 *****************************************************************************/

#include "defs.h"
#include "cgmcmn.h"
#include "proto_ntrans.h"

/*
 *  Restore/Save data settings 
 */
#define RESTORE_NPF     0               /* Restore settings from an NPF */
#define SAVE_NPF        1               /* Save settings to an NPF */

typedef struct  {
	Dimension	current_width;
	Dimension	current_height;
        Dimension       old_width;
        Dimension       old_height;
        int             current_pixmap;
        int             pixmap_no;
        int             old_pixmap_no;
}       Pixmaps_str;

/*
 * GLOBAL WIDGET & WINDOW VARIABLES
 */

#ifdef  CREATE 
int		load_flag;
Display  	*display;
Meta_str        meta_st;
nc_file_header  meta_head;
Graphics_str	GraphicsData;
Group_str       GroupList[MAX_NO_GROUPS];
Pixmaps_str     PixmapData;
prt_multiPanel_t  prt_multiPanel_info[MAXPANEL][MAXPANEL];

Widget	toplevel;
Widget	toplevel_form;
Widget	menubar_form;
Widget  legend_frame;
Widget	DrawingW;
Widget	menu_b[5];
Widget  file_select_toplevel;
Widget  group_panel_toplevel;
Widget  group_select_toplevel;
Widget  dwell_panel;
Widget  loopcrt_top;

int             FrameNo;
int             COLRMODE;
int             ViewFrame;
int             SelectGroupNo;
Boolean		OpenModel;
int             GroupNo = 0;
int             GroupLoadFlag = 0;
char            MetaFile[256];
Pixel    	pixels[MAXCOLORS];
FILE		*logptr;
#else
extern int		load_flag;
extern Display		*display;
extern Meta_str		meta_st;
extern nc_file_header	meta_head;
extern Graphics_str	GraphicsData;
extern Group_str	GroupList[];
extern Pixmaps_str	PixmapData;
extern prt_multiPanel_t	prt_multiPanel_info[MAXPANEL][MAXPANEL];

extern Widget	toplevel;
extern Widget	toplevel_form;
extern Widget	menubar_form;
extern Widget   legend_frame;
extern Widget   DrawingW;
extern Widget	menu_b[];
extern Widget   file_select_toplevel;
extern Widget   group_panel_toplevel;
extern Widget   group_select_toplevel;
extern Widget   dwell_panel;
extern Widget   loopcrt_top;

extern int      FrameNo;
extern int      COLRMODE; /* COLRMODE = 0 --- black and white */
                          /* COLRMODE = 1 --- color printer */
extern int      ViewFrame;
extern int      SelectGroupNo;
extern Boolean	OpenModel;
extern int      GroupNo;
extern int      GroupLoadFlag;
extern char     MetaFile[];
extern Pixel    pixels[];
extern FILE	*logptr;
#endif
