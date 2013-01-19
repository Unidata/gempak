#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "ctbcmn.h"

#define	VOL_TBL_NAME	"volcano.tbl"
#define	VOL_TBL_TYPE	"stns"

static int     _volNum = 0;
static Boolean _gotVolInfo = FALSE;
static StnLst *_volTbl=NULL;

/*
 *  Private functions
 */
void NxmVolcano_rdInfo ( void );

/************************************************************************
 * NxmVolcano.c                                                         *
 *                                                                      *
 * This module contains all volcano related functions.                  *
 *                                                                      *
 * CONTENTS:                                                            *
 *  NxmVolcano_menuCreate()  creates a volcano list menu      	        *
 *  NxmVolcano_getNum()      gets the total # of volcano stations      	*
 *  NxmVolcano_getInfo()     gets the latlon&name of a volcano      	*
 *  NxmVolcano_getArea()     gets the area info of a volcano		*
 *  NxmVolcano_getSmNm()     gets the smithsonian number of a volcano	*
 *  NxmVolcano_getElev()     gets the elevation of a volcano		*
 *  NxmVolcano_getIdx()      gets the index of a volcano                *
 *  NxmVolcano_rdInfo()      reads info. from volcano.tbl     		*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmVolcano_menuCreate ( Widget parent, Widget textwid,
                               XtCallbackProc push_cb )
/************************************************************************
 * NxmVolcano_menuCreate                                                *
 *                                                                      *
 * This function creates a volcano list menu.                           *
 *                                                                      *
 * Widget NxmVolcano_menuCreate(parent, textwid, push_cb)               *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent        Widget  parent widget ID                              *
 *  textwid       Widget  text field id on the left side                *
 *  (*push_cb)()  void    callback function for push btn                *
 *                                                                      *
 * Output parameters:                                                   *
 *                           NONE                                       *
 * Return parameters:                                                   *
 *       NxmVolcano_create   Widget   The menu bar widget               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA        10/02   initial coding                          *
 * M. Li/SAIC		11/03	Re-arranged cascade button group	*
 ***********************************************************************/
{
    Widget    menub, menu, cascade, push_btn, vol_menu[24];
    int       toff = 5, menu_idx, ii, iret, nn;
    static    Boolean   first = TRUE;
    char      *vol_list[] = {"AA-AM", "AN-AZ", "B", "CA-CH", "CI-CZ", "D-E", 
		             "F", "G", "H", "I-J", "KA-KH", "KI-KZ", "L",
			     "MA-MC", "ME-MZ", "N", "O-P", "Q-R", "SA-SE",
			     "SF-SZ", "TA-TH", "TI-TZ", "U", "V-Z" }; 
    XmString  xmstr;
    Pixel     fg, bg;
    long      btn_idx, ignore;
    char      filename[256];
    static    Pixmap    menu_pxm;
/*---------------------------------------------------------------------*/
/*
 * If it is the first time NxmVolcano_menuCreate() be called, read 
 * volcano info. from volcano.tbl.
 */
    if ( _gotVolInfo == FALSE ) {

         NxmVolcano_rdInfo();
    }

/*
 * Create volcano menu bar and cascade button.
 */
    menub = XmCreateMenuBar (parent, "tmbar", NULL, 0);

    XtVaSetValues (menub,
                   XmNleftAttachment,           XmATTACH_WIDGET,
                   XmNleftWidget,               textwid,
                   XmNleftOffset,               toff,
		   XmNbottomAttachment,      	XmATTACH_FORM,
                   XmNbottomOffset,          	toff,
		   XmNrightAttachment,          XmATTACH_FORM,
                   XmNrightOffset,              toff,
                   XmNmarginHeight,             0,
                   XmNmarginWidth,              0,
                   XmNborderWidth,              0,
                   XmNwidth,                    5,
                   XmNhighlightThickness,       1,
                   XmNshadowThickness,          1,
                   NULL);

    menu  = XmCreatePulldownMenu (menub, "tmmenu", NULL, 0);

    cascade = XtVaCreateManagedWidget ("tmcascade",
                                       xmCascadeButtonWidgetClass, menub,
                                       XmNsubMenuId, menu,
                                       NULL);
    if (first) {
        first = FALSE;

        XtVaGetValues (parent,
                       XmNforeground,   &fg,
                       XmNbackground,   &bg,
                       NULL);

        cfl_inqr ("menu_arrow.xbm", "$NAWIPS/icons/nmap", &ignore, 
                  filename, &iret);

        if (iret == 0) {
            menu_pxm = XmGetPixmap (XtScreen (parent), filename, fg, bg);
        }
        else {
            menu_pxm = XmUNSPECIFIED_PIXMAP;
        }
    }

    if (menu_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP) {
        xmstr = XmStringCreateLocalized ("\\/");

        XtVaSetValues (cascade,
                       XmNlabelString, xmstr,
                       NULL);

        XmStringFree (xmstr);
    }
    else {
        XtVaSetValues (cascade,
                       XmNlabelType,            XmPIXMAP,
                       XmNlabelPixmap,          menu_pxm,
                       XmNmarginHeight,         0,
                       XmNmarginWidth,          0,
                       NULL);
    }

/*
 * Create the first push button.
 */
    btn_idx = 0;
    xmstr = XmStringCreateLocalized ("-Not_listed.__Enter_Name/Location-");
    push_btn = XtVaCreateManagedWidget ("vol_list_button",
                                         xmPushButtonWidgetClass, menu,
                                         XmNlabelString,       xmstr,
                                         XmNuserData,          textwid,
                                         NULL);
    XmStringFree (xmstr);
    XtAddCallback ( push_btn, XmNactivateCallback,
                    (XtCallbackProc)push_cb, (XtPointer)btn_idx );
    btn_idx++;

/*
 * Create cascade button group.
 */
    nn = XtNumber(vol_list);
    for ( ii = 0; ii < nn; ii++ ) {
	xmstr = XmStringCreateLocalized (vol_list[ii]);
	vol_menu[ii] = XmCreatePulldownMenu (menu, "tmmenu", NULL, 0);
    	XtVaCreateManagedWidget ("tmcascade",
                                  xmCascadeButtonWidgetClass, menu,
                                  XmNlabelString,  xmstr,
                                  XmNsubMenuId,    vol_menu[ii],
                                  NULL);
	XmStringFree (xmstr);
    }

/*
 * Create the rest of volcano push buttons.
 */
    for ( ii = 0; ii < _volNum; ii++ ) {

/*
 * Check which cascade group the volcano station belongs to.
 */
        if ( strcasecmp (_volTbl[ii].name, "AN") < 0 ) {
             menu_idx = 0;
        }
        else if ( strcasecmp (_volTbl[ii].name, "B") < 0 ) {
             menu_idx = 1;
        }
        else if ( strcasecmp (_volTbl[ii].name, "C") < 0 ) {
             menu_idx = 2;
        }
        else if ( strcasecmp (_volTbl[ii].name, "CI") < 0 ) {
             menu_idx = 3;
        }
        else if ( strcasecmp (_volTbl[ii].name, "D") < 0 ) {
             menu_idx = 4;
        }
        else if ( strcasecmp (_volTbl[ii].name, "F") < 0 ) {
             menu_idx = 5;
        }
        else if ( strcasecmp (_volTbl[ii].name, "G") < 0 ) {
             menu_idx = 6;
        }
        else if ( strcasecmp (_volTbl[ii].name, "H") < 0 ) {
             menu_idx = 7;
        }
        else if ( strcasecmp (_volTbl[ii].name, "I") < 0 ) {
             menu_idx = 8;
        }
        else if ( strcasecmp (_volTbl[ii].name, "K") < 0 ) {
             menu_idx = 9;
        }
        else if ( strcasecmp (_volTbl[ii].name, "KI") < 0 ) {
             menu_idx = 10;
        }
        else if ( strcasecmp (_volTbl[ii].name, "L") < 0 ) {
             menu_idx = 11;
        }
        else if ( strcasecmp (_volTbl[ii].name, "M") < 0 ) {
             menu_idx = 12;
        }
        else if ( strcasecmp (_volTbl[ii].name, "ME") < 0 ) {
             menu_idx = 13;
        }
        else if ( strcasecmp (_volTbl[ii].name, "N") < 0 ) {
             menu_idx = 14;
        }
        else if ( strcasecmp (_volTbl[ii].name, "O") < 0 ) {
             menu_idx = 15;
        }
        else if ( strcasecmp (_volTbl[ii].name, "Q") < 0 ) {
             menu_idx = 16;
        }
        else if ( strcasecmp (_volTbl[ii].name, "S") < 0 ) {
             menu_idx = 17;
        }
        else if ( strcasecmp (_volTbl[ii].name, "SF") < 0 ) {
             menu_idx = 18;
        }
        else if ( strcasecmp (_volTbl[ii].name, "T") < 0 ) {
             menu_idx = 19;
        }
	else if ( strcasecmp (_volTbl[ii].name, "TI") < 0 ) {
             menu_idx = 20;
        }
        else if ( strcasecmp (_volTbl[ii].name, "U") < 0 ) {
             menu_idx = 21;
        }
        else if ( strcasecmp (_volTbl[ii].name, "V") < 0 ) {
             menu_idx = 22;
        }
        else {
             menu_idx = 23;
        }

        xmstr = XmStringCreateLocalized (_volTbl[ii].name);
        push_btn = XtVaCreateManagedWidget ("vol_list_button",
                                xmPushButtonWidgetClass, vol_menu[menu_idx],
                                XmNlabelString,          xmstr,
                                XmNuserData,             textwid,
                                NULL);
        XmStringFree (xmstr);
        XtAddCallback ( push_btn, XmNactivateCallback,
                        (XtCallbackProc)push_cb, (XtPointer)btn_idx );
        btn_idx++;

    }

    XtManageChild(menub);
    return (menub);

}

/*=====================================================================*/

int NxmVolcano_getNum ( void )
/************************************************************************
 * NxmVolcano_getNum                                                    *
 *                                                                      *
 * This function returns the total # of volcano stations     	        *
 *                                                                      *
 * int NxmVolcano_getNum ()                           	                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *   NxmVolcano_getNum  int    The total # of volcanoes                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA     10/02     initial coding                           *
 ***********************************************************************/
{
    return (_volNum);
}

/*=====================================================================*/

void NxmVolcano_getInfo ( int index, float* lat, float* lon, char* name )
/************************************************************************
 * NxmVolcano_getInfo                                                   *
 *                                                                      *
 * This function returns the latlon and name of a particular volcano.   *
 *                                                                      *
 * void NxmVolcano_getInfo(index, lat, lon, name)                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  index	int            the index of the volcano        	        *
 *                                                                      *
 * Output parameters:                                                   *
 *  *lat	float          latitude  of the volcano                 *
 *  *lon        float          longitude of the volcano                 *
 *  *name       char           name of the volcano                      *
 *                                                                      *
 * Return parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA     10/02     initial coding                           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *lat = _volTbl[index].rlat;    
    *lon = _volTbl[index].rlon;
    strcpy ( name, _volTbl[index].name );
}

/*=====================================================================*/

void NxmVolcano_getArea ( int index, char* area )
/************************************************************************
 * NxmVolcano_getArea                                                   *
 *                                                                      *
 * This function returns the area info of a particular volcano.		*
 *                                                                      *
 * void NxmVolcano_getArea(index, area)					*
 *                                                                      *
 * Input parameters:                                                    *
 *  index	int            the index of the volcano        	        *
 *                                                                      *
 * Output parameters:                                                   *
 *  *area       char           area info of the volcano                 *
 *                                                                      *
 * Return parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA     07/03     initial coding                           *
 * H. Zeng/XTRIA     10/03     used new area info.			*
 ***********************************************************************/
{
    strcpy ( area, _volTbl[index].misc_info );
}

/*=====================================================================*/

void NxmVolcano_getSmNm ( int index, char* smnm )
/************************************************************************
 * NxmVolcano_getSmNm							*
 *                                                                      *
 * This function returns the smithsonian number of a particular volcano.*
 *                                                                      *
 * void NxmVolcano_getSmNm(index, smnm)					*
 *                                                                      *
 * Input parameters:                                                    *
 *  index	int            the index of the volcano        	        *
 *                                                                      *
 * Output parameters:                                                   *
 *  *smnm       char           smithsonian number of the volcano        *
 *                                                                      *
 * Return parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA     07/03     initial coding                           *
 * H. Zeng/XTRIA     10/03     used stid for smithsonian number		*
 ***********************************************************************/
{
    strcpy ( smnm, _volTbl[index].stid );
}

/*=====================================================================*/

void NxmVolcano_getElev ( int index, float* elev )
/************************************************************************
 * NxmVolcano_getElev                                                   *
 *                                                                      *
 * This function returns the elevation of a particular volcano.		*
 *                                                                      *
 * void NxmVolcano_getElev(index, elev)					*
 *                                                                      *
 * Input parameters:                                                    *
 *  index	int            the index of the volcano        	        *
 *                                                                      *
 * Output parameters:                                                   *
 *  *elev	float          elevation of the volcano                 *
 *                                                                      *
 * Return parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA     07/03     initial coding                           *
 ***********************************************************************/
{
    *elev = _volTbl[index].elev;
}

/*=====================================================================*/

int NxmVolcano_getIdx ( char* name )
/************************************************************************
 * NxmVolcano_getIdx                                                    *
 *                                                                      *
 * This function returns the index for a particular volcano name.       *
 *                                                                      *
 * void NxmVolcano_getIdx( name )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  *name	char           name of the volcano        	        *
 *                                                                      *
 * Output parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 * Return parameters:                                                   *
 *  NxmVolcano_getIdx    int   index of the volcano                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA     10/02     initial coding                           *
 ***********************************************************************/
{
    int       ii, index;
/*---------------------------------------------------------------------*/

    index = -1;

    for ( ii = 0; ii < _volNum; ii++ ) {

        if ( strcmp (_volTbl[ii].name, name) == 0 ) {
             index = ii;
             break;
	}
    }
    return (index);
}

/*=====================================================================*/

void NxmVolcano_rdInfo ( void )
/************************************************************************
 * NxmVolcano_rdInfo                                                    *
 *                                                                      *
 * This function reads all volcano info. into a "struct station_list"   *
 * type array from volcano.tbl.                                         *
 *                                                                      *
 * void NxmVolcano_rdInfo()                                             *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA        10/02   Initial coding                          *
 * H. Zeng/XTRIA	10/03   added location field			*
 ***********************************************************************/
{
    int nstn, iret;
/*---------------------------------------------------------------------*/
/*
 *  Read volcano table
 */
    ctb_rstn(VOL_TBL_NAME, VOL_TBL_TYPE, &nstn, &_volTbl, &iret);

    if ( iret != 0 || nstn <= 0 ) {

      printf("CAN NOT ACCESS VOLCANO TABLE OR IT IS EMPTY!!\n");
      exit(1);
    }

    _volNum     = nstn;
    _gotVolInfo = TRUE;
}
