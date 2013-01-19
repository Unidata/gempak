/************************************************************************
 * nwx_gui.h								*
 *									*
 * This header file includes the necessary GEMPAK, X11, Motif and NXM	*
 * header files, defines structures and macros, and declares variables.	*
 * It should be included by any module that contains GUI code.		*
 ** 									*
 * Log:									*
 * S. Jacobs/NMC	 8/94						*
 * C. Lin/EAI	 	 9/95						*
 * D.W.Plummer/NCEP	 9/96	added another element in smethod_t (OBS)*
 * D.W.Plummer/NCEP	11/96	added ddttm to usrslct_t structure	*
 *                              also increased MAX_PLOT_STN to 3800	*
 * D. Kidwell/NCEP	 8/98	Added watch box information		*
 * D. Kidwell/NCEP	 4/99	Deleted watch box cancel flag		*
 * D. Kidwell/NCEP	 5/99	Replaced MAX_PLOT_STN with LLSTFL       *
 * T. Piper/SAIC	 6/02	Removed unused selct_by variable	*
 * T. Piper/SAIC	 7/02	Moved reportSTD from srcb.c to get fname*
 * T. Piper/SAIC	 7/02	Reinstated the select by state option	*
 * T. Piper/SAIC	 8/02	Increased PRNMAX to support state option*
 * R. Tian/SAIC		 4/03	Added textmode_t and modeflg 		*
 * R. Tian/SAIC		11/03	Added autoMenuBtnW	 		*
 * R. Tian/SAIC		12/03	Added stnindex           		*
 * T. Piper/SAIC	07/04	Increased MAX_REPORTS from 300 -> 1000	*
 * E. Safford/SAIC	09/07	split gui.h to nwx_gui.h and nwx_cmn.h  *
 *				  add Nxm wrapper prototypes		*
 ***********************************************************************/
#ifndef NWX_GUI
#define NWX_GUI

#include <Xm/XmAll.h>

#include "proto_nmaplib.h"
#include "proto_xw.h"
#include "Nxm.h"


/*
 *  Global GUI related variables
 */
extern Widget mapCanvW;			/* widget for drawing map */
extern Widget textW;			/* widget for displaying text */
extern Widget dataSelectW;		/* popup window for data selection */
extern Widget prntBtnW;			/* print button in text window */
extern Widget dttmtxtW;			/* date/time info widget in text window */
extern Widget prdgrptxtW;		/* product/group info widget in text window */
extern Widget autoMenuBtnW;		/* auto-update menu button */

/*
 *  GUI related prototypes:
 */
Widget 	dslw_create ( Widget );
Widget	mapw_create ( Widget );
int	mapw_rgstr  ( Widget );
Widget	txtw_create ( Widget );

/*
 *  Nxm wrapper prototypes:
 */
void	wnxm_NxmWarn_show ( 		Widget parent, char *message );
void 	wnxm_NxmCursor_setCursor (	Widget parent, int ref );
void 	wnxm_NxmClose_menuReset (	Widget shell,
					void( *func )( Widget, XtPointer, XtPointer ),
					XtPointer call );
Widget 	wnxm_NxmMenuPulldownBuild (	Widget	parent, WidgetList return_item_w,
					char	*menu_title,
					KeySym	menu_mnemonic,
					_NXMmenuItem	*items );
void	wnxm_NxmHelp_helpBtnCb (	Widget, long, XtPointer );
Widget	wnxm_NxmExit_create (		Widget parent, char *title, char *message,
					void( *ok_cb )(Widget,XtPointer,XtPointer),
					void( *cancel_cb )(Widget,XtPointer,XtPointer));
int	wnxm_NxmGmpkInit (		Widget wid, int	mode, void( *init_func )( int *iret ) );

void	wnxm_NxmGmpkRgstr (		Widget wid, char *name, void( *rgstr_func )( void ) );
void	wnxm_xxflsh (			int *raise, int	*iret );
#endif
