#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

void NxmVers_showTitle ( Widget topshell )
/************************************************************************
 * NxmVers_showTitle                                                    *
 *                                                                      *
 * This function displays version information in the application title  *
 * string.                         					*
 *                                                                      *
 * NxmVers_showTitle ( topshell )			                *
 *                                                                      *
 * Input parameters:                                                    *
 *  topshell          Widget     ID of toplevel shell widget.           *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE                               		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      03/97  						*
 * I. Durham/GSC   05/98  Changed underscore decl. to an include	*
 * A. Hardy/GSC    04/99  Removed NAWIPS; added cst_lcuc for def_title  *
 * T. Piper/SAIC	02/04	Renamed file from NxmVers.c		*
 ***********************************************************************/
{
char 	*def_title;
char  	vers[128], title[256], uc_title[256];
int  	iret;

/*---------------------------------------------------------------------*/
/*
 * add version into the title string
 */
        XtVaGetValues(topshell, XmNtitle, &def_title, NULL);
        cst_lcuc ( def_title, uc_title, &iret );
        ss_vers(vers, &iret, sizeof(vers));
        sprintf(title, "%s ( %s )", uc_title, vers);
        XtVaSetValues(topshell, XmNtitle, title, NULL);
}
