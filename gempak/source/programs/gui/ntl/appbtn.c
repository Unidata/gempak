#include "geminc.h"
#include "gemprm.h"
#include "app.h"
#include "proto_xw.h"

void appbtnCallback ( Widget, char *apname, XtPointer );

#ifdef Linux
void appbtnQuitOkCallback ( Widget, XtPointer, XtPointer )
					__attribute__((noreturn));
#else
void appbtnQuitOkCallback ( Widget, XtPointer, XtPointer );
#endif


/************************************************************************
 * appbtn.c                                                             *
 *                                                                      *
 * This module deals with creating application buttons and the callback *
 * for each application.						*
 *                                                                      *
 * CONTENTS:                                                            *
 *	appbtnCreate()	 create application buttons			*
 *	appbtnCallback() callback function for each button  		*
 *	appbtnQuitOkCallback() callback function for EXIT button  	*
 ***********************************************************************/

/*=====================================================================*/

int appbtnCreate ( Widget parent, apptab *ap )
/************************************************************************
 * appbtnCreate		                                                *
 *                                                                      *
 * This function creates the application buttons.            		*
 *                                                                      *
 * int appbtnCreate(parent, ap)                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *	parent	Widget 	parent widget ID				*
 *	*ap	apptab  info from application table 			*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *  appbtnCreate     int      the number of application button created  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi       08/94    Patched to use external table            *
 *                               for application programs               *
 * C. Lin              6/95    reorganize the program and comments      *
 * C. Lin/EAI         12/95    clean up the header                      *
 * C. Lin/EAI         11/96    use Form instead of BulletinBoard        *
 * S. Wang/GSC	      04/97    change Pxm button parameter		*
 * G. Krueger/EAI      9/97    Changed NxmWarning -> NxmWarn_show	*
 * R. Tian/SAIC       01/03    add True flag to NxmBxmBtn_create(Multi) *
 * T. Piper/SAIC        11/03   Replaced NAWIPS_EXE with GEMEXE         *
 * T. Piper/SAIC	01/08	Replaced GEMEXE with OS_BIN		*
 ***********************************************************************/
{ 
Widget          form ;
char            prog_name[MAX_LBUF];
struct stat     statbuf;
char            message[MAX_MESSAGE];

/*---------------------------------------------------------------------*/
/*
 *  Only check for executable file existence if it's not "EXIT".
 */
    if ( strcmp(ap->name, "EXIT") != 0 ) {
        progGetName( prog_name, "OS_BIN", ap->name);
        if (stat(prog_name, &statbuf) != 0) {
            sprintf(message, 
		"Executable %s doesn't exist\n", ap->name);
            NxmWarn_show( parent, message);
            return 0;
        }
        else if ( ! ( S_ISREG( statbuf.st_mode) &&
                      ( statbuf.st_mode & S_IXUSR ) ) ) {

/*
 *  If it's not a regular file or not an executable file.
 */
            sprintf(message, "File %s is not an executable\n", prog_name);
            NxmWarn_show( parent, message);
            return 0;
        }
    } /* end of if ( strcmp(ap->name, "EXIT") != 0 ) */

    form = XtVaCreateManagedWidget( "form",
                            xmFormWidgetClass, parent,
                            NULL);
    ap->button = NxmBxmBtn_create( form, ap->name, NULL, BUTTONW,
                            BUTTONH, ap->fg , ap->bg,
                            NULL, ap->icon, NULL, True,
                            (XtCallbackProc)appbtnCallback, ap->name );

    XtVaSetValues(ap->button, XmNmultiClick, 
			    XmMULTICLICK_DISCARD, NULL);

    ap->label = XtVaCreateManagedWidget( ap->name,
                            xmLabelWidgetClass, form,
			    XmNtopAttachment,   XmATTACH_WIDGET,
			    XmNtopWidget,       ap->button,
			    XmNtopOffset,       0,
			    NULL);
    return 1;
}

/*=====================================================================*/
/* ARGSUSED */
void appbtnCallback ( Widget wdgt, char *apname, XtPointer call )
/************************************************************************
 *									*
 * appbtnCallback							*
 *                                                                      *
 * Callback function for the application buttons.            		*
 *                                                                      *
 * void appbtnCallback(wdgt, apname, call)                              *
 *                                                                      *
 * Input parameters:                                                    *
 *	wdgt	Widget 	  parent widget ID				*
 *	*apname	char      application name 				*
 *	call	XtPointer not used 					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   C. Lin              6/95    reorganize the program and comments    *
 *   C. Lin/EAI         12/95    clean up the header                    *
 *   G. Krueger/EAI	 8/97	 NxmExitDialog->NxmExit_create		*
 *   S. Jacobs/NCEP	12/98	 Removed tabs from WARNING string	*
 *   E. Safford/GSC	02/01	 update param list on NxmExit_create	*
 * T. Piper/SAIC	11/03	Replaced NAWIPS_EXE with GEMEXE		*
 * T. Piper/SAIC	11/07	Changed GEMEXE to OS_BIN		*
 ***********************************************************************/
{
struct stat buf;
char prog_name[MAX_LBUF];

char message[] = {"        WARNING:\n\n\
Exiting NTL will cause all other\n\
NTL-invoked processes to exit...\n\n \
       OK to EXIT?"}; 
/*---------------------------------------------------------------------*/

    if (strcmp(apname, "EXIT") != 0) {
       progGetName( prog_name, "OS_BIN", apname );
       printf(" Invoke ... %s\n", prog_name);

       if ( (stat(prog_name, &buf) == 0) && /* program file exist */
            (S_ISREG( buf.st_mode) &&  /* and it's a regular file */
            (buf.st_mode & S_IXUSR) ) )   /* and it's executable */

              processPID[numOfProcess++] = 
			(int)progInvoke(wdgt, prog_name, apname);
        }
        else 
           NxmExit_create(wdgt, "Exit Confirmation", message, 
			  appbtnQuitOkCallback, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void appbtnQuitOkCallback ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 *									*
 * appbtnQuitOkCallback							*
 *                                                                      *
 * Callback function for exit the application.            		*
 *                                                                      *
 * void appbtnQuitOkCallback(wdgt, clnt, call)                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	wdgt	Widget 	  parent widget ID				*
 *	clnt	XtPointer not used					*
 *	call	XtPointer not used					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   C. Lin              6/95    reorganize the program and comments    *
 *   C. Lin/EAI         12/95    clean up the header                    *
 * T. Piper/SAIC	07/03	replaced gemdisplay with XtDisplay()	*
 ***********************************************************************/
{
    int ii;

/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < numOfProcess; ii++ ) {
        if ( processPID[ii] > -1 ) 
            kill( (long)processPID[ii], SIGKILL );
    }

    xdsclr(XtDisplay(wdgt));

    XtCloseDisplay(XtDisplay(wdgt));
    free( apps_bp );
    exit(0);
}
