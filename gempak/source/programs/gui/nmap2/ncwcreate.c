#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_mainw.h"	      /* COMPW_NAME */
#include "nmap_data.h"
#include "nmapprm.h"


static Widget 	_compPopW;  	/* computational window popup */
static Widget 	_compwCanvasW;  /* canvas in computational window */

/*
 *  Private functions
 */
void ncw_rgstr 	    ( void );
 
/************************************************************************
 * nmap_compw.c                                                         *
 *                                                                      *
 * This module creates the Computational window and manages its use.	*
 *                                                                      *
 * CONTENTS:                                                            *
 *      ncw_create()   creates the computational window.          	*
 *      ncw_isup()     check if the computational window is managed.    *
 *      ncw_rgstr()    registers the computational window to GEMPAK.    *
 *      ncw_set()      Sets the window to the comp window		*
 *      ncw_unset()    Sets the window back to the main canvas		*
 ***********************************************************************/

/*=====================================================================*/

Widget ncw_create ( Widget parent )
/************************************************************************
 * ncw_create                                              		*
 *                                                                      *
 * This function creates the computational window.    			*
 *                                                                      *
 * Widget ncw_create(parent)                   				*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 * Output parameters:                                                   *
 *  ncw_create	 Widget  computational window widget ID             	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/06	From nmap_mapw.c			*
 ***********************************************************************/
{
    int		wdth, hght;
/*---------------------------------------------------------------------*/
    /*
     * create dialog shell
     */
    _compPopW = XmCreateFormDialog ( parent, "ncw_popup", NULL, 0 );
    XtVaSetValues ( _compPopW, 
		XmNnoResize,			True, 
		NULL );

    /*
     *  Setting the parent shell's XmNmappedWhenManaged value to False
     *  means that the window will not be displayed to the user, even 
     *  when realized and managed.  If you ever want to view it, set
     *  XmNmappedWhenManaged to True.
     */
    XtVaSetValues ( XtParent(_compPopW),
		XmNtitle,			"Computational",
  		XmNmappedWhenManaged,		False, 
                NULL );


    /*
     * create the computational window area 
     */
    mcanvw_getDims ( &wdth, &hght );

    _compwCanvasW = XtVaCreateManagedWidget ( "ncw_canvas",
		xmDrawingAreaWidgetClass,	_compPopW, 
		XmNwidth,			wdth,
		XmNheight,			hght,
		NULL );


    return ( _compPopW );

}

/*=====================================================================*/

void ncw_rgstr ( void )
/************************************************************************
 * ncw_rgstr                                                          	*
 *                                                                      *
 * This function registers the canvas widget of the computational window*
 * as a GEMPAK window.                                                  *
 *                                                                      *
 * void ncw_rgstr()                                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/06	From mapw_rgstr				*
 ***********************************************************************/
{
char wname[10];

/*---------------------------------------------------------------------*/

        strcpy(wname, COMPW_NAME);
        NxmGmpkRgstr(_compwCanvasW, wname, NULL);

}

/*=====================================================================*/

void ncw_set ( void )
/************************************************************************
 * ncw_set                                                            	*
 *                                                                      *
 * This function registers the canvas widget of the computational window*
 * as a GEMPAK window.                                                  *
 *                                                                      *
 * void ncw_rgstr()                                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/06						*
 ***********************************************************************/
{
int		ier, iret;
static int	first=G_TRUE;
char 		wname[10];
Boolean		qcomp;

/*---------------------------------------------------------------------*/

    ctb_pfbool ( "COMP_WINDOW", &qcomp, &ier );

    if ( qcomp )  {

        XtManageChild ( _compPopW );

        if ( first == G_TRUE )  {
	    ncw_rgstr ( );
	    first = G_FALSE;
        }

        strcpy(wname, COMPW_NAME);
        gslwin(wname, &iret, strlen(wname)); 
	if ( iret != 0 )  NxmErr_update();

    }

}

/*=====================================================================*/

void ncw_unset ( void )
/************************************************************************
 * ncw_unset                                                          	*
 *                                                                      *
 * This function registers the canvas widget of the computational window*
 * as a GEMPAK window.                                                  *
 *                                                                      *
 * void ncw_rgstr()                                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/06						*
 ***********************************************************************/
{
int	ier, iret;
char 	wname[10];
Boolean	qcomp;

/*---------------------------------------------------------------------*/

    ctb_pfbool ( "COMP_WINDOW", &qcomp, &ier );

    if ( qcomp )  {

        XtUnmanageChild ( _compPopW );

        strcpy(wname, MCANVW_NAME);
        gslwin(wname, &iret, strlen(wname)); 
	if ( iret != 0 )  NxmErr_update();

    }

}

/*=====================================================================*/

Boolean ncw_isup ( void )
/************************************************************************
 * ncw_isup								*
 *									*
 * Query whether the computational window is managed or not.		*
 *									*
 * Note: the computational window is used for consistant computation.  	*
 *       PGEN actions like refreshing, selecting, etc. should still be 	*
 *       done under the user's projection, not the computational window.*
 *									*
 * Boolean ncw_isup ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	ncw_isup()		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/08	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged (_compPopW) );
}
