#include "ngdcmn.h"

void ngd_plot ( Pixmap pixmap, int index, char panel[], dattm_t time, 
		dattm_t endtim, int mrange, int intrvl, int match, 
		int minute, int ititl, int *iret )
/************************************************************************
 * ngd_plot								*
 *									*
 * This routine plots the data associated with the specified index to 	*
 * the specified pixmap and panel for the given time.			*
 *									*
 * ngd_plot ( pixmap, index, panel, time, endtim, mrange, intrvl,	*
 *	      match, minute, ititl, iret )				*
 *									*
 * Input parameters:							*
 *	pixmap		Pixmap		Pixmap for plotting the data	*
 *	index		int		Index to data info arrays	*
 *	panel[]		char		Panel location for the data	*
 *	time		dattm_t		Date/time for the data		*
 *	endtim		dattm_t		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	match		int		Flag for time match scheme	*
 *	minute		int		Number of min diff for match	*
 *	ititl		int		Title line			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		01/00	added call to xscpxm			*
 * S. Law/GSC		06/00	MAXGRID -> MAXTMPLT			*
 * S. Jacobs/NCEP	 2/02	Return any errors from ngd_dspl		*
 * T. Lee/SAIC		08/03	Added time interval to calling sequence	*
 ***********************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *	Check for valid index.
     */
    if  ( ( index < 0 || index >= MAXTMPLT ) ||
	  ( grddt[index].alias[0] == CHNULL ) )  {
	*iret = -1;
	return;
    }

    /*
     *	Set the requested pixmap.
     */
    xscpxm (pixmap, &ier);

    /*
     *	Plot the data.
     */
    ngd_dspl ( panel, time, grddt[index].alias,
	       &(grddt[index].isbcat),
	       grddt[index].cycle, grddt[index].rstfil,
	       endtim, &mrange, &intrvl, &match, &minute, &ititl, iret,
	       strlen ( panel ), strlen ( time ),
	       strlen ( grddt[index].alias ), 
	       strlen ( grddt[index].cycle ), 
	       strlen ( grddt[index].rstfil ),
	       strlen ( endtim ) );
}
