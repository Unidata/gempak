#include "nsfcmn.h"
#include "Nxm.h"

void nsf_plot ( Pixmap pixmap, int index, char panel[], 
		dttms_t time, dttms_t endtim, int mrange, int intrvl,
		int match, int minute, int ititl, int ibfr, int mbfr,
		int iaftr, int maftr, int mstrct, int *iret )
/************************************************************************
 * nsf_plot								*
 *									*
 * This routine plots the data associated with the specified index to 	*
 * the specified pixmap and panel for the given time.			*
 *									*
 * nsf_plot ( pixmap, index, panel, time, endtim, mrange, intrvl, match,*
 *	      minute, ibfr, mbfr, iaftr, maftr, mstrct, ititl, iret )	*
 *									*
 * Input parameters:							*
 *	pixmap		Pixmap		Pixmap for plotting the data	*
 *	index		int		Index to data info arrays	*
 *	panel[]		char		Panel location for the data	*
 *	time		dttms_t		Date/time for the data		*
 *	endtim		dttms_t		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	match		int		Flag for time match scheme	*
 *	minute		int		Number of min diff for match	*
 *	ibfr		int		Bin hours before current time	*
 *	mbfr		int		Bin minutes before current time	*
 *	iaftr		int		Bin hours after current time	*
 *	maftr		int		Bin minutes after current time	*
 *	mstrct		int		Most recent only flag		*
 *	ititl		int		Title line			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		01/00	added call to xscpxm			*
 * S. Law/GSC		06/00	MAXSFC -> MAXTMPLT			*
 * T. Lee/SAIC		08/03	Added time interval to calling sequence	*
 * T. Lee/SAIC		10/04	Added bin hours to calling sequence	*
 * F. J. Yen/NCEP	04/08	Added bin minutes & most recent flag-CSC*
 ***********************************************************************/
{
    int		ier, ierr;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *	Check for valid index.
     */
    if  ( ( index < 0 || index >= MAXTMPLT ) ||
	  ( sfcdt[index].alias[0] == CHNULL ) )  {
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
    nsf_dspl ( panel, time, sfcdt[index].alias,
	       &(sfcdt[index].isbcat),
	       sfcdt[index].cycle, sfcdt[index].parms,
	       sfcdt[index].color, sfcdt[index].filter,
	       sfcdt[index].txtatt, endtim, &mrange, &intrvl,
	       &match, &minute, &ititl, &ibfr, &mbfr,
               &iaftr, &maftr, &mstrct, &ier,
	       strlen ( panel ), strlen ( time ),
	       strlen ( sfcdt[index].alias ), 
	       strlen ( sfcdt[index].cycle ), 
	       strlen ( sfcdt[index].parms ),
	       strlen ( sfcdt[index].color ),
	       strlen ( sfcdt[index].filter ),
	       strlen ( sfcdt[index].txtatt ),
	       strlen ( endtim ) );
    if ( ier == 2 ) {
	er_wmsg ( "NSF", &ier, NULL, &ierr, 3, 0 ); 
	NxmErr_update( );
    }
}
