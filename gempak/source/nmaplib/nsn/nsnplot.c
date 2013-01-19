#include "nsncmn.h"
#include "Nxm.h"

void nsn_plot ( Pixmap pixmap, int index, char panel[], 
		dttms_t time, dttms_t endtim, int mrange, int intrvl,
		int match, int minute, int ititl, int ibfr, int mbfr,
		int iaftr, int maftr, int mstrct, int *iret ) 
/************************************************************************
 * nsn_plot								*
 *									*
 * This routine plots the data associated with the specified index to 	*
 * the specified pixmap and panel for the given time.			*
 *									*
 * nsn_plot ( pixmap, index, panel, time, endtim, mrange, intrvl, match,*
 *	      minute, ititl, ibfr, mbfr, iaftr, maftr, mstrct, iret )	*
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
 * S. Law/GSC		06/00	MAXSND -> MAXTMPLT			*
 * T. Lee/SAIC		08/03	Added time interval to calling sequence	*
 * T. Lee/SAIC		10/04	Added bin hours to calling sequence	*
 * F. J. Yen/NCEp	04/08	Added bin mins & most recent flag to CS	*
 ***********************************************************************/
{
    int		ier, ierr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *	Check for valid index.
     */
    if  ( ( index < 0 || index >= MAXTMPLT ) ||
	  ( snddt[index].alias[0] == CHNULL ) )  {
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
    nsn_dspl ( panel, time, snddt[index].alias,
	       &(snddt[index].isbcat),
	       snddt[index].cycle, snddt[index].parms,
	       snddt[index].color, snddt[index].level,
	       snddt[index].vcord, snddt[index].filter,
	       snddt[index].txtatt, endtim, &mrange, &intrvl,
	       &match, &minute, &ititl, &ibfr, &mbfr, &iaftr,
	       &maftr, &mstrct, &ier,
	       strlen ( panel ), strlen ( time ),
	       strlen ( snddt[index].alias ), 
	       strlen ( snddt[index].cycle ), 
	       strlen ( snddt[index].parms ),
	       strlen ( snddt[index].color ),
	       strlen ( snddt[index].level ),
	       strlen ( snddt[index].vcord ),
	       strlen ( snddt[index].filter ),
	       strlen ( snddt[index].txtatt ),
	       strlen ( endtim ) );
    if ( ier == 2 ) {
        er_wmsg ( "NSN", &ier, NULL, &ierr, 3, 0 );
        NxmErr_update( );
    }
}
