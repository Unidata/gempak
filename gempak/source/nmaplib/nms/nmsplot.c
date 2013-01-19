#include "nmscmn.h"

void nms_plot ( Pixmap pixmap, int index, char panel[], 
		dttms_t time, dttms_t endtim, int mrange, int match, 
		int minute, int ititl, int *iret )
/************************************************************************
 * nms_plot								*
 *									*
 * This routine plots the data associated with the specified index to 	*
 * the specified pixmap and panel for the given time.			*
 *									*
 * nms_plot ( pixmap, index, panel, time, endtim, mrange, match,	*
 *	      minute, ititl, iret )					*
 *									*
 * Input parameters:							*
 *	pixmap		Pixmap		Pixmap for plotting the data	*
 *	index		int		Index to data info arrays	*
 *	panel[]		char		Panel location for the data	*
 *	time		dttms_t		Date/time for the data		*
 *	endtim		dttms_t		End time of range		*
 *	mrange		int		Minutes in time range		*
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
 * S. Jacobs/NCEP	11/99	Changed arrays of info to structures	*
 * S. Jacobs/NCEP	12/99	Updated data structures			*
 * S. Law/GSC		01/00	added call to xscpxm			*
 * S. Jacobs/NCEP	 3/00	Updated data structures			*
 * S. Law/GSC		06/00	MAXMISC -> MAXTMPLT			*
 * M. Li/SAIC		04/03	Added the second color			*
 * M. Li/SAIC		05/03   fvalu = 0 if the flag is off		*
 * F. J. Yen/NCEP	 6/04	Added ityp for arrw			*
 * H. Zeng/SAIC		06/07	Added ionoff array			*
 ***********************************************************************/
{
    int		ii, ier;
    int		ionoff[MAXPLT], icolr[MAXPLT], 
		icolr2[MAXPLT], iflag[MAXPLT];
    float	fvalu[MAXPLT],
		fline[MAXPLT*3], farrw[MAXPLT*4],
		fsym1[MAXPLT*3], fsym2[MAXPLT*3];
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *	Check for valid index.
     */
    if  ( ( index < 0 || index >= MAXTMPLT ) ||
	  ( mscdt[index].alias[0] == CHNULL ) )  {
	*iret = -1;
	return;
    }

    /*
     *	Set the requested pixmap.
     */
    xscpxm ((int)pixmap, &ier);

    /*
     *	Set the data needed by the display routine to the
     *	global values.
     */
    for ( ii = 0; ii < MAXPLT; ii++ )  {
	if  ( mscdt[index].msctyp[ii].ionoff == 1 )  {
	    ionoff[ii] = 1;
	    icolr[ii]  = mscdt[index].msctyp[ii].icolr;
	    icolr2[ii] = mscdt[index].msctyp[ii].icolr2;	
	    fvalu[ii]  = mscdt[index].msctyp[ii].value;
	}
	else {
	    ionoff[ii] = 0;
	    icolr[ii]  = 0;
	    icolr2[ii] = 0;
	    fvalu[ii]  = 0.0F; 
	}

	iflag[ii] = mscdt[index].mscflg[ii].iflg;

	fline[ii*2+0] = mscdt[index].msctyp[ii].line.size;
	fline[ii*2+1] = (float)mscdt[index].msctyp[ii].line.iwid;

	fsym1[ii*3+0] = mscdt[index].msctyp[ii].symb[0].code;
	fsym1[ii*3+1] = mscdt[index].msctyp[ii].symb[0].size;
	fsym1[ii*3+2] = (float)mscdt[index].msctyp[ii].symb[0].iwid;

	fsym2[ii*3+0] = mscdt[index].msctyp[ii].symb[1].code;
	fsym2[ii*3+1] = mscdt[index].msctyp[ii].symb[1].size;
	fsym2[ii*3+2] = (float)mscdt[index].msctyp[ii].symb[1].iwid;

	farrw[ii*4+0] = mscdt[index].msctyp[ii].arrw.size;
	farrw[ii*4+1] = mscdt[index].msctyp[ii].arrw.hdsz;
	farrw[ii*4+2] = (float)mscdt[index].msctyp[ii].arrw.iwid;
	farrw[ii*4+3] = (float)mscdt[index].msctyp[ii].arrw.ityp;
    }

    /*
     *	Plot the data.
     */
    nms_dspl ( panel, time, mscdt[index].alias,
	       &(mscdt[index].isbcat), mscdt[index].filnam,
	       &(mscdt[index].numtyp), ionoff, icolr, icolr2,
	       &(mscdt[index].numflg), iflag,
	       fvalu, fline, fsym1, fsym2, farrw, &ititl, &ier,
	       strlen ( panel ), strlen ( time ),
	       strlen ( mscdt[index].alias ), 
	       strlen ( mscdt[index].filnam ) );

}
