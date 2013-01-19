#define PGLOBAL
#include "faxcmn.h"
#include "cpgcmn.h"
#include "xbm.h"


void rinita ( char *pname, float *rxsz, float *rysz, int *ileft,
	 int *ibot, int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * rinita								*
 *									*
 * This subroutine is called to initialize a new device driver.		*
 *									*
 * rinita ( pname, itype, rxsz, rysz, ileft, ibot, iright, itop, 	*
 *					numclr, iret )			*
 *									*
 * Input parameters:							*
 *	*pname		char 		Output file name		*
 *	*rxsz		float		Xsize				*
 *	*rysz		float		Ysize				*
 *									*
 * Output parameters:							*
 *	*ileft		int		Left device coordinate		*
 *	*ibot		int		Bottom device coordinate	*
 *	*iright		int		Right device coordinate		*
 *	*itop		int		Top device coordinate		*
 *	*numclr		int		Max number of colors for device	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 * E. Wehner/EAI	 2/96	Remove hardcoded parameters, added	*
 *				color tables				*
 * S. Jacobs/NCEP	 4/96	Added ileft,ibot,iright,itop,nncolr	*
 *				to calling sequence; added calculation	*
 *				of paper and device size		*
 * E. Wehner/EAi	5/96	Adopted for FAX driver			*
 * E. Wehner/EAi	12/96	Elminated unused parameters		*
 * E. Wehner/Eai	3/97	set map size based on rotation		*
 * M. Linda/GSC		 6/97	Fixed MEMSET and init of ILEFT and IBOT	*
 * M. Linda/GSC		 6/97	Modified for XBM driver			*
 * S. Jacobs/NCEP        6/97   Removed line width variables and rslwid *
 * T. Piper/SAIC        02/04   Removed lenf parameter                  *
 ***********************************************************************/
{
	int	ii, mapsize;
	char	tmpfil[133];
	PrdRec	prec;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *  Remove spaces, and other stuff from the product name.
 */

	for (ii = 0; ii< MAX_NAMESZ;ii++)
	{
	    if (pname[ii] <= ' ') pname[ii] = '\0';
	}

/*
 *  Pull the configuration information from the product table. 
 */
/*  There should be two tokens in the product name.  The first
 *  is the wheel number, the second is the subset number.  Isolate
 *  these two components before requesting the settings. */
        strcpy(prec.pwheel,strtok(pname, ";") );
        strcpy(prec.psubset, strtok(NULL, "; ") );
        
        strcpy ( prec . pfname  ,       "" );
        strcpy ( prec . psubset ,   "0167" );
        strcpy ( prec . ptype   ,    "FAX" );
        strcpy ( prec . pdesc   , "BitMap" );
        strcpy ( prec . pwheel  ,   "999X" );
        strcpy ( prec . rle     ,   "NMC60" );

        prec . xsize    = 32;
        prec . ysize    = 32;
        prec . origx    = 0;
        prec . origy    = 0;
        prec . sszx     = 0.;
        prec . sszy     = 0.;
        prec . rotate   = 0.0;
        prec . bitpix   = 1;
        prec . pagesz   = 0;

/*
 *  Set txszx, txszy (hardware text sizes) here if hardware
 *  text is used.
 */
	txszx  = 11.;
	txszy  = 11.;
	irfont = 1;
	isfont = 0;

/*
 *
 *  landscape = landscape/portrait orientation flag
 *
 *  ileft  = left device coordinate
 *  ibot   = bottom device coordinate
 *  iright = right device coordinate
 *  itop   = top device coordinate
 *
 */

        if (prec.ysize > 1728)
        {  /* flip, no questions asked */
	    landscape = G_TRUE;
	    *ileft  = 0;
	    *ibot   = 0;
	    *iright = prec.ysize;
	    *itop   = prec.xsize;
	    rot = 90.0;
	    prec.rotate = 90.0;
	    num_scans = prec.ysize;
	    bpscan = prec.xsize;
	    *rxsz = (float)prec.ysize;
	    *rysz = (float)prec.xsize;

        }
        else
        {
            if 	((prec.rotate > 89.0) && (prec.rotate < 91.0)) 
	    {
	        landscape = G_TRUE;
	        *ileft  = 0;
	        *ibot   = 0;
	        *iright = prec.ysize;
	        *itop   = prec.xsize;
	        rot = 90.0;
	        prec.rotate = 90.0;
		num_scans = prec.ysize;
		bpscan = prec.xsize;
	        *rxsz = (float)prec.ysize;
	        *rysz = (float)prec.xsize;
 	    
	    }
	    else
	    {
	        landscape = G_FALSE;
	        *ileft  = 0;
	        *ibot   = 0;
	        *iright = prec.ysize;
	        *itop   = prec.xsize;
		bpscan = prec.ysize;
		num_scans = prec.xsize;
	        *rxsz = (float)prec.ysize;
	        *rysz = (float)prec.xsize;
	    }
        }

/*
 *  Allocate space for the pixmap.
 */
	mapsize = (int)((prec.xsize * prec.ysize) * (float)((float)prec.bitpix/8.0));
	pixmap = (char *)calloc(mapsize, sizeof(char));

/*
 *  Set file to initially closed.
 */
	opnfil = G_FALSE;

/*
 *  Save the file name
 */
	strcpy ( tmpfil, pname );

/*
 *  If the new file name is not empty, set the current file name.
 */
	if  ( tmpfil[0] != CHNULL )
	{
            sprintf(filnam, "%s/%s.ras", getenv("FAX_TEMP"), tmpfil);
	    *iret = G_NEWWIN;
	}

/*
 *  Set the number of colors to be returned to DEVCHR.
 *
 *  nncolor (numclr) = number of device colors
 *	( A maximum of MXCLNM = 32 may be initialized. )
 */
	nncolr = 1;
	*numclr = nncolr;

/*
 *      Set up the on and off bit masks.
 *
 *      MSKON is a set of masks to turn a specific bit on.
 *      The masks set a particular bit for ORing with the specified
 *      byte.
 *
 *      MSKOFF is a set of masks to turn of specific bit off.
 *      The masks set all but a particular bit for ANDing with the
 *      specified byte.
 *
 *      MSKONR and MSKOFFR are the same as above, but they have the
 *      bits in the reverse order.
 */
        for ( ii = 0; ii < 8; ii++ )  {
            mskon[ii]  = 1 << (7-ii);
            mskoff[ii] = ~ mskon[ii];
            mskonr[ii]  = 1 << ii;
            mskoffr[ii] = ~ mskonr[ii];
        }
}
