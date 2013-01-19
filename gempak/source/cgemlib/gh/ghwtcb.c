#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "ghcmn.h"

void gh_wtcb ( char *filnam,  char *sev, char *vtime, char *stnum,
               char *stname,  char *wbas,  char *advnm, 
               char *fpd, 
               int *ilcl,  int *iltp, int *itcww,
               int *icount, float lat[],   float lon[],  
               int *iret)
/************************************************************************
 * gh_wtcb                                                              *
 *                                                                      *
 * This function opens a VGF file containing breakpoint information,	*
 * reads in the TCA structure, and returns elements of the structure.	*
 *                                                                      *
 * gh_wtcb ( filnam, istnum, iyear, ibasin, iadvnm, isttyp, stname,	*
 *           vtime, icount, isev, ityp, igeog, lat, lon, bpstr, numbkp,	*
 *           tzone, status, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*filnam		char	Name of VGF file with breakpoint info	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*istnum		int	Storm number				*
 *	*iyear		int	Year (YYYY)				*
 *	*ibasin		int	Basin 					*
 *				    0: Eastern_Pacific 			*
 *				    1: Central_Pacific 			*
 *				    2: Western_Pacific 			*
 *				    3: Atlantic				*
 *	*iadvnm		char	Advisory number				*
 *	*isttyp		int	Storm type				*
 *				    0: Hurricane 			*
 *				    1: Tropical Storm 			*
 *				    2: Tropical Depression		* 
 *				    3: Subtropical Storm 		*
 *				    4: Subtropical Depression		*
 *	*stname		char	Storm name				*
 *	*vtime		char	Valid time				*
 *	*icount		int	Number of watches/warning		*
 *	isev[]		int	Array of severity:			*
 *				    0: Tropical Storm 			*
 *				    1: Hurricane 			*
 *	ityp[]		int	Array of advisoty type			*
 *				    0: Watch 				*
 *				    1: Warning				*
 *	igeog[]		int	Array of special geography		*
 *				    0: None 				*
 *				    1: Islands 				*
 *				    2: Water				*
 *	lat[]		float	Array of breakpoint latitudes 		*
 *	lon[]		float	Array of breakpoint longitudes 		*
 *	*bpstr		char	String with breakpoint names		*
 *	numbkp[]	int	Array of number of breakpoints		*
 *	*tzone		char	Time zone				*
 *	*status		char	Issue status				*
 *      *iret           int	Return code                     	*
 *                                  0: normal                      	*
 *                                 -1: VGF file not found		*
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 06/07   Created					*
 ***********************************************************************/
{
    int         	loc, gptyp, ii, ier, npts;
    char		ofname[128]; 
    long		ifilesize;

    VG_DBStruct		el;
    FILE            	*ofptr;

/*---------------------------------------------------------------------*/
    *iret = 0;

    cfl_inqr (  filnam, NULL, &ifilesize, ofname, &ier );
 
    if ( ier != 0 ) {
       cvg_crvgf ( filnam, &ier );
       ofptr = (FILE *) cfl_ropn (filnam, "",&ier);
    }
    else {
       ofptr = (FILE *) cfl_ropn (filnam, "", &ier);
    }

    /*
     *  Create header
     */
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type = (char) TCBKL_ELM;
    el.hdr.filled  = 0;
    el.hdr.closed  = 0;
    el.hdr.delete  = 0;
    el.hdr.version = 0;
    el.hdr.maj_col = 2;
    el.hdr.min_col = 2;
    gptyp = 7;
    el.hdr.grptyp = (char) gptyp;
    el.hdr.grpnum = 10;

    /*
     *  Create TcInfo
     */
    sprintf ( el.elem.tcb.info.issueStatus, "%s", "O" );
    sprintf ( el.elem.tcb.info.stormNum, "%s", stnum);
    sprintf ( el.elem.tcb.info.basin, "%s", wbas);
    sprintf ( el.elem.tcb.info.advisoryNum, "%s" ,advnm);
    sprintf ( el.elem.tcb.info.stormName, "%s", stname );
    sprintf ( el.elem.tcb.info.stormType, "%s", sev );
    sprintf ( el.elem.tcb.info.validTime, "%s", vtime );
    sprintf ( el.elem.tcb.info.timezone, "%s", "UTC" );
    sprintf ( el.elem.tcb.info.fcstpd, "%s", fpd );

    /*
     *  Create TcWW Data
     */
    el.elem.tcb.lincol = *ilcl;
    el.elem.tcb.linwid = *iltp;
    el.elem.tcb.tcww = *itcww;
    el.elem.tcb.numBkPts   = *icount;
    npts = *icount;

    /*
     * Allocate memory for array size.
     */
    if ( !(el.elem.tcb.bkPntLn = malloc ( el.elem.tcb.numBkPts
                                  *sizeof( Breakpt_T) ))) {
           return;
    }

    for ( ii = 0; ii < *icount; ii++ )  {
        el.elem.tcb.bkPntLn[ii].lat = lat [ii];
        el.elem.tcb.bkPntLn[ii].lon = lon [ii];
        sprintf (el.elem.tcb.bkPntLn[ii].breakPtName,"9999");
    }


    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof ( TcInfo)
                        + sizeof (int)*4 + npts*sizeof (Breakpt_T )) ;

    cvg_writefD( &el, -1, el.hdr.recsz, filnam, &loc, &ier );

    cfl_clos ( ofptr, &ier );

}
