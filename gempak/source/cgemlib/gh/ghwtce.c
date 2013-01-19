#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "ghcmn.h"

void gh_wtce ( char *filnam,  char *sev, char *vtime, char *stnum,
               char *stname,  char *wbas,  char *advnm, 
               char *fpd, 
               int *ilcl,  int *iltp, int *ifcl, int *iftp, 
               int *icount, float lat[],   float lon[],  
               int *iret)
/************************************************************************
 * gh_wtce                                                              *
 *                                                                      *
 * This function opens a VGF file and writes TCERR element to it.	*
 *                                                                      *
 * gh_wtce ( filnam, sev, vtime, stnum, stname, wbas, advnm, fpd,	*
 *           ilcl, iltp, ifcl, iftp, icount, lat, lon, iret)		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*filnam		char	Name of output VGF file with TCERR elm	*
 *	*sev		char	TC intensity				*
 *	*vtime		char	Advisory valid time			*
 *	*stnum		char	Storm number				*
 *	*stname		char	Storm name				*
 *	*stnum		char	Storm number				*
 *	*wbas		char	Basin 					*
 *	*advnm		char	Advisory number				*
 *	*fpd		char	Forecast period				*
 *	*ilcl		int	Line color				*
 *	*iltp		int	Line type				*
 *	*ifcl		int	Fill color				*
 *	*iftp		int	Fill type				*
 *	*iadvnm		char	Advisory number				*
 *	*icount		int	Number of points in the error cone	*
 *	lat[]		float	Array of error cone latitudes 		*
 *	lon[]		float	Array of error cone longitudes 		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int	Return code                     	*
 *                                  0: normal                      	*
 *                                 -1: VGF file not found		*
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 06/07   Created					*
 ***********************************************************************/
{
    int         	loc, gptyp, ii, ier;
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
    el.hdr.vg_type = (char) TCERR_ELM;
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
    sprintf ( el.elem.tce.info.issueStatus, "%s", "O" );
    sprintf ( el.elem.tce.info.stormNum, "%s", stnum);
    sprintf ( el.elem.tce.info.basin, "%s", wbas);
    sprintf ( el.elem.tce.info.advisoryNum, "%s" ,advnm);
    sprintf ( el.elem.tce.info.stormName, "%s", stname );
    sprintf ( el.elem.tce.info.stormType, "%s", sev );
    sprintf ( el.elem.tce.info.validTime, "%s", vtime );
    sprintf ( el.elem.tce.info.timezone, "%s", "UTC" );
    sprintf ( el.elem.tce.info.fcstpd, "%s", fpd );
    /*
     *  Create Cone Data
     */
    el.elem.tce.cone.lincol = *ilcl;
    el.elem.tce.cone.lintyp = *iltp;
    el.elem.tce.cone.filcol = *ifcl;
    el.elem.tce.cone.filtyp = *iftp;
    el.elem.tce.cone.npts   = *icount;
    for ( ii = 0; ii < *icount; ii++ )  {
        el.elem.tce.cone.latlon[ii]        = lat [ii];
        el.elem.tce.cone.latlon[ii+*icount] =lon [ii];
    }

    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof ( TcInfo)
                        + sizeof (int)*5 + sizeof (float) * (*icount) * 2 );

    cvg_writefD( &el, -1, el.hdr.recsz, filnam, &loc, &ier );

    cfl_clos ( ofptr, &ier );

}
