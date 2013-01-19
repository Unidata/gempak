#include "cvgcmn.h"

/*
 *  Private function to load a TaInfo struct from a string.
 */
static void cvg_readTcInfo ( TcInfo *info, char *  ptcinfo, int *iret );

void cvg_rdtce ( FILE *fptr, long fil_pos, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdtce								*
 *									*
 * This function reads a TCE element from the given file. 		*
 *									*
 * cvg_rdtce ( fptr, fil_pos, el, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	fil_pos		long		Position to read from		*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Element structure		*
 *	*iret		int		Return code			*
 *					 -3 = Seek to a bad location	*
 *					 -8 = No file has been opened	*
 *					 -14 = Fail to read elem	*
 **									*
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	04/07	Created					*
 ***********************************************************************/
{
    int		ier, nbytes, nbin, npts, lcol, ltyp, fcol,ftyp,tmp,one=1;
    char	*ptcinfo;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( fptr == NULL ) {
        *iret = -8;
        return;
    }


    /*
     *	Seek to the given position.
     */
    cfl_seek( fptr, fil_pos, 0, &ier );
    if ( ier != 0 ) {
        *iret = -3;
        return;    
    }

    /*
     *	Read the TcInfo string.
     */
    nbytes = (int) ( sizeof(TcInfo) );

    if ( !(ptcinfo = (char *) malloc ( nbytes + 1 ))) return;
    ptcinfo[ nbytes ] = '\0';
    cfl_read ( fptr, nbytes, (void *) ptcinfo, &nbin, &ier); 
    
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    /*
     *	Load the TcInfo string into el->TcInfo.
     */
    cvg_readTcInfo ( &el->elem.tce.info, ptcinfo, iret );
    free ( ptcinfo );

    /*
     *  Read cone line and fill color and type 
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&lcol, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    cfl_read ( fptr, nbytes, (void *)&ltyp, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    cfl_read ( fptr, nbytes, (void *)&fcol, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    cfl_read ( fptr, nbytes, (void *)&ftyp, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    el->elem.tce.cone.lincol = lcol;
    el->elem.tce.cone.lintyp = ltyp;
    el->elem.tce.cone.filcol = fcol;
    el->elem.tce.cone.filtyp = ftyp;

    /*
     *  Read the number of points.
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&npts, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    el->elem.tce.cone.npts = npts;

    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX ) {
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
    }
                                                                                      
    if ( npts < 0 || npts > MAXPTS ) {
        *iret = -14;
        return;
    }

    /*
     *  Read the latlon points.
     */
    nbytes = sizeof(float) * npts * 2;
    cfl_read ( fptr, nbytes, (void *) &el->elem.tce.cone.latlon, &nbin, &ier);

    if ( nbin != nbytes ) {
        *iret = -14;
    }

}

/*=====================================================================*/


void cvg_rdtct ( FILE *fptr, long fil_pos, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdtct								*
 *									*
 * This function reads a TCT element from the given file. 		*
 *									*
 * cvg_rdtct ( fptr, fil_pos, el, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	fil_pos		long		Position to read from		*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Element structure		*
 *	*iret		int		Return code			*
 *					 -3 = Seek to a bad location	*
 *					 -8 = No file has been opened	*
 *					 -14 = Fail to read elem	*
 **									*
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	04/07	Created					*
 * m.gamazaychikov/SAIC	10/08	Added stSrc to TCT element structure	*
 ***********************************************************************/
{
    int		ii, ier, nbytes, nbin, npts, lcol, ltyp, one=1, tmp;
    int		maxnum=15, maxch=100, istnum;
    char	tag_str[ STD_STRLEN ],*buf, *dft = "", **starr;
    char	*ptcinfo, *ptctrack, rpstr[30];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( fptr == NULL ) {
        *iret = -8;
        return;
    }

    /*
     *	Seek to the given position.
     */
    cfl_seek( fptr, fil_pos, 0, &ier );
    if ( ier != 0 ) {
        *iret = -3;
        return;    
    }

    /*
     *	Read the TcInfo string.
     */
    nbytes = (int) ( sizeof(TcInfo) );

    if ( !(ptcinfo = (char *) malloc ( nbytes + 1 ))) return;
    ptcinfo[ nbytes ] = '\0';
    cfl_read ( fptr, nbytes, (void *) ptcinfo, &nbin, &ier); 
    
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    /*
     *	Load the TcInfo string into el->TcInfo.
     */
    cvg_readTcInfo ( &el->elem.tct.info, ptcinfo, iret );
    free ( ptcinfo );
 
    /*
     *  Read track line color and type 
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&lcol, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    cfl_read ( fptr, nbytes, (void *)&ltyp, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    /*
     *  Read the number of points.
     */
    cfl_read ( fptr, nbytes, (void *)&npts, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    el->elem.tct.lincol = lcol;
    el->elem.tct.lintyp = ltyp;
    el->elem.tct.numTrackPts = npts;

    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX ) {
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
    }
    /*
     *  Read the TC Track.
     */
    nbytes = npts*(int)(sizeof(TcTrack));

    if ( !(ptctrack = (char *) malloc ( nbytes + 1 ))) return;
    ptctrack[ nbytes ] = '\0';
    cfl_read ( fptr, nbytes, (void *) ptctrack, &nbin, &ier); 
    
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    /*
     * Allocate memory for array size.
     */
     starr = (char **)malloc(maxnum * sizeof(char *));
     for ( ii = 0; ii < maxnum; ii++ ) {
          starr[ii] = (char *)malloc((maxch+1) * sizeof(char *));
     }

    if ( !( buf = malloc ( strlen( ptctrack ) ) ) ) return;
    if ( !(el->elem.tct.trackPnt = malloc ( npts*sizeof( TcTrack) ))) {
           return;
    }

    /*
     *	Retrieve the tagged fields and load them into el->TcTrack structure
     */
    for ( ii = 0; ii < npts; ii++ ) {
       sprintf ( tag_str, "%s_%d", TCT_TRKPNT_TAG, ii );
       cst_gtag ( tag_str, ptctrack, dft, buf, &ier );
       cst_rpst ( buf, " ","_", buf, &ier );
       cst_rpst ( buf, " ","_", buf, &ier );
       cst_clst (buf, '|', "", maxnum, maxch, starr, &istnum, &ier);
       cst_crnm (starr [0],  &el->elem.tct.trackPnt[ ii ].lat, &ier);
       cst_crnm (starr [1],  &el->elem.tct.trackPnt[ ii ].lon, &ier);
       strcpy( el->elem.tct.trackPnt[ ii ].advDate, starr [2]);
       strcpy( el->elem.tct.trackPnt[ ii ].tau, starr [3]);
       strcpy( el->elem.tct.trackPnt[ ii ].mxWnd, starr [4]);
       strcpy( el->elem.tct.trackPnt[ ii ].wGust, starr [5]);
       strcpy( el->elem.tct.trackPnt[ ii ].mslp, starr [6]);
       cst_rpst ( starr [7], "_"," ", rpstr, &ier );
       strcpy( el->elem.tct.trackPnt[ ii ].tcDv, rpstr);
       strcpy( el->elem.tct.trackPnt[ ii ].tcDvLbl, starr [8]);
       strcpy( el->elem.tct.trackPnt[ ii ].tcDir, starr [9]);
       strcpy( el->elem.tct.trackPnt[ ii ].tcSpd, starr [10]);
       strcpy( el->elem.tct.trackPnt[ ii ].dtLbl, starr [11]);
       cst_rpst ( starr [12], "_"," ", rpstr, &ier );
       strcpy( el->elem.tct.trackPnt[ ii ].stSrc, rpstr);
    }

    /*
     * Free memory.
     */
     for( ii = 0; ii < maxnum; ii++ )
        free( starr[ii] );
                                                                                            
     if ( starr )
        free( (char **) starr );

     free ( ptctrack );
     free (buf);                                                                                       
     *iret = 0;
}
/*=====================================================================*/

void cvg_rdtcb ( FILE *fptr, long fil_pos, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdtcb								*
 *									*
 * This function reads a TCB element from the given file. 		*
 *									*
 * cvg_rdtcb ( fptr, fil_pos, el, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	fil_pos		long		Position to read from		*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Element structure		*
 *	*iret		int		Return code			*
 *					 -3 = Seek to a bad location	*
 *					 -8 = No file has been opened	*
 *					 -14 = Fail to read elem	*
 **									*
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	05/07	Created					*
 ***********************************************************************/
{
    int		ii, ier, nbytes, nbin, npts, lcol, lwid, tcww, nbname;
    int         one = 1, tmp;
    char	*ptcinfo;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( fptr == NULL ) {
        *iret = -8;
        return;
    }


    /*
     *	Seek to the given position.
     */
    cfl_seek( fptr, fil_pos, 0, &ier );
    if ( ier != 0 ) {
        *iret = -3;
        return;    
    }

    /*
     *	Read the TcInfo string.
     */
    nbytes = (int) ( sizeof(TcInfo) );

    if ( !(ptcinfo = (char *) malloc ( nbytes + 1 ))) return;
    ptcinfo[ nbytes ] = '\0';
    cfl_read ( fptr, nbytes, (void *) ptcinfo, &nbin, &ier); 
    
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    /*
     *	Load the TcInfo string into el->TcInfo.
     */
    cvg_readTcInfo ( &el->elem.tcb.info, ptcinfo, iret );
    free ( ptcinfo );

    /*
     *  Read breakpoint line color and width
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&lcol, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    cfl_read ( fptr, nbytes, (void *)&lwid, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    cfl_read ( fptr, nbytes, (void *)&tcww, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    el->elem.tcb.lincol   = lcol;
    el->elem.tcb.linwid   = lwid;
    el->elem.tcb.tcww     = tcww;

    /*
     *  Read the number of points.
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&npts, &nbin, &ier);
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    el->elem.tcb.numBkPts = npts;
    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX ) {
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
    }
    if ( !(el->elem.tcb.bkPntLn = malloc ( npts*sizeof( Breakpt_T ) ))) {
           return;
    }
    nbytes = sizeof(float);
    nbname = MAX_BREAK_PT_STR;
    /*
     *  Read the latlon points.
     */
    for ( ii = 0; ii < npts; ii++ ) {
     cfl_read ( fptr, nbytes, (void *)&el->elem.tcb.bkPntLn[ii].lat, &nbin, &ier );
     cfl_read ( fptr, nbytes, (void *)&el->elem.tcb.bkPntLn[ii].lon, &nbin, &ier );
     cfl_read ( fptr, nbname, (void *)&el->elem.tcb.bkPntLn[ii].breakPtName, &nbin, &ier );
    }

    if ( nbin != nbytes ) {
        *iret = -14;
    }

    
}

/*=====================================================================*/

static void cvg_readTcInfo ( TcInfo *info, char *tc_str, int *iret )
/************************************************************************
 * cvg_readTcInfo							*
 *									*
 * This function loads a TcInfo string into a TcInfo structure.		*
 * 									*
 * cvg_readTcInfo ( info, tc_str, iret )				*
 *									*
 * Input parameters:							*
 *	*info		TcInfo		Pointer to TcInfo struct	*
 *	*tc_str	char	TcInfo 		string				*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:								 	*
 * m.gamazaychikov/SAIC	04/07	Created					*
 ***********************************************************************/
{
    int		ier;
    char	buf[ STD_STRLEN ], *dft = "";
/*---------------------------------------------------------------------*/
    
    /*
     *  Retrieve the tagged field and loads into specified field.
     */    
    cst_gtag ( TCA_STORMNUM_TAG, tc_str, dft, buf, &ier );
    strcpy ( info->stormNum, buf );       
        
    cst_gtag ( TCA_ISSUESTATUS_TAG, tc_str, dft, buf, &ier );
    strcpy ( info->issueStatus, buf );       

    cst_gtag ( TCA_BASIN_TAG, tc_str, dft, buf, &ier );
    strcpy ( info->basin, buf );       
        
    cst_gtag ( TCA_ADVISORYNUM_TAG, tc_str, dft, buf, &ier );
    strcpy ( info->advisoryNum, buf );       
        
    cst_gtag ( TCA_STORMNAME_TAG, tc_str, dft, buf, &ier );
    strcpy( info->stormName, buf ); 
        
    cst_gtag ( TCA_STORMTYPE_TAG, tc_str, dft, buf, &ier );
    strcpy( info->stormType, buf ); 
        
    cst_gtag ( TCA_VALIDTIME_TAG, tc_str, dft, buf, &ier );
    strcpy( info->validTime, buf ); 
        
    cst_gtag ( TCA_TIMEZONE_TAG, tc_str, dft, buf, &ier );
    strcpy( info->timezone, buf ); 

    cst_gtag ( TCA_FCSTPRD_TAG, tc_str, dft, buf, &ier );
    strcpy( info->fcstpd, buf ); 

    *iret = 0;
}
