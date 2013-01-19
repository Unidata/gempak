#include "cvgcmn.h"

#define ONEBLOCK	2048	

/*
 *  Private function 
 */
static int cvg_catTcStr ( char * str, char **ptcinfo, int *size );

void cvg_writtce ( FILE *fptr, VG_DBStruct *el, int *iret )
/**************************************************************************
 * cvg_writtce                                                            *
 *                                                                        *
 * This function writes a TCErr element to file at the current location.  *
 *                                                                        *
 * cvg_writtce ( fptr, el, iret )                                         *
 *                                                                        *
 * Input Parameters:                                                      *
 *	*fptr 		FILE		File pointer                      *
 *	*el		VG_DBStruct	Pointer to a TCE element          *
 *                                                                        *
 * Output Parameters:                                                     *
 *                                                                        *
 *	*iret		int		Return code                       *
 *                                      0:	Normal                    *
 *                                      -6: 	No file has been opened   *
 *                                      -17:	Failed to load TcInfo	  *
 **                                                                       *
 * Log:                                                                   *
 * m.gamazaychikov/SAIC	04/07		Created                           *
 *************************************************************************/
{
   int		ier, nbytes, nbhdr, npts, tmp, one=1;
   char		*ptcinfo;
/*-----------------------------------------------------------------------*/  

   *iret = 0;

   if ( fptr == NULL ) {
      *iret = -6;
      return;
   }

   /*
    *  Write out element header.
    */
   nbhdr = sizeof(VG_HdrStruct);
   cfl_writ( fptr, nbhdr,  (unsigned char *)&el->hdr, &ier);

   /*
    *  Load the TcInfo into a string
    */
   nbytes = cvg_writTcInfo ( el->elem.tce.info, &ptcinfo, &ier);

   if ( ier < 0 ) {
      *iret = -17;
      return;
   }

   nbytes = sizeof (TcInfo);
   cfl_writ( fptr, nbytes, (unsigned char *)ptcinfo, &ier);
   free( ptcinfo );

   /*
    *  Write out the cone of uncertainty data.
    */
   nbytes = sizeof(int);
   npts = el->elem.tce.cone.npts;
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tce.cone.lincol, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tce.cone.lintyp, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tce.cone.filcol, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tce.cone.filtyp, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&npts, &ier);
   /*
    *  Swap the number of points for some platforms before
    *  using them to write out lat/lons.
    */
    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX )   {
                                                                                      
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
   }
   nbytes = sizeof(float) * npts * 2;
   cfl_writ ( fptr, nbytes, (unsigned char *)&el->elem.tce.cone.latlon, &ier );

}

/*=======================================================================*/

void cvg_writtcb ( FILE *fptr, VG_DBStruct *el, int *iret )
/**************************************************************************
 * cvg_writtcb                                                            *
 *                                                                        *
 * This function writes a TCBkl element to file at the current location.  *
 *                                                                        *
 * cvg_writtcb ( fptr, el, iret )                                         *
 *                                                                        *
 * Input Parameters:                                                      *
 *	*fptr 		FILE		File pointer                      *
 *	*el		VG_DBStruct	Pointer to a TCE element          *
 *                                                                        *
 * Output Parameters:                                                     *
 *                                                                        *
 *	*iret		int		Return code                       *
 *                                      0:	Normal                    *
 *                                      -6: 	No file has been opened   *
 *                                      -17:	Failed to load TcInfo	  *
 **                                                                       *
 * Log:                                                                   *
 * m.gamazaychikov/SAIC	05/07		Created                           *
 *************************************************************************/
{
   int		ii, ier, nbytes, nbhdr, npts, nbname, tmp, one=1;
   char		*ptcinfo;
/*-----------------------------------------------------------------------*/  

   *iret = 0;

   if ( fptr == NULL ) {
      *iret = -6;
      return;
   }

   /*
    *  Write out element header.
    */
   nbhdr = sizeof(VG_HdrStruct);
   cfl_writ( fptr, nbhdr,  (unsigned char *)&el->hdr, &ier);

   /*
    *  Load the TcInfo into a string
    */
   nbytes = cvg_writTcInfo ( el->elem.tcb.info, &ptcinfo, &ier);

   if ( ier < 0 ) {
      *iret = -17;
      return;
   }

   nbytes = sizeof (TcInfo);
   cfl_writ( fptr, nbytes, (unsigned char *)ptcinfo, &ier);
   free( ptcinfo );

   /*
    *  Write out the bk line data.
    */
   nbytes = sizeof(int);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tcb.lincol, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tcb.linwid, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tcb.tcww, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tcb.numBkPts, &ier);
   nbytes = sizeof(float);
   nbname = MAX_BREAK_PT_STR;
   npts = el->elem.tcb.numBkPts;
   /*
    *  Swap the number of points for some platforms before
    *  using them to write out lat/lons.
    */
    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX )   {
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
   }
   for ( ii = 0; ii < npts; ii++ ) {
     cfl_writ ( fptr, nbytes, (unsigned char *)&el->elem.tcb.bkPntLn[ii].lat, &ier );
     cfl_writ ( fptr, nbytes, (unsigned char *)&el->elem.tcb.bkPntLn[ii].lon, &ier );
     cfl_writ ( fptr, nbname, (unsigned char *)&el->elem.tcb.bkPntLn[ii].breakPtName, &ier );
   }
}

/*=======================================================================*/

void cvg_writtct ( FILE *fptr, VG_DBStruct *el, int *iret )
/**************************************************************************
 * cvg_writtct                                                            *
 *                                                                        *
 * This function writes a TCTrk element to file at the current location.  *
 *                                                                        *
 * cvg_writtct ( fptr, el, iret )                                         *
 *                                                                        *
 * Input Parameters:                                                      *
 *	*fptr 		FILE		File pointer                      *
 *	*el		VG_DBStruct	Pointer to a TCTrk element        *
 *                                                                        *
 * Output Parameters:                                                     *
 *                                                                        *
 *	*iret		int		Return code                       *
 *                                      0:	Normal                    *
 *                                      -6: 	No file has been opened   *
 *                                      -17:	Failed to load TcInfo     *
 **                                                                       *
 * Log:                                                                   *
 * m.gamazaychikov/SAIC	05/07		Created                           *
 *************************************************************************/
{
   int		ier, nbytes, nbhdr, npts, tmp, one=1;
   char		*ptcinfo, *ptctrack;
/*-----------------------------------------------------------------------*/  

   *iret = 0;

   if ( fptr == NULL ) {
      *iret = -6;
      return;
   }

   /*
    *  Write out element header.
    */
   nbhdr = sizeof(VG_HdrStruct);
   cfl_writ( fptr, nbhdr,  (unsigned char *)&el->hdr, &ier);

   /*
    *  Load the TcInfo into a string
    */
   nbytes = cvg_writTcInfo ( el->elem.tce.info, &ptcinfo, &ier);

   if ( ier < 0 ) {
      *iret = -17;
      return;
   }
   nbytes = sizeof (TcInfo);
   cfl_writ( fptr, nbytes, (unsigned char *)ptcinfo, &ier);
   free( ptcinfo );

   /*
    *  Write out TcTrack line type, color and number of points.
    */
   nbytes = sizeof(int);
   npts = el->elem.tct.numTrackPts;
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tct.lincol, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&el->elem.tct.lintyp, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)&npts, &ier);
   /*
    *  Swap the number of points for some platforms before
    *  using them to write out lat/lons.
    */
    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX )   {
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
   }

   /*
    *  Write out the TcTrack data
    */
   nbytes = cvg_writTcTrack ( el, &ptctrack, &ier);
   nbytes = npts * (int) (sizeof (TcTrack));
   cfl_writ( fptr, nbytes, (unsigned char *)ptctrack, &ier);
   free( ptctrack );

}

/*=======================================================================*/

int cvg_writTcInfo ( TcInfo info, char **ptcinfo, int *iret )
/**************************************************************************
 * cvg_writTcInfo                                                         *
 *                                                                        *
 * This function loads TcInfo fields into a string to write to file.	  *
 *                                                                        *
 * cvg_writTcInfo ( info, ptcinfo, iret )                                 *
 *                                                                        *
 * Input Parameters:                                                      *
 * 	info		TcInfo		TcInfo structure                  *
 * 	                                                                  *
 * Output Parameters:                                                     *
 * 	*ptcinfo	char		Pointer to a string	  	  *
 *	*iret		int		Return code                       *
 *					0:	Normal                    *
 *					-1:	Failed to load TcInfo     *
 * Return Parameters:                                                     *
 *			int		length of the string		  *
 * 	                                                                  *
 * NOTE: *ptcinfo should be freed by the calling routine.                 *
 * 	                                                                  *
 * Logs:                                                                  *
 * m.gamazaychikov/SAIC	04/07		Created	                          *
 *************************************************************************/
{
   int	size;
   char str[ STD_STRLEN ];
/*-----------------------------------------------------------------------*/
   
   *iret = 0;
   
   if ( (*ptcinfo = ( char * ) malloc ( ONEBLOCK ) ) == NULL ) {
      *iret = -1;
      return 0;
   }

   size = ONEBLOCK;
   *ptcinfo[ 0 ] = '\0';

   /*
    * Load storm number
    */
   sprintf( str, "<%s>%s", TCA_STORMNUM_TAG, info.stormNum );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load issue status        
    */
   sprintf( str, "<%s>%s", TCA_ISSUESTATUS_TAG, info.issueStatus );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load basin       
    */
   sprintf( str, "<%s>%s", TCA_BASIN_TAG, info.basin );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load advisory number      
    */
   sprintf( str, "<%s>%s", TCA_ADVISORYNUM_TAG, info.advisoryNum );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load storm name           
    */
   sprintf( str, "<%s>%s", TCA_STORMNAME_TAG, info.stormName );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load storm type           
    */
   sprintf( str, "<%s>%s", TCA_STORMTYPE_TAG, info.stormType );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load valid time           
    */
   sprintf( str, "<%s>%s", TCA_VALIDTIME_TAG, info.validTime );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load time zone          
    */
   sprintf( str, "<%s>%s", TCA_TIMEZONE_TAG, info.timezone );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load forecast period          
    */
   sprintf( str, "<%s>%s", TCA_FCSTPRD_TAG, info.fcstpd );
   if ( ( cvg_catTcStr( str, ptcinfo, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   return strlen( *ptcinfo );	  
}
/*=======================================================================*/

int cvg_writTcTrack ( VG_DBStruct *el, char **ptctrack, int *iret )
/**************************************************************************
 * cvg_writTcInfo                                                         *
 *                                                                        *
 * This function loads TcInfo fields into a string to write to file.	  *
 *                                                                        *
 * cvg_writTcInfo ( el, ptctrack, iret )                                  *
 *                                                                        *
 * Input Parameters:                                                      *
 * 	el		VG_DBStruct	TCT element			  *
 * 	                                                                  *
 * Output Parameters:                                                     *
 * 	*ptctrack	char		Pointer to a string	  	  *
 *	*iret		int		Return code                       *
 *					0:	Normal                    *
 *					-1:	Failed to load TcTrack	  *
 * Return Parameters:                                                     *
 *			int		length of the string		  *
 * 	                                                                  *
 * NOTE: *ptctrack should be freed by the calling routine.                *
 * 	                                                                  *
 * Logs:                                                                  *
 * m.gamazaychikov/SAIC	04/07		Created	                          *
 * m.gamazaychikov/SAIC	10/08		Added stSrc to TCT element struct *
 *************************************************************************/
{
   int		size, npts, ii, tmp, one=1;
   char 	str[ STD_STRLEN ];
/*-----------------------------------------------------------------------*/
   
   *iret = 0;
   
   if ( (*ptctrack = ( char * ) malloc ( ONEBLOCK ) ) == NULL ) {
      *iret = -1;
      return 0;
   }

   size = ONEBLOCK;
   *ptctrack[ 0 ] = '\0';
   npts = el->elem.tct.numTrackPts;
   /*
    *  Swap the number of points for some platforms before
    *  using them to write out lat/lons.
    */
    if ( MTMACH == MTULTX ||
         MTMACH == MTALPH ||
         MTMACH == MTLNUX )   {
        mv_swp4 ( &one, &npts, &tmp );
        npts = tmp;
   }
   for ( ii = 0; ii < npts; ii++ ) {
        sprintf( str, "<%s_%d>%f|%f|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s",
               TCT_TRKPNT_TAG, ii, 
	       el->elem.tct.trackPnt[ ii ].lat,
	       el->elem.tct.trackPnt[ ii ].lon,
               el->elem.tct.trackPnt[ ii ].advDate,
               el->elem.tct.trackPnt[ ii ].tau,
               el->elem.tct.trackPnt[ ii ].mxWnd,
               el->elem.tct.trackPnt[ ii ].wGust,
               el->elem.tct.trackPnt[ ii ].mslp,
               el->elem.tct.trackPnt[ ii ].tcDv,
               el->elem.tct.trackPnt[ ii ].tcDvLbl,
               el->elem.tct.trackPnt[ ii ].tcDir,
               el->elem.tct.trackPnt[ ii ].tcSpd,
               el->elem.tct.trackPnt[ ii ].dtLbl,
               el->elem.tct.trackPnt[ ii ].stSrc );
                                                                                                        
         if ( ii != npts - 1 ) {
             strcat( str, "|" );
         }
                                                                                                      
         if ( ( cvg_catTcStr( str, ptctrack, &size ) ) != 0 ) {
             *iret = -1;
             return 0;
         }
   }

   return strlen( *ptctrack );	  
}
/*=======================================================================*/

static int cvg_catTcStr( char * str, char **ptcinfo, int *size )
/**************************************************************************
 * cvg_catTcStr	                                                          *
 *                                                                        *
 * This function appends a tcinfo attribute to a string 		  *
 *                                                                        *
 * cvg_catTcStr ( str, ptcinfo, size )                        		  *
 *                                                                        *
 * Input Parameters:                                                      *
 * 	*str		char		TC attribute string               *
 * 	                                                                  *
 * Output Parameters:                                                     *
 * 	*ptcinfo	char		Pointer to memory block		  *
 *	*size		int		size of the memory block          *
 * 	                                                                  *
 * Return Parameters:                                                     *
 *			int		0:	Normal                    *
 *					-1:	Not enough memory	  * 
 * 	                                                                  *
 * Logs:                                                                  *
 * m.gamazaychikov/SAIC	04/07		Created	after cvg_catTcaStr	  *
 *************************************************************************/
{
   char *newtcinfo= NULL;
/*-----------------------------------------------------------------------*/

   if ( ( strlen( str ) + strlen( *ptcinfo ) ) < (size_t)*size ) {
      strcat( *ptcinfo, str );
   }
   else {
      if ( ( newtcinfo = ( char * ) realloc( *ptcinfo, *size + ONEBLOCK ) ) == NULL ) {
 	 return -1;
      }
      *size += ONEBLOCK;
      strcat( newtcinfo, str );
      *ptcinfo = newtcinfo;
   }

   return 0;
}
