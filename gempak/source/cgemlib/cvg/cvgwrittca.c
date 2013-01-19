#include "cvgcmn.h"

#define ONEBLOCK	2048	

/*
 *  Private function 
 */
static int cvg_catTcaStr ( char * str, char **ptca, int *size );

void cvg_writtca ( FILE *fptr, VG_DBStruct *el, int *iret )
/**************************************************************************
 * cvg_writtca                                                            *
 *                                                                        *
 * This function writes a TCA element to file at the current location.    *
 *                                                                        *
 * cvg_writtca ( fptr, el, iret )                                         *
 *                                                                        *
 * Input Parameters:                                                      *
 *	*fptr 		FILE		File pointer                      *
 *	*el		VG_DBStruct	Pointer to a TCA element          *
 *                                                                        *
 * Output Parameters:                                                     *
 *                                                                        *
 *	*iret		int		Return code                       *
 *                                      0:	Normal                    *
 *                                      -6: 	No file has been opened   *
 *                                      -17:	Failed to load TcaInfo    *
 **                                                                       *
 * Log:                                                                   *
 * B. Yin/SAIC		02/04		Created                           *
 * B. Yin/SAIC		05/04		Allocated memory for TCA string   *
 * S. Gilbert/NCEP	12/05		Updated element hdr.recsz before  *
 *                                      writing out TCA element.          *
 *************************************************************************/
{
   int		ier, nbytes, one=1, temp;
   char		*ptca;
/*-----------------------------------------------------------------------*/  

   *iret = 0;

   if ( fptr == NULL ) {
      *iret = -6;
      return;
   }


   /*
    *  Load the TcaInfo into a string
    */
   nbytes = cvg_loadTcaFileInfo ( el->elem.tca.info, &ptca, &ier);

   if ( ier < 0 ) {
      *iret = -17;
      return;
   }

   /*
    *  Update element header record size.  Remember to swap
    *  bytes on some machines.
    */
   temp = nbytes + sizeof( VG_HdrStruct );
   if ( MTMACH == MTULTX ||
        MTMACH == MTALPH ||
        MTMACH == MTLNUX )
       ier = mv_swp4( &one, &temp, &(el->hdr.recsz) );
   else
       el->hdr.recsz = temp;

   /*
    *  Write out element header and TcaInfo string.
    */
   cfl_writ( fptr, sizeof( VG_HdrStruct ), (unsigned char *)&el->hdr, &ier);
   cfl_writ( fptr, nbytes, (unsigned char *)ptca, &ier);
   free( ptca );
}

/*=======================================================================*/

int cvg_loadTcaFileInfo ( TcaInfo info, char **ptca, int *iret )
/**************************************************************************
 * cvg_loadTcaFileInfo                                                    *
 *                                                                        *
 * This function loads TcaInfo fields into a string to write to file.	  *
 *                                                                        *
 * cvg_loadTcaFileInfo ( info, ptca, iret )                               *
 *                                                                        *
 * Input Parameters:                                                      *
 * 	info		TcaInfo		TcaInfo structure                 *
 * 	                                                                  *
 * Output Parameters:                                                     *
 * 	*ptca		char		Pointer to a string		  *
 *	*iret		int		Return code                       *
 *					0:	Normal                    *
 *					-1:	Failed to load Tca fields *
 * Return Parameters:                                                     *
 *			int		length of the string		  *
 * 	                                                                  *
 * NOTE: *ptca should be freed by the calling routine.                    *
 * 	                                                                  *
 * Logs:                                                                  *
 * B. Yin/SAIC		02/04		Created	                          *
 * B. Yin/SAIC		03/04		Added storm type in tca structure *
 * B. Yin/SAIC		05/04		Allocated memory dynamically	  *
 * B. Yin/SAIC		07/04		Modified for more than 2 bkpts	  *
 * B. Yin/SAIC		12/04		Added timezone			  *
 * B. Yin/SAIC		05/04		Added issue status, removed year  *
 * S. Gilbert/NCEP	11/05		Added text_lat, text_lon,         *
 *                                      text_font, text_size & text_width *
 * S. Gilbert/NCEP	01/06		Changed format specifier for      *
 *                                      advisoryNum                       *
 *************************************************************************/
{
   int	counter, size, ii;
   char str[ STD_STRLEN ];
/*-----------------------------------------------------------------------*/
   
   *iret = 0;
   
   if ( (*ptca = ( char * ) malloc ( ONEBLOCK ) ) == NULL ) {
      *iret = -1;
      return 0;
   }

   size = ONEBLOCK;
   *ptca[ 0 ] = '\0';

   /*
    * Load storm number
    */
   sprintf( str, "<%s>%d", TCA_STORMNUM_TAG, info.stormNum );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load issue status        
    */
   sprintf( str, "<%s>%c", TCA_ISSUESTATUS_TAG, info.issueStatus );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load basin       
    */
   sprintf( str, "<%s>%d", TCA_BASIN_TAG, info.basin );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load advisory number      
    */
   sprintf( str, "<%s>%s", TCA_ADVISORYNUM_TAG, info.advisoryNum );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load storm name           
    */
   sprintf( str, "<%s>%s", TCA_STORMNAME_TAG, info.stormName );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load storm type           
    */
   sprintf( str, "<%s>%d", TCA_STORMTYPE_TAG, info.stormType );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load valid time           
    */
   sprintf( str, "<%s>%s", TCA_VALIDTIME_TAG, info.validTime );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load time zone          
    */
   sprintf( str, "<%s>%s", TCA_TIMEZONE_TAG, info.timezone );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load Text Box latitude
    */
   sprintf( str, "<%s>%f", TCA_TEXTLAT_TAG, info.text_lat );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load Text Box longitude
    */
   sprintf( str, "<%s>%f", TCA_TEXTLON_TAG, info.text_lon );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load Text Box Font
    */
   sprintf( str, "<%s>%d", TCA_TEXTFONT_TAG, info.text_font );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load Text Box text size
    */
   sprintf( str, "<%s>%f", TCA_TEXTSIZE_TAG, info.text_size );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load Text Box text width
    */
   sprintf( str, "<%s>%d", TCA_TEXTWIDTH_TAG, info.text_width );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load watches/warning number           
    */
   sprintf( str, "<%s>%d", TCA_WWNUM_TAG, info.wwNum );
   if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
      *iret = -1;
      return 0;
   }

   /*
    * Load array of watches/warnings          
    */
   if ( info.wwNum != 0 ) {
      for ( counter = 0; counter < info.wwNum; counter++ ) {
	  /*
	   * Load TCA watches/warning string 
	   */ 
	  sprintf( str, "<%s_%d>%d|%d|%d", 
			  TCA_TCAWWSTR_TAG, counter, 
			  info.tcaww[ counter ].severity,
			  info.tcaww[ counter ].advisoryType,
			  info.tcaww[ counter ].specialGeog );
          if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
             *iret = -1;
             return 0;
          }

	  /*
	   * Load TCA break points
	   */ 
	  sprintf( str, "<%s_%d>%d<%s_%d>", 
			  TCA_NUMBKPTS_TAG, counter, info.tcaww[ counter ].numBreakPts,
			  TCA_BREAKPTS_TAG, counter ); 

          if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
             *iret = -1;
             return 0;
          }

	  for ( ii = 0; ii < info.tcaww[ counter ].numBreakPts; ii++ ) {
	      sprintf( str, "%f|%f|%s", 
	               info.tcaww[ counter ].breakPnt[ ii ].lat,
	               info.tcaww[ counter ].breakPnt[ ii ].lon,
	               info.tcaww[ counter ].breakPnt[ ii ].breakPtName );

              if ( ii != info.tcaww[ counter ].numBreakPts - 1 ) {
                 strcat( str, "|" );
	      }

              if ( ( cvg_catTcaStr( str, ptca, &size ) ) != 0 ) {
                 *iret = -1;
                 return 0;
              }
          }
      }
   }
   return strlen( *ptca );	  
}
/*=======================================================================*/


static int cvg_catTcaStr( char * str, char **ptca, int *size )
/**************************************************************************
 * cvg_catTcaStr	                                                  *
 *                                                                        *
 * This function appends a tca attribute to a string 			  *
 *                                                                        *
 * cvg_catTcaStr ( str, ptca, size )                        		  *
 *                                                                        *
 * Input Parameters:                                                      *
 * 	*str		char		Tca attribute string              *
 * 	                                                                  *
 * Output Parameters:                                                     *
 * 	*ptca		char		Pointer to memory block		  *
 *	*size		int		size of the memory block          *
 * 	                                                                  *
 * Return Parameters:                                                     *
 *			int		0:	Normal                    *
 *					-1:	Not enough memory	  * 
 * 	                                                                  *
 * Logs:                                                                  *
 * B. Yin/SAIC		05/04		Created				  *
 *************************************************************************/
{
   char *newtca= NULL;
/*-----------------------------------------------------------------------*/

   if ( ( strlen( str ) + strlen( *ptca ) ) < (size_t)*size ) {
      strcat( *ptca, str );
   }
   else {
      if ( ( newtca = ( char * ) realloc( *ptca, *size + ONEBLOCK ) ) == NULL ) {
 	 return -1;
      }
      *size += ONEBLOCK;
      strcat( newtca, str );
      *ptca = newtca;
   }

   return 0;
}
