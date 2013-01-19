#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

#define HURRICANE_WARNING       0
#define HURRICANE_WATCH         1
#define TROPICAL_STORM_WARNING  2
#define TROPICAL_STORM_WATCH    3

#define PLOT_TROPICAL_STORM_WATCH    	0
#define PLOT_HURRICANE_WATCH         	1
#define PLOT_TROPICAL_STORM_WARN	2
#define PLOT_HURRICANE_WARN	       	3

static void tca_makeIndex ( const TcaInfo *info, int **segIndex );

void cds_tca ( VG_DBStruct *el_ptr, int *iret )
/************************************************************************
 * cds_tca								*
 *									*
 * This function displays TCAs to the output device.			*
 *									*
 * cds_tca ( el_ptr, iret )						*
 *									*
 * Input parameters:							*
 * 	*el_ptr		VG_DBStruct	Pointer to VG record structure	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 0: normal			*
 *					-1: station id not found	*
 **									*
 * Log:									*
 * B. Yin/SAIC		02/04	Created                 		*
 * B. Yin/SAIC		03/04	Changed gh_bksq calling sequence	*
 * B. Yin/SAIC		03/04	Appended blank to string stn		*
 * B. Yin/SAIC          04/04   Changed w_type values for watch/warning *
 * m.gamazaychikov/SAIC	04/04 	Replaced call to gh_bkus -> to gh_bkpl	*
 *				Replaced call to gh_bkrd -> to gh_bktb	*
 * B. Yin/SAIC          05/04   Added code to free the table.		*
 * B. Yin/SAIC          05/04   Modified code to handle blank id	*
 * B. Yin/SAIC          07/04   Added code to handle water and islands 	*
 * m.gamazaychikov/SAIC 08/04   Changed code to handle water and islands*
 * B. Yin/SAIC          01/05   Added code to plot segments in order 	*
 * S. Gilbert/NCEP      11/05   Added Zero WW option                    *
 * S. Gilbert/NCEP      01/06   Changed advisoryNum in gh_0bkp arg list *
 * m.gamazaychikov/SAIC 04/08   Changed CS to gh_bkpl to include size_t	*
 ***********************************************************************/
{
    int		ier, ii, jj, counter, iseq, iarea, ibrkpts, nclose, npt;
    int		plot, ibkus[ 50 ][ 4 ], numbus[ 4 ], iiarea[ 50 ][ 4 ];
    int         icnt[ 1 ][ 4 ], *segIndex, wwType;
    char	stn[ 9 ], nstn[ 90 ], dev[ 6 ];
    char	tbl_name[ ] = "TCA_BKPTS"; 
    char	*str_ptr;
/*---------------------------------------------------------------------*/

    *iret 	= 0;
    ier		= 0;
    iseq	= 0;
    iarea	= 0;
    npt 	= 0;			/* points in the hot list */
    str_ptr 	= NULL;		
	
    strcpy( dev, "XW" );
    strcpy( stn, " " );

    /*
     * If there are no watches/warnings in the TCA element,
     * display info text box for the storm.
     */
    if ( el_ptr->elem.tca.info.wwNum == 0 ) {
       gh_0bkp( el_ptr->elem.tca.info.stormName,
                el_ptr->elem.tca.info.advisoryNum,
                (int *)&(el_ptr->elem.tca.info.stormType),
                &(el_ptr->elem.tca.info.text_lat),
                &(el_ptr->elem.tca.info.text_lon),
                &(el_ptr->elem.tca.info.text_font),
                &(el_ptr->elem.tca.info.text_size),
                &(el_ptr->elem.tca.info.text_width), &ier,
                strlen(el_ptr->elem.tca.info.stormName),
                strlen(el_ptr->elem.tca.info.advisoryNum) );
       return;
    }
    
    /*
     * Initialization
     */
    clo_init( &ier );
    gh_bktb( &ier );
    gh_rtbl( &ier );

    for ( ii = 0; ii < 50; ii++ ) {
	for ( jj = 0; jj < 4; jj++ ) {
	    ibkus [ ii ][ jj ] = 0;
            iiarea[ ii ][ jj ] = 0;
        }
    }
    for ( jj = 0; jj < 4; jj++ ) {
        icnt [ 0 ][ jj ] = 0;
    }
    
    tca_makeIndex ( &(el_ptr->elem.tca.info), &segIndex );

    /*
     * go through each warning/watch
     */
    for ( plot = 0; plot < 4; plot++ ) {

	switch ( plot ) {

	    case PLOT_TROPICAL_STORM_WATCH :

		wwType = TROPICAL_STORM_WATCH;
		break;

	    case PLOT_HURRICANE_WATCH :  

		wwType = HURRICANE_WATCH;
		break;

	    case PLOT_TROPICAL_STORM_WARN :

		wwType = TROPICAL_STORM_WARNING;
		break;

	    case PLOT_HURRICANE_WARN :

		wwType = HURRICANE_WARNING;
		break;

	    default:

		wwType = HURRICANE_WARNING;
		break;
	}

        for ( counter = 0; counter < el_ptr->elem.tca.info.wwNum; counter++ ) {

	    if ( segIndex[ counter ] != plot ) continue;

	    /*
	     *  Initialize 
	     */
            for ( ii = 0; ii < 4; ii++ ) {
                numbus[ ii ] = 0;
            }

	    numbus[ wwType ] = 1;

	    /*
	     *  Get sequence numbers of the break points
	     */
  	    for ( ibrkpts = 0; 
	          ibrkpts < el_ptr->elem.tca.info.tcaww[ counter ].numBreakPts; 
	          ibrkpts++ ) {

	        /*
	         *  Get 5 closest station ids for the break point
	         */
                nclose 	= 5;		/* number of closest stations looking at */
	        clo_tclosest( tbl_name,
	                  el_ptr->elem.tca.info.tcaww[ counter ].breakPnt[ ibrkpts ].lat,
                          el_ptr->elem.tca.info.tcaww[ counter ].breakPnt[ ibrkpts ].lon,
	                  nclose, &ier );
	        clo_tgid( tbl_name, nclose, nclose * sizeof( stn ), &npt, nstn, &ier );

	        if ( npt == 0 ) {
 	           *iret = -1;
	           continue;
	        }

	        /*
	         * Get the first station id that is not blank
	         */
                if ( (str_ptr = strtok( nstn, ";" ) )  == NULL ) {
                   --nclose;
                   while ( --nclose ) {
                         str_ptr =  strtok( NULL, ";" );
                         if ( str_ptr != NULL ) {
                            break;
                         }
                   }
                }

	        if ( str_ptr == NULL ) {
                   *iret = -1;
                   continue;
                }

	        strcpy( stn, str_ptr );
	        strncat( stn, "       ", 8 - strlen( stn ) );
	
	        /*
	         * Get the sequence number of the break point
	         */
                gh_bksq( stn, &iseq, &iarea, &ier );
                ibkus [ ibrkpts ][ wwType ] = iseq;
                iiarea[ ibrkpts ][ wwType ] = iarea;
                icnt[ 0 ][ wwType ] = el_ptr->elem.tca.info.tcaww[ counter ].numBreakPts;
	    }

	    /*
	     * Draw
	     */
            gh_bkpl( ibkus, numbus, dev, iiarea, icnt, &ier, strlen(dev) );
        }
    }

    G_FREE ( segIndex, int );
    gh_ftbl ( &ier );
}

/*=====================================================================*/

static void tca_makeIndex ( const TcaInfo *info, int **segIndex )
/************************************************************************
 * cds_makeIndex							*
 *									*
 * This routine creates an ordered index for TCA segments.		*
 * segIndex needs to be freed by the calling routine.			*
 *									*
 * cds_makeIndex ( info, segIndex )					*
 *									*
 * Input parameters:							*
 * 	*info		TcaInfo		TCA info structure		*
 *									*
 * Output parameters:							*
 *	**segIndex	int		array contins ordered seg#.	*
 **									*
 * Log:									*
 * B. Yin/SAIC		01/05	Created                 		*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    /* 
     *  Allocate memory for the index array
     */
    G_MALLOC ( *segIndex, int, info->wwNum, "TCA segment index array" );

    /*
     *  Generate the ordered segment index
     */
    for ( ii = 0; ii < info->wwNum; ii++ ) {

	(*segIndex)[ ii ] = -1;

        if ( info->tcaww[ ii ].severity == HURRICANE_S &&
	     info->tcaww[ ii ].advisoryType == WARNING ) {

           (*segIndex)[ ii ] = PLOT_HURRICANE_WARN;

	}
 	else if ( info->tcaww[ ii ].severity == HURRICANE_S &&
                  info->tcaww[ ii ].advisoryType == WATCH ) {

           (*segIndex)[ ii ] = PLOT_HURRICANE_WATCH;

	}
 	else if ( info->tcaww[ ii ].severity == TROPICAL_STORM_S &&
                  info->tcaww[ ii ].advisoryType == WARNING ) {

           (*segIndex)[ ii ] = PLOT_TROPICAL_STORM_WARN;

	}
 	else if ( info->tcaww[ ii ].severity == TROPICAL_STORM_S &&
                  info->tcaww[ ii ].advisoryType == WATCH ) {
		  
           (*segIndex)[ ii ] = PLOT_TROPICAL_STORM_WATCH;

	}

    }

}
