#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "ghcmn.h"

void gh_bkrv ( char *filnam,  int *istnum,  int *iyear,    int *ibasin, 
               char  *iadvnm,  int *isttyp,  char *stname,  char *vtime, 
               int  *icount,  int isev[],   int ityp[],    int igeog[],
               float lat[],   float lon[],  char *bpstr,   int numbkp[],
               char *tzone,   char *status, int *iret )
/************************************************************************
 * gh_bkrv                                                              *
 *                                                                      *
 * This function opens a VGF file containing breakpoint information,	*
 * reads in the TCA structure, and returns elements of the structure.	*
 *                                                                      *
 * gh_bkrv ( filnam, istnum, iyear, ibasin, iadvnm, isttyp, stname,	*
 *           vtime, icount, isev, ityp, igeog, lat, lon, bpstr, numbkp,	*
 *           tzone, status, iret )						*
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
 * m.gamazaychikov/SAIC 04/04   Created					*
 * m.gamazaychikov/SAIC 08/04   Added numbkp as output parameter	*
 * B. Yin/SAIC		12/04	Added time zone				*
 * B. Yin/SAIC		04/05	Added issue status, removed year,	*
 *				Freed memory				*
 * E. Safford/SAIC	05/05	free memory from first while loop	*
 * B. Yin/SAIC		06/05	check 'more' before free tca memory	*
 * S. Gilbert/NCEP	01/06	changed iadvnm from int to char         *
 ***********************************************************************/
{
    int         	jj, ii, ier, ne, more, curpos, ibpt;
    char		syear[ 3 ];

    VG_DBStruct		el;
    FILE            	*ofptr;

/*---------------------------------------------------------------------*/
    *iret = 0;
    *icount = 0;

    /*
     *  Open the file and loop through all the elements
     *  to set the range records.
     */
    bpstr[0] = '\0';
    crg_init ( &ier );
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    ofptr = (FILE *) cfl_ropn(filnam, "", &ier);

    if ( ier != 0 )  {            /* open failed */
        *iret = -1;
        return;
    }
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
        
        cvg_rdrecnoc ( filnam, ofptr, curpos, &el, &ier );
        if ( ier < 0 )  {
            more = G_FALSE;
        }
        else  {
            crg_set ( &el, curpos, 1, &ier );
            curpos += el.hdr.recsz;
            ne++;
        }

	if(( el.hdr.vg_type == TCA_ELM ) && ( more == G_TRUE ) ){
  	    cvg_freeBkpts ( &el ); 
	}
    }
    cfl_clos ( ofptr, &ier );

    /*
     *  Loop through all the elements of VG records.
     */
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    ofptr = (FILE *) cfl_ropn(filnam, "", &ier);
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {

        cvg_rdrecnoc( filnam, ofptr, curpos, &el, &ier );

        if ( ier < 0 )  {
            more = G_FALSE;
        }
        else if ( el.hdr.recsz > 0 ) {

          curpos += el.hdr.recsz;

          if ( el.hdr.vg_type == TCA_ELM ) {
                /*
                 * Copy records from the structure
                 * into local variables.
                 */

		strncpy ( syear, el.elem.tca.info.validTime, 2 );
		syear[ 2 ] = '\0';
                *istnum = el.elem.tca.info.stormNum;
                *iyear  = atoi ( syear ) + 2000;
                *ibasin = el.elem.tca.info.basin;
                strcpy (iadvnm, el.elem.tca.info.advisoryNum);
                *isttyp = el.elem.tca.info.stormType;
                *icount = el.elem.tca.info.wwNum;
		*status = el.elem.tca.info.issueStatus;
                strcpy (stname, el.elem.tca.info.stormName);
                strcpy (vtime,  el.elem.tca.info.validTime);
                strcpy (tzone,  el.elem.tca.info.timezone);

                jj = 0;
                for ( ii = 0; ii < *icount; ii++ )  {
                  isev  [ii] = el.elem.tca.info.tcaww[ii].severity;
                  ityp  [ii] = el.elem.tca.info.tcaww[ii].advisoryType;
                  igeog [ii] = el.elem.tca.info.tcaww[ii].specialGeog;
                  numbkp[ii] = el.elem.tca.info.tcaww[ii].numBreakPts;

                  for ( ibpt = 0; ibpt < numbkp[ii]; ibpt++ ) {
                    lat[jj] = el.elem.tca.info.tcaww[ii].breakPnt[ibpt].lat;
                    lon[jj] = el.elem.tca.info.tcaww[ii].breakPnt[ibpt].lon;
                    strcat (bpstr, el.elem.tca.info.tcaww[ii].breakPnt[ibpt].breakPtName);
                    strcat (bpstr, ";");
                    jj = jj+1;
                  }
                }

		/*
		 *  Free TCA memory
		 */
		cvg_freeBkpts ( &el );
            }
        }
    }

    cfl_clos ( ofptr, &ier );

}
