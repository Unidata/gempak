#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_list ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_list								*
 *									*
 * This function displays lists to the output device.			*
 *									*
 * cds_list ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		09/02	initial coding				*
 * J. Wu/SAIC		11/02	revise for the new LIST structure	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * S. Jacobs/NCEP	 3/03	Changed State list to use state id	*
 * J. Wu/SAIC		06/03	handle attr. setting in uattribd.tbl	*
 * A. Hardy/NCEP	 3/04	Added LISTYP_MZCNTY			*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int		ii, filcol, filtyp, minpts, lincol, lintyp, linwid;
    int		mrkcol, mrktyp, mrkwid, ier;
    float	filsiz, filter, mrksiz;
    char	bndtyp[32], tag[32], symtyp[5];
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    /*
     *  Call "GPLBND" with a proper bound type string to draw the bounds.
     *     e.g. "CNTY_BNDS|<FIPS>1103".
     */
    if ( el->elem.lst.info.subtyp == LISTYP_WFO ) 
        strcpy ( tag, "CWA_BNDS" );
    else if ( el->elem.lst.info.subtyp == LISTYP_COUNTY )
        strcpy ( tag, "CNTY_BNDS" );
    else if ( el->elem.lst.info.subtyp == LISTYP_STATE )
        strcpy ( tag, "STATE_BNDS" );	
    else if ( el->elem.lst.info.subtyp == LISTYP_ZONE )
        strcpy ( tag, "PFZ" );	
    else if ( el->elem.lst.info.subtyp == LISTYP_MZCNTY )
        strcpy ( tag, "WBCMZ_BNDS" );	
    else 
        strcpy ( tag, "" );	
      
    strcat ( tag, "|<" );
    
    if ( el->elem.lst.info.subtyp == LISTYP_WFO ) {
        strcat ( tag, "WFO" );
    }
    else if ( el->elem.lst.info.subtyp == LISTYP_STATE ) {
        strcat ( tag, "STATE" );
    }
    else  {
        strcat ( tag, "FIPS" );
    }

    strcat ( tag, ">" );
    
    filcol = filtyp = minpts = lincol = lintyp = linwid = 0;
    filsiz = filter = 0.0F;
            
    strcpy ( symtyp, "MARK" );
    
    mrkcol = ( cdsUattr[indx].maj_col == 0 ) ?
		el->hdr.maj_col : cdsUattr[indx].maj_col;
    mrktyp = ( cdsUattr[indx].info.lst->mrktyp == 0 ) ?
		el->elem.lst.info.mrktyp : cdsUattr[indx].info.lst->mrktyp;
    mrksiz = ( G_DIFF(cdsUattr[indx].info.lst->mrksiz, 0.0F) ) ?
		el->elem.lst.info.mrksiz : cdsUattr[indx].info.lst->mrksiz; 
    mrkwid = ( cdsUattr[indx].info.lst->mrkwid == 0 ) ?
		el->elem.lst.info.mrkwid : cdsUattr[indx].info.lst->mrkwid;
         
    for ( ii = 0; ii < el->elem.lst.data.nitems; ii++ ) {
	strcpy ( bndtyp, tag );
	strcat ( bndtyp, el->elem.lst.data.item[ii] );       
	gplbnd ( bndtyp, &filcol, &filsiz, &filtyp, &filter,
	         &minpts, &lincol, &lintyp, &linwid,
	         symtyp, &mrkcol, &mrktyp, &mrksiz, &mrkwid, &ier,
		 strlen(bndtyp), strlen(symtyp) );    
    }        

}
