#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

void cds_jet ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_jet								*
 *									*
 * This function displays a jet element to the output device.		*
 *									*
 * cds_jet ( el, indx, iret )						*
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
 * J. Wu/SAIC		09/03	initial coding				*
 * J. Wu/SAIC		10/03	revise to use the input jet's header	*
 * J. Wu/SAIC		12/03	cleanup					*
 * J. Wu/SAIC		12/03	snap jet before display			*
 * D.W.Plummer/NCEP	02/04	center barb plotting and add mask	*
 * J. Wu/SAIC		05/04	control barb/hash color through table	*
 * 				and move barb clearing control to GUI	*
 * J. Wu/SAIC		09/04	allow to set text type in uattribd.tbl	*
 * H. Zeng/SAIC		10/04	added '+' to flight lvl			*
 * M. Li/SAIC		10/05	Added ctb_rdprf				*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 * S. Danz/AWC		04/06	initialized spl.info.spltyp,		*
 *				 wnd.info.wndtyp, spt.info.sptxtyp	*
 * J. Lewis/AWC         03/07   remove checks for delta format          *
 * J. Lewis/AWC         03/07   remove formatting of flight lvl         *
 ***********************************************************************/
{
    int		ii, ier, idx;
    int		color, type, dir, width, str;
    int		align, font, thw, turbsym, ttype;
    float	size, rotn;
    char	smooth, closed, filled;
    VG_DBStruct	jet, el_tmp;        
/*---------------------------------------------------------------------*/

    *iret = 0;
        
    /*
     *  Make a local copy of the jet.
     */ 
    jet = *el;
    
    /*
     *  Snap the barbs/hashes onto the jet. This will not change the jet
     *  saved in the work file but allows a proper display of the jet
     *  when the map projetction changes.
     */
    cvg_snapjet ( &jet, &jet, &ier );
    

    /*
     *  Build/display an SPLN_ELM - jet line - from the input jet.
     *  Save the attr settings before display and restore after display.
     */ 
    el_tmp.hdr = jet.hdr;
    el_tmp.hdr.vg_class = (char)CLASS_LINES;
    el_tmp.hdr.vg_type = (char)SPLN_ELM;
    el_tmp.elem.spl.info.spltyp = DEFLTSET; 

    cds_getinx ( &el_tmp, &idx, &ier );
    
    color  = cdsUattr[idx].maj_col; 
    smooth = cdsUattr[idx].smooth;
    closed = cdsUattr[idx].closed;
    filled = cdsUattr[idx].filled;
    type   = cdsUattr[idx].info.spl->spltyp;
    dir    = cdsUattr[idx].info.spl->spldir;
    size   = cdsUattr[idx].info.spl->splsiz;
    width  = cdsUattr[idx].info.spl->splwid;
    str    = cdsUattr[idx].info.spl->splstr;
    
    cdsUattr[idx].maj_col = cdsUattr[indx].maj_col; 
    cdsUattr[idx].smooth  = cdsUattr[indx].smooth;
    cdsUattr[idx].closed  = cdsUattr[indx].closed;
    cdsUattr[idx].filled  = cdsUattr[indx].filled;
    cdsUattr[idx].info.spl->spltyp = cdsUattr[indx].info.jet->line.spltyp;
    cdsUattr[idx].info.spl->spldir = cdsUattr[indx].info.jet->line.spldir;
    cdsUattr[idx].info.spl->splsiz = cdsUattr[indx].info.jet->line.splsiz;
    cdsUattr[idx].info.spl->splwid = cdsUattr[indx].info.jet->line.splwid;
    cdsUattr[idx].info.spl->splstr = cdsUattr[indx].info.jet->line.splstr;

    el_tmp.hdr.maj_col = jet.elem.jet.line.splcol;
    el_tmp.elem.spl    = jet.elem.jet.line.spl;
    
    cds_dspelm ( &el_tmp, &ier );
       
    cdsUattr[idx].maj_col = color; 
    cdsUattr[idx].smooth  = smooth;
    cdsUattr[idx].closed  = closed;
    cdsUattr[idx].filled  = filled;
    cdsUattr[idx].info.spl->spltyp = type;
    cdsUattr[idx].info.spl->spldir = dir;
    cdsUattr[idx].info.spl->splsiz = size;
    cdsUattr[idx].info.spl->splwid = width;
    cdsUattr[idx].info.spl->splstr = str;
  

    /*
     *  Build BARB_ELMs - jet barbs - from the input jet,
     *  Save the attr settings before display and restore after display.
     */ 
    el_tmp.hdr = jet.hdr;
    el_tmp.hdr.vg_class = (char)CLASS_WINDS;
    el_tmp.hdr.vg_type = (char)BARB_ELM;
    el_tmp.elem.wnd.info.wndtyp = DEFLTSET;

    cds_getinx ( &el_tmp, &idx, &ier );

    color = cdsUattr[idx].maj_col; 
    size  = cdsUattr[idx].info.wnd->size;
    width = cdsUattr[idx].info.wnd->width;
    type  = cdsUattr[idx].info.wnd->wndtyp;
    
    cdsUattr[idx].maj_col	   = cdsUattr[indx].info.jet->barb[0].wndcol; 
    cdsUattr[idx].info.wnd->size   = cdsUattr[indx].info.jet->barb[0].wnd.info.size;
    cdsUattr[idx].info.wnd->width  = cdsUattr[indx].info.jet->barb[0].wnd.info.width;
    cdsUattr[idx].info.wnd->wndtyp = cdsUattr[indx].info.jet->barb[0].wnd.info.wndtyp;
           
    for ( ii = 0; ii < jet.elem.jet.nbarb; ii++ ) {
        el_tmp.hdr.maj_col = jet.elem.jet.barb[ii].wndcol;
        el_tmp.elem.wnd    = jet.elem.jet.barb[ii].wnd;
	/* Force barb to plot centered at (lat,lon) with filled flags */
        el_tmp.elem.wnd.info.wndtyp = 22;
        cds_dspelm ( &el_tmp, &ier );
    }    
    
    cdsUattr[idx].maj_col          = color; 
    cdsUattr[idx].info.wnd->size   = size;
    cdsUattr[idx].info.wnd->width  = width;
    cdsUattr[idx].info.wnd->wndtyp = type;


    /*
     *  Build/display SPTX_ELMs - jet barb texts - from the input jet,
     *  Save the attr settings before display and restore after display.
     */ 
    el_tmp.hdr = jet.hdr;
    el_tmp.hdr.vg_class = (char)CLASS_TEXT;
    el_tmp.hdr.vg_type = (char)SPTX_ELM;
    el_tmp.elem.spt.info.sptxtyp = DEFLTSET;

    cds_getinx ( &el_tmp, &idx, &ier );

    color = cdsUattr[idx].maj_col; 
    rotn  = cdsUattr[idx].info.spt->text.info.rotn;
    ttype = cdsUattr[idx].info.spt->text.info.sptxtyp;
    align = cdsUattr[idx].info.spt->text.info.ialign;
    size  = cdsUattr[idx].info.spt->text.info.sztext;
    width = cdsUattr[idx].info.spt->text.info.iwidth;
    font  = cdsUattr[idx].info.spt->text.info.itxfn;
    thw   = cdsUattr[idx].info.spt->text.info.ithw;
    turbsym = cdsUattr[idx].info.spt->text.info.turbsym;
  
    cdsUattr[idx].maj_col                     = cdsUattr[indx].info.jet->barb[0].sptcol; 
    cdsUattr[idx].info.spt->text.info.rotn    = cdsUattr[indx].info.jet->barb[0].spt.info.rotn;
    cdsUattr[idx].info.spt->text.info.sptxtyp = cdsUattr[indx].info.jet->barb[0].spt.info.sptxtyp;
    cdsUattr[idx].info.spt->text.info.ialign  = cdsUattr[indx].info.jet->barb[0].spt.info.ialign;
    cdsUattr[idx].info.spt->text.info.sztext  = cdsUattr[indx].info.jet->barb[0].spt.info.sztext;
    cdsUattr[idx].info.spt->text.info.iwidth  = cdsUattr[indx].info.jet->barb[0].spt.info.iwidth;
    cdsUattr[idx].info.spt->text.info.itxfn   = cdsUattr[indx].info.jet->barb[0].spt.info.itxfn;
    cdsUattr[idx].info.spt->text.info.ithw    = cdsUattr[indx].info.jet->barb[0].spt.info.ithw;
    cdsUattr[idx].info.spt->text.info.turbsym = cdsUattr[indx].info.jet->barb[0].spt.info.turbsym;

    for ( ii = 0; ii < jet.elem.jet.nbarb; ii++ ) {
        el_tmp.hdr.maj_col = jet.elem.jet.barb[ii].sptcol;
        el_tmp.elem.spt    = jet.elem.jet.barb[ii].spt;

	/* Force text to plot relative to (lat,lon) with filled flags */
	el_tmp.elem.spt.info.ialign = 0;
        
	/*
	 *  Pass the specified color to plot the text.
	 */
        el_tmp.elem.spt.info.txtcol = jet.elem.jet.barb[ii].sptcol;

	/*
	 *  Pass the specified text type to plot the text.
	 */
	el_tmp.elem.spt.info.sptxtyp = ( cdsUattr[idx].info.spt->text.info.sptxtyp == 0 ) ?
	            el_tmp.elem.spt.info.sptxtyp : cdsUattr[idx].info.spt->text.info.sptxtyp;
        
	cds_dspelm ( &el_tmp, &ier );
    }    
    
    cdsUattr[idx].maj_col		   = color; 
    cdsUattr[idx].info.spt->text.info.rotn    = rotn;
    cdsUattr[idx].info.spt->text.info.sptxtyp = ttype;
    cdsUattr[idx].info.spt->text.info.ialign  = align;
    cdsUattr[idx].info.spt->text.info.sztext  = size;
    cdsUattr[idx].info.spt->text.info.iwidth  = width;
    cdsUattr[idx].info.spt->text.info.itxfn   = font;
    cdsUattr[idx].info.spt->text.info.ithw    = thw;
    cdsUattr[idx].info.spt->text.info.turbsym = turbsym;


    /*
     *  Build/display HASH_ELM - jet hashs - from the input jet. 
     *  Save the attr settings before display and restore after display.
     */ 
    el_tmp.hdr = jet.hdr;
    el_tmp.hdr.vg_class = (char)CLASS_WINDS;
    el_tmp.hdr.vg_type = (char)HASH_ELM;
    el_tmp.elem.wnd.info.wndtyp = DEFLTSET;

    cds_getinx ( &el_tmp, &idx, &ier );

    color = cdsUattr[idx].maj_col; 
    size  = cdsUattr[idx].info.wnd->size;
    width = cdsUattr[idx].info.wnd->width;
    type  = cdsUattr[idx].info.wnd->wndtyp;
    
    cdsUattr[idx].maj_col         = cdsUattr[indx].info.jet->hash[0].wndcol; 
    cdsUattr[idx].info.wnd->size   = cdsUattr[indx].info.jet->hash[0].wnd.info.size;
    cdsUattr[idx].info.wnd->width  = cdsUattr[indx].info.jet->hash[0].wnd.info.width;
    cdsUattr[idx].info.wnd->wndtyp = cdsUattr[indx].info.jet->hash[0].wnd.info.wndtyp;
               
    for ( ii = 0; ii < jet.elem.jet.nhash; ii++ ) {
        el_tmp.hdr.maj_col = jet.elem.jet.hash[ii].wndcol;
        el_tmp.elem.wnd    = jet.elem.jet.hash[ii].wnd;
        cds_dspelm ( &el_tmp, &ier );
    }    

    cdsUattr[idx].maj_col = color; 
    cdsUattr[idx].info.wnd->size = size;
    cdsUattr[idx].info.wnd->width = width;
    cdsUattr[idx].info.wnd->wndtyp = type;

}


