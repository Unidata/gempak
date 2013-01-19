#include "afcmn.h"


/************************************************************************
 * afdisplay.c                                             		*
 *                                                                      *
 * This module contains the subroutines to display the final FROM lines *
 * when in nmap2.							*
 *                                                                      *
 * CONTENTS:                                                            *
 *   public function:                                                   *
 *	af_displayPts		- display AIRMET FROM line		*
 *									*
 *   private function:							*
 *	af_xmlGtag		- get tag value from a xml str		*
 ***********************************************************************/


/*
 *  Private functions
 */
static void af_xmlGtag          ( const char *tag, const char *str, const char *def, 
				  char *value, char **nxt_ptr, int *iret );


/*=====================================================================*/

/*
 * public function
 */
void af_displayPts ( char* xml_str ) 
/************************************************************************
 * af_displayPts                                                        *
 *                                                                      *
 * This routine displays the lat/lon points in the xml string via gline	*
 * and gmark.								*
 *                                                                      *
 * void af_displayPts ( xml_str )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      xml_str		char*		xml string			*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		09/05	initial coding				*
 * H. Zeng/SAIC		10/05	added loop for multi segments		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06	draw each point as a marker		*
 * D.W.Plummer/NCEP	03/07	add mult display colors of FROM points	*
 * D.W.Plummer/NCEP	04/07	correct mult display colors of FROM pts	*
 ***********************************************************************/
{
    int    close_flg, npts, nval, np, iret, one = 1, ii;
    int	   line_typ, line_wid, line_col, typ_flg, wid_flg;
    int    icolrx, iltypx, ilthwx, iwidthx, iwhwx;
    int	   imarkx, imkhwx, imkwidx;
    int	   pcolor, pts_color[3], pts_type, mkhw, mkwid;
    float  *lats, *lons, szmarkx, mksiz;
    int    *reduceFlgPts, plot_special;
    char   tag_val[4096], *str_ptr, *nxt_ptr;
    Boolean	draw_pts; 
/*---------------------------------------------------------------------*/
  
    /*
     * get FROM line type, width, and color choice from prefs.tbl
     */
    ctb_pfstr ( "GFA_LINE_COLOR", tag_val, &iret );
    if ( iret == 0 ) {
      sscanf ( tag_val, "%d", &line_col );
    }
    else {
      line_col = 31;
    }

    ctb_pfstr ( "GFA_LINE_TYPE", tag_val, &iret );
    if ( iret == 0 ) {
      sscanf ( tag_val, "%d", &line_typ );
    }
    else {
      line_typ = 1;
    }

    ctb_pfstr ( "GFA_LINE_WIDTH", tag_val, &iret );
    if ( iret == 0 ) {
      sscanf ( tag_val, "%d", &line_wid );
    }
    else {
      line_wid = 3;
    }

    /*
     * get flag for drawing points, marker type/color choice
     */
    ctb_pfbool( "GFA_AF_DRAWPTS", &draw_pts, &iret );

    if ( draw_pts == G_TRUE ) {
        ctb_pfstr ( "GFA_PTS_COLOR", tag_val, &iret );
        pts_color[0] = 2;
	cst_ilst (tag_val, ';', pts_color[0], 3, pts_color, &nval, &iret );

        ctb_pfstr ( "GFA_PTS_TYPE", tag_val, &iret );
        if ( iret == 0 ) {
            sscanf ( tag_val, "%d", &pts_type );
        }
        else {
            pts_type = 17;
        }
    }


    /*
     * Set the line type flag and line width flag.
     */
    typ_flg = 0;
    wid_flg = 0;

    
    /*
     * save the existing line/marker and color attributes.
     */
    gqcolr ( &icolrx, &iret );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &iret );
    
    if ( draw_pts ) {
        gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &iret );
    }
    
    
    /*
     * set the new line/marker attributes.
     */
    gsline ( &line_typ, &typ_flg, &line_wid, &wid_flg, &iret );

    if ( draw_pts ) {
        mkhw   = 1;
        mksiz  = 1.0F;
        mkwid  = 2;
    
        gsmrkr ( &pts_type, &mkhw, &mksiz, &mkwid, &iret );
    }

    /*
     * Fetch the info from xml str and draw the line(s).
     */
    str_ptr = xml_str;
    nxt_ptr = NULL;
    lats    = NULL;
    lons    = NULL;

    while ( str_ptr != NULL && str_ptr[0] != '\0' ) {

        /*
         * get the close flag info.
         */
        af_xmlGtag ("<closeFlg>", str_ptr, "\0", tag_val, &nxt_ptr, &iret);
        if ( iret != 0 )  break;
        if ( sscanf(tag_val, "%d", &close_flg) != 1 )  break;

        /*
         * get the number of points info.
         */
        str_ptr = nxt_ptr;
        af_xmlGtag ("<nLatLonPts>", str_ptr, "\0", tag_val, &nxt_ptr, &iret);
        if ( iret != 0 )  break;
        if ( sscanf(tag_val, "%d", &npts) != 1 )  break;

        /*
         * Allocate spaces for lats&lons.
         */
        G_MALLOC (lats, float, npts+1, "af_displayPts lats");
        G_MALLOC (lons, float, npts+1, "af_displayPts lons");

        /*
         * get the latitudes info.
         */
        str_ptr = nxt_ptr;
        af_xmlGtag ("<latPts>", str_ptr, "\0", tag_val, &nxt_ptr, &iret);
        if ( iret != 0 )  break;

        cst_rlst (tag_val, ' ', -9999.0, npts, lats, &nval, &iret);
        if ( iret<0 || nval!=npts )  break;

        /*
         * get the longitudes info.
         */
        str_ptr = nxt_ptr;
        af_xmlGtag ("<lonPts>", str_ptr, "\0", tag_val, &nxt_ptr, &iret);
        if ( iret != 0 )  break;

        cst_rlst (tag_val, ' ', -9999.0, npts, lons, &nval, &iret);
        if ( iret<0 || nval!=npts )  break;

        /*
         * check the close flag, repeat the first point if it is set.
         */
        if ( close_flg != 0 ) {

            lats[npts] = lats[0];
            lons[npts] = lons[0];
            np = npts + 1;
        }
        else {
            np = npts;
        }

        gscolr ( &line_col, &iret );
        gline ( sys_M, &np, lats, lons, &iret, strlen(sys_M) );
        
        /*
         *  Draw each point.
         */    

        if ( draw_pts ) {

            /* 
 	     * get the reduceFlgPts info.
 	     */
            str_ptr = nxt_ptr;
            af_xmlGtag ("<reduceFlgPts>", str_ptr, "\0", tag_val, &nxt_ptr, &iret);
            plot_special = ( iret == 0 ) ? G_TRUE : G_FALSE;

	    if ( plot_special )  {
                G_MALLOC (reduceFlgPts, int, npts+1, "af_displayPts reduceFlgPts");
                cst_ilst (tag_val, ' ', pts_color[0], npts, reduceFlgPts, &nval, &iret);
	    }

            for ( ii = 0; ii < npts; ii++ ) {    
                pcolor = ( plot_special == G_TRUE ) ? pts_color[reduceFlgPts[ii]] : pts_color [ 0 ];
                gscolr ( &pcolor, &iret );
                gmark( sys_M, &one, &lats[ii], &lons[ii], &iret, strlen(sys_M));
            }

	    if ( plot_special )  {
                G_FREE (reduceFlgPts, int );
	    }

        }
    
        /*
         * set the str_ptr value forward.
         */
        str_ptr = nxt_ptr;  

        /*
         * Free spaces for lats&lons.
         */
        G_FREE ( lats, float );
        G_FREE ( lons, float );

    } /* the end of while ( str_ptr != ... */


    /*
     * Free spaces for lats&lons.
     */
    G_FREE ( lats, float );
    G_FREE ( lons, float );

    if ( draw_pts ) {
        gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &iret );
    }
    
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &iret );
    gscolr ( &icolrx, &iret );
    
}

/*=====================================================================*/

static void af_xmlGtag ( const char *tag, const char *str, const char *def, 
			 char *value, char **nxt_ptr, int *iret )
/************************************************************************
 * af_xmlGtag								*
 *									*
 * This subroutine gets a tag value from a xml string containing many   *
 * tags. If the tag name is not found, the default value is returned    *
 * and the return code is set to 3. If there are more than one tag of   *
 * the same name in the str, the value of the first one is returned.	*
 *									*
 * af_xmlGtag ( tag, str, def, value, nxt_ptr, iret )			*
 *									*
 * Input parameters:							*
 *	*tag		char		opening tag name		*
 *	*str		char		xml String to search		*
 *	*def		char		returned value if tag not found	*
 *									*
 * Output parameters:							*
 *	*value		char		returned value of tag		*
 *	**nxt_ptr	char		ptr to the char following the   *
 *					closing tag			*
 *	*iret		int		return code			*
 *					  0 = normal, value found	*
 *					  3 = not found, default value  *
 *					      returned			*
 **									*
 * Log:									*
 * H. Zeng/SAIC		09/05	initial coding				*
 * H. Zeng/SAIC		10/05	added a new parameter			*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    char  close_tag[256], *ptr1=NULL, *ptr2=NULL;         
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Check if values of tag & str are valid.
     */
    if ( tag == NULL || tag[0] == '\0' || 
         str == NULL || str[0] == '\0'    ) {

      strcpy (value, def);
      (*nxt_ptr) = NULL;
      *iret = 3;
      return;
    }

    /*
     * construct closing tag from tag str.
     */
    strcpy (close_tag, "</");
    strcat (close_tag, tag+1);

    /*
     * get the positions of opening tag and closing tag.
     * if failed, return.
     */
    ptr1 = strstr (str, tag);
    ptr2 = strstr (str, close_tag);

    if ( ptr1==NULL || ptr2==NULL || ptr2 <= ptr1 ) {

      strcpy (value, def);
      (*nxt_ptr) = NULL;
      *iret = 3;
      return;
    }

    /*
     * get the tag value.
     */
    memcpy ((void*)value, (void*)(ptr1+strlen(tag)), 
	    (size_t)(ptr2-ptr1-strlen(tag)) );
    value[ptr2-ptr1-strlen(tag)] = '\0';

    (*nxt_ptr) = ptr2 + strlen(close_tag);

}

/*=====================================================================*/


