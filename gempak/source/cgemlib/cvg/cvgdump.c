#include "cvgcmn.h"
#include "drwids.h"

void cvg_dump ( VG_DBStruct el, int size, int nout, int flag, int *iret )
/************************************************************************
 * cvg_dump								*
 *									*
 * This function dumps the contents of an element from a VG file.  The	*
 * function can also return decoded header information.			*
 *									*
 * cvg_dump ( el, size, nout, flag, iret )				*
 *									*
 * Input parameters:							*
 *	el		VG_DBStruct	VG record structure		*
 *	size		int		Number of bytes in VG record	*
 *	nout		int		Number of bytes to output	*
 *	flag		int		Dump actual contents of element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					-11 = # bytes to output too big	*
 *					-24 = no VG header loaded	*
 **									*
 * Log:									*
 * D. Keiser/GSC	 1/97	Copied from UTF_DUMP			*
 * D. Keiser/GSC	 3/97	Added several new types			*
 * D. Keiser/GSC	 4/97	Added special lines			*
 * E. Safford/GSC	 6/97	Added special text			*
 * E. Wehner/EAi	 6/97	Added file header element		*
 * E. Safford/GSC	 7/97	Updated special text 			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * F.J.Yen/NCEP	 	10/97	Corrected spelling; added fill and 	*
 *				closure					*
 * S. Jacobs/NCEP	 3/98	Added to print value of fill pattern	*
 * I. Durham/GSC	 4/98	Added DARR_ELM and HASH_ELM		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/GSC         10/98   Added CMBSY_ELM                         *
 * T. Piper/GSC		10/98	Prolog update				*
 * A. Hardy/GSC         12/98   Added CIRCLE_ELM                        *
 * S. Jacobs/NCEP	 5/99	Added TRKSTORM_ELM			*
 * G. Krueger/EAI	 5/99	Modified circles for latlon array	*
 * S. Law/GSC		07/99	Added SIGINTL_ELM			*
 * S. Law/GSC		08/99	Added remaining SIGMETs			*
 * D.W.Plummer/NCEP	12/99	Added new watch format and status info	*
 * D.W.Plummer/NCEP	 1/00	Added watch county information		*
 * S. Law/GSC		02/00	Added SIGCCF_ELM			*
 * F. J. Yen/NCEP	08/00	Updated SIGMETs; Added unused1 & unused2*
 * D.W.Plummer/NCEP	10/00	Changes for new watch box element	*
 * M. Li/GSC		10/00	Added text font & size for TRACK	*
 * J. Wu/GSC		11/00	Removed unused CONTOUR_ELM case	        *
 * J. Wu/GSC		02/01	Corrected spd/dir output format in CCF 	*
 * J. Wu/GSC		02/01	Modified 'unused1' & 'unused2' to   	*
 *			        'smooth' & 'version' 			*
 * D.W.Plummer/NCEP	 8/01	Add watch FIPS code dump		*
 * J. Wu/SAIC		10/01	small change to output format   	*
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * J. Wu/SAIC		09/02	add CLASS_LIST			 	*
 * J. Wu/SAIC		11/02	revise LIST structure		 	*
 * H. Zeng/XTRIA        01/03   added WatchBox marker info.             *
 * D.W.Plummer/NCEP	06/03	added ASHCLD_ELM and VOLC_ELM		*
 * J. Wu/SAIC		09/03	add CLASS_MET -> JET_ELM		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * H. Zeng/XTRIA	11/03   added fields for ASHCLD&VOLC		*
 * J. Wu/SAIC		01/04	add CLASS_MET -> GFA_ELM		*
 * J. Wu/SAIC		02/04	dump text location in GFA_ELM		*
 * B. Yin/SAIC		02/04	Added CLASS_MET -> TCA_ELM		*
 * B. Yin/SAIC		03/04	Added storm type in tca structure	*
 * B. Yin/SAIC		05/04	Modified tca storm type and basin output*
 * J. Wu/SAIC		05/04	add weather type description to GFA_ELM	*
 * B. Yin/SAIC		07/04	Modified to dump more than 2 bkpts	*
 * E. Saffors/SAIC	09/04	update for gfa top/bottom to char array *
 * J. Wu/SAIC		10/04	use cvg_getFld to access GFA attributes	*
 * B. Yin/SAIC		11/04	Modified to dump GFAs with any tags	*
 * B. Yin/SAIC		12/04	Added timezone for TCAs			*
 * B. Yin/SAIC		04/05	Added issueStatus for TCA, removed year	*
 * S. Gilbert/NCEP      11/05   Added text_lat, text_lon, text_font,    *
 *                              text_size, and text_width for TCA       *
 * S. Gilbert/NCEP      01/06   Changed format specifier for advisoryNum*
 * m.gamazaychikov/SAIC	05/07	Add TCE, TCB and TCT elements		*
 * m.gamazaychikov/SAIC	10/08	Modified dump of TCT elements		*
 * L. Hinson/AWC        01/12   Add CLASS_MET -> SGWX_ELM               *
 * S. Jacobs/NCEP	 3/13	Added Post-Trop Cycl for a type of TCA	*
 * M. Onderlinde/NHC     9/16   Added Potential Trop Cycl for a TCA     *
 ***********************************************************************/
{
    int			ii, i, j, ier, counter;
    int			maxlen, ncnty, nret;
    char		grp[4], string[2], info[128], *gfaStr;
    char		tag[ STD_STRLEN ], value[ STD_STRLEN ];
    char		*ptr1, *ptr2;
    unsigned char	*buffer, *outpos, *start;
    float		lat, lon;
/*---------------------------------------------------------------------*/
    *iret = 0;
    strcpy(grp, "CVG");
    strcpy(string, " ");
    i = 0;

/*
**  Dump the contents of the element. 
*/
    if ( nout > size )
	*iret = -11;
    else if ( el.hdr.recsz == 0 )
	*iret = -24;
    else {
	if ( flag ) {
	    buffer = (unsigned char *) &el;
	    start = buffer;
	    outpos = buffer + nout;
	    printf ( "\n\n" );
	    while ( start < outpos ) {
		printf ("byte# %5i decimal %5u hex%5x\n",
						i, *start, *start);
		start++;
		i++;
	    }
	}
	printf ( "\n\n" );
	printf ( "VG HEADER INFORMATION:\n\n" );
	if ( el.hdr.delete == 0 )
	    printf ( "Deletion flag: FALSE\n" );
	else
	    printf ( "Deletion flag: TRUE\n" );
	if ( el.hdr.vg_type == LINE_ELM )
	    printf ( "Vector graphic type: LINE\n" );
	else if ( el.hdr.vg_type == FRONT_ELM )
	    printf ( "Vector graphic type: FRONT\n" );
	else if ( el.hdr.vg_type == CIRCLE_ELM )
	    printf ( "Vector graphic type: CIRCLE\n" );
	else if ( el.hdr.vg_type == WXSYM_ELM )
	    printf ( "Vector graphic type: WEATHER SYMBOL\n" );
	else if ( el.hdr.vg_type == WBOX_ELM )
	    printf ( "Vector graphic type: WATCH BOX\n" );
	else if ( el.hdr.vg_type == BARB_ELM )
	    printf ( "Vector graphic type: WIND BARB\n" );
	else if ( el.hdr.vg_type == ARROW_ELM )
	    printf ( "Vector graphic type: WIND ARROW\n" );
	else if ( el.hdr.vg_type == DARR_ELM )
	    printf ( "Vector graphic type: DIRECTIONAL ARROW\n" );
	else if ( el.hdr.vg_type == HASH_ELM )
	    printf ( "Vector graphic type: HASH MARK\n" );
	else if ( el.hdr.vg_type == CTSYM_ELM )
	    printf ( "Vector graphic type: CLOUD SYMBOL\n" );
	else if ( el.hdr.vg_type == ICSYM_ELM )
	    printf ( "Vector graphic type: ICING SYMBOL\n" );
	else if ( el.hdr.vg_type == PTSYM_ELM )
	    printf ( "Vector graphic type: PRESSURE TENDENCY SYMBOL\n" );
	else if ( el.hdr.vg_type == PWSYM_ELM )
	    printf ( "Vector graphic type: PAST WEATHER SYMBOL\n" );
	else if ( el.hdr.vg_type == SKSYM_ELM )
	    printf ( "Vector graphic type: SKY COVER SYMBOL\n" );
	else if ( el.hdr.vg_type == SPSYM_ELM )
	    printf ( "Vector graphic type: SPECIAL SYMBOL\n" );
	else if ( el.hdr.vg_type == TBSYM_ELM )
	    printf ( "Vector graphic type: TURBULENCE SYMBOL\n" );
	else if ( el.hdr.vg_type == TEXT_ELM )
	    printf ( "Vector graphic type: TEXT\n" );
	else if ( el.hdr.vg_type == TEXTC_ELM )
	    printf ( "Vector graphic type: CENTERED TEXT\n" );
	else if ( el.hdr.vg_type == MARK_ELM )
	    printf ( "Vector graphic type: MARKER\n" );
	else if ( el.hdr.vg_type == SPLN_ELM )
	    printf ( "Vector graphic type: SPECIAL LINE\n" );
	else if ( el.hdr.vg_type == SPTX_ELM )
	    printf ( "Vector graphic type: SPECIAL TEXT\n" );
	else if ( el.hdr.vg_type == FILEHEAD_ELM)
	    printf ("Vector graphic type:  FILE HEADER \n");
	else if ( el.hdr.vg_type == CMBSY_ELM)
	    printf ("Vector graphic type:  COMBINATION SYMBOL \n");
	else if ( el.hdr.vg_type == TRKSTORM_ELM)
	    printf ("Vector graphic type:  STORM TRACK \n");
	else if ( el.hdr.vg_type == SIGAIRM_ELM)
	    printf ("Vector graphic type:  AIRMET \n");
	else if ( el.hdr.vg_type == SIGCONV_ELM)
	    printf ("Vector graphic type:  CONVECTIVE SIGMET \n");
	else if ( el.hdr.vg_type == SIGINTL_ELM)
	    printf ("Vector graphic type:  INTERNATIONAL SIGMET \n");
	else if ( el.hdr.vg_type == SIGNCON_ELM)
	    printf ("Vector graphic type:  NON-CONVECTIVE SIGMET \n");
	else if ( el.hdr.vg_type == SIGOUTL_ELM)
	    printf ("Vector graphic type:  CONVECTIVE OUTLOOK \n");
	else if ( el.hdr.vg_type == SIGCCF_ELM)
	    printf ("Vector graphic type:  CONVECTIVE FORECAST \n");
	else if ( el.hdr.vg_type == LIST_ELM)
	    printf ("Vector graphic type:  LIST \n");
	else if ( el.hdr.vg_type == VOLC_ELM)
            printf ("Vector graphic type:  VAA VOLCANO \n");
        else if ( el.hdr.vg_type == ASHCLD_ELM)
            printf ("Vector graphic type:  VAA ASH CLOUD \n");
	else if ( el.hdr.vg_type == JET_ELM)
	    printf ("Vector graphic type:  JET \n");
	else if ( el.hdr.vg_type == GFA_ELM)
	    printf ("Vector graphic type:  GFA \n");
        else if ( el.hdr.vg_type == SGWX_ELM)
            printf ("Vector graphic type: SGWX \n");
	else if ( el.hdr.vg_type == TCA_ELM)
	    printf ("Vector graphic type:  TCA \n");
	else if ( el.hdr.vg_type == TCERR_ELM)
	    printf ("Vector graphic type:  TCE \n");
	else if ( el.hdr.vg_type == TCTRK_ELM)
	    printf ("Vector graphic type:  TCT \n");
	else if ( el.hdr.vg_type == TCBKL_ELM)
	    printf ("Vector graphic type:  TCB \n");
	else
	    printf ("Vector graphic type:  UNKNOWN\n");

	printf ( "Major color: %d\n", el.hdr.maj_col );
	printf ( "Minor color: %d\n", el.hdr.min_col );
	
	printf ( "Smooth: %d\n", el.hdr.smooth );
	printf ( "Version: %d\n", el.hdr.version );
	
	if ( el.hdr.vg_type == LINE_ELM || el.hdr.vg_type == SPLN_ELM ) {
	    if ( el.hdr.filled == 0 ) {
	      printf ( "Filled flag: FALSE\n" );
	    }
	    else {
	      printf ( "Filled flag: TRUE (value=%d)\n", el.hdr.filled );
	    }
	    if ( el.hdr.closed == 0 ) {
	      printf ( "Closure flag: FALSE\n" );
	    }
	    else {
	      printf ( "Closure flag: TRUE\n" );
	    }
	}
	printf ( "VG record size: %d bytes\n", el.hdr.recsz );
	printf ( "Range:\n" );
	printf ( "  Minimum lat/lon point: %f %f\n",
			el.hdr.range_min_lat, el.hdr.range_min_lon );
	printf ( "  Maximum lat/lon point: %f %f\n",
			el.hdr.range_max_lat, el.hdr.range_max_lon );
	printf (" Group type:	%i	Group Number:	%i \n",
			el.hdr.grptyp, el.hdr.grpnum);

	printf ( "\n\nVG ELEMENT INFORMATION:\n\n" );
	if ( el.hdr.vg_type == FRONT_ELM ) {
	    printf ( "Number of points along front: %d\n",
					el.elem.frt.info.numpts );
	    printf ( "Front code: %d\n", el.elem.frt.info.fcode );
	    printf ( "Front pip size: %d\n", el.elem.frt.info.fpipsz );
	    printf ( "Front pip stroke: %d\n", el.elem.frt.info.fpipst );
	    printf ( "Front pip direction: %d\n",
						el.elem.frt.info.fpipdr);
	    printf ( "Front label: %s\n", el.elem.frt.info.frtlbl );
	    printf ( "Front lat/lon points:\n" );
	    for ( j = 0; j < el.elem.frt.info.numpts; j++ )
		printf ( "	%f	%f\n", el.elem.frt.latlon[j],
			el.elem.frt.latlon[j+el.elem.frt.info.numpts] );
	}
	else if ( el.hdr.vg_type == WBOX_ELM ) {
	    printf ( "Number of points along watch box: %d\n",
					el.elem.wbx.info.numpts );
	    printf ( "Watch box number: %d\n",
					el.elem.wbx.info.w_number );
	    printf ( "Watch box file: %s\n", el.elem.wbx.info.w_file );
	    printf ( "Watch box type:  %d    ",
					el.elem.wbx.info.w_type );
	    switch ( el.elem.wbx.info.w_type )  {
		case	TRWWTCH	:
		    printf ( "Thunderstorm watch\n" );	break;
		case	TORWTCH	:
		    printf ( "Tornado watch\n" );	break;
		default	:
		    printf ( "- undefined -\n" );
	    }
	    printf ( "Watch box style: %d    ",
					el.elem.wbx.info.w_style );
	    switch ( el.elem.wbx.info.w_style )  {
		case	WBC	:
		    printf ( "Watch-by-county\n" );	break;
		case	PGRAM	:
		    printf ( "Parallelogram\n" );	break;
		default	:
		    printf ( "- undefined -\n" );
	    }
            printf ( "County marker type: %6d\n", el.elem.wbx.info.w_mrktyp );
            printf ( "County marker size: %6.2f\n", el.elem.wbx.info.w_mrksiz);
            printf ( "County marker width:%6d\n", el.elem.wbx.info.w_mrkwid );
	    printf ( "Watch box lat/lon points:\n" );
	    for ( j = 0; j < el.elem.wbx.info.numpts; j++ )
		printf ( "	%f	%f\n", el.elem.wbx.latlon[j],
			el.elem.wbx.latlon[j+el.elem.wbx.info.numpts] );
	    /*	*/
	    printf ( "Watch anchor pt #1 id: %s\n", el.elem.wbx.info.w_a0id );
	    printf ( "Watch anchor pt #1 lt: %6.2f\n", el.elem.wbx.info.w_a0lt );
	    printf ( "Watch anchor pt #1 ln: %6.2f\n", el.elem.wbx.info.w_a0ln );
	    printf ( "Watch anchor pt #1 dis: %d\n", el.elem.wbx.info.w_a0dis );
	    printf ( "Watch anchor pt #1 dir: %s\n", el.elem.wbx.info.w_a0dir );
	    printf ( "Watch anchor pt #2 id: %s\n", el.elem.wbx.info.w_a1id );
	    printf ( "Watch anchor pt #2 lt: %6.2f\n", el.elem.wbx.info.w_a1lt );
	    printf ( "Watch anchor pt #2 ln: %6.2f\n", el.elem.wbx.info.w_a1ln );
	    printf ( "Watch anchor pt #2 dis: %d\n", el.elem.wbx.info.w_a1dis );
	    printf ( "Watch anchor pt #2 dir: %s\n", el.elem.wbx.info.w_a1dir );
	    /*	*/
	    printf ( "WATCH ISSUE INFORMATION:\n" );
	    printf ( "Issue status: %d\n", el.elem.wbx.info.w_istat );
	    printf ( "Start time: %s\n", el.elem.wbx.info.w_iss_t );
	    printf ( "Expiration time: %s\n", el.elem.wbx.info.w_exp_t );
	    printf ( "Severity: %d\n", el.elem.wbx.info.w_severity );
	    printf ( "Time Zone: %s\n", el.elem.wbx.info.w_timezone );
	    printf ( "Hail Size: %s\n", el.elem.wbx.info.w_hailsz );
	    printf ( "Gusts: %s\n", el.elem.wbx.info.w_windg );
	    printf ( "Tops: %s\n", el.elem.wbx.info.w_tops );
	    printf ( "MSMV direction: %s\n", el.elem.wbx.info.w_msmv_d );
	    printf ( "MSMV speed: %s\n", el.elem.wbx.info.w_msmv_s );
	    printf ( "States: %s\n", el.elem.wbx.info.w_states );
	    printf ( "Adjacent Areas: %s\n", el.elem.wbx.info.w_adjarea );
	    printf ( "Replacement watches: %s\n", el.elem.wbx.info.w_replw );
	    printf ( "Forecaster(s): %s\n", el.elem.wbx.info.w_fcstr );
	    printf ( "Issued flag: %d\n", el.elem.wbx.info.w_issued );
	    /*	*/
	    printf ( "WATCH STATUS MESSAGE INFORMATION:\n" );
	    printf ( "WSM start time: %s\n", el.elem.wbx.info.wsm_iss_t );
	    printf ( "WSM expiration time: %s\n", el.elem.wbx.info.wsm_exp_t );
	    printf ( "WSM reference dirctn: %s\n", el.elem.wbx.info.wsm_ref );
	    printf ( "WSM from line: %s\n", el.elem.wbx.info.wsm_from );
	    printf ( "WSM meso: %s\n", el.elem.wbx.info.wsm_meso );
	    printf ( "WSM forecaster(s): %s\n", el.elem.wbx.info.wsm_fcstr );
	    /*	*/
	    printf ( "WATCH COUNTY INFORMATION:\n" );
	    ncnty = el.elem.wbx.info.numcnty;
	    printf ( "Number of counties: %d\n", ncnty );
	    printf ( "County Location - Info:\n" );
	    maxlen = sizeof( info );
	    clo_init ( &ier );
	    for ( ii = 0; ii < ncnty; ii++ )  {
		lat = el.elem.wbx.info.cn_ltln[ii];
		lon = el.elem.wbx.info.cn_ltln[ii+ncnty];
		clo_findnum ( "COUNTY", el.elem.wbx.info.cn_fips[ii],
			maxlen, &nret, info, &ier );
		printf("%3d - LOC=%6.2f,%6.2f, FIPS=%d\n\tINFO:%s\n", 
			ii, lat, lon,
			el.elem.wbx.info.cn_fips[ii], 
			info );
	    }
	}
	else if ( ( el.hdr.vg_type == WXSYM_ELM ) ||
		  ( el.hdr.vg_type == ICSYM_ELM ) ||
		  ( el.hdr.vg_type == CTSYM_ELM ) ||
		  ( el.hdr.vg_type == PTSYM_ELM ) ||
		  ( el.hdr.vg_type == PWSYM_ELM ) ||
		  ( el.hdr.vg_type == SKSYM_ELM ) ||
		  ( el.hdr.vg_type == SPSYM_ELM ) ||
		  ( el.hdr.vg_type == TBSYM_ELM ) ||
		  ( el.hdr.vg_type == MARK_ELM )  ||
		  ( el.hdr.vg_type == CMBSY_ELM ) ) {
	    printf ( "Number of symbols: %d\n", el.elem.sym.info.numsym);
	    printf ( "Symbol width: %d\n", el.elem.sym.info.width );
	    printf ( "Symbol size: %f\n", el.elem.sym.info.size );
	    printf ( "Symbol ityp: %d\n", el.elem.sym.info.ityp );

	    printf ( "Symbol code(s), lat/lon point(s), offset(s):\n" );
	    for ( j = 0; j < el.elem.sym.info.numsym; j++ ) {
		printf ( "\t%f\t%f\t%f\t%d\t%d\n",
			 el.elem.sym.data.code[j],
			 el.elem.sym.data.latlon[j],
		 el.elem.sym.data.latlon[j+el.elem.sym.info.numsym],
			 el.elem.sym.data.offset_xy[j],
		 el.elem.sym.data.offset_xy[j+el.elem.sym.info.numsym] );
	    }
	}
	else if ( ( el.hdr.vg_type == BARB_ELM ) ||
		  ( el.hdr.vg_type == ARROW_ELM ) ) {
	    printf ( "Number of wind symbols: %d\n",
						el.elem.wnd.info.numwnd);
	    printf ( "Wind width: %d\n", el.elem.wnd.info.width );
	    printf ( "Wind size: %f\n", el.elem.wnd.info.size );
	    printf ( "Wind attribute type: %d\n",
						el.elem.wnd.info.wndtyp);
	    printf ( "Wind arrow head size: %f\n",
						el.elem.wnd.info.hdsiz );

	    printf ( "Lat/lon point(s), speed(s), direction(s):\n" );
	    for ( j = 0; j < el.elem.wnd.info.numwnd; j++ ) {
		printf ( "\t%f\t%f\t%f\t%f\n",
		 el.elem.wnd.data.latlon[j],
		 el.elem.wnd.data.latlon[j+el.elem.wnd.info.numwnd],
		 el.elem.wnd.data.spddir[j],
		 el.elem.wnd.data.spddir[j+el.elem.wnd.info.numwnd] );
	    }
	}
	else if ( el.hdr.vg_type == DARR_ELM ) {
	    printf ( "Number of wind symbols: %d\n",
						el.elem.wnd.info.numwnd);
	    printf ( "Wind width: %d\n", el.elem.wnd.info.width );
	    printf ( "Wind size: %f\n", el.elem.wnd.info.size );
	    printf ( "Wind attribute type: %d\n",
						el.elem.wnd.info.wndtyp);
	    printf ( "Wind arrow head size: %f\n",
						el.elem.wnd.info.hdsiz );

	    printf ( "Lat/lon point(s), direction(s):\n" );
	    for ( j = 0; j < el.elem.wnd.info.numwnd; j++ ) {
		printf ( "\t%f\t%f\t%f\n",
		 el.elem.wnd.data.latlon[j],
		 el.elem.wnd.data.latlon[j+el.elem.wnd.info.numwnd],
		 el.elem.wnd.data.spddir[j+el.elem.wnd.info.numwnd] );
	    }
	}
	else if ( el.hdr.vg_type == HASH_ELM ) {
	    printf ( "Number of hash symbols: %d\n",
						el.elem.wnd.info.numwnd);
	    printf ( "Hash line width: %d\n", el.elem.wnd.info.width );
	    printf ( "Hash size: %f\n", el.elem.wnd.info.size );
	    printf ( "Hash spacing: %d\n", el.elem.wnd.info.wndtyp);

	    printf ( "Lat/lon point(s), direction(s):\n" );
	    for ( j = 0; j < el.elem.wnd.info.numwnd; j++ ) {
		printf ( "\t%f\t%f\t%f\n",
		 el.elem.wnd.data.latlon[j],
		 el.elem.wnd.data.latlon[j+el.elem.wnd.info.numwnd],
		 el.elem.wnd.data.spddir[j+el.elem.wnd.info.numwnd] );
	    }
	}
	else if ( el.hdr.vg_type == LINE_ELM ) {
	    printf ( "Number of points along line: %d\n",
					el.elem.lin.info.numpts );
	    printf ( "Line type: %d\n", el.elem.lin.info.lintyp );
	    printf ( "Line type flag: %d\n", el.elem.lin.info.lthw );
	    printf ( "Line width size multiplier: %d\n",
						el.elem.lin.info.width );
	    printf ( "Line width flag: %d\n", el.elem.lin.info.lwhw );
	    printf ( "Line lat/lon points:\n" );
	    for ( j = 0; j < el.elem.lin.info.numpts; j++ )
		printf ( "	%f	%f\n", el.elem.lin.latlon[j],
			el.elem.lin.latlon[j+el.elem.lin.info.numpts] );
	}
	else if ( ( el.hdr.vg_type == TEXT_ELM ) ||
		  ( el.hdr.vg_type == TEXTC_ELM ) ) {
	    printf ( "Text size: %f\n", el.elem.txt.info.sztext );
	    printf ( "Text font: %d\n", el.elem.txt.info.itxfn );
	    printf ( "Text rotation: %f\n", el.elem.txt.info.rotn );
	    printf ( "Text width: %d\n", el.elem.txt.info.iwidth );
	    printf ( "Text HW flag: %d\n", el.elem.txt.info.ithw );
	    printf ( "Text justification: %d\n",el.elem.txt.info.ialign);
	    printf ( "Text offsets: %d  %d\n",
			el.elem.txt.info.offset_x,
			el.elem.txt.info.offset_y );
	    printf ( "Text location: %f  %f\n",
			el.elem.txt.info.lat,
			el.elem.txt.info.lon );
	    printf ( "Text string: >%s<\n\n", el.elem.txt.text );
	}
	else if ( el.hdr.vg_type == SPLN_ELM ) {
	    printf ( "Number of points along special line: %d\n",
					el.elem.spl.info.numpts );
	    printf ("Special line type: %d", el.elem.spl.info.spltyp );
	    if ( el.elem.spl.info.spltyp == 1 )
		printf ( "  Ball and Chain\n" );
	    else if ( el.elem.spl.info.spltyp == 2 )
		printf ( "  Zig Zag or Sawtooth\n" );
	    else if ( el.elem.spl.info.spltyp == 3 )
		printf ( "  Scallop\n" );
	    else if ( el.elem.spl.info.spltyp == 4 )
		printf ( "  Arrow\n" );
	    else
		printf ( "\n" );
	    
	    printf ( "Special line stroke multiplier: %d\n",
					el.elem.spl.info.splstr );
	    printf ( "Special line direction indicator: %d\n",
					el.elem.spl.info.spldir );
	    printf ( "Special line size: %f\n",
					el.elem.spl.info.splsiz );
	    printf ( "Special line width: %d\n",
					el.elem.spl.info.splwid );
	    printf ( "Line lat/lon points:\n" );
	    for ( j = 0; j < el.elem.spl.info.numpts; j++ )
		printf ( "	%f	%f\n", el.elem.spl.latlon[j],
			el.elem.spl.latlon[j+el.elem.spl.info.numpts] );
	}
	else if ( el.hdr.vg_type == SPTX_ELM ) { 
	    printf ( "Special text type: %d\n", el.elem.spt.info.sptxtyp);
	    printf ( "Special turbulence symbol: %d\n", 
						el.elem.spt.info.turbsym);
	    printf ( "Special text size: %f\n", el.elem.spt.info.sztext );
	    printf ( "Special text font: %d\n", el.elem.spt.info.itxfn );
	    printf ( "Special text width: %d\n", el.elem.spt.info.iwidth );
	    printf ( "Special text rotation: %f\n", el.elem.spt.info.rotn);
	    printf ( "Special text alignment: %d\n", el.elem.spt.info.ialign);
	    printf ( "Special text X offset: %d\n", el.elem.spt.info.offset_x);
	    printf ( "Special text Y offset: %d\n", el.elem.spt.info.offset_y);
	    printf ( "Special text HW flag: %d\n", el.elem.spt.info.ithw );
	    printf ( "Special text color: %d\n", el.elem.spt.info.txtcol);
	    printf ( "Special text fill color: %d\n", 
						el.elem.spt.info.filcol);
	    printf ( "Special text line color: %d\n", 
						el.elem.spt.info.lincol);
	    printf ( "Special text location: %f  %f\n",
			el.elem.spt.info.lat,
			el.elem.spt.info.lon );
	    printf ( "Text string: >%s<\n\n", el.elem.spt.text );
	}
	else if ( el.hdr.vg_type == CIRCLE_ELM ) {
	    printf ( "Number of points for circle radius: %d\n",
					el.elem.cir.info.numpts );
	    printf ( "Circle type: %d\n", el.elem.cir.info.lintyp );
	    printf ( "Circle type flag: %d\n", el.elem.cir.info.lthw );
	    printf ( "Circle width size multiplier: %d\n",
						el.elem.cir.info.width );
	    printf ( "Circle width flag: %d\n\n", el.elem.cir.info.lwhw );
	    printf ( "Circle x center point: %f\n", 
	                                      el.elem.cir.data.latlon[0]);
	    printf ( "Circle y center point: %f\n", 
	                                      el.elem.cir.data.latlon[2]);
	    printf ( "Circle x circumference point: %f\n", 
	                                      el.elem.cir.data.latlon[1]);
	    printf ( "Circle y circumference point: %f\n", 
	                                      el.elem.cir.data.latlon[3]);
        }
	else if ( el.hdr.vg_type == TRKSTORM_ELM ) {
	    printf ( "Type of storm track: %d\n",
					el.elem.trk.info.subtype );
	    printf ( "Number of initial points along line: %d\n",
					el.elem.trk.info.nipts );
	    printf ( "Total number of points along line: %d\n",
					el.elem.trk.info.npts );
	    printf ( "Initial line type: %d\n",
					el.elem.trk.info.ltype1 );
	    printf ( "Extrapolated line type: %d\n",
					el.elem.trk.info.ltype2 );
	    printf ( "Line width size multiplier: %d\n",
					el.elem.trk.info.width );
	    printf ( "Initial marker type: %d\n",
					el.elem.trk.info.mtype1 );
	    printf ( "Extrapolated marker type: %d\n",
					el.elem.trk.info.mtype2 );
	    printf ( "Extrapolated time increment: %d\n",
					el.elem.trk.info.incr );
	    printf ( "Storm speed: %f\n", el.elem.trk.info.speed );
	    printf ( "Storm direction: %f\n", el.elem.trk.info.dir );
	    printf ( " STORM label text font: %d\n", el.elem.trk.info.itxfn);
	    printf ( " STORM label text size: %f\n", el.elem.trk.info.sztext);
	    printf ( "Line lat/lon points and times:\n" );
	    for ( j = 0; j < el.elem.trk.info.npts; j++ )
		printf ( "	%f	%f       %s\n",
			el.elem.trk.latlon[j],
			el.elem.trk.latlon[j+el.elem.trk.info.npts],
			el.elem.trk.info.times[j]);
	}
	else if ( el.hdr.vg_type == SIGCCF_ELM ) {
	    printf ( "Type of CCF: %d\n",
					el.elem.ccf.info.subtype );
	    printf ( "Number of points: %d\n",
					el.elem.ccf.info.npts );
	    printf ( "Coverage: %d\n",
					el.elem.ccf.info.cover );
	    printf ( "Tops: %d\n",
					el.elem.ccf.info.tops );
	    printf ( "Probability: %d\n",
					el.elem.ccf.info.prob );
	    printf ( "Growth: %d\n",
					el.elem.ccf.info.growth );
	    printf ( "Speed: %4.0f\n",
					el.elem.ccf.info.spd );
	    printf ( "Direction: %4.0f\n",
					el.elem.ccf.info.dir );
	    printf ( "Line lat/lon points and times:\n" );
	    for ( j = 0; j < el.elem.ccf.info.npts; j++ )
		printf ( "	%f	%f\n",
			el.elem.ccf.latlon[j],
			el.elem.ccf.latlon[j+el.elem.ccf.info.npts]);
	}
        else if ( el.hdr.vg_type == ASHCLD_ELM ) {
            printf ( "Type of VAA Ash Cloud: %d\n",
                                        el.elem.ash.info.subtype );
            printf ( "Number of points: %d\n",
                                        el.elem.ash.info.npts );
            printf ( "Distance: %f\n",
                                        el.elem.ash.info.distance );
            printf ( "Forecast hour: %d\n",
                                        el.elem.ash.info.fhr );
            printf ( "Line type: %d\n",
                                        el.elem.ash.info.lintyp );
            printf ( "Line width: %d\n",
                                        el.elem.ash.info.linwid );
            printf ( "Speed: %s\n",
                                        el.elem.ash.info.spds);
            printf ( "Direction: %s\n",
                                        el.elem.ash.info.dir );
            printf ( "Flight Level 1: %s\n",
                                        el.elem.ash.info.flvl1 );
            printf ( "Flight Level 2: %s\n",
                                        el.elem.ash.info.flvl2 );
            printf ( "Ash cloud lat/lon points:\n" );
            for ( j = 0; j < el.elem.ash.info.npts; j++ )
                printf ( "      %f      %f\n",
                        el.elem.ash.latlon[j],
                        el.elem.ash.latlon[j+el.elem.ash.info.npts]);
        }
        else if ( el.hdr.vg_type == VOLC_ELM ) {
            printf ( "Name of Volcano: %s\n",
                                        el.elem.vol.info.name );
            printf ( "Symbol code: %f\n",
                                        el.elem.vol.info.code );
            printf ( "Symbol size: %f\n",
                                        el.elem.vol.info.size );
            printf ( "Symbol width: %d\n",
                                        el.elem.vol.info.width );
            printf ( "Volcano number: %s\n",
                                        el.elem.vol.info.number );
            printf ( "Volcano location: %s\n",
                                        el.elem.vol.info.location );
            printf ( "Volcano area: %s\n",
                                        el.elem.vol.info.area );
            printf ( "Volcano elevation: %s\n",
                                        el.elem.vol.info.elev );
            printf ( "Originating Station: %s\n",
                                        el.elem.vol.info.origstn );
            printf ( "VAAC: %s\n",
                                        el.elem.vol.info.vaac );
            printf ( "WMO ID: %s\n",
                                        el.elem.vol.info.wmoid );
            printf ( "Header number: %s\n",
                                        el.elem.vol.info.hdrnum );
            printf ( "Year: %s\n",
                                        el.elem.vol.info.year );
            printf ( "Advisory number: %s\n",
                                        el.elem.vol.info.advnum );
            printf ( "Information source: %s\n",
                                        el.elem.vol.info.infosorc );
            printf ( "Additional info source: %s\n",
                                        el.elem.vol.info.addlsorc );
            printf ( "Details: %s\n",
                                        el.elem.vol.info.details );
            printf ( "Obs date: %s\n",
                                        el.elem.vol.info.obsdate );
            printf ( "Obs time: %s\n",
                                        el.elem.vol.info.obstime );
            printf ( "Obs ash cloud: %s\n",
                                        el.elem.vol.info.obsashcld );
            printf ( "Fcst ash cloud (06): %s\n",
                                        el.elem.vol.info.fcst_06 );
            printf ( "Fcst ash cloud (12): %s\n",
                                        el.elem.vol.info.fcst_12 );
            printf ( "Fcst ash cloud (18): %s\n",
                                        el.elem.vol.info.fcst_18 );
            printf ( "Remarks: %s\n",
                                        el.elem.vol.info.remarks );
            printf ( "Next advisory: %s\n",
                                        el.elem.vol.info.nextadv );
            printf ( "Forecaster(s): %s\n",
                                        el.elem.vol.info.fcstrs );
            printf ( "Volcano plot location:\n" );
            printf ( "      %f      %f\n",
                        el.elem.vol.latlon[0],
                        el.elem.vol.latlon[1] );
            printf ( "Volcano plot offset:\n" );
            printf ( "      %d      %d\n",
                        el.elem.vol.offset_xy[0],
                        el.elem.vol.offset_xy[1] );
        }
	else if ( el.hdr.vg_class == CLASS_SIGMETS ) {
	    printf ( "Type of SIGMET: %d\n",
					el.elem.sig.info.subtype );
	    printf ( "Number of points along line: %d\n",
					el.elem.sig.info.npts );
	    printf ( "Line type: %d\n",
					el.elem.sig.info.lintyp );
	    printf ( "Line width: %d\n",
					el.elem.sig.info.linwid );
	    printf ( "Side of line: %d\n",
					el.elem.sig.info.sol );
	    printf ( "Area(MWO) indicator of unit: %s\n",
					el.elem.sig.info.area );
	    printf ( "Location indicator of FIR unit(s): %s\n",
					el.elem.sig.info.fir );
	    printf ( "Status (0=new, 1=amend, 2=cancel): %d\n",
					el.elem.sig.info.status );
	    printf ( "Distance (nautical miles): %f\n",
					el.elem.sig.info.distance );
	    printf ( "Message id: %s\n",
					el.elem.sig.info.msgid );
	    printf ( "Sequence number: %d\n",
					el.elem.sig.info.seqnum );
	    printf ( "Start valid time (ddhhmm): %s\n",
					el.elem.sig.info.stime );
	    printf ( "End valid time (ddhhmm):   %s\n",
					el.elem.sig.info.etime );
	    printf ( "Descriptive remarks: %s\n",
					el.elem.sig.info.remarks );
	    printf ( "Supersonic indicator: %d\n",
					el.elem.sig.info.sonic );
	    printf ( "Phenomenon: %s\n",
					el.elem.sig.info.phenom );
	    printf ( "Phenomenon II: %s\n",
					el.elem.sig.info.phenom2 );
	    printf ( "Phenomenon name/site: %s\n",
					el.elem.sig.info.phennam );
	    printf ( "Phenomenon lat: %s\n",
					el.elem.sig.info.phenlat );
	    printf ( "Phenomenon lon: %s\n",
					el.elem.sig.info.phenlon );
	    printf ( "Pressure: %d\n",
					el.elem.sig.info.pres );
	    printf ( "Max wind: %d\n",
					el.elem.sig.info.maxwind );
	    printf ( "Free text: %s\n",
					el.elem.sig.info.freetext );
	    printf ( "Trend: %s\n",
					el.elem.sig.info.trend );
	    printf ( "Movement: %s\n",
					el.elem.sig.info.move );
	    printf ( "Observed/Forecast indicator: %d\n",
					el.elem.sig.info.obsfcst );
	    printf ( "Observed/Forecast time (ddhhmm, UTC): %s\n",
					el.elem.sig.info.obstime );
	    printf ( "Flight level 100s ft): %d\n",
					el.elem.sig.info.fl );
	    printf ( "Speed of phenomenon: %d\n",
					el.elem.sig.info.spd );
	    printf ( "Direction of phenomenon (compass): %s\n",
					el.elem.sig.info.dir );
	    printf ( "Tops: %s\n",
					el.elem.sig.info.tops );
	    printf ( "Forecaster name : %s\n",
					el.elem.sig.info.fcstr );
	    printf ( "Line lat/lon points and times:\n" );
	    for ( j = 0; j < el.elem.sig.info.npts; j++ )
		printf ( "	%f	%f\n",
			el.elem.sig.latlon[j],
			el.elem.sig.latlon[j+el.elem.sig.info.npts]);
	}
	else if ( el.hdr.vg_class == CLASS_LIST ) {
	    printf ( "LIST type: %d - ", el.elem.lst.info.subtyp );
	    
	    if ( el.elem.lst.info.subtyp == 1 )
	        printf ( "%s\n", "COUNTY" );
	    else if ( el.elem.lst.info.subtyp == 2 ) 
	        printf ( "%s\n", "ZONE" );
	    else if ( el.elem.lst.info.subtyp == 3 ) 
	        printf ( "%s\n", "WFO" );
	    else 
	        printf ( "%s\n", "STATE" );
					
	    printf ( "Marker type:  %d\n", el.elem.lst.info.mrktyp );
	    printf ( "Marker size:  %f\n", el.elem.lst.info.mrksiz );
	    printf ( "Marker width: %d\n", el.elem.lst.info.mrkwid );
	    
	    printf ( "  Item	Lat	Lon:\n");
	    for ( j = 0; j < el.elem.lst.data.nitems; j++ )		
		printf ( "  %s	%6.2f	%6.2f\n", el.elem.lst.data.item[j],
		          el.elem.lst.data.lat[j], el.elem.lst.data.lon[j]);
	}
	else if ( el.hdr.vg_type == JET_ELM ) {
	    /*
	     *  Jet line information
	     */
	    printf ( "Number of points along jet line: %d\n",
				el.elem.jet.line.spl.info.numpts );
	    printf ("Jet line color: %d\n",
	                        el.elem.jet.line.splcol );	    
	    printf ("Jet line type: %d\n",
	                        el.elem.jet.line.spl.info.spltyp );	    
	    printf ( "Jet line stroke multiplier: %d\n",
				el.elem.jet.line.spl.info.splstr );
	    printf ( "Jet line direction indicator: %d\n",
				el.elem.jet.line.spl.info.spldir );
	    printf ( "Jet line size: %4.1f\n",
				el.elem.jet.line.spl.info.splsiz );
	    printf ( "Jet line width: %d\n",
				el.elem.jet.line.spl.info.splwid );
	    printf ( "Jet line lat/lon points:\n" );
	    for ( j = 0; j < el.elem.jet.line.spl.info.numpts; j++ ) {
		printf ( "\t%7.2f\t%7.2f\n", el.elem.jet.line.spl.latlon[j],
		  el.elem.jet.line.spl.latlon[j+
		               el.elem.jet.line.spl.info.numpts] );
            }
	    
	    /*
	     *  Jet barb information
	     */
	    printf ( "\nNumber of barbs along jet: %d\n", el.elem.jet.nbarb );

	    printf ( "\nBarb\tcolor\twidth\tsize\ttype\thdsize"
	             "\t lat\tlon\tspeed\tdirection\n" );
	    for ( j = 0; j < el.elem.jet.nbarb; j++ ) {
		printf ( "%d\t%d\t%d\t%4.1f\t%d\t%4.1f\t"
		         "%6.2f\t%7.2f\t%7.2f\t%7.2f\n",
		   j+1,
		   el.elem.jet.barb[j].wndcol,
		   el.elem.jet.barb[j].wnd.info.width,
		   el.elem.jet.barb[j].wnd.info.size,
		   el.elem.jet.barb[j].wnd.info.wndtyp,
		   el.elem.jet.barb[j].wnd.info.hdsiz,
		   el.elem.jet.barb[j].wnd.data.latlon[0],
		   el.elem.jet.barb[j].wnd.data.latlon[1],
		   el.elem.jet.barb[j].wnd.data.spddir[0],
		   el.elem.jet.barb[j].wnd.data.spddir[1] );
	    }

	    /*
	     *  Barb text information
	     */
	    printf ( "\nText\tcolor\twidth\tsize\ttype\tfont\talign\t");
	    printf ( "rotn\txoff\tyoff\tlat\tlon\ttext\n");
	    for ( j = 0; j < el.elem.jet.nbarb; j++ ) {
	        printf ( "%d\t%d\t%d\t%4.1f\t%d\t%d\t%d\t%4.1f\t"
		         "%d\t%d\t%6.2f\t%7.2f\t%s\n",
			j+1,
			el.elem.jet.barb[j].sptcol,
			el.elem.jet.barb[j].spt.info.iwidth,
			el.elem.jet.barb[j].spt.info.sztext,
			el.elem.jet.barb[j].spt.info.sptxtyp,
			el.elem.jet.barb[j].spt.info.itxfn,
			el.elem.jet.barb[j].spt.info.ialign,
			el.elem.jet.barb[j].spt.info.rotn,
			el.elem.jet.barb[j].spt.info.offset_x,
			el.elem.jet.barb[j].spt.info.offset_y,
			el.elem.jet.barb[j].spt.info.lat,
			el.elem.jet.barb[j].spt.info.lon,
                        el.elem.jet.barb[j].spt.text );
            }

	    /*
	     *  Hash information
	     */
	    printf ( "\nNumber of hashs along jet: %d\n", el.elem.jet.nhash );

	    printf ( "\nHash\tcolor\twidth\tsize\ttype"
	             "\t lat\tlon\tdirection\n" );
	    for ( j = 0; j < el.elem.jet.nhash; j++ ) {
		printf ( "%d\t%d\t%d\t%4.1f\t%d\t"
		         "%6.2f\t%7.2f\t%7.2f\n",
		   j+1,
		   el.elem.jet.hash[j].wndcol,
		   el.elem.jet.hash[j].wnd.info.width,
		   el.elem.jet.hash[j].wnd.info.size,
		   el.elem.jet.hash[j].wnd.info.wndtyp,
		   el.elem.jet.hash[j].wnd.data.latlon[0],
		   el.elem.jet.hash[j].wnd.data.latlon[1],
		   el.elem.jet.hash[j].wnd.data.spddir[1] );
	    }	    	    
	}
	else if ( el.hdr.vg_type == GFA_ELM ) {
	    printf ( "Number of blocks:   %d\n", el.elem.gfa.info.nblocks );
	    
    	    G_MALLOC ( gfaStr, char, el.elem.gfa.info.nblocks * STD_STRLEN + 1, "cvgdump gfa" );

	    gfaStr[ 0 ] = '\0';
	    for ( ii = 0; ii < el.elem.gfa.info.nblocks; ii++ ) {
		strcat ( gfaStr, *el.elem.gfa.info.blockPtr[ ii ] );
	    }

	    ptr1 = gfaStr;

	    while ( ( ptr1 = strchr ( ptr1, '<' ) ) &&
	 	    ( ptr2 = strchr ( ptr1, '>' ) ) ) {

		strncpy ( tag, &ptr1[ 1 ], ptr2 - ptr1 - 1 );
		tag[ ptr2 - ptr1 - 1 ] = '\0';

		if ( ( ptr1 = strchr ( ptr2, '<' ) ) ) {
		   strncpy ( value, &ptr2[ 1 ], ptr1 - ptr2 - 1 );
		   value[ ptr1 - ptr2 - 1 ] = '\0';
		}
		else {
		   strcpy ( value, ptr2 + 1 );
		}
	
		printf ( "%s:\t\t%s\n", tag, value );
	  	ptr1 = ptr2 + 1;
	    }

    	    G_FREE ( gfaStr, char );

	    printf ( "Number of points:   %d\n", el.elem.gfa.info.npts );

	    printf ( "GFA Lat/Lon points:\n");
	    for ( j = 0; j < el.elem.gfa.info.npts; j++ )		
		printf ( "  %6.2f	%6.2f\n",
			  el.elem.gfa.latlon[j],
		          el.elem.gfa.latlon[j + el.elem.gfa.info.npts] );	    
	}
	else if ( el.hdr.vg_type == SGWX_ELM ) {
          printf ( "Type of SGWX: %d\n",
                                        el.elem.sgwx.info.subtype );
          printf ( "Number of points: %d\n",
                                        el.elem.sgwx.info.npts );
          printf ( "Line lat/lon points and times:\n" );
          for ( j = 0; j < el.elem.sgwx.info.npts; j++ )
            printf ( "	%f	%f\n",
			el.elem.sgwx.latlon[j],
			el.elem.sgwx.latlon[j+el.elem.sgwx.info.npts]);
          if (el.elem.sgwx.info.subtype == 3) {
            if (el.elem.sgwx.info.splsym > 0) {
              printf( "Special Symbol Number =%d\n",el.elem.sgwx.info.splsym);
            } else {
              printf( "Weather Symbol Number =%d\n",el.elem.sgwx.info.wxsym);
            }
          }
        }
	else if ( el.hdr.vg_type == TCA_ELM ) {
	    printf ( "Storm number:\t %d\n", el.elem.tca.info.stormNum );

	    switch ( el.elem.tca.info.issueStatus ) {
		case TCA_OPERATIONAL:
			printf( "Issue Status:\t Operational\n" );
			break;
		case TCA_EXPERIMENTAL:
			printf( "Issue Status:\t Experimental\n" );
			break;
		case TCA_TEST:
			printf( "Issue Status:\t Test\n" );
			break;
		case TCA_EXPERIMENTAL_OPNL:
			printf( "Issue Status:\t Experimental Operational\n" );
			break;
		default:
			printf( "Issue Status:\t None\n" );
			break;
	    }

	    switch ( el.elem.tca.info.basin ) {
		case 0:
			printf( "Basin:\t\t Atlantic\n" );
			break;
		case 1:
			printf( "Basin:\t\t E. Pacific\n" );
			break;
		case 2:
			printf( "Basin:\t\t C. Pacific\n" );
			break;
		case 3:
			printf( "Basin:\t\t W. Pacific\n" );
			break;
		default:
			printf( "Basin:\t\t unknown\n" );
			break;
  	    }
	    printf ( "Advisory number: %s\n", el.elem.tca.info.advisoryNum );
	    printf ( "Storm name:\t %s\n", el.elem.tca.info.stormName );
	    switch ( el.elem.tca.info.stormType ) {
		case 0:
			printf( "Storm type:\t Hurricane\n" );
			break;
		case 1:
			printf( "Storm type:\t Tropical Storm\n" );
			break;
		case 2:
			printf( "Storm type:\t Tropical Depression\n" );
			break;
		case 3:
			printf( "Storm type:\t Subtropical Storm\n" );
			break;
		case 4:
			printf( "Storm type:\t Subtropical Depression\n" );
			break;
		case 5:
			printf( "Storm type:\t Post-Tropical Cyclone\n" );
			break;
		case 6:
			printf( "Storm type:\t Potential Tropical Cyclone\n" );
			break;
		default:
			printf( "Storm type:\t unknown\n" );
			break;
	    }
	    printf ( "Time:\t\t %s\n", el.elem.tca.info.validTime );
	    printf ( "Time Zone:\t %s\n", el.elem.tca.info.timezone );
	    printf ( "Text Box Lat:\t %f\n", el.elem.tca.info.text_lat );
	    printf ( "Text Box Lon:\t %f\n", el.elem.tca.info.text_lon );
	    printf ( "Text Box Font:\t %d\n", el.elem.tca.info.text_font );
	    printf ( "Text Box text size:\t %f\n", el.elem.tca.info.text_size );
	    printf ( "Text Box text width:\t %d\n", el.elem.tca.info.text_width );
	    printf ( "Number of watches/warnings:\t %d\n", el.elem.tca.info.wwNum );

	    for ( counter = 0; counter < el.elem.tca.info.wwNum; counter++ ) {
	        printf( "Watch/Warning %d\n", counter + 1 );
		printf( "Severity:\t\t %s\n", 
			( el.elem.tca.info.tcaww[ counter ].severity == 0 )?
			"Tropical Storm" : "Hurricane" );
		printf( "Advisory type:\t\t %s\n", 
			( el.elem.tca.info.tcaww[ counter ].advisoryType == 0 )?
			"Watch" : "Warning" );

		switch ( el.elem.tca.info.tcaww[ counter ].specialGeog ) {
			case 0:
				printf( "Special geography type:\t None\n" );
				break;
			case 1:
				printf( "Special geography type:\t Islands\n" );
				break;
			case 2:
				printf( "Special geography type:\t Water\n" );
				break;
			default:
				printf( "Special geography type:\t unknown\n" );
				break;
		}

		printf( "Number of break points: \t %d\n", 
			el.elem.tca.info.tcaww[ counter ].numBreakPts );

		for ( ii = 0; ii < el.elem.tca.info.tcaww[ counter ].numBreakPts; ii++ ) {
		    printf( "Break point(Lat Lon Name): %6.2f   %6.2f   %s\n",
			el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat,
			el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon,
			el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName  );
		}
	    }
	}

	else if ( el.hdr.vg_type == TCERR_ELM ) {
	    printf ( "Storm number:\t %s\n", el.elem.tce.info.stormNum );
            printf ( "Issue Status:\t %s\n",el.elem.tce.info.issueStatus );
	    printf ( "Basin:\t %s\n", el.elem.tce.info.basin );
	    printf ( "Advisory number:\t %s\n", el.elem.tce.info.advisoryNum );
	    printf ( "Storm name:\t %s\n", el.elem.tce.info.stormName );
	    printf( "Storm type:\t %s\n", el.elem.tce.info.stormType );
	    printf ( "Time:\t\t %s\n", el.elem.tce.info.validTime );
	    printf ( "Time Zone:\t %s\n", el.elem.tce.info.timezone );
	    printf ( "Forecast Period:\t %s\n", el.elem.tce.info.fcstpd );

	    printf ( "TC error cone line color:\t %d\n", el.elem.tce.cone.lincol );
	    printf ( "TC error cone line type:\t %d\n", el.elem.tce.cone.lintyp );
	    printf ( "TC error cone fill color:\t %d\n", el.elem.tce.cone.filcol );
	    printf ( "TC error cone fill type:\t %d\n", el.elem.tce.cone.filtyp );
	    printf ( "TC error cone number of pts:\t %d\n", el.elem.tce.cone.npts );

	    printf( "  Lat\t  Lon\t\n");
	    for ( ii = 0; ii < el.elem.tce.cone.npts; ii++ ) {
		 printf( "%6.2f\t%6.2f\n", 
                         el.elem.tce.cone.latlon[ii],
			 el.elem.tce.cone.latlon[ii+el.elem.tce.cone.npts]);
	    }

	}

	else if ( el.hdr.vg_type == TCTRK_ELM ) {
	    printf ( "Storm number:\t %s\n", el.elem.tct.info.stormNum );
            printf ( "Issue Status:\t %s\n",el.elem.tct.info.issueStatus );
	    printf ( "Basin:\t %s\n", el.elem.tct.info.basin );
	    printf ( "Advisory number:\t %s\n", el.elem.tct.info.advisoryNum );
	    printf ( "Storm name:\t %s\n", el.elem.tct.info.stormName );
	    printf( "Storm type:\t %s\n", el.elem.tct.info.stormType );
	    printf ( "Time:\t\t %s\n", el.elem.tct.info.validTime );
	    printf ( "Time Zone:\t %s\n", el.elem.tct.info.timezone );
	    printf ( "Forecast Period:\t %s\n", el.elem.tct.info.fcstpd );

	    printf ( "TC track line color:\t %d\n", el.elem.tct.lincol );
	    printf ( "TC track line type:\t %d\n", el.elem.tct.lintyp );
	    printf ( "TC track number of pts:\t %d\n", el.elem.tct.numTrackPts );
	    printf( "  Lat\t  Lon\tAdvDate\tTau\t" 
                    "MaxWind\tGust\tMSLP\t"
                    "TcDev\t\tDevLbl\tTcDir\tTCSpd\tDateLbl\tStormSource\n");

	    for ( ii = 0; ii < el.elem.tct.numTrackPts; ii++ ) {
		    printf( "%6.2f\t%6.2f\t%s\t%s\t%s\t%s\t%s\t%-10.14s\t%s\t%s\t%s\t%s\t%s\n",
			el.elem.tct.trackPnt[ ii ].lat,
			el.elem.tct.trackPnt[ ii ].lon,
			el.elem.tct.trackPnt[ ii ].advDate,
			el.elem.tct.trackPnt[ ii ].tau,
			el.elem.tct.trackPnt[ ii ].mxWnd,
			el.elem.tct.trackPnt[ ii ].wGust,
			el.elem.tct.trackPnt[ ii ].mslp,
			el.elem.tct.trackPnt[ ii ].tcDv,
			el.elem.tct.trackPnt[ ii ].tcDvLbl,
			el.elem.tct.trackPnt[ ii ].tcDir,
			el.elem.tct.trackPnt[ ii ].tcSpd,
			el.elem.tct.trackPnt[ ii ].dtLbl,
			el.elem.tct.trackPnt[ ii ].stSrc);
	    }

	}

	else if ( el.hdr.vg_type == TCBKL_ELM ) {
	    printf ( "Storm number:\t %s\n", el.elem.tcb.info.stormNum );
            printf ( "Issue Status:\t %s\n",el.elem.tcb.info.issueStatus );
	    printf ( "Basin:\t %s\n", el.elem.tcb.info.basin );
	    printf ( "Advisory number:\t %s\n", el.elem.tcb.info.advisoryNum );
	    printf ( "Storm name:\t %s\n", el.elem.tcb.info.stormName );
	    printf ( "Storm type:\t %s\n", el.elem.tcb.info.stormType );
	    printf ( "Time:\t\t %s\n", el.elem.tcb.info.validTime );
	    printf ( "Time Zone:\t %s\n", el.elem.tcb.info.timezone );
	    printf ( "Forecast Period:\t %s\n", el.elem.tcb.info.fcstpd );

	    printf ( "TC WW line color:\t %d\n", el.elem.tcb.lincol );
	    printf ( "TC WW line type:\t %d\n", el.elem.tcb.linwid );
	    printf ( "TC WW line severity:\t %d\n", el.elem.tcb.tcww );
	    printf ( "TC breakpint number of pts:\t %d\n", el.elem.tcb.numBkPts );
	    printf( "  Lat\t  Lon\tBkName\t\n"); 

	    for ( ii = 0; ii < el.elem.tcb.numBkPts; ii++ ) {
		    printf( "%6.2f\t%6.2f\t%s\t\n",
			el.elem.tcb.bkPntLn[ ii ].lat,
			el.elem.tcb.bkPntLn[ ii ].lon,
			el.elem.tcb.bkPntLn[ii].breakPtName);
	    }

	}

	else if (el.hdr.vg_type == FILEHEAD_ELM)
	{
	    printf ("Version: %s \n", el.elem.fhed.version);
	    printf ("Comments: %s \n", el.elem.fhed.notes);

	}
    }
}
