#include "cvgcmn.h"
#include "drwids.h"
#include "pgprm.h"
#include "cvgoldstructs.h"

void cvg_crtfnt( char *fname, int *iret );
void cvg_crtwbx( char *fname, int *iret );
void cvg_crtlin( char *fname, int *iret );
void cvg_crtsym( char *fname, int *iret );
void cvg_crttxt( char *fname, int *iret );
void cvg_crtwnd( char *fname, int *iret );
void cvg_crttrk( char *fname, int *iret );
void cvg_crtsig( char *fname, int *iret );
void cvg_crtcir( char *fname, int *iret );
void cvg_crtlst( char *fname, int *iret );
void cvg_crtmet( char *fname, int *iret );
void cvg_crtgfa( char *fname, int *iret );
void cvg_crttca( char *fname, int *iret );
void cvg_crttce( char *fname, int *iret );
void cvg_crttct( char *fname, int *iret );
void cvg_crttcb( char *fname, int *iret );
void cvg_crtsgwx( char *fname, int *iret );
void cvg_crthdr( VG_DBStruct *el, int np, float *lat, float *lon, int *iret);
void cvg_writv0_elm ( v0_VG_DBStruct *el, int start, int numbytes,  
                      char *fname, int *location, int *iret );
void cvg_writv1_elm ( v1_VG_DBStruct *el, int start, int numbytes, 
                      char *fname, int *location, int *iret );
void cvg_writv2_elm ( v2_VG_DBStruct *el, int start, int numbytes, 
                      char *fname, int *location, int *iret );
void cvg_writv3_elm ( v3_VG_DBStruct *el, int start, int numbytes, 
                      char *fname, int *location, int *iret );
void cvg_writv4_elm ( v4_VG_DBStruct *el, int start, int numbytes, 
                      char *fname, int *location, int *iret );
void cvg_writv5_elm ( v5_VG_DBStruct *el, int start, int numbytes, 
		      char *fname, int *location, int *iret );

/************************************************************************
 * cvgcrall.c                                                           *
 *                                                                      *
 * This module contains functions for manually creating a single VGF    * 
 * element of any type through keyboard input.                          *
 *                                                                      *
 * CONTENTS:                                                            *
 * cvg_crelm()		Creates specified VG elements			*
 * cvg_crtfnt()		Creates FRONT					*
 * cvg_crtwbx()		Creates WATCH BOX				*
 * cvg_crtlin()		Creates LINE (including special line)		*
 * cvg_crtsym()		Creates SYMBOL					*
 * cvg_crttxt()		Creates TEXT (including special text)		*
 * cvg_crtwnd()		Creates WIND					*
 * cvg_crttrk()		Creates STORM TRACK				*
 * cvg_crtsig()		Creates SIGMET (including CCF)			*
 * cvg_crtcir()		Creates CIRCLE					*
 * cvg_crtlst()		Creates LIST					*
 * cvg_crtmet()		Creates MET->JET element			*
 * cvg_crtgfa()		Creates MET->GFA element			*
 * cvg_crttca()		Creates MET->TCA element			*
 * cvg_crttce()		Creates MET->TCE element			*
 * cvg_crttct()		Creates MET->TCT element			*
 * cvg_crttcb()		Creates MET->TCB element			*
 * cvg_crthdr()		Creates a common header for an element		*
 * cvg_crtsgwx()        Creates MET->SGWX element
 * cvg_writv0_elm()	Write version 0 elements to file           	*
 * cvg_writv1_elm()	Write version 1 elements to file           	*
 * cvg_writv2_elm()	Write version 2 elements to file           	*
 * cvg_writv3_elm()	Write version 3 elements to file           	*
 * cvg_writv4_elm()	Write version 4 elements to file           	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created				        	*
 * J. Wu/GSC	02/01	Replaced cvg_writelm() with cvg_writef()        *
 * J. Wu/GSC	02/01	Packaged cvg_swap_v?() into cvgoldswap.c   	*
 *			and added validation for SIGMET inputs 		*
 * J. Wu/GSC	02/01	Modified 'unused1' & 'unused2' to 'smooth' &  	*
 *			'version' due to the renaming in VG structure	*
 * S. Jacobs/NCEP	02/01	Added machine type MTLNUX		*
 * T. Piper/GSC	03/01	Fixed IRIX6 compiler warnings			*
 * J. Wu/SAIC	06/02	update for version 4 and 5	        	*
 * J. Wu/SAIC	09/02	add cvg_crtlst()        			*
 * J. Wu/SAIC	11/02	revise LIST structure        			*
 * D.W.Plummer/NCEP 06/03 added ASHCLD_ELM and VOLC_ELM			*
 * J. Wu/SAIC	09/03	add CLASS_MET -> JET_ELM        		*
 * T. Lee/SAIC	11/03	used cvgcmn.h					*
 * m.gamazaychikov/SAIC 04/07   add cvg_rdtce, cvg_rdtct and cvg_rdtcb  *
 * L. Hinson/AWC 01/12  Add CLASS_MET -> SGWX_ELM                       *
 ***********************************************************************/

/*=====================================================================*/

void cvg_crelm ( char *fname, int *iret )
/************************************************************************
 * cvg_crelm								*
 *									*
 * This function calles other cvg_crt*() to create VG elements and then *
 * writes to the given file                                             *
 *									*
 * cvg_crelm ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	                                -15 - Error creating VGF file	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * J. Wu/GSC	09/02	add CLASS_LIST				        *
 * J. Wu/GSC	09/03	add CLASS_MET				        *
 ***********************************************************************/
{
    char 	sel[LLSCRN];
    int		elm_class, ier;
    Boolean	clsflg, cont;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    ier = 0;
    cont = TRUE;
    clsflg = FALSE;
    
    printf ("Select an element class or type EXIT:\n");
    printf ("	 1 = FRONTS	 2 = WATCHES	 3 = LINES \n");
    printf ("	 4 = SYMBOLS	 5 = TEXT	 6 = WINDS\n");            
    printf ("	10 = TRACKS     11 = SIGMETS    12 = CIRCLE\n");
    printf ("	14 = LIST       15 = MET\n");

    sel[0] = '\0';

    while ( !clsflg && sel[0] != 'e' && sel[0] != 'E' ) {
        scanf ( " %s", sel );
	elm_class = atoi ( sel );
        
	if ( elm_class == CLASS_FRONTS || elm_class == CLASS_WATCHES ||
             elm_class == CLASS_LINES  || elm_class == CLASS_SYMBOLS ||
             elm_class == CLASS_TEXT   || elm_class == CLASS_WINDS   ||
             elm_class == CLASS_TRACKS || elm_class == CLASS_SIGMETS ||
             elm_class == CLASS_CIRCLE || elm_class == CLASS_LIST    ||
	     elm_class == CLASS_MET )	     
            clsflg = G_TRUE;
        else if ( sel[0] != 'e' && sel[0] != 'E' ) {
            printf ( "Invalid choice entered!\n" );
        }
	else {
	    *iret = -15;
	}
    }
        
    if ( sel[0] == 'e' || sel[0] == 'E' ) cont = FALSE;
    
    while ( cont ) {	
	cont = FALSE;
	
	switch ( elm_class ) {
	    case CLASS_FRONTS: 
	        cvg_crtfnt( fname, &ier);	    
	        break;
	    case CLASS_WATCHES: 
        	cvg_crtwbx( fname, &ier);	    
	        break;
	    case CLASS_LINES: 
	        cvg_crtlin( fname, &ier);	    
	        break;
	    case CLASS_SYMBOLS: 
	        cvg_crtsym( fname, &ier);	    
	        break;
	    case CLASS_TEXT: 
	        cvg_crttxt( fname, &ier);	    
	        break;
	    case CLASS_WINDS: 
	        cvg_crtwnd( fname, &ier);	    
	        break;
	    case CLASS_TRACKS: 
	        cvg_crttrk( fname, &ier);	    
	        break;
	    case CLASS_SIGMETS: 
	        cvg_crtsig( fname, &ier);	    
	        break;
	    case CLASS_CIRCLE: 
	        cvg_crtcir( fname, &ier);	    
	        break;	
	    case CLASS_LIST: 
	        cvg_crtlst( fname, &ier);	    
	        break;	
	    case CLASS_MET: 
	        cvg_crtmet( fname, &ier);	    
	        break;	
	    default:
		break;   
        }
        
	if ( ier < 0 ) *iret = -15;
    }
}

/*=====================================================================*/

void cvg_crtfnt ( char *fname, int *iret )
/************************************************************************
 * cvg_crtfnt								*
 *									*
 * This function creates a FRONT element & writes it to the given file  *
 *									*
 * cvg_crtfnt ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{
    int		np, ii, start, loc, ier, smth;
    float	lat[MAXPTS], lon[MAXPTS];
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */

    el.hdr.vg_class = CLASS_FRONTS;
    el.hdr.vg_type = FRONT_ELM;
    
    printf ( "Specify FRONT attributes:\n" );
    printf ( "Enter the front code (0-999):\n" );
    scanf ( " %i", &el.elem.frt.info.fcode );
    printf ( "Enter the pip size (>0):\n" );
    scanf ( " %i", &el.elem.frt.info.fpipsz );
    printf ( "Enter the pip stroke (>0):\n" );
    scanf ( " %i", &el.elem.frt.info.fpipst );
    printf ( "Enter the pip direction:\n" );
    scanf ( " %i", &el.elem.frt.info.fpipdr );
    printf ("Enter the # of points along the front (0-500):\n" );
    scanf ( " %i", &np );

    if ( np <= 0 || np > MAXPTS ) {
        *iret = -28;
	return;
    }
    
    printf ( "Enter %i coordinate pairs in MAP coordinate:\n", np );
    for ( ii = 0; ii < np; ii++ )
        scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );	

    printf ("Smoothed? 0 = No 1 = Light 2 = Heavy \n");
    scanf (" %i", &smth);
    el.hdr.smooth = (char)smth;
        
    el.hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * np) ) + 
	      	     sizeof(VG_HdrStruct) + sizeof(FrontInfo) );
    el.elem.frt.info.numpts = np;
    strcpy ( el.elem.frt.info.frtlbl, "STJ" );

    cvg_crthdr( &el, np, lat, lon, &ier);
    
    for ( ii = 0; ii < np; ii++ ) {
	el.elem.frt.latlon[ii] = lat[ii];
	el.elem.frt.latlon[ii + np] = lon[ii];
    }
     	   
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtwnd ( char *fname, int *iret )
/************************************************************************
 * cvg_crtwnd								*
 *									*
 * This function creates a WIND element & write to the given file       *
 *									*
 * cvg_crtwnd ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{   
    int		np, start, type, loc, ier;
    float	lat[1], lon[1];
    char	sel[LLSCRN];
    Boolean	typflg;
    VG_DBStruct	el;    
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    typflg = G_FALSE;
    np = 1;
    
    el.hdr.vg_class = CLASS_WINDS;

    printf ( "Enter one of the following wind symbol types:\n" );
    printf ( "Barb\t\t%2d\n", BARB_ELM );  
    printf ( "Arrow\t\t%2d\n", ARROW_ELM );  
    printf ( "Dir arrow\t%2d\n", DARR_ELM );  
    printf ( "Hash mark\t%2d\n\n", HASH_ELM );  
    while ( !typflg ) {
        scanf ( " %s", sel );
 	type = atoi ( sel );       
	if ( type == BARB_ELM || type == ARROW_ELM ||
             type == DARR_ELM || type == HASH_ELM)
            typflg = G_TRUE;
        else
            printf ( "Invalid type entered!\n" );
    }
    el.hdr.vg_type = (char)type;
    
    printf ( "Enter the wind symbol width:\n" );
    scanf ( " %i", &el.elem.wnd.info.width );
    printf ( "Enter the wind symbol size:\n" );
    scanf ( " %f", &el.elem.wnd.info.size );
    printf ( "Enter the wind symbol attribute type:\n" );
    scanf ( " %i", &el.elem.wnd.info.wndtyp );
    if ( type == ARROW_ELM ) {
        printf ( "Enter the arrow head size:\n" );
        scanf ( " %f", &el.elem.wnd.info.hdsiz );
    }
    else
        el.elem.wnd.info.hdsiz = 0.0F;
	
    printf ( "Enter %i lat/lon point, speed,", np);
    printf ( " and direction:\n" );
    scanf ( " %f %f %f %f", &(lat[0]), &(lon[0]),
                            &(el.elem.wnd.data.spddir[0]),
			    &(el.elem.wnd.data.spddir[np]) );
    
    el.hdr.recsz = (int)( sizeof( float ) * (size_t)np * 4 + 
	        sizeof(VG_HdrStruct) + sizeof(WindInfo) );    
    el.elem.wnd.info.numwnd = np;

    cvg_crthdr( &el, np, lat, lon, &ier);

    el.elem.wnd.data.latlon[0 ] = lat[0];
    el.elem.wnd.data.latlon[np] = lon[0];

    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crttxt ( char *fname, int *iret )
/************************************************************************
 * cvg_crttxt								*
 *									*
 * This function create a TEXT or special TEXT element & writes to file *
 *									*
 * cvg_crttxt ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * J. Wu/SAIC	02/03	add midlevel cloud type   			*
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{
    int		np, start, loc, ier, type, lens, ii;
    float	lat[1], lon[1];
    int		itxfn, ithw, iwidth, ialign, ixoff, iyoff;
    float	rotn, sztext;
    char	text[MAX_TEXT], sel[LLSCRN], mcloud[8][LLSCRN], sep[]="|";
    Boolean	typflg;
    VG_DBStruct	el;    
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    typflg = G_FALSE;
    np = 1;
    
    el.hdr.vg_class = CLASS_TEXT;

    printf ( "Enter one of the following text types:\n" );
    printf ( "Regular Text\t	%2d\n", TEXT_ELM );  
    printf ( "Justified Text\t	%2d\n", TEXTC_ELM );  
    printf ( "Special Text\t	%2d\n\n", SPTX_ELM );  
    while ( !typflg ) {
        scanf ( " %s", sel );
 	type = atoi ( sel );       
         if ( type == TEXT_ELM || type == TEXTC_ELM ||
             type == SPTX_ELM )
            typflg = G_TRUE;
        else
            printf ( "Invalid text type entered!\n" );
    }
    el.hdr.vg_type = (char)type;
    
    if ( type == TEXT_ELM ) 
        printf ( "Text attributes:\n" );
    else if ( type == TEXTC_ELM ) 
        printf ( "Justified text attributes:\n" );
    else
        printf ( "Special text attributes:\n" );
    
    printf ( "Enter the text font(itxfn):\n" );
    scanf ( " %i", &itxfn );
    printf ( "Enter the text HW flag:\n" );
    scanf ( " %i", &ithw );
    printf ( "Enter the text width size multiplier:\n" );
    scanf ( " %i", &iwidth );
    printf ( "Enter the text size:\n" );
    scanf ( " %f", &sztext );
    printf ( "Enter the text x coordinate:\n" );
    scanf ( " %f", &lat[0] );
    printf ( "Enter the text y coordinate:\n" );
    scanf ( " %f", &lon[0] );
    printf ( "Enter the rotation:\n" );
    scanf ( " %f", &rotn );
    printf ( "Enter the x offset:\n" );
    scanf ( " %i", &ixoff );
    printf ( "Enter the y offset:\n" );
    scanf ( " %i", &iyoff );    
    printf ( "Enter the alignment value (-1, 0, 1)\n" );
    scanf ( " %i", &ialign );
    
    if ( el.hdr.vg_type == SPTX_ELM ) {           
        printf ( "Enter the Special Text Type (1-15):\n" );
        scanf ( " %i", &el.elem.spt.info.sptxtyp );
        if ( el.elem.spt.info.sptxtyp == 15 ) { 
	    /* 
	     * collect information for midlevel cloud and encode
	     * into a single string as input to "text" field. 
	     */
	    printf ( "Enter midlevel cloud types:\n" );
            printf ( "    (Select from CU,ST,SC,NS,AS,AC,CS,CC,CI\n" );
            printf ( "     and separate each with semicolon ';' )\n" );            
	    scanf ( " %s", mcloud[0] );	    
	    printf ( "Enter midlevel cloud amount:\n" );
            printf ( "    (Select from SKC,SCT,BKN,OVC,LYR\n" );
            printf ( "     and separate each with semicolon ';' )\n" );            
	    scanf ( " %s", mcloud[1] );	    
	    printf ( "Enter icing symbol (0-8):\n" );
	    scanf ( " %s", mcloud[2] );	    
	    printf ( "Enter icing base/top level (e.g, 220/140):\n" );
	    scanf ( " %s", mcloud[3] );	    
	    printf ( "Enter midlevel turbulance symbol (0-10):\n" );
	    scanf ( " %s", mcloud[4] );	    
	    printf ( "Enter turbulance base/top level (e.g, 220/140):\n" );
	    scanf ( " %s", mcloud[5] );
	    printf ( "Enter the thunderstorm types:\n" );
            printf ( "    (Select from ISOL, OCNL, FRQ, EMBD, CB\n" );
            printf ( "     and separate each with semicolon ';' )\n" );            
	    scanf ( " %s", mcloud[6] );	    
	    printf ( "Enter thunderstorm base/top level (e.g, 220/140):\n" );
	    scanf ( " %s", mcloud[7] );
	
	    strcpy ( text, "");
	    for ( ii = 0; ii < 8; ii++ ) {
	        strcat ( text, mcloud[ii] );
		if ( ii < 7 ) strcat ( text, sep );        	            
	    }
	    
	    el.elem.spt.info.turbsym = 0;
	} 
	else {	
	    printf ( "Enter the Turbulence/Icing Symbol:\n" );
            scanf ( " %i", &el.elem.spt.info.turbsym );
	}
    }
    
    /*
     *  Input "text" for all non-MCLOUD text types.
     */
    if ( el.hdr.vg_type != SPTX_ELM || 
         ( el.hdr.vg_type == SPTX_ELM && el.elem.spt.info.sptxtyp != 15 )) {           
        printf ( "Enter the text:\n" );    
        scanf ( " %s", text );
    }
    
    lens = (int)strlen(text);
    cvg_crthdr( &el, np, lat, lon, &ier);
    
    if ( el.hdr.vg_type == SPTX_ELM ) {           
	el.elem.spt.info.lat = lat[0];
	el.elem.spt.info.lon = lon[0];
        el.elem.spt.info.offset_x = ixoff;
        el.elem.spt.info.offset_y = iyoff;
        el.elem.spt.info.ialign  = ialign;	       
	el.elem.spt.info.rotn    = rotn;
        el.elem.spt.info.sztext  = sztext;
        el.elem.spt.info.itxfn   = itxfn;
        el.elem.spt.info.ithw    = ithw;
        el.elem.spt.info.iwidth  = iwidth;
        
        el.elem.spt.info.txtcol  = el.hdr.maj_col;
        el.elem.spt.info.lincol  = el.hdr.min_col;
        el.elem.spt.info.filcol  = el.hdr.min_col;
        el.hdr.recsz = (int)( sizeof(VG_HdrStruct) + 
		         sizeof(SpTextInfo) + (size_t)lens + 1 ) ;
        strcpy ( el.elem.spt.text, text );
    }
    else {
	el.elem.txt.info.lat = lat[0];
	el.elem.txt.info.lon = lon[0];	    
        el.elem.txt.info.offset_x = ixoff;
        el.elem.txt.info.offset_y = iyoff;
	el.elem.txt.info.ialign = el.elem.txt.info.ialign%10 - 2;	    
        el.elem.txt.info.rotn   = rotn;
        el.elem.txt.info.sztext = sztext;
        el.elem.txt.info.itxfn  = itxfn;
        el.elem.txt.info.ithw   = ithw;
        el.elem.txt.info.iwidth = iwidth;
	el.hdr.recsz = (int)( sizeof( VG_HdrStruct) + 
	                 sizeof( TextInfo) + (size_t)lens + 1 );	
        strcpy ( el.elem.txt.text, text );
   }

    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtlin ( char *fname, int *iret )
/************************************************************************
 * cvg_crtlin								*
 *									*
 * This function creates a LINE or special LINE element & writes to file*
 *									*
 * cvg_crtlin ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{   
    int		np, ii, start, loc, ier, type, clos, filled, smth;
    float	lat[MAXPTS], lon[MAXPTS];
    char	sel[LLSCRN];
    Boolean	typflg;
    VG_DBStruct	el;    
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    typflg = G_FALSE;
    
    el.hdr.vg_class = CLASS_LINES;

    printf("Enter the type of line:\n");
    printf ( "	Regular Line\t %2d\n", LINE_ELM );
    printf ( "	Special Line\t %2d\n", SPLN_ELM );

    while ( !typflg ) {
        scanf ( " %s", sel );
 	type = atoi ( sel );       
        if ( type == LINE_ELM || type == SPLN_ELM )
            typflg = G_TRUE;
        else
            printf ( "Invalid line type entered!\n" );
    }
    el.hdr.vg_type = (char)type;
    
    if ( type == LINE_ELM ) {
        printf ( "Line attributes:\n" );
        printf ( "Enter the line type:\n" );
        scanf ( " %i", &el.elem.lin.info.lintyp );
        printf ( "Enter the line type flag:\n" );
        scanf ( " %i", &el.elem.lin.info.lthw );
        printf ( "Enter the line width size multiplier:\n" );
        scanf ( " %i", &el.elem.lin.info.width);
        printf ( "Enter the line width flag:\n" );
        scanf ( " %i", &el.elem.lin.info.lwhw );
    }
    else {
        printf ( "Special line attributes:\n" );
        printf ( "Enter the special line type:\n" );
        scanf ( " %i", &el.elem.spl.info.spltyp );
        printf ( "Enter the spcl line stroke multiplier:\n");
        scanf ( " %i", &el.elem.spl.info.splstr );
        printf("Enter the spcl line direction indicator:\n");
        scanf ( " %i", &el.elem.spl.info.spldir );
        printf ( "Enter the special line size:\n" );
        scanf ( " %f", &el.elem.spl.info.splsiz );
        printf ( "Enter the special line width:\n" );
        scanf ( " %i", &el.elem.spl.info.splwid );
    }   

    printf ("Enter the # of points along the line (0-500):\n" );
    scanf ( " %i", &np );
        
    if ( np <= 0 || np > MAXPTS ) {
        *iret = -28;
	return;
    }

    printf ( "Enter %i coordinate pairs in MAP coordinate:\n", np );
    for ( ii = 0; ii < np; ii++ )
       scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );
    printf("Closed? 0 = No 1 = Yes \n");
    scanf( " %i", &clos);
    el.hdr.closed = (char)clos;
    printf("Filled? 0 = No 1 = Yes \n");
    scanf( " %i", &filled);
    el.hdr.filled = (char)filled;
    printf ("Smoothed? 0 = No 1 = Light 2 = Heavy \n");
    scanf (" %i", &smth);
    el.hdr.smooth = (char)smth;
    
    if ( type == LINE_ELM ) {
        el.elem.lin.info.numpts = np;
        el.hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * np)) + 
		      sizeof(VG_HdrStruct) + sizeof(LineInfo) );
	for ( ii = 0; ii < np; ii++ ) {
	    el.elem.lin.latlon[ii]    = lat[ii];
	    el.elem.lin.latlon[ii+np] = lon[ii];
	}
    }
    else {
	el.elem.spl.info.numpts = np;
        el.hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * np)) + 
		      sizeof(VG_HdrStruct) + sizeof(SpLineInfo) );
	for ( ii = 0; ii < np; ii++ ) {
	    el.elem.spl.latlon[ii]    = lat[ii];
	    el.elem.spl.latlon[ii+np] = lon[ii];
	}
    }
    
    cvg_crthdr( &el, np, lat, lon, &ier);
        
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtsym ( char *fname, int *iret )
/************************************************************************
 * cvg_crtsym								*
 *									*
 * This function creates a SYMBOL element & writes to file     		*
 *									*
 * cvg_crtsym ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{
    int		np, start, loc, ier, type;
    float	lat[MAXPTS], lon[MAXPTS];
    char	sel[LLSCRN];
    Boolean	typflg;
    VG_DBStruct	el;    
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    typflg = G_FALSE;
    np = 1;
    
    el.hdr.vg_class = CLASS_SYMBOLS;
    
    printf("Enter one of the following symbol types:\n");
    printf ( "	Weather\t %2d\n", WXSYM_ELM );
    printf ( "	Cloud\t %2d\n", CTSYM_ELM );
    printf ( "	Icing\t %2d\n", ICSYM_ELM );
    printf ( "	Prs Tnd\t %2d\n", PTSYM_ELM );
    printf ( "	Past Wx\t %d\n", PWSYM_ELM );
    printf ( "	Sky Cvr\t %2d\n", SKSYM_ELM );
    printf ( "	Special\t %2d\n", SPSYM_ELM );
    printf ( "	Turbul\t %2d\n\n", TBSYM_ELM );
    while ( !typflg ) {
        scanf ( " %s", sel );
 	type = atoi ( sel );       
        if ( type == WXSYM_ELM || type == CTSYM_ELM ||
             type == ICSYM_ELM || type == PTSYM_ELM ||
             type == PWSYM_ELM || type == SKSYM_ELM ||
             type == SPSYM_ELM || type == TBSYM_ELM )
            typflg = G_TRUE;
        else
            printf ( "Invalid type entered!\n" );
    }
    el.hdr.vg_type = (char)type;
    
    printf ( "Enter %i symbol code:\n", np );
    scanf ( " %f", &el.elem.sym.data.code[0] );
    printf ( "Enter the symbol width:\n" );
    scanf ( " %i", &el.elem.sym.info.width );
    printf ( "Enter the symbol size:\n" );
    scanf ( " %f", &el.elem.sym.info.size );
    printf ( "Enter the symbol ityp:\n" );
    scanf ( " %i", &el.elem.sym.info.ityp );
    printf ( "Enter %i lat/lon point:\n", np );
    scanf ( " %f %f", &(lat[0]), &(lon[0]) );
    printf ( "Enter %i set of offset:\n", np );
    scanf ( " %i %i", &(el.elem.sym.data.offset_xy[0]), 
                      &(el.elem.sym.data.offset_xy[np]) );

    el.hdr.recsz = (int)( (size_t)np * ( sizeof(float) * 3 + sizeof(int) * 2 ) +
	        sizeof(VG_HdrStruct) + sizeof(SymInfo) );
    el.elem.sym.info.numsym = np;
    cvg_crthdr( &el, np, lat, lon, &ier);    

    el.elem.sym.data.latlon[0 ] = lat[0];
    el.elem.sym.data.latlon[np] = lon[0];

    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtsig ( char *fname, int *iret )
/************************************************************************
 * cvg_crtsig								*
 *									*
 * This function creates a SIGMET, CCF or ASHCLD or VOLC element and	*
 *  writes to file.							*
 *									*
 * cvg_crtsig ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		01/01	Created					*
 * J. Wu/GSC		01/01	Added SIGMET old version 0		*
 * J. Wu/GSC		02/01	Added validation for inputs 		*
 * H. Zeng/XTRI 	11/03	modified a field for ASHCLD&VOLC	*
 * S. Danz/AWC		07/06	Switch to new cvg_writefD() function    *
 * S. Jacobs/NCEP	 3/10	Changed KZOA to KZAK			*
 * S. Guan/NCEP         11/16   Changed KZNY to KZWY                    *
 ***********************************************************************/
{
    int			np, ii, jj, start, loc, ier, type;
    int			version, cls, fil, sm, tmp_num;
    char		sel[LLSCRN], tmp_str[LLSCRN];
    float		lat[MAX_SIGMET], lon[MAX_SIGMET];
    double		dbltmp;
    Boolean		typflg;
    VG_DBStruct		el;    
    v0_VG_DBStruct	v0_el;

    char  *mwo_27[ 4  ] = { "KMKC", "KNHC", "PHNL", "KKCI" };
    char  *mwo_28[ 10 ] = { "KSFO", "KSLC", "KCHI", "KDFW", "KBOS",
                            "KMIA", "PHNL", "PANC", "PAFA", "PAJN" };
    
    char  *fir_27[ 6 ] = { "PAZA", "KZMA", "KZAK", 
                           "KZHU", "KZWY", "TJZS" };
			    
    char  *msgid_27[ 13 ] = { "ALFA", "BRAVO", "CHARLIE", "DELTA", 
                              "ECHO", "FOXTROT", "GOLF", "HOTEL", 
                              "INDIA", "JULIETT", "KILO", "LIMA", 
                              "MIKE" };
    char  *msgid_28[ 10 ] = { "NOVEMBER", "OSCAR", "PAPA", "QUEBEC", 
                              "ROMEO", "UNIFORM", "VICTOR",
                              "WHISKEY", "XRAY", "YANKEE" };
    char  *msgid_29[ 3  ] = { "EAST", "CENTRAL", "WEST" };
    char  *msgid_31[ 3  ] = { "SIERRA", "TANGO", "ZULU" };

    char  *rmks_27[ 8 ] = { "-none-", 
    			    "BASED_ON_SATELLITE_OBS",
    	  		    "BASED_ON_ACFT_AND_SAT", 
	 		    "BASED_ON_LATST_ADVSRY",
	     		    "BASED_ON_SAT_AND_LTG_OBS", 
	     		    "BASED_ON_SATELLITE_OBS_AND_LATST_ADVSRY",
	    		    "BASED_ON_LATST_WASHINGTON_VAAC_ADVISORY", 
	     		    "BASED_ON_ACFT_RPT" };
    char  *phm_27[ 16 ] = { "FRQ_TS", "OBSC_TS", "EMBD_TS", "SQL_TS",
    			     "MOD_TURB", "SEV_TURB", "SEV_ICE",
	     		     "SEV_ICE(FZRA)", "ISOL_CB", "OCNL_CB",
			     "MOD_SEV_TURB", "MOD_SEV_ICE", 
			     "VOLCANIC_ASH", "TROPICAL_CYCLONE",
			     "TROPICAL_STORM", "TROPICAL_DEPRESSION" };

    char  *dirc_27 [ 16 ] = { "N", "NNE", "NE", "ENE", "E", "ESE",
                              "SE", "SSE", "S", "SSW", "SW", "WSW",
			      "W", "SNS", "NW", "NNW" };

    char  *top_27_1 [ 3 ] = { "-none-", "FCST" , "TOPS"};
    char  *top_27_2 [ 4 ] = { "TO", "ABV" , "BLW", "BTN"};
    char  *top_27_4 [ 2 ] = { "-none-", "AND" };
    char  *space = " ";
    char  *sap = "|";
    
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    typflg = G_FALSE;
    version = CUR_SIG_VER;
    
    el.hdr.vg_class = CLASS_SIGMETS;
       
    printf ( "Enter the sigmet type: \n " 
                "	27 = international SIGMET\n "
		"	28 = non-convective SIGMET\n "
		"	29 = convective SIGMET\n"
		"	30 = convective outlook\n"
		"	31 = AIRMET \n" 
		"	32 = CCF\n"
		"	35 = VAA volcano element\n"
		"	36 = VAA ash cloud element\n" );
    while ( !typflg ) {
        scanf ( " %s", sel );
 	type = atoi ( sel );       
         if ( ( type >= 27 && type <= 32 ) || type == 35 || type == 36 )  {
             typflg = G_TRUE;
	     if ( type != SIGCCF_ELM && type != ASHCLD_ELM && type != VOLC_ELM) {
		 printf ( "Enter sigmet version number:\n" );
		 for ( ii = 0; ii <= CUR_SIG_VER; ii++ )
		     printf ( "\t%d - SIGMET Version %d\n", ii, ii ); 
                 scanf ( " %s", sel );
 	         version = atoi ( sel );       
                 
		 if ( version < 0 || version > CUR_SIG_VER ) 
		     version = CUR_SIG_VER;        
                 printf ( "Creating version %d sigmet...\n", version );       
	     }
         }    
         else
            printf ( "Invalid sigmet type entered!\n" );
    }
    el.hdr.vg_type = (char)type;
        
    if ( type != VOLC_ELM )  {

        printf ( "Enter the # of points for sigmet area (0-500):\n" );
        scanf  ( " %i", &np );

        if ( np <= 0 || np > MAX_SIGMET ) {
            *iret = -28;
	    return;
        }

        printf ( "Enter %i coordinate pairs in MAP coordinate:\n", np );
        for ( ii = 0; ii < np; ii++ )
            scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );

    }
    else if ( type == VOLC_ELM )  {
	np = 1;
    }
    
    if ( el.hdr.vg_type != SIGCCF_ELM  &&
	 el.hdr.vg_type != VOLC_ELM &&
	 el.hdr.vg_type != ASHCLD_ELM ) {
        printf ( "Enter the area type (0 = Enclosed area  ");
        printf ( "1 = Line    2 = ISOL ):\n" );
	scanf  ( " %i", &el.elem.sig.info.subtype );               
	printf ( "Enter line type:\n");
        scanf  ( " %i", &el.elem.sig.info.lintyp );
        printf ( "Enter the line width size multiplier:\n" );
        scanf  ( " %i", &el.elem.sig.info.linwid);    
        printf ( "Enter side of line:\n" );
        printf ( "0 - ESOL  1 - NOF  2 - SOF  3 - EOF  4 - WOF\n" );
        scanf  ( " %i", &el.elem.sig.info.sol );
        
	printf ( "Enter the area (MWO) indicator of unit:\n" );
        if ( type == SIGINTL_ELM || type == SIGCONV_ELM ) { 
	    for ( ii = 0; ii < 4; ii++ )
	         printf ( " %s", mwo_27[ii] );	        
	}
	else {
	    for ( ii = 0; ii < 10; ii++ )
	         printf ( " %s", mwo_28[ii] );	        	
	}
	printf ( "\n" );
        scanf  ( " %s", el.elem.sig.info.area );
        
	if ( type == SIGINTL_ELM ) { 
	    printf ( "Enter the location indicator of FIR unit\n" );
	    for ( ii = 0; ii < 6; ii++ )
	         printf ( " %s", fir_27[ii] );	        
 	    printf ( "\n" );
            scanf  ( " %s", el.elem.sig.info.fir );
	}
	else 
	    el.elem.sig.info.fir[0] = '\0';
    
        printf ( "Enter the status (0 = New  1 = Amend  2 = Cancel):\n");
        scanf  ( " %i", &el.elem.sig.info.status );
        printf ( "Enter the distance (nautical miles):\n" );
        scanf  ( " %f", &el.elem.sig.info.distance );
        
	printf ( "Enter the message id (ALFA, BRAVO, etc.):\n" );
	if ( type == SIGINTL_ELM ) {
	    for ( ii = 0; ii < 13; ii++ )
	         printf ( " %s", msgid_27[ii] );	        
	}
	else if ( type == SIGNCON_ELM ) {
	    for ( ii = 0; ii < 10; ii++ )
	         printf ( " %s", msgid_28[ii] );	        
	}
	else if ( type == SIGAIRM_ELM ) 
	    for ( ii = 0; ii < 3; ii++ )
	         printf ( " %s", msgid_31[ii] );
	else  
	    for ( ii = 0; ii < 3; ii++ )
	         printf ( " %s", msgid_29[ii] );
 	printf ( "\n" );        
	scanf  ( " %s", el.elem.sig.info.msgid );
	
        printf ( "Enter the sequence number (1, 2, 3,...):\n" );
        scanf  ( " %i", &el.elem.sig.info.seqnum );
        printf ( "Enter the start valid time (ddhhmm):\n" );
        scanf  ( " %s", el.elem.sig.info.stime );
        printf ( "Enter the end valid time (ddhhmm):\n" );
        scanf  ( " %s", el.elem.sig.info.etime );
	
	if ( type == SIGINTL_ELM ) { 
            printf ( "Enter the descriptive remarks:" );
	    for ( ii = 0; ii < 8; ii++ )
	         printf ( "\n\t %d - %s", ii, rmks_27[ii] );	        
            printf("\n");
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num >= 0 && tmp_num <= 7 )
                strcpy( el.elem.sig.info.remarks, rmks_27[ tmp_num ] );
	    else
                strcpy( el.elem.sig.info.remarks, rmks_27[ 0 ] ); 
	}
	else 
	    el.elem.sig.info.remarks[0] = '\0';
	    
        printf ( "Enter the supersonic indicator (0, 1):\n" );
        scanf  ( " %i", &el.elem.sig.info.sonic );

	if ( type == SIGINTL_ELM ) {
            printf ( "Enter the phenomenon:" );
	    for ( ii = 0; ii < 16; ii++ ) {
		dbltmp = fmod( (double)ii, 2.0 );
	        if ( (int)dbltmp == 0 )  printf( "\n" ); 
	        printf ( "\t %d - %s \t", ii, phm_27[ii] );
	    }
	    printf( "\n" );
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num >= 0 && tmp_num <= 15 )
                strcpy( el.elem.sig.info.phenom, phm_27[ tmp_num ] );	
	    else
                strcpy( el.elem.sig.info.phenom, phm_27[ 0 ] );
        }
	else if ( type == SIGNCON_ELM ) 
            strcpy( el.elem.sig.info.phenom, "TURBULENCE" ); 
	else if ( type == SIGCONV_ELM ) 
            strcpy( el.elem.sig.info.phenom, "TORNADO" ); 
	else if ( type == SIGAIRM_ELM ) 
            strcpy( el.elem.sig.info.phenom, "IFR" ); 
        else
	    el.elem.sig.info.phenom[ 0 ] = '\0';
	    	
		
        if ( version == CUR_SIG_VER ) {
	    printf ( "Enter the second phenomenon:\n" );
	    scanf  ( " %s", el.elem.sig.info.phenom2 );
            printf ( "Enter the volcano or hurricane name:\n" );
            scanf  ( " %s", el.elem.sig.info.phennam );
            printf ( "Enter the volcano or hurricane latitude:\n" );
            scanf  ( " %s", el.elem.sig.info.phenlat );
            printf ( "Enter the volcano or hurricane longitude:\n" );
            scanf  ( " %s", el.elem.sig.info.phenlon );
            printf ( "Enter the pressure:\n" );
            scanf  ( " %i", &el.elem.sig.info.pres );
            printf ( "Enter the max wind:\n" );
            scanf  ( " %i", &el.elem.sig.info.maxwind );
            printf ( "Enter the free text:\n" );
            scanf  ( " %s", el.elem.sig.info.freetext );        
        }

        printf ( "Enter the trend (NC, WKN, INTSF):\n" );
        scanf  ( " %s", el.elem.sig.info.trend );
        printf ( "Enter the movement (STNRY, MVG):\n" );
        scanf  ( " %s", el.elem.sig.info.move );
        printf ( "Enter the observed/forecast indicator (0,1,2):\n" );
        scanf  ( " %i", &el.elem.sig.info.obsfcst );
        printf ( "Enter the observed/forecast time (ddhhmm, UTC):\n" );
        scanf  ( " %s", el.elem.sig.info.obstime );
        printf ( "Enter the flight level (100s ft):\n" );
        scanf  ( " %i", &el.elem.sig.info.fl );
        printf ( "Enter the speed of phenomenon (kts):\n" );
        scanf  ( " %i", &el.elem.sig.info.spd );
	
        printf ( "Enter the direction of phenomenon (compass):" );
	for ( ii = 0; ii < 16; ii++ ) {
	    dbltmp = fmod( (double)ii, 8.0 );
	     if ( (int)dbltmp == 0 )  printf( "\n" ); 
 	     printf ( " %s", dirc_27[ii] );	        
        }
	printf( "\n" );
	scanf  ( " %s", el.elem.sig.info.dir );
	
	if ( type != SIGINTL_ELM )	 
	    strcpy( el.elem.sig.info.tops, "-none-|TO| |-none| |" );	
        else {
	    printf ( "Enter the tops:\n" );
            printf ( "  First field ( 0- None  1 - FCST  2 - TOPS ):\n" );
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num >= 0 && tmp_num <= 2 )
                strcat( el.elem.sig.info.tops, top_27_1[ tmp_num ] );	
	    else
                strcat( el.elem.sig.info.tops, top_27_1[ 0 ] );
            
	    strcat( el.elem.sig.info.tops, sap );
        
	    printf ( "  Second field ( 0 - TO  1 - ABV  2 - BLW  3 - BTN ):\n" );
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num >= 0 && tmp_num <= 3 )
                strcat( el.elem.sig.info.tops, top_27_2[ tmp_num ] );	
	    else
                strcat( el.elem.sig.info.tops, top_27_2[ 0 ] );

	    strcat( el.elem.sig.info.tops, sap );

	    printf ( "  Third field ( 0 - None  1 - Enter new text ):\n" );
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num == 0  )
                strcat( el.elem.sig.info.tops, space );	
	    else {
	        printf ( "  Enter new text:\n" );
                scanf  ( " %s", tmp_str );	
                strcat( el.elem.sig.info.tops, tmp_str);
	    }

	    strcat( el.elem.sig.info.tops, sap );

	    printf ( "  Fourth field ( 0 - None  1 - AND ):\n" );
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num == 1 )
                strcat( el.elem.sig.info.tops, top_27_4[ tmp_num ] );	
	    else
                strcat( el.elem.sig.info.tops, top_27_4[ 0 ] );

	    strcat( el.elem.sig.info.tops, sap );

	    printf ( "  Fifth field ( 0 - None  1 - Enter new text ):\n" );
	    scanf  ( " %i", &tmp_num );
            if ( tmp_num == 0  )
                strcat( el.elem.sig.info.tops, space );	
	    else {
	        printf ( "  Enter new text:\n" );
                scanf  ( " %s", tmp_str );	
                strcat( el.elem.sig.info.tops, tmp_str);
            }
	    
	    strcat( el.elem.sig.info.tops, "\0" );	
	}
		
        printf ( "Enter the forecaster name (ALDRIDGE, CARLIE...):\n" );
        scanf  ( " %s", el.elem.sig.info.fcstr );
        if ( version == CUR_SIG_VER ) {
            el.elem.sig.info.npts = np;
            el.hdr.recsz = (int)(sizeof (VG_HdrStruct) +
			sizeof (float) * (size_t)(np * 2) + sizeof (SigmetInfo));
	    for ( ii = 0, jj = np; ii < np; ii++, jj++) {
	        el.elem.sig.latlon[ii] = lat[ii];
	        el.elem.sig.latlon[jj] = lon[ii];
	    }	    
	}
    }
    
    else if ( el.hdr.vg_type == SIGCCF_ELM ) {
	el.hdr.recsz = (sizeof (VG_HdrStruct) + sizeof (CCFType) );   
        el.elem.ccf.info.npts = np;
        printf ( "Enter the CCF subtype (0 = Enclosed area   "  );
        printf ( "1 = Line   2 = ISOL ):\n" );
	scanf  ( " %i", &el.elem.ccf.info.subtype );
        printf ( "Enter the coverage (1 - 75-100%%  2 = 50-74%%  3 = 25-49%%):\n" );
	scanf  ( " %i", &el.elem.ccf.info.cover );
        printf ( "Enter the tops (1 - 370+   2 - 310-370   3 = <310):\n" );
	scanf  ( " %i", &el.elem.ccf.info.tops );
        printf ( "Enter the probability (1 - 70-100%%  2 = 40-69%%  3 = 1-39%%):\n" );
	scanf  ( " %i", &el.elem.ccf.info.prob );
        printf ( "Enter the growth (1 - ++    2 - +   3 - NC   4 - - ):\n" );
	scanf  ( " %i", &el.elem.ccf.info.growth );
        printf ( "Enter the speed (knots  0-60):\n" );
	scanf  ( " %f", &el.elem.ccf.info.spd );
        printf ( "Enter the direction (compass 0-15):\n" );
	scanf  ( " %f", &el.elem.ccf.info.dir );
        for ( ii = 0, jj = np; ii < np; ii++, jj++) {
	     el.elem.ccf.latlon[ii] = lat[ii];
	     el.elem.ccf.latlon[jj] = lon[ii];
 	}        
   }

    else if ( el.hdr.vg_type == ASHCLD_ELM ) {
        el.hdr.recsz = (int)(sizeof (VG_HdrStruct) + sizeof (AshInfo) +
                        (size_t)(np * 2) * sizeof(float) );
        el.elem.ash.info.npts = np;
        printf ( "Enter the VAA ash cloud subtype (0 = Enclosed area   "  );
        printf ( "1 = Line   2 = ISOL ):\n" );
        scanf  ( " %i", &el.elem.ash.info.subtype );
        printf ( "Enter the distance (nautical miles):\n" );
        scanf  ( " %f", &el.elem.ash.info.distance );
        printf ( "Enter the forecast hour :\n" );
        scanf  ( " %d", &el.elem.ash.info.fhr );
        printf ( "Enter the line type :\n" );
        scanf  ( " %d", &el.elem.ash.info.lintyp );
        printf ( "Enter the line width :\n" );
        scanf  ( " %d", &el.elem.ash.info.linwid );
        printf ( "Enter the speed :\n" );
        scanf  ( " %s", el.elem.ash.info.spds);
        printf ( "Enter the direction (8-pt compass - N, NE, E, etc.):\n" );
        scanf  ( " %s", el.elem.ash.info.dir );
        printf ( "Enter the flight level #1:\n" );
        scanf  ( " %s", el.elem.ash.info.flvl1 );
        printf ( "Enter the flight level #2:\n" );
        scanf  ( " %s", el.elem.ash.info.flvl2 );

        for ( ii = 0, jj = np; ii < np; ii++, jj++) {
             el.elem.ash.latlon[ii] = lat[ii];
             el.elem.ash.latlon[jj] = lon[ii];
        }
    }
    
    else if ( el.hdr.vg_type == VOLC_ELM ) {
        el.hdr.recsz = (int)(sizeof (VG_HdrStruct) + sizeof (VolInfo) +
                        (size_t)(np * 4) * sizeof(float) );
        el.elem.ash.info.npts = np;
        printf ( "Please use '_' for BLANKS when typing in STRINGS.\n");
        printf ( "Enter volcano name (max %d chars) : \n", 
		(int)sizeof(el.elem.vol.info.name)-1  );
        scanf  ( " %s", el.elem.vol.info.name );
        printf ( "Enter volcano symbol code (201) : " );
        scanf  ( " %f", &el.elem.vol.info.code );
        printf ( "Enter the symbol size : " );
        scanf  ( " %f", &el.elem.vol.info.size );
        printf ( "Enter the symbol line width : " );
        scanf  ( " %d", &el.elem.vol.info.width );
        printf ( "Enter the Smithsonian number (eg., 1401-09, max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.number)-1 );
        scanf  ( " %s", el.elem.vol.info.number );
        printf ( "Enter the volcano area (eg., mexico, max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.area)-1 );
        scanf  ( " %s", el.elem.vol.info.area );
        printf ( "Enter the volcano elevation (meters, max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.elev)-1 );
        scanf  ( " %s", el.elem.vol.info.elev );
        printf ( "Enter the originating station (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.origstn)-1 );
        scanf  ( " %s", el.elem.vol.info.origstn );
        printf ( "Enter the VAAC (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.vaac)-1 );
        scanf  ( " %s", el.elem.vol.info.vaac );
        printf ( "Enter the WMO ID (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.wmoid)-1 );
        scanf  ( " %s", el.elem.vol.info.wmoid );
        printf ( "Enter the header number (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.hdrnum)-1 );
        scanf  ( " %s", el.elem.vol.info.hdrnum );
        printf ( "Enter the year (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.year)-1 );
        scanf  ( " %s", el.elem.vol.info.year );
        printf ( "Enter the advisory number (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.advnum)-1 );
        scanf  ( " %s", el.elem.vol.info.advnum );
        printf ( "Enter the information source (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.infosorc)-1 );
        scanf  ( " %s", el.elem.vol.info.infosorc );
        printf ( "Enter the additional info source (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.addlsorc)-1 );
        scanf  ( " %s", el.elem.vol.info.addlsorc );
        printf ( "Enter the details (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.details)-1 );
        scanf  ( " %s", el.elem.vol.info.details );
        printf ( "Enter the obs date (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.obsdate)-1 );
        scanf  ( " %s", el.elem.vol.info.obsdate );
        printf ( "Enter the obs time (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.obstime)-1 );
        scanf  ( " %s", el.elem.vol.info.obstime );
        printf ( "Enter the obsashcld (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.obsashcld)-1 );
        scanf  ( " %s", el.elem.vol.info.obsashcld );
        printf ( "Enter the fcst_06 (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.fcst_06)-1 );
        scanf  ( " %s", el.elem.vol.info.fcst_06 );
        printf ( "Enter the fcst_12 (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.fcst_12)-1 );
        scanf  ( " %s", el.elem.vol.info.fcst_12 );
        printf ( "Enter the fcst_18 (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.fcst_18)-1 );
        scanf  ( " %s", el.elem.vol.info.fcst_18 );
        printf ( "Enter the remarks (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.remarks)-1 );
        scanf  ( " %s", el.elem.vol.info.remarks );
        printf ( "Enter the next advisory (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.nextadv)-1 );
        scanf  ( " %s", el.elem.vol.info.nextadv );
        printf ( "Enter the fcstrs (max %d chars) : ",
	      (int)sizeof(el.elem.vol.info.fcstrs)-1 );
        scanf  ( " %s", el.elem.vol.info.fcstrs );

	np = 1;

        printf ( "Enter the volcano latitude : " );
        scanf  ( " %f", &el.elem.vol.latlon[0] );
	lat[0] = el.elem.vol.latlon[0];
        printf ( "Enter the volcano longitude : " );
        scanf  ( " %f", &el.elem.vol.latlon[1] );
	lon[0] = el.elem.vol.latlon[1];

        printf ( "Enter the plotting offset_x : " );
        scanf  ( " %d", &el.elem.vol.offset_xy[0] );
        printf ( "Enter the plotting offset_x : " );
        scanf  ( " %d", &el.elem.vol.offset_xy[1] );

    }
    
    printf("Closed? 0 = No 1 = Yes \n");
    scanf( " %i", &cls);
    el.hdr.closed = (char)cls;
    printf("Filled? 0 = No 1 = Yes \n");
    scanf( " %i", &fil);
    el.hdr.filled = (char)fil;
    printf ("Smoothed? 0 = No 1 = Light 2 = Heavy \n");
    scanf (" %i", &sm);
    el.hdr.smooth = (char)sm;
    
    printf(" call cvg_crthdr \n");
    cvg_crthdr( &el, np, lat, lon, &ier); /* Set header */
    printf(" cvg_crthdr done\n");
    
    if ( version == CUR_SIG_VER ) {
        printf(" call cvg_writefD \n");
        cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
        printf(" cvg_writefD done \n");
    }

    /*
     *   Build v0 SIGMET from v1 if necessary (except CCF).
     */
    if ( el.hdr.vg_type != SIGCCF_ELM &&
         el.hdr.vg_type != ASHCLD_ELM &&
         el.hdr.vg_type != VOLC_ELM  &&  version == 0 ) {
	v0_el.hdr.delete = (char)el.hdr.delete;
	v0_el.hdr.vg_class = (char)el.hdr.vg_class;
        v0_el.hdr.vg_type = (char)el.hdr.vg_type;
        v0_el.hdr.filled = (char)el.hdr.filled;
        v0_el.hdr.closed = (char)el.hdr.closed;
        v0_el.hdr.smooth = (char)el.hdr.smooth;
	v0_el.hdr.version = (char)version;
        v0_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v0_el.hdr.grpnum = el.hdr.grpnum;        
        v0_el.hdr.maj_col = el.hdr.maj_col;        
        v0_el.hdr.min_col = el.hdr.min_col;        
        v0_el.hdr.recsz = (int)(sizeof (VG_HdrStruct) +
			sizeof (float) * (size_t)(np * 2) + 
			sizeof (v0_SigmetInfo));
        v0_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v0_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v0_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v0_el.hdr.range_max_lon = el.hdr.range_max_lon;        

        v0_el.elem.sig.info.npts = np;
        v0_el.elem.sig.info.subtype = el.elem.sig.info.subtype;	  
        v0_el.elem.sig.info.lintyp = el.elem.sig.info.lintyp;
        v0_el.elem.sig.info.linwid = el.elem.sig.info.linwid;
        v0_el.elem.sig.info.sol = el.elem.sig.info.sol;        
	strcpy( v0_el.elem.sig.info.area, el.elem.sig.info.area ); 
        strcpy( v0_el.elem.sig.info.fir, el.elem.sig.info.fir );	 
        v0_el.elem.sig.info.status = el.elem.sig.info.status;
        v0_el.elem.sig.info.distance = el.elem.sig.info.distance;	
        strcpy( v0_el.elem.sig.info.msgid, el.elem.sig.info.msgid );	
        v0_el.elem.sig.info.seqnum = el.elem.sig.info.seqnum;	
        strcpy( v0_el.elem.sig.info.stime, el.elem.sig.info.stime );
        strcpy( v0_el.elem.sig.info.etime, el.elem.sig.info.etime );
        strcpy( v0_el.elem.sig.info.remarks, el.elem.sig.info.remarks);	
        v0_el.elem.sig.info.sonic = el.elem.sig.info.sonic;	
        strcpy( v0_el.elem.sig.info.phenom, el.elem.sig.info.phenom );
        strcpy( v0_el.elem.sig.info.trend, el.elem.sig.info.trend );
        strcpy( v0_el.elem.sig.info.move, el.elem.sig.info.move );	
        v0_el.elem.sig.info.obsfcst = el.elem.sig.info.obsfcst;	
        strcpy( v0_el.elem.sig.info.obstime, el.elem.sig.info.obstime);	
        v0_el.elem.sig.info.fl = el.elem.sig.info.fl;
        v0_el.elem.sig.info.spd = el.elem.sig.info.spd;	
        strcpy( v0_el.elem.sig.info.dir, el.elem.sig.info.dir );
        strcpy( v0_el.elem.sig.info.tops, el.elem.sig.info.tops );
        strcpy( v0_el.elem.sig.info.fcstr, el.elem.sig.info.fcstr);
	
        for ( ii = 0, jj = np; ii < np; ii++, jj++) {
	    v0_el.elem.sig.latlon[ii] = lat[ii];
	    v0_el.elem.sig.latlon[jj] = lon[ii];
	}
		
        cvg_writv0_elm( &v0_el, start, v0_el.hdr.recsz, fname, 
	                &loc, &ier); 	
     }     
     
     if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crttrk ( char *fname, int *iret )
/************************************************************************
 * cvg_crttrk								*
 *									*
 * This function creates a STORM TRACK element & writes to file         *
 *									*
 * cvg_crttrk ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{
    int		np, ii, jj, start, loc, ier;
    float	lat[MAX_TRACKS], lon[MAX_TRACKS];
    fdttms_t 	times[MAX_TRACKS];
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */

    el.hdr.vg_class	= CLASS_TRACKS;
    el.hdr.vg_type	= TRKSTORM_ELM;

    printf ( "Storm Track attributes:\n" );
    printf ( "Enter the track subtype:\n" );
    scanf ( " %i", &el.elem.trk.info.subtype );
    printf ( "Enter the initial line type:\n" );
    scanf ( " %i", &el.elem.trk.info.ltype1 );
    printf ( "Enter the extrapolated line type:\n" );
    scanf ( " %i", &el.elem.trk.info.ltype2 );
    printf ( "Enter the line width size multiplier:\n" );
    scanf ( " %i", &el.elem.trk.info.width);

    printf ( "Enter the initial marker type:\n" );
    scanf ( " %i", &el.elem.trk.info.mtype1 );
    printf ( "Enter the extrapolated marker type:\n" );
    scanf ( " %i", &el.elem.trk.info.mtype2 );

    printf ("Enter the time increment in minutes:\n" );
    scanf ( " %i", &el.elem.trk.info.incr );
    printf ("Enter the storm speed (m/s):\n" );
    scanf ( " %f", &el.elem.trk.info.speed );
    printf ("Enter the storm direction (degrees):\n" );
    scanf ( " %f", &el.elem.trk.info.dir );

    printf ("Enter the # of initial points along the line:\n" );
    scanf ( " %i", &el.elem.trk.info.nipts );
    printf ("Enter the total # of points along the line:\n" );
    scanf ( " %i", &np );
       
    if ( np <= 0 || np > MAX_TRACKS ) {
        *iret = -28;
	return;
    }

    printf ( "Enter %i points (lat,lon,time string) (MAP):\n", np );
    for ( ii= 0; ii < np; ii++ )
        scanf ( " %f %f %s", &(lat[ii]), &(lon[ii]), times[ii] );
    printf ( "Enter the skip factor:\n" );
    scanf ( " %i", &el.elem.trk.info.skip );
    printf ( "Enter the text font(itxfn):\n" );
    scanf ( " %i", &el.elem.trk.info.itxfn );
    printf ( "Enter the text type flag (ithw):\n" );
    scanf ( " %i", &el.elem.trk.info.ithw );
    printf ( "Enter the text size:\n" );
    scanf ( " %f", &el.elem.trk.info.sztext );

    el.hdr.recsz = (sizeof (VG_HdrStruct) + sizeof (TrackType)); 
    el.elem.trk.info.npts = np;
    cvg_crthdr( &el, np, lat, lon, &ier);
    
    for (ii = 0, jj = np; ii < np; ii++, jj++) {
	el.elem.trk.latlon[ii] = lat[ii];
	el.elem.trk.latlon[jj] = lon[ii];
	strcpy ( el.elem.trk.info.times[ii], times[ii] );
    }
     
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtwbx ( char *fname, int *iret )
/************************************************************************
 * cvg_crtwbx								*
 *									*
 * This function creates a WATCH BOX element & writes to file           *
 *									*
 * cvg_crtwbx ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * J. Wu/GSC	01/01	Added WATCH BOX old version 0, 1, 2		*
 * J. Wu/SAIC	06/02	update for version 4 & 5			*
 * H. Zeng/XTRIA01/03   added version 6                                 *
 * T. Lee/SAIC	04/03	fixed watch states and adjacent states input	*
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{
    int			np, ns, ii, start, loc, ier, nm, version;
    float		lat[MAXPTS], lon[MAXPTS];
    char		sel[LLSCRN];
    char	        state[4];
    VG_DBStruct		el;
    v0_VG_DBStruct	v0_el;
    v1_VG_DBStruct	v1_el;
    v2_VG_DBStruct	v2_el;
    v3_VG_DBStruct	v3_el;
    v4_VG_DBStruct	v4_el;
    v5_VG_DBStruct	v5_el;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */

    el.hdr.vg_class = CLASS_WATCHES;
    el.hdr.vg_type = WBOX_ELM;

    printf ( "Enter the watch box version number:\n" );
    for ( ii = 0; ii <= CUR_WBX_VER; ii++ )
        printf ( "\t%d - WATCH BOX Version %d\n", ii, ii );                
    scanf ( " %s", sel );
    version = atoi ( sel );       
    
    if ( version < 0 || version > CUR_WBX_VER ) version = CUR_WBX_VER;        
    printf ( "Creating version %d watch box...\n", version );       
    
    printf ( "Enter the # of points along the box (0-500):\n" );
    scanf ( " %i", &np );
    
    if ( np <= 0 || np > MAXPTS ) {
        *iret = -28;
	return;
    }

    printf ( "Enter %i coordinate pairs in MAP coordinate:\n", np );
    for ( ii = 0; ii < np; ii++ )
        scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );

    /* 
     *  Get watch definition information.
     */
    printf ( "Enter the watch style (6 - WBC    4 - PGRAM):\n" );
    scanf ( " %i", &el.elem.wbx.info.w_style );
    printf ( "Enter the PGRAM watch shape (1-NS  2-EW  3-ESOL):\n" );
    scanf ( " %i", &el.elem.wbx.info.w_shape );
    
    if ( version >= 3 ) {
        printf ( "Enter the watch anchor point #1 station id:\n" );
        scanf ( " %s", el.elem.wbx.info.w_a0id );
        printf ( "Enter the watch anchor point #1 latitude:\n" );
        scanf ( " %f", &el.elem.wbx.info.w_a0lt );
        printf ( "Enter the watch anchor point #1 longitude:\n" );
        scanf ( " %f", &el.elem.wbx.info.w_a0ln );
        printf ( "Enter the watch anchor point #1 distance (sm):\n" );
        scanf ( " %i", &el.elem.wbx.info.w_a0dis );
        printf ( "Enter the watch anchor point #1 dir (16-pt):\n" );
        scanf ( " %s", el.elem.wbx.info.w_a0dir );
        printf ( "Enter the watch anchor point #2 station id:\n" );
        scanf ( " %s", el.elem.wbx.info.w_a1id );
        printf ( "Enter the watch anchor point #2 latitude:\n" );
        scanf ( " %f", &el.elem.wbx.info.w_a1lt );
        printf ( "Enter the watch anchor point #2 longitude:\n" );
        scanf ( " %f", &el.elem.wbx.info.w_a1ln );
        printf ( "Enter the watch anchor point #2 distance (sm):\n" );
        scanf ( " %i", &el.elem.wbx.info.w_a1dis );
        printf ( "Enter the watch anchor point #2 dir (16-pt):\n" );
        scanf ( " %s", el.elem.wbx.info.w_a1dir );
    }
    
    /* 
     *  Get watch formatting and issuance relevant information.
     */    
    printf ( "Enter the watch number:\n" );
    scanf ( " %i", &el.elem.wbx.info.w_number );
    printf ( "Enter the watch filename:\n" );
    scanf ( " %s", el.elem.wbx.info.w_file );    
    printf ( "Enter the watch type (2 - TORNADO   6 - SVR T-STM):\n" );
    scanf ( " %i", &el.elem.wbx.info.w_type );
    
    if ( version >= 1 ) {   
        printf ( "Enter the watch issuing status (0 - N, 1 - Y):\n" );
        scanf ( " %i", &el.elem.wbx.info.w_istat );
        printf ( "Enter the watch issue time:\n" );
        scanf ( " %s", el.elem.wbx.info.w_iss_t );
        printf ( "Enter the watch expiration time:\n" );
        scanf ( " %s", el.elem.wbx.info.w_exp_t );    
        printf ( "Enter the watch severity:\n" );
        scanf ( " %i", &el.elem.wbx.info.w_severity );    
        printf ( "Enter the watch primary time zone:\n" );
        scanf ( " %s", el.elem.wbx.info.w_timezone );    
        printf ( "Enter the watch hail size:\n" );
        scanf ( " %s", el.elem.wbx.info.w_hailsz );    
        printf ( "Enter the watch wind gust:\n" );
        scanf ( " %s", el.elem.wbx.info.w_windg );    
        printf ( "Enter the watch tops:\n" );
        scanf ( " %s", el.elem.wbx.info.w_tops );    
        printf ( "Enter the watch mean storm motion vector (dir):\n" );
        scanf ( " %s", el.elem.wbx.info.w_msmv_d );    
        printf ( "Enter the watch mean storm motion vector (spd):\n" );
        scanf ( " %s", el.elem.wbx.info.w_msmv_s );    
        printf ( "Enter # of states included:\n" );
	scanf ( " %i", &ns );
        printf ( "Enter the watch states included:\n" );
	ii = 0;
	while ( ii < ns ) {
	  scanf (" %s", state );
	  strcat ( state, " ");
	  strcat ( el.elem.wbx.info.w_states, state );
	  ii++;
	}

        printf ( "Enter # of adjacent states included:\n" );
	scanf ( " %i", &ns );
        printf ( "Enter the adjacent states included:\n" );
	ii = 0;
	while ( ii < ns ) {
	  scanf (" %s", state );
	  strcat ( state, " ");
	  strcat ( el.elem.wbx.info.w_adjarea, state );
	  ii++;
	}

        printf ( "Enter the watch replacement watch numbers:\n" );
        scanf ( " %s", el.elem.wbx.info.w_replw );    
        printf ( "Enter the watch issuing forecaster name(s):\n" );
        scanf ( " %s", el.elem.wbx.info.w_fcstr );    
        printf ( "Enter the watch flag for issuance: 0 - N, 1 - Y:\n" );
        scanf ( " %i", &el.elem.wbx.info.w_issued );
    }
    
    /* 
     *  Get watch status message formatting & issuance relevant info
     *  (version 1 & up )
     */
    if ( version >= 1 ) {
        printf ( "Enter the wsm issue time:\n" );
        scanf ( " %s", el.elem.wbx.info.wsm_iss_t);    
        printf ( "Enter the wsm expiration time:\n" );
        scanf ( " %s", el.elem.wbx.info.wsm_exp_t);    
        printf ( "Enter the wsm reference direction:\n" );
        scanf ( " %s", el.elem.wbx.info.wsm_ref );    
        printf ( "Enter the wsm most recent -from- line:\n" );
        scanf ( " %s", el.elem.wbx.info.wsm_from );    
        printf ( "Enter the wsm mesoscale discussion number:\n" );
        scanf ( " %s", el.elem.wbx.info.wsm_meso );    
        printf ( "Enter the wsm issuing forecaster name(s):\n" );
        scanf ( " %s", el.elem.wbx.info.wsm_fcstr );    
    }
    
    /* 
     * Get county information ( version 2 & up )
     */
    if ( version >= 2 ) {
        printf ( "Enter the number of counties:\n" );
        scanf ( " %i", &nm );
        el.elem.wbx.info.numcnty = nm;
        printf ( "Enter the county plot flag:\n" );
        scanf ( " %i", &el.elem.wbx.info.cn_flag );
	if ( nm > 0 ) 
	    printf ( "Enter %i county ( FIPS, lat, lon ) triples\n", nm );
        for ( ii = 0; ii < nm; ii++ )
            scanf ( " %i %f %f", &el.elem.wbx.info.cn_fips[ii],
	                         &el.elem.wbx.info.cn_ltln[ii],
	                         &el.elem.wbx.info.cn_ltln[ii + nm] );	
    }

    /*
     * Get marker information ( version 6 & up )
     */
    if ( version >= 6) {
        printf ( "Enter the marker type ( 1-21 ):\n" );
        scanf ( " %i", &el.elem.wbx.info.w_mrktyp );
        printf ( "Enter the marker size ( 0.1-10.0 ):\n" );
        scanf ( " %f", &el.elem.wbx.info.w_mrksiz );
        printf ( "Enter the marker width ( 1-10 ):\n" );
        scanf ( " %i", &el.elem.wbx.info.w_mrkwid );
    }

    cvg_crthdr( &el, np, lat, lon, &ier);  /* set header */	          

    /*
     *   Build old version v0, v1, v2, v3, v4 from v5 if necessary.
     */    
    if ( version == 0 ) {
        v0_el.hdr.delete = (char)el.hdr.delete;
	v0_el.hdr.vg_class = (char)el.hdr.vg_class;
        v0_el.hdr.vg_type = (char)el.hdr.vg_type;
        v0_el.hdr.filled = (char)el.hdr.filled;
        v0_el.hdr.closed = (char)el.hdr.closed;
        v0_el.hdr.smooth = (char)el.hdr.smooth;
	v0_el.hdr.version = (char)version;
        v0_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v0_el.hdr.grpnum = el.hdr.grpnum;        
        v0_el.hdr.maj_col = el.hdr.maj_col;        
        v0_el.hdr.min_col = el.hdr.min_col;        
        v0_el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct ) 
	                  + sizeof( v0_WatchBoxInfo ) );	
        v0_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v0_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v0_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v0_el.hdr.range_max_lon = el.hdr.range_max_lon;        
      
	v0_el.elem.wbx.info.numpts = np;
        v0_el.elem.wbx.info.w_style = el.elem.wbx.info.w_style;
        v0_el.elem.wbx.info.w_type = el.elem.wbx.info.w_type;
        v0_el.elem.wbx.info.w_number = el.elem.wbx.info.w_number;
        v0_el.elem.wbx.info.w_shape = el.elem.wbx.info.w_shape;
	strcpy( v0_el.elem.wbx.info.w_file, el.elem.wbx.info.w_file ); 
	
        for ( ii = 0; ii < np; ii++ ) {
            v0_el.elem.wbx.latlon[ii] = lat[ii];
            v0_el.elem.wbx.latlon[ii + np] = lon[ii];
        }

	
	cvg_writv0_elm( &v0_el, start, v0_el.hdr.recsz, fname, 
	                &loc, &ier ); 	    
    }

    else if ( version == 1 ) {
        v1_el.hdr.delete = (char)el.hdr.delete;
	v1_el.hdr.vg_class = (char)el.hdr.vg_class;
        v1_el.hdr.vg_type = (char)el.hdr.vg_type;
        v1_el.hdr.filled = (char)el.hdr.filled;
        v1_el.hdr.closed = (char)el.hdr.closed;
        v1_el.hdr.smooth = (char)el.hdr.smooth;
	v1_el.hdr.version = (char)version;
        v1_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v1_el.hdr.grpnum = el.hdr.grpnum;        
        v1_el.hdr.maj_col = el.hdr.maj_col;        
        v1_el.hdr.min_col = el.hdr.min_col;        
        v1_el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct) 
	                  + sizeof( v1_WatchBoxInfo ) );	
        v1_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v1_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v1_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v1_el.hdr.range_max_lon = el.hdr.range_max_lon;        

        v1_el.elem.wbx.info.numpts = np;
        v1_el.elem.wbx.info.w_style = el.elem.wbx.info.w_style;
        v1_el.elem.wbx.info.w_type = el.elem.wbx.info.w_type;
        v1_el.elem.wbx.info.w_number = el.elem.wbx.info.w_number;
        v1_el.elem.wbx.info.w_shape = el.elem.wbx.info.w_shape;
	strcpy( v1_el.elem.wbx.info.w_file, el.elem.wbx.info.w_file ); 

        v1_el.elem.wbx.info.w_istat = el.elem.wbx.info.w_istat;
	strcpy( v1_el.elem.wbx.info.w_iss_t, el.elem.wbx.info.w_iss_t ); 
	strcpy( v1_el.elem.wbx.info.w_exp_t, el.elem.wbx.info.w_exp_t ); 
        v1_el.elem.wbx.info.w_severity = el.elem.wbx.info.w_severity;
 	strcpy( v1_el.elem.wbx.info.w_timezone, el.elem.wbx.info.w_timezone);
 	strcpy( v1_el.elem.wbx.info.w_hailsz, el.elem.wbx.info.w_hailsz ); 
 	strcpy( v1_el.elem.wbx.info.w_windg, el.elem.wbx.info.w_windg ); 
 	strcpy( v1_el.elem.wbx.info.w_tops, el.elem.wbx.info.w_tops ); 
 	strcpy( v1_el.elem.wbx.info.w_msmv_d, el.elem.wbx.info.w_msmv_d );
 	strcpy( v1_el.elem.wbx.info.w_msmv_s, el.elem.wbx.info.w_msmv_s );
 	strcpy( v1_el.elem.wbx.info.w_states, el.elem.wbx.info.w_states );
	strcpy( v1_el.elem.wbx.info.w_adjarea, el.elem.wbx.info.w_adjarea );
	strcpy( v1_el.elem.wbx.info.w_replw, el.elem.wbx.info.w_replw );
	strcpy( v1_el.elem.wbx.info.w_fcstr, el.elem.wbx.info.w_fcstr );
        v1_el.elem.wbx.info.w_issued = el.elem.wbx.info.w_issued;
	
	strcpy( v1_el.elem.wbx.info.wsm_iss_t, el.elem.wbx.info.wsm_iss_t );
	strcpy( v1_el.elem.wbx.info.wsm_exp_t, el.elem.wbx.info.wsm_exp_t );
	strcpy( v1_el.elem.wbx.info.wsm_ref, el.elem.wbx.info.wsm_ref ); 
	strcpy( v1_el.elem.wbx.info.wsm_from, el.elem.wbx.info.wsm_from );
	strcpy( v1_el.elem.wbx.info.wsm_meso, el.elem.wbx.info.wsm_meso );
	strcpy( v1_el.elem.wbx.info.wsm_fcstr, el.elem.wbx.info.wsm_fcstr );
	 
        for ( ii = 0; ii < np; ii++ ) {
            v1_el.elem.wbx.latlon[ii] = lat[ii];
            v1_el.elem.wbx.latlon[ii + np] = lon[ii];
        }

        cvg_writv1_elm( &v1_el, start, v1_el.hdr.recsz, fname, 
	                &loc, &ier ); 	        
    }

    else if ( version == 2 ) {
        v2_el.hdr.delete = (char)el.hdr.delete;
	v2_el.hdr.vg_class = (char)el.hdr.vg_class;
        v2_el.hdr.vg_type = (char)el.hdr.vg_type;
        v2_el.hdr.filled = (char)el.hdr.filled;
        v2_el.hdr.closed = (char)el.hdr.closed;
        v2_el.hdr.smooth = (char)el.hdr.smooth;
	v2_el.hdr.version = (char)version;
        v2_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v2_el.hdr.grpnum = el.hdr.grpnum;        
        v2_el.hdr.maj_col = el.hdr.maj_col;        
        v2_el.hdr.min_col = el.hdr.min_col;        
        v2_el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct )
	                  + sizeof( v2_WatchBoxInfo ) );	
        v2_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v2_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v2_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v2_el.hdr.range_max_lon = el.hdr.range_max_lon;        

        v2_el.elem.wbx.info.numpts = np;
        v2_el.elem.wbx.info.w_style = el.elem.wbx.info.w_style;
        v2_el.elem.wbx.info.w_type = el.elem.wbx.info.w_type;
        v2_el.elem.wbx.info.w_number = el.elem.wbx.info.w_number;
        v2_el.elem.wbx.info.w_shape = el.elem.wbx.info.w_shape;
	strcpy( v2_el.elem.wbx.info.w_file, el.elem.wbx.info.w_file ); 

        v2_el.elem.wbx.info.w_istat = el.elem.wbx.info.w_istat;
	strcpy( v2_el.elem.wbx.info.w_iss_t, el.elem.wbx.info.w_iss_t ); 
	strcpy( v2_el.elem.wbx.info.w_exp_t, el.elem.wbx.info.w_exp_t ); 
        v2_el.elem.wbx.info.w_severity = el.elem.wbx.info.w_severity;
 	strcpy( v2_el.elem.wbx.info.w_timezone, el.elem.wbx.info.w_timezone ); 
 	strcpy( v2_el.elem.wbx.info.w_hailsz, el.elem.wbx.info.w_hailsz ); 
 	strcpy( v2_el.elem.wbx.info.w_windg, el.elem.wbx.info.w_windg ); 
 	strcpy( v2_el.elem.wbx.info.w_tops, el.elem.wbx.info.w_tops ); 
 	strcpy( v2_el.elem.wbx.info.w_msmv_d, el.elem.wbx.info.w_msmv_d ); 
 	strcpy( v2_el.elem.wbx.info.w_msmv_s, el.elem.wbx.info.w_msmv_s ); 
 	strcpy( v2_el.elem.wbx.info.w_states, el.elem.wbx.info.w_states ); 
	strcpy( v2_el.elem.wbx.info.w_adjarea, el.elem.wbx.info.w_adjarea ); 
	strcpy( v2_el.elem.wbx.info.w_replw, el.elem.wbx.info.w_replw ); 
	strcpy( v2_el.elem.wbx.info.w_fcstr, el.elem.wbx.info.w_fcstr ); 
        v2_el.elem.wbx.info.w_issued = el.elem.wbx.info.w_issued;
	
	strcpy( v2_el.elem.wbx.info.wsm_iss_t, el.elem.wbx.info.wsm_iss_t ); 
	strcpy( v2_el.elem.wbx.info.wsm_exp_t, el.elem.wbx.info.wsm_exp_t ); 
	strcpy( v2_el.elem.wbx.info.wsm_ref, el.elem.wbx.info.wsm_ref ); 
	strcpy( v2_el.elem.wbx.info.wsm_from, el.elem.wbx.info.wsm_from ); 
	strcpy( v2_el.elem.wbx.info.wsm_meso, el.elem.wbx.info.wsm_meso ); 
	strcpy( v2_el.elem.wbx.info.wsm_fcstr, el.elem.wbx.info.wsm_fcstr ); 
        
	v2_el.elem.wbx.info.numcnty = nm;
	v2_el.elem.wbx.info.cn_flag = el.elem.wbx.info.cn_flag;
        for ( ii = 0; ii < nm; ii++ ) {
            v2_el.elem.wbx.info.cn_stat[ii] = G_TRUE;
            v2_el.elem.wbx.info.cn_ltln[ii] = el.elem.wbx.info.cn_ltln[ii];
            v2_el.elem.wbx.info.cn_ltln[ii + nm] 
	                                 = el.elem.wbx.info.cn_ltln[ii + nm];
        }
        
        for ( ii = 0; ii < np; ii++ ) {
            v2_el.elem.wbx.latlon[ii] = lat[ii];
            v2_el.elem.wbx.latlon[ii + np] = lon[ii];
        }
	
        cvg_writv2_elm( &v2_el, start, v2_el.hdr.recsz, fname, 
	                &loc, &ier ); 	       
    }
    
    else if ( version == 3 ) {
        v3_el.hdr.delete = (char)el.hdr.delete;
	v3_el.hdr.vg_class = (char)el.hdr.vg_class;
        v3_el.hdr.vg_type = (char)el.hdr.vg_type;
        v3_el.hdr.filled = (char)el.hdr.filled;
        v3_el.hdr.closed = (char)el.hdr.closed;
        v3_el.hdr.smooth = (char)el.hdr.smooth;
	v3_el.hdr.version = (char)version;
        v3_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v3_el.hdr.grpnum = el.hdr.grpnum;        
        v3_el.hdr.maj_col = el.hdr.maj_col;        
        v3_el.hdr.min_col = el.hdr.min_col;        
        v3_el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct )
	                  + sizeof( v3_WatchBoxInfo ) );	
        v3_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v3_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v3_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v3_el.hdr.range_max_lon = el.hdr.range_max_lon;        

        v3_el.elem.wbx.info.numpts = np;
        v3_el.elem.wbx.info.w_style = el.elem.wbx.info.w_style;
        v3_el.elem.wbx.info.w_type = el.elem.wbx.info.w_type;
        v3_el.elem.wbx.info.w_number = el.elem.wbx.info.w_number;
        v3_el.elem.wbx.info.w_shape = el.elem.wbx.info.w_shape;
	strcpy( v3_el.elem.wbx.info.w_file, el.elem.wbx.info.w_file ); 

        v3_el.elem.wbx.info.w_istat = el.elem.wbx.info.w_istat;
	strcpy( v3_el.elem.wbx.info.w_iss_t, el.elem.wbx.info.w_iss_t ); 
	strcpy( v3_el.elem.wbx.info.w_exp_t, el.elem.wbx.info.w_exp_t ); 
        v3_el.elem.wbx.info.w_severity = el.elem.wbx.info.w_severity;
 	strcpy( v3_el.elem.wbx.info.w_timezone, el.elem.wbx.info.w_timezone ); 
 	strcpy( v3_el.elem.wbx.info.w_hailsz, el.elem.wbx.info.w_hailsz ); 
 	strcpy( v3_el.elem.wbx.info.w_windg, el.elem.wbx.info.w_windg ); 
 	strcpy( v3_el.elem.wbx.info.w_tops, el.elem.wbx.info.w_tops ); 
 	strcpy( v3_el.elem.wbx.info.w_msmv_d, el.elem.wbx.info.w_msmv_d ); 
 	strcpy( v3_el.elem.wbx.info.w_msmv_s, el.elem.wbx.info.w_msmv_s ); 
 	strcpy( v3_el.elem.wbx.info.w_states, el.elem.wbx.info.w_states ); 
	strcpy( v3_el.elem.wbx.info.w_adjarea, el.elem.wbx.info.w_adjarea ); 
	strcpy( v3_el.elem.wbx.info.w_replw, el.elem.wbx.info.w_replw ); 
	strcpy( v3_el.elem.wbx.info.w_fcstr, el.elem.wbx.info.w_fcstr ); 
        v3_el.elem.wbx.info.w_issued = el.elem.wbx.info.w_issued;
	
	strcpy( v3_el.elem.wbx.info.wsm_iss_t, el.elem.wbx.info.wsm_iss_t ); 
	strcpy( v3_el.elem.wbx.info.wsm_exp_t, el.elem.wbx.info.wsm_exp_t ); 
	strcpy( v3_el.elem.wbx.info.wsm_ref, el.elem.wbx.info.wsm_ref ); 
	strcpy( v3_el.elem.wbx.info.wsm_from, el.elem.wbx.info.wsm_from ); 
	strcpy( v3_el.elem.wbx.info.wsm_meso, el.elem.wbx.info.wsm_meso ); 
	strcpy( v3_el.elem.wbx.info.wsm_fcstr, el.elem.wbx.info.wsm_fcstr ); 
        
	v3_el.elem.wbx.info.numcnty = nm;
	v3_el.elem.wbx.info.cn_flag = el.elem.wbx.info.cn_flag;
        for ( ii = 0; ii < nm; ii++ ) {
            v3_el.elem.wbx.info.cn_stat[ii] = G_TRUE;
            v3_el.elem.wbx.info.cn_ltln[ii] = el.elem.wbx.info.cn_ltln[ii];
            v3_el.elem.wbx.info.cn_ltln[ii + nm] 
	                                 = el.elem.wbx.info.cn_ltln[ii + nm];
        }
        
        for ( ii = 0; ii < np; ii++ ) {
            v3_el.elem.wbx.latlon[ii] = lat[ii];
            v3_el.elem.wbx.latlon[ii + np] = lon[ii];
        }
	
        cvg_writv3_elm( &v3_el, start, v3_el.hdr.recsz, fname, 
	                &loc, &ier ); 	       
    }
    
    else if ( version == 4 ) {
        v4_el.hdr.delete = (char)el.hdr.delete;
	v4_el.hdr.vg_class = (char)el.hdr.vg_class;
        v4_el.hdr.vg_type = (char)el.hdr.vg_type;
        v4_el.hdr.filled = (char)el.hdr.filled;
        v4_el.hdr.closed = (char)el.hdr.closed;
        v4_el.hdr.smooth = (char)el.hdr.smooth;
	v4_el.hdr.version = (char)version;
        v4_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v4_el.hdr.grpnum = el.hdr.grpnum;        
        v4_el.hdr.maj_col = el.hdr.maj_col;        
        v4_el.hdr.min_col = el.hdr.min_col;        
        v4_el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct )
	                  + sizeof( v4_WatchBoxInfo ) );	
        v4_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v4_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v4_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v4_el.hdr.range_max_lon = el.hdr.range_max_lon;        

        v4_el.elem.wbx.info.numpts = np;
        v4_el.elem.wbx.info.w_style = el.elem.wbx.info.w_style;
        v4_el.elem.wbx.info.w_type = el.elem.wbx.info.w_type;
        v4_el.elem.wbx.info.w_number = el.elem.wbx.info.w_number;
        v4_el.elem.wbx.info.w_shape = el.elem.wbx.info.w_shape;
	strcpy( v4_el.elem.wbx.info.w_file, el.elem.wbx.info.w_file ); 

        v4_el.elem.wbx.info.w_istat = el.elem.wbx.info.w_istat;
	strcpy( v4_el.elem.wbx.info.w_iss_t, el.elem.wbx.info.w_iss_t ); 
	strcpy( v4_el.elem.wbx.info.w_exp_t, el.elem.wbx.info.w_exp_t ); 
        v4_el.elem.wbx.info.w_severity = el.elem.wbx.info.w_severity;
 	strcpy( v4_el.elem.wbx.info.w_timezone, el.elem.wbx.info.w_timezone ); 
 	strcpy( v4_el.elem.wbx.info.w_hailsz, el.elem.wbx.info.w_hailsz ); 
 	strcpy( v4_el.elem.wbx.info.w_windg, el.elem.wbx.info.w_windg ); 
 	strcpy( v4_el.elem.wbx.info.w_tops, el.elem.wbx.info.w_tops ); 
 	strcpy( v4_el.elem.wbx.info.w_msmv_d, el.elem.wbx.info.w_msmv_d ); 
 	strcpy( v4_el.elem.wbx.info.w_msmv_s, el.elem.wbx.info.w_msmv_s ); 
 	strcpy( v4_el.elem.wbx.info.w_states, el.elem.wbx.info.w_states ); 
	strcpy( v4_el.elem.wbx.info.w_adjarea, el.elem.wbx.info.w_adjarea ); 
	strcpy( v4_el.elem.wbx.info.w_replw, el.elem.wbx.info.w_replw ); 
	strcpy( v4_el.elem.wbx.info.w_fcstr, el.elem.wbx.info.w_fcstr ); 
        v4_el.elem.wbx.info.w_issued = el.elem.wbx.info.w_issued;
	
	strcpy( v4_el.elem.wbx.info.wsm_iss_t, el.elem.wbx.info.wsm_iss_t ); 
	strcpy( v4_el.elem.wbx.info.wsm_exp_t, el.elem.wbx.info.wsm_exp_t ); 
	strcpy( v4_el.elem.wbx.info.wsm_ref, el.elem.wbx.info.wsm_ref ); 
	strcpy( v4_el.elem.wbx.info.wsm_from, el.elem.wbx.info.wsm_from ); 
	strcpy( v4_el.elem.wbx.info.wsm_meso, el.elem.wbx.info.wsm_meso ); 
	strcpy( v4_el.elem.wbx.info.wsm_fcstr, el.elem.wbx.info.wsm_fcstr ); 
        
	v4_el.elem.wbx.info.numcnty = nm;
	v4_el.elem.wbx.info.cn_flag = el.elem.wbx.info.cn_flag;
        for ( ii = 0; ii < nm; ii++ ) {
            v4_el.elem.wbx.info.cn_stat[ii] = G_TRUE;
            v4_el.elem.wbx.info.cn_fips[ii] = el.elem.wbx.info.cn_fips[ii];
            v4_el.elem.wbx.info.cn_ltln[ii] = el.elem.wbx.info.cn_ltln[ii];
            v4_el.elem.wbx.info.cn_ltln[ii + nm] 
	                                 = el.elem.wbx.info.cn_ltln[ii + nm];
        }
        
        for ( ii = 0; ii < np; ii++ ) {
            v4_el.elem.wbx.latlon[ii] = lat[ii];
            v4_el.elem.wbx.latlon[ii + np] = lon[ii];
        }
	
        cvg_writv4_elm( &v4_el, start, v4_el.hdr.recsz, fname, 
	                &loc, &ier ); 	       
    }    

    else if ( version == 5 ) {
        v5_el.hdr.delete = (char)el.hdr.delete;
	v5_el.hdr.vg_class = (char)el.hdr.vg_class;
        v5_el.hdr.vg_type = (char)el.hdr.vg_type;
        v5_el.hdr.filled = (char)el.hdr.filled;
        v5_el.hdr.closed = (char)el.hdr.closed;
        v5_el.hdr.smooth = (char)el.hdr.smooth;
	v5_el.hdr.version = (char)version;
        v5_el.hdr.grptyp = (char)el.hdr.grptyp;        
        v5_el.hdr.grpnum = el.hdr.grpnum;        
        v5_el.hdr.maj_col = el.hdr.maj_col;        
        v5_el.hdr.min_col = el.hdr.min_col;        
        v5_el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct )
	                  + sizeof( v5_WatchBoxInfo ) );	
        v5_el.hdr.range_min_lat = el.hdr.range_min_lat;        
        v5_el.hdr.range_min_lon = el.hdr.range_min_lon;        
        v5_el.hdr.range_max_lat = el.hdr.range_max_lat;        
        v5_el.hdr.range_max_lon = el.hdr.range_max_lon;        

        v5_el.elem.wbx.info.numpts = np;
        v5_el.elem.wbx.info.w_style = el.elem.wbx.info.w_style;
        v5_el.elem.wbx.info.w_type = el.elem.wbx.info.w_type;
        v5_el.elem.wbx.info.w_number = el.elem.wbx.info.w_number;
        v5_el.elem.wbx.info.w_shape = el.elem.wbx.info.w_shape;
	strcpy( v5_el.elem.wbx.info.w_file, el.elem.wbx.info.w_file ); 

        v5_el.elem.wbx.info.w_istat = el.elem.wbx.info.w_istat;
	strcpy( v5_el.elem.wbx.info.w_iss_t, el.elem.wbx.info.w_iss_t ); 
	strcpy( v5_el.elem.wbx.info.w_exp_t, el.elem.wbx.info.w_exp_t ); 
        v5_el.elem.wbx.info.w_severity = el.elem.wbx.info.w_severity;
 	strcpy( v5_el.elem.wbx.info.w_timezone, el.elem.wbx.info.w_timezone ); 
 	strcpy( v5_el.elem.wbx.info.w_hailsz, el.elem.wbx.info.w_hailsz ); 
 	strcpy( v5_el.elem.wbx.info.w_windg, el.elem.wbx.info.w_windg ); 
 	strcpy( v5_el.elem.wbx.info.w_tops, el.elem.wbx.info.w_tops ); 
 	strcpy( v5_el.elem.wbx.info.w_msmv_d, el.elem.wbx.info.w_msmv_d ); 
 	strcpy( v5_el.elem.wbx.info.w_msmv_s, el.elem.wbx.info.w_msmv_s ); 
 	strcpy( v5_el.elem.wbx.info.w_states, el.elem.wbx.info.w_states ); 
	strcpy( v5_el.elem.wbx.info.w_adjarea, el.elem.wbx.info.w_adjarea ); 
	strcpy( v5_el.elem.wbx.info.w_replw, el.elem.wbx.info.w_replw ); 
	strcpy( v5_el.elem.wbx.info.w_fcstr, el.elem.wbx.info.w_fcstr ); 
        v5_el.elem.wbx.info.w_issued = el.elem.wbx.info.w_issued;
	
	strcpy( v5_el.elem.wbx.info.wsm_iss_t, el.elem.wbx.info.wsm_iss_t ); 
	strcpy( v5_el.elem.wbx.info.wsm_exp_t, el.elem.wbx.info.wsm_exp_t ); 
	strcpy( v5_el.elem.wbx.info.wsm_ref, el.elem.wbx.info.wsm_ref ); 
	strcpy( v5_el.elem.wbx.info.wsm_from, el.elem.wbx.info.wsm_from ); 
	strcpy( v5_el.elem.wbx.info.wsm_meso, el.elem.wbx.info.wsm_meso ); 
	strcpy( v5_el.elem.wbx.info.wsm_fcstr, el.elem.wbx.info.wsm_fcstr ); 
        
	v5_el.elem.wbx.info.numcnty = nm;
	v5_el.elem.wbx.info.cn_flag = el.elem.wbx.info.cn_flag;
        for ( ii = 0; ii < nm; ii++ ) {
            v5_el.elem.wbx.info.cn_fips[ii] = el.elem.wbx.info.cn_fips[ii];
            v5_el.elem.wbx.info.cn_ltln[ii] = el.elem.wbx.info.cn_ltln[ii];
            v5_el.elem.wbx.info.cn_ltln[ii + nm] 
	                                 = el.elem.wbx.info.cn_ltln[ii + nm];
        }
        
        for ( ii = 0; ii < np; ii++ ) {
            v5_el.elem.wbx.latlon[ii] = lat[ii];
            v5_el.elem.wbx.latlon[ii + np] = lon[ii];
        }
	
        cvg_writv5_elm( &v5_el, start, v5_el.hdr.recsz, fname, 
	                &loc, &ier ); 	       
    }    










    else {  /* latest version 5 */
        el.hdr.recsz = (int)( sizeof(float) * 2 * (size_t)np + sizeof( VG_HdrStruct ) 
	              + sizeof( WatchBoxInfo ) );	
        el.elem.wbx.info.numpts = np;
        for ( ii = 0; ii < np; ii++ ) {
            el.elem.wbx.latlon[ii] = lat[ii];
            el.elem.wbx.latlon[ii + np] = lon[ii];
        }

        cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    }
    
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtcir ( char *fname, int *iret )
/************************************************************************
 * cvg_crtcir								*
 *									*
 * This function creates a CIRCLE element & writes to the given file    *
 *									*
 * cvg_crtcir ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{
    int		np, ii, start, loc, ier;
    float	lat[2], lon[2];
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    np = 2;

    el.hdr.vg_class = CLASS_CIRCLE;
    el.hdr.vg_type = CIRCLE_ELM;

    printf ( "Circle attributes:\n" );
    printf ( "Enter the circle type:\n" );
    scanf ( " %i", &el.elem.cir.info.lintyp );
    printf ( "Enter the circle type flag:\n" );
    scanf ( " %i", &el.elem.cir.info.lthw );
    printf ( "Enter the circle width size multiplier:\n" );
    scanf ( " %i", &el.elem.cir.info.width);
    printf ( "Enter the circle width flag:\n" );
    scanf ( " %i", &el.elem.cir.info.lwhw );
    printf("Enter the center latitude:\n");
    scanf (" %f", &lat[0]);
    printf("Enter the center longitude:\n");
    scanf (" %f", &lon[0]);
    printf("Enter the circumference latitude:\n");
    scanf (" %f", &lat[1]);
    printf("Enter the circumference longitude:\n");
    scanf (" %f", &lon[1]);
    
    el.hdr.recsz = (int)( (sizeof(float) * 2 * (size_t)np) + sizeof(VG_HdrStruct) + 
		 sizeof(LineInfo) );
    el.elem.cir.info.numpts = np;

    cvg_crthdr( &el, np, lat, lon, &ier);
	    
    for ( ii = 0; ii < np; ii++ ) {
        el.elem.cir.data.latlon[ii] = lat[ii];
        el.elem.cir.data.latlon[ii + 2] = lon[ii];
    }
      
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crthdr ( VG_DBStruct *el, int np, float *lat, float *lon, int *iret )
/************************************************************************
 * cvg_crthdr								*
 *									*
 * This function builds common header attributes for an VGF element	*
 *									*
 * cvg_crthdr ( el, np, lat, lon, iret )	                        *
 *									*
 * Input parameters:							*
 *	*el	VG_DBStruct	pointer to an element structure	        *
 *	 np	int		number of points 	                *
 *	*lat	float		latitude to initialize max/min range	*
 *	*lon	float		longitude to initialize max/min range   *
 *									*
 * Output parameters:							*
 *	*iret	int		Return code				*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	01/01	Created					        *
 ***********************************************************************/
{   
    int		gptyp, ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    
    cvg_initelm( el );

    /* 
     *   Get the major/minor color and group type/number.
     */    
    printf ( "Enter the major color:\n" );
    scanf ( " %i", &el->hdr.maj_col );
    printf ( "Enter the minor color:\n" );
    scanf ( " %i", &el->hdr.min_col );
    printf("Enter the group type (>=0):\n");
    scanf( " %i", &gptyp);
    el->hdr.grptyp = (char) gptyp;
    printf("Enter the group number (>=0):\n");
    scanf (" %i", &el->hdr.grpnum);

    /*
     *  Find the maximum and minimum range.
     */ 
    el->hdr.range_min_lon = lon[0];
    el->hdr.range_min_lat = lat[0];
    el->hdr.range_max_lon = lon[0];
    el->hdr.range_max_lat = lat[0];

    for ( ii = 0; ii < np; ii++ ) {
        if ( el->hdr.range_min_lon > lon[ii] )
             el->hdr.range_min_lon = lon[ii];
        if ( el->hdr.range_min_lat > lat[ii] )
             el->hdr.range_min_lat = lat[ii];
        if ( el->hdr.range_max_lon < lon[ii] )
             el->hdr.range_max_lon = lon[ii];
        if ( el->hdr.range_max_lat < lat[ii] )
             el->hdr.range_max_lat = lat[ii];
    }    
}

/*=====================================================================*/

void cvg_writv0_elm ( v0_VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writv0_elm							*
 *									*
 * This function writes a version 0 element to a vector graphics file.	*
 *									*
 * cvg_writv0_elm ( el, start, numbytes, fname, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		v0_VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-3  = seek to a bad location	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 **									*
 * Log:									*
 * J. Wu/GSC		01/01	Copied from cvgwritelm.c 	        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	outfile[FILE_FULLSZ], newfil[133];
    FILE	*fp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Find the proper file to write.
 */ 
   if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }

/*
 *  Check the proper color settings.
 */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 
    
/*
 *  Inquire if the file exists. If not, create it. 
 */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
    
/*
 *  Open file for update. 
 */
    fp = cfl_uopn( outfile, &ier ); 
    
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
         *iret = -1;
         return;
    }

/* 
 *  Seek to the specified location if open successfully and 
 *  record the starting location for the new element.
 */
    if ( start == -1 ) {
         cfl_seek( fp, 0, SEEK_END, &ier ); 
         *location = (int) fsize;
    }
    else {
         cfl_seek( fp, (long)start, SEEK_SET, &ier );
         *location = start;
    }
    
    if ( ier != 0 )  {
        *iret = -3;
 	cfl_clos( fp, &ier );
        if ( ier != 0 ) *iret = -2;
	return; 
    }

/*
 *  Swap bit order of VG elements if necessary.
 */
   if ( MTMACH == MTULTX ||
        MTMACH == MTALPH ||
        MTMACH == MTLNUX ) {
	if ( numbytes == (int)sizeof( VG_HdrStruct ) ) {
 	    cvg_swap_v0( SWPHDR, G_FALSE, *el, el, &ierr );  
	} 
 	else {
	    cvg_swap_v0( SWPALL, G_FALSE, *el, el, &ierr );  
	} 
    }	

/*
 *  Write to file & close.
 */    
    cfl_writ( fp, numbytes, (void *)el, &ier );  	
    if ( ier != 0 )  *iret = -17;

    cfl_clos( fp, &ier );
    if ( ier != 0 )  *iret = -2;
}

/*=====================================================================*/

void cvg_writv1_elm ( v1_VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writv1_elm							*
 *									*
 * This function writes an version 1 element to a vector graphics file.	*
 *									*
 * cvg_writv1_elm ( el, start, numbytes, fname, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		v1_VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-3  = seek to a bad location	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 **									*
 * Log:									*
 * J. Wu/GSC		01/01	Copied from cvgwritelm.c 	        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	outfile[FILE_FULLSZ], newfil[133];
    FILE	*fp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Find the proper file to write.
 */ 
   if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }

/*
 *  Check the proper color settings.
 */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 
    
/*
 *  Inquire if the file exists. If not, create it. 
 */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
    
/*
 *  Open file for update. 
 */
    fp = cfl_uopn( outfile, &ier ); 
    
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
         *iret = -1;
         return;
    }

/* 
 *  Seek to the specified location if open successfully and 
 *  record the starting location for the new element.
 */
    if ( start == -1 ) {
         cfl_seek( fp, 0, SEEK_END, &ier ); 
         *location = (int) fsize;
    }
    else {
         cfl_seek( fp, (long)start, SEEK_SET, &ier );
         *location = start;
    }
    
    if ( ier != 0 )  {
        *iret = -3;
 	cfl_clos( fp, &ier );
        if ( ier != 0 ) *iret = -2;
	return; 
    }

/*
 *  Swap bit order of VG elements if necessary.
 */
   if ( MTMACH == MTULTX ||
        MTMACH == MTALPH ||
        MTMACH == MTLNUX ) {
	if ( numbytes == (int)sizeof( VG_HdrStruct ) ) {
 	    cvg_swap_v1( SWPHDR, G_FALSE, *el, el, &ierr );  
	} 
 	else {
	    cvg_swap_v1( SWPALL, G_FALSE, *el, el, &ierr );  
	} 
    }	

/*
 *  Write to file & close.
 */    
    cfl_writ( fp, numbytes, (void *)el, &ier );  	
    if ( ier != 0 )  *iret = -17;

    cfl_clos( fp, &ier );
    if ( ier != 0 )  *iret = -2;
}

/*=====================================================================*/

void cvg_writv2_elm ( v2_VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writv2_elm							*
 *									*
 * This function writes an version 2 element to a vector graphics file.	*
 *									*
 * cvg_writv2_elm ( el, start, numbytes, fname, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		v2_VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-3  = seek to a bad location	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 **									*
 * Log:									*
 * J. Wu/GSC		01/01	Copied from cvgwritelm.c 	        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	outfile[FILE_FULLSZ], newfil[133];
    FILE	*fp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Find the proper file to write.
 */ 
   if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }

/*
 *  Check the proper color settings.
 */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 
    
/*
 *  Inquire if the file exists. If not, create it. 
 */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
    
/*
 *  Open file for update. 
 */
    fp = cfl_uopn( outfile, &ier ); 
    
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
         *iret = -1;
         return;
    }

/* 
 *  Seek to the specified location if open successfully and 
 *  record the starting location for the new element.
 */
    if ( start == -1 ) {
         cfl_seek( fp, 0, SEEK_END, &ier ); 
         *location = (int) fsize;
    }
    else {
         cfl_seek( fp, (long)start, SEEK_SET, &ier );
         *location = start;
    }
    
    if ( ier != 0 )  {
        *iret = -3;
 	cfl_clos( fp, &ier );
        if ( ier != 0 ) *iret = -2;
	return; 
    }

/*
 *  Swap bit order of VG elements if necessary.
 */
   if ( MTMACH == MTULTX ||
        MTMACH == MTALPH ||
        MTMACH == MTLNUX ) {
	if ( numbytes == (int)sizeof( VG_HdrStruct ) ) {
 	    cvg_swap_v2( SWPHDR, G_FALSE, *el, el, &ierr );  
	} 
 	else {
	    cvg_swap_v2( SWPALL, G_FALSE, *el, el, &ierr );  
	} 
    }	

/*
 *  Write to file & close.
 */    
    cfl_writ( fp, numbytes, (void *)el, &ier );  	
    if ( ier != 0 )  *iret = -17;

    cfl_clos( fp, &ier );
    if ( ier != 0 )  *iret = -2;
}

/*=====================================================================*/

void cvg_writv3_elm ( v3_VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writv3_elm							*
 *									*
 * This function writes an version 3 element to a vector graphics file.	*
 *									*
 * cvg_writv3_elm ( el, start, numbytes, fname, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		v3_VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-3  = seek to a bad location	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/02	Copied from cvg_writv3_elm 	        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	outfile[FILE_FULLSZ], newfil[133];
    FILE	*fp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Find the proper file to write.
 */ 
   if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }

/*
 *  Check the proper color settings.
 */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 
    
/*
 *  Inquire if the file exists. If not, create it. 
 */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
    
/*
 *  Open file for update. 
 */
    fp = cfl_uopn( outfile, &ier ); 
    
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
         *iret = -1;
         return;
    }

/* 
 *  Seek to the specified location if open successfully and 
 *  record the starting location for the new element.
 */
    if ( start == -1 ) {
         cfl_seek( fp, 0, SEEK_END, &ier ); 
         *location = (int) fsize;
    }
    else {
         cfl_seek( fp, (long)start, SEEK_SET, &ier );
         *location = start;
    }
    
    if ( ier != 0 )  {
        *iret = -3;
 	cfl_clos( fp, &ier );
        if ( ier != 0 ) *iret = -2;
	return; 
    }

/*
 *  Swap bit order of VG elements if necessary.
 */
   if ( MTMACH == MTULTX ||
        MTMACH == MTALPH ||
        MTMACH == MTLNUX ) {
	if ( numbytes == (int)sizeof( VG_HdrStruct ) ) {
 	    cvg_swap_v3( SWPHDR, G_FALSE, *el, el, &ierr );  
	} 
 	else {
	    cvg_swap_v3( SWPALL, G_FALSE, *el, el, &ierr );  
	} 
    }	

/*
 *  Write to file & close.
 */    
    cfl_writ( fp, numbytes, (void *)el, &ier );  	
    if ( ier != 0 )  *iret = -17;

    cfl_clos( fp, &ier );
    if ( ier != 0 )  *iret = -2;
}

/*=====================================================================*/

void cvg_writv4_elm ( v4_VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writv4_elm							*
 *									*
 * This function writes an version 4 element to a vector graphics file.	*
 *									*
 * cvg_writv4_elm ( el, start, numbytes, fname, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		v4_VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-3  = seek to a bad location	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/02	Copied from cvg_writv3_elm 	        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	outfile[FILE_FULLSZ], newfil[133];
    FILE	*fp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Find the proper file to write.
 */ 
   if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }

/*
 *  Check the proper color settings.
 */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 
    
/*
 *  Inquire if the file exists. If not, create it. 
 */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
    
/*
 *  Open file for update. 
 */
    fp = cfl_uopn( outfile, &ier ); 
    
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
         *iret = -1;
         return;
    }

/* 
 *  Seek to the specified location if open successfully and 
 *  record the starting location for the new element.
 */
    if ( start == -1 ) {
         cfl_seek( fp, 0, SEEK_END, &ier ); 
         *location = (int) fsize;
    }
    else {
         cfl_seek( fp, (long)start, SEEK_SET, &ier );
         *location = start;
    }
    
    if ( ier != 0 )  {
        *iret = -3;
 	cfl_clos( fp, &ier );
        if ( ier != 0 ) *iret = -2;
	return; 
    }

/*
 *  Swap bit order of VG elements if necessary.
 */
   if ( MTMACH == MTULTX ||
        MTMACH == MTALPH ||
        MTMACH == MTLNUX ) {
	if ( numbytes == (int)sizeof( VG_HdrStruct ) ) {
 	    cvg_swap_v4( SWPHDR, G_FALSE, *el, el, &ierr );  
	} 
 	else {
	    cvg_swap_v4( SWPALL, G_FALSE, *el, el, &ierr );  
	} 
    }	

/*
 *  Write to file & close.
 */    
    cfl_writ( fp, numbytes, (void *)el, &ier );  	
    if ( ier != 0 )  *iret = -17;

    cfl_clos( fp, &ier );
    if ( ier != 0 )  *iret = -2;
}

/*=====================================================================*/

void cvg_writv5_elm ( v5_VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writv5_elm							*
 *									*
 * This function writes an version 5 element to a vector graphics file.	*
 *									*
 * cvg_writv5_elm ( el, start, numbytes, fname, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		v5_VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-3  = seek to a bad location	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 **									*
 * Log:									*
 * H. Zeng/XTRIA        01/03   initial coding                          *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	outfile[FILE_FULLSZ], newfil[133];
    FILE	*fp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Find the proper file to write.
 */ 
   if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }

/*
 *  Check the proper color settings.
 */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 
    
/*
 *  Inquire if the file exists. If not, create it. 
 */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
    
/*
 *  Open file for update. 
 */
    fp = cfl_uopn( outfile, &ier ); 
    
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
         *iret = -1;
         return;
    }

/* 
 *  Seek to the specified location if open successfully and 
 *  record the starting location for the new element.
 */
    if ( start == -1 ) {
         cfl_seek( fp, 0, SEEK_END, &ier ); 
         *location = (int) fsize;
    }
    else {
         cfl_seek( fp, (long)start, SEEK_SET, &ier );
         *location = start;
    }
    
    if ( ier != 0 )  {
        *iret = -3;
 	cfl_clos( fp, &ier );
        if ( ier != 0 ) *iret = -2;
	return; 
    }

/*
 *  Swap bit order of VG elements if necessary.
 */
   if ( MTMACH == MTULTX || 
	MTMACH == MTALPH || 
	MTMACH == MTLNUX ) {
	if ( numbytes == (int)sizeof( VG_HdrStruct ) ) {
 	    cvg_swap_v5( SWPHDR, G_FALSE, *el, el, &ierr );  
	} 
 	else {
	    cvg_swap_v5( SWPALL, G_FALSE, *el, el, &ierr );  
	} 
    }	

/*
 *  Write to file & close.
 */    
    cfl_writ( fp, numbytes, (void *)el, &ier );  	
    if ( ier != 0 )  *iret = -17;

    cfl_clos( fp, &ier );
    if ( ier != 0 )  *iret = -2;
}

/*=====================================================================*/

void cvg_crtlst ( char *fname, int *iret )
/************************************************************************
 * cvg_crtlst								*
 *									*
 * This function creates a LIST element & writes it to the given file  *
 *									*
 * cvg_crtlst ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of items	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		09/02	Created					*
 * J. Wu/SAIC		11/02	revise the LIST structure	        *
 * A. Hardy/NCEP	 3/04	Added WBCMZ LIST type			*
 * S. Danz/AWC	        07/06	Switch to new cvg_writefD() function    *
 ***********************************************************************/
{
    int		nitems, ii, start, loc, ier, type;
    char	clotyp[32], locnam[32];
    char	loctag[32], locitem[32];
    float	plat, plon;
    VG_DBStruct	el;
    Boolean	cont = TRUE;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    
    el.hdr.vg_class = CLASS_LIST;
    el.hdr.vg_type = LIST_ELM;
    
    printf ( "Specify LIST attributes:\n" );
    
    printf ( "Enter LIST type:\n" );
    printf ( "		1 -  COUNTY\n" );
    printf ( "		2 -  ZONE\n" );
    printf ( "		3 -  WFO\n" );
    printf ( "		4 -  STATE\n" );     
    printf ( "		5 -  WBCMZ\n" );
    
    while ( cont ) {
        scanf ( " %i", &type );
	
	if ( type >= LISTYP_COUNTY && type <= LISTYP_STATE ) {
	    el.elem.lst.info.subtyp = type;
	    cont = FALSE;
	}
	else {
            printf ( "Invalid choice entered!\n" );
        }
    }
            
    printf ( "Enter the marker type:\n" );
    scanf ( " %i", &el.elem.lst.info.mrktyp );
    printf ( "Enter the marker size:\n" );
    scanf ( " %f", &el.elem.lst.info.mrksiz );
    printf ( "Enter the marker width:\n" );
    scanf ( " %i", &el.elem.lst.info.mrkwid );

    printf ("Enter the # of item in this list (0-1600):\n" );
    scanf ( " %i", &nitems );

    if ( nitems <= 0 || nitems > MAXLISTITEMS ) {
        *iret = -28;
	return;
    }
    
    el.elem.lst.data.nitems = nitems;
    
    /*
     *  An item is the code listed after the list descripter in the
     *  table file. For example, in cntybnds.tbl.info, you could find
     *  some lines as
     *      "<FIPS>1103<STATE>AL<TIME_ZONE>C <FE_AREA>nc"
     *  here, "1103" is the code you could use as an item input.
     */    
    printf ( "Enter %i items for the list:\n", nitems );
    printf ( "	( COUNTY/ZONE/STATE/WBCMZ - use FIPS code\n");
    printf ( "	  WFO  - use WFO code )\n");
        
    strcpy ( locnam, "<");
    if ( el.elem.lst.info.subtyp == LISTYP_WFO )  {
        strcat ( locnam, "WFO" );
        strcpy ( clotyp, "CWA_BNDS" );
    }
    else {
        strcat ( locnam, "FIPS" );
        
	if ( el.elem.lst.info.subtyp == LISTYP_COUNTY )
            strcpy ( clotyp, "CNTY_BNDS" );
	else if ( el.elem.lst.info.subtyp == LISTYP_STATE )
            strcpy ( clotyp, "STATE_BNDS" );	
	else if ( el.elem.lst.info.subtyp == LISTYP_MZCNTY )
            strcpy ( clotyp, "WBCMZ_BNDS" );
	else
            strcpy ( clotyp, "PFZ" );	
    }    
    strcat ( locnam, ">" );
    
    for ( ii = 0; ii < nitems; ii++ ) {
        scanf ( " %s", locitem ); 
	cst_lcuc ( locitem, locitem, &ier );
	strcpy ( el.elem.lst.data.item[ii], locitem );	        	
	    
	strcpy ( loctag, locnam );
	strcat ( loctag, locitem );    
	    
	clo_init( &ier );
        clo_bstype ( clotyp, &ier );		/* set bounds type */
	clo_bstag ( loctag, &ier );		/* set bounds tag */	
        clo_bgcent ( &plat, &plon, &ier );	/* get bounds centriod */	
	    
	el.elem.lst.data.lat[ii] = plat;	        	
	el.elem.lst.data.lon[ii] = plon;	        	               
    }
       
    el.hdr.recsz = ( sizeof(VG_HdrStruct) + 
                     sizeof(ListInfo) + sizeof(ListData) );

    cvg_crthdr( &el, nitems, &el.elem.lst.data.lat[0], 
                             &el.elem.lst.data.lon[0], &ier);
         	   
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;

}

/*=====================================================================*/

void cvg_crtmet ( char *fname, int *iret )
/************************************************************************
 * cvg_crtmet								*
 *									*
 * This function creates MET (JET_ELM/GFA_ELM) elements & writes to file*
 *									*
 * cvg_crtmet ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	09/03	Created					        *
 * J. Wu/SAIC	01/04	add GFA element				        *
 * B. Yin/SAIC	02/04	added TCA element			        *
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 * m.gamazaychikov/SAIC	04/07	added TCE element			*
 * m.gamazaychikov/SAIC	04/07	added TCT element			*
 * m.gamazaychikov/SAIC	05/07	added TCB element			*
 * L. Hinson/AWC        10/11   add SGWX_ELM
 ***********************************************************************/
{   
    int		np, ii, start, loc, ier, type, clos, filled, smth;
    int		width, wndtyp, color, sptxtyp, turbsym;
    int		itxfn, ithw, iwidth, ialign, ixoff, iyoff;    
    float	lat[MAX_JETPTS], lon[MAX_JETPTS], size, rotn, sztext;
    char	text[MAX_TEXT], sel[LLSCRN];
    Boolean	typflg;
    VG_DBStruct	el;    

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    typflg = G_FALSE;
    
    el.hdr.vg_class = CLASS_MET;

    printf("Enter the type of MET element:\n");
    printf ( "\t  JET\t %2d\n\t  SGWX\t %2d\n\tGFA\t %2d\n\t  TCA\t %2d\n\t  TCE\t %2d\n\t  TCT\t %2d\n\t  TCB\t %2d\n", 
	     JET_ELM, SGWX_ELM, GFA_ELM, TCA_ELM, TCERR_ELM, TCTRK_ELM, TCBKL_ELM);

    while ( !typflg ) {
        scanf ( " %s", sel );
 	type = atoi ( sel );       
        if ( type == JET_ELM )
            typflg = G_TRUE;
        else if ( type == GFA_ELM )
            typflg = G_TRUE;
        else if ( type == SGWX_ELM )
            typflg = G_TRUE;
        else if ( type == TCA_ELM )
            typflg = G_TRUE;
        else if ( type == TCERR_ELM )
            typflg = G_TRUE;
        else if ( type == TCTRK_ELM )
            typflg = G_TRUE;
        else if ( type == TCBKL_ELM )
            typflg = G_TRUE;
        else
            printf ( "Invalid MET type entered!\n" );
    }
    el.hdr.vg_type = (char)type;
    
    
    /*
     *  Create GFA element.
     */
    if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_crtgfa ( fname, &ier );
        return;
    }
    if (el.hdr.vg_type == SGWX_ELM ) {
        cvg_crtsgwx ( fname, &ier );
        return;
    } else if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_crttca ( fname, &ier );
        return;
    }
    else if ( el.hdr.vg_type == TCERR_ELM ) {
        cvg_crttce ( fname, &ier );
        return;
    }
    else if ( el.hdr.vg_type == TCTRK_ELM ) {
        cvg_crttct ( fname, &ier );
        return;
    }
    else if ( el.hdr.vg_type == TCBKL_ELM ) {
        cvg_crttcb ( fname, &ier );
        return;
    }

    /*
     *  Create jet line
     */
    printf ( "\nJet line attributes:\n\n" );
    printf ( "Enter the jet line color:\n" );
    scanf ( " %i", &el.elem.jet.line.splcol );
    printf ( "Enter the jet line type:\n" );
    scanf ( " %i", &el.elem.jet.line.spl.info.spltyp );
    printf ( "Enter the jet line stroke multiplier:\n");
    scanf ( " %i", &el.elem.jet.line.spl.info.splstr );
    printf("Enter the jet line direction indicator:\n");
    scanf ( " %i", &el.elem.jet.line.spl.info.spldir );
    printf ( "Enter the jet line size:\n" );
    scanf ( " %f", &el.elem.jet.line.spl.info.splsiz );
    printf ( "Enter the jet line width:\n" );
    scanf ( " %i", &el.elem.jet.line.spl.info.splwid );

    printf ("Enter the # of points along the jet line (0-50):\n" );
    scanf ( " %i", &np );
        
    if ( np <= 0 || np > MAX_JETPTS ) {
        *iret = -28;
	return;
    }

    printf ( "Enter %i coordinate pairs in MAP coordinate:\n", np );
    for ( ii = 0; ii < np; ii++ )
       scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );
    printf("Closed? 0 = No 1 = Yes \n");
    scanf( " %i", &clos);
    el.hdr.closed = (char)clos;
    printf("Filled? 0 = No 1 = Yes \n");
    scanf( " %i", &filled);
    el.hdr.filled = (char)filled;
    printf ("Smoothed? 0 = No 1 = Light 2 = Heavy \n");
    scanf (" %i", &smth);
    el.hdr.smooth = (char)smth;
    
    if ( type == JET_ELM ) {
	el.elem.jet.line.spl.info.numpts = np;
	for ( ii = 0; ii < np; ii++ ) {
	    el.elem.jet.line.spl.latlon[ii]    = lat[ii];
	    el.elem.jet.line.spl.latlon[ii+np] = lon[ii];
	}
    }
    
    
    /*
     *  Create wind barbs
     */
    printf ( "\nJet barb attributes:\n\n" );    
    printf ("Enter the # of barbs along the jet line:\n" );
    scanf ( " %i", &np );

    if ( np < 0 )  np = 0;    
    if ( np > el.elem.jet.line.spl.info.numpts ) {
        np = el.elem.jet.line.spl.info.numpts;
    }
    
    el.elem.jet.nbarb = np;
    
    printf ( "Enter the barb color:\n" );
    scanf ( " %i", &color );
    printf ( "Enter the barb width:\n" );
    scanf ( " %i", &width );
    printf ( "Enter the barb size:\n" );
    scanf ( " %f", &size );
    printf ( "Enter the barb attribute type:\n" );
    scanf ( " %i", &wndtyp );
    	
    if ( np > 0 ) {        
        printf ( "Enter %i lat/lon points, speeds, and directions:\n", np);

        for ( ii = 0; ii < np; ii++ ) {
	    el.elem.jet.barb[ii].wndcol = color;
	    el.elem.jet.barb[ii].wnd.info.numwnd = 1;
            el.elem.jet.barb[ii].wnd.info.hdsiz = 0.0F;
            el.elem.jet.barb[ii].wnd.info.width = width;
	    el.elem.jet.barb[ii].wnd.info.size = size;
	    el.elem.jet.barb[ii].wnd.info.wndtyp = wndtyp;
	    
            scanf ( " %f %f %f %f",
	            &(el.elem.jet.barb[ii].wnd.data.latlon[0]),
	            &(el.elem.jet.barb[ii].wnd.data.latlon[1]),
                    &(el.elem.jet.barb[ii].wnd.data.spddir[0]),
		    &(el.elem.jet.barb[ii].wnd.data.spddir[1]) );
        }        
    }

    
    /*
     *  Create texts 
     */     
    printf ( "\nJet text attributes:\n\n" );    
    printf ( "Enter the text color:\n" );
    scanf ( " %i", &color );
    printf ( "Enter the text font(itxfn):\n" );
    scanf ( " %i", &itxfn );
    printf ( "Enter the text HW flag:\n" );
    scanf ( " %i", &ithw );
    printf ( "Enter the text width multiplier:\n" );
    scanf ( " %i", &iwidth );
    printf ( "Enter the text size:\n" );
    scanf ( " %f", &sztext );
    printf ( "Enter the rotation:\n" );
    scanf ( " %f", &rotn );
    printf ( "Enter the x offset:\n" );
    scanf ( " %i", &ixoff );
    printf ( "Enter the y offset:\n" );
    scanf ( " %i", &iyoff );    
    printf ( "Enter the alignment value (-1, 0, 1)\n" );
    scanf ( " %i", &ialign );
    printf ( "Enter the Special Text Type (1-14):\n" );
    scanf ( " %i", &sptxtyp );
    printf ( "Enter the Turbulence/Icing Symbol:\n" );
    scanf ( " %i", &turbsym );    

    if ( np > 0 ) {        
        printf ( "Enter %i pairs of lat/lon points and texts:\n", np);

        for ( ii = 0; ii < np; ii++ ) {
	    el.elem.jet.barb[ii].sptcol = color;
	    
	    el.elem.jet.barb[ii].spt.info.lat = lat[0];
	    el.elem.jet.barb[ii].spt.info.lon = lon[0];
        
	    el.elem.jet.barb[ii].spt.info.offset_x = ixoff;
            el.elem.jet.barb[ii].spt.info.offset_y = iyoff;
            el.elem.jet.barb[ii].spt.info.ialign  = ialign;	       
	    el.elem.jet.barb[ii].spt.info.rotn    = rotn;
            el.elem.jet.barb[ii].spt.info.sztext  = sztext;
            el.elem.jet.barb[ii].spt.info.itxfn   = itxfn;
            el.elem.jet.barb[ii].spt.info.ithw    = ithw;
            el.elem.jet.barb[ii].spt.info.iwidth  = iwidth;
        
            el.elem.jet.barb[ii].spt.info.txtcol  = color;
            el.elem.jet.barb[ii].spt.info.lincol  = el.hdr.min_col;
            el.elem.jet.barb[ii].spt.info.filcol  = el.hdr.min_col;
            el.elem.jet.barb[ii].spt.info.sptxtyp = sptxtyp;
            el.elem.jet.barb[ii].spt.info.turbsym = turbsym;
            strcpy ( el.elem.jet.barb[ii].spt.text, "" );    
	    
	    scanf ( " %f %f %s",
	            &(el.elem.jet.barb[ii].spt.info.lat),
	            &(el.elem.jet.barb[ii].spt.info.lon),
		    text );
            strcpy ( el.elem.jet.barb[ii].spt.text, text );    
        }        
    }


    /*
     *  Create wind hashs
     */
    printf ("\nJet hash attributes:\n\n" );    
    printf ("Enter the # of hashs along the jet line:\n" );
    scanf ( " %i", &np );

    if ( np < 0 )  np = 0;    
    if ( np > el.elem.jet.line.spl.info.numpts ) {
        np = el.elem.jet.line.spl.info.numpts;
    }
    
    el.elem.jet.nhash = np;
    
    printf ( "Enter the hash color:\n" );
    scanf ( " %i", &color );
    printf ( "Enter the hash width:\n" );
    scanf ( " %i", &width );
    printf ( "Enter the hash size:\n" );
    scanf ( " %f", &size );
    printf ( "Enter the hash attribute type:\n" );
    scanf ( " %i", &wndtyp );
    	
    if ( np > 0 ) {        
        printf ( "Enter %i lat/lon points, speeds, and directions:\n", np);

        for ( ii = 0; ii < np; ii++ ) {
	    el.elem.jet.hash[ii].wndcol = color;
	    el.elem.jet.hash[ii].wnd.info.numwnd = 1;
            el.elem.jet.hash[ii].wnd.info.hdsiz = 0.0F;
            el.elem.jet.hash[ii].wnd.info.width = width;
	    el.elem.jet.hash[ii].wnd.info.size = size;
	    el.elem.jet.hash[ii].wnd.info.wndtyp = wndtyp;
	    
            scanf ( " %f %f %f %f",
	            &(el.elem.jet.hash[ii].wnd.data.latlon[0]),
	            &(el.elem.jet.hash[ii].wnd.data.latlon[1]),
                    &(el.elem.jet.hash[ii].wnd.data.spddir[0]),
		    &(el.elem.jet.hash[ii].wnd.data.spddir[1]) );
        }    
    
    }
    
    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof(JetType) );

    /*
     *  Create jet header
     */     
    np = el.elem.jet.line.spl.info.numpts;
    cvg_crthdr( &el, np, lat, lon, &ier);
        
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crtgfa ( char *fname, int *iret )
/************************************************************************
 * cvg_crtgfa								*
 *									*
 * This function creates a GFA element & writes to file			*
 *									*
 * cvg_crtgfa ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		01/04	create					*
 * J. Wu/SAIC		02/04	add lat/lon text position		*
 * J. Wu/SAIC		06/04	add GFA_GFA subtype			*
 * E. Safford/SAIC	09/04	update for top/bottom as char arrays	*
 * J. Wu/SAIC		09/04	remove airmet & non-convetive airmet	*
 * J. Wu/SAIC		10/04	use cvg_getFld to access GFA attributes	*
 * B. Yin/SAIC		11/04	remove unnecessary gfa tags		*
 * E. Safford/SAIC	07/05	remove sequence#, add gfa_category 	*
 * B. Yin/SAIC          11/05   change gfa_category to gfa_subtype      *
 * S. Danz/AWC	        07/06	Switch to new cvg_writefD() function    *
 * L. Hinson/AWC        06/08   Add prompt for cycle time to set tag    *
 *                              TAG_GFA_CYCLE                           *
 ***********************************************************************/
{   
    int		np, ii, start, loc, smth, filled, ier, areatype;
    float	lat[MAXPTS], lon[MAXPTS];
    char	value[STD_STRLEN];
    VG_DBStruct	el;    

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */
    
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type = (char) GFA_ELM;
                
    /*
     *  Initialize number of blocks to 0.
     */
    el.elem.gfa.info.nblocks = 0;
    

    /*
     *  Create GfaInfo
     */

    sprintf(value, " %s", "2" );
    cvg_setFld ( &el, TAG_GFA_SUBTYPE, value, &ier );
        
    printf ( "\nGFA attributes:\n\n" );

    printf ( "Enter the weather type:\n");
    printf ( "\t  0 - SIGWX\t\n\t  1 - SIG CLD\t\n");
    printf ( "\t  2 - TCU CLD (TCU)\t\n\t  3 - ICE\t\n");
    printf ( "\t  4 - TURB\t\n\t  5 - MTW\t\n");
    printf ( "\t  6 - CLD\t\n\t  7 - MT_OBSC\t\n");
    printf ( "\t  8 - IFR\t\n\t  9 - FZLVL\t\n");
    printf ( "\t 10 - M_FZLVL\t\n\t 11 - SFC_WND\t\n");
    printf ( "\t 12 - LLWS\t\n"); 
    
    scanf ( " %s", value );
    areatype = G_MAX ( atoi(value), 0 );    
    
    switch ( areatype ) {
	case 0:            
	    strcpy ( value, "SIGWX" );
	  break;
	    
	case 1:
            strcpy ( value, "SIG_CLD_(CIG)" );
	  break;

	case 2:
            strcpy ( value, "SIG_CLD_(TCU)" );
	  break;

	case 3:
            strcpy ( value, "ICE" );
	  break;
 	    
	case 4:
            strcpy ( value, "TURB" );
	  break;
 	    
	case 5:
            strcpy ( value, "MTW" );
	  break;

 	case 6:
            strcpy ( value, "CLD" );
	  break;

	case 7:
            strcpy ( value, "MT_OBSC" );
	  break;
	   
	case 8:
            strcpy ( value, "IFR" );
	  break;
	    
	case 9:
            strcpy ( value, "FZLVL" );
	  break;

	case 10:
            strcpy ( value, "M_FZLVL" );
	  break;

	case 11:
            strcpy ( value, "SFC_WND" );
	  break;

	case 12:
            strcpy ( value, "LLWS" );
	  break;

	default:
            strcpy ( value, "SIGWX" );
	  break;	    
    }	
        
    cvg_setFld ( &el, TAG_GFA_AREATYPE, value, &ier );

    printf ( "Enter the tag: (1W...9W)\n");
    scanf ( " %s", value );		
    cvg_setFld ( &el, TAG_GFA_TAG, value, &ier );

    printf ( "Enter the status: (NRML/NEW/COR/AMD/CAN)\n");
    scanf ( " %s", value );		
    cvg_setFld ( &el, TAG_GFA_STATUS, value, &ier );
    
    printf ( "Enter the cycle hour:\n");
    printf ( "\t 3, 9, 15, 21\n");
    scanf ( " %s", value );
    cvg_setFld ( &el, TAG_GFA_CYCLE, value, &ier );

    printf ( "Enter the forecast hour:\n");
    printf ( "\t  0, 3, 6, 9, 12, 0-0, 3-3, 6-6, 0-3, 0-6, 3-6, 6-12, 9-12\n");
    scanf ( " %s", value );		
    cvg_setFld ( &el, TAG_GFA_FCSTHR, value, &ier );

    if( areatype == 3 || areatype == 4 ) { 
        printf ( "Enter the top level of area:\n");
        scanf ( " %s", value );		
        cvg_setFld ( &el, TAG_GFA_TOP, value, &ier );
    
        printf ( "Enter the bottom level of area:\n");
        scanf ( " %s", value );		
        cvg_setFld ( &el, TAG_GFA_BOTTOM, value, &ier );
    }

    printf ( "Enter the GFA subtype:\n");
    printf ( "\t 0 = Snapshot\n");
    printf ( "\t 1 = User drawn smear\n");
    printf ( "\t 2 = System generated smear\n");
    printf ( "\t 3 = User drawn outlook\n" );
    printf ( "\t 4 = System generated outlook\n" );
    scanf ( " %s", value );
    cvg_setFld ( &el, TAG_GFA_SUBTYPE, value, &ier );

    printf ( "Enter the line width:\n");
    scanf ( " %s", value );		
    cvg_setFld ( &el, TAG_GFA_LINEWIDTH, value, &ier );
    
    printf ( "Enter the longitude of the text location:\n" );    
    scanf ( " %s", value );		
    cvg_setFld ( &el, TAG_GFA_LON, value, &ier );
    
    printf ( "Enter the latitude of the text location:\n" );
    scanf ( " %s", value );		
    cvg_setFld ( &el, TAG_GFA_LAT, value, &ier );


    printf ("Enter the # of points for the gfa line (>=3):\n" );
    scanf ( " %i", &np );		
        
    if ( np < 2 || np > MAXPTS ) {
        *iret = -28;
	return;
    }

    printf ( "Enter %i coordinate pairs (lat lon) in MAP coordinate:\n", np );
    for ( ii = 0; ii < np; ii++ )
       scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );

    printf("Filled? 0 = No 1 = Yes \n");
    scanf( " %i", &filled);
    el.hdr.filled = (char)filled;
    printf ("Smoothed? 0 = No 1 = Light 2 = Heavy \n");
    scanf (" %i", &smth);
    el.hdr.smooth = (char)smth;
    
    for ( ii = 0; ii < np; ii++ ) {
	el.elem.gfa.latlon[ii] = lat[ii];
	el.elem.gfa.latlon[ii + np] = lon[ii];
    }
           

    /*
     *  Create GFA header
     */     
    cvg_crthdr( &el, np, lat, lon, &ier);
    
    el.elem.gfa.info.npts = np;    
    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof(int) * 2 +
		sizeof(char) * STD_STRLEN * el.elem.gfa.info.nblocks ) +
		sizeof(float) * np * 2;
    
    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = -19;
    

    /*
     *  Free the block pointer.
     */     
    cvg_freeElPtr ( &el );

}

/*=====================================================================*/

void cvg_crtsgwx (char *fname, int *iret )
/************************************************************************
 * cvg_crtsgwx								*
 *									*
 * This function creates a SGWX element & writes to file		*
 *									*
 * cvg_crtsgwx ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 - error saving record	*
 *					-28 = invalid number of points	*
 *									*
 **									*
 * Log::                                                                *
 * L. Hinson/AWC        01/12           Created                         *
 ************************************************************************/
{
  int           np, ii, ier, start, loc;
  float         lat[MAXPTS], lon[MAXPTS];
  VG_DBStruct	el;
  
  *iret = G_NORMAL;
  start = -1;
  
  el.hdr.vg_class = (char) CLASS_MET;
  el.hdr.vg_type =  (char) SGWX_ELM;
  el.elem.sgwx.info.npts = 0;
  
  printf ( "\nSGWX line attributes:\n\n" );
  printf ("Enter the SGWX subtype (0 = TURB, 1 = CONV, 2=MIDLVL, 3=SYMBOL \n" );
  scanf (" %i ", &el.elem.sgwx.info.subtype );
  printf ("Enter the # of points for the SGWX area (0-128):\n" );
  scanf ( " %i", &np );
  if ( np <= 0 || np > 128 ) {
    *iret = -28;
    return;
  }
  printf ( "Enter %i coordinate pairs in MAP coordinates:\n", np );
  for ( ii = 0; ii < np; ii++ )
    scanf ( " %f %f", &(lat[ii]), &(lon[ii]) );
  for (ii = 0; ii < np; ii++ ) {
    el.elem.sgwx.latlon[ii] = lat[ii];
    el.elem.sgwx.latlon[ii+np] = lon[ii];
  }
  el.hdr.filled = 0;
  el.hdr.closed = 1;
  el.hdr.smooth = 1;
  el.elem.gfa.info.npts = np;
  el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + + sizeof (SGWXType) );
  cvg_crthdr ( &el, np, lat, lon, &ier); /* Set header */
  cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier );
  if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crttca ( char *fname, int *iret )
/************************************************************************
 * cvg_crttca								*
 *									*
 * This function creates a TCA element & writes to file			*
 *									*
 * cvg_crttca ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 = error saving record	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC	02/04	Created					        *
 * B. Yin/SAIC  03/04   Added storm type in tca structure 		*
 * B. Yin/SAIC  05/04   Modified code to get tca string size dynamically*
 * B. Yin/SAIC  07/04   Modified code to get more than 2 bkpts		*
 * B. Yin/SAIC	12/04	Added time zone					*
 * B. Yin/SAIC	04/05	Added issue status, removed year		*
 * S. Gilbert/NCEP	11/05	Added text_lat, text_lon, text_font,    *
 *                              text_size, and text_width               *
 * S. Gilbert/NCEP	01/06	Change format specifier for advisoryNum*
 * S. Danz/AWC	07/06	Switch to new cvg_writefD() function     	*
 ***********************************************************************/
{   
    int 	start, counter, ii, dummy_loc, n_pts, ier;
    char	*ptca;
    float	lat[ MAXPTS ], lon[ MAXPTS ];
    VG_DBStruct	el;    

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      		/* Indicating write to the end of file */
    
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type = (char) TCA_ELM;
                
    /*
     *  Create TcaInfo
     */
    printf ( "\nTCA attributes:\n\n" );
    printf ( "Enter the issue status: O/E/T/X \n");
    scanf ( " %c", &el.elem.tca.info.issueStatus );
    printf ( "Enter the storm number:\n" );
    scanf ( " %i", &el.elem.tca.info.stormNum );
    printf ( "Enter the basin: 0 = Atlantic 1 = E_Pacific 2 = C_Pacific 3 = W_Pacific\n");
    scanf ( " %i", (int*)&el.elem.tca.info.basin );
    printf ( "Enter the advisory number:\n");
    scanf ( " %s", el.elem.tca.info.advisoryNum );
    printf ( "Enter the storm name:\n");
    scanf ( " %s", el.elem.tca.info.stormName );
    printf ( "Enter the storm type:\n");
    printf ( "0 = Hurricane 1 = Tropical Storm 2 = Tropical Depression " );
    printf ( "3 = Subtropical Storm 4 = Subtropical Depression\n");
    scanf ( " %i", (int*)&el.elem.tca.info.stormType );
    printf ( "Enter the valid time in GEMPAK format:\n");
    scanf ( " %s", el.elem.tca.info.validTime );
    printf ( "Enter the time zone:\n");
    scanf ( " %s", el.elem.tca.info.timezone );
    printf ( "Enter the number of watches/warnings:\n");
    scanf ( " %i", &el.elem.tca.info.wwNum );

    if ( el.elem.tca.info.wwNum == 0 ) {
        printf ( "Enter the latitude of text box message:\n");
        scanf ( " %f", &el.elem.tca.info.text_lat );
        lat[0] = el.elem.tca.info.text_lat;
        printf ( "Enter the longitude of text box message:\n");
        scanf ( " %f", &el.elem.tca.info.text_lon );
        lon[0] = el.elem.tca.info.text_lon;
        n_pts = 1;
        printf ( "Enter the font number for text box message:\n");
        scanf ( " %d", &el.elem.tca.info.text_font );
        printf ( "Enter the text size for text box message:\n");
        scanf ( " %f", &el.elem.tca.info.text_size );
        printf ( "Enter the text width for text box message:\n");
        scanf ( " %d", &el.elem.tca.info.text_width );
    }
    else {
        el.elem.tca.info.text_lat = 25.8;
        el.elem.tca.info.text_lon = -80.4;
        el.elem.tca.info.text_font = 1;
        el.elem.tca.info.text_size = 1.;
        el.elem.tca.info.text_width = 3;
        n_pts = 0;
        for ( counter = 0; counter < el.elem.tca.info.wwNum; counter++ ) {
            printf ( "\nAttributes of watches/warnings number %d:\n", counter + 1 );
            printf ( "Enter the storm severity: 0 = Tropical Storm 1 = Hurricane\n");
            scanf ( " %i", (int*)&el.elem.tca.info.tcaww[ counter ].severity );
            printf ( "Enter the advisory type: 0 = Watch 1 = Warning\n");
            scanf ( " %i", (int*)&el.elem.tca.info.tcaww[ counter ].advisoryType );
            printf ( "Enter the special geography type:" );
	    printf ( " 0 = None 1 = Islands 2 = Water\n");
            scanf ( " %i", (int*)&el.elem.tca.info.tcaww[ counter ].specialGeog );
 
            if ( el.elem.tca.info.tcaww[ counter ].specialGeog == NO_TYPE ) {
           el.elem.tca.info.tcaww[ counter ].numBreakPts = 2;
            }
  	    else {
	       printf( "Enter the number of break points:\n" );
               scanf( " %i", (int*)&el.elem.tca.info.tcaww[ counter ].numBreakPts );
            } 
   
            if ( !( el.elem.tca.info.tcaww[ counter ].breakPnt = malloc ( 
 				el.elem.tca.info.tcaww[ counter ].numBreakPts *
				sizeof ( Breakpt_T ) ))) return;

	    for ( ii = 0; ii < el.elem.tca.info.tcaww[ counter ].numBreakPts; ii++ ) {	 
      
                printf ( "Enter the name of the break point %d:\n", ii + 1 );
                scanf ( " %s", el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName );
                printf ( "Enter the latitude and the longitude of the break point %d:\n", ii + 1 );
                scanf ( " %f %f", &el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat,
                          &el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon );
                lat[ n_pts ] = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat;
                lon[ n_pts++ ] = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon;

            }
        }
    }
    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + 
			cvg_loadTcaFileInfo ( el.elem.tca.info, &ptca, &ier ) );
    free( ptca );

    /*
     *  Create tca header
     */     
    cvg_crthdr( &el, n_pts, lat, lon, &ier);
        
    cvg_writefD( &el, start, el.hdr.recsz, fname, &dummy_loc, &ier ); 	
    
    for ( counter = 0; counter < el.elem.tca.info.wwNum; counter++ ) {
        free( el.elem.tca.info.tcaww[ counter ].breakPnt );
    }

    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crttce ( char *fname, int *iret )
/************************************************************************
 * cvg_crttce								*
 *									*
 * This function creates a TCE element & writes to file			*
 *									*
 * cvg_crttce ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 = error saving record	*
 *									*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	04/07	Created				        *
 ***********************************************************************/
{   
    int 	start, ii, dummy_loc, np, n_pts, ier;
    float	lat[ MAXPTS ], lon[ MAXPTS ];
    VG_DBStruct	el;    

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    n_pts = 0;
    start = -1;      		/* Indicating write to the end of file */
    
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type  = (char) TCERR_ELM;
                
    /*
     *  Create TcInfo
     */
    printf ( "\nTC attributes:\n\n" );
    printf ( "Enter the issue status: O/E/T/X \n");
    scanf ( " %s", el.elem.tce.info.issueStatus );
    printf ( "Enter the storm number:\n" );
    scanf ( " %s", el.elem.tce.info.stormNum );
    printf ( "Enter the basin: 0 = Atlantic 1 = E_Pacific 2 = C_Pacific 3 = W_Pacific\n");
    scanf ( " %s", el.elem.tce.info.basin );
    printf ( "Enter the advisory number:\n");
    scanf ( " %s", el.elem.tce.info.advisoryNum );
    printf ( "Enter the storm name:\n");
    scanf ( " %s", el.elem.tce.info.stormName );
    printf ( "Enter the storm type:\n");
    printf ( "0 = Hurricane 1 = Tropical Storm 2 = Tropical Depression " );
    printf ( "3 = Subtropical Storm 4 = Subtropical Depression\n");
    scanf ( " %s", el.elem.tce.info.stormType );
    printf ( "Enter the valid time in GEMPAK format:\n");
    scanf ( " %s", el.elem.tce.info.validTime );
    printf ( "Enter the time zone:\n");
    scanf ( " %s", el.elem.tce.info.timezone );
    printf ( "Enter the forecast period (72 or 120):\n");
    scanf ( " %s", el.elem.tce.info.fcstpd );

    printf( "Enter the %s-hour error cone attributes\n", el.elem.tce.info.fcstpd );
    printf( "line color:\n" );
    scanf( " %i", (int*)&el.elem.tce.cone.lincol );
    printf( "line type:\n" );
    scanf( " %i", (int*)&el.elem.tce.cone.lintyp );
    printf( "fill color:\n" );
    scanf( " %i", (int*)&el.elem.tce.cone.filcol );
    printf( "fill pattern:\n" );
    scanf( " %i", (int*)&el.elem.tce.cone.filtyp );

    printf( "number of points:\n" );
    scanf( " %i", (int*)&el.elem.tce.cone.npts );
    np = el.elem.tce.cone.npts;

    printf ( "cone coordinate pairs (lat lon) in MAP coordinate:\n");
    for ( ii = 0; ii < np; ii++ ) {
        scanf ( " %f %f", &el.elem.tce.cone.latlon[ii],
                          &el.elem.tce.cone.latlon[ii+np]);
        lat [n_pts]   = el.elem.tce.cone.latlon[ii];
        lat [n_pts++] = el.elem.tce.cone.latlon[ii+np];
    }

    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof ( TcInfo) 
                        + sizeof (int)*5 + sizeof (float) * np * 2);

    /*
     *  Create tce header and write out the element
     */     
    cvg_crthdr( &el, n_pts, lat, lon, &ier);
        
    cvg_writefD( &el, start, el.hdr.recsz, fname, &dummy_loc, &ier ); 	
    
    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crttct ( char *fname, int *iret )
/************************************************************************
 * cvg_crttct								*
 *									*
 * This function creates a TCT element & writes to file			*
 *									*
 * cvg_crttct ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 = error saving record	*
 *									*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	04/07	Created				        *
 ***********************************************************************/
{   
    int 	start, ii, dummy_loc, np, n_pts, ier;
    float	lat[ MAXPTS ], lon[ MAXPTS ];
    VG_DBStruct	el;    

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    n_pts = 0;
    start = -1;      		/* Indicating write to the end of file */
    
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type  = (char) TCTRK_ELM;
                
    /*
     *  Create TcInfo
     */
    printf ( "\nTC attributes:\n\n" );
    printf ( "Enter the issue status: O/E/T/X \n");
    scanf ( " %s", el.elem.tct.info.issueStatus );
    printf ( "Enter the storm number:\n" );
    scanf ( " %s", el.elem.tct.info.stormNum );
    printf ( "Enter the basin: 0 = Atlantic 1 = E_Pacific 2 = C_Pacific 3 = W_Pacific\n");
    scanf ( " %s", el.elem.tct.info.basin );
    printf ( "Enter the advisory number:\n");
    scanf ( " %s", el.elem.tct.info.advisoryNum );
    printf ( "Enter the storm name:\n");
    scanf ( " %s", el.elem.tct.info.stormName );
    printf ( "Enter the storm type:\n");
    printf ( "0 = Hurricane 1 = Tropical Storm 2 = Tropical Depression " );
    printf ( "3 = Subtropical Storm 4 = Subtropical Depression\n");
    scanf ( " %s", el.elem.tct.info.stormType );
    printf ( "Enter the valid time in GEMPAK format:\n");
    scanf ( " %s", el.elem.tct.info.validTime );
    printf ( "Enter the time zone:\n");
    scanf ( " %s", el.elem.tct.info.timezone );
    printf ( "Enter the forecast period (72 or 120):\n");
    scanf ( " %s", el.elem.tct.info.fcstpd );

    printf( "Enter the %s-hour track attributes\n", el.elem.tct.info.fcstpd );
    printf( "line color:\n" );
    scanf( " %i", (int*)&el.elem.tct.lincol );
    printf( "line type:\n" );
    scanf( " %i", (int*)&el.elem.tct.lintyp );

    printf( "Enter the %s-hour track number of points:\n" , el.elem.tct.info.fcstpd);
    scanf( " %i", (int*)&el.elem.tct.numTrackPts );
    np = el.elem.tct.numTrackPts;

    if ( !( el.elem.tct.trackPnt = malloc ( el.elem.tct.numTrackPts 
                                 * sizeof (TcTrack) ))) return;

    for ( ii = 0; ii < el.elem.tct.numTrackPts; ii++ ) {
        printf ( "Enter the advisory date for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].advDate );
        printf ( "Enter the forecast period for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].tau );
        printf ( "Enter the MAX sustained wind for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].mxWnd );
        printf ( "Enter the wind gust for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].wGust );
        printf ( "Enter the mslp for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].mslp );
        printf ( "Enter the level of TC developement for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].tcDv );
        printf ( "Enter the label for level of TC developement for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].tcDvLbl );
        printf ( "Enter the direction of TC movement for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].tcDir );
        printf ( "Enter the speed of TC movement for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].tcSpd );
        printf ( "Enter the date label for track point%d:\n", ii + 1 );
        scanf ( " %s", el.elem.tct.trackPnt[ ii ].dtLbl );
        printf ( "Enter the latitude and the longitude of track point %d:\n", ii + 1 );
        scanf ( " %f %f", &el.elem.tct.trackPnt[ii].lat, &el.elem.tct.trackPnt[ii].lon );
        lat[ n_pts ]   = el.elem.tct.trackPnt[ii].lat;
        lon[ n_pts++ ] = el.elem.tct.trackPnt[ii].lon;
    }

    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof ( TcInfo) 
                        + sizeof (int)*3 + np*sizeof (TcTrack) );

    /*
     *  Create tct header and write out the element
     */     
    cvg_crthdr( &el, n_pts, lat, lon, &ier);
        
    cvg_writefD( &el, start, el.hdr.recsz, fname, &dummy_loc, &ier ); 	
    
    free( el.elem.tct.trackPnt );

    if ( ier < 0 ) *iret = -19;
}

/*=====================================================================*/

void cvg_crttcb ( char *fname, int *iret )
/************************************************************************
 * cvg_crttcb								*
 *									*
 * This function creates a TCB element & writes to file			*
 *									*
 * cvg_crttcb ( fname, iret )	                                        *
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	 				-19 = error saving record	*
 *									*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	05/07	Created				        *
 ***********************************************************************/
{   
    int 	start, ii, dummy_loc, np, n_pts, ier;
    float	lat[ MAXPTS ], lon[ MAXPTS ];
    VG_DBStruct	el;    

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    n_pts = 0;
    start = -1;      		/* Indicating write to the end of file */
    
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type  = (char) TCBKL_ELM;
                
    /*
     *  Create TcInfo
     */
    printf ( "\nTC attributes:\n\n" );
    printf ( "Enter the issue status: O/E/T/X \n");
    scanf ( " %s", el.elem.tcb.info.issueStatus );
    printf ( "Enter the storm number:\n" );
    scanf ( " %s", el.elem.tcb.info.stormNum );
    printf ( "Enter the basin: 0 = Atlantic 1 = E_Pacific 2 = C_Pacific 3 = W_Pacific\n");
    scanf ( " %s", el.elem.tcb.info.basin );
    printf ( "Enter the advisory number:\n");
    scanf ( " %s", el.elem.tcb.info.advisoryNum );
    printf ( "Enter the storm name:\n");
    scanf ( " %s", el.elem.tcb.info.stormName );
    printf ( "Enter the storm type:\n");
    printf ( "0 = Hurricane 1 = Tropical Storm 2 = Tropical Depression " );
    printf ( "3 = Subtropical Storm 4 = Subtropical Depression\n");
    scanf ( " %s", el.elem.tcb.info.stormType );
    printf ( "Enter the valid time in GEMPAK format:\n");
    scanf ( " %s", el.elem.tcb.info.validTime );
    printf ( "Enter the time zone:\n");
    scanf ( " %s", el.elem.tcb.info.timezone );
    printf ( "Enter the forecast period (72 or 120):\n");
    scanf ( " %s", el.elem.tcb.info.fcstpd );

    printf( "Enter the TCWW attributes\n");
    printf( "line color:\n" );
    scanf( " %i", (int*)&el.elem.tcb.lincol );
    printf( "line width:\n" );
    scanf( " %i", (int*)&el.elem.tcb.linwid );
    printf( "TC WW severity (HU WRNG =0, HU WTCH=1, TS WRNG=2, TS WTCH=3):\n" );
    scanf( " %i", (int*)&el.elem.tcb.tcww );

    printf( "Enter the TCWW number of points:\n" );
    scanf( " %i", (int*)&el.elem.tcb.numBkPts );
    np = el.elem.tcb.numBkPts;

    if ( !( el.elem.tcb.bkPntLn = malloc ( el.elem.tcb.numBkPts 
                                 * sizeof (Breakpt_T) ))) return;

    for ( ii = 0; ii < el.elem.tcb.numBkPts ; ii++ ) {
        printf ( "Enter latitude, longitude and name of the break point %d:\n", ii + 1 );
        scanf ( " %f %f %s", &el.elem.tcb.bkPntLn[ii].lat, &el.elem.tcb.bkPntLn[ii].lon,
               el.elem.tcb.bkPntLn[ii].breakPtName );
        lat[ n_pts ]   = el.elem.tcb.bkPntLn[ii].lat;
        lon[ n_pts++ ] = el.elem.tcb.bkPntLn[ii].lon;
    }

    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof ( TcInfo) 
                        + sizeof (int)*4 + np*sizeof (Breakpt_T) );

    /*
     *  Create tct header and write out the element
     */     
    cvg_crthdr( &el, n_pts, lat, lon, &ier);
    cvg_writefD( &el, start, el.hdr.recsz, fname, &dummy_loc, &ier ); 	
    free( el.elem.tcb.bkPntLn );
    if ( ier < 0 ) *iret = -19;
}
