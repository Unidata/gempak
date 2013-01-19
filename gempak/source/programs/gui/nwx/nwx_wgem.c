#include "nwx_cmn.h"


/************************************************************************
 * nwx_wgem.c                                                           *
 *                                                                      *
 * This module contains wrapper functions for the gemplot system calls  *
 * from the NWX functions.						*
 *                                                                      *
 * CONTENTS:                                                            *
 *	wgem_er_wmsg		er_wmsg() wrapper			*
 *	wgem_gclear 		gclear()  wrapper			*
 *	wgem_geplot		geplot()  wrapper			*
 *	wgem_gmesg		gmesg()   wrapper			*
 *	wgem_ggtpnt		ggtpnt()  wrapper			*
 *	wgem_gg_ltln 		gg_ltln() wrapper			*
 *	wgem_gg_map 		gg_map()  wrapper			*
 *	wgem_gg_maps		gg_maps() wrapper			*
 *	wgem_gg_panl		gg_panl() wrapper			*
 *	wgem_gg_wlbl		gg_wlbl() wrapper			*
 *	wgem_gg_wstr		gg_wstr() wrapper			*
 *	wgem_gline  		gline()   wrapper			*
 *	wgem_gmark  		gmark()   wrapper			*
 *	wgem_gqline  		gqline()  wrapper			*
 *	wgem_gqmprj 		gqmprj()  wrapper			*
 *	wgem_gqtext 		gqtext()  wrapper			*
 *	wgem_gsatim 		gsatim()  wrapper			*
 *	wgem_gscolr 		gscolr()  wrapper			*
 *	wgem_gsline 		gsline()  wrapper			*
 *	wgem_gsmrkr		gsmrkr()  wrapper			*
 *	wgem_gsmfil		gsmfil()  wrapper			*
 *	wgem_gstext 		gstext()  wrapper			*
 *	wgem_gtext 		gtext()   wrapper			*
 *	wgem_gtextc 		gtextc()  wrapper			*
 *	wgem_gtrans		gtrans()  wrapper			*
 *	wgem_in_text		in_text() wrapper			*
 *	wgem_xxflsh		xxflsh()  wrapper			*
 ***********************************************************************/


/*=====================================================================*/

void wgem_er_wmsg( char *errgrp, int *numerr, char *errstr, int *iret ) 
/************************************************************************
 * wgem_er_wmsg              						*
 *									*
 * This function is a wrapper for er_wmsg. 				*
 *									*
 * void wgem_er_wmsg( errgrp, numerr, errstr, iret )			*
 *									*
 * Input parameters:							*
 *	*errgrp		char	error group				*
 *	*numerr		int	error number				*
 *	*errstr		char	error string to be embedded		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    er_wmsg( errgrp, numerr, errstr, iret, strlen( errgrp ), strlen( errstr ) );
}

/*=====================================================================*/

void wgem_gclear( int *iret ) 
/************************************************************************
 * wgem_gclear              						*
 *									*
 * This function is a wrapper for gclear. 				*
 *									*
 * void wgem_gclear( iret )						*
 *									*
 * Input parameters:							*
 *	   		None	                   			*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gclear( iret );
}

/*=====================================================================*/

void wgem_geplot( int *iret ) 
/************************************************************************
 * wgem_geplot              						*
 *									*
 * This function is a wrapper for geplot. 				*
 *									*
 * void wgem_gplot( iret )						*
 *									*
 * Input parameters:							*
 *	   		None	                   			*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    geplot( iret );
}

/*=====================================================================*/

void wgem_gmesg( char *messag, int *iret )  
/************************************************************************
 * wgem_gmesg              						*
 *									*
 * This function is a wrapper for gmesg. 				*
 *									*
 * void wgem_gmesg( messag, iret )					*
 *									*
 * Input parameters:							*
 *	*messag		char	message					*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gmesg( messag, iret, strlen( messag ) );
}

/*=====================================================================*/

void wgem_ggtpnt( char *sys, int *ityp, float *x, float *y, int *iret ) 
/************************************************************************
 * wgem_gclear              						*
 *									*
 * This function is a wrapper for ggtpnt, which returns the requested   *
 * number of points from the cursor position with the mouse button is   *
 * pressed. 								*
 *									*
 * void wgem_gclear( iret )						*
 *									*
 * Input parameters:							*
 *	*sys		char	coordinate system			*
 *	*ityp		int	type of cursor				*
 *									*
 * Output parameters:							*
 *	*x		float	X coordinates/latitudes			*
 *	*y		float	Y coordinates/longitudes		*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    ggtpnt( sys, ityp, x, y, iret, strlen( sys ) );
}

/*=====================================================================*/

void wgem_gg_ltln( char *ltln, int *iret ) 
/************************************************************************
 * wgem_gg_ltln               						*
 *									*
 * This function is a wrapper for gg_map.				*
 *									*
 * void wgem_gg_ltln( ltln, iret )					*
 *									*
 * Input parameters:							*
 *	*ltln 		char	line color/type/width/lblfr/inc		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gg_ltln( ltln, iret, strlen( ltln ) );
}

/*=====================================================================*/

void wgem_gg_map( char *map, int *iret ) 
/************************************************************************
 * wgem_gg_map               						*
 *									*
 * This function is a wrapper for gg_map.				*
 *									*
 * void wgem_gg_map( proj, garea, imgfil, idrpfl, iret )		*
 *									*
 * Input parameters:							*
 *	*map 		char	map color/line/type/width/filter	*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gg_map( map, iret, strlen( map ) );
}

/*=====================================================================*/

void wgem_gg_maps( char *proj, char *garea, char *imgfil, int *idrpfl, int *iret ) 
/************************************************************************
 * wgem_gg_maps              						*
 *									*
 * This function is a wrapper for gg_maps.				*
 *									*
 * void wgem_gg_maps( proj, garea, imgfil, idrpfl, iret )		*
 *									*
 * Input parameters:							*
 *	*proj		char	map projection   			*
 *	*garea		char	graphics area 				*
 *	*imgfil		char	image (sat/rad) file name		*
 *									*
 * Output parameters:							*
 *	*idrpfl		int	flag to drop the image 			*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gg_maps( proj, garea, imgfil, idrpfl, iret, 
    		strlen( proj ), strlen( garea ), strlen( imgfil ) );
}

/*=====================================================================*/

void wgem_gg_panl( char *panl, int *iret )
/************************************************************************
 * wgem_gg_panl              						*
 *									*
 * This function is a wrapper for gg_panl.				*
 *									*
 * void wgem_gg_panl( panl, iret )                       		*
 *									*
 * Input parameters:							*
 *	*panl		char	panel string     			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gg_panl( panl, iret, strlen( panl ) );
}

/*=====================================================================*/

void wgem_gg_wlbl( int *np, float *rlat, float *rlon, 
		   float *alat, float *alon, int *iret )
/************************************************************************
 * wgem_gg_wlbl              						*
 *									*
 * This function is a wrapper for gg_wlbl. 				*
 *									*
 * void wgem_gg_wlbl( np, rlat, rlon, alat, alon, iret )		*
 *									*
 * Input parameters:							*
 *	*np		int	number of points			*
 *	*rlat		float	latitudes of points			*
 *	*rlon		float	longitudes of points			*
 *									*
 * Output parameters:							*
 *	*alat		float	latitude of label			*
 *	*alon		float	longitude of label			*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gg_wlbl( np, rlat, rlon, alat, alon, iret );
}

/*=====================================================================*/

void wgem_gg_wstr( char *string, int *line, int *iret )
/************************************************************************
 * wgem_gg_wstr              						*
 *									*
 * This function is a wrapper for gg_wstr. 				*
 *									*
 * void wgem_gg_wstr( string, line, iret )				*
 *									*
 * Input parameters:							*
 *	*string		char	string to be written			*
 *	*line		int	line number        			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gg_wstr( string, line, iret, strlen( string ) );
}

/*=====================================================================*/

void wgem_gline( char *sys, int *np, float *x, float *y, int *iret ) 
/************************************************************************
 * wgem_gline              						*
 *									*
 * This function is a wrapper for gline. 				*
 *									*
 * void wgem_gline( sys, np, x, y, iret )				*
 *									*
 * Input parameters:							*
 *	*sys		char	coordinate system			*
 *	*np		int	number of points   			*
 *	*x 		float	X coordinates / lats			*
 *	*y 		float	Y coordinates / lons			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gline( sys, np, x, y, iret, strlen( sys ) );
}

/*=====================================================================*/

void wgem_gmark( char *sys, int *np, float *x, float *y, int *iret ) 
/************************************************************************
 * wgem_gmark              						*
 *									*
 * This function is a wrapper for gmark. 				*
 *									*
 * void wgem_gmark( sys, np, x, y, iret )				*
 *									*
 * Input parameters:							*
 *	*sys		char	coordinate system			*
 *	*np		int	number of points   			*
 *	*x 		float	X coordinates / lats			*
 *	*y 		float	Y coordinates / lons			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gmark( sys, np, x, y, iret, strlen( sys ) );
}

/*=====================================================================*/

void wgem_gqline( int *iltyp, int *ilthw, int *iwidth, int *iwhw, int *iret ) 
/************************************************************************
 * wgem_gqline              						*
 *									*
 * This function is a wrapper for gqline. 				*
 *									*
 * void wgem_gqline( iltyp, ilthw, iwidth, iwhw, iret )			*
 *									*
 * Input parameters:							*
 *	*iltyp		int	line type number 			*
 *	*ilthw		int	line type flag     			*
 *	*iwidth 	int	line width          			*
 *	*iwhw		int	line width flag     			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gqline( iltyp, ilthw, iwidth, iwhw, iret );
}

/*=====================================================================*/

void wgem_gsmfil( char *mapfil, int *iret ) 
/************************************************************************
 * wgem_gsmfil              						*
 *									*
 * This function is a wrapper for gsmfil. 				*
 *									*
 * void wgem_gsmfil( mapfil, iret )                  			*
 *									*
 * Input parameters:							*
 *	*mapfil		char	map file name    			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gsmfil( mapfil, iret, strlen( mapfil ) );
}

/*=====================================================================*/

void wgem_gqmprj( char *proj, float *angl1, float *angl2, float *angl3,
		  float *dlatll, float *dlonll, float *dlatur, 
		  float *dlonur, int *iret )
/************************************************************************
 * wgem_gqmprj								*
 *									*
 * This function is a wrapper for gqmprj.				*
 *									*
 * void wgem_gqmprj ( proj, angl1, angl2, angl3, dlatll, dlonll, 	*
 *		      dlatur, dlonur, iret )				*
 *									*
 * Input parameters:							*
 *	*proj		char	projection string			*
 *	*angl1		float	angle 1					*
 *	*angl2		float	angle 2					*
 *	*angl3		float 	angle 3					*
 *	*dlatll		float	lat, lower left corner			*
 *	*dlonll		float	lon, lower left corner			*
 *	*dlatur		float	lat, upper right corner			*
 *	*dlonur		float	lon, upper right corner			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gqmprj( proj, angl1, angl2, angl3, dlatll, dlonll, dlatur, dlonur,
    		iret, sizeof( proj ) );
}

/*=====================================================================*/

void wgem_gqtext( int *itxfn, int *itxhw, float *sztext, int *itxwid,
		  int *brdr, int *irrotn, int *ijust, int *iret )
/************************************************************************
 * wgem_gqtext								*
 *									*
 * This function is a wrapper for gqtext.				*
 *									*
 * void wgem_gqmprj ( itxfn, itxhw, sztext, itxwid, brdr, irrotn,       *
 *		      ijust, iret )          				*
 *									*
 * Input parameters:							*
 *	*itxfn		int	font number      			*
 *	*itxhw		int	software/hardware flag			*
 *	*sztext		float	text size multiplier			*
 *	*itxwid		int 	text line width				*
 *	*ibrdr		int	text border/blank fill flag		*
 *	*irrotn		int	text north relative rotation flag	*
 *	*ijust		int	text justification     			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gqtext( itxfn, itxhw, sztext, itxwid, brdr, irrotn, ijust, iret );
}

/*=====================================================================*/

void wgem_gsatim( char *filnam, int *iret ) 
/************************************************************************
 * wgem_gsatim								*
 *									*
 * This function is a wrapper for gsatim.				*
 *									*
 * void wgem_gsatim ( filnam, iret )                                    *
 *									*
 * Input parameters:							*
 *	*filnam		char	file name        			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gsatim( filnam, iret, strlen( filnam ) ); 
}

/*=====================================================================*/

void wgem_gscolr( int *icolr, int *iret )
/************************************************************************
 * wgem_gscolr             						*
 *									*
 * This function is a wrapper for gscolr.				*
 *									*
 * void wgem_gscolr( icolr, iret )					*
 *									*
 * Input parameters:							*
 *	*icolr		int	color number       			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gscolr( icolr, iret );
}

/*=====================================================================*/

void wgem_gsline( int *iltyp, int *ilthw, int *iwidth, int *ilwhw, int *iret )
/************************************************************************
 * wgem_gsline             						*
 *									*
 * This function is a wrapper for gsline.				*
 *									*
 * void wgem_gsline( iltyp, ilthw, iwidth, ilwhw, iret )		*
 *									*
 * Input parameters:							*
 *	*iltyp		int	line type          			*
 *	*ilthw		int	sw/hw line type flag      		*
 *	*iwidth		int	line width size multiplier		*
 *	*ilwhw		int	sw/hw line width flag     		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gsline( iltyp, ilthw, iwidth, ilwhw, iret );
}

/*=====================================================================*/

void wgem_gsmrkr( int *imark, int *imkhw, float *szmark, 
		  int *imkwid, int *iret )
/************************************************************************
 * wgem_gsmrkr             						*
 *									*
 * This function is a wrapper for gsmrkr.				*
 *									*
 * void wgem_gsmrkr( imark, imkhw, sizmark, imkwid, iret )		*
 *									*
 * Input parameters:							*
 *	*imark		int	marker number      			*
 *	*imkhw		int	sw/hw marker flag         		*
 *	*szmark		float	marker size multiplier    		*
 *	*imkwid		int	marker line width         		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gsmrkr( imark, imkhw, szmark, imkwid, iret );
}

/*=====================================================================*/

void wgem_gstext( int *itxfn, int *itxhw, float *sztext, int *itxwid, 
		  int *ibrdr, int *irrotn, int *ijust, int *iret )
/************************************************************************
 * wgem_gstext             						*
 *									*
 * This function is a wrapper for gstext.				*
 *									*
 * void wgem_gstext( itxfn, itxhw, sztext, itxwid, ibrdr, irrotn,       *
 *		     ijust, iret )					*
 *									*
 * Input parameters:							*
 *	*itxfn		int	text font          			*
 *	*itxhw		int	Text sw/hw flag     			*
 *	*sztext		float	text size           			*
 *	*itxwid		int	text line width    			*
 *	*ibrdr		int	text border/blank fill size		*
 *	*irrotn		int	text north-relative rotation flag	*
 *	*ijust		int	text justification         		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gstext( itxfn, itxhw, sztext, itxwid, ibrdr, irrotn, ijust, iret );
}

/*=====================================================================*/

void wgem_gtextc( char *sys, float *x, float *y, char *cchar, float *rotat,
		  int *ixoff, int *iyoff, int *iret )
/************************************************************************
 * wgem_gtextc             						*
 *									*
 * This function is a wrapper for gtextc.				*
 *									*
 * void wgem_gtextc( sys, x, y, cchar, rotat, ixoff, iyoff, iret )	*
 *									*
 * Input parameters:							*
 *	*sys		char	coordinate system  			*
 *	*x		float	X coordinates / lats			*
 *	*y		float	Y coordinates / lons			*
 *	*cchar		char	text string to plot			*
 *	*rotat		float	rotation angle in degrees		*
 *	*ixoff		int	X offest in half characters		*
 *	*iyoff		int	Y offest in half characters		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/07	initial coding                          *
 ***********************************************************************/
{
    gtextc( sys, x, y, cchar, rotat, ixoff, iyoff, iret, 
    	    strlen( sys ), strlen( cchar ) );
}

/*=====================================================================*/

void wgem_gtext( char *sys, float *x, float *y, char *cchar, float *rotat,
		  int *ixoff, int *iyoff, int *iret )
/************************************************************************
 * wgem_gtext              						*
 *									*
 * This function is a wrapper for gtext.				*
 *									*
 * void wgem_gtext( sys, x, y, cchar, rotat, ixoff, iyoff, iret )	*
 *									*
 * Input parameters:							*
 *	*sys		char	coordinate system  			*
 *	*x		float	X coordinates / lats			*
 *	*y		float	Y coordinates / lons			*
 *	*cchar		char	text string to plot			*
 *	*rotat		float	rotation angle in degrees		*
 *	*ixoff		int	X offest in half characters		*
 *	*iyoff		int	Y offest in half characters		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gtext( sys, x, y, cchar, rotat, ixoff, iyoff, iret, 
    	    strlen( sys ), strlen( cchar ) );
}

/*=====================================================================*/

void wgem_gtrans( char *sysin, char *sysout, int *np, float *xin, float *yin, 
		  float *xout, float *yout, int *iret )
/************************************************************************
 * wgem_gtrans              						*
 *									*
 * This function is a wrapper for gtrans.				*
 *									*
 * void wgem_gtrans( sysin, sysout, np, xin, yin, xout, yout, iret )	*
 *									*
 * Input parameters:							*
 *	*sysin		char	input coordinate system			*
 *	*sysout		char	output coordinate system		*
 *	*np		int	number of points			*
 *	*xin		float	input X coordinates / lats		*
 *	*yin		float	input Y coordinates / lons		*
 *									*
 * Output parameters:							*
 *	*xout		float	output X coordinates / lats		*
 *	*yout		float	output Y coordinates / lons		*
 *	*iret		int	return code				*
 **									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    gtrans( sysin, sysout, np, xin, yin, xout, yout, iret, 
    	    strlen( sysin ), strlen( sysout ) );
}

/*=====================================================================*/

void wgem_in_text( char *text, int *iret )
/************************************************************************
 * wgem_in_text              						*
 *									*
 * This function is a wrapper for in_text.				*
 *									*
 * void wgem_in_text( text, iret )                                  	*
 *									*
 * Input parameters:							*
 *	*text		char	input text             			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 **									*
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    in_text( text, iret, strlen( text ) );
}

/*=====================================================================*/
