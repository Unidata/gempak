#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"

/************************************************************************
 * nmap_gmpk.c                                                          *
 *                                                                      *
 * This module contains the GEMPAK related functions for nmap.         	*
 *                                                                      *
 * CONTENTS:                                                            *
 *      gmpk_init()   initialize GEMPAK variables.               	*
 *      gmpk_initTxt() initialize text.               			*
 ***********************************************************************/

/*=====================================================================*/

void gmpk_init ( int *iret )
/************************************************************************
 * gmpk_init                                                         	*
 *                                                                      *
 * This function initializes GEMPAK variables. 				*
 *                                                                      *
 * void gmpk_init ( iret )                                            	*
 *                                                                      *
 * Input parameters:    NONE                                            *
 *                                                                      *
 * Output parameters:  		                                        *
 *      *iret           int     return code                             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      02/93                                                *
 * C. Lin/EAI      03/94                                                *
 * C. Lin/EAI       1/95  add comments, clean up                        *
 *                        add GetColorDefs()                            *
 * C. Lin/EAI      12/95  clean up					*
 * C. Lin/EAI       1/97  add er_stat()					*
 * C. Lin/EAI       1/97  add initialization of gdpltb common block	*
 * C. Lin/EAI       2/98  set allocflag[] in xwcmn.h for graphics	*
 * I. Durham/GSC    5/98  changed underscore decl. to an include	*
 * S. Jacobs/NCEP   7/01  Added call to gmpk_initTxt			*
 * S. Chiswell/UPC  9/02  Added call to xscint for 24 bit support	*
 * K. Brill/HPC     4/03  Use dg_intl to initialize the DG library	*
 * T. Piper/SAIC	05/03	removed items common w/NxmGmpkInit 	*
 * R. Tian/SAIC		01/04	added nuflg to dg_intl call		*
 * R. Tian/SAIC		02/04	removed nuflg from dg_intl call		*
 ***********************************************************************/
{
int  level, bufflg, dttmflg, lflag;
char pname[30];

/*---------------------------------------------------------------------*/
	*iret = 0;
	 /*
          * set the error buffering scheme
	  * different level may be set by the application
	  */
	level   = 0;
	bufflg  = 1;
	dttmflg = 1;
	er_stat( &level, &bufflg, &dttmflg, iret);  /* iret always 0! */

	/*
	 * initialize the GEMPAK Grid Diagnostics (DG) library
	 */
	dg_intl ( iret );  /* iret always 0! */

	/*
	 * initialize GEMPAK gdpltb common block
	 */
	strcpy(pname, "CLEAR");
	lflag = 1;
	gdpstt(pname, &lflag, iret, strlen(pname));
	if ( *iret != 0 ) return;

	strcpy(pname, "SHORT_TITLE");
	lflag = 0;
	gdpstt(pname, &lflag, iret, strlen(pname));
        if ( *iret != 0 ) return;

	strcpy(pname, "PLOT_MAP");
	lflag = 1;
	gdpstt(pname, &lflag, iret, strlen(pname));
        if ( *iret != 0 ) return;

	/*
	 * Initialize the text font info.
	 */
	gmpk_initTxt( );

	return;

}

/*=====================================================================*/

void gmpk_initTxt ( void )
/************************************************************************
 * gmpk_initTxt                                                         *
 *                                                                      *
 * This function initializes text attributes.				*
 *                                                                      *
 * void gmpk_initTxt( )                                             	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      6/97                                                 *
 * S. Jacobs/NCEP	 9/97	Changed call to gstext			*
 * S. Jacobs/NCEP	 7/01	Changed default font to Courier		*
 ***********************************************************************/
{
int   font, type, width, ibrdr, irrotn, ijust, ier;
float size;

/*---------------------------------------------------------------------*/

	font   = 1;	/* courier		*/
        type   = 2;	/* hardware		*/
        size   = 1.0F;	/* medium		*/
        width  = 1;	/* line width		*/
	ibrdr  = 111;	/* no box, no fill	*/
	irrotn = 1;	/* screen relative	*/
	ijust  = 1;	/* left justified	*/

        gstext( &font, &type, &size, &width,
		&ibrdr, &irrotn, &ijust, &ier );

}

/*=====================================================================*/
