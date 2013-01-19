#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "proto_xw.h"

void pgwxd_udlist ( VG_DBStruct el, int *iret );


/************************************************************************
 * nmap_pgwxd.c                                                         *
 *                                                                      *
 * This module controls the creation of the front and tropical cyclone	*
 * text file from the surface analysis vgf for the weather depiction	*
 * charts.								*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *      pgwxd_update()        create weather depiction primary function	*
 *      pgwxd_udlist()        append information to wx depiction text	*
 *      pgwxd_getfname()      build the weather depiction text file name*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void pgwxd_update ( char *fname, int *iret )
/************************************************************************
 * pgwxd_update                                                    	*
 *                                                                      *
 * This function creates a front and tropical cyclone text file for the	*
 * weather depiction from a surface analysis vgf.			*
 * void pgwxd_update( fname, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname		char		VG Filename			*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 1/99	Created.				*
 * F. J. Yen/NCEP	 1/99	Included LINE_ELM for processing SQLN.	*
 * S. Jacobs/NCEP	 2/99	Changed SQLN to FRONT_ELM		*
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * J. Wu/SAIC		01/02	update the current layer only		*
 * E. Safford/SAIC	04/02	add check on fpos			*
 ***********************************************************************/
{
int    		j, ier, ne, fpos, cur_layer, el_layer;
int		ifrt, istr, nblnkfill;
char            q_vgclass, q_vgtype;

char	*frnt_typ[] = {  "STNY", "STNYAS",  "WARM", "WARMAS",  "COLD", 
			"COLDAS",   "OCLD",  "DRYLINE",   "TROF", "SQLN" };
char	*frnt_str[] = { "NOSPEC", "WK", "WK", "WK", "MDT", 
			"MDT", "MDT", "STG", "STG", "STG" };
char	blnktext[] = { "                     " };

VG_DBStruct     el;

char            textstr[80];
/*---------------------------------------------------------------------*/

    *iret = 0;

    clo_init ( &ier );

    /*
     *  Clear the display area.
     */
    pgprd_clear();

    /*
     *  Display date/time template
     */
    sprintf(textstr,"yyyymmddhh");
    pgprd_putstr ( textstr, &ier );

    /*
     *  Scan for fronts, hash marks, tropical storms, and hurricanes.
     */    
    cur_layer = pglayer_getCurLayer ();
    for ( ne = 0; ne < MAX_EDITABLE_ELEMS; ne++ )  {

        crg_gtyp( ne, &q_vgclass, &q_vgtype, &ier );
        crg_goffset( ne, &fpos, &ier );
	if ( fpos < 0 ) continue;

	el_layer = crg_getLayer( fpos );

	if ( el_layer == cur_layer && q_vgtype == FRONT_ELM )  {

            cvg_rdrec( fname, fpos, &el, &ier );

	    ifrt = el.elem.frt.info.fcode / 100;

	    if ( ifrt != 7  &&  ifrt != 8 && ifrt != 9 )  {

	        istr = ( el.elem.frt.info.fcode / 10 ) % 10;
                sprintf( textstr, "\n%s %s ", frnt_typ[ifrt], frnt_str[istr] );

	    }
	    else  {
                sprintf( textstr, "\n%s ", frnt_typ[ifrt] );

	    }

            pgprd_putstr ( textstr, &ier );

	    nblnkfill = 21 - strlen(textstr); 
	    cst_ncpy ( textstr, blnktext, nblnkfill, iret );
            pgprd_putstr ( textstr, &ier );

            pgwxd_udlist ( el, iret );

        }
        else if ( el_layer == cur_layer && q_vgtype == HASH_ELM )  {

            cvg_rdrec( fname, fpos, &el, &ier );

            pgprd_putstr ( "\nHASH                ", &ier );

            pgwxd_udlist ( el, iret );

	}
        else if ( el_layer == cur_layer && q_vgtype == SPSYM_ELM )  {

            cvg_rdrec( fname, fpos, &el, &ier );

	    for ( j = 0; j< el.elem.sym.info.numsym; j++ ) {
		if ( G_NINT(el.elem.sym.data.code[j]) == 26 ) {
		    pgprd_putstr ( "\nHRCN                ", &ier );
        	    pgwxd_udlist ( el, iret );
		}
		if ( G_NINT(el.elem.sym.data.code[j]) == 25 ) {
		    pgprd_putstr ( "\nTROP                ", &ier );
        	    pgwxd_udlist ( el, iret );
		}
	    }

	}
    }

    pgprd_putstr ( "\nEND\n", &ier );

}

/*=====================================================================*/

void pgwxd_udlist ( VG_DBStruct el, int *iret )
/************************************************************************
 * pgwxd_udlist                                                    	*
 *                                                                      *
 * This function processes vertices and other information for element	*
 * el.									*
 *									*
 * void pgwxd_udlist ( el, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  el			VG_DBStruct	Element to process		*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 1/99	Created.				*
 * F. J. Yen/NCEP	 1/99	Included LINE_ELM for processing SQLN.	*
 ***********************************************************************/
{
int    		i, ier, npts, pipdir, irev, fmords;
int		ilat, ilon;
int		leftside = 2;
float		*ltln;

char            textstr[80];
/*---------------------------------------------------------------------*/

    *iret = 0;
    fmords = 0;
    pipdir = 1;

    if ( el.hdr.vg_type == FRONT_ELM ) {
	ltln = &el.elem.frt.latlon[0];
	npts = el.elem.frt.info.numpts;
	fmords = el.elem.frt.info.fcode % 10;

	pipdir = el.elem.frt.info.fpipdr;
	if ( pipdir < 0 ) {
	    irev = npts;
	}	

	if ( fmords != 5 && fmords !=8 ) {
	    fmords = 0;
	}

	/*
	 *  Set pip side to left side of line (value of 2)
         */
    	sprintf ( textstr, "%3d%2d%2d", npts, leftside, fmords );
    }
	
    else if ( el.hdr.vg_type == HASH_ELM ) {
	ltln = &el.elem.wnd.data.latlon[0];
	npts = el.elem.wnd.info.numwnd;
	sprintf ( textstr, "%3d", npts );
    }

    else if ( el.hdr.vg_class == CLASS_SYMBOLS ) {
	ltln = &el.elem.sym.data.latlon[0];
	npts = el.elem.sym.info.numsym;
	sprintf ( textstr, "%3d", npts );
    }

    else if ( el.hdr.vg_type == LINE_ELM ) {
	ltln = &el.elem.lin.latlon[0];
	npts = el.elem.lin.info.numpts;
	fmords = 0;
    	sprintf ( textstr, "%3d%2d%2d", npts, leftside, fmords );

    }

    pgprd_putstr ( textstr, &ier );

    for ( i = 0; i < npts; i++ )  {

	if ( pipdir > 0 ) {
	    ilat = G_NINT( (ltln[i] * 100.0F) );
	    ilon = G_NINT( (ltln[i+npts] * 100.0F) );
	}
	else {
	    irev--;
	    ilat = G_NINT( (ltln[irev] * 100.0F) );
	    ilon = G_NINT( (ltln[irev+npts] * 100.0F) );
	}
	ilon = (int) G_ABS( (float)ilon );

	sprintf ( textstr, "\n%6d%6d", ilat, ilon );
	pgprd_putstr ( textstr, &ier );

	if ( el.hdr.vg_type == HASH_ELM ) {
	    sprintf ( textstr, " %3.0f", el.elem.wnd.data.spddir[i+npts] );
	    pgprd_putstr ( textstr, &ier );
	}

    }

}

/*=====================================================================*/

void pgwxd_getfname ( char *fname, int *iret )
/************************************************************************
 * pgwxd_getfname                                                       *
 *                                                                      *
 * Get filename								*
 *                                                                      *
 * void pgwxd_getfname(fname, iret)					*
 *                                                                      *
 * Input parameters:                                                    *
 *			NONE						*
 *                                                                      *
 * Output parameters:                                                   *
 *  *fname      	char    	Filename                        *
 *  *iret       	int     	Return Code                     *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 1/99	Created.				*
 * F. J. Yen/NCEP	 3/99	Changed output file name to wxd.dat.	*
 ***********************************************************************/
{
    *iret = 0;

    sprintf( fname, "wxd.dat" );
}

/*=====================================================================*/
