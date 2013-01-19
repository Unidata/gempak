#include "geminc.h"
#include "gemprm.h"
#include "hints.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "Nxm.h"
#include "nmap_data.h"


#define AODT_VTBL      "aodtvers.tbl"

static char	_aodtwVer[16];

typedef struct  {
        char    name[80];       /* AODT version name    */
        char    vernum[12];     /* AODT version number  */
} VERinfo;

typedef struct {
        int     nver;           /* Number of AODT versions      */
        VERinfo *info;          /* AODT version information     */
} AODTVer_t;

AODTVer_t	_verTbl;

/************************************************************************
 * nmap_aodtw.c								*
 *									*
 * This module defines AODT popup window for nmap			*
 *									*
 * CONTENTS:								*
 *	aodtw_create()		create the AODT window			*
 *	aodtw_popup()		pop up the AODT window			*
 *	aodtw_isUp()		query if the window is up 		*
 *	aodtw_refresh()		redraw the ghostings			*
 *	aodtw_readTable()	load AODT version info table		*
 *	aodtw_setvers()		set AODT version			*
 *	aodtw_getnumvers()	query the number of AODT versions	*
 *	aodtw_getversnm()	query AODT version names		*
 ***********************************************************************/

/*=====================================================================*/

void aodtw_create ( Widget parent )
/************************************************************************
 * aodtw_create								*
 *									*
 * This function creates the AODT popup window.				*
 *									*
 * Widget aodtw_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * aodtw_create	Widget	ID of the AODT popup window			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA     01/04	initial coding				*
 * H. Zeng/SAIC	     04/04	added history file menu			*
 * H. Zeng/SAIC	     05/04	added print button callback		*
 * H. Zeng/SAIC	     06/04	change # of row for _mesgTxtW		*
 * E. Safford/SAIC   05/05	free fontlist				*
 * M. Li/SAIC        05/05      Added callback function for _histTxtW   *
 * M. Li/SAIC	     06/05	Rewrite for v6.3 and v6.4		*
 * M. Li/SAIC	     12/06	Added v7.2, removed v6.3		*
 ***********************************************************************/
{
    char    mesg[] = "Invalid AODT version!\n";
/*---------------------------------------------------------------------*/

    if ( strcmp ( _aodtwVer, "7.2" ) == 0 ) {
        if ( aodtw72_create ( parent )  == NULL ) 
	    aodtw72_create ( parent );
    }
    else if ( strcmp ( _aodtwVer, "6.4" ) == 0 ) {
        if ( aodtw64_create ( parent )  == NULL ) 
   	    aodtw64_create ( parent );
    }
    else {
        NxmWarn_show (parent, mesg);
    }
}

/*=====================================================================*/

void aodtw_popup ( void )
/************************************************************************
 * aodtw_popup								*
 *									*
 * This function pops up AODT popup window.				*
 *									*
 * void aodtw_popup()							*
 *									*
 * Input parameters:							*
 *	ver		char*	which version of AODT			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/SAIC		04/04	added version info			*
 * T. Piper/SAIC	12/04	Added aodtw_refresh			*
 * M. Li/SAIC		06/05	Rewrite for v6.3 and v6.4		*
 * M. Li/SAIC	     	12/06	Added v7.2, removed v6.3		*
 ***********************************************************************/
{
    Widget  draw_w;
    char    mesg[] = "Invalid AODT version!\n";
/*---------------------------------------------------------------------*/

    if ( strcmp ( _aodtwVer, "7.2" ) == 0 ) {
	aodtw72_popup ( "v7.2" );
    }
    else if ( strcmp ( _aodtwVer, "6.4" ) == 0 ) {
        aodtw64_popup ( "v6.4" );
    }
    else {
  	draw_w = (Widget)mcanvw_getDrawingW();
        NxmWarn_show (draw_w, mesg);
    }
}

/*=====================================================================*/

Boolean aodtw_isUp ( void ) 
/************************************************************************
 * aodtw_isUp								*
 *									*
 * This function queries whether the AODT window is up.			*
 *									*
 * Boolean aodtw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * cldhgtw_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * M. Li/SAIC		06/05	Rewrite for v6.3 and v6.4		*
 * M. Li/SAIC	     	12/06	Added v7.2, removed v6.3		*
 ***********************************************************************/
{
    Widget  draw_w;
    char    mesg[] = "Invalid AODT version!\n";
/*---------------------------------------------------------------------*/

    if ( strcmp ( _aodtwVer, "7.2" ) == 0 ) {
        return aodtw72_isUp();
    }
    else if ( strcmp ( _aodtwVer, "6.4" ) == 0 ) {
        return aodtw64_isUp();
    }
    else {
        draw_w = (Widget)mcanvw_getDrawingW();
        NxmWarn_show (draw_w, mesg);
    }
    return (False);
}

/*=====================================================================*/

void aodtw_refresh ( Boolean make_new )
/************************************************************************
 * aodtw_refresh                                                        *
 *                                                                      *
 * This function redraws the ghosting.                                  *
 *                                                                      *
 * void aodtw_refresh ( make_new )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 * make_new	Boolean	Flag for make_new or using existing location	*
 * 									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/04   Copied from cldhgtw_refresh             *
 * M. Li/SAIC		06/05	Rewrite for v6.3 and v6.4		*
 * M. Li/SAIC	     	12/06	Added v7.2, removed v6.3		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( strcmp ( _aodtwVer, "7.2" ) == 0 ) {
        aodtw72_refresh ( make_new );
    }
    else if ( strcmp ( _aodtwVer, "6.4" ) == 0 ) {
        aodtw64_refresh ( make_new );
    }

}

/*=====================================================================*/

void aodtw_readTable ( int *iret )
/************************************************************************
 * aodtw_readTable                                                      *
 *                                                                      *
 * This function loads the aodtvers.tbl table into its structure.       *
 *                                                                      *
 * aodtw_readTable ( iret )                                             *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 * 	*iret           int     Return code                             *
 *                                -1 - Unable to open table      	*
 *                                -2 - No valid records in table 	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		06/05						*
 ***********************************************************************/
{
FILE    *fp;
int     ii, nn, nr, ier;
char    buffer[256];

/*---------------------------------------------------------------------*/
        *iret = G_NORMAL;

        /*
         *  Open the aodtvers table. If not found return an error.
         */

        fp = (FILE *)cfl_tbop( AODT_VTBL, "nmap", &ier);
        if ( fp == NULL  ||  ier != 0 )  {
                *iret = -1;
                return;
        }

        cfl_tbnr(fp, &nr, &ier);
        if ( ier != 0 || nr == 0 ) {
            *iret = -2;
            return;
        }

        _verTbl.info = (VERinfo *)malloc( nr * sizeof(VERinfo) );
	for ( ii = 0; ii < nr; ii++ ) {
	    _verTbl.info[ii].name[0]    = CHNULL;
	    _verTbl.info[ii].vernum[0]  = CHNULL;
	}

	rewind(fp);
        nn  = 0;
        while ( nn < nr && !feof(fp) ) {

            cfl_trln( fp, 256, buffer, &ier );
            if ( ier != 0 ) break;

            sscanf( buffer, "%s %s", _verTbl.info[nn].name, _verTbl.info[nn].vernum );
            nn++;
        }

        cfl_clos(fp, &ier);

        _verTbl.nver = nn;

}

/*=====================================================================*/

void aodtw_setvers ( char *vers, int *iret )
/************************************************************************
 * aodtw_setvers                                                        *
 *                                                                      *
 * This function determines which version the user wants based on the   *
 * mouse action for the AODT button and the entries read from table 	*
 * aodtvers.tbl.							*
 *                                                                      *
 * aodtw_setvers ( vers, iret )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *	*vers	char	version name 					*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*iret	int     Return code                             	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/05                                           *
 ***********************************************************************/
{
int	ii;
Boolean	found = False;

/*---------------------------------------------------------------------*/
        *iret = G_NORMAL;

	_aodtwVer[0] = CHNULL;
	for ( ii = 0; ii < _verTbl.nver; ii++ ) { 
	    if ( strcmp ( _verTbl.info[ii].name, vers ) == 0 ) {
		strcpy ( _aodtwVer, _verTbl.info[ii].vernum );
		found = True;
		break;
	    }
	}
	if ( !found ) *iret = -1;
}

/*=====================================================================*/

void aodtw_getnumvers ( int *numv, int *maxlen)
/************************************************************************
 * aodtw_getnumvers                                                     *
 *                                                                      *
 * This function queries the number of AODT versions and the maximum    *
 * length of the version names in table aodtvers.tbl.              	*
 *                                                                      *
 * aodtw_getnumvers ( numv, maxlen )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *numv   int     Number of AODT versions                         *
 *	*maxlen int	Maximum length of AODT version names		*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/05                                           *
 ***********************************************************************/
{
int     ii;
size_t	mlen;
/*---------------------------------------------------------------------*/
	*numv = _verTbl.nver;

	mlen = 0;
        for ( ii = 0; ii < _verTbl.nver; ii++ ) {
	    mlen = G_MAX ( mlen, strlen ( _verTbl.info[ii].name ) ); 
        }
        *maxlen = mlen;
}

/*=====================================================================*/

void aodtw_getversnm ( char ***vname )
/************************************************************************
 * aodtw_getversnm                                                      *
 *                                                                      *
 * This function queries AODT version names from table aodtvers.tbl.   	*
 *                                                                      *
 * aodtw_getversnm ( vname )                                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      **vname	char	AODT version names                              *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/05                                           *
 * M. Li/SAIC		07/05	mlen->mlen+1, used G_MALLOC		*
 ***********************************************************************/
{
int     ii, nv, mlen;

/*---------------------------------------------------------------------*/

 	aodtw_getnumvers ( &nv, &mlen );

   	G_MALLOC ( (*vname), char*, nv, "*vname" );
	for ( ii = 0; ii < nv; ii++ ) {
	    G_MALLOC ( (*vname)[ii], char, (mlen + 1), "(*vname)[ii]" );
	}
	
        for ( ii = 0; ii < nv; ii++ ) {
            strcpy ( (*vname)[ii], _verTbl.info[ii].name );
        }
}

/*=====================================================================*/
