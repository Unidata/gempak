#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

static char             *_newFormatTxt;

/*
 *  private callback functions
 */
void pgsfp_updateNewFormat ( char *fname, int max_size, int *iret );


/************************************************************************
 * nmap_pgsfp.c                                                         *
 *                                                                      *
 * This module controls the surface prog creation.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *      pgsfp_update()        create surface prog primary function	*
 *	pgsfp_updateNewFormat() create new format surface prog text	*
 *      pgsfp_udlist()        append information to sfc prog text	*
 *	pgsfp_getfname()	build a file name for surface prog	*
 *	pgsfp_getNewFormatTxt() get pointer to new format text string	*
 *                                                                      *
 * Log:									*
 * F. J. Yen/NCEP	 3/99	Remove pgsfp_getfname. Use cvg_getfnmame*
 ***********************************************************************/

/*=====================================================================*/

void pgsfp_update ( char *fname, int *iret )
/************************************************************************
 * pgsfp_update                                                    	*
 *                                                                      *
 * This function creates a surface prog text product from a vgf file.	*
 *                                                                      *
 * void pgsfp_update( fname, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname		char	VG Filename				*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int	Return code				*
 *				-1 = unable to convert			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 5/98						*
 * I. Durham/GSC         5/98   Changed underscore decl. to an include  *
 * F. J. Yen/NCEP	 1/99	Cleaned up; changed STR to STG; add SQLN*
 * S. Jacobs/NCEP	 2/99	Removed check for LINE_ELM types	*
 * S. Jacobs/NCEP	 2/99	Added front type 9 as TROF for squalls	*
 * D.W.Plummer/NCEP	 2/99	perform G_NINT on label locations	*
 * E. Safford/GSC	04/99	remove unneeded call to crg_ggnxt	*
 * M. Li/GSC		04/00	added a check for string text input 	*
 * D.W.Plummer/NCEP	 5/01	added chk for high & low group types	*
 * J. Wu/SAIC		01/02	update the current layer only		*
 * H. Zeng/SAIC		09.06	added call to pgsfp_updateNewFormat	*
 ***********************************************************************/
{
unsigned int	numchr;
int    		ier, nh, ne, nel, fpos, nc, txt_size;
int    		grpnum, q_grpnum, cur_layer, el_layer;
int		ilat, ilon, ival, ifrt, istr;
char            grptyp, q_grptyp, q_vgclass, q_vgtype, found;
char		first;
float		val;

char   	*sfprg_hilostr[] = { "HIGHS", "LOWS" };
int   	 sfprg_hilocod[] = {      12,     13 };
int   	 sfprg_hilogrp[] = {       5,      6 };

char	*frnt_typ[] = {  "STNRY", "STNRYAS",  "WARM", "WARMAS",  "COLD", 
			"COLDAS",   "OCFNT",  "TROF",   "TROF", "TROF" };
char	*frnt_str[] = { "NOSPEC", "WK", "WK", "WK", "MDT", 
			"MDT", "MDT", "STG", "STG", "STG" };

VG_DBStruct     el;

char            textstr[80], instr[80];
/*---------------------------------------------------------------------*/

    *iret = 0;

    clo_init ( &ier );

    /*
     *  Clear the display area.
     */
    pgprd_clear();

    /*
     *  Display preliminary information
     */
    sprintf(textstr,"12HR PROG VALID xxxxxxZ");
    pgprd_putstr ( textstr, &ier );

    /*
     *  Loop through hilo types.
     */
    cur_layer = pglayer_getCurLayer ();
    for ( nh = 0; nh < 2; nh++ )  {

	first = G_TRUE;

	/*
	 *  Search for symbols; if symbol exists and has a group type
	 *  identifier of 5 (high), 6 (low) or 8 (label), then continue.
	 */
	for ( ne = 0; ne < MAX_EDITABLE_ELEMS; ne++ )  {

	    crg_ggrp( ne, &q_grptyp, &q_grpnum, &ier );
	    crg_gtyp( ne, &q_vgclass, &q_vgtype, &ier );
	    crg_goffset( ne, &fpos, &ier );
            el_layer = crg_getLayer( fpos );
	    
	    if ( el_layer == cur_layer &&
	        (int)q_vgtype == SPSYM_ELM && fpos > 0 && 
		( q_grptyp==sfprg_hilogrp[nh] || q_grptyp==8 ) )  {

		grptyp = q_grptyp;
		grpnum = q_grpnum;

		/*
		 *  Found a valid symbol with grptyp = HIGH/LOW or LABEL; 
		 *  check for code.
		 */
		cvg_rdrec( fname, fpos, &el, &ier );

		if ( (int)el.elem.sym.data.code[0] == sfprg_hilocod[nh] ||
		     ( sfprg_hilocod[nh] == 13 && (int)el.elem.sym.data.code[0] == 38 ) )  {

		    if ( first == G_TRUE )  {
		        sprintf(textstr, "\n%s ", sfprg_hilostr[nh] );
	    	        pgprd_putstr ( textstr, &ier );
			first = G_FALSE;
			numchr = strlen( textstr );
		    }

		    ilat = G_NINT ( el.elem.sym.data.latlon[0] );
		    ilon = G_NINT ( G_ABS( el.elem.sym.data.latlon[1] ));

		    /*
		     *  Search for corresponding text string.
		     */
		    nel = 0;
		    found = G_FALSE;
		    while ( found == G_FALSE && nel < MAX_EDITABLE_ELEMS )  {

			if ( nel != ne )  {

	    		    crg_ggrp( nel, &q_grptyp, &q_grpnum, &ier );
		    	    crg_gtyp( nel, &q_vgclass, &q_vgtype, &ier );
			    crg_goffset( nel, &fpos, &ier);

		    	    if ( (int)q_vgclass == CLASS_TEXT  && fpos > 0 &&
				q_grptyp == grptyp && q_grpnum == grpnum )  {

				cvg_rdrec( fname, fpos, &el, &ier );

				if ((int) q_vgtype == SPTX_ELM) {
                                    sscanf( el.elem.spt.text, "%f", &val );
				    strcpy(instr, el.elem.spt.text);
                                }
                                else {
                                    sscanf( el.elem.txt.text, "%f", &val );
				    strcpy(instr, el.elem.txt.text);
                                }
				cst_ldsp(instr, instr, &nc, &ier);
				ival = G_NINT(val);

				if ( ival < 100 )  {
				    if ( ival >  50 )  ival = ival +  900;
				    if ( ival <= 50 )  ival = ival + 1000;
				}

				if (!isdigit(instr[0]))
				    sprintf(textstr, "0000 ");
 				else
				    sprintf(textstr, "%d ", ival );

				numchr = numchr + strlen( textstr );
				if ( numchr >= 80 )  {
				    if (!isdigit(instr[0]))
					sprintf(textstr, "\n0000 ");
				    else
				        sprintf(textstr, "\n%d ", ival );

				    numchr = strlen( textstr );
				}

	    		    	pgprd_putstr ( textstr, &ier );

				found = G_TRUE;

			    }

			}

			nel++;

		    }

		    sprintf(textstr, "%d%d ", ilat, ilon );

                    numchr = numchr + strlen( textstr );
                    if ( numchr >= 80 )  {
                        sprintf(textstr, "\n%d%d ", ilat, ilon );
			numchr = strlen( textstr );
                    }

	    	    pgprd_putstr ( textstr, &ier );

		}

	    }

	}

    }

    /*
     *  Now scan for fronts
     */
    for ( ne = 0; ne < MAX_EDITABLE_ELEMS; ne++ )  {

        crg_ggrp( ne, &q_grptyp, &q_grpnum, &ier );
        crg_gtyp( ne, &q_vgclass, &q_vgtype, &ier );
        crg_goffset( ne, &fpos, &ier );
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
            pgsfp_udlist ( el.elem.frt.latlon, el.elem.frt.info.numpts,
		    el.elem.frt.info.fpipdr, iret );

	}

    }

    /*
     * create a Surface Prog text message of new format, store the 
     * text message in a local global character string.
     */
    txt_size = pgprd_getTxtSiz();
    pgsfp_updateNewFormat ( fname, txt_size+1000, iret );

}

/*=====================================================================*/

void pgsfp_updateNewFormat ( char *fname, int max_size, int *iret )
/************************************************************************
 * pgsfp_updateNewFormat                                                *
 *                                                                      *
 * This function creates a new format surface prog text product from a  *
 * vgf file.								*
 *                                                                      *
 * void pgsfp_updateNewFormat( fname, max_size, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname		char	VG Filename				*
 *  max_size		int	maximum size for the text message	*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int	Return code				*
 *				-1 = unable to convert			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		09/06	initial coding				*
 * S. Jacobs/NCEP	 8/10	Fixed text wrap for hires format	*
 ***********************************************************************/
{
unsigned int	numchr;
int    		ier, irev, npts, ii, nh, ne, nel, fpos, nc;
int    		grpnum, q_grpnum, cur_layer, el_layer;
int		ilat, ilon, ival, ifrt, pipdir;
char            grptyp, q_grptyp, q_vgclass, q_vgtype, found;
char		first, textstr[80], instr[80];
float		val, *latlon;

char   	*sfprg_hilostr[] = { "HIGHS", "LOWS" };
int   	 sfprg_hilocod[] = {      12,     13 };
int   	 sfprg_hilogrp[] = {       5,      6 };

char	*frnt_typ[] = {  "STNRY", "STNRYAS",  "WARM", "WARMAS",  "COLD", 
			"COLDAS",   "OCFNT",  "TROF",   "TROF", "TROF" };

VG_DBStruct     el;
/*---------------------------------------------------------------------*/

    *iret = 0;

    clo_init ( &ier );

    /*
     *  Clear char string _newFormatTxt and allocate new spaces for it.
     */
    G_FREE   ( _newFormatTxt, char );
    G_MALLOC ( _newFormatTxt, char, max_size+1, 
	       "_newFormatTxt malloc failure" );

    /*
     *  Display preliminary information
     */
    sprintf(textstr,"12HR PROG VALID xxxxxxZ");
    strcpy ( _newFormatTxt, textstr );

    /*
     *  Loop through hilo types.
     */
    cur_layer = pglayer_getCurLayer ();
    for ( nh = 0; nh < 2; nh++ )  {

	first = G_TRUE;

	/*
	 *  Search for symbols; if symbol exists and has a group type
	 *  identifier of 5 (high), 6 (low) or 8 (label), then continue.
	 */
	for ( ne = 0; ne < MAX_EDITABLE_ELEMS; ne++ )  {

	    crg_ggrp( ne, &q_grptyp, &q_grpnum, &ier );
	    crg_gtyp( ne, &q_vgclass, &q_vgtype, &ier );
	    crg_goffset( ne, &fpos, &ier );
            el_layer = crg_getLayer( fpos );
	    
	    if ( el_layer == cur_layer &&
	        (int)q_vgtype == SPSYM_ELM && fpos > 0 && 
		( q_grptyp==sfprg_hilogrp[nh] || q_grptyp==8 ) )  {

		grptyp = q_grptyp;
		grpnum = q_grpnum;

		/*
		 *  Found a valid symbol with grptyp = HIGH/LOW or LABEL; 
		 *  check for code.
		 */
		cvg_rdrec( fname, fpos, &el, &ier );

		if ( (int)el.elem.sym.data.code[0] == sfprg_hilocod[nh] ||
		     ( sfprg_hilocod[nh] == 13 && (int)el.elem.sym.data.code[0] == 38 ) )  {

		    if ( first == G_TRUE )  {
		        sprintf(textstr, "\n%s ", sfprg_hilostr[nh] );
			strcat ( _newFormatTxt, textstr );
			first = G_FALSE;
			numchr = strlen( textstr );
		    }

		    ilat = G_NINT ( el.elem.sym.data.latlon[0] * 10.0 );
		    ilon = G_NINT ( G_ABS( el.elem.sym.data.latlon[1] * 10.0 ));

		    /*
		     *  Search for corresponding text string.
		     */
		    nel = 0;
		    found = G_FALSE;
		    while ( found == G_FALSE && nel < MAX_EDITABLE_ELEMS )  {

			if ( nel != ne )  {

	    		    crg_ggrp( nel, &q_grptyp, &q_grpnum, &ier );
		    	    crg_gtyp( nel, &q_vgclass, &q_vgtype, &ier );
			    crg_goffset( nel, &fpos, &ier);

		    	    if ( (int)q_vgclass == CLASS_TEXT  && fpos > 0 &&
				q_grptyp == grptyp && q_grpnum == grpnum )  {

				cvg_rdrec( fname, fpos, &el, &ier );

				if ((int) q_vgtype == SPTX_ELM) {
                                    sscanf( el.elem.spt.text, "%f", &val );
				    strcpy(instr, el.elem.spt.text);
                                }
                                else {
                                    sscanf( el.elem.txt.text, "%f", &val );
				    strcpy(instr, el.elem.txt.text);
                                }
				cst_ldsp(instr, instr, &nc, &ier);
				ival = G_NINT(val);

				if ( ival < 100 )  {
				    if ( ival >  50 )  ival = ival +  900;
				    if ( ival <= 50 )  ival = ival + 1000;
				}

				if (!isdigit(instr[0]))
				    sprintf(textstr, "0000 ");
 				else
				    sprintf(textstr, "%d ", ival );

				numchr = numchr + strlen( textstr );
				if ( numchr >= 80 )  {
				    if (!isdigit(instr[0]))
					sprintf(textstr, "\n0000 ");
				    else
				        sprintf(textstr, "\n%d ", ival );

				    numchr = strlen( textstr );
				}

			        strcat ( _newFormatTxt, textstr );

				found = G_TRUE;

			    }

			}

			nel++;

		    }

		    sprintf(textstr, "%03d%04d ", ilat, ilon );

                    numchr = numchr + strlen( textstr );
                    if ( numchr >= 80 )  {
                        sprintf(textstr, "\n%03d%04d ", ilat, ilon );
			numchr = strlen( textstr );
                    }

		    strcat ( _newFormatTxt, textstr );

		}

	    }

	}

    }

    /*
     *  Now scan for fronts
     */
    for ( ne = 0; ne < MAX_EDITABLE_ELEMS; ne++ )  {

        crg_ggrp( ne, &q_grptyp, &q_grpnum, &ier );
        crg_gtyp( ne, &q_vgclass, &q_vgtype, &ier );
        crg_goffset( ne, &fpos, &ier );
        el_layer = crg_getLayer( fpos );

        if ( el_layer == cur_layer && q_vgtype == FRONT_ELM )  {

            cvg_rdrec( fname, fpos, &el, &ier );
	    ifrt = el.elem.frt.info.fcode / 100;
            sprintf( textstr, "\n%s ", frnt_typ[ifrt] );
	    strcat ( _newFormatTxt, textstr );

	    /*
             * Append lat/lon coordinates to the surface prog text.
             */
	    latlon = el.elem.frt.latlon;
	    npts   = el.elem.frt.info.numpts;
	    pipdir = el.elem.frt.info.fpipdr;
	    if ( el.elem.frt.info.fpipdr < 0 )  irev = npts;

            for ( ii = 0; ii < npts; ii++ )  {

	      if ( pipdir > 0 ) {
	           ilat = G_NINT( latlon[ii] * 10.0 );
	           ilon = G_NINT( G_ABS(latlon[ii+npts] * 10.0) );
	      }
	      else {
	           irev--;
	           ilat = G_NINT( latlon[irev] * 10.0 );
	           ilon = G_NINT( G_ABS(latlon[irev+npts] * 10.0) );
	      }

	      sprintf(textstr, "%03d%04d ", ilat, ilon );
	      strcat ( _newFormatTxt, textstr );

	      if ( (ii+1)%7 == 0 && (ii+1) < npts )  {
	         sprintf(textstr, "\n" );
	         strcat ( _newFormatTxt, textstr );
	      }

	    }  /* the end of for ( ii... */

	}

    }

}

/*=====================================================================*/

void pgsfp_udlist ( float *latlon, int npts, int pipdir, int *iret )
/************************************************************************
 * pgsfp_udlist                                                    	*
 *                                                                      *
 * This function appends lat/lon coordinates to the surface prog text.	*
 *									*
 * void pgsfp_udlist ( latlon, npts, pipdir, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *  *latlon	float		latlon coordinates			*
 *  npts	int		number of coordinates in latlon		*
 *  pipdir	int		pip flip flag	(-1 if flipped)		*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 5/98						*
 * F. J. Yen/NCEP	 1/99	Handled pip dir. & SQLN.  Change prolog.*
 ***********************************************************************/
{
int    		i, ier, irev;
int		ilat, ilon;

char            textstr[80];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (pipdir < 0) {
	irev = npts;
    }

    for ( i = 0; i < npts; i++ )  {

	if ( pipdir > 0 ) {
	    ilat = G_NINT( latlon[i] );
	    ilon = G_NINT( latlon[i+npts] );
	}
	else {
	    irev--;
	    ilat = G_NINT( latlon[irev] );
	    ilon = G_NINT( latlon[irev+npts] );
	}
	ilon = (int)G_ABS( (float)ilon );

	sprintf(textstr, "%d%d ", ilat, ilon );
	pgprd_putstr ( textstr, &ier );

	if ( (i+1)%10 == 0 && (i+1) < npts )  {
	    sprintf(textstr, "\n" );
	    pgprd_putstr ( textstr, &ier );
	}

    }

}

/*=====================================================================*/

void pgsfp_getfname ( char *fname, int *iret )
/************************************************************************
 * pgsfp_getfname                                                  	*
 *                                                                      *
 * Build filename for sfc prog.						*
 *                                                                      *
 * void pgsfp_getfname(fname, iret)					*
 *                                                                      *
 * Input parameters:                                                    *
 *  none								*
 *                                                                      *
 * Output parameters:                                                   *
 *  *fname	char	Filename					*
 *  *iret	int	Return Code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 5/01						*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
int	ier;
char	fnm[256], fhr[8], *cptr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    pgfilw_getFileName (0, fnm);

    if (strlen (fnm) == (size_t)0 ||
        strncmp (fnm, cvg_getworkfile(), strlen (cvg_getworkfile())) == 0) {
        strcpy (fname, "NoVGFileName.dat");
    }
    else {

	cptr = strchr ( fnm, '.' );

	if ( cptr == (char *)NULL )  {
            strcpy (fname, "InvalidVGFileName.dat");
	}
	else  {
	    cptr -= 4;
            cst_ncpy (fhr, cptr, 4, &ier);
            sprintf (fname, "%s.dat", fhr);
	}

    }

}

/*=====================================================================*/

char* pgsfp_getNewFormatTxt ( void )
/************************************************************************
 * pgsfp_getNewFormatTxt                                                *
 *                                                                      *
 * This function returns a pointer to the new format text string.	*
 *                                                                      *
 * char* pgsfp_getNewFormatTxt ()					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:							*
 *		none							*
 *                                                                      *
 * Return parameters:                                                   *
 *	pgsfp_gerNewFormatTxt	char*	pointer to the text string      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		09/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return ( _newFormatTxt );

}

/*=====================================================================*/
