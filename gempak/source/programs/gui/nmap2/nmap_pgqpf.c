#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

void pgqpf_udlist ( VG_DBStruct *el, int *nptmlin, int *iret );

/************************************************************************
 * nmap_pgqpf.c                                                         *
 *									*
 * This module controls the QPF prog creation.				*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *      pgqpf_update()        create QPF prog primary function		*
 *      pgqpf_udlist()        append information to qpf prog text	*
 *                                                                      *
 **									*
 * Log:									*
 * F. J. Yen/NCEP	3/99	Remove pgqpf_getfname.  Use cvg_getfname*
 ***********************************************************************/

/*=====================================================================*/

void pgqpf_update ( char *fname, int *iret )
/************************************************************************
 * pgqpf_update                                                    	*
 *                                                                      *
 * This function creates a QPF prog text product from a vgf file.	*
 *                                                                      *
 * void pgqpf_update( fname, iret )					*
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
 * F. J. Yen/NCEP	 6/98						*
 * F. J. Yen/NCEP	 2/99	Remove header line.			*
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * J. Wu/SAIC		01/02	update the current layer only		*
 ***********************************************************************/
{
int    		i, j, ngrp, ier, ne, fpos, cur_layer, el_layer ;
int    		grpnum, q_grpnum, grpnumsav[MAX_EDITABLE_ELEMS/2];
int		inx, lastinx, jnx, insert, arrinx[MAX_EDITABLE_ELEMS/2];
int		nelmx, nelm;
int		*inxarry, nptmlin;
int		ival, curval, arval[MAX_EDITABLE_ELEMS/2];
double		val, fcurval;
char		q_grptyp, q_vgclass, q_vgtype;
char		valid, foundline, grpdone; 
VG_DBStruct     elmnt;
char            textstr[80];
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Clear the text.
     */
    pgprd_clear();

    /*
     *  Search for text; if text exists and has a group type
     *  identifier > 0, then continue.
     */
    lastinx = -1;
    ngrp = 0;
    cur_layer = pglayer_getCurLayer ();    
    for ( ne = 0; ne < MAX_EDITABLE_ELEMS; ne++ )  {

	crg_ggrp( ne, &q_grptyp, &q_grpnum, &ier );
	crg_gtyp( ne, &q_vgclass, &q_vgtype, &ier );
	crg_goffset( ne, &fpos, &ier );
	el_layer = crg_getLayer( fpos );

	if (  el_layer == cur_layer && (int)q_vgclass == CLASS_TEXT 
	      && q_grptyp > 0  && q_grpnum > 0 && fpos > 0)  {

	    grpdone = G_FALSE;
	    for ( i = 0; i < ngrp; i++ )  {
	        if (grpnumsav[i] == q_grpnum) {
		    grpdone = G_TRUE;
		    break;
		}
	    }
	    if (grpdone == G_FALSE) {
	        grpnum = q_grpnum;
	        /*
	         *  Determine if valid text by testing if there is at least one
		 *  line in group.  If other than text and line elements, then
		 *  not valid.
		 */

		crg_ggnel ( q_grptyp, q_grpnum, &nelmx, &ier );
		inxarry = (int*)malloc(nelmx*sizeof(int));
		crg_gginx ( q_grptyp, q_grpnum, nelmx, inxarry, &nelm, &ier );
		curval = 0;
		foundline = G_FALSE;
		valid = G_TRUE;
		for ( j = 0; j < nelm; j++ ) { 
		    /*
		     *  Get offset and read record 
		     */
		    crg_goffset ( inxarry[j], &fpos, &ier );
		    cvg_rdrec( fname, fpos, &elmnt, &ier );

		    if ( (int)elmnt.hdr.vg_class == CLASS_TEXT ) {
			if ( (int)elmnt.hdr.vg_type == TEXT_ELM ||
			        (int)elmnt.hdr.vg_type == TEXTC_ELM ) {
			  val = atof (elmnt.elem.txt.text);
			}
			else {
			  val = atof (elmnt.elem.spt.text);
			}
			ival = (int)(val * 100. + .5);
			if (curval == 0) {
			    curval = ival;
			}
			else {
			    if (ival != curval) {
				fcurval = (double)curval * .01; 
				printf(" WARNING:  Text in same group"
				       " not equal.  Using %6.2f.  Other "
				       "value = %6.2f\n", fcurval, val);
			    }
			}
		    }
		    else if ( (int)elmnt.hdr.vg_type == LINE_ELM ) {
			foundline = G_TRUE;
		    }
		    else {
			valid = G_FALSE;
			break;
		    }
		}

		free (inxarry);
				
		if ( valid == G_TRUE && foundline == G_TRUE ) {

		    /*
		     * This is a valid text
		     */

		    grpnumsav[ngrp++] = grpnum;

		    /*
		     *  store curval (precip value) and record index
		     *  into arrays in ascending order of curval
		     */

		    insert = G_FALSE;
		    for ( inx = 0; inx <= lastinx; inx++) {
			if (ival < arval[inx]) {

			  for ( jnx = ++lastinx; inx < jnx; jnx--) {
			    arval[jnx] = arval[jnx - 1];
			    arrinx[jnx] = arrinx[jnx - 1];
			  }
				  
			  arval[jnx] = curval;
			  arrinx [jnx] = ne;
			  insert = G_TRUE;
			  break;
			}
		    }
		    if (insert == G_FALSE) {
		        arval[++lastinx] = curval;
		        arrinx[lastinx] = ne;
		    }

		}

	    }

        }

    }
    /*
     * Put out records
     */

    for (jnx=0; jnx <= lastinx; jnx++) {

	if ( jnx != 0 ) {
	    sprintf ( textstr, "\n" );
	    pgprd_putstr ( textstr, &ier );
	}
	val = (double)arval[jnx] * .01;
	if ( arval[jnx] < 1000 ) {
	    sprintf ( textstr, " %04.2f ", val);
	}
	else {
	    sprintf ( textstr, "%05.2f ", val);
	}
	pgprd_putstr ( textstr, &ier );

	/*
	 * Get corresponding line records.  First get index of text record.
	 */
	
	crg_ggrp( arrinx[jnx], &q_grptyp, &q_grpnum, &ier );
	crg_gtyp( arrinx[jnx], &q_vgclass, &q_vgtype, &ier );

        crg_ggnel ( q_grptyp, q_grpnum, &nelmx, &ier );
        inxarry = (int*)malloc(nelmx*sizeof(int));
        crg_gginx ( q_grptyp, q_grpnum, nelmx, inxarry, &nelm, &ier );

        nptmlin = 0;
	for (j=0; j<nelm; j++) {
	    crg_goffset( inxarry[j], &fpos, &ier );
	    cvg_rdrec( fname, fpos, &elmnt, &ier );
	    if ( (int)elmnt.hdr.vg_type == LINE_ELM ) {

 	        pgqpf_udlist ( &elmnt, &nptmlin, iret );
	    }
	}
	free (inxarry);
    }
}

/*=====================================================================*/

void pgqpf_udlist ( VG_DBStruct *elmnt, int *nptmlin, int *iret )
/************************************************************************
 * pgqpf_udlist                                                    	*
 *                                                                      *
 * This function reads in the element at position fpos in VG file fname	*
 * and processes the vertices						*
 *									*
 * void	pgqpf_udlist ( elmnt, nptmlin, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *elmnt	VG_DBStruct						*
 *									*
 * Input/output parameters:						*
 *  *nptmlin	int		No. of points already put out minus 1	*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 6/98						*
 * S. Jacobs/NCEP	 3/03	Remove duplicate points after rounding	*
 ***********************************************************************/
{
int    		i, ier, npts;
int		ilat, ilon, ilatsv, ilonsv, knt;
char            textstr[80], latlontxt[8], firstpnt;
/*---------------------------------------------------------------------*/

    *iret = 0;

    npts = elmnt->elem.lin.info.numpts;
    ilatsv = IMISSD;
    ilonsv = IMISSD;
    knt = 0;

    for ( i = 0; i < npts; i++ )  {
	if ( (i==0) && (*nptmlin==0) ) {
	    firstpnt = G_TRUE;
	}
	else {
	    firstpnt = G_FALSE;
	}

	ilat = (int)(elmnt->elem.lin.latlon[i] * 10.F + .5F);
	ilon = (int)(G_ABS( elmnt->elem.lin.latlon[i+npts]) * 10.F + .5F);
	ilon = (ilon)%1000;

	if  ( ilat == ilatsv && ilon == ilonsv )  {
	    /* Skip this point */;
	}
	else {

	    if ( (knt+1+*nptmlin)%8 == 1 && firstpnt == G_FALSE ) {
		sprintf(textstr, "\n      " );
		pgprd_putstr ( textstr, &ier );
	    }
	    sprintf(textstr, "%03d%03d ", ilat, ilon );
	    pgprd_putstr ( textstr, &ier );
	    /*
	     * If closed line, save first lat/lon text string
	     */
	    if ( knt == 0 && elmnt->hdr.closed == (char)1 ) {
		strcpy (latlontxt, textstr); 
	    }

	    knt++;
	    ilatsv = ilat;
	    ilonsv = ilon;
	}

    }


    if ( elmnt->hdr.closed == (char)1 ) {
	/*
	 *  closed line (so not multi-line), put out first lat/lon text string
	 */
        if ( (knt)%8 == 0 )  {
	    sprintf(textstr, "\n      " );
	    pgprd_putstr ( textstr, &ier );
        }
	pgprd_putstr ( latlontxt, &ier );
	*nptmlin = npts;
    }
    else {
        *nptmlin = *nptmlin + npts;
    }

}
