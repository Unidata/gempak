#include "cvgcmn.h"

#define GRPTYP		99
#define NUMCOMBO	27

void cvgkj_set ( int num_sym, float *cmbcode, float *cnvtbl, int topflg,
			VG_DBStruct *el, int *oset_xy, int *iret );
void cvgkj_wrt ( VG_DBStruct *el, int *oset_xy, int num_sym,
			FILE *ofp, int *ofposa, int *iret );

void cvg_c3k3j ( char *ifname, char *ofname, int *iret )
/************************************************************************
 * cvg_c3k3j								*
 *									*
 * This function converts a VGF file from version 5.4.3.k to 5.4.3.j 	*
 * Necessary conversion:						*
 *	 A Combo Symbol in 5.4.3.k is a single element.  Whereas, in 	*
 *	 5.4.3.j it is a group of three symbol elements.		*
 *									*
 * cvg_c3k3j ( ifname, ofname, iret )					*
 *									*
 * Input parameters:							*
 *	*ifname		char		Input filename			*
 *	*ofname		char		Output filename			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = Error opening input file	*
 *					 -2 = Error during conversion	*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 2/99	Created	from cvg_c3i3h.			*
 * S. Jacobs/NCEP	 2/99	Save the input element record size	*
 * F. J. Yen/NCEP	 2/99	Allow input to be version 5.4.3.l also	*
 * S. Jacobs/NCEP	 3/99	Also convert from version 5.4.3.m	*
 * A. Hardy/GSC		 4/99	Also convert from version 5.4.3.n	*
 * F. J. Yen/NCEP	 5/99	Initialized grp_typ and grp_num.	*
 * A. Hardy/GSC		 1/01   changed ifptr and ofptr from int to FILE*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    VG_DBStruct	elmnt;
    FILE	*ifptr, *ofptr;
    int 	more, ier, flag, j;
    long	maxb;
    int         ifpos, ofpos, ier1, nxtinrec;
    int		loglev, iwrtflg, owrtflg;
    int		nbin, num_sym, grp_num;
    int		oset_xy[MAXPTS*2];
    float	combocode[MAXPTS];
    float	topsym[] = {-9999, 61, 61, 71, 61, 80, 79, 80,
			      51, 61, 61, 63, 63, 61, 65,
			      63, 65, 73, 71, 73, 73,
			      61, 80,  71,  75, 75, 61, 61};
    float	botsym[] = {-9999, 51, 80, 85, 71, 85, 66, -9,
			      56, -9, 56, 80, 65, 63, 80,
			      95, 95, 85, 73, 75, 105,
			      95, 95, 105, 105, 85, 66, 79};
    char        grp[4], grp_typ, chgrp_num;
    char 	inewfil[256];
    char	iversk[]="Version 5.4.3.k";
    char	iversl[]="Version 5.4.3.l";
    char	iversm[]="Version 5.4.3.m";
    char	iversn[]="Version 5.4.3.n";
    char	overs[]="Version 5.4.3.j";
    unsigned char	*buffer;
/*---------------------------------------------------------------------*/

    *iret = 0;

    strcpy(grp, "CVG");
    loglev = 0;

    if ( !ifname || !ofname )  {
	*iret = -1;
	return;
    }

    /*
     *  Open input file.
     */
    cfl_inqr(ifname, NULL, &maxb, inewfil, &ier);

    if ( ier == -1 )  {

	printf("File %s does not exist.\n", ifname );
	*iret = -1;
	return;
	
    }

    iwrtflg = 0;
    cvg_open(ifname, iwrtflg, &ifptr, &ier);

    if ( ( ier != 0 ) || ( ifptr == NULL ) )  {
	*iret = -1;
	er_lmsg( &loglev, grp, &ier, ifname, &ier1, strlen(grp),
                                strlen(ifname));

	return;
    }
    else  {
	/*
	 *  Check for proper version number in input file header
	 */
        ifpos = 0;
        cvg_rdhdr(ifname, ifptr, ifpos, maxb, &elmnt, &flag, &ier);
        cvg_rdele(&elmnt, ifpos, elmnt.hdr.recsz, ifptr, &ier);

	if ( elmnt.hdr.vg_type == FILEHEAD_ELM )  {
	
	    if ( strcmp(elmnt.elem.fhed.version, iversl) != 0 &&
		 strcmp(elmnt.elem.fhed.version, iversk) != 0 &&
		 strcmp(elmnt.elem.fhed.version, iversm) != 0 &&
		 strcmp(elmnt.elem.fhed.version, iversn) != 0 ) {

		printf( "Input file %s not version %s or %s or %s\n",
			ifname, iversm, iversl, iversk);

		printf( "Input file is version %s\n",
			elmnt.elem.fhed.version);

		*iret = -1;
    		cfl_clos( ifptr, &ier);
		return;

	    }

	}
	/*
	 *  Get the next group number for COMBOSYM which
	 *  is 99 for version j.
	*/
	grp_typ = (char) GRPTYP;
	cvq_nxtgnm ( ifname, grp_typ, &grp_num, &ier );
    }

    /*
     *  Open (create) output file.
     */
    cfl_inqr(ofname, NULL, &maxb, inewfil, &ier);

    if ( ier != -1 )  {

	printf("File %s already exists.\n", ofname );
	*iret = -1;
    	cfl_clos( ifptr, &ier);
	return;
	
    }
    else  {

        owrtflg = 1;
        cvg_open(ofname, owrtflg, &ofptr, &ier);

        if ( ( ier < 0 ) || ( ofptr == NULL ) )  {
	    *iret = -1;
	    er_lmsg( &loglev, grp, &ier, ofname, &ier1, strlen(grp),
                                strlen(ofname));
	    *iret = -1;
    	    cfl_clos( ifptr, &ier);
	    return;
        }

    }

    cfl_inqr(ifname, NULL, &maxb, inewfil, &ier);

    ifpos  = 0;
    ofpos  = 0;
    grp_typ = (char) GRPTYP;
    more = G_TRUE;

    while ( ( more ) && ( ifpos < maxb ) )  {

	/*
	 *  	Read the next VG header.
	 */
        cvg_rdhdr(ifname, ifptr, ifpos, maxb, &elmnt, &flag, &ier);

	nxtinrec = elmnt.hdr.recsz;

	/*
	 *  If the number of bytes is valid, continue.
	 */
        if ( (elmnt.hdr.recsz > 0) && (elmnt.hdr.recsz < 65536) )  {

	    if ( elmnt.hdr.delete == 0 )  {
            
    		/* 
     		 *  Check for Combo Symbols.
     		 */
	    	if ( elmnt.hdr.vg_type == CMBSY_ELM )  {

    		    /* 
     		     *  Read element
     		     */
		    cvg_rdele(&elmnt, ifpos, elmnt.hdr.recsz, ifptr, &ier);
		    num_sym = elmnt.elem.sym.info.numsym;

		    for ( j = 0; j < num_sym; j++) {

			/*
			 *  Save combo sym code
			 */
			combocode[j] = elmnt.elem.sym.data.code[j];
			/*
			 *  Set el for slash
			 */	
		        elmnt.hdr.vg_type  = SPSYM_ELM;
			elmnt.elem.sym.data.code[j] = 31;
		    }
		    grp_num++;
		    chgrp_num = (char) grp_num;
		    elmnt.hdr.grpnum = chgrp_num;
		    elmnt.hdr.grptyp = grp_typ;

		    /*
		     *  Write symbol record for slash:
		     */
		    cvgkj_wrt ( &elmnt, elmnt.elem.sym.data.offset_xy, num_sym,
			    ofptr, &ofpos, &ier);

		    if ( ier != 0 ) {
		        *iret = -2;
		        more = G_FALSE;
		    }

		    /*
		     *  Set element with top left symbol(s) and offset(s)
		     */
		    cvgkj_set ( num_sym, combocode, topsym, 1, &elmnt,
				oset_xy, &ier);

		    /*
		     *  Write record for top left symbols.
		     */
		    cvgkj_wrt ( &elmnt, oset_xy, num_sym, ofptr, &ofpos, &ier);

		    if ( ier != 0 ) {
			*iret = -2;
			more = G_FALSE;
		    }
		   	 
		    /*
		     *  Set element with bottom right symbol(s)
		     *  and offset(s)
		     */
		    cvgkj_set ( num_sym, combocode, botsym, 0, &elmnt,
				oset_xy, &ier);

		    /*
		     *  Write record for bottom right symbols;
		     */
		    cvgkj_wrt ( &elmnt, oset_xy, num_sym, ofptr, &ofpos, &ier);

		    if ( ier != 0 ) {
			*iret = -2;
		   	more = G_FALSE;
		    }

		}
		else {
                    /*
                     *  Just copy other elements directly via buffer copy
                     *  Don't do direct element copy because the data
                     *  for certain elements gets packed!
                     *  Set output file for writing and write record.
                     */
                    buffer = (unsigned char *)malloc(elmnt.hdr.recsz);
                    cfl_seek( ifptr, ifpos, 0, &ier);
                    cfl_read( ifptr, elmnt.hdr.recsz, buffer, &nbin, &ier );

                    cfl_seek( ofptr, ofpos, 0, &ier );
                    cfl_writ( ofptr, elmnt.hdr.recsz, (unsigned char *)buffer, &ier );
                    free (buffer);

                    ofpos += elmnt.hdr.recsz;

                    if ( ier != 0 )  {
                        *iret = -2;
                        more = G_FALSE;
                    }

		}
	    }
	}
	else {
	    more = G_FALSE;
	}
        ifpos += nxtinrec;
    }

    /*
     *  Output file was created with current version number...
     *  Change to version 5.4.3.j
     */
    ofpos = 0;
    cvg_rdhdr(ofname, ofptr, ofpos, maxb, &elmnt, &flag, &ier);
    cvg_rdele(&elmnt, ofpos, elmnt.hdr.recsz, ofptr, &ier);

    strcpy( elmnt.elem.fhed.version, overs );
    cfl_seek( ofptr, ofpos, 0, &ier );
    cfl_writ( ofptr, elmnt.hdr.recsz, (unsigned char *)&elmnt, &ier );

    cfl_clos( ifptr, &ier);

    cfl_clos( ofptr, &ier);

    return;

}
/*=====================================================================*/

void cvgkj_set ( int num_sym, float *cmbcode, float *cnvtbl, 
		 int topflg, VG_DBStruct *el, int *oset_xy, int *iret )
/************************************************************************
 * cvgkj_set	                                                        *
 *                                                                      *
 * This function determines and sets the symbol element with the top or	*
 * bottom symbol and calculates the offsets.				*
 *                                                                      *
 * cvgkj_set ( num_sym, cmbcode, cnvtbl, topflg, el, oset_xy, iret)	*
 *                                                                      *
 * Input parameters:                                                    *
 *	num_sym		int		Number of symbols		*
 *	*cmbcode	float		Array of combo symbol codes	*
 *	*cnvtbl		float		Conversion symbol code array	*
 *	topflg		int		Flag for top symbol		*
 *									*
 * Input/output parameters:						*
 *	*el		VG_DBStruct	Element to set symbol		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*oset_xy	int		Offset array			*
 *	*iret		int		Return Code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP        2/99   Created.                                *
 ***********************************************************************/
{
    int		j, svsym;
/*---------------------------------------------------------------------*/

    *iret = 0;

    for ( j = 0; j < num_sym; j++) {

	/*
	 *  Determine top or bottom symbol and set element
	 */
	if ( cmbcode[j] < 0 || cmbcode[j] > NUMCOMBO ) {
	    svsym = -9999;
	}
	else {
	    svsym = (int)cnvtbl[(int)cmbcode[j]];
	}

	if ( svsym != -9999 ) {
	    if ( svsym == -9 ) {
                el->hdr.vg_type = PWSYM_ELM;
   	        el->elem.sym.data.code[j] = 9;
	    }
	    else {
                el->hdr.vg_type = WXSYM_ELM;
    	        el->elem.sym.data.code[j] = (float)svsym;
	    }
	    /*
	     *  Calculate offsets
	     */
	    if (topflg == 1) {
		oset_xy[j] = el->elem.sym.data.offset_xy[j] - 
			2 * el->elem.sym.info.size;
		oset_xy[j+num_sym] = el->elem.sym.data.offset_xy[j+
			num_sym] + 1.25 * el->elem.sym.info.size;
	    }
	    else {
		oset_xy[j] = el->elem.sym.data.offset_xy[j] + 
			2 * el->elem.sym.info.size;
		oset_xy[j+num_sym] = el->elem.sym.data.offset_xy[j+
			num_sym] - 1.25 * el->elem.sym.info.size;
	    }
	}
	else {
	    /*
	     *  Invalid code, so overwrite slash with slash
	     */
            el->hdr.vg_type = SPSYM_ELM;
    	    el->elem.sym.data.code[j] = 31;
	    oset_xy[j] = el->elem.sym.data.offset_xy[j];
	    oset_xy[j+num_sym] = el->elem.sym.data.offset_xy[j+
			num_sym];
	}
    }
}
/*=====================================================================*/

void cvgkj_wrt ( VG_DBStruct *elmnt, int *oset_xy, int num_sym, 
				FILE *ofptr, int *ofposa, int *iret )
/************************************************************************
 * cvgkj_wrt	                                                        *
 *                                                                      *
 * This function writes a symbol record.				*
 *                                                                      *
 * cvgkj_wrt ( elmnt, oset_xy, num_sym, ofptr, ofposa, iret)		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*elmnt		VG_DBStruct	Element to write		*
 *	*oset_xy	int		Offset array			*
 *	num_sym		int		Number of symbols		*
 *	*ofptr		FILE		File pointer			*
 *                                                                      *
 * Input/Output parameters:						*
 *	*ofposa		int		File position			*
 *									*
 * Output parameters:                                                   *
 *	*iret		int             Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP        2/99   Created.                                *
 * S. Jacobs/NCEP	 2/99	Compute new output record size		*
 * A. Hardy/GSC		 1/01   changed ofptr from int to FILE		*
 ***********************************************************************/
{
    int		nbyte1, nbyte2, nbyte3;	
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Write symbol record:
     *    Write ( header + info + code ) in slot first.
     *    Then write ( latlon ) in slot.
     *    Then write ( offset ) in slot.
     */
    cfl_seek ( ofptr, *ofposa, 0, iret);

    nbyte1 = sizeof( VG_HdrStruct ) + sizeof( SymInfo ) +
       		sizeof( float ) * num_sym;
    nbyte2 = sizeof( float ) * num_sym * 2;
    nbyte3 = sizeof( int ) * num_sym * 2;

    elmnt->hdr.recsz = nbyte1 + nbyte2 + nbyte3;

    cfl_writ( ofptr, nbyte1, (unsigned char *)elmnt, iret);
    *ofposa = *ofposa + nbyte1;

    cfl_writ( ofptr, nbyte2, (unsigned char *)elmnt->elem.sym.data.latlon, iret);
    *ofposa = *ofposa + nbyte2;
		   
    cfl_writ( ofptr, nbyte3, (unsigned char *)oset_xy, iret);
    *ofposa = *ofposa + nbyte3;
}
