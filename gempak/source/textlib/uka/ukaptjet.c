#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  IWIND          1	/* need height info */
#define  IHASH          2	/* need wind info  */
#define  JETS_LINE 	6	/* jets line type */

void uka_ptjet ( FILE *ifpout, char *fname, FILE *fptr, long size,
                char gptyp, char *gtstr, int maxgnm, char *chlvl, int *iret )
/************************************************************************
 * uka_ptjet								*
 *                                                                      *
 * This function scans a VG file for elements of group type "JETS" and  *
 * prints out all that match to an ASCII file.				*
 *                                                                      *
 * uka_ptjet ( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, chlvl, 	*
 * 	 	iret )  						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input VG file name		*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"JETS" group type number	*
 *      *gtstr		char		"JETS" group type name		*
 *	maxgnm		int		Maximum number of group numbers *
 *	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					= 0 normal			*
 *					= 1  No grouped elements found	*
 *					= 16 No jet element found	*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        12/01   Created					*
 * A. Hardy/SAIC         2/02   Fixed freeing memory error 		*
 * A. Hardy/SAIC         2/02   Initialize flight levels to missing	*
 * A. Hardy/SAIC         4/02   Checked flipped line; set hash flt lvel *
 *                              missing. & core pts. speed to SIGRLMS	*
 * A. Hardy/NCEP         6/02   Added sig_grpchk			*
 * A. Hardy/NCEP         7/02   Fixed memory leak 			*
 * A. Hardy/NCEP         8/02   Add old gpnum ck for error processing   *
 * A. Hardy/NCEP         1/03   Reworked dropping elements onto jet ln  *
 * A. Hardy/NCEP         1/03   Fixed dropping elements logic		*
 * R. Tian/SAIC		 5/03	Major change, redesigned the algorithm	*
 * M. Li/SAIC		 2/04	Process jet element			*	
 * M. Li/SAIC		 4/04	Added flight level deltas		*
 * M. Li/SAIC		 5/04	Copied from sig_jets			*
 * M. Li/SAIC		 6/04	Improved error handling			*
 * M. Li/SAIC		 9/04	Added chlvl				*
 * M. Li/SAIC		10/05	Checked for new format of Jet info	*
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag, ipos, lenf;
    int         members[MAXOFF], level, order[MAXPTS]; 
    int		ii, jj, kk;
    int         *dupgrp, ingrp, numjet, numjet0, ier, ier1;
    char	vgclss, vgnum, sggrp[12], strln[256];
    Boolean	process;
    int 	mode, istat, iunit, itype;
    char 	device[8], filnam[20];
    float 	xsize, ysize;
    float       jlat[MAXPTS], jlon[MAXPTS], tlvl, tabv, tblw;
    float	wlat[MAXPTS], wlon[MAXPTS], wspd[MAXPTS], wlvl[MAXPTS];
    float	flat[MAXPTS], flon[MAXPTS], flvl[MAXPTS];
    float       olat[MAXPTS], olon[MAXPTS], ospd[MAXPTS], olvl[MAXPTS];
    float	wlvla[MAXPTS], wlvlb[MAXPTS], olvla[MAXPTS], olvlb[MAXPTS];
    int    	wtyp[MAXPTS];
    char        chrlvl[10], chrabv[10], chrblw[10];
    int		njp, nwp, nfp, nop, nhp, iergp, ierel;
    jets_t	*jet, *new;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    iergp = 0;
    ierel = 0;

    istoff  = 0;
    numjet  = 0;
    level   = 0;
    process = False;
    jet = (jets_t *)NULL;
    strcpy(sggrp, "UKA");
    dupgrp = (int *)calloc((size_t)(maxgnm + 1), sizeof(int));

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_LINES;
    vgnum  = SPLN_ELM;
    sbtyp  = JETS_LINE;

    /*
     * Init GEMPLT and set DEVICE.
     */

    mode = 1;
    ginitp ( &mode, &istat, &ier );

    iunit = itype = 1;
    xsize = ysize = 1.0F;
    strcpy ( device, "GN" );
    strcpy ( filnam, "CGR_INSERT" );
    gsdeva ( device, &iunit, filnam, &itype, &xsize, &ysize, &ier,
             strlen(device), strlen(filnam) );

    if ( maxgnm > 0 ) {
      while ( 1 ) {
	
   	/*
    	 * Read an element header to find the group type number.
    	 */

        cvg_rdgtn ( fname, fptr, &size, istoff, gptyp, vgclss, vgnum, 
                    sbtyp, &ieloff, &gpnum, &ier );
        istoff = ieloff;

	/*
	 * dupgrp[gpnum] != 0 means that more than one records with 
	 * the same group number were found.
	 */

	if ( dupgrp[gpnum] != 0 ) {
	    continue;
	}

	cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF, 
	    	      members, &ingrp, &ier);
	uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	             &process, &ier); 

	if ( ier != 0 && gpnum > 0 ) {
	    sprintf (strln,"JETS group number %d", gpnum);
            er_lmsg ( &level, sggrp, &ier, strln, &ier1,
	              strlen(sggrp), strlen(strln) );
	}  

	if ( process ) {

	    nwp = nfp = 0;

	    /*
	     * Begin looping through the individual elements with the 
	     * same group type and group number.
	     */
	    for ( ii = 0; ii < ingrp; ii++ ) {

	        cvg_rdhdr ( fname, fptr, members[ii], (int) size, &el, 
		            &flag, &ier );
                cvg_rdele ( &el, members[ii], el.hdr.recsz, fptr, &ier );

                /*
                 * Find the jet line.
                 */

	        if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == SPLN_ELM )    &&
	             ( el.elem.spl.info.spltyp == JETS_LINE ) ) {

		    njp = el.elem.spl.info.numpts;
	    
                    /*
		     * Check to see if line is flipped.
		     */

		    if ( el.elem.spl.info.spldir != 1 ) {
		        for ( jj = (njp-1), kk = 0; jj >= 0; jj--, kk++ ) {
		            jlat[jj] = el.elem.spl.latlon[kk];
		            jlon[jj] = el.elem.spl.latlon[kk + njp];
		        }
		    }
		    else {
		        for ( jj = 0; jj < njp; jj++ ) {
		            jlat[jj] = el.elem.spl.latlon[jj];
		            jlon[jj] = el.elem.spl.latlon[jj + njp];
			}
		    }

	        }

	        /*
	         * Find all wind barb and hash mark.
		 */

                else if ( ( el.hdr.vg_class == CLASS_WINDS ) && 
	                  ( ( el.hdr.vg_type  == BARB_ELM )  ||
	                    ( el.hdr.vg_type  == HASH_ELM ) ) ) {

	            if ( el.hdr.vg_type  == BARB_ELM )  {
	                wspd[nwp] = pr_knms ( &el.elem.wnd.data.spddir[0] );
	                wlvl[nwp] = SIGRLMS;
			wlvla[nwp] = SIGRLMS;
			wlvlb[nwp] = SIGRLMS;
		        wtyp[nwp] = IWIND;
		    }
	            else if ( el.hdr.vg_type  == HASH_ELM )  {
	                wspd[nwp] = SIGRLMS;
	                wlvl[nwp] = SIGRLMS;
			wlvla[nwp] = SIGRLMS;
                        wlvlb[nwp] = SIGRLMS;
		        wtyp[nwp] = IHASH;
		    }
		    wlat[nwp] = el.elem.wnd.data.latlon[0];
		    wlon[nwp] = el.elem.wnd.data.latlon[el.elem.wnd.info.numwnd];

		    nwp++;

	        }

		/*
		 * Find all flight level.
		 */

		else if ( ( el.hdr.vg_class == CLASS_TEXT ) &&
		          ( el.hdr.vg_type == SPTX_ELM ) ) {

		    strcpy( chrlvl, el.elem.spt.text + 2 );
                    cst_crnm ( chrlvl, &flvl[nfp], &ier );
                    flvl[nfp] *= 100.0F;
                    flat[nfp] = el.elem.spt.info.lat;
                    flon[nfp] = el.elem.spt.info.lon;

                    nfp++;

		}

	    }

	    /*
	     * Match flight level to wind barb.
	     */
	    
	    for ( ii = 0; ii < nwp; ii++ ) {
		if ( wtyp[ii] == IWIND ) {
		    clo_closest ( flat, flon, nfp, wlat[ii], wlon[ii], 1, 
		                  order, &ier );
		    wlvl[ii] = pr_hgfm ( &(flvl[order[0]]) );
		    wlvla[ii] = SIGRLMS;
		    wlvlb[ii] = SIGRLMS;
		}
	    }

	    /*
	     * Insert wind barb and hash mark points into the jet line,
	     * and match the corresponding speed and levle.
	     */

	    uka_jtin ( njp, jlat, jlon, nwp, wlat, wlon, wspd, 
			   wlvl, wlvla, wlvlb, wtyp, &nop, olat, olon, 
			   ospd, olvl, olvla, olvlb, &ier );

	    if ( ier == 0 ) {

		/*
                 * Set up the link list of jets.
                 */

                new = ( jets_t *) malloc (sizeof ( jets_t ) );
                new->npt = nop;
                for ( ii = 0; ii < nop; ii++ ) {
                    new->lat[ii] = olat[ii];
                    new->lon[ii] = olon[ii];
                    new->level[ii] = olvl[ii];
		    new->levabv[ii] = olvla[ii];
		    new->levblw[ii] = olvlb[ii];
                    new->speed[ii] = ospd[ii];
                }
                new->next = jet;
                jet = new;

                numjet++;
	    }
	    else {
	        sprintf ( strln, "JETS group number %d", gpnum);
                er_lmsg ( &level, sggrp, &ier, strln, &ier1,
                          strlen(sggrp), strlen(strln) );
	    }
	} 

	dupgrp[gpnum] = 1;

	if ( ieloff == IMISSD || ieloff >= (int)size ) {

            if ( numjet == 0 ) {
                *iret = iergp = 1;
                er_lmsg ( &level, sggrp, &iergp, gtstr, &ier1,
                          strlen(sggrp), strlen(gtstr) );
            }

	    break;
	}

      }
    }
    else {
	*iret = iergp = 1;
        er_lmsg ( &level, sggrp, &iergp, gtstr, &ier1,
                          strlen(sggrp), strlen(gtstr) );
    }

    /*
     * Look for and process the jet elements.
     */

    numjet0 = numjet;
    rewind ( fptr );
    vgclss = CLASS_MET;
    vgnum  = JET_ELM;
    sbtyp  = JETS_LINE;
    istoff  = 0;

    cvg_rdgtn ( fname, fptr, &size, istoff, 0, vgclss, vgnum,
                    sbtyp, &ieloff, &gpnum, &ier );
    istoff = ieloff;

    if ( ( ieloff != IMISSD ) && ( gpnum == 0 ) && ( ier == 0 ) ) {

	cvg_srchgrp ( fname, fptr, &size, 0, 0, MAXOFF,
                      members, &ingrp, &ier);

       if ( ingrp > 0 && ier == 0 ) {
	  for ( ii = 0; ii < ingrp; ii++ ) {

	    cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el,
                                &flag, &ier );
            cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	    /*
	     * Find the jet element.
	     */

	    if ( ( el.hdr.vg_class == CLASS_MET ) &&
                 ( el.hdr.vg_type == JET_ELM )    &&
                 ( el.elem.spt.info.sptxtyp == JETS_LINE ) ) {

		/*
	  	 * Find the number of core points and their location.
		 */
		njp = el.elem.jet.line.spl.info.numpts;

                for ( jj = 0; jj < njp; jj++ ) {
		     jlat[jj] = el.elem.jet.line.spl.latlon[jj];
                     jlon[jj] = el.elem.jet.line.spl.latlon[jj + njp];
                }

	 	/*
		 * Find number of barbs, locations, speeds and flight level.
		 */
		nwp = el.elem.jet.nbarb;

		for ( jj = 0; jj < nwp; jj++ ) {
                     wlat[jj] = el.elem.jet.barb[jj].wnd.data.latlon[0];
                     wlon[jj] = el.elem.jet.barb[jj].wnd.data.latlon[1];
		     wspd[jj] = pr_knms ( &(el.elem.jet.barb[jj].wnd.data.spddir[0]) );
		     cst_ncpy( chrlvl, el.elem.jet.barb[jj].spt.text + 2, 3, &ier );
                     cst_crnm ( chrlvl, &tlvl, &ier );
                     tlvl *= 100.0F;
		     wlvl[jj] = pr_hgfm ( &tlvl );
		
		     lenf = strlen ( el.elem.jet.barb[jj].spt.text );
		     if ( lenf  > 5 ) { 
		     	ipos = 0;
		     	tabv = 0.0F;
		     	tblw = 0.0F;
			strcpy ( chrlvl, el.elem.jet.barb[jj].spt.text + 6 );
		     	cst_srch ( 0, (int)strlen(chrlvl), "/", chrlvl, &ipos, &ier);
		     	cst_ncpy ( chrabv, chrlvl, ipos, &ier);
		     	strcpy ( chrblw, chrlvl + ipos + 1);
		     	cst_crnm ( chrabv, &tabv, &ier );
		     	cst_crnm ( chrblw, &tblw, &ier );
		     	tabv = 100.0F * pr_hgfm ( &tabv);
		     	tblw = 100.0F * pr_hgfm ( &tblw);

		       /*
                        * Check for old or new format.
			*/
			if ( tblw < wlvl[jj] ) {
		     	    wlvla[jj] = wlvl[jj] + tabv;
		     	    wlvlb[jj] = wlvl[jj] + tblw;
			}
			else {
			    wlvlb[jj] = tabv;
                            wlvla[jj] = tblw;
			}
		     }
		     else {
			wlvla[jj] = SIGRLMS; 
                        wlvlb[jj] = SIGRLMS; 
		     }

		     wtyp[jj] = IWIND;
                }
		/*
		 * Find number of harshes and their locations.
		 */
		nhp = el.elem.jet.nhash;

		if ( nhp > 0 ) {
		    for ( jj = nwp; jj < nhp+nwp; jj++ ) {
		    	wlat[jj] = el.elem.jet.hash[jj-nwp].wnd.data.latlon[0];
                    	wlon[jj] = el.elem.jet.hash[jj-nwp].wnd.data.latlon[1];
			wspd[jj] = SIGRLMS;
			wlvl[jj] = SIGRLMS;
			wlvla[jj] = SIGRLMS;
			wlvlb[jj] = SIGRLMS;
			wtyp[jj] = IHASH;
		    }
		
		    nwp += nhp;
		}

		/*
                 * Insert wind barb and hash mark points into the jet line,
                 * and match the corresponding speed and level.
                 */
		uka_jtin ( njp, jlat, jlon, nwp, wlat, wlon, wspd, 
			      wlvl, wlvla, wlvlb, wtyp, &nop, olat, olon, 
			      ospd, olvl, olvla, olvlb, &ier );

		if ( ier == 0 ) {

                    /*
                     * Set up the link list of jets.
                     */

                    new = ( jets_t *) malloc (sizeof ( jets_t ) );
                    new->npt = nop;
                    for ( jj = 0; jj < nop; jj++ ) {
                    	new->lat[jj] = olat[jj];
                    	new->lon[jj] = olon[jj];
                    	new->level[jj] = olvl[jj];
			new->levabv[jj] = olvla[jj];
                    	new->levblw[jj] = olvlb[jj];	
                    	new->speed[jj] = ospd[jj];
                    }
                    new->next = jet;
                    jet = new;

                    numjet++;
                }
                else {
                    sprintf ( strln, "JETS elements");
                    er_lmsg ( &level, sggrp, &ier, strln, &ier1,
                          	strlen(sggrp), strlen(strln) );
                }
	    }
	  }
	}

	/*
	 * Check if any new jet elements were found.
	 */
	if ( numjet0 == numjet ) {
	    *iret = ierel = 16;
	    sprintf ( strln, "JET");
            er_lmsg ( &level, sggrp, &ierel, strln, &ier1,
                              strlen(sggrp), strlen(strln) );
	}

    }
    else {
	*iret = ierel = 16;
        sprintf ( strln, "JET");
        er_lmsg ( &level, sggrp, &ierel, strln, &ier1,
                              strlen(sggrp), strlen(strln) );
    }

    if ( iergp == 0 || ierel == 0 ) *iret = 0;

    /* 
     * Write out JETS information in BUFR ASCII format.
     */

    if ( numjet > 0 ) {
        cas_wrjets ( ifpout, jet, numjet, chlvl, &ier );
    }

    /*
     * Free memory from the linked list.
     */

    free ( dupgrp );
    while ( jet ) {
	new = jet;
	jet = jet->next;
        free ( new );
    }

}
