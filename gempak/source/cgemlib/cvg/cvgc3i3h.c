#include "cvgcmn.h"

void cvg_c3i3h ( char *ifname, char *ofname, int *iret )
/************************************************************************
 * cvg_c3i3h								*
 *									*
 * This function converts a VGF file from version 5.4.3.i to 5.4.3.h 	*
 * Necessary conversions:						*
 *	1) Front pip size (fpipsz) in 5.4.3i is multiplied by 100; 	*
 *	   convert back to unscaled integer.				*
 *	2) Regular text in 5.4.3i is stored under special text using	*
 *	   parameter sptxtyp=0; replace these as regular text.		*
 *									*
 * cvg_c3i3h ( ifname, ofname, iret )					*
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
 * D.W.Plummer/NCEP	 6/98	Created					*
 * D.W.Plummer/NCEP	 6/98	Bug fix for read/write of normal elems	*
 * A. Hardy/GSC          1/01   Changed ofptr and ifptr from int to FILE*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int 	more, ier, flag;
    long	maxb;
    char 	inewfil[256];
    VG_DBStruct	el, el_txt;
    FILE	*ifptr, *ofptr;
    char        grp[4];
    int         ifpos, ofpos, ier1; 
    int		loglev, iwrtflg, owrtflg;
    char	ivers[]="Version 5.4.3.i";
    char	overs[]="Version 5.4.3.h";
    unsigned char	*buffer;
    int		nbin;
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
        cvg_rdhdr(ifname, ifptr, ifpos, maxb, &el, &flag, &ier);
        cvg_rdele(&el, ifpos, el.hdr.recsz, ifptr, &ier);

	if ( el.hdr.vg_type == FILEHEAD_ELM )  {
	
	    if ( strcmp(el.elem.fhed.version, ivers) != 0 )  {
			
		printf("Input file %s not version %s\n", ifname, ivers);
		printf("Input file is version %s\n", el.elem.fhed.version);
		*iret = -1;
		return;

	    }

	}

    }

    /*
     *  Open (create) output file.
     */
    cfl_inqr(ofname, NULL, &maxb, inewfil, &ier);

    if ( ier != -1 )  {

	printf("File %s already exists.\n", ofname );
	*iret = -1;
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
	    return;
        }

    }

    cfl_inqr(ifname, NULL, &maxb, inewfil, &ier);

    ifpos  = 0;
    ofpos  = 0;
    more = G_TRUE;

    while ( ( more ) && ( ifpos < maxb ) )  {

	/*
	 *  	Read the next VG header.
	 */
        cvg_rdhdr(ifname, ifptr, ifpos, maxb, &el, &flag, &ier);

	/*
	 *  If the number of bytes is valid, continue.
	 */
        if ( (el.hdr.recsz > 0) && (el.hdr.recsz < 65536) )  {

	    if ( el.hdr.delete == 0 )  {
            
    		/* 
     		 *  Check for front types first...
     		 */
	    	if ( el.hdr.vg_type == FRONT_ELM )  {
            
    		    /* 
     		     *  Read element
     		     */
		    cvg_rdele(&el, ifpos, el.hdr.recsz, ifptr, &ier);

		    if ( el.elem.frt.info.fpipsz > 10 )  {

		        el.elem.frt.info.fpipsz = 
			    G_NINT ( el.elem.frt.info.fpipsz / 100.0 );

		        if ( el.elem.frt.info.fpipsz == 0 )
			    el.elem.frt.info.fpipsz = 1;

		        /*
		         * Set output file for writing and write revised record.
		         */
		        cfl_seek( ofptr, ofpos, 0, &ier );
		        cfl_writ( ofptr, el.hdr.recsz, (unsigned char *)&el, &ier );

		        ofpos += el.hdr.recsz;

		        if ( ier != 0 )  {
                            *iret = -2;
		            more = G_FALSE;
		        }

		    }

		}
    		/* 
     		 *  Now check for special text type.
     		 */
	    	else if ( el.hdr.vg_type == SPTX_ELM )  {

    		    /* 
     		     *  Read element
     		     */
		    cvg_rdele(&el, ifpos, el.hdr.recsz, ifptr, &ier);

	    	    if ( el.elem.spt.info.sptxtyp == 0 )  {

		        el_txt.hdr = el.hdr;
		        el_txt.hdr.vg_type  = TEXT_ELM;

                        el_txt.elem.txt.info.rotn   = el.elem.spt.info.rotn;
                        el_txt.elem.txt.info.sztext = el.elem.spt.info.sztext;
                        el_txt.elem.txt.info.itxfn  = el.elem.spt.info.itxfn;
                        el_txt.elem.txt.info.ithw   = el.elem.spt.info.ithw;
                        el_txt.elem.txt.info.iwidth = el.elem.spt.info.iwidth;
                        el_txt.elem.txt.info.ialign = el.elem.spt.info.ialign;
                        el_txt.elem.txt.info.lat    = el.elem.spt.info.lat;
                        el_txt.elem.txt.info.lon    = el.elem.spt.info.lon;
                        el_txt.elem.txt.info.offset_x = 
				el.elem.spt.info.offset_x;
                        el_txt.elem.txt.info.offset_y = 
				el.elem.spt.info.offset_y;

		        strcpy( el_txt.elem.txt.text, el.elem.spt.text);

    		        el_txt.hdr.recsz = ( sizeof(VG_HdrStruct) + 
					     sizeof(TextInfo) +
                		             strlen(el_txt.elem.txt.text) + 1 );

		        /*
		         * Set output file for writing and write record.
		         */
		        cfl_seek( ofptr, ofpos, 0, &ier );
		        cfl_writ( ofptr, el_txt.hdr.recsz, (unsigned char *)&el_txt, &ier );

		    }
		    else  {

		        cfl_seek( ofptr, ofpos, 0, &ier );
		        cfl_writ( ofptr, el.hdr.recsz, (unsigned char *)&el, &ier );

		    }

		    ofpos += el.hdr.recsz;

		    if ( ier != 0 )  {
                        *iret = -2;
		        more = G_FALSE;
		    }

		}
                else  {

                    /*
                     *  Just copy other elements directly via buffer copy
                     *  Don't do direct element copy because the data
		     *  for certain elements gets packed!
		     *  Set output file for writing and write record.
		     */
                    buffer = (unsigned char *)malloc(el.hdr.recsz);
                    cfl_seek( ifptr, ifpos, 0, &ier);
                    cfl_read( ifptr, el.hdr.recsz, buffer, &nbin, &ier );

		    cfl_seek( ofptr, ofpos, 0, &ier );
		    cfl_writ( ofptr, el.hdr.recsz, (unsigned char *)buffer, &ier );
		    free (buffer);

		    ofpos += el.hdr.recsz;

		    if ( ier != 0 )  {
                        *iret = -2;
		        more = G_FALSE;
		    }

		}

	    }

	}
	else
	{
	    more = G_FALSE;
        }

	ifpos += el.hdr.recsz;

    }

    /*
     *  Output file was created with current version number...
     *  Change to version 5.4.3.h
     */
    ofpos = 0;
    cvg_rdhdr(ofname, ofptr, ofpos, maxb, &el, &flag, &ier);
    cvg_rdele(&el, ofpos, el.hdr.recsz, ofptr, &ier);

    strcpy( el.elem.fhed.version, overs );
    cfl_seek( ofptr, ofpos, 0, &ier );
    cfl_writ( ofptr, el.hdr.recsz, (unsigned char *)&el, &ier );

    cfl_clos( ifptr, &ier);

    cfl_clos( ofptr, &ier);

    return;

}
