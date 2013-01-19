#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

/*
 *  Private function to convert 4-digit text to 2-digit
 */
static void _cdspptxt ( VG_DBStruct *el, int indx, char *ntxt, int *iret );

void cds_text ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_text								*
 *									*
 * This function displays text to the output device.			*
 *									*
 * cds_text ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	 3/97	Initial coding (copied from     	*
 *						   cvg_lndsp)		*
 * E. Safford/GSC	 4/97  	Removed unused variables 		*
 * D. Keiser/GSC	 5/97	Renamed from cvg_txdsp			*
 * E. Safford/GSC	 6/97	Added handling for special text 	*
 * S. Jacobs/NCEP	 7/97	Replaced GSTXSY with GSCOLR		*
 * E. Safford/GSC	 7/97	Modified for additional special 	*
 *                                       text params                   	*
 * D.W.Plummer/NCEP	 7/97	Added cdsColor checks			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * S. Jacobs/NCEP	 9/97	Changed call to gstext			*
 * S. Jacobs/NCEP	12/97	Removed call to gtextc			*
 * S. Jacobs/NCEP	12/97	Fixed a typo: ptxt -> pspt		*
 * C. Lin/EAI	        03/98	Added scaling factors			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Law/GSC		05/98	Added catch for sptxtyp = 0		*
 * S. Jacobs/NCEP	 7/98	Added North-rel rotation to special text*
 * S. Jacobs/NCEP	 2/99	Changed default border type to 111	*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC            03/01   Fixed GPMAP post-processing text size	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * J. Wu/SAIC           05/04   convert 4-digit text label to 2-digit	*
 * T. Piper/SAIC	06/05	Apply 4-digit to 2-digit for all types	*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 * T. Piper/SAIC	12/05	Fixed error, ptxt used in pspt section	*
 * S. Jacobs/NCEP	 3/06	Fixed error, txt used in spt section	*
 ***********************************************************************/
{
    int		ibrdr, irrotn, iwdth, kialign, ier;
    int		kitxfn, kithw, kturbsym;
    int         icolrx, itxfnx, itxhwx, itxwidx, ibrdrx, irrotnx;
    int         ijustx;
    float	rotn, txtsz;
    float       sztextx;
    TextType	*ptxt;
    SptxType	*pspt;
    char	newtxt[MAX_TEXT];
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Save plot and text attributes.
     */
    gqcolr ( &icolrx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );

    ptxt = &(el->elem.txt);
    pspt = &(el->elem.spt);


    if ((el->hdr.vg_type == TEXT_ELM) || (el->hdr.vg_type == TEXTC_ELM)) {
        /*
         * Set the color for the feature.
         */
	if ( cdsColor == 0 ) {
	    if ( cdsUattr[indx].maj_col == 0 ) {
		gscolr( &(el->hdr.maj_col), &ier);
	    }
	    else {
		gscolr (&cdsUattr[indx].maj_col, &ier);
	    }
	}
	else {
	    gscolr( &cdsColor, &ier);
	}

        /*
         * Set text attributes.
         */

	ibrdr  = 111;
	irrotn = ( (int)ptxt->info.rotn / 1000 ) + 1;
	rotn = (float) fmod ( (double)ptxt->info.rotn, 1000. );

        txtsz  =  ( cdsUattr[indx].info.txt->info.sztext > 0.0F ) ?
		  (float)cdsUattr[indx].info.txt->info.sztext * cdsTxtSiz :
		    (float)ptxt->info.sztext * cdsTxtSiz;
        iwdth  = (int)( ( ( cdsUattr[indx].info.txt->info.iwidth == 0 ) ?
		    (float)ptxt->info.iwidth :
		    (float)cdsUattr[indx].info.txt->info.iwidth )  * cdsTxtWdth);
	kitxfn = ( cdsUattr[indx].info.txt->info.itxfn == 0 ) ?
		    ptxt->info.itxfn : cdsUattr[indx].info.txt->info.itxfn;
	kialign = ( cdsUattr[indx].info.txt->info.ialign == 0 ) ?
		    ptxt->info.ialign : cdsUattr[indx].info.txt->info.ialign;
        gstext (&kitxfn, &(ptxt->info.ithw), 
	        &txtsz, &iwdth, &ibrdr, &irrotn, 
		&kialign, &ier);	

        /*
         * Display the text with application layer command.
         */

	gtext ( sys_M, 
	       &(ptxt->info.lat),
	       &(ptxt->info.lon),
	       ptxt->text,
	       &rotn,
	       &(ptxt->info.offset_x),
	       &(ptxt->info.offset_y),
	       &ier, 
	       strlen(sys_M), strlen(ptxt->text));
    }
    else {				/* Special Text */

	ibrdr  = 111;
	irrotn = ( (int)pspt->info.rotn / 1000 ) + 1;
	rotn   = (float) fmod ( (double)pspt->info.rotn, 1000. );

	kialign = ( cdsUattr[indx].info.spt->text.info.ialign == 0 ) ?
		pspt->info.ialign : cdsUattr[indx].info.spt->text.info.ialign;

	if (pspt->info.sptxtyp == 0) kialign += 2;

        txtsz  =  ( cdsUattr[indx].info.spt->text.info.sztext > 0.0F ) ?
		    (float)cdsUattr[indx].info.spt->text.info.sztext * cdsTxtSiz :
		    (float)pspt->info.sztext * cdsTxtSiz;

        iwdth  = ( cdsUattr[indx].info.spt->text.info.iwidth == 0 ) ?
		    (int)((float)pspt->info.iwidth * cdsSptWdth) :
		    (int)((float)cdsUattr[indx].info.spt->text.info.iwidth * cdsSptWdth);

	kitxfn = ( cdsUattr[indx].info.spt->text.info.itxfn == 0 ) ?
		    pspt->info.itxfn : cdsUattr[indx].info.spt->text.info.itxfn;

	kithw  = ( cdsUattr[indx].info.spt->text.info.ithw == 0 ) ?
		    pspt->info.ithw : cdsUattr[indx].info.spt->text.info.ithw;

        gstext (&kitxfn, &kithw, &txtsz, &iwdth, &ibrdr, &irrotn, 
		&kialign, &ier);	

	if ( cdsColor == 0 ) {
	    if ( cdsUattr[indx].maj_col == 0 ) {
		gscolr( &(pspt->info.txtcol), &ier);
	    }
	    else {
		gscolr (&cdsUattr[indx].maj_col, &ier);
	    }
	}
	else {
	    gscolr( &cdsColor, &ier);
	}
	
/*
*  Post-processing - convert 4-digit text to 2-digit.
*/
	_cdspptxt ( el, indx, newtxt, &ier );
	    	    	    
	if ( pspt->info.sptxtyp == 0 ) {

	    gtext ( sys_M, 
		   &(pspt->info.lat),
		   &(pspt->info.lon),
		   newtxt,
		   &rotn,
		   &(pspt->info.offset_x),
		   &(pspt->info.offset_y),
		   &ier, 
		   strlen(sys_M), strlen(newtxt));
	    	    
	}
	else {
	    kturbsym = ( cdsUattr[indx].info.spt->text.info.turbsym == 0 ) ?
		   pspt->info.turbsym :
		     cdsUattr[indx].info.spt->text.info.turbsym;

	    gtxsy ( sys_M, 
		   &(pspt->info.sptxtyp), 
		   &kturbsym,
		   &kialign,
		   &(pspt->info.offset_x),
		   &(pspt->info.offset_y),
		   &rotn,
		   &(pspt->info.lat),
		   &(pspt->info.lon),
		   newtxt, &ier,
		   strlen(sys_M), strlen(newtxt)); 
	}
    }

    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gscolr ( &icolrx, &ier );
}

/*=====================================================================*/

static void _cdspptxt ( VG_DBStruct *el, int indx, char *ntxt, int *iret )
/************************************************************************
 * _cdspptxt								*
 *									*
 * This function converts 4-digit isobar/high/low labels to 2-digit for	*
 * display.  The content of the element remains unchanged.		*
 *									*
 * static _cdspptxt ( el, indx, newtxt, iret )				*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table	*
 *									*
 * Output parameters:							*
 *	*newtxt		char		The processed text for display	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC           05/04   initial coding 				*
 ***********************************************************************/
{
    int         pres, txtlen, ii, sd, ier;
    char	cdsppid[32];
    Boolean	valid_pres = False;
/*---------------------------------------------------------------------*/
    
    *iret = 0;    
    strcpy ( ntxt, el->elem.spt.text );

	    
    /*
     *  convert 4-digit isobar text to 2-digit if required.
     */
    strcpy ( cdsppid, cdsUattr[indx].info.spt->ppid );
    cst_lcuc ( cdsppid, cdsppid, &ier );
	    	    
    if ( strcmp( cdsppid, "PMSL" ) == 0 ) { 
        txtlen = strlen ( ntxt );	        		        
			
	pres = atoi ( el->elem.spt.text ); 		    
	if ( 950 < pres && pres <= 1050 ) {
	    valid_pres = True;
		        
	    /*
	     *  Find the acceptable end position of digits.
	     */	    
	    for ( ii = 0; ii < txtlen; ii++ ) {
		if ( isdigit( ntxt[ii] ) ) {
		    sd = ii + 3;
		    if ( pres >= 1000 )  sd++;
		    break;
		}
	    }

					    
            /*
	     *  A non-blank character after the acceptable end
	     *  position indicates the text is not an isobar,
	     *  high, or low label.
	     */	    
	    for ( ii = sd; ii < txtlen; ii++ ) {
		if ( ntxt[ii] != ' ' ) {
		    valid_pres = False;
		    break;
		}
	    }
	}

	/*
	 *  Display only the last 2 digits in the text.
	 */	    
	if ( valid_pres ) {
	    pres = pres % 100;
	    if ( pres < 10 ) {
		sprintf ( ntxt, "0%i", pres );    
	    }
	    else {
		sprintf ( ntxt, "%i", pres );    
	    }            
	}
    }		
}
