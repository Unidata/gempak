#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"

#define MIN_DIFF	0.01


void cvg_openj ( char *filnam, int wrtflg, FILE **fptr, int *iret );
void cvg_rdjrecnoc ( char *fname, FILE *fptr, int fpos, VG_DBStruct *el, int *iret );
char* getVgType(int vg_type);
char* trimwhitespace(char *str);
Bool checkSztext (float sztext1, float sztext2);

Bool compareHdr(VG_DBStruct el1, VG_DBStruct el2, int pnts);
Bool compareFrt(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareLin(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareSpl(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareWbx(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareSym(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareVec(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareSpt(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareCir(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareTrk(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareSig(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareCcf(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareLst(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareAsh(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareVol(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareJet(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareTca(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareGfa(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareCloudSpt(VG_DBStruct el1, VG_DBStruct el2, int ne1);
Bool compareCloudSpl(VG_DBStruct el1, VG_DBStruct el2, int ne1);

char		buffer[10000]="";

struct list_el {
   int rec;
   VG_DBStruct el;
   struct list_el * next;
};
typedef struct list_el item1, item2;

int totEleNum; /* Total elements number */

int compareVgf(char *vgfn, char *asfn)

/************************************************************************
 * CompareVgf								*
 *									*
 * This program compares two vgf files.                                 *
 * It first compares the numbers of the els in the two files.           *
 * then check if the els in the first file can find a match in the      *
 * second file.					                        *
 *									*
 *									*
 * Log: 								*
 * 									*
 * Q. Zhou/Chug     06/10   Original		                        *
 * Q. Zhou/Chug     12/10   Added Cloud, Turb, Contour, Outlook,        *
 *                          SymLabel, volcano, ash and ccf              *
 *                          Removed compare that CAVE doesn't have now  *
 * Q. Zhou/Chug     02/11   Changed MIN_DIFF to 0.01.			*
 * 			    Don't compare grptyp & grpnum, Cave has no. *
 *                          Don't compare min_col
 ***********************************************************************/
{
int		ne1, ne2, ne, more1, more2, ier;
int		wrtflg, curpos1, curpos2;
VG_DBStruct	el1, el2 ;  
char		infile1[128], ifname1[128], infile2[128], ifname2[128];
long		ifilesize1, ifilesize2;
FILE		*ifptr1, *ifptr2;


item1 * curr1, * head1, *head0;
item2 * curr2, * head2;
head0 = NULL;
head1 = NULL;
head2 = NULL;

/*---------------------------------------------------------------------*/
    
    /*printf ( "Enter the first VGF file name :\n" );
    scanf ( " %s", vgfn );
    printf ( "Enter the second VGF file name :\n" );
    scanf ( " %s", asfn );*/
   
    wrtflg = 0;
    cvg_openj ( vgfn, wrtflg, &(ifptr1), &ier );
    if ( ier != 0 )  {
        printf("Error opening the first VGF file %s\n", infile1 );
        exit (0);
    }

    cvg_openj ( asfn, wrtflg, &(ifptr2), &ier );
    if ( ier != 0 )  {
        printf("Error opening the second VGF file %s\n", infile2 );
        exit (0);
    }

    cfl_inqr ( vgfn, NULL, &ifilesize1, ifname1, &ier );
    cfl_inqr ( asfn, NULL, &ifilesize2, ifname2, &ier );

    ne1 = 0; /* element count in the first file*/
    ne2 = 0; /* element count in the second file*/
    ne = 0; /* loop count*/
    more1 = G_TRUE;
    more2 = G_TRUE;
    curpos1 = 424; /*header file size is 424.  SKIP header*/
    curpos2 = 424;

    ifptr1 = (FILE *) cfl_ropn(ifname1, "", &ier);
    ifptr2 = (FILE *) cfl_ropn(ifname2, "", &ier);

   /*add to lists*/
//    head0 = head1 = curr1;
    while ( ne1 < MAX_EDITABLE_ELEMS && more1 == G_TRUE )  {
	
	cvg_rdjrecnoc( ifname1, ifptr1, curpos1, &el1, &ier );
        if ( ier < 0 )  {
            more1 = G_FALSE;	   
        }
	else {
	curpos1 += el1.hdr.recsz;

	ne1++;
	curr1 = (item1 *)malloc(sizeof(item1));
	curr1->rec = ne1;
      	curr1->el = el1;
      	curr1->next  = head1;
      	head1 = curr1;
/*head1->next = curr1;
head1 = curr1;
head1->next = NULL;*/
	}
    }
	
    while ( ne2 < MAX_EDITABLE_ELEMS && more2 == G_TRUE )  {
	cvg_rdjrecnoc( ifname2, ifptr2, curpos2, &el2, &ier );
	if ( ier < 0 )  {
            more2 = G_FALSE;	    
        } 
	else {
	curpos2 += el2.hdr.recsz;

	ne2++;
	curr2 = (item2 *)malloc(sizeof(item2));
      	curr2->rec =ne2;
	curr2->el =el2;
      	curr2->next  = head2;
      	head2 = curr2; 
	}
    }

    /*compare*/
    totEleNum= ne1; /*last read in element was put to 1st on the list*/
    printf("Total elements number %d\n", totEleNum);

    // if has tca, ne1 != ne2 is reasonable
    if (ne1 != ne2) { 
	// add while loop to eliminate tca. tca can get different size 	
	item1 * testTca = NULL;
	testTca = curr1;
	int hasTca = 0;

    	while (testTca) { 
	    if (testTca->el.hdr.vg_type == TCA_ELM) {
		hasTca = 1;
		break;
	    }
	    testTca = testTca->next;
	}

	if (hasTca == 0) {
	    printf("*** The two files %s and %s contain the different record sizes.\n\n", vgfn, asfn);
	    return FALSE;
	}
    }

    ne1 = 0;
    

    head1 = curr1;
    while (curr1) {   
	ne1++;	
	ne = 0;

    	
   	while (curr2) { 
	    if (curr1->el.hdr.vg_type == curr2->el.hdr.vg_type && curr2->el.hdr.delete != 1) {
	    //only consider curr2->el.hdr.delete != 1. We added the flag to the checked element
		if (curr1->el.hdr.vg_type == FILEHEAD_ELM) { /*"FILEHEAD_ELM" won't happen*/
		    curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		}
		else if (curr1->el.hdr.vg_type == LINE_ELM) { 
		    if (compareLin(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++; 
			break;
		    }
		    
		}
		else if (curr1->el.hdr.vg_type == FRONT_ELM ) { 

		    if (compareFrt(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) { 
			curr2->el.hdr.delete = 1;
			ne++;
			break;
		    }
		    
		}
		else if (curr1->el.hdr.vg_type == SPLN_ELM ) { 
		    if (curr1->el.hdr.grptyp == 1 || curr1->el.hdr.grptyp == 2) { /* cloud/turb */
		    	if (compareCloudSpl(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			    curr2->el.hdr.delete = 1;
		    	    ne++;
			    break;
		    	}
		    }
		    else {
		    	if (compareSpl(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			    curr2->el.hdr.delete = 1;
		    	    ne++;
			    break;
		    	}
		    }
		}
		   
		else if (curr1->el.hdr.vg_type == WBOX_ELM ) { /*"WBOX_ELM"*/
		    if (compareWbx(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == 34 ) { /*"WBOX_ELM"*/
		    curr2->el.hdr.delete = 1;
		    ne++;
		    break; //don't do anything
		}
		else if (curr1->el.hdr.vg_type == WXSYM_ELM ||curr1->el.hdr.vg_type == SPSYM_ELM
		|| ( curr1->el.hdr.vg_type >= 10 && curr1->el.hdr.vg_type <= 16 )
		|| curr1->el.hdr.vg_type == MARK_ELM || curr1->el.hdr.vg_type == CMBSY_ELM ) {
		/*CTSYM_ELM: ICSYM_ELM: PTSYM_ELM: PWSYM_ELM: SKSYM_ELM: SPSYM_ELM: TBSYM_ELM:*/		
		    if (compareSym(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == BARB_ELM || curr1->el.hdr.vg_type == ARROW_ELM
		|| curr1->el.hdr.vg_type == DARR_ELM || curr1->el.hdr.vg_type == HASH_ELM ) {
		    if (compareVec(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}

		else if (curr1->el.hdr.vg_type == SPTX_ELM) {
		    if (curr1->el.hdr.grptyp == 1 ) { /* cloud */
		    	if (compareCloudSpt(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			    curr2->el.hdr.delete = 1;
		    	    ne++;
			    break;
		    	}
		    }
		    else { 
		    	if (compareSpt(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) { 
			    curr2->el.hdr.delete = 1;
		    	    ne++;
			    break;
		    	}
		    }
		}

		else if (curr1->el.hdr.vg_type == CIRCLE_ELM) {
		    if (compareCir(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == TRKSTORM_ELM) {
		    if (compareTrk(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type >= 27 && curr1->el.hdr.vg_type <= 31 ) {
		/*"SIGINTL_ELM" "SIGNCON_ELM" "SIGCONV_ELM" "SIGOUTL_ELM" "SIGAIRM_ELM" */
		    if (compareSig(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == SIGCCF_ELM ) {
		    if (compareCcf(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == LIST_ELM ) { 
		    if (compareLst(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == ASHCLD_ELM ) { 
		    if (compareAsh(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == VOLC_ELM ) { 
		    if (compareVol(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == JET_ELM ) {
		    if (compareJet(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == TCA_ELM ) { 
		    if (compareTca(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}
		else if (curr1->el.hdr.vg_type == GFA_ELM ) { 
		    if (compareGfa(curr1->el, curr2->el, ne1) && compareHdr(curr1->el, curr2->el, ne1)) {
			curr2->el.hdr.delete = 1;
		    	ne++;
			break;
		    }
		}    		   
            }
	    ne2++;
	    curr2 = curr2->next;
	} //while2

	curr2 = head2; /*curr2 move to head, compare will pass if delete=1 */

	if (ne ==0) {
	    
 	    printf("The element %d %s in the first file is not contained in the second file.\n", (totEleNum-ne1+1), getVgType(curr1->el.hdr.vg_type));
	    printf("\n*** The two files %s and %s contain the different information!\n\n", vgfn, asfn);
	    return FALSE;
	}
	
	curr1 = curr1->next;
    } //while1

    printf("\nNote that the header file is ignored.  The Delete, ranges, Version and grpnum fields of the hdr are ignored.\nNote that the speed, dir, itxfn and times fields of the Track are ignored.\nNote that the gfa_subType and gfa_txtLayout of the Gfa are ignored.\nNote that the linetype and szarrow of the Ccf are ignored.\n");
    printf("\nThe two files %s and %s contain the same information.\n\n", vgfn, asfn);

    return TRUE;

}


/*Bool comparePoint(float[] latlon1, float[] latlon2, int pnts) {
    int i;
    for (i=0; i<pnts; i++) {
	if (latlon[i] != latlon[i]) 
 	
	    return FALSE;
    }
    return TRUE;
}*/


Bool compareHdr(VG_DBStruct el1, VG_DBStruct el2, int ne1) { //printf("here2\n");
    if ( el1.hdr.vg_type == SPTX_ELM ) {

	if (el1.hdr.vg_class == el2.hdr.vg_class
	&& el1.hdr.filled == el2.hdr.filled
	&& el1.hdr.closed == el2.hdr.closed
	&& el1.hdr.smooth == el2.hdr.smooth 
	&& (el1.hdr.maj_col == el2.hdr.maj_col || el1.hdr.maj_col == el2.elem.spt.info.txtcol
	    || el1.elem.spt.info.txtcol == el2.hdr.maj_col)	
	)

	    return TRUE;
    }
    else {
	if ( el1.hdr.vg_class == el2.hdr.vg_class
	&& el1.hdr.filled == el2.hdr.filled 
	&& el1.hdr.closed == el2.hdr.closed
	&& el1.hdr.smooth == el2.hdr.smooth 
	&& el1.hdr.maj_col == el2.hdr.maj_col
	//&& el1.hdr.min_col == el2.hdr.min_col 
	)
	return TRUE;
    }

    /*if ( (el1.hdr.vg_type == WBOX_ELM && el2.hdr.vg_type == WBOX_ELM) //watch grptyp=98 || grptyp==0
	|| (el1.hdr.grptyp ==8 || el2.hdr.grptyp ==8) 		//contour grptyp 8,5,6
	|| (el1.hdr.grpnum >0 && el2.hdr.grpnum >0 && el1.hdr.vg_type == SPTX_ELM)) {   
	
	if (el1.hdr.vg_class == el2.hdr.vg_class
	&& el1.hdr.filled == el2.hdr.filled 
	&& el1.hdr.closed == el2.hdr.closed
	&& el1.hdr.smooth == el2.hdr.smooth 
	&& (el1.hdr.maj_col == el2.hdr.maj_col || el1.hdr.maj_col == el2.elem.spt.info.txtcol
	    || el1.elem.spt.info.txtcol == el2.hdr.maj_col)  // for text
	)

	    return TRUE;
    }
    
    else if ( el1.hdr.grpnum >0 && el2.hdr.grpnum >0 ) {

	if (el1.hdr.vg_class == el2.hdr.vg_class
	&& el1.hdr.filled == el2.hdr.filled //
	&& el1.hdr.closed == el2.hdr.closed
	&& el1.hdr.smooth == el2.hdr.smooth 
//	&& el1.hdr.grptyp == el2.hdr.grptyp 
	//&& el1.hdr.grpnum == el2.hdr.grpnum //cloud
	&& (el1.hdr.maj_col == el2.hdr.maj_col || el1.hdr.maj_col == el2.elem.spt.info.txtcol
	    || el1.elem.spt.info.txtcol == el2.hdr.maj_col)	
	)

	    return TRUE;
    }
    else if ( el1.hdr.vg_type == SPTX_ELM ) {

	if (el1.hdr.vg_class == el2.hdr.vg_class
	&& el1.hdr.filled == el2.hdr.filled
	&& el1.hdr.closed == el2.hdr.closed
	&& el1.hdr.smooth == el2.hdr.smooth 
	//&& el1.hdr.grptyp == el2.hdr.grptyp 
	//&& (el1.hdr.grpnum == el2.hdr.grpnum || el1.hdr.grpnum ==-1 || el2.hdr.grpnum ==-1)
	&& (el1.hdr.maj_col == el2.hdr.maj_col || el1.hdr.maj_col == el2.elem.spt.info.txtcol
	    || el1.elem.spt.info.txtcol == el2.hdr.maj_col)	
	)

	    return TRUE;
    }
    else {
	if ( el1.hdr.vg_class == el2.hdr.vg_class
	&& el1.hdr.filled == el2.hdr.filled 
	&& el1.hdr.closed == el2.hdr.closed
	&& el1.hdr.smooth == el2.hdr.smooth 
	//&& el1.hdr.grptyp == el2.hdr.grptyp 
	//&& (el1.hdr.grpnum == el2.hdr.grpnum || el1.hdr.grpnum ==-1 || el2.hdr.grpnum ==-1)
	&& el1.hdr.maj_col == el2.hdr.maj_col
	&& el1.hdr.min_col == el2.hdr.min_col 
	)*/
    
    return FALSE;
}

Bool compareFrt(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.frt.info.numpts;
    int p2 = el2.elem.frt.info.numpts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.frt.latlon[i] - el2.elem.frt.latlon[i]) > MIN_DIFF) {
	    return FALSE;}
    }
    
    /* some example has fwidth 0, it should be from fcode, and could be 2 */
    if (el1.elem.frt.info.fwidth <=0 ||el2.elem.frt.info.fwidth <=0)
	;
    else if (el1.elem.frt.info.fwidth != el2.elem.frt.info.fwidth)
	return FALSE;

    if ( el1.elem.frt.info.numpts == el2.elem.frt.info.numpts
	&& el1.elem.frt.info.fcode == el2.elem.frt.info.fcode
	&& el1.elem.frt.info.fpipsz == el2.elem.frt.info.fpipsz
	/*&& el1.elem.frt.info.fpipst == el2.elem.frt.info.fpipst
	&& el1.elem.frt.info.fpipdr == el2.elem.frt.info.fpipdr //pres_pmsl_2010120212f000.vgf has -1	 
	&& strcmp( el1.elem.frt.info.frtlbl, el2.elem.frt.info.frtlbl) ==0*/
	) {

    	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");

	printf("%s\n", buffer);
	return TRUE;
    }
    else
	return FALSE;
}

Bool compareLin(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.lin.info.numpts;
    int p2 = el2.elem.lin.info.numpts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.lin.latlon[i] - el2.elem.lin.latlon[i]) > MIN_DIFF) 
	    return FALSE;
    }	
	
    if ( p1 == p2 
	&& el1.elem.lin.info.lintyp == el2.elem.lin.info.lintyp
	&& el1.elem.lin.info.width == el2.elem.lin.info.width 
	/*&& el1.elem.lin.info.lthw == el2.elem.lin.info.lthw //pres_pmsl_2010120212f000.vgf 1, 2
	&& el1.elem.lin.info.lwhw == el2.elem.lin.info.lwhw */
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else {
	return FALSE;
    }
}

Bool compareSpl(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.spl.info.numpts;
    int p2 = el2.elem.spl.info.numpts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.spl.latlon[i] - el2.elem.spl.latlon[i]) > MIN_DIFF) 
	    return FALSE;
    }		

    if ( p1 == p2 
	&& el1.elem.spl.info.spltyp == el2.elem.spl.info.spltyp
	/*&& el1.elem.spl.info.splstr == el2.elem.spl.info.splstr
	&& el1.elem.spl.info.spldir == el2.elem.spl.info.spldir*/
	&& el1.elem.spl.info.splsiz == el2.elem.spl.info.splsiz
	&& el1.elem.spl.info.splwid == el2.elem.spl.info.splwid
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareCloudSpl(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.spl.info.numpts;
    int p2 = el2.elem.spl.info.numpts;
    if (p1 != p2)
	return FALSE;

    /* for cloud/turb, line points might be different
    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.spl.latlon[i]-el2.elem.spl.latlon[i]) > MIN_DIFF) 
	    return FALSE;
    }*/		
//printf("cloud line lat %f\n",el1.elem.spl.latlon[0]);
    if ( p1 == p2 
	&& el1.elem.spl.info.spltyp == el2.elem.spl.info.spltyp
	/*&& el1.elem.spl.info.splstr == el2.elem.spl.info.splstr
	&& el1.elem.spl.info.spldir == el2.elem.spl.info.spldir*/
	&& el1.elem.spl.info.splsiz == el2.elem.spl.info.splsiz
	&& el1.elem.spl.info.splwid == el2.elem.spl.info.splwid
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareSym(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    if ( el1.elem.sym.info.numsym == el2.elem.sym.info.numsym 
	&& el1.elem.sym.info.width == el2.elem.sym.info.width
	&& el1.elem.sym.info.size == el2.elem.sym.info.size
	/*&& el1.elem.sym.info.ityp == el2.elem.sym.info.ityp*/
	&& el1.elem.sym.data.code[0] == el2.elem.sym.data.code[0]
	&& fabs(el1.elem.sym.data.latlon[0] - el2.elem.sym.data.latlon[0]) < MIN_DIFF
	&& fabs(el1.elem.sym.data.latlon[1] - el2.elem.sym.data.latlon[1]) < MIN_DIFF
	&& el1.elem.sym.data.offset_xy[0] == el2.elem.sym.data.offset_xy[0]
	&& el1.elem.sym.data.offset_xy[1] == el2.elem.sym.data.offset_xy[1]
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;

}

Bool compareVec(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    if ( el1.elem.wnd.info.numwnd == el2.elem.wnd.info.numwnd 
	&& el1.elem.wnd.info.width == el2.elem.wnd.info.width 
	&& el1.elem.wnd.info.size == el2.elem.wnd.info.size
	&& el1.elem.wnd.info.wndtyp == el2.elem.wnd.info.wndtyp
	&& el1.elem.wnd.info.hdsiz == el2.elem.wnd.info.hdsiz
  	&& fabs(el1.elem.wnd.data.spddir[0] - el2.elem.wnd.data.spddir[0]) < MIN_DIFF
  	&& fabs(el1.elem.wnd.data.spddir[1] - el2.elem.wnd.data.spddir[1]) < MIN_DIFF
	&& fabs(el1.elem.wnd.data.latlon[0] - el2.elem.wnd.data.latlon[0]) < MIN_DIFF
	&& fabs(el1.elem.wnd.data.latlon[1] - el2.elem.wnd.data.latlon[1]) < MIN_DIFF
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareSpt(VG_DBStruct el1, VG_DBStruct el2, int ne1) {

    /* compare text */
    if (el1.elem.spt.info.sptxtyp == 15 && el2.elem.spt.info.sptxtyp == 15) { // midLevelCloud
	char *temp1 = (char*)(el1.elem.spt.text); // CU;CI||5|300|77||ISOL;FRQ|300/200
	char *temp2 = (char*)(el2.elem.spt.text);
	const char delimiter[] = "|";
    	strsep(&temp1, delimiter); /* cut off cloud type from temp1: |5|300|77||ISOL;FRQ|300/200 */
	strsep(&temp1, delimiter); /* cut off cloud amt: 5|300|77||ISOL;FRQ|300/200 */
	strsep(&temp2, delimiter);
	strsep(&temp2, delimiter);

	if (strcmp( temp1, temp2) != 0) /* compare rest */
	    return FALSE;
    }
    else {	
	if (strcmp( el1.elem.spt.text, el2.elem.spt.text) !=0 ) {
    	// memcpy(el1.elem.spt.text, el2.elem.spt.text, strlen(el2.elem.spt.text))
	    // situation like 450 and 450/, it should be true
	    if (strlen(el1.elem.spt.text) >=3 
		&& strncmp( el1.elem.spt.text, el2.elem.spt.text, 3) ==0 )
		;
	    else 
	    	return FALSE;
	}
    }


    /* compare sztext */
    Bool szEql = checkSztext (el1.elem.spt.info.sztext, el2.elem.spt.info.sztext);
    if (szEql == FALSE)
    	return FALSE;
    
    if ( fabs(el1.elem.spt.info.rotn - el2.elem.spt.info.rotn) < MIN_DIFF
        //&& el1.elem.spt.info.sptxtyp == el2.elem.spt.info.sptxtyp //underline is not implemented
        //&& el1.elem.spt.info.turbsym == el2.elem.spt.info.turbsym
        && fabs(el1.elem.spt.info.itxfn - el2.elem.spt.info.itxfn) < MIN_DIFF
        /*&& el1.elem.spt.info.ithw == el2.elem.spt.info.ithw  //no software font 1 in xml
        && el1.elem.spt.info.iwidth == el2.elem.spt.info.iwidth //not in xml */
        && el1.elem.spt.info.txtcol == el2.elem.spt.info.txtcol
        /*&& el1.elem.spt.info.lincol == el2.elem.spt.info.lincol
        && el1.elem.spt.info.filcol == el2.elem.spt.info.filcol*/
        && el1.elem.spt.info.ialign == el2.elem.spt.info.ialign
        && fabs(el1.elem.spt.info.lat - el2.elem.spt.info.lat) < MIN_DIFF
        && fabs(el1.elem.spt.info.lon - el2.elem.spt.info.lon) < MIN_DIFF
//        && el1.elem.spt.info.offset_x == el2.elem.spt.info.offset_x
//        && el1.elem.spt.info.offset_y == el2.elem.spt.info.offset_y
	) {
     
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareCloudSpt(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    /* compare sztext */
    Bool szEql = checkSztext (el1.elem.spt.info.sztext, el2.elem.spt.info.sztext);
    if (szEql == FALSE)
    	return FALSE;

    /* don't compare text and sptxtyp. text convert to midLevel*/
    if ( el1.elem.spt.info.rotn == el2.elem.spt.info.rotn 
        && el1.elem.spt.info.turbsym == el2.elem.spt.info.turbsym
        && el1.elem.spt.info.itxfn == el2.elem.spt.info.itxfn
        /*&& el1.elem.spt.info.ithw == el2.elem.spt.info.ithw  //no software font 1 in xml
        && el1.elem.spt.info.iwidth == el2.elem.spt.info.iwidth*/
        && el1.elem.spt.info.txtcol == el2.elem.spt.info.txtcol
        /*&& el1.elem.spt.info.lincol == el2.elem.spt.info.lincol
        && el1.elem.spt.info.filcol == el2.elem.spt.info.filcol*/
        && el1.elem.spt.info.ialign == el2.elem.spt.info.ialign
        && fabs(el1.elem.spt.info.lat - el2.elem.spt.info.lat) < MIN_DIFF
        && fabs(el1.elem.spt.info.lon - el2.elem.spt.info.lon) < MIN_DIFF
        && el1.elem.spt.info.offset_x == el2.elem.spt.info.offset_x
        && el1.elem.spt.info.offset_y == el2.elem.spt.info.offset_y
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareCir(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.cir.info.numpts;
    int p2 = el2.elem.cir.info.numpts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.cir.data.latlon[i] - el2.elem.cir.data.latlon[i]) > MIN_DIFF) {
	    return FALSE;
	}
    }

    if ( el1.elem.cir.info.numpts == el2.elem.cir.info.numpts 
	&& el1.elem.cir.info.width - el2.elem.cir.info.width <0.1
	&& el1.elem.cir.info.lintyp == el2.elem.cir.info.lintyp 	
	/*&& el1.elem.cir.info.lthw == el2.elem.cir.info.lthw
	&& el1.elem.cir.info.lwhw == el2.elem.cir.info.lwhw*/	
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareTrk(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.trk.info.npts;
    int p2 = el2.elem.trk.info.npts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.trk.latlon[i] - el2.elem.trk.latlon[i]) > MIN_DIFF) {
	    return FALSE;
	}
    }	

    if ( el1.elem.trk.info.subtype == el2.elem.trk.info.subtype 
	&& el1.elem.trk.info.npts == el2.elem.trk.info.npts	
	&& el1.elem.trk.info.nipts == el2.elem.trk.info.nipts
	/*&& el1.elem.trk.info.ltype1 == el2.elem.trk.info.ltype1
	&& el1.elem.trk.info.ltype2 == el2.elem.trk.info.ltype2
	&& el1.elem.trk.info.mtype1 == el2.elem.trk.info.mtype1
	&& el1.elem.trk.info.mtype2 == el2.elem.trk.info.mtype2*/
	&& el1.elem.trk.info.width == el2.elem.trk.info.width
	//&& el1.elem.trk.info.speed == el2.elem.trk.info.speed //cave didn't record it
	//&& el1.elem.trk.info.dir == el2.elem.trk.info.dir //cave didn't record it
	&& el1.elem.trk.info.incr == el2.elem.trk.info.incr
	&& el1.elem.trk.info.skip == el2.elem.trk.info.skip
	//&& el1.elem.trk.info.itxfn == el2.elem.trk.info.itxfn //check cave
	//&& fabs(el1.elem.trk.info.sztext - el2.elem.trk.info.sztext) <0.01
	//&& el1.elem.trk.info.ithw == el2.elem.trk.info.ithw
	//&& el1.elem.trk.info.times == el2.elem.trk.info.times
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.\n");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareWbx(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.wbx.info.numpts; //8
    int p2 = el2.elem.wbx.info.numpts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.wbx.latlon[i] - el2.elem.wbx.latlon[i]) > MIN_DIFF) 
	    return FALSE;
	if ( el1.elem.wbx.info.cn_fips[i] != el2.elem.wbx.info.cn_fips[i])
	    return FALSE;
	
    }	

    if ( el1.elem.wbx.info.numpts == el2.elem.wbx.info.numpts 
	//&& el1.elem.wbx.info.w_style == el2.elem.wbx.info.w_style //N/A
	&& el1.elem.wbx.info.w_shape == el2.elem.wbx.info.w_shape
	&& el1.elem.wbx.info.w_mrktyp == el2.elem.wbx.info.w_mrktyp
	&& el1.elem.wbx.info.w_mrksiz == el2.elem.wbx.info.w_mrksiz
	&& el1.elem.wbx.info.w_mrkwid == el2.elem.wbx.info.w_mrkwid
	&& strcmp( el1.elem.wbx.info.w_a0id, el2.elem.wbx.info.w_a0id) ==0
	&& el1.elem.wbx.info.w_a0lt == el2.elem.wbx.info.w_a0lt
	&& el1.elem.wbx.info.w_a0ln == el2.elem.wbx.info.w_a0ln
	&& el1.elem.wbx.info.w_a0dis == el2.elem.wbx.info.w_a0dis
	&& strcmp( el1.elem.wbx.info.w_a0dir, el2.elem.wbx.info.w_a0dir) ==0
	&& strcmp( el1.elem.wbx.info.w_a1id, el2.elem.wbx.info.w_a1id) ==0
	&& el1.elem.wbx.info.w_a1lt == el2.elem.wbx.info.w_a1lt
	&& el1.elem.wbx.info.w_a1ln == el2.elem.wbx.info.w_a1ln
	&& el1.elem.wbx.info.w_a1dis == el2.elem.wbx.info.w_a1dis
	&& strcmp( el1.elem.wbx.info.w_a1dir, el2.elem.wbx.info.w_a1dir) ==0
	&& el1.elem.wbx.info.w_istat == el2.elem.wbx.info.w_istat
	&& el1.elem.wbx.info.w_number == el2.elem.wbx.info.w_number
	&& strcmp( el1.elem.wbx.info.w_iss_t, el2.elem.wbx.info.w_iss_t) ==0
	&& strcmp( el1.elem.wbx.info.w_exp_t, el2.elem.wbx.info.w_exp_t) ==0
	&& el1.elem.wbx.info.w_type == el2.elem.wbx.info.w_type
	//&& el1.elem.wbx.info.w_severity == el2.elem.wbx.info.w_severity //tag2vgf seems to convert it wrong. always 0.
	&& strcmp( el1.elem.wbx.info.w_timezone, el2.elem.wbx.info.w_timezone) ==0
	&& ( strcmp( el1.elem.wbx.info.w_hailsz, "")==0 || strcmp( el2.elem.wbx.info.w_hailsz, "")==0
	    || strcmp( el1.elem.wbx.info.w_hailsz, el2.elem.wbx.info.w_hailsz) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_windg, "")==0 || strcmp( el2.elem.wbx.info.w_windg, "")==0
	    || strcmp( el1.elem.wbx.info.w_windg, el2.elem.wbx.info.w_windg) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_tops, "")==0 || strcmp( el2.elem.wbx.info.w_tops, "")==0
	    || strcmp( el1.elem.wbx.info.w_tops, el2.elem.wbx.info.w_tops) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_msmv_d, "")==0 || strcmp( el2.elem.wbx.info.w_msmv_d, "")==0
	    || strcmp( el1.elem.wbx.info.w_msmv_d, el2.elem.wbx.info.w_msmv_d) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_msmv_s, "")==0 || strcmp( el2.elem.wbx.info.w_msmv_s, "")==0
	    || strcmp( el1.elem.wbx.info.w_msmv_s, el2.elem.wbx.info.w_msmv_s) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_states, "")==0 || strcmp( el2.elem.wbx.info.w_states, "")==0
	    || strcmp(trimwhitespace(el1.elem.wbx.info.w_states), trimwhitespace(el2.elem.wbx.info.w_states)) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_adjarea, "")==0 || strcmp( el2.elem.wbx.info.w_adjarea, "")==0
	    || strcmp( el1.elem.wbx.info.w_adjarea, el2.elem.wbx.info.w_adjarea) ==0)
	&& ( strcmp( el1.elem.wbx.info.w_replw, "")==0 || strcmp( el2.elem.wbx.info.w_replw, "")==0
	    || atoi( el1.elem.wbx.info.w_replw) == atoi(el2.elem.wbx.info.w_replw)) //0003 vs 3
	&& ( strcmp( el1.elem.wbx.info.w_fcstr, "")==0 || strcmp( el2.elem.wbx.info.w_fcstr, "")==0
	    || strcmp( el1.elem.wbx.info.w_fcstr, el2.elem.wbx.info.w_fcstr) ==0)
	/*&& ( strcmp( el1.elem.wbx.info.w_file, "")==0 || strcmp( el2.elem.wbx.info.w_file, "")==0
	    || strcmp( el1.elem.wbx.info.w_file, el2.elem.wbx.info.w_file) ==0)
	&& ( strcmp( el1.elem.wbx.info.wsm_ref, "")==0 || strcmp( el2.elem.wbx.info.wsm_ref, "")==0
	    || strcmp( el1.elem.wbx.info.wsm_ref, el2.elem.wbx.info.wsm_ref) ==0)*/
	&& ( strcmp( el1.elem.wbx.info.wsm_iss_t, "")==0 || strcmp( el2.elem.wbx.info.wsm_iss_t, "")==0
	    || strcmp( el1.elem.wbx.info.wsm_iss_t, el2.elem.wbx.info.wsm_iss_t) ==0)
	&& ( strcmp( el1.elem.wbx.info.wsm_exp_t, "")==0 || strcmp( el2.elem.wbx.info.wsm_exp_t, "")==0
	    || strcmp( el1.elem.wbx.info.wsm_exp_t, el2.elem.wbx.info.wsm_exp_t) ==0)	
	&& ( strcmp( el1.elem.wbx.info.wsm_from, "")==0 || strcmp( el2.elem.wbx.info.wsm_from, "")==0
	    || strcmp( el1.elem.wbx.info.wsm_from, el2.elem.wbx.info.wsm_from) ==0)
	&& ( strcmp( el1.elem.wbx.info.wsm_meso, "")==0 || strcmp( el2.elem.wbx.info.wsm_meso, "")==0
	    || atoi( el1.elem.wbx.info.wsm_meso) == atoi(el2.elem.wbx.info.wsm_meso) )
	&& ( strcmp( el1.elem.wbx.info.wsm_fcstr, "")==0 || strcmp( el2.elem.wbx.info.wsm_fcstr, "")==0
	    || strcmp( el1.elem.wbx.info.wsm_fcstr, el2.elem.wbx.info.wsm_fcstr) ==0)
	&& el1.elem.wbx.info.numcnty == el2.elem.wbx.info.numcnty
	&& el1.elem.wbx.info.cn_flag == el2.elem.wbx.info.cn_flag
	
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareLst(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    if ( el1.elem.lst.data.nitems == el2.elem.lst.data.nitems
	&& el1.elem.lst.info.subtyp == el2.elem.lst.info.subtyp 
	&& el1.elem.lst.info.mrktyp == el2.elem.lst.info.mrktyp
	&& el1.elem.lst.info.mrksiz == el2.elem.lst.info.mrksiz
	&& el1.elem.lst.info.mrkwid == el2.elem.lst.info.mrkwid
	&& el1.elem.lst.data.nitems == el2.elem.lst.data.nitems
	//&& el1.elem.lst.data.latlon[2*el1.elem.lst.data.nitems] == el2.elem.lst.data.latlon[2*el1.elem.lst.data.nitems]
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareSig(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.sig.info.npts; 
    int p2 = el2.elem.sig.info.npts;
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.sig.latlon[i] - el2.elem.sig.latlon[i]) > MIN_DIFF) 
	    return FALSE;
    }	
    
    /*extra for intl */
    if (el1.hdr.vg_type == 27) {
	if ( strcmp( el1.elem.sig.info.stime, el2.elem.sig.info.stime) ==0
	&& strcmp( el1.elem.sig.info.etime, el2.elem.sig.info.etime) ==0
	&& ( strcmp(el1.elem.sig.info.remarks, "")==0 || strcmp(el2.elem.sig.info.remarks, "")==0
	|| strcmp( trimwhitespace(el1.elem.sig.info.remarks), trimwhitespace(el2.elem.sig.info.remarks)) ==0 )
	//&& el1.elem.sig.info.sonic == el2.elem.sig.info.sonic  N/A
	&& ( strcmp(el1.elem.sig.info.phenom, "")==0 || strcmp(el2.elem.sig.info.phenom, "")==0
	|| strcmp( trimwhitespace(el1.elem.sig.info.phenom), trimwhitespace(el2.elem.sig.info.phenom)) ==0 )
	&& ( strcmp(el1.elem.sig.info.phenom2, "")==0 || strcmp(el2.elem.sig.info.phenom2, "")==0
	|| strcmp( trimwhitespace(el1.elem.sig.info.phenom2), trimwhitespace(el2.elem.sig.info.phenom2)) ==0 )
	/*&& ( strcmp(el1.elem.sig.info.phennam, "") || strcmp(el2.elem.sig.info.phennam, "")				// N/A
	||strcmp( trimwhitespace(el1.elem.sig.info.phennam), trimwhitespace(el2.elem.sig.info.phennam)) ==0 )*/
	
	&& strcmp( el1.elem.sig.info.phenlat, el2.elem.sig.info.phenlat) ==0
	&& strcmp( el1.elem.sig.info.phenlon, el2.elem.sig.info.phenlon) ==0
	&& el1.elem.sig.info.pres == el2.elem.sig.info.pres
	&& el1.elem.sig.info.maxwind == el2.elem.sig.info.maxwind
	&& strcmp( el1.elem.sig.info.freetext, el2.elem.sig.info.freetext) ==0
	&& ( strcmp(el1.elem.sig.info.trend, "")==0 || strcmp(el2.elem.sig.info.trend, "")==0
	||strcmp( trimwhitespace(el1.elem.sig.info.trend), trimwhitespace(el2.elem.sig.info.trend)) ==0 )
	&& strcmp( el1.elem.sig.info.move, el2.elem.sig.info.move) ==0 
	/*&& el1.elem.sig.info.obsfcst == el2.elem.sig.info.obsfcst  N/A
	&& strcmp( el1.elem.sig.info.obstime, el2.elem.sig.info.obstime) ==0
	&& el1.elem.sig.info.fl == el2.elem.sig.info.fl
	&& strcmp( el1.elem.sig.info.fcstr, el2.elem.sig.info.fcstr) ==0*/
	&& el1.elem.sig.info.spd == el2.elem.sig.info.spd
	&& ( strcmp(el1.elem.sig.info.dir, "")==0 || strcmp(el2.elem.sig.info.dir, "")==0
	|| strcmp( trimwhitespace(el1.elem.sig.info.dir), trimwhitespace(el2.elem.sig.info.dir)) ==0 )
	&& strcmp( el1.elem.sig.info.tops, el2.elem.sig.info.tops) ==0
	) {
	    ;
    	}
	else 
	    return FALSE;
    }

    /*all sigmet*/
    if ( el1.elem.sig.info.subtype == el2.elem.sig.info.subtype 
	&& el1.elem.sig.info.npts == el2.elem.sig.info.npts
	&& el1.elem.sig.info.lintyp == el2.elem.sig.info.lintyp
	&& el1.elem.sig.info.linwid == el2.elem.sig.info.linwid
	&& el1.elem.sig.info.sol == el2.elem.sig.info.sol
	&& strcmp( trimwhitespace(el1.elem.sig.info.area), trimwhitespace(el2.elem.sig.info.area)) ==0 
	//&& strcmp( el1.elem.sig.info.fir, el2.elem.sig.info.fir) ==0 const.
	&& el1.elem.sig.info.status == el2.elem.sig.info.status
	&& el1.elem.sig.info.distance == el2.elem.sig.info.distance
	&& strcmp( trimwhitespace(el1.elem.sig.info.msgid), trimwhitespace(el2.elem.sig.info.msgid)) ==0
	&& el1.elem.sig.info.seqnum == el2.elem.sig.info.seqnum
	) {
	   
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareCcf(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int p1 = el1.elem.ccf.info.npts; 
    int p2 = el2.elem.ccf.info.npts; 
    if (p1 != p2)
	return FALSE;

    int i=0;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.ccf.latlon[i] - el2.elem.ccf.latlon[i]) > MIN_DIFF) 
	    return FALSE;
    }	

    if ( el1.elem.ccf.info.subtype == el2.elem.ccf.info.subtype 
	/*&& el1.elem.ccf.info.linetype == el2.elem.ccf.info.linetype  const
	&& el1.elem.ccf.info.szarrow == el2.elem.ccf.info.szarrow   null*/
	&& el1.elem.ccf.info.growth == el2.elem.ccf.info.growth
	&& el1.elem.ccf.info.prob == el2.elem.ccf.info.prob
	&& el1.elem.ccf.info.tops == el2.elem.ccf.info.tops
	&& el1.elem.ccf.info.cover == el2.elem.ccf.info.cover 
	&& fabs(el1.elem.ccf.info.dir - el2.elem.ccf.info.dir) < MIN_DIFF
	&& fabs(el1.elem.ccf.info.spd - el2.elem.ccf.info.spd) < MIN_DIFF
	&& el1.elem.ccf.info.textlat == el2.elem.ccf.info.textlat
	&& el1.elem.ccf.info.textlon == el2.elem.ccf.info.textlon
	&& el1.elem.ccf.info.arrowlat == el2.elem.ccf.info.arrowlat
	&& el1.elem.ccf.info.arrowlon == el2.elem.ccf.info.arrowlon
	// textlauout
	) {
	   
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareJet(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int i, j, k;
    int lin1 = el1.elem.jet.line.spl.info.numpts;
    int lin2 = el2.elem.jet.line.spl.info.numpts;
    if (lin1 != lin2)
	return FALSE;
/*line*/
    for (k=0; k<lin1; k++) {
	if (fabs(el1.elem.jet.line.spl.latlon[k] - el2.elem.jet.line.spl.latlon[k]) > MIN_DIFF) 
	    return FALSE;
    }

    for (k=0; k<lin1; k++) {
	if ( lin1 == lin2 
	&& el1.elem.spl.info.spltyp == el2.elem.spl.info.spltyp
	/*&& el1.elem.spl.info.splstr == el2.elem.spl.info.splstr
	&& el1.elem.spl.info.spldir == el2.elem.spl.info.spldir*/
	&& el1.elem.spl.info.splsiz == el2.elem.spl.info.splsiz
	&& el1.elem.spl.info.splwid == el2.elem.spl.info.splwid
	) 
	    continue;
	else 
	    return FALSE;
    }

/*barb, text*/
    int wnd1 = el1.elem.jet.nbarb;
    int wnd2 = el2.elem.jet.nbarb;
    if (wnd1 != wnd2)
	return FALSE;

    for (i=0; i<wnd1; i++) {//clear, directiononly

	if ( el1.elem.jet.barb[i].wndcol == el2.elem.jet.barb[i].wndcol	
	&& el1.elem.jet.barb[i].wnd.info.width == el2.elem.jet.barb[i].wnd.info.width	
	&& el1.elem.jet.barb[i].wnd.info.size == el2.elem.jet.barb[i].wnd.info.size	
	&& el1.elem.jet.barb[i].wnd.info.wndtyp == el2.elem.jet.barb[i].wnd.info.wndtyp	
	&& el1.elem.jet.barb[i].wnd.info.hdsiz == el2.elem.jet.barb[i].wnd.info.hdsiz	
	&& fabs(el1.elem.jet.barb[i].wnd.data.spddir[0] - el2.elem.jet.barb[i].wnd.data.spddir[0]) < MIN_DIFF	
	&& fabs(el1.elem.jet.barb[i].wnd.data.spddir[1] - el2.elem.jet.barb[i].wnd.data.spddir[1]) < MIN_DIFF	
	&& fabs(el1.elem.jet.barb[i].wnd.data.latlon[0] - el2.elem.jet.barb[i].wnd.data.latlon[0]) < MIN_DIFF	
	&& fabs(el1.elem.jet.barb[i].wnd.data.latlon[1] - el2.elem.jet.barb[i].wnd.data.latlon[1]) < MIN_DIFF ) 
	    continue;
	else
	    return FALSE;
    }

    /* compare sztext */
   /* Bool szEql = checkSztext (el1.elem.spt.info.sztext, el2.elem.spt.info.sztext);
    if (szEql == FALSE)
    	return FALSE;
*/
    //text position shifted	
    for (i=0; i<wnd1; i++) {
    	if (el1.elem.jet.barb[i].sptcol == el2.elem.jet.barb[i].sptcol	
	&& el1.elem.jet.barb[i].spt.info.rotn == el2.elem.jet.barb[i].spt.info.rotn	
	/*&& el1.elem.jet.barb[i].spt.info.sztext == el2.elem.jet.barb[i].spt.info.sztext	
	&& el1.elem.jet.barb[i].spt.info.sptxtyp == el2.elem.jet.barb[i].spt.info.sptxtyp	
	&& el1.elem.jet.barb[i].spt.info.turbsym == el2.elem.jet.barb[i].spt.info.turbsym	
	&& el1.elem.jet.barb[i].spt.info.itxfn == el2.elem.jet.barb[i].spt.info.itxfn	
	&& el1.elem.jet.barb[i].spt.info.ithw == el2.elem.jet.barb[i].spt.info.ithw	
	&& el1.elem.jet.barb[i].spt.info.iwidth == el2.elem.jet.barb[i].spt.info.iwidth	
	&& el1.elem.jet.barb[i].spt.info.txtcol == el2.elem.jet.barb[i].spt.info.txtcol	
	&& el1.elem.jet.barb[i].spt.info.lincol == el2.elem.jet.barb[i].spt.info.lincol	
	&& el1.elem.jet.barb[i].spt.info.filcol == el2.elem.jet.barb[i].spt.info.filcol	
	&& el1.elem.jet.barb[i].spt.info.ialign == el2.elem.jet.barb[i].spt.info.ialign*/	
	/*&& el1.elem.jet.barb[i].spt.info.lat == el2.elem.jet.barb[i].spt.info.lat //shifted	
	&& el1.elem.jet.barb[i].spt.info.lon == el2.elem.jet.barb[i].spt.info.lon*/	
	&& el1.elem.jet.barb[i].spt.info.offset_x == el2.elem.jet.barb[i].spt.info.offset_x )	
	//&& el1.elem.jet.barb[i].spt.info.offset_y == el2.elem.jet.barb[i].spt.info.offset_y ) //-3 -- 0
	    continue;
	else
	    return FALSE;
    }

/*hash*/
    int hsh1 = el1.elem.jet.nhash;
    int hsh2 = el2.elem.jet.nhash;
    if (hsh1 != hsh2)
	return FALSE;
    for (j=0; j<hsh1; j++) {
	if ( el1.elem.jet.hash[j].wndcol == el2.elem.jet.hash[j].wndcol	
	&& el1.elem.jet.hash[j].wnd.info.width == el2.elem.jet.hash[j].wnd.info.width	
	&& el1.elem.jet.hash[j].wnd.info.size == el2.elem.jet.hash[j].wnd.info.size	
	&& el1.elem.jet.hash[j].wnd.info.wndtyp == el2.elem.jet.hash[j].wnd.info.wndtyp	
	&& el1.elem.jet.hash[j].wnd.info.hdsiz == el2.elem.jet.hash[j].wnd.info.hdsiz	
	&& fabs(el1.elem.jet.hash[j].wnd.data.spddir[0] - el2.elem.jet.hash[j].wnd.data.spddir[0]) <MIN_DIFF	
	&& fabs(el1.elem.jet.hash[j].wnd.data.spddir[1] - el2.elem.jet.hash[j].wnd.data.spddir[1]) < MIN_DIFF	
	&& fabs(el1.elem.jet.hash[j].wnd.data.latlon[0] - el2.elem.jet.hash[j].wnd.data.latlon[0]) < MIN_DIFF	
	&& fabs(el1.elem.jet.hash[j].wnd.data.latlon[1] - el2.elem.jet.hash[j].wnd.data.latlon[1]) < MIN_DIFF ) 
	    continue;
	else
	    return FALSE;
    }


    if ( el1.elem.jet.line.splcol == el2.elem.jet.line.splcol 
	&& el1.elem.jet.line.spl.info.spltyp == el2.elem.jet.line.spl.info.spltyp
	/*&& el1.elem.jet.line.spl.info.splstr == el2.elem.jet.line.spl.info.splstr
	&& el1.elem.jet.line.spl.info.spldir == el2.elem.jet.line.spl.info.spldir  //spldir=0 in jet. spldir=1" in spl*/
	&& el1.elem.jet.line.spl.info.splsiz == el2.elem.jet.line.spl.info.splsiz
	&& el1.elem.jet.line.spl.info.splwid == el2.elem.jet.line.spl.info.splwid
	) {
    
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareTca(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    int i, j;
    int count1 = el1.elem.tca.info.wwNum;
    int count2 = el2.elem.tca.info.wwNum;

    /* compare info.tcaww, and info.tcaww.numBreakPts, then rest of the info */
    /*wwNum could change for island, water. don't compare advisory part when count1 != count2*/
    if (count1 == count2) { 
    
    	for (i=0; i<count1; i++) {
	    if (el1.elem.tca.info.tcaww[i].severity == el2.elem.tca.info.tcaww[i].severity 
	    && el1.elem.tca.info.tcaww[i].advisoryType == el2.elem.tca.info.tcaww[i].advisoryType 
	    && el1.elem.tca.info.tcaww[i].specialGeog == el2.elem.tca.info.tcaww[i].specialGeog
	    && el1.elem.tca.info.tcaww[i].numBreakPts == el2.elem.tca.info.tcaww[i].numBreakPts ){
	    	;
    	    }
    	    else 
	    return FALSE;

	    for (j=0; j< el1.elem.tca.info.tcaww[i].numBreakPts; j++) {
	    	if (el1.elem.tca.info.tcaww[i].breakPnt[j].lat == el2.elem.tca.info.tcaww[i].breakPnt[j].lat
		&& el1.elem.tca.info.tcaww[i].breakPnt[j].lon == el2.elem.tca.info.tcaww[i].breakPnt[j].lon
		&& strcmp(el1.elem.tca.info.tcaww[i].breakPnt[j].breakPtName, el2.elem.tca.info.tcaww[i].breakPnt[j].breakPtName)==0 ) {
		    ;
	    	}
	    	else 
		    return FALSE;
	    }
    	}
    }

    if ( el1.elem.tca.info.stormNum == el2.elem.tca.info.stormNum 
	&& el1.elem.tca.info.issueStatus == el2.elem.tca.info.issueStatus // 0/1 char
	&& el1.elem.tca.info.basin == el2.elem.tca.info.basin
	&& strcmp( el1.elem.tca.info.advisoryNum, el2.elem.tca.info.advisoryNum )==0
	&& ( strcmp(el1.elem.tca.info.validTime, "")==0 
          || strcmp(el2.elem.tca.info.validTime, "")==0
	  || strcmp(el1.elem.tca.info.validTime, el2.elem.tca.info.validTime )==0 )
	&& strcmp( el1.elem.tca.info.stormName, el2.elem.tca.info.stormName )==0
	&& el1.elem.tca.info.stormType == el2.elem.tca.info.stormType
	&& strcmp( el1.elem.tca.info.timezone, el2.elem.tca.info.timezone )==0
	&& fabs(el1.elem.tca.info.text_lat - el2.elem.tca.info.text_lat)  < MIN_DIFF
	&& fabs(el1.elem.tca.info.text_lon - el2.elem.tca.info.text_lon)  < MIN_DIFF
	/*&& el1.elem.tca.info.text_font == el2.elem.tca.info.text_font 
	&& el1.elem.tca.info.text_size == el2.elem.tca.info.text_size 
	&& el1.elem.tca.info.text_width == el2.elem.tca.info.text_width  N/A*/) {
	
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else 
	return FALSE;
}

Bool compareGfa(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    char str[10000]="", str2[10000]=""; 
    char areaType[10]="",fcstHr[10]="", lineWidth[10]="",cycle[10]="",tagDesk[10]="",status[10]="";
    char areas[10]="", states[100]="", condsBegin[10]="", condsEnd[10]="", type[100]="";

    char subType[10]="",linelm[10]="",linetype[10]=""; /*arrowSize[10]="",txtColor[10]="",txtSize[10]="", txtFont[10]="",txtHardware[10]="",txtWidth[10]="",txtAlign[10]="";*/
    char txtLayout[50]="",lat[20]="",lon[20]="",arrow_lat[20]="",arrow_lon[20]="";
    char top[10]="", bottom[10]="", rg[10]="", category[10]="", frequency[10]="", contour[10]="", level[10]="", fzlRange[10]="";
    char intensity[10]="", speed[10]="", dueto[10]="", severity[10]="", coverage[10]="", fzlTop[10]="", fzlBottom[10]="";

    char areaType2[10]="",fcstHr2[10]="",lineWidth2[10]="",cycle2[10]="",tagDesk2[10]="",status2[10]="";
    char areas2[10]="", states2[100]="", condsBegin2[10]="", condsEnd2[10]="", type2[100]="";
    char subType2[10]="",linelm2[10]="",linetype2[10]=""; /*arrowSize2[10]="",txtColor2[10]="",txtSize2[10]="", txtFont2[10]="",txtHardware2[10]="",txtWidth2[10]="",txtAlign2[10]="";*/
    char txtLayout2[50]="",lat2[20]="",lon2[20]="",arrow_lat2[20]="",arrow_lon2[20]="";
    char top2[10]="", bottom2[10]="", rg2[10]="", category2[10]="", frequency2[10]="", contour2[10]="", level2[10]="", fzlRange2[10]="";
    char intensity2[10]="", speed2[10]="", dueto2[10]="", severity2[10]="", coverage2[10]="", fzlTop2[10]="", fzlBottom2[10]="";
    int ier;


    int p1 = el1.elem.gfa.info.npts; 
    int p2 = el2.elem.gfa.info.npts;
    if (p1 != p2)
	return FALSE;

    p1 = el1.elem.gfa.info.nblocks; 
    p2 = el2.elem.gfa.info.nblocks;
    if (p1 != p2)
	return FALSE;

    int i;
    for (i=0; i<p1; i++) {
	if (fabs(el1.elem.gfa.latlon[i]- el2.elem.gfa.latlon[i]) > MIN_DIFF) 
	    return FALSE;
    }


    for ( i = 0; i < el1.elem.gfa.info.nblocks; i++ ) {
	strcat ( str, *(el1.elem.gfa.info.blockPtr[ i ]) );
	strcat ( str2, *(el2.elem.gfa.info.blockPtr[ i ]) );
	
    }
   
/*el1*/
cst_gtag ( "gfa_areaType", str, "", areaType, &ier );
cst_gtag ( "gfa_fcstHr", str, "", fcstHr, &ier );
cst_gtag ( "gfa_tag", str, "", tagDesk, &ier );
cst_gtag ( "gfa_cycle", str, "", cycle, &ier );
cst_gtag ( "gfa_status", str, "", status, &ier ); 

cst_gtag ( "gfa_areas", str, "", areas, &ier );
cst_gtag ( "gfa_region", str, "", rg, &ier );
cst_gtag ( "gfa_statesList", str, "", states, &ier );
cst_gtag ( "gfa_condsBegin", str, "", condsBegin, &ier ); 
cst_gtag ( "gfa_condsEnd", str, "", condsEnd, &ier );
cst_gtag ( "gfa_txtLayout", str, "", txtLayout, &ier );
cst_gtag ( "gfa_arrow_lat", str, "", arrow_lat, &ier ); 
cst_gtag ( "gfa_arrow_lon", str, "", arrow_lon, &ier );
cst_gtag ( "gfa_lat", str, "", lat, &ier ); 
cst_gtag ( "gfa_lon", str, "", lon, &ier );
cst_gtag ( "gfa_lineWidth", str, "", lineWidth, &ier );

cst_gtag ( "gfa_subType", str, "", subType, &ier );
cst_gtag ( "gfa_linelm", str, "", linelm, &ier );
cst_gtag ( "gfa_linetype", str, "", linetype, &ier );
/*cst_gtag ( "gfa_arrowSize", str, "", arrowSize, &ier );
cst_gtag ( "gfa_txtColor", str, "", txtColor, &ier ); 
cst_gtag ( "gfa_txtSize", str, "", txtSize, &ier );
cst_gtag ( "gfa_txtFont", str, "", txtFont, &ier ); 
cst_gtag ( "gfa_txtHardware", str, "", txtHardware, &ier );
cst_gtag ( "gfa_txtWidth", str, "", txtWidth, &ier );
cst_gtag ( "gfa_txtAlign", str, "", txtAlign, &ier ); */

cst_gtag ( "Type", str, "", type, &ier );
cst_gtag ( "gfa_top", str, "", top, &ier );
cst_gtag ( "gfa_bottom", str, "", bottom, &ier );
cst_gtag ( "Category", str, "", category, &ier ); 
cst_gtag ( "Frequency", str, "", frequency, &ier );
cst_gtag ( "Contour", str, "", contour, &ier ); 
cst_gtag ( "Level", str, "", level, &ier );
cst_gtag ( "gfa_fzlRange", str, "", fzlRange, &ier );
cst_gtag ( "Intensity", str, "", intensity, &ier ); 
cst_gtag ( "Speed", str, "", speed, &ier );
cst_gtag ( "DUE TO", str, "", dueto, &ier ); 
cst_gtag ( "Severity", str, "", severity, &ier );
cst_gtag ( "Coverage", str, "", coverage, &ier ); 
cst_gtag ( "gfa_fzlTop", str, "", fzlTop, &ier );
cst_gtag ( "gfa_fzlBottom", str, "", fzlBottom, &ier );

/*el2*/
cst_gtag ( "gfa_areaType", str2, "", areaType2, &ier );
cst_gtag ( "gfa_fcstHr", str2, "", fcstHr2, &ier );
cst_gtag ( "gfa_tag", str2, "", tagDesk2, &ier );
cst_gtag ( "gfa_cycle", str2, "", cycle2, &ier );
cst_gtag ( "gfa_status", str2, "", status2, &ier ); 

cst_gtag ( "gfa_areas", str2, "", areas2, &ier );
cst_gtag ( "gfa_region", str2, "", rg2, &ier );
cst_gtag ( "gfa_statesList", str2, "", states2, &ier );
cst_gtag ( "gfa_condsBegin", str2, "", condsBegin2, &ier ); 
cst_gtag ( "gfa_condsEnd", str2, "", condsEnd2, &ier );
cst_gtag ( "gfa_txtLayout", str2, "", txtLayout2, &ier );
cst_gtag ( "gfa_arrow_lat", str2, "", arrow_lat2, &ier ); 
cst_gtag ( "gfa_arrow_lon", str2, "", arrow_lon2, &ier );
cst_gtag ( "gfa_lat", str2, "", lat2, &ier ); 
cst_gtag ( "gfa_lon", str2, "", lon2, &ier );
cst_gtag ( "gfa_lineWidth", str2, "", lineWidth2, &ier );

cst_gtag ( "gfa_subType", str2, "", subType2, &ier ); 
cst_gtag ( "gfa_linelm", str2, "", linelm2, &ier );
cst_gtag ( "gfa_linetype", str2, "", linetype2, &ier );
/*cst_gtag ( "gfa_arrowSize", str2, "", arrowSize2, &ier );
cst_gtag ( "gfa_txtColor", str2, "", txtColor2, &ier ); 
cst_gtag ( "gfa_txtSize", str2, "", txtSize2, &ier );
cst_gtag ( "gfa_txtFont", str2, "", txtFont2, &ier ); 
cst_gtag ( "gfa_txtHardware", str2, "", txtHardware2, &ier );
cst_gtag ( "gfa_txtWidth", str2, "", txtWidth2, &ier );
cst_gtag ( "gfa_txtAlign", str2, "", txtAlign2, &ier );*/ 

cst_gtag ( "Type", str2, "", type2, &ier );
cst_gtag ( "gfa_top", str2, "", top2, &ier );
cst_gtag ( "gfa_bottom", str2, "", bottom2, &ier );
cst_gtag ( "Category", str2, "", category2, &ier ); 
cst_gtag ( "Frequency", str2, "", frequency2, &ier );
cst_gtag ( "Contour", str2, "", contour2, &ier ); 
cst_gtag ( "Level", str2, "", level2, &ier );
cst_gtag ( "gfa_fzlRange", str2, "", fzlRange2, &ier );
cst_gtag ( "Intensity", str2, "", intensity2, &ier ); 
cst_gtag ( "Speed", str2, "", speed2, &ier );
cst_gtag ( "DUE TO", str2, "", dueto2, &ier ); 
cst_gtag ( "Severity", str2, "", severity2, &ier );
cst_gtag ( "Coverage", str2, "", coverage2, &ier ); 
cst_gtag ( "gfa_fzlTop", str2, "", fzlTop2, &ier );
cst_gtag ( "gfa_fzlBottom", str2, "", fzlBottom2, &ier );
 

    if ( strcmp (areaType, areaType2) == 0 
	&& strcmp (fcstHr, fcstHr2) == 0
	&& strcmp (tagDesk, tagDesk2) == 0
	&& atoi(cycle) == atoi(cycle2)
	&& strcmp (status, status2) == 0 
	&& (strcmp(rg, "")==0 || strcmp(rg2, "")==0 || strcmp(rg, rg2)==0)
	
	&& (strcmp(type, "")==0 || strcmp(type2, "")==0 || strcmp(type, type2)==0)
	&& (strcmp(top, "")==0 || strcmp(top2, "")==0 || strcmp(top, top2)==0)
	&& (strcmp(bottom, "")==0 || strcmp(bottom2, "")==0 || strcmp(bottom, bottom2)==0)
	&& (strcmp(category, "")==0 || strcmp(category2, "")==0 || strcmp(category, category2)==0) 
	&& (strcmp(frequency, "")==0 || strcmp(frequency2, "")==0 || strcmp(frequency, frequency2)==0)
	&& (strcmp(contour, "")==0 || strcmp(contour2, "")==0 || strcmp(contour, contour2)==0)
	&& (strcmp(level, "")==0 || strcmp(level2, "")==0 || strcmp(level, level2)==0)
	&& (strcmp(fzlRange, "")==0 || strcmp(fzlRange2, "")==0 || strcmp(fzlRange, fzlRange2)==0)
	&& (strcmp(intensity, "")==0 || strcmp(intensity2, "")==0 || strcmp(intensity, intensity2)==0)
	&& (strcmp(speed, "")==0 || strcmp(speed2, "")==0 || strcmp(speed, speed2)==0)
	&& (strcmp(dueto, "")==0 || strcmp(dueto2, "")==0 || strcmp(dueto, dueto2)==0)
	&& (strcmp(severity, "")==0 || strcmp(severity2, "")==0 || strcmp (severity, severity2)==0)
	&& (strcmp(coverage, "")==0 || strcmp(coverage2, "")==0 || strcmp (coverage, coverage2)==0)
	&& (strcmp(fzlTop, "")==0 || strcmp(fzlTop2, "")==0 || strcmp(fzlTop, fzlTop2)==0)
	&& (strcmp(fzlBottom, "")==0 || strcmp(fzlBottom2, "")==0 || strcmp(fzlBottom, fzlBottom2)==0)

	&& strcmp (areas, areas2) == 0
	&& strcmp (states, states2) == 0
	&& strcmp (condsBegin, condsBegin2) == 0 
	&& strcmp (condsEnd, condsEnd2) == 0
	&& atoi(lineWidth) == atoi(lineWidth2) 
//	&& strcmp (txtLayout, txtLayout2) == 0 //diff settings table
//	&& fabs( atoi(arrow_lat) - atoi(arrow_lat2)) < MIN_DIFF ?
//	&& fabs( atoi(arrow_lon) - atoi(arrow_lon2)) < MIN_DIFF
//	&& fabs( lat - lat2) < MIN_DIFF //str
//	&& fabs( lon - lon2) < MIN_DIFF
	&& fabs( atoi(lat) - atoi(lat2)) < MIN_DIFF
	&& fabs( atoi(lon) - atoi(lon2)) < MIN_DIFF
//	&& strcmp (subType, subType2) == 0  //eg. 151=152=155 153=154=156
	&& strcmp (linelm, linelm2) == 0
	&& strcmp (linetype, linetype2) == 0
	/*&& atoi(arrowSize) == atoi(arrowSize2)
	&& atoi(txtSize) == atoi(txtSize2)
	&& strcmp (txtColor, txtColor2) == 0
	&& strcmp (txtFont, txtFont2) == 0 
	&& strcmp (txtHardware, txtHardware2) == 0
	&& atoi(txtWidth) == atoi(txtWidth2)
	&& strcmp (txtAlign, txtAlign2) == 0 */
	
	) {
   
	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared. ");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

Bool compareAsh(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
//printf("linwid1 %f\n", el1.elem.ash.info.linwid);
//printf("linwid2 %f\n", el2.elem.ash.info.linwid);
    if ( el1.elem.ash.info.npts == el2.elem.ash.info.npts 
	&& el1.elem.ash.info.subtype == el2.elem.ash.info.subtype
	&& el1.elem.ash.info.distance == el2.elem.ash.info.distance
	&& el1.elem.ash.info.fhr == el2.elem.ash.info.fhr
	/*&& el1.elem.ash.info.lintyp == el2.elem.ash.info.lintyp
	//&& el1.elem.ash.info.sol == el2.elem.ash.info.sol N/A*/
	//&& el1.elem.ash.info.linwid == el2.elem.ash.info.linwid //linwid both 0.0, but can't compare. no field.
	&& el1.elem.ash.info.spd == el2.elem.ash.info.spd
	&& strcmp (el1.elem.ash.info.dir, el2.elem.ash.info.dir) ==0
	&& strcmp (el1.elem.ash.info.flvl1, el2.elem.ash.info.flvl1) ==0
	&& strcmp (el1.elem.ash.info.flvl2, el2.elem.ash.info.flvl2) ==0
	&& fabs(el1.elem.ash.spt.info.lat - el2.elem.ash.spt.info.lat) < MIN_DIFF
	&& fabs(el1.elem.ash.spt.info.lon - el2.elem.ash.spt.info.lon) < MIN_DIFF
	&& strcmp (el1.elem.ash.spt.text, el2.elem.ash.spt.text) ==0
	) {

	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}
Bool compareVol(VG_DBStruct el1, VG_DBStruct el2, int ne1) {
    if ( strcmp (el1.elem.vol.info.name, el2.elem.vol.info.name) ==0	
	&& el1.elem.vol.info.code == el2.elem.vol.info.code
	&& el1.elem.vol.info.size == el2.elem.vol.info.size
	&& el1.elem.vol.info.width == el2.elem.vol.info.width
	&& strcmp (el1.elem.vol.info.number, el2.elem.vol.info.number) ==0
	&& strcmp (el1.elem.vol.info.location, el2.elem.vol.info.location) ==0
	&& strcmp (el1.elem.vol.info.area, el2.elem.vol.info.area) ==0
	&& strcmp (el1.elem.vol.info.origstn, el2.elem.vol.info.origstn) ==0
	&& strcmp (el1.elem.vol.info.vaac, el2.elem.vol.info.vaac) ==0
	&& strcmp (el1.elem.vol.info.wmoid, el2.elem.vol.info.wmoid) ==0
	&& strcmp (el1.elem.vol.info.hdrnum, el2.elem.vol.info.hdrnum) ==0
	&& strcmp (el1.elem.vol.info.elev, el2.elem.vol.info.elev) ==0
	&& strcmp (el1.elem.vol.info.year, el2.elem.vol.info.year) ==0
	&& strcmp (el1.elem.vol.info.advnum, el2.elem.vol.info.advnum) ==0
	&& strcmp (el1.elem.vol.info.corr, el2.elem.vol.info.corr) ==0
	&& strcmp (el1.elem.vol.info.infosorc, el2.elem.vol.info.infosorc) ==0
	&& strcmp (el1.elem.vol.info.addlsorc, el2.elem.vol.info.addlsorc) ==0
	&& strcmp (el1.elem.vol.info.avcc, el2.elem.vol.info.avcc) ==0
	&& strcmp (el1.elem.vol.info.details, el2.elem.vol.info.details) ==0
	&& strcmp (el1.elem.vol.info.obsdate, el2.elem.vol.info.obsdate) ==0
	&& strcmp (el1.elem.vol.info.obstime, el2.elem.vol.info.obstime) ==0
	&& strcmp (el1.elem.vol.info.obsashcld, el2.elem.vol.info.obsashcld) ==0
	&& strcmp (el1.elem.vol.info.fcst_06, el2.elem.vol.info.fcst_06) ==0
	&& strcmp (el1.elem.vol.info.fcst_12, el2.elem.vol.info.fcst_12) ==0
	&& strcmp (el1.elem.vol.info.fcst_18, el2.elem.vol.info.fcst_18) ==0
	&& strcmp (el1.elem.vol.info.remarks, el2.elem.vol.info.remarks) ==0
	&& strcmp (el1.elem.vol.info.nextadv, el2.elem.vol.info.nextadv) ==0
	&& strcmp (el1.elem.vol.info.fcstrs, el2.elem.vol.info.fcstrs) ==0
	&& fabs(el1.elem.vol.latlon[0] - el2.elem.vol.latlon[0]) < MIN_DIFF
	&& fabs(el1.elem.vol.latlon[1] - el2.elem.vol.latlon[1]) < MIN_DIFF
	&& el1.elem.vol.offset_xy[0] == el2.elem.vol.offset_xy[0]
	&& el1.elem.vol.offset_xy[1] == el2.elem.vol.offset_xy[1]
	) {

	sprintf(buffer, "%s %d %s %s", "The element", (totEleNum-ne1+1), getVgType(el1.hdr.vg_type), "in the first file is compared.");
	printf("%s\n", buffer);

	return TRUE;
    }
    else
	return FALSE;
}

char* getVgType(int vg_type) {
    char* vgType;

    if (vg_type == 1)
	vgType = "Line";
    else if (vg_type == 2)
	vgType = "Front";
    else if (vg_type == 20)
	vgType = "SpLine";
    else if (vg_type == 6)
	vgType = "WatchBox";
    else if (vg_type == 21)
	vgType = "SpText";
    else if (vg_type == 4)
	vgType = "Circle";
    else if (vg_type == 26)
	vgType = "Track";
    else if (vg_type == 34)
	vgType = "List";
    else if (vg_type == 35)
	vgType = "Vocano";
    else if (vg_type == 36)
	vgType = "Ash";
    else if (vg_type == 37)
	vgType = "Jet";
    else if (vg_type == 38)
	vgType = "Gfa";
    else if (vg_type == 39)
	vgType = "Tca";
    else if (vg_type >= 27 && vg_type <= 31)
	vgType = "Sigmet";
    else if (vg_type == 5 || vg_type >= 10 ||vg_type <= 16)
	vgType = "Symbol";
    else if (vg_type == 19)
	vgType = "Mark";
    else if (vg_type == 25)
	vgType = "Combo";
    else if (vg_type == 8 || vg_type == 9 || vg_type == 23 || vg_type == 24)
	vgType = "Vector";
    else 
	vgType = (char *)vg_type;

    return vgType;
}

Bool checkSztext (float sztext1, float sztext2) {
    
    if ( (fabs(sztext1 -0.714) <0.0001 && sztext2 <0.8)
	|| (fabs(sztext1 -0.857) <0.0001 && sztext2 >=0.8 && sztext2 <=0.9)
	|| (fabs(sztext1 -1.0)   <0.0001 && sztext2 >0.9 && sztext2 <=1.15)
	|| (fabs(sztext1 -1.286) <0.0001 && sztext2 >1.15 && sztext2 <=1.4)
	|| (fabs(sztext1 -1.714) <0.0001 && sztext2 >1.4 && sztext2 <=1.9)
	|| (fabs(sztext1 -2.429) <0.0001 && sztext2 >1.9) 
	|| (fabs(sztext2 -0.714) <0.0001 && sztext1 <0.8)
	|| (fabs(sztext2 -0.857) <0.0001 && sztext1 >=0.8 && sztext1 <=0.9)
	|| (fabs(sztext2 -1.0)   <0.0001 && sztext1 >0.9 && sztext1 <=1.15)
	|| (fabs(sztext2 -1.286) <0.0001 && sztext1 >1.15 && sztext1 <=1.4)
	|| (fabs(sztext2 -1.714) <0.0001 && sztext1 >1.4 && sztext1 <=1.9)
	|| (fabs(sztext2 -2.429) <0.0001 && sztext1 >1.9) )

	return TRUE;
    else 
	return FALSE;
}

/*void ListDelete(item2 **listP, VG_DBStruct value)
{
  item2 *currP, *prevP;
  // For 1st node, indicate there is no previous. 
  prevP = NULL;
  
   // Visit each node, maintaining a pointer to
   // the previous node we just visited.
   
  for (currP = *listP;
        currP != NULL;
        prevP = currP, currP = currP->next) {

    if (&(currP->el) == &value) {  // Found it. 
      if (prevP == NULL) {
        // Fix beginning pointer. 
        *listP = currP->next;
      } else {
        
         // Fix previous node's next to
         // skip over the removed node.
         
        prevP->next = currP->next;
      }
      // Deallocate the node. 
      free(currP);

      // Done searching. 
      return;
    }
  }
}*/

