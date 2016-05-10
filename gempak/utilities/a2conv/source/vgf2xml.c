#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"

/*
 *---------------------------------------------------------------------*
 *          Functions used by vgfToXml from other libraries            *
 *---------------------------------------------------------------------*
 */
void cvg_openj ( char *filnam, int wrtflg, FILE **fptr, int *iret );
void cvg_rdjrecnoc ( char *fname, FILE *fptr, int fpos, VG_DBStruct *el, int *iret );
int  cvg_v2x ( VG_DBStruct *el, char *buffer, int *iret );
void cgr_segdist ( int *np, float *xx, float *yy, float *fx, float *fy,
     float *distance, int *nearest_vrt, int *next_vrt, float *nx, float *ny, int *iret );


/*
 *---------------------------------------------------------------------*
 *          Private functions and macros defined by vgfToXml              *
 *---------------------------------------------------------------------*
 */ 
 
struct list_el {
   VG_DBStruct el;
   struct list_el * next;
};
typedef struct list_el item1;

void insert(item1 ** head, int sd);

item1 *llist_bubble_sort(item1 *curr2);
item1 *llist_bubble_sort_cloud_turb(item1 *curr2);
item1 *llist_bubble_sort_contour_group(item1 *curr2);
item1 *llist_bubble_sort_contour_color(item1 *curr2);

char  *getGroupType(int grptyp);
char  *getSymType(VG_DBStruct *el);
void getMatchContAttr(char *tblArray[], int *grpColor, char* contArray[], int tblArrayLen, int *ier);
void getMatchGrptyp(char *lineGrptyp[], int *grptyp, int lineGrptypLen, char* grptypArray[], int *ier);

void convertElm(item1 *curr2, char *buffer, FILE *ofptr, char *tblArray[], int tblArrayLen);
void getCcf(VG_DBStruct *el, char *buffer, int *ier);
void getWatch(VG_DBStruct *el, char *buffer, int *grpNum, int *ier);
void getContour(VG_DBStruct *el, char *buffer, int *contText, int *grpNum, int *grpColor, char *tblArray[], int tblArrayLen, int *ier);
void getSymLab(VG_DBStruct *el, char *buffer, int *symText, int *grpNum, int *ier);
void getOutlooks(VG_DBStruct *el, char *buffer, char* outType, int *outlookText, int *grpNum, int *ier);
void getCommonCollection(VG_DBStruct *el, char *buffer, int *commonText, int *grpNum, int *ier);
void getCloud(VG_DBStruct *el, char *buffer, int *cloudText, int *grpNum, int *grpInit, int *ier);
void getTurb(VG_DBStruct *el, char *buffer, int *turbText, int *grpNum, int *grpInit, int *ier);
void getVolc(VG_DBStruct *el, char *buffer, int *vol, int *ashFhr, int *ier);
void writeEndingCollection(VG_DBStruct *el, FILE *ofptr, int *grpNum, int *grpInit, int *grpColor, int *contText, int *symText, int *outlookText, int *commonText, int *cloudText, int *turbText, int *hasSameElm, int *ier);
void writeEndingAsh(VG_DBStruct *el, FILE *ofptr, int *ier);

int isSymbolLike(int vg_type);
int isSymbolMarkLike(int vg_type);
int isLineLike(int vg_type);
int isOutlookLineLike(int vg_type);
int isOutlookLike(int grp_type);

/************************************************************************
 * vgf2xml.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/
/*
 *---------------------------------------------------------------------*
 *                   Public interface for vgfToXML                     *
 *---------------------------------------------------------------------*
 */

/*=====================================================================*/

int vgfToXml(char *vgfn, char *asfn, char *activity, char *subActivity, char *site,
             char *forecaster, char *status, char *contTbl)

/********************************************************************************
 * vgfToXml									*
 *										*
 * This program is a re-write of VGF2TAG. It converts a file of pgen elements 	*
 * in VGF format into an XML file.						*
 *										*
 *										*
 * Log: 									*
 * Q.Zhou /Chug         01/10   modified from vgf2tag to generate xml   	*
 * Q.Zhou /Chug         06/10   Added tag handling for collections      	*
 * Q.Zhou /Chug         09/10   Added method to read all els to a list. 	*
 *                              Added method to sort the list           	*
 * Q.Zhou /Chug         10/10   Increased sort conditions for Cloud grp 	*
 *                              Added Cloud, Outlook, volcano, ccfp...  	*
 * Q.Zhou /Chug         12/10   Handle grptyp 5,6,8 to contour, symLabel	*
 *                              Symbol H,L with grptyp 5,6 are contour  	*
 * Q.Zhou /Chug         12/10   Added sort for contour w. grouped lines 	*
 * Q.Zhou /Chug         02/11   Handled contour line & text n*n match   	*
 *				Add common collection for cave non handled grps *
 * Q.Zhou /Chug         02/11   Added/modified watch box ending tag     	*
 *				Modified common collection for cloud, outlook.. * 
 * Q.Zhou /Chug         04/11   Added grptyp 9(Tropical),27(Isobars) to contours*
 *                              Refined contour, outlook, common collection rule*
 * Q.Zhou /Chug		11/11   Added parameter activity and subActivity.       *
 *                              Changed CCF to add collection and contents      *
 * Q.Zhou /Chug		12/11   Added contour attrubutes. Added read from table *
 * Q.Zhou /Chug		12/11   Handled vgf delete situation                    *
 *                              Handled group with single symbol -- normal DE   *
 * Q.Zhou /Chug		03/12   Added grptyp & contour to the commandline table *
 * Q.Zhou /Chug         06/12   Added head only vgf conversion                  *
 * Q.Zhou /Chug         08/12   Sigmet line width unit changed                  *
 * Q.Zhou /SGT          09/13   Fixed converting symbol without text to contours*
 * Q.Zhou /SGT          02/14   Sort contours by color      			*   
 * Q.Zhou /SGT          02/14   Remove grouped single line/text out of contours *
 * J. Wu  /SGT          08/14   Convert time in "Contours" into proper format   *
 * J. Wu  /SGT          02/15   Convert Gempak time in format "YYYYMMDD/HH"  	*
 * J. Wu  /SGT          03/15   R6872-add site/forecaster/status in command line*
 ********************************************************************************/
{
    int		nw, more, ier;
    int		wrtflg, curpos;
    VG_DBStruct	el ; 
    char	buffer[10000]; 
    char	infile[128], ifname[128];
    long	ifilesize;
    FILE	*ifptr, *ofptr;

    item1       *curr2, *head1, *head2; /* head1=begin, head2=end */

/*---------------------------------------------------------------------*/

    /*
     *  Opening the input VGF file  
     */
    wrtflg = 0;
    cvg_openj ( vgfn, wrtflg, &(ifptr), &ier );

    if ( ier != 0 )  {
        printf("Error opening VGF file %s\n", infile );
        exit (0);
    }

    cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );

    ofptr = cfl_wopn ( asfn, &ier );

    nw = 0;
    more = G_TRUE;
    curpos = 0;
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
   
    
    /*
     * Get productType string - activity(subActivity). 
     */
    char *productType;
    productType = (char *)malloc(128);
   

    if (activity != NULL && strlen(activity) != 0 && strcmp(activity, "Default")!=0 && strcmp(activity, "default")!=0) {
	
        if (subActivity != NULL && strlen(subActivity) != 0 
		&& strcmp(subActivity, "Default")!=0 && strcmp(subActivity, "default")!=0) {
	    sprintf(productType, "%s%s%s%s", activity, "(", subActivity, ")");
	}
        else {
	    productType = activity;           
        }
    }
    else {
	productType = "Default";
    }   
   
    /*
     *  Load the specified table - This is used for converting Contours or
     *  converting a group type 8 (LABEL) into a CAVE outlook element - normally
     *  a group type 8 will be converted into Contours element in CAVE. 
     *  
     *  If a VGF file contains grouped lines and labels, they will be checked for 
     *  group type against this table. If a match is found, the matching information 
     *  will be used to perform the conversion. 
     *
     *  groupIndicator		groupType	convertTo	convertedTypeName
     *  group				8	outlook		FLOOD
     *
     * Contour conversion information
     * To get contour parameter, level, forecast hour ... according to the contour line color
     * 
     * If a VGF file contains contour lines, they will be checked against the contour 
     * color on this table. If a match is found, the information from the rest of the 
     * row will be used to convert into an XML file.
     * 
     * color  	param   level1    level2    fcstHr  cint 		time1    time2
     * 5  	HGHT    1000        -1      f000    60/0/100	20111213/1200    -1
     * 6     	HGHT    1000       500      f024     6/0/100	20111213/1200    -1
     * 2     	TEMP     850        -1      f018     3/0/100	20111213/1200    -1
     * 3     	PRES       0        -1      f012     4/0/100	20111213/1200 	 -1
     *  
     */
    char *tblArray[64];   //store vgfConvert.tbl info
    
    int i = 0;
    char fname[128], contTitle[256]=""; 
    FILE *fptr;
    long filesize;
    int iret; 
       
    if (contTbl != NULL && strcmp(contTbl, "") != 0) {
	cfl_inqr ( contTbl, NULL, &filesize, fname, &iret );
	if ( iret == 0 ) {	
    	    fptr = (FILE *) cfl_ropn(fname, "", &iret);

    	    if (iret == 0) {
	        while (!feof ( fptr ) ) {

    	    	    cfl_rdln( fptr, sizeof(contTitle), contTitle, &iret );

		    if ( iret == 0 && contTitle[0] != '!') {
		       tblArray[i] = (char*)malloc(sizeof(contTitle));
		       strcpy(tblArray[i], contTitle);		
		    
		       i++;
		    } 
	        }
	    }
	    
	    fclose(fptr);
    	}
	else {
            printf( "Warning: cannot find %s in current directory!\n", fname );	
	}
    }

    /*
     * Get activity site, forecaster, and status. 
     */
    char *asite, *aforecaster, *astatus;
    asite = (char *)malloc(128);
    aforecaster = (char *)malloc(128);
    astatus = (char *)malloc(128);   

    if (site != NULL && strlen(site) != 0 ) {
        strcpy( asite, site );
    }
    else {
        asite = "Default";
    }

    if (forecaster != NULL && strlen(forecaster) != 0 ) {
        strcpy( aforecaster, forecaster );
    }
    else {
        forecaster = "Default";
    }

    if (status != NULL && strlen(status) != 0 ) {
        strcpy( astatus, status );
    }
    else {
        astatus = "UNKNOWN";
    }

    /*
     * Read VG file header and write out an XML header. 
     */
    cvg_rdjrecnoc( ifname, ifptr, curpos, &el, &ier );

    if (el.hdr.vg_type == FILEHEAD_ELM) {   
	
    	sprintf( buffer, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n  <Products xmlns:ns2=\"http://www.example.org/productType\">\n    <Product name=\"%s\" type=\"%s\" forecaster=\"%s\" center=\"%s\" status=\"%s\" onOff=\"true\" useFile=\"true\" filePath=\"\" fileName=\"\">\n      <Layer name=\"Default\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n", productType, productType, aforecaster, asite, astatus);
	
    	cfl_writ ( ofptr, (int)strlen(buffer), (unsigned char*)buffer, &ier );
   	curpos += el.hdr.recsz;  //424
	//can't  free (productType);
    }

    /*
     * Read all other elements in the VG file 
     */
    head1 = NULL;
    head2 = NULL;
    curr2 = NULL;

    while ( nw < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
	cvg_rdjrecnoc( ifname, ifptr, curpos, &el, &ier );

	if ( ier < 0 )  {
            more = G_FALSE;
	}
	else {
	    if (el.hdr.vg_type != FILEHEAD_ELM && el.hdr.delete ==0) {
	    	curpos += el.hdr.recsz;
	    	curr2 = (item1 *)malloc(sizeof(item1));     		
	    	curr2->el =el;

	    	//add curr2 to head1 head2 list
		if (head1 == NULL) {
    		    head1 = head2 = curr2;
    		    head1->next = NULL;
		}
		else {	
	    	    head2->next = curr2;
	    	    head2 = curr2;
	    	    head2->next = NULL;
		}
	    }
	    else if (el.hdr.vg_type == FILEHEAD_ELM && el.hdr.delete ==0) { //second FILEHEAD, stop
		break;
	    }
	    else {          // don't read if delete !=0
		curpos += el.hdr.recsz;
	    }
      	}

 	nw++;			
    }

    
    /*
     *  If there is only a header in the VG file, end the XML file.
     *  Otherwise, sort the list of VG elements.
     */

    if ( nw <= 1 ) {
        sprintf( buffer, "        </DrawableElement>\n      </Layer>\n    </Product>\n  </Products>\n");

    	cfl_writ ( ofptr, (int)strlen(buffer), (unsigned char*)buffer, &ier );
    	printf("The vgf file %s has no Pgen content.\n", vgfn);
    	fclose(ofptr);

    	return ( 0 );
    }
    else {

	/* sort on grptyp, grpnum, vg_type... */
     	head1 = llist_bubble_sort(head1); 
    }

    /*
     *  Convert VG element list into CAVE elements in XML.
     */
    curr2 = head1;
    convertElm(curr2, buffer, ofptr, tblArray, i);
  
    /*
     * free spaces
     */
    int j = 0;
    for (j=0; j<i; j++)
	free (tblArray[j]);

    //can't free(curr2);
    fclose(ofptr);
    
    return ( 0 );

}



/*----------------------------------------------------------------------*
 *                   Private functions for vgfToXml                     *
 *----------------------------------------------------------------------*
 */
 
void convertElm(item1 *curr2, char *buffer, FILE *ofptr, char *tblArray[], int tblArrayLen)
/********************************************************************************
 * convertElm									*
 *										*
 * Subroutine to convert a sorted list of VG elements into CAVE elements in XML.*
 *										*
 ********************************************************************************/
{
    int ier;
    int nw = 0;
    char *xml_end = "";   /* last few tags */
    int hasContour = 0;   /* =1 means has contours */
    int hasMultiLine = 0; /* =1 means contour lines have same grptyp and grpnum; */
    int hasSameElm = 1;   /* =1 means there are only lines, or texts, or makers. =0 means mixed */

    item1 *head;

    int contText = 0;/* contour text flag. contText =1, line drawed. =2, text drawed; =3, sym drawed */
    int symText = 0;    /* symbol label flag */  
    int outlookText = 0;/* outlook text flag. outlookText =1, line drawed */
    int commonText = 0; /* common collection label flag */  
    int cloudText = 0;  /* cloud flag. cloudText =1, line drawed */
    int turbText = 0;   /* turb flag. turbText =1, line drawed */

    int grpNum = 0;  /* first collection indicator, associate with grpnum */
    int grpColor =0; /* for contours. If grpColor incr. then contour number incr.*/
    int grpInit = 0; /* for cloud, turb. sub collection indicator, inside one grpnum. */
    int vol = 0;     /* volcano flag */
    int ashFhr = -1; /* ash flag */
    
    char *lineGrptyp[32]; 	//store only the grptyp rows in vgfConvert.tbl
    char* grptypArray[3] = {"0", "default", "default"}; //equivalent to one row of lineGrptyp
   	
    /* 
     * lineGrptyp is used for first part of the vgfConvert.tbl 
     * For example, get lineGrptyp for converting groupType to FLOOD
     */
    int i=0, j=0;
    if (tblArrayLen != 0) {
    	for (i=0; i<tblArrayLen; i++) {

	    if (tblArray[i][0] == 'g') {
	    	lineGrptyp[j] = (char*)malloc(strlen(tblArray[i])+1);
	    	strcpy(lineGrptyp[j], tblArray[i]);	
	
	    	j++;
	    }	
	}
    }

    /* 
     * Check the list for hasContour, hasMultiLine, hasSameElm
     */
    /* check if it has contour: grptyp=8 and have line+tex in 1 group */
    head = curr2;    
    while (curr2) {
 	/* find matching grptyp from vgfConvert.tbl. 
	 * grptypArray[0]=grptyp, grptypArray[1]=convertToType, grptypArray[2]=pgenType. grptypArray default to 0, default, default.
	 * Need to check convertTo type grptypArray[1] against 'default' 
	 */
	int gt = curr2->el.hdr.grptyp;
	if ( j != 0 && gt >0 && curr2->el.hdr.grpnum > 0 )
    	     getMatchGrptyp(lineGrptyp, &gt, j, grptypArray, &ier);

	if (curr2->el.hdr.grptyp ==8 && strcmp (grptypArray[1], "default") ==0
	    && isLineLike(curr2->el.hdr.vg_type) == 1
	    && curr2->next != NULL && curr2->next->el.hdr.grptyp ==8 
	    && curr2->next->el.hdr.vg_type == SPTX_ELM
	    && strlen(curr2->next->el.elem.spt.text) == strspn(curr2->next->el.elem.spt.text, "0123456789.+-")
	    && curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum ) {
		
	    	hasContour = 1;
		grpColor = curr2->el.hdr.maj_col;
		printf("+ Contours founded.\n");
	    	break;
	}

	/* no need to find ourlook
	if (isOutlookLike(curr2-> el.hdr.grptyp) ==1
	    && isOutlookLineLike(curr2->el.hdr.vg_type) == 1
	    && curr2->next != NULL && curr2->el.hdr.grptyp == curr2->next->el.hdr.grptyp 
	    && curr2->next->el.hdr.vg_type == SPTX_ELM
	    && curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum ) {
		
	    	hasOutlook = 1; //not used
                printf("Outlooks founded.\n" );
	    	break;
	}*/
	curr2 = curr2->next;	
    }


    /* check if contour lines are grouped together */
    curr2 = head;
    while (curr2) {
	
	if ((curr2->el.hdr.grptyp ==8 ) 
 	    && isLineLike(curr2->el.hdr.vg_type) ==1
	    && curr2->next != NULL && isLineLike(curr2->next->el.hdr.vg_type) ==1
	    && curr2->next->el.hdr.grptyp == curr2->el.hdr.grptyp
	    && curr2->next->el.hdr.grpnum == curr2->el.hdr.grpnum ) {

	    hasMultiLine = 1;
	    break;
	}
	
	curr2 = curr2->next;
    }

    /* check if grptyp>0, but only texts, only lines. This is not a contour. */
    curr2 = head;
    while (curr2 ) {
	if (curr2->el.hdr.vg_type == VOLC_ELM || curr2->el.hdr.vg_type == ASHCLD_ELM
	    || curr2->el.hdr.vg_type == WBOX_ELM || curr2->el.hdr.vg_type == SIGCCF_ELM	    
	    || (curr2->next != NULL //&& curr2->el.hdr.grptyp >0
		//&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
		&& curr2->el.hdr.vg_type != curr2->next->el.hdr.vg_type)) {
	   
	    hasSameElm = 0; 
	    break;
	}
		
	curr2 = curr2->next;
    }
    

    /* 
     * Further modifications and sort on the list
     */

	/* For contours, Assign min_col in each grpnum to its LINE's maj_col. 
	 * So to put same colored groups together. Don't do symbol */ 

	int grpcol = 0;
        int grpnum = 0;
    	curr2 = head;

	while (curr2) {
	    if (curr2->el.hdr.grptyp == 8 ) {
          
		if (isLineLike(curr2->el.hdr.vg_type) == 1 ) { 		    
		    grpcol = curr2->el.hdr.maj_col;
	        }

		curr2->el.hdr.min_col = grpcol;
		
	    }
	    curr2 = curr2->next;
	}

	/* For contour, If a group has only line, only synbol, or only text, change to non group, grptyp=0 */
	curr2 = head;
	while (curr2) {

	    if ((curr2->el.hdr.grptyp == 8 || curr2->el.hdr.grptyp == 5 
		    || curr2->el.hdr.grptyp == 6 || curr2->el.hdr.grptyp == 9)
		&& hasContour != 0
		) {
		
		if (isLineLike(curr2->el.hdr.vg_type) == 1 || isSymbolMarkLike(curr2->el.hdr.vg_type) == 1) {
		    grpnum = curr2->el.hdr.grpnum;		    
	        }

		if ((isLineLike(curr2->el.hdr.vg_type) == 1 && curr2->next == NULL) // only line
		    || (isLineLike(curr2->el.hdr.vg_type) == 1  
			&& curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum) ) {
 		   
		    curr2->el.hdr.grpnum = 0;
		    curr2->el.hdr.grptyp = 0;
	        }
		else if ((isSymbolMarkLike(curr2->el.hdr.vg_type) == 1 && curr2->next == NULL) // only symbol
		    ||(isSymbolMarkLike(curr2->el.hdr.vg_type) == 1 
 		    	&& curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum) 
		    || (isSymbolMarkLike(curr2->el.hdr.vg_type) == 1 && curr2->next->el.hdr.vg_type != 21) ) { //sym could be 1			

		    curr2->el.hdr.grpnum = 0;
		    curr2->el.hdr.grptyp = 0;
	        }
		else if (curr2->el.hdr.vg_type == 21 && curr2->el.hdr.grpnum != grpnum) { // only text

		    curr2->el.hdr.grpnum = 0;
		    curr2->el.hdr.grptyp = 0;
	        }

	    }
	    curr2 = curr2->next;
	}

    /* Sort cloud and turb further more. Also move grptyp=0 elements got from contour to top */
    curr2 = head; 
    head = llist_bubble_sort_cloud_turb(head); 

    /* For contours: sort by min_col */
    curr2 = head;
    head = llist_bubble_sort_contour_color(head);

    /* For contours, sort to line, text, line, text in same groupNum */
    if (hasMultiLine ==1 && hasSameElm ==0) {
	
	head = llist_bubble_sort_contour_group(head);
    	
    }

    /* test sorting */
    /*    curr2 = head;
	while (curr2) {
	    printf("After sort: %d %d %d\n", curr2->el.hdr.vg_type, curr2->el.hdr.grptyp, curr2->el.hdr.grpnum);
	    
            curr2 = curr2->next;
	}
     */  	


    /* 
     * write beginning tag of the xml 
     */

    curr2 = head;    
    while (curr2) {
 	if ( curr2->el.hdr.recsz > 0 )  {
	    /* find matching grptyp from vgfConvert.tbl. 
		grptypArray[0]=grptyp, grptypArray[1]=convertToType, grptypArray[2]=pgenType. grptypArray default to 0, default, default.
		Need to check convertTo type grptypArray[1] against 'default' */
	    int gt = curr2->el.hdr.grptyp;
	    if (j != 0 && gt >0 && curr2->el.hdr.grpnum >0)
    	    	getMatchGrptyp(lineGrptyp, &gt, j, grptypArray, &ier);


	    /* grptyp>0, and there are only lines,  
	    if (hasSameElm ==1 && curr2->el.hdr.grptyp > 0 && isOutlookLineLike(curr2->el.hdr.vg_type) == 1) { 
                getContour(&(curr2->el), buffer, &contText, &grpNum, &grpColor, tblArray, tblArrayLen, &ier); 
  				
	    }*/

	    /* grptyp>0, but there are only lines, or texts, or one kind elements */
            if (hasSameElm ==1)
               cvg_v2x ( &(curr2->el), buffer, &ier );   
 
	    /* Watch. initial grptyp & grpnum =0. GRPTYP_WATCH=98 defined in pgprm.h. */  
            else if ( curr2->el.hdr.grptyp == GRPTYP_WATCH || curr2->el.hdr.vg_type == WBOX_ELM) 
		getWatch(&(curr2->el), buffer, &grpNum, &ier);

	    /* volcano layer */
	    else if (curr2->el.hdr.vg_type == VOLC_ELM || curr2->el.hdr.vg_type == ASHCLD_ELM ) 
		getVolc(&(curr2->el), buffer, &vol, &ashFhr, &ier);
	 		   
	    /* CCF */
	    else if ( curr2->el.hdr.vg_type == SIGCCF_ELM) {
		getCcf(&(curr2->el), buffer, &ier);}

	    /* grptyp =0 and not collection  //curr2->el.hdr.grpnum <= 0*/
	    else if ( curr2->el.hdr.grptyp == 0 || curr2->el.hdr.vg_type == GFA_ELM
		|| curr2->el.hdr.vg_type == TCA_ELM )
		cvg_v2x ( &(curr2->el), buffer, &ier ); 

	    /* Cloud group */
	    else if (curr2->el.hdr.grptyp == 1 || atoi(grptypArray[0]) == 1) {
		// only cloud lines in a group
		
		if (cloudText != 0 //curr2->el.hdr.vg_type == SPTX_ELM 
		    || (curr2->el.hdr.vg_type == SPLN_ELM && curr2->el.elem.spl.info.spltyp == 3)
		    || (curr2->el.hdr.vg_type == SPLN_ELM && curr2->el.elem.spl.info.spltyp == 4 
			&& cloudText != 0)) {
		    getCloud(&(curr2->el), buffer, &cloudText, &grpNum, &grpInit, &ier);
		}
		else {
		    printf("+ Unexpected contents for group type CLOUD. Converting to default collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		}
	    }
		
	    /* Turb group */
	    else if (curr2->el.hdr.grptyp == 2 || atoi(grptypArray[0]) == 2) {
		if (turbText != 0 //curr2->el.hdr.vg_type == SPTX_ELM 
		    || curr2->el.hdr.vg_type == LINE_ELM 
		    || (curr2->el.hdr.vg_type == SPLN_ELM && curr2->el.elem.spl.info.spltyp ==4 
			&& turbText !=0)) {

		    getTurb(&(curr2->el), buffer, &turbText, &grpNum, &grpInit, &ier);
		}
		else {
		    printf("+ Unexpected contents for group type TURB. Converting to default collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		}
	    }

	    /* High, Low or Tropical group */ 
	    else if (curr2->el.hdr.grptyp == 5 || atoi(grptypArray[0]) == 5 
			|| curr2->el.hdr.grptyp == 6 || atoi(grptypArray[0]) == 6
		    	|| curr2->el.hdr.grptyp == 9 || atoi(grptypArray[0]) == 9) {	
	

	 	if ((hasContour !=0 && curr2->next != NULL 
			&& curr2->next->el.hdr.vg_type == SPTX_ELM
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
			&& strlen(curr2->next->el.elem.spt.text) == strspn(curr2->next->el.elem.spt.text, "0123456789.+-")
		    	&& isSymbolMarkLike(curr2->el.hdr.vg_type) ==1)
		    || contText != 0) {     /* last condition is for text or sym */
 //printf("Cont2 %d",hasContour);	 	
		    getContour(&(curr2->el), buffer, &contText, &grpNum, &grpColor, tblArray, tblArrayLen, &ier);
		}
		else if (isSymbolMarkLike(curr2->el.hdr.vg_type) ==1
			&& curr2->next != NULL  
			&& curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum) {
			
		    // only symbol in the grpnum, convert to normal DE
		    cvg_v2x ( &(curr2->el), buffer, &ier ); 

		}

		else if ((hasContour ==0  && commonText ==0
			&& curr2->next != NULL 
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
			&& curr2->next->el.hdr.vg_type == SPTX_ELM
		    	&& isSymbolMarkLike(curr2->el.hdr.vg_type) ==1)
		    || symText !=0) 

		    getSymLab(&(curr2->el), buffer, &symText, &grpNum, &ier);

		
		else {
		    printf("+ Unexpected contents for group type ");
		    printf("%s", getGroupType(curr2->el.hdr.grptyp));
		    printf(". Converting to default collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		}		    
	    }


	    /* outlook group */
	    else if (isOutlookLike(curr2->el.hdr.grptyp) ==1 
		    || atoi(grptypArray[0]) == 7 || atoi(grptypArray[0]) == 9
		    || (atoi(grptypArray[0]) >=12 && atoi(grptypArray[0]) <=14)
		    || atoi(grptypArray[0]) == 16 || atoi(grptypArray[0]) == 24) {
		

		// only outlook lines in a group
		/*if (((curr2->el.hdr.vg_type == SPLN_ELM || curr2->el.hdr.vg_type == LINE_ELM) && curr2->next == NULL)
		    || ((curr2->el.hdr.vg_type == SPLN_ELM || curr2->el.hdr.vg_type == LINE_ELM) && curr2->next != NULL 
			&& ((curr2->next->el.hdr.vg_type == SPLN_ELM || curr2->next->el.hdr.vg_type == LINE_ELM)
			    || curr2->el.hdr.grptyp != curr2->next->el.hdr.grptyp 
			    || curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum))) {

		    printf("+ This outlook line is not an Outlook. Convert it to normal collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		} */
		
		if ( (isOutlookLineLike(curr2->el.hdr.vg_type) == 1
		    	&& curr2->next != NULL 
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum 
			&& (curr2->next->el.hdr.vg_type == SPTX_ELM 
			    || isSymbolMarkLike(curr2->next->el.hdr.vg_type) ==1) )
			    //|| isOutlookLineLike(curr2->el.hdr.vg_type) == 1) )?
		    	|| outlookText != 0) {
		        		           
		    	getOutlooks(&(curr2->el), buffer, grptypArray[2], &outlookText, &grpNum, &ier);		   
		}
		
		else {
		    printf("+ Unexpected contents for group type ");
		    printf("%s", getGroupType(curr2->el.hdr.grptyp));
		    printf(". Converting to default collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		}
	    }

	    /* Contour  */
 	    else if (curr2->el.hdr.grptyp == 8 || atoi(grptypArray[0]) == 8 
			|| curr2->el.hdr.grptyp == 27 || atoi(grptypArray[0]) == 27) {

		if ( ((strcmp (grptypArray[1], "outlook") ==0) && curr2->next != NULL 
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
			&& isLineLike(curr2->el.hdr.vg_type) ==1  )
		    || outlookText >0) {

		    getOutlooks(&(curr2->el), buffer, grptypArray[2], &outlookText, &grpNum, &ier);	 
		}
	    	else if ((hasContour !=0 && curr2->next != NULL 
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
			//&& curr2->next->el.hdr.vg_type == SPTX_ELM // 1 line n text
			&& strlen(curr2->next->el.elem.spt.text) == strspn(curr2->next->el.elem.spt.text, "0123456789.+-") 
		    	&& (isSymbolMarkLike(curr2->el.hdr.vg_type) ==1 || isLineLike(curr2->el.hdr.vg_type) ==1) ) 
		    || contText != 0) {      // last condition is for text, 1 or 2
		
		    getContour(&(curr2->el), buffer, &contText, &grpNum, &grpColor, tblArray, tblArrayLen, &ier); 
		}
		else if ((hasContour ==0  && commonText == 0
			&& curr2->next != NULL 
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
			&& curr2->next->el.hdr.vg_type == SPTX_ELM
		    	&& isSymbolMarkLike(curr2->el.hdr.vg_type) ==1)
		    || symText !=0) {

		    getSymLab(&(curr2->el), buffer, &symText, &grpNum, &ier); 
		}
		else if (hasContour ==0  && curr2->next != NULL //&& curr2->next != SPTX_ELM
			&& curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum
			&& (isSymbolMarkLike(curr2->el.hdr.vg_type) ==1 || isLineLike(curr2->el.hdr.vg_type) ==1)) {
			 
		    // only symbol in the grpnum, no text, convert to normal DE
		    cvg_v2x ( &(curr2->el), buffer, &ier ); 
		}
		else if (hasContour ==0  && curr2->next == NULL
			&& (isSymbolMarkLike(curr2->el.hdr.vg_type) ==1 || isLineLike(curr2->el.hdr.vg_type) ==1)) {
		    // symbol/line at end & grp>0 & no contour, convert to normal DE
		    cvg_v2x ( &(curr2->el), buffer, &ier ); 
		}
		else {
		    printf("+ Unexpected contents for group type ");
		    printf("%s", getGroupType(curr2->el.hdr.grptyp));
		    printf(". Converting to default collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		}
	    }
	
	    /* Front group */ 
	    else if (curr2->el.hdr.grptyp == 3 ) {
		if ( (curr2->el.hdr.vg_type == FRONT_ELM	
		    	&& curr2->next != NULL 
			&& curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
			&& curr2->next->el.hdr.vg_type == SPTX_ELM)
		    || symText !=0) {

		    getSymLab(&(curr2->el), buffer, &symText, &grpNum, &ier); 
		} else {
		    printf("+ Unexpected contents for group type FRONT. Converting to default collection.\n");
		    getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
		}
	    }

	    /* jet group */ 
	    else if (curr2->el.hdr.grptyp == 4 ) {
		getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
	    }


	    /* Other group */
	    else {
		printf("+ Unexpected contents for group type ");
		printf("%s", getGroupType(curr2->el.hdr.grptyp));
		printf(". Converting to default collection.\n");

		getCommonCollection(&(curr2->el), buffer, &commonText, &grpNum, &ier);
	    }

	    /* 
             * write buffer, don't write List, filehead 
             */
	    if (ier==0 && curr2->el.hdr.vg_type != LIST_ELM && curr2->el.hdr.vg_type != FILEHEAD_ELM) { 
	      
	        cfl_writ ( ofptr, (int)strlen(buffer), (unsigned char*)buffer, &ier );
		if ( ier == 0 )  nw++;
	    }

	    /* 
 	     * Finish ending tags.
	     */

	    /* Finish ending tags. Volcano and ash:  add ash upto 4 layers */
	    if (curr2->next == NULL && 
		(curr2->el.hdr.vg_type == VOLC_ELM || curr2->el.hdr.vg_type == ASHCLD_ELM)) {
		writeEndingAsh(&(curr2->el), ofptr, &ier);  
 	    }


	    /*Finishing collection ending tag when curr2->el.hdr.grptyp >0 */
	    /* Watch grptyp is 0 or 98, LIST_ELM is skipped */
	    if (( ((curr2->el.hdr.grptyp == 98 && curr2->el.hdr.vg_type == SPLN_ELM) 
			|| curr2->el.hdr.vg_type == WBOX_ELM) 
		    && curr2->next == NULL)
		|| (curr2-> el.hdr.vg_type == WBOX_ELM  && curr2->next != NULL
		    && (curr2->next-> el.hdr.vg_type == WBOX_ELM ||curr2->next-> el.hdr.vg_type == LIST_ELM
			|| curr2->el.hdr.grptyp != 98)) //can't be status line
		|| (curr2->el.hdr.grptyp == 98 && curr2->el.hdr.vg_type == SPLN_ELM 
		    && curr2->next != NULL && curr2->next->el.hdr.vg_type != SPLN_ELM)){
	   
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier); }

	    /* Is not contour but is last element */
	    else if (curr2-> el.hdr.grptyp >0 && hasSameElm !=1 && curr2->next == NULL 
		&& curr2->el.hdr.grptyp != 8 && curr2->el.hdr.grptyp != 9 && curr2->el.hdr.grptyp != 27
		&& curr2->el.hdr.vg_type != SIGCCF_ELM
		&& curr2->el.hdr.vg_type != GFA_ELM && curr2->el.hdr.vg_type != TCA_ELM) {
		 
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);
	    }

	    /* has same elements */
	    else if (curr2-> el.hdr.grptyp >0 && hasSameElm ==1 && isLineLike(curr2-> el.hdr.vg_type) == 1 && curr2->next == NULL )

		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);

	    /* contours grpnum>0 */
	    /* text */
	    else if ((curr2-> el.hdr.grptyp ==5 || curr2-> el.hdr.grptyp ==6
		    || curr2-> el.hdr.grptyp ==8 ||curr2-> el.hdr.grptyp ==9 ||curr2-> el.hdr.grptyp ==27)
		&& curr2->el.hdr.vg_type == SPTX_ELM 
		&& curr2->next != NULL && contText ==2 
		&& ( (curr2->next->el.hdr.grptyp !=8 && curr2->next->el.hdr.grptyp !=5 
		        && curr2->next->el.hdr.grptyp !=6 && curr2->next->el.hdr.grptyp !=9
			&& curr2->next->el.hdr.grptyp !=27)
		    || ((curr2->next->el.hdr.grptyp ==8 || curr2->next->el.hdr.grptyp ==5 
		    	|| curr2->next->el.hdr.grptyp ==6 || curr2->next->el.hdr.grptyp ==9
			|| curr2->next->el.hdr.grptyp ==27)
		    	&& curr2->next->el.hdr.vg_type != SPTX_ELM //last text
		    	&& isSymbolMarkLike(curr2->next->el.hdr.vg_type) != 1
		    	&& isLineLike(curr2->next->el.hdr.vg_type) != 1 )) ) {
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}

	    /* symbol w/o text */
	    else if ( (curr2-> el.hdr.grptyp ==5 || curr2-> el.hdr.grptyp ==6
		    || curr2-> el.hdr.grptyp ==8 ||curr2-> el.hdr.grptyp ==9 ||curr2-> el.hdr.grptyp ==27)
		&& curr2->next != NULL && contText ==3 
		&& isSymbolMarkLike(curr2->el.hdr.vg_type) == 1 		
		&& ( (curr2->next->el.hdr.grptyp !=8 && curr2->next->el.hdr.grptyp !=5 
		        && curr2->next->el.hdr.grptyp !=6 && curr2->next->el.hdr.grptyp !=9
			&& curr2->next->el.hdr.grptyp !=27)
		    || ((curr2->next->el.hdr.grptyp ==8 || curr2->next->el.hdr.grptyp ==5 
		    	|| curr2->next->el.hdr.grptyp ==6 || curr2->next->el.hdr.grptyp ==9
			|| curr2->next->el.hdr.grptyp ==27)
		    	&& curr2->next->el.hdr.vg_type != SPTX_ELM //last text
		    	&& isSymbolMarkLike(curr2->next->el.hdr.vg_type) != 1
		    	&& isLineLike(curr2->next->el.hdr.vg_type) != 1 )) ) {
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}

	    /* text */
	    else if (curr2-> el.hdr.grptyp ==8 && curr2->el.hdr.vg_type == SPTX_ELM 
		&& curr2->next != NULL && outlookText == 2 
		&& ( curr2->next->el.hdr.grptyp !=8 
		    || (curr2->next->el.hdr.grptyp ==8 	&& isLineLike(curr2->next->el.hdr.vg_type) != 1 
			&& curr2->next->el.hdr.vg_type != SPTX_ELM)) ) {
		    	 		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}

	    /* labeled symbol */
	    else if ((curr2-> el.hdr.grptyp ==5 || curr2-> el.hdr.grptyp ==6 
		    || curr2->el.hdr.grptyp ==8 || curr2->el.hdr.grptyp ==9 || curr2->el.hdr.grptyp ==27)
		&& curr2->el.hdr.vg_type == SPTX_ELM 
		&& curr2->next != NULL && symText ==2 
		&& (curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum 
			|| curr2->el.hdr.grptyp != curr2->next->el.hdr.grptyp)){
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}

	    /* common collection */
	    else if ((curr2-> el.hdr.grptyp ==5 || curr2-> el.hdr.grptyp ==6 
		    || curr2->el.hdr.grptyp ==8 || curr2->el.hdr.grptyp ==9 || curr2->el.hdr.grptyp ==27)
		&& curr2->next != NULL && commonText !=0 
		&& (curr2->el.hdr.grpnum != curr2->next->el.hdr.grpnum 
			|| curr2->el.hdr.grptyp != curr2->next->el.hdr.grptyp)){
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}

	    /* added for last element or line without text */
	    else if ((curr2-> el.hdr.grptyp ==5 || curr2-> el.hdr.grptyp ==6 
		    || curr2->el.hdr.grptyp ==8 || curr2->el.hdr.grptyp ==9 || curr2->el.hdr.grptyp ==27)
		&& curr2->next == NULL ) {

		if (contText != 0 && hasContour !=0){	
		    char* xml_end = "              </DrawableElement>\n            </DECollection>\n          </Contours>\n";
	    	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );
	    	    contText = 0;
	    	    grpInit = 0;
	    	    grpNum = 0;
		}
		else if (outlookText != 0 ) {
		    char* xml_end = "              </DrawableElement>\n            </DECollection>\n          </Outlook>\n";
	    	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );
	    	    outlookText = 0;
	    	    grpNum = 0;
		}
		else if (symText != 0 || commonText != 0) {
		    char* xml_end = "            </DrawableElement>\n          </DECollection>\n";
	    	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );
	    	    symText = 0;
	    	    commonText = 0;
	    	    grpNum = 0;
		}
		//writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}
	    }

	    // outlook 
	    else if (isOutlookLike(curr2-> el.hdr.grptyp) ==1
		&& curr2->el.hdr.vg_type == SPTX_ELM && outlookText ==2
		&& curr2->next != NULL 
		&& curr2-> el.hdr.grptyp != curr2->next-> el.hdr.grptyp )
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);
	    //temp need lin tx lin tx
	    else if (isOutlookLike(curr2-> el.hdr.grptyp) ==1
                //&& curr2->el.hdr.vg_type == SPLN_ELM
                && curr2->next != NULL
                && curr2-> el.hdr.vg_type == curr2->next-> el.hdr.vg_type
                && curr2-> el.hdr.grpnum == curr2->next-> el.hdr.grpnum)

		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);

	    else if (isOutlookLike(curr2-> el.hdr.grptyp) ==1
		&& curr2->next != NULL && commonText !=0 //common
		&& (curr2-> el.hdr.grpnum != curr2->next-> el.hdr.grpnum 
		    || curr2->el.hdr.grptyp != curr2->next->el.hdr.grptyp))
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);

	    else if (isOutlookLike(curr2-> el.hdr.grptyp) ==1
		&& curr2->next == NULL ){
		
		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);}

	    else if (curr2-> el.hdr.grptyp >0 && hasSameElm !=1 && curr2->next != NULL 
		&& curr2->el.hdr.vg_type != SIGCCF_ELM
		&& curr2->el.hdr.vg_type != GFA_ELM && curr2->el.hdr.vg_type != TCA_ELM
		&& contText ==0  //is not contour
		&& curr2->el.hdr.grptyp != curr2->next->el.hdr.grptyp) 

		writeEndingCollection(&(curr2->el), ofptr, &grpNum, &grpInit, &grpColor, &contText, &symText, &outlookText, &commonText, &cloudText, &turbText, &hasSameElm, &ier);
	    
	} /*else if ( el.hdr.recsz > 0 )*/	
	
	curr2 = curr2->next;
    } /*while */
 
    //free(curr2);
    
    /* 
     * End the XML.
     */
    if ( nw > 0 ) {  
    	xml_end = "        </DrawableElement>\n";
    	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );
    	xml_end = "      </Layer>\n";
    	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );
    	xml_end = "    </Product>\n";
    	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );
    	xml_end = "  </Products>\n";
    	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, &ier );

    } 
}

/*---------------------------------------------------------------------*/   

void getCcf(VG_DBStruct *el, char *buffer, int *ier)
/********************************************************************************
 * getCcf									*
 *										*
 * Subroutine to convert a VG CCFP element into CAVE CCFP_SIGMET in XML		*
 *										*
 ********************************************************************************/
{
    char *collection;
    //collectionName="CCFP_SIGMET:::10:::N:::null:::null:::40-74%:::300-340:::50-100%:::+:::Area"
    char *dir, *cover, *tops, *prob, *growth, *stype;

    if (el->elem.ccf.info.cover == 3)
	cover = "25-39%";
    else if (el->elem.ccf.info.cover == 2)
	cover = "40-74%";
    else if (el->elem.ccf.info.cover == 1)
	cover = "75-100%";

    if (el->elem.ccf.info.tops == 4)
	tops = "250-290";
    else if (el->elem.ccf.info.tops == 3)
	tops = "300-340";
    else if (el->elem.ccf.info.tops == 2)
	tops = "350-390";
    else if (el->elem.ccf.info.tops == 1)
	tops = "400+";

    if (el->elem.ccf.info.prob == 3)
	prob = "25-49%";
    else if (el->elem.ccf.info.prob == 1)
	prob = "50-100%";
    
    if (el->elem.ccf.info.growth == 4)
	growth = "-";
    else if (el->elem.ccf.info.growth == 3)
	growth = "NC";
    else if (el->elem.ccf.info.growth == 2)
	growth = "+";

    if (el->elem.ccf.info.subtype == 0)
	stype = "Area";
    else if (el->elem.ccf.info.subtype == 1)
	stype = "Line";
    else if (el->elem.ccf.info.subtype == 2)
	stype = "LineMed";

    if (el->elem.ccf.info.dir == 0)
	dir = "N";
    else if (el->elem.ccf.info.dir == 22.5)
	dir = "NNE";
    else if (el->elem.ccf.info.dir == 45)
	dir = "NE";
    else if (el->elem.ccf.info.dir == 67.5)
	dir = "ENE";
    else if (el->elem.ccf.info.dir == 90)
	dir = "E";
    else if (el->elem.ccf.info.dir == 112.5)
	dir = "ESE";
    else if (el->elem.ccf.info.dir == 135)
	dir = "SE";
    else if (el->elem.ccf.info.dir == 157.5)
	dir = "SSE";
    else if (el->elem.ccf.info.dir == 180)
	dir = "S";
    else if (el->elem.ccf.info.dir == 202.5)
	dir = "SSW";
    else if (el->elem.ccf.info.dir == 225)
	dir = "SW";
    else if (el->elem.ccf.info.dir == 247.5)
	dir = "WSW";
    else if (el->elem.ccf.info.dir == 270)
	dir = "W";
    else if (el->elem.ccf.info.dir == 292.5)
	dir = "WNW";
    else if (el->elem.ccf.info.dir == 315)
	dir = "NW";
    else if (el->elem.ccf.info.dir == 337.5)
	dir = "NNW";
    else
	dir = "N";

    
    collection = (char*)malloc(128*sizeof(char));
    sprintf(collection, "%s%d%s%s%s%s%s%s%s%s%s%s%s%s", "CCFP_SIGMET:::", (int)el->elem.ccf.info.spd, ":::", dir,  
    ":::null:::null:::", cover, ":::", tops, ":::", prob, ":::", growth, ":::", stype);

    sprintf( buffer, "          <DECollection pgenCategory=\"SIGMET\" pgenType=\"CCFP_SIGMET\" collectionName=\"%s\">\n            <DrawableElement>\n",  collection);
    
    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	
    free(collection);

    //put ccf ending tag here
    sprintf( &(buffer[strlen(buffer)]), "            </DrawableElement>\n          </DECollection>\n");        		    		        
}

/*---------------------------------------------------------------------*/   

void getWatch(VG_DBStruct *el, char *buffer, int *grpNum, int *ier) 
/********************************************************************************
 * getWatch									*
 *										*
 * Subroutine to convert a VG Watch element into CAVE Watch element in XML.	*
 *										*
 ********************************************************************************/
{
    if ( el->hdr.vg_type == WBOX_ELM ) { 
	
	sprintf( buffer, "          <DECollection collectionName=\"Watch\">\n            <DrawableElement>\n" );	
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );		        		    		    
    }

    else if ( el->hdr.vg_type == SPLN_ELM ) { 
	cvg_v2x ( el, buffer, ier );
    }
}

/*---------------------------------------------------------------------*/   

void getMatchContAttr(char *tblArray[], int *grpColor, char* contArray[], int tblArrayLen, int *ier)
/********************************************************************************
 * getMatchContAttr								*
 *										*
 * Subroutine to get specified information to convert Contours.			*
 *										*
 ********************************************************************************/
{
    char delims[] = " \t";
    char *firstWd = NULL;
    int i=0, j=0;

    if (tblArrayLen ==0)
   	return;

    char *array[tblArrayLen]; 
    for ( j=0; j< tblArrayLen; j++) {
	array[j] = (char*)malloc(strlen(tblArray[j])+1); 	
	strcpy(array[j], tblArray[j]);
    }

    if (*array != NULL && *array != '\0') {
	for ( i=0; i< tblArrayLen; i++) {
	    //printf("contArr %s\n", array[i]);
	    firstWd = strtok( array[i], delims);
	    if (atoi(firstWd) == *grpColor) {
		break;
	    }
	}
    	
    	if (atoi(firstWd) == *grpColor) {  
  	    contArray[0] = strtok( NULL, delims );
	    contArray[1] = strtok( NULL, delims );
	    contArray[2] = strtok( NULL, delims );
	    contArray[3] = strtok( NULL, delims );
	    contArray[4] = strtok( NULL, delims );
	    contArray[5] = strtok( NULL, delims );
	    contArray[6] = strtok( NULL, delims );
	    //printf("contArray %s %s %s %s\n", array[i], contArray[0], contArray[1], contArray[2] );	
	}
    }
}

/*---------------------------------------------------------------------*/   

void getMatchGrptyp(char *lineGrptyp[], int *grptyp, int lineGrptypLen, char* grptypArray[], int *ier)
/********************************************************************************
 * getMatchGrptyp								*
 *										*
 * Subroutine to get specified group type conversion.				*
 *										*
 ********************************************************************************/
{
    char delims[] = " \t";
    char *firstWd;
    int i=0, j=0;

    // init grptypArray
    grptypArray[0] = "0";
    grptypArray[1] = "default";
    grptypArray[2] = "default";

    if (lineGrptypLen ==0 || grptyp == 0)
   	return;

    char *array[lineGrptypLen];

    for ( j=0; j< lineGrptypLen; j++) {
	array[j] = (char*)malloc(strlen(lineGrptyp[j])+1); 	
	strcpy(array[j], lineGrptyp[j]);
    }

    if (*array != NULL && *array != '\0') {
	for ( i=0; i< lineGrptypLen; i++) {
	    //printf("grpArr %s\n", array[i]);
	    firstWd = strtok( array[i], delims);
	    grptypArray[0] = strtok( NULL, delims );
	    
	    if (atoi(grptypArray[0]) == *grptyp) 
		break;
	}

    	if (atoi(grptypArray[0]) == *grptyp) {  
  	    grptypArray[1] = strtok( NULL, delims );
	    grptypArray[2] = strtok( NULL, delims );
	    //free ( array[i] );
	    //printf("grpArray %s %s %s %s\n", array[i], grptypArray[0], grptypArray[1], grptypArray[2] );
	}	
    }
}

/*---------------------------------------------------------------------*/   
void getXmlTimeString(char *xmlTime, char *gempakTimeStr )
/********************************************************************************
 * getXmlTimeString								*
 *										*
 * Subroutine to convert a gempak format time string "YYYYMMDD/HHMM" or 	*
 * "YYYYMMDD/HH" into a JAXB XML format as "YYYY-MM-DDTHH:MM:SS.mmmZ"		*
 *										*
 ********************************************************************************/
{   
     if ( gempakTimeStr != NULL ) {
        xmlTime[0] = '\0'; 
        if ( strchr(gempakTimeStr, '/') != NULL && (strlen(gempakTimeStr) >= 11 ) ) {        
            
            strncpy(xmlTime, gempakTimeStr, 4);
 
            xmlTime[4]='-';
            xmlTime[5]=gempakTimeStr[4];
            xmlTime[6]=gempakTimeStr[5];
            xmlTime[7]='-';
            xmlTime[8]=gempakTimeStr[6];
            xmlTime[9]=gempakTimeStr[7];        
            xmlTime[10]='T';
            xmlTime[11]=gempakTimeStr[9];
            xmlTime[12]=gempakTimeStr[10];
            if ( strlen(gempakTimeStr) < 13 ) {
                strcat(xmlTime,":00");
            }
            else {        
                xmlTime[13]=':';
                xmlTime[14]=gempakTimeStr[11];
                xmlTime[15]=gempakTimeStr[12];      
                xmlTime[16]='\0';
            }     
  
            strcat(xmlTime, ":00.000Z");
        }
        else {
            if ( gempakTimeStr != NULL ) {
                strcpy(xmlTime, gempakTimeStr);
            }
        }
    }  
}


/*---------------------------------------------------------------------*/   

void getContour(VG_DBStruct *el, char *buffer, int *contText, int *grpNum, 
                int *grpColor, char *tblArray[], int tblArrayLen, int *ier)
/********************************************************************************
 * getContour									*
 *										*
 * Subroutine to convert a VG contour into CAVE Contours element in XML.	*
 *										*
 ********************************************************************************/
{
    char* contArray[7] = {"HGHT", "1000", "-1", "f000", "10", "20110131/1200", "-1"}; //array of 7 strings
    char *time1 = NULL, *time2 = NULL;
    char  xmltime1[32], xmltime2[32];
  
    if ( *grpNum ==0) {
	/* Only change grpColor color when it's line or circle.  Although it can mix up line and sym. 
	   *grpColor is first contour line color now. Next row may not change the color if the element is symbol.*/
	if (isLineLike(el->hdr.vg_type) == 1 ) { //no el->hdr.vg_type == SPSYM_ELM
	    *grpColor = el->hdr.maj_col;
	}
	*grpNum = el->hdr.grpnum;	//!=0    

    	/* Get contour attributes from contTbl if it is not empty.  e.g. 2       TEMP    850     900     f006    10/0/100 */
    	getMatchContAttr(tblArray, grpColor, contArray, tblArrayLen, ier);
        
        /*
         * The time should be in format as "YYYY-MM-DDTHH:MM:SS.mmmZ"
         *  If input in table is "YYYYMMDD/HHMM" we need to convert it.
         */
        time1 = (char*)malloc(strlen(contArray[5])+1); 
	strcpy(time1,contArray[5] );
        if ( atoi(contArray[6]) < 0 ) {
            time2 = (char*)malloc(strlen(time1)+1); 
	    strcpy(time2,time1 );
        }
        else {
            time2 = (char*)malloc(strlen(contArray[6])+1); 
	    strcpy(time2,contArray[6] );
        }

        getXmlTimeString(xmltime1, time1);
        getXmlTimeString(xmltime2, time2);

	sprintf(buffer, "          <Contours collectionName=\"Contours\" pgenCategory=\"MET\" pgenType=\"Contours\" parm=\"%s\" level=\"%s\" level2=\"%s\" forecastHour=\"%s\" cint=\"%s\" time1=\"%s\" time2=\"%s\">\n", contArray[0], contArray[1], contArray[2], contArray[3], contArray[4], xmltime1, xmltime2 );

	if (isOutlookLineLike(el->hdr.vg_type) == 1) {
	    sprintf( &(buffer[strlen(buffer)]), "            <DECollection collectionName=\"ContourLine\">\n              <DrawableElement>\n" );	
            cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 1;
	}
	else if (el->hdr.vg_type == CIRCLE_ELM ) {
	    sprintf( &(buffer[strlen(buffer)]), "            <DECollection collectionName=\"ContourCircle\">\n              <DrawableElement>\n" );
	    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 1;
	}
	else if (isSymbolMarkLike(el->hdr.vg_type) == 1 ) {
	    sprintf( &(buffer[strlen(buffer)]), "            <DECollection collectionName=\"ContourMinmax\">\n              <DrawableElement>\n" );
	    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 3;
	}
	
    }

    
    else if (*grpColor !=0 && *grpColor != el->hdr.maj_col
	    && isLineLike(el->hdr.vg_type) == 1 ) {  
		
	*grpColor = el->hdr.maj_col;	
	*grpNum = el->hdr.grpnum; 

   	/* Get contour attributes from contTbl if it is not empty.  e.g. 2       TEMP    850     900     f006    10/0/100 */
    	getMatchContAttr(tblArray, grpColor, contArray, tblArrayLen, ier);

        /*
         * The time should be in format as "YYYY-MM-DDTHH:MM:SS.mmmZ"
         *  If input in table is "YYYYMMDD/HHMM" we need to convert it.
         */
        time1 = (char*)malloc(strlen(contArray[5])+1); 
	strcpy(time1,contArray[5] );
        if ( atoi(contArray[6]) < 0 ) {
            time2 = (char*)malloc(strlen(time1)+1); 
	    strcpy(time2,time1 );
        }
        else {
            time2 = (char*)malloc(strlen(contArray[6])+1); 
	    strcpy(time2,contArray[6] );
        }

        getXmlTimeString(xmltime1, time1);
        getXmlTimeString(xmltime2, time2);
   
	sprintf( buffer, "              </DrawableElement>\n            </DECollection>\n          </Contours>\n");		    
	sprintf(&(buffer[strlen(buffer)]), "          <Contours collectionName=\"Contours\" pgenCategory=\"MET\" pgenType=\"Contours\" parm=\"%s\" level=\"%s\" level2=\"%s\" forecastHour=\"%s\" cint=\"%s\" time1=\"%s\" time2=\"%s\">\n", contArray[0], contArray[1], contArray[2], contArray[3], contArray[4], xmltime1, xmltime2);
	
	if (isOutlookLineLike(el->hdr.vg_type) == 1) {		 
	    sprintf( &(buffer[strlen(buffer)]), "            <DECollection collectionName=\"ContourLine\">\n              <DrawableElement>\n" );	
            cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 1;
	}
	else if (el->hdr.vg_type == CIRCLE_ELM ) {
	    sprintf( &(buffer[strlen(buffer)]), "            <DECollection collectionName=\"ContourCircle\">\n              <DrawableElement>\n" );
	    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 1;
	}	
    }
	    
    else if ( isLineLike(el->hdr.vg_type) == 1 ||isSymbolMarkLike(el->hdr.vg_type) == 1 ) {
	
	if (isOutlookLineLike(el->hdr.vg_type) == 1) {	
	    	sprintf( buffer, "              </DrawableElement>\n            </DECollection>\n            <DECollection collectionName=\"ContourLine\">\n              <DrawableElement>\n" );
	    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 1;
	    *grpNum = el->hdr.grpnum;
	}
	else if (el->hdr.vg_type == CIRCLE_ELM ) {
	    	sprintf( buffer, "              </DrawableElement>\n            </DECollection>\n            <DECollection collectionName=\"ContourCircle\">\n              <DrawableElement>\n" );

	    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 1;
	    *grpNum = el->hdr.grpnum;
	}
	else {
	    	sprintf( buffer, "              </DrawableElement>\n            </DECollection>\n            <DECollection collectionName=\"ContourMinmax\">\n              <DrawableElement>\n" );
	    cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	    *contText = 3;
	    *grpNum = el->hdr.grpnum;
	}
		
    }

    else if ( el->hdr.vg_type == SPTX_ELM) { 
	*grpNum = el->hdr.grpnum;  		
	cvg_v2x ( el, buffer, ier );
	*contText = 2;	
	    
    }
}

/*---------------------------------------------------------------------*/   

void getSymLab(VG_DBStruct *el, char *buffer, int *symText, int *grpNum, int *ier)
/********************************************************************************
 * getSymLab									*
 *										*
 * Subroutine to convert a VG symbol, marker, front element into CAVE labeled   *
 * symbol, marker, front element in XML.					*
 *										*
 ********************************************************************************/
{
    char *category = "";
    char* pgenType = "";
    char* pgenName = "";

    if (isSymbolLike(el->hdr.vg_type) == 1
	|| el->hdr.vg_type == CMBSY_ELM) {
	category = "Symbol";
	pgenType = getSymType( el );
	pgenName = "labeledSymbol";
    }
    else if (el->hdr.vg_type == MARK_ELM) {
	category = "Marker";
	pgenType = getSymType( el );
	pgenName = "labeledSymbol";
    }
    else if (el->hdr.vg_type == FRONT_ELM) {
	category = "Front";
	pgenType = getSymType( el );
	pgenName = "labeledFront";
    }

    if (*grpNum ==0 ) {
	*grpNum = el->hdr.grpnum;
    		    		 
	sprintf(buffer, "          <DECollection pgenCategory=\"%s\" pgenType=\"%s\" collectionName=\"%s\">\n            <DrawableElement>\n", category, pgenType, pgenName);	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );	
	*symText = 1;	      
    }

    else if (*grpNum != el->hdr.grpnum ) { 
	*grpNum = el->hdr.grpnum;
	sprintf(buffer, "            </DrawableElement>\n          </DECollection>\n          <DECollection pgenCategory=\"%s\" pgenType=\"%s\" collectionName=\"%s\">\n            <DrawableElement>\n",  category, pgenType, pgenName);	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );	
	*symText = 1;	 
    }

    else {
	cvg_v2x ( el, buffer, ier );
	*symText = 2;
    }
}

/*---------------------------------------------------------------------*/   
 
void getOutlooks(VG_DBStruct *el, char *buffer, char* outType, int *outlookText, 
                 int *grpNum, int *ier) 
/********************************************************************************
 * getOutlooks									*
 *										*
 * Subroutine to convert a VG outlook into CAVE Outlook element in XML.		*
 *										*
 ********************************************************************************/
{

    char* outlookType;
    if (strcmp(outType, "default") ==0)
	outlookType = getGroupType(el->hdr.grptyp);
    else
	outlookType = outType;
    
    if (*grpNum ==0 ) { 
	*grpNum = el->hdr.grpnum;		    
	sprintf(buffer, "          <Outlook outlookType=\"%s\" name=\"Outlook\" pgenCategory=\"MET\" pgenType=\"%s\" parm=\"HGMT\" level=\"1000\" time=\"2010-03-22T12:45:08.981Z\" cint=\"10/0/100\">\n", outlookType, outlookType );		 
	sprintf( &(buffer[strlen(buffer)]), "            <DECollection collectionName=\"OutlookLine\">\n              <DrawableElement>\n" );	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );	
	*outlookText = 1;	    
    }
    
    else if (el->hdr.grpnum != *grpNum) {	
	*grpNum = el->hdr.grpnum;		
	sprintf( buffer, "              </DrawableElement>\n            </DECollection>\n            <DECollection collectionName=\"OutlookLine\">\n              <DrawableElement>\n" );
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	*outlookText = 1;
    }

    else if (el->hdr.grpnum == *grpNum && (isOutlookLineLike(el->hdr.vg_type) == 1)) {	
	*grpNum = el->hdr.grpnum;		
	sprintf( buffer, "              </DrawableElement>\n            </DECollection>\n            <DECollection collectionName=\"OutlookLine\">\n              <DrawableElement>\n" );
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	*outlookText = 1;
    }

    else if ( el->hdr.vg_type == SPTX_ELM) {   
	cvg_v2x ( el, buffer, ier );
	*outlookText = 2;
    }
    
}

/*---------------------------------------------------------------------*/   

void getCommonCollection(VG_DBStruct *el, char *buffer, int *commonText, 
                         int *grpNum, int *ier) 
/********************************************************************************
 * getCommonCollection								*
 *										*
 * Subroutine to convert a VG element into a common CAVE collection		*
 *										*
 ********************************************************************************/
{
    char* pgenCate = "";
    char* pgenType = "";
    char* pgenName = getGroupType(el->hdr.grptyp); 

    if (isSymbolLike(el->hdr.vg_type) == 1
	|| el->hdr.vg_type == CMBSY_ELM) {
	pgenCate = "Symbol";
	pgenType = "Symbol";

    }
    else if (el->hdr.vg_type == MARK_ELM) {
	pgenType = "Marker";
	pgenCate = "Marker";

    }
    else if (el->hdr.vg_type == FRONT_ELM) {
	pgenType = "Front";
	pgenCate = "Front";

    }
    else if (el->hdr.vg_type == SPTX_ELM) {
	pgenType = "Text";
	pgenCate = "Text";

    }
    else if (el->hdr.vg_type == LINE_ELM) {
	pgenType = "LINE_SOLID";
	pgenCate = "Lines";

    }
    else if (el->hdr.vg_type == SPLN_ELM) {
	pgenType = "";
	pgenCate = "Lines";

    }
    else {
	pgenType = "";
	pgenCate = "";
    }

    if (el->hdr.vg_type == JET_ELM)
	cvg_v2x ( el, buffer, ier );	
    
    else if (*grpNum ==0 ) {
	*grpNum = el->hdr.grpnum;
			    		 
	sprintf(buffer, "          <DECollection pgenCategory=\"%s\" pgenType=\"%s\" collectionName=\"%s\">\n            <DrawableElement>\n", pgenCate, pgenType, pgenName);	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );	
	*commonText = 1;	    
    }

    else if (*grpNum != el->hdr.grpnum ) {  
	*grpNum = el->hdr.grpnum;
	sprintf(buffer, "            </DrawableElement>\n          </DECollection>\n          <DECollection pgenCategory=\"%s\" pgenType=\"%s\" collectionName=\"%s\">\n            <DrawableElement>\n", pgenCate, pgenType, pgenName);	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );	
	*commonText = 1;	    
    }

    else {
	cvg_v2x ( el, buffer, ier );
	*commonText = 2;
    }
}

/*---------------------------------------------------------------------*/   

void getCloud(VG_DBStruct *el, char *buffer, int *cloudText, int *grpNum, 
              int *grpInit, int *ier) 
/********************************************************************************
 * getCloud									*
 *										*
 * Subroutine to convert a VG cloud into CAVE Cloud element in XML.		*
 *										*
 ********************************************************************************/
{

    /* draw Scollop line(3) */
    if ( *grpNum ==0 && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 3) {
	*grpNum = el->hdr.grpnum;	
	sprintf(buffer, "          <DECollection pgenCategory=\"MET\" pgenType=\"Cloud\" collectionName=\"Cloud\">\n            <DrawableElement>\n" );		 	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	*cloudText = 1;
    }
    
    else if ( *grpNum < el->hdr.grpnum && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 3) {		
	*grpNum = el->hdr.grpnum;	    
	sprintf( buffer, "            </DrawableElement>\n          </DECollection>\n");	
	sprintf(&(buffer[strlen(buffer)]), "          <DECollection pgenCategory=\"MET\" pgenType=\"Cloud\" collectionName=\"Cloud\">\n            <DrawableElement>\n");		 	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	*cloudText = 1;
    }

    // Let *grpNum!=0, so can't start from here
    else if (*grpNum !=0 && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 3) { 
	cvg_v2x ( el, buffer, ier);
    }

    /* draw Arrow line(4), 1 arrow line/1 text, or only 1 text(how?) */    
    else if (*grpNum !=0 && *grpInit ==0 && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 4) {	
	*grpInit = 1;
	sprintf( buffer, "              <DECollection collectionName=\"Label\">\n                <DrawableElement>\n" );
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
    }

    else if (*grpNum !=0 && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 4) {
	cvg_v2x ( el, buffer, ier);	*cloudText = 1;
    }

    /* draw text */
    else if (*grpNum !=0 && *grpInit ==0 && el->hdr.vg_type == SPTX_ELM) {	
	//*grpInit = 1;	
	sprintf( buffer, "              <DECollection collectionName=\"Label\">\n                <DrawableElement>\n" );
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	sprintf( &(buffer[strlen(buffer)]), "                </DrawableElement>\n              </DECollection>\n" );
    } 

    else if (*grpNum !=0 && el->hdr.vg_type == SPTX_ELM) {   
	*grpInit = 0;	
	cvg_v2x ( el, buffer, ier );	
	sprintf( &(buffer[strlen(buffer)]), "                </DrawableElement>\n              </DECollection>\n" );	    
    }
}

/*---------------------------------------------------------------------*/   

void getTurb(VG_DBStruct *el, char *buffer, int *turbText, int *grpNum, 
             int *grpInit, int *ier) 
/********************************************************************************
 * getTurb									*
 *										*
 * Subroutine to convert a VG Turb into CAVE Turb element in XML.		*
 *										*
 ********************************************************************************/
{

    /* draw Turb line(dashed_line) */
    if ( *grpNum ==0 && el->hdr.vg_type == LINE_ELM ) {
	*grpNum = el->hdr.grpnum;	
	sprintf(buffer, "          <DECollection pgenCategory=\"MET\" pgenType=\"Turbulence\" collectionName=\"Turbulence\">\n            <DrawableElement>\n" );		 	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	*turbText = 1;
    }
    
    else if ( *grpNum < el->hdr.grpnum && el->hdr.vg_type == LINE_ELM ) {		
	*grpNum = el->hdr.grpnum;	    
	sprintf( buffer, "            </DrawableElement>\n          </DECollection>\n");	
	sprintf(&(buffer[strlen(buffer)]), "          <DECollection pgenCategory=\"MET\" pgenType=\"Turbulence\" collectionName=\"Turbulence\">\n            <DrawableElement>\n");		 	
        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	*turbText = 1;
    }

    else if (*grpNum !=0 && el->hdr.vg_type == LINE_ELM ) {
	cvg_v2x ( el, buffer, ier);
    }

    /* draw Arrow line(4) */    
    else if (*grpNum !=0 && *grpInit ==0 && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 4) {	
	*grpInit = 1;
	sprintf( buffer, "              <DECollection collectionName=\"Label\">\n                <DrawableElement>\n" );
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
    }
   
    else if (*grpNum !=0 && el->hdr.vg_type == SPLN_ELM && el->elem.spl.info.spltyp == 4) {
	cvg_v2x ( el, buffer, ier);
    }

    /* draw text */
    else if (*grpNum !=0 && *grpInit ==0 && el->hdr.vg_type == SPTX_ELM) {	
	if (el->elem.spt.info.sptxtyp == 0 || el->elem.spt.info.sptxtyp == 3
	|| el->elem.spt.info.sptxtyp == 4 || el->elem.spt.info.sptxtyp == 5
	|| el->elem.spt.info.sptxtyp == 10 || el->elem.spt.info.sptxtyp == 11
	|| el->elem.spt.info.sptxtyp == 13 || el->elem.spt.info.sptxtyp == 14) {
	    printf("+ The text for the turbulence line is not an avn text.\n");
 	}
	
	sprintf( buffer, "              <DECollection collectionName=\"Label\">\n                <DrawableElement>\n" );
	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
	sprintf( &(buffer[strlen(buffer)]), "                </DrawableElement>\n              </DECollection>\n" );
    }

    else if (*grpNum !=0 && el->hdr.vg_type == SPTX_ELM) {  
	if (el->elem.spt.info.sptxtyp == 0 || el->elem.spt.info.sptxtyp == 3
	|| el->elem.spt.info.sptxtyp == 4 || el->elem.spt.info.sptxtyp == 5
	|| el->elem.spt.info.sptxtyp == 10 || el->elem.spt.info.sptxtyp == 11
	|| el->elem.spt.info.sptxtyp == 13 || el->elem.spt.info.sptxtyp == 14) {
	    printf("+ The text for the turbulence line is not an avn text.\n");
 	}
	
	*grpInit = 0;	
	cvg_v2x ( el, buffer, ier );	
	sprintf( &(buffer[strlen(buffer)]), "                </DrawableElement>\n              </DECollection>\n" );	    
    }
}

/*---------------------------------------------------------------------*/   

void getVolc(VG_DBStruct *el, char *buffer, int *vol, int *ashFhr, int *ier) 
/********************************************************************************
 * getVolc									*
 *										*
 * Subroutine to convert a VG volcano into CAVE Volcano element in XML.		*
 *										*
 ********************************************************************************/
{

    char *strAsh00 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"OBS\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n";

    char *strAsh06 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F06\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"0\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n";

    char *strAsh12 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F12\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"0\" green=\"255\" blue=\"255\" alpha=\"255\"/>\n        <DrawableElement>\n";

    char *strAsh18 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F18\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"0\" blue=\"255\" alpha=\"255\"/>\n        <DrawableElement>\n";



    if ( *vol == 0 && el->hdr.vg_type == VOLC_ELM ) {
	*vol = 1;	
	cvg_v2x ( el, buffer, ier );
    }

    else if ( *vol == 1 && el->hdr.vg_type == VOLC_ELM ) {
	cvg_v2x ( el, buffer, ier );
    }

    else if ( *vol == 0 && el->hdr.vg_type == ASHCLD_ELM ) {
	*vol = 1;
	*ashFhr = el->elem.ash.info.fhr;

	if (el->elem.ash.info.fhr ==0) {
	sprintf(buffer, "%s", strAsh00 );	
	}
    	else if (el->elem.ash.info.fhr ==6) {
	    sprintf( buffer, "%s%s", strAsh00, strAsh06);
    	}
    	else if (el->elem.ash.info.fhr ==12) {
	    sprintf( buffer, "%s%s%s", strAsh00, strAsh06, strAsh12);
    	}
    	else if (el->elem.ash.info.fhr ==18) {
	    sprintf( buffer, "%s%s%s%s", strAsh00, strAsh06, strAsh12, strAsh18);
    	}

        cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
    }

    else if ( *ashFhr < el->elem.ash.info.fhr && el->hdr.vg_type == ASHCLD_ELM ) {
	*ashFhr = el->elem.ash.info.fhr;
	
	if (el->elem.ash.info.fhr ==0) {
	    sprintf( buffer, "        </DrawableElement>\n      </Layer>\n      <Layer name=\"OBS\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n");    
    	}
    	else if (el->elem.ash.info.fhr ==6) {
	    sprintf( buffer, "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F06\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"0\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n");
    	}
    	else if (el->elem.ash.info.fhr ==12) {
	    sprintf( buffer, "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F12\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"0\" green=\"255\" blue=\"255\" alpha=\"255\"/>\n        <DrawableElement>\n");
    	}
    	else if (el->elem.ash.info.fhr ==18) {
	    sprintf( buffer, "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F18\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"0\" blue=\"255\" alpha=\"255\"/>\n        <DrawableElement>\n");
    	}		 	
        
    	cvg_v2x ( el, &(buffer[strlen(buffer)]), ier );
    }

    else if (*ashFhr == el->elem.ash.info.fhr && el->hdr.vg_type == ASHCLD_ELM){
	cvg_v2x ( el, buffer, ier );
    }
}

/*---------------------------------------------------------------------*/   

void writeEndingCollection( VG_DBStruct *el, FILE *ofptr, int *grpNum, int *grpInit, 
                            int *grpColor, int *contText, int *symText, int *outlookText, 
			    int *commonText, int *cloudText, int *turbText, int *hasSameElm, int *ier) 
/********************************************************************************
 * writeEndingCollection							*
 *										*
 * Subroutine to wirte ending XML tags.						*
 *										*
 ********************************************************************************/
{
    char *xml_end = "        </DrawableElement>\n";

    if ((el->hdr.grptyp == 98 && el->hdr.vg_type == SPLN_ELM )
	|| el->hdr.vg_type == WBOX_ELM) {   
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	
    }
    /*else if (*hasSameElm == 1) {
        xml_end = "              </DrawableElement>\n            </DECollection>\n          </Contours>\n";

        cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*grpNum = 0;
    }*/
    else if ( el->hdr.grptyp ==1 || el->hdr.grptyp ==2 ) {  	
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*grpInit = 0;
	*grpNum = 0;
 	*commonText = 0;
	*cloudText = 0;
	*turbText = 0;
    }
    else if (*outlookText != 0) {
	xml_end = "              </DrawableElement>\n            </DECollection>\n          </Outlook>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*outlookText = 0;
	*grpNum = 0;
    }
    else if (*contText == 2 ||*contText == 3) {
	xml_end = "              </DrawableElement>\n            </DECollection>\n          </Contours>\n";
        cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*contText = 0;
	*grpInit = 0;
	*grpNum = 0;
    }
    else if (*symText != 0) {
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*symText = 0;
	*grpNum = 0;
    }
    else if (*commonText != 0) {
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*commonText = 0;
	*grpNum = 0;
    }
    


/*  The following was commented out 
    else if ( el->hdr.grptyp ==1 || el->hdr.grptyp ==2 ) {  	
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*grpInit = 0;
	*grpNum = 0;
 	*commonText = 0;
	*cloudText = 0;
	*turbText = 0;
    }
    else if ( el->hdr.grptyp == 3 || el->hdr.grptyp == 4) { 
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*grpNum = 0;
	*symText = 0;
	*commonText = 0;
    }

    else if ( el->hdr.grptyp == 5 || el->hdr.grptyp == 6 || el->hdr.grptyp == 9) {  
	if (*symText != 0 || *commonText != 0) {
	    xml_end = "            </DrawableElement>\n          </DECollection>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *symText = 0;
	    *commonText = 0;
	    *grpNum = 0;
	}
	else if (*contText != 0) {
	    xml_end = "              </DrawableElement>\n            </DECollection>\n          </Contours>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *contText = 0;
	    *grpInit = 0;
	    *grpNum = 0;
	}
    }

    else if ( (el->hdr.grptyp == 8 && el->hdr.grpnum > 0) || el->hdr.grptyp == 27) { //avoid gfa 
	if (*symText != 0) { 
	    xml_end = "            </DrawableElement>\n          </DECollection>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *symText = 0;
	     *grpNum = 0;
	}
	else if (*commonText != 0) { 
	    xml_end = "          </DrawableElement>\n        </DECollection>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *commonText = 0;
	    *grpNum = 0;
	}
	else if (*contText == 2) {
	    xml_end = "              </DrawableElement>\n            </DECollection>\n          </Contours>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *contText = 0;
	    *grpInit = 0;
	    *grpNum = 0;
	}
    }
		
    else if ( el->hdr.grptyp == 7 || el->hdr.grptyp >= 12 || el->hdr.grptyp == 13
	|| el->hdr.grptyp == 14 || el->hdr.grptyp == 16 || el->hdr.grptyp == 24) {  
	if (*outlookText != 0) {
	    xml_end = "              </DrawableElement>\n            </DECollection>\n          </Outlook>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *outlookText = 0;
	    *grpNum =0;
	}
	else if (*commonText !=0) {
	    xml_end = "            </DrawableElement>\n          </DECollection>\n";
	    cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	    *commonText = 0;
	    *grpNum = 0;
	}
    }

    else {   
	xml_end = "            </DrawableElement>\n          </DECollection>\n";
	cfl_writ ( ofptr, (int)strlen(xml_end), (unsigned char*)xml_end, ier );
	*grpNum = 0;
    }
*/

}

/*---------------------------------------------------------------------*/
  
void writeEndingAsh(VG_DBStruct *el, FILE *ofptr, int *ier) 
/********************************************************************************
 * writeEndingAsh								*
 *										*
 * Subroutine to end an CAVE Ash/Volcano elemnet in XML.			*
 *										*
 ********************************************************************************/
{
 
    char xmlStr[800] = " ";
    char *endAsh00 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F00\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n";
    char *endAsh06 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F06\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"0\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n        <DrawableElement>\n";
    char *endAsh12 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F12\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"0\" green=\"255\" blue=\"255\" alpha=\"255\"/>\n        <DrawableElement>\n";
    char *endAsh18 = "        </DrawableElement>\n      </Layer>\n      <Layer name=\"F18\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n        <Color red=\"255\" green=\"0\" blue=\"255\" alpha=\"255\"/>\n        <DrawableElement>\n";
    

    if (el->hdr.vg_type == ASHCLD_ELM) { //add missing layer upto F18
	if (el->elem.ash.info.fhr == 12) {
	   
	    sprintf( xmlStr, "%s", endAsh18);
	    cfl_writ ( ofptr, (int)strlen(xmlStr), (unsigned char*)xmlStr, ier );
	}
	else if (el->elem.ash.info.fhr == 6) {
	   
	    sprintf( xmlStr, "%s%s", endAsh12, endAsh18);
	    cfl_writ ( ofptr, (int)strlen(xmlStr), (unsigned char*)xmlStr, ier );
	}
	else if (el->elem.ash.info.fhr == 0) {
	   
	    sprintf( xmlStr, "%s%s%s", endAsh06, endAsh12, endAsh18);
	    cfl_writ ( ofptr, (int)strlen(xmlStr), (unsigned char*)xmlStr, ier );
	}	
    }

    else if (el->hdr.vg_type == VOLC_ELM) {
	   
	    sprintf( xmlStr, "%s%s%s%s", endAsh00, endAsh06, endAsh12, endAsh18);
	    cfl_writ ( ofptr, (int)strlen(xmlStr), (unsigned char*)xmlStr, ier );
    }

    /* may not need these since after sort, they are not used again 
    *vol = 0;
    *ashFhr = -1:
    */
}

/*---------------------------------------------------------------------*/

item1 *llist_bubble_sort( item1 *curr2 ) 
/********************************************************************************
 * llist_bubble_sort								*
 *										*
 * Bubble sort on linked list, cloud is sorted to dashed_line, scalloped_line	*
 * pointed_line, and text							*
 *										*
 ********************************************************************************/
{ 
    item1 *a = NULL; //curr
    item1 *b = NULL; //next
    item1 *c = NULL; //begin
    item1 *e = NULL; //end
    item1 *tmp = NULL;  
    
   /* 
    * the `c' node precedes the `a' and `e' node pointing up 
    * the node to which the comparisons are being made.  
    */    
    while (e != curr2->next) { 
 	c = a = curr2;
 	b = a->next;

  	while(a != e) {

	    if ( (a->el.hdr.grptyp > b->el.hdr.grptyp)
		|| (a->el.hdr.grptyp == b->el.hdr.grptyp 
			&& a->el.hdr.grpnum > b->el.hdr.grpnum)
		|| (a->el.hdr.grptyp == b->el.hdr.grptyp 
			&& a->el.hdr.grpnum == b->el.hdr.grpnum
			&& a->el.hdr.grptyp != 1
			&& a->el.hdr.grptyp != 2
			&& a->el.hdr.vg_type > b->el.hdr.vg_type)
		/* for group cloud or turb */
		|| (a->el.hdr.grptyp == b->el.hdr.grptyp     //not cloud, but in cloud's group  
			&& a->el.hdr.grpnum == b->el.hdr.grpnum
			&& (a->el.hdr.grptyp == 1 || a->el.hdr.grptyp == 2) 
			&& b->el.hdr.vg_type != LINE_ELM 
			&& b->el.hdr.vg_type != SPLN_ELM
			&& b->el.hdr.vg_type != SPTX_ELM)

		|| (a->el.hdr.grptyp == b->el.hdr.grptyp       
			&& a->el.hdr.grpnum == b->el.hdr.grpnum
			&& (a->el.hdr.grptyp == 1 || a->el.hdr.grptyp == 2)
			
			&& (a->el.hdr.vg_type > b->el.hdr.vg_type 
			    || (a->el.hdr.vg_type == SPLN_ELM && b->el.hdr.vg_type == SPLN_ELM &&
				a->el.elem.spl.info.spltyp > b->el.elem.spl.info.spltyp)) )
		) {
		
		
    		if(a == curr2) {
     		    tmp = b -> next;
     		    b->next = a;
     		    a->next = tmp;
     		    curr2 = b;
     		    c = b;		    
    		} else {
     		    tmp = b->next;
     		    b->next = a;
     		    a->next = tmp;
     		    c->next = b;
     		    c = b;
    		}
   	    } else {
    		c = a;
    		a = a->next;
   	    }

   	    b = a->next;

   	    if(b == e) 
		e = a;
	    
  	}
    }

    return curr2;
}

/*---------------------------------------------------------------------*/

item1 *llist_bubble_sort_cloud_turb( item1 *curr2 )
/*********************************************************************************
 * llist_bubble_sort								 *
 *										 *
 * Bubble sort on linked list for cloud and turb.  				 *
 * Continue sort pointed_line and text, so closed/turb line and text are together*
 * Also move grptyp=0 elements got from contour to top				 *
 *********************************************************************************/
{ 
    item1 *a = NULL; //curr
    item1 *b = NULL; //next
    item1 *c = NULL; //begin
    item1 *e = NULL; //end
    item1 *tmp = NULL;  

    /* 
     * the `c' node precedes the `a', and `e' node pointing up 
     * the node to which the comparisons are being made.  
     */
   while (e != curr2->next) {
	c = a = curr2;
 	b = a->next;

  	while(a != e) {
  
	    if( a->el.hdr.grptyp > b->el.hdr.grptyp
		|| (a->el.hdr.grptyp == b->el.hdr.grptyp 
			&& a->el.hdr.grpnum > b->el.hdr.grpnum)
		/*|| (a->el.hdr.grptyp == b->el.hdr.grptyp 
			&& a->el.hdr.grpnum == b->el.hdr.grpnum
			&& a->el.hdr.grptyp != 1
			&& a->el.hdr.grptyp != 2
			&& a->el.hdr.vg_type > b->el.hdr.vg_type)
		
		|| (a->el.hdr.grptyp == b->el.hdr.grptyp     //not cloud, but in cloud's group  
			&& a->el.hdr.grpnum == b->el.hdr.grpnum
			&& (a->el.hdr.grptyp == 1 || a->el.hdr.grptyp == 2) 
			&& b->el.hdr.vg_type != LINE_ELM 
			&& b->el.hdr.vg_type != SPLN_ELM
			&& b->el.hdr.vg_type != SPTX_ELM) */
		/* for grouping cloud and turb */
		|| (a->el.hdr.grptyp == b->el.hdr.grptyp       
			&& a->el.hdr.grpnum == b->el.hdr.grpnum
			&& (a->el.hdr.grptyp == 1 || a->el.hdr.grptyp == 2)
			&& a->el.hdr.vg_type != LINE_ELM 
			&& a->el.elem.spl.info.spltyp != 3 
			&& ( (a->el.hdr.vg_type == SPLN_ELM && b->el.hdr.vg_type == SPLN_ELM &&
			        /* distance between one small group */
				fabs(a->el.elem.spl.latlon[a->el.elem.spl.info.numpts] - b->el.elem.spl.latlon[b->el.elem.spl.info.numpts])
				+ fabs(a->el.elem.spl.latlon[0] - b->el.elem.spl.latlon[0]) >3  &&
				/* distance between this point to origin. avoid sort back and forth */
				fabs(a->el.elem.spl.latlon[0]) + fabs(a->el.elem.spl.latlon[a->el.elem.spl.info.numpts])
				> fabs(b->el.elem.spl.latlon[0]) + fabs(b->el.elem.spl.latlon[b->el.elem.spl.info.numpts]) )
			    || (a->el.hdr.vg_type == SPLN_ELM && b->el.hdr.vg_type == SPTX_ELM && 
				fabs(a->el.elem.spl.latlon[a->el.elem.spl.info.numpts] - b->el.elem.spt.info.lon)
				+ fabs(a->el.elem.spl.latlon[0] - b->el.elem.spt.info.lat) >3 )

			    || (a->el.hdr.vg_type == SPTX_ELM && b->el.hdr.vg_type == SPTX_ELM)) )
		) {

		
    		if(a == curr2) {
     		    tmp = b -> next;
     		    b->next = a;
     		    a->next = tmp;
     		    curr2 = b;
     		    c = b;
		   
    		} else {
     		    tmp = b->next;
     		    b->next = a;
     		    a->next = tmp;
     		    c->next = b;
     		    c = b;
    		}
		
   	    } else {
    		c = a;
    		a = a->next;
   	    }

   	    b = a->next;

   	    if(b == e)
    	    	e = a;
  	}
    }

    return curr2;
}

/*---------------------------------------------------------------------*/

item1 *llist_bubble_sort_contour_color( item1 *curr2) // char *tblArray[], int  tblArrayLen)
/********************************************************************************
 * llist_bubble_sort_contour_color						*
 *										*
 * Bubble sort for contours: Put same color contour next to each other.       	*
 *			     Move only line or text in a group out of contours.	*
 ********************************************************************************/
{
    item1 *a = NULL; //curr
    item1 *b = NULL; //next
    item1 *c = NULL; //begin
    item1 *e = NULL; //end
    item1 *tmp = NULL;  

    while (e != curr2->next) {
	c = a = curr2;
 	b = a->next;

  	while(a != e) {
  
	    if( a->el.hdr.grptyp ==8 && b->el.hdr.grptyp ==8	 
		    && (a->el.hdr.min_col > b->el.hdr.min_col || a->next == e)
		) {		
				
    		if(a == curr2) {
     		    tmp = b -> next;
     		    b->next = a;
     		    a->next = tmp;
     		    curr2 = b;
     		    c = b;
		   
    		} else {
     		    tmp = b->next;
     		    b->next = a;
     		    a->next = tmp;
     		    c->next = b;
     		    c = b;
    		}
		
   	    } else {
    		c = a;
    		a = a->next;
   	    }

   	    b = a->next;
   	    if(b == e)
    	    	e = a;
  	}
    }

    return curr2;
}

/*---------------------------------------------------------------------*/

item1 *llist_bubble_sort_contour_group( item1 *curr2 )
/********************************************************************************
 * llist_bubble_sort_contour_group						*
 *										*
 * Bubble sort on same groupNum, sort to line, text, line, text ...		*
 *										*
 ********************************************************************************/
{ 
    item1 *tmp = NULL;
    item1 *head = NULL;  

    head = curr2;
    while (curr2 != NULL) { 
 	
	if( isLineLike(curr2->el.hdr.vg_type) == 1
	    && curr2->next !=NULL 
	    && isLineLike(curr2->next->el.hdr.vg_type) == 1
	    && curr2->el.hdr.grptyp == curr2->next->el.hdr.grptyp 
	    && curr2->el.hdr.grpnum == curr2->next->el.hdr.grpnum
	    && curr2->el.hdr.grptyp == 8 ) { 

	    item1 *curr1;	     
	    curr1 = curr2; /*curr2 is first line to compare*/
	    int lineCnt = 0;
	    int textCnt = 0;
	    int grptyp = curr1->el.hdr.grptyp; //init grptyp for this group
	    int grpnum = curr1->el.hdr.grpnum;
	    
	    item1 *lastLin = NULL;
	    item1 *lastText = NULL;
	    
	    
	    while (curr1 != NULL && isLineLike(curr1->el.hdr.vg_type) == 1 //curr1 = (nil)
		&& curr1->el.hdr.grptyp == grptyp && curr1->el.hdr.grpnum == grpnum) {
		lastLin = curr1;
		curr1 = curr1->next;
		lineCnt = lineCnt +1;
	    }
	    
	    while (curr1 != NULL && curr1->el.hdr.vg_type == SPTX_ELM
		&& curr1->el.hdr.grptyp == grptyp && curr1->el.hdr.grpnum == grpnum) {
		lastText = curr1;
		curr1 = curr1->next;
		textCnt = textCnt +1; 
	    }


	    		
	    /* calculate distance */
	    item1 *minDistPt = NULL;
	    item1 *prevMinPt = NULL;
	    item1 *text = NULL; //text pointer in the line, text group
	    float minDist= 99999999;
	    float minDistTexts = 99999999;
	    float dist = 0;
	    int numpts;
	    int i = 0;
	    int nearest_vrt, next_vrt, iret;
	    float nx, ny;
	    
	    text = lastLin;

	    if (lastText != NULL && lineCnt <= textCnt) {
	    	while (text != NULL && text->next != NULL && text != lastText ) { // from lastLin(text) to lastText is the text element's number

		    numpts = curr2->el.elem.lin.info.numpts;

    		    for (i=0; i<numpts; i++){
		    
		    	cgr_segdist ( &numpts, 
			&(curr2->el.elem.lin.latlon[i]), &(curr2->el.elem.lin.latlon[numpts+i]), 
	    		&(text->next->el.elem.spt.info.lat), &(text->next->el.elem.spt.info.lon),
     	    		&dist, &nearest_vrt, &next_vrt, &nx, &ny, &iret );
 
 		    	if (dist < minDist) 
	    		minDist = dist;		
		    }

		    if (minDist < minDistTexts) {
	    	    	minDistTexts = minDist;
		    	minDistPt = text->next;
		    	prevMinPt = text;	 	 
		    }

		    text = text->next; 
	    	}
	    
	    	/* swap. text added 1 from last while, prevMinPt didn't. */
	    	if (//text != NULL //&& lastText != NULL 
		 minDistPt != NULL && prevMinPt != NULL && prevMinPt != lastText) { 

	    	    //if (minDistPt->next !=NULL){
	    		tmp = minDistPt->next;
	    		minDistPt->next = curr2->next;
	    		curr2->next = minDistPt;
     	    		prevMinPt->next = tmp;

	    	    /*}
	    	    else if (minDistPt->next ==NULL){
	    	
		    	curr2->next = minDistPt;
	    	    	minDistPt->next = prevMinPt;	    	    
     	    	    	prevMinPt->next = NULL;
	    	    }*/		
	    	}		
	    }//if (lastText != NULL) 

	    else if (lastText != NULL && lineCnt > textCnt) { 
	    //Text should be same to make sense. Copy text to be after each line
		
		item1 * copyText = (item1 *)malloc(sizeof(item1));
    		copyText->el = text->next->el;
		copyText->next = NULL;

		tmp = curr2->next;
		curr2->next = copyText; 
		copyText->next = tmp;		
	    }
	}

	curr2 = curr2->next;
    }	//while    
	
    
    curr2 = head;
    return curr2;
}

/*---------------------------------------------------------------------*/

char* getGroupType(int grptyp) 
/********************************************************************************
 * getGroupType									*
 *										*
 * Get the name of the group type.						*
 *										*
 * Note: 									*
 *    This could be replaced by ces_gtgnam ( int grpid, char *grpnam, int *iret)*
 *										*
 ********************************************************************************/
{ 

    char *outlookType = "";

    if (grptyp == 1)
	outlookType = "CLOUD";
    else if (grptyp == 2)
	outlookType = "TURB";
    else if (grptyp == 3)
	outlookType = "FRONT";
    else if (grptyp == 4)
	outlookType = "JETS";
    else if (grptyp == 5)
	outlookType = "HIGH";
    else if (grptyp == 6)
	outlookType = "LOW";
    else if (grptyp == 7)
	outlookType = "OUTLOOK";
    else if (grptyp == 8)
	outlookType = "LABEL";
    else if (grptyp == 9)
	outlookType = "TROPICL";
    else if (grptyp == 10)
	outlookType = "STNMDL";
    else if (grptyp == 11)
	outlookType = "MRFSTN";
    else if (grptyp == 12)
	outlookType = "HAILOTLK";
    else if (grptyp == 13)
	outlookType = "TORNOTLK";
    else if (grptyp == 14)
	outlookType = "WINDOTLK";
    else if (grptyp == 15)
	outlookType = "TOTL_SVR";
    else if (grptyp == 16)
	outlookType = "FIREOUTL";
    else if (grptyp == 17)
	outlookType = "CATG_SVR";
    else if (grptyp == 18)
	outlookType = "MESO_DSC";
    else if (grptyp == 24)
	outlookType = "TSTMOLK";
    else if (grptyp == 25)
	outlookType = "EXT_SVR";
    else if (grptyp == 26)
	outlookType = "EXT_FIRE";
    else if (grptyp == 27)
	outlookType = "ISOBARS";
    else if (grptyp == 28)
	outlookType = "HI_FCST";
    else if (grptyp == 29)
	outlookType = "LO_FCST";
    else if (grptyp == 30)
	outlookType = "WHFT";
    else if (grptyp == 31)
	outlookType = "WHM";
    else if (grptyp == 32)
	outlookType = "WPER";
    else if (grptyp == 33)
	outlookType = "PROB";
    else if (grptyp == 34)
	outlookType = "ENH20"; 
    else if (grptyp == 35)
	outlookType = "ENH00";
    else if (grptyp == 36)
	outlookType = "ENH04";
    else if (grptyp == 37)
	outlookType = "ENH12";
    else if (grptyp == 38)
	outlookType = "ENH16";
    else
	outlookType = "";

    return outlookType;
}

/*---------------------------------------------------------------------*/

char* getSymType(VG_DBStruct *el) 
/********************************************************************************
 * getSymType									*
 *										*
 * Get the name of the symbol type.						*
 *										*
 *										*
 ********************************************************************************/
{
    char *pgenType = "";

    if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "HIGH_PRESSURE_H";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "LOW_PRESSURE_L";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "PRESENT_WX_005";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "PRESENT_WX_010";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 45.0 )
	pgenType = "PRESENT_WX_045";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 51.0 )
	pgenType = "PRESENT_WX_051";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 56.0 )
	pgenType = "PRESENT_WX_056";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 61.0 )
	pgenType = "PRESENT_WX_061";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 63.0 )
	pgenType = "PRESENT_WX_063";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 65.0 )
	pgenType = "PRESENT_WX_065";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 66.0 )
	pgenType = "PRESENT_WX_066";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 71.0 )
	pgenType = "PRESENT_WX_071";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 73.0 )
	pgenType = "PRESENT_WX_073";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 75.0 )
	pgenType = "PRESENT_WX_075";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 79.0 )
	pgenType = "PRESENT_WX_079";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 80.0 )
	pgenType = "PRESENT_WX_080";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 85.0 )
	pgenType = "PRESENT_WX_085";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 89.0 )
	pgenType = "PRESENT_WX_089";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 95.0 )
	pgenType = "PRESENT_WX_095";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 105.0 )
	pgenType = "PRESENT_WX_105";
    else if ( el->hdr.vg_type == WXSYM_ELM && el->elem.sym.data.code[0] == 201.0 )
	pgenType = "PRESENT_WX_201";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 25.0 )
	pgenType = "TROPICAL_STORM_NH";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 26.0 )
	pgenType = "HURRICANE_NH";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 27.0 )
	pgenType = "TROPICAL_STORM_SH";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 28.0 )
	pgenType = "HURRICANE_SH";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 32.0 )
	pgenType = "STORM_CENTER";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 33.0 )
	pgenType = "TROPICAL_DEPRESSION";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 34.0 )
	pgenType = "TROPICAL_CYCLONE";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 35.0 )
	pgenType = "FLAME";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 36.0 )
	pgenType = "X_CROSS";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 38.0 )
	pgenType = "LOW_X_FILLED";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 39.0 )
	pgenType = "TROPICAL_STORM_NH_WPAC";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 40.0 )
	pgenType = "TROPICAL_STORM_SH_WPAC";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 41.0 )
	pgenType = "NUCLEAR_FALLOUT";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 43.0 )
	pgenType = "LETTER_A_FILLED";
    //else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 10 )
	//pgenType = "LETTER_C";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 45.0 )
	pgenType = "LETTER_C_FILLED";
    //else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 10 )
	//pgenType = "LETTER_X";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 47.0 )
	pgenType = "LETTER_X_FILLED";
    //else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 10 )
	//pgenType = "LETTER_N";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 49.0 )
	pgenType = "LETTER_N_FILLED";
    else if ( el->hdr.vg_type == SPSYM_ELM && el->elem.sym.data.code[0] == 50.0 )
	pgenType = "30_KT_BARB";
    else if ( el->hdr.vg_type == ICSYM_ELM && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "ICING_09";
    else if ( el->hdr.vg_type == ICSYM_ELM && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "ICING_10";
    else if ( el->hdr.vg_type == PWSYM_ELM && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "PAST_WX_09";

    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "PAST_WX_09";

    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "PLUS_SIGN";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "OCTAGON";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "TRIANGLE";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "BOX";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "SMALL_X";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "Z_WITH_BAR";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "X_WITH_TOP_BAR";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "DIAMOND";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "UP_ARROW";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "Y";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 11.0 )
	pgenType = "BOX_WITH_DIAGONALS";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "ASTERISK";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "HOURGLASS_X";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 14.0 )
	pgenType = "STAR";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 15.0 )
	pgenType = "DOT";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 16.0 )
	pgenType = "LARGE_X";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 17.0 )
	pgenType = "FILLED_OCTAGON";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 18.0 )
	pgenType = "FILLED_TRIANGLE";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 19.0 )
	pgenType = "FILLED_BOX";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 20.0 )
	pgenType = "FILLED_DIAMOND";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 21.0 )
	pgenType = "FILLED_STAR";
    else if ( el->hdr.vg_type == MARK_ELM && el->elem.sym.data.code[0] == 22.0 )
	pgenType = "MINUS_SIGN";

    else if ( el->hdr.vg_type == FRONT_ELM)
	pgenType = "Front";
    else 
	pgenType = "";


    return pgenType;
}

int isSymbolLike(int vg_type) {
/********************************************************************************
 * isSymbolLike									*
 *										*
 * is symbol .						                *
 *										*
 ********************************************************************************/
    int is = 0;
    if (  vg_type == CTSYM_ELM || vg_type ==ICSYM_ELM
	|| vg_type == PTSYM_ELM || vg_type == PWSYM_ELM
	|| vg_type == SKSYM_ELM || vg_type == SPSYM_ELM
	|| vg_type == TBSYM_ELM || vg_type == WXSYM_ELM ) {

        is = 1;
    }
    return is;
}

int isSymbolMarkLike(int vg_type) {
/********************************************************************************
 * isSymbolMarkLike									*
 *										*
 * is symbol or mark or combo-symbol.						                *
 *										*
 ********************************************************************************/
    int is = 0;
    if (  vg_type == CTSYM_ELM || vg_type ==ICSYM_ELM
	|| vg_type == PTSYM_ELM || vg_type == PWSYM_ELM
	|| vg_type == SKSYM_ELM || vg_type == SPSYM_ELM
	|| vg_type == TBSYM_ELM || vg_type == WXSYM_ELM
	|| vg_type == MARK_ELM  || vg_type == CMBSY_ELM )	

        is = 1;

    return is;
}

int isLineLike(int vg_type) {
/********************************************************************************
 * isLineLike									*
 *										*
 * is line, spline or circle.						        *
 *										*
 ********************************************************************************/
    int is = 0;
    if (vg_type == LINE_ELM || vg_type == SPLN_ELM || vg_type == CIRCLE_ELM) {

        is = 1;
    }
    return is;
}

int isOutlookLineLike(int vg_type) {
/********************************************************************************
 * isOutlookLineLike									*
 *										*
 * is lineor spline.						        *
 *										*
 ********************************************************************************/
    int is = 0;
    if (vg_type == LINE_ELM || vg_type == SPLN_ELM ) {

        is = 1;
    }
    return is;
}

int isOutlookLike(int grp_type) {
/********************************************************************************
 * isOutlookLike									*
 *										*
 * is following outlooks.						        *
 *										*
 ********************************************************************************/
    int is = 0;
    if (grp_type == 7 || grp_type == 9 || grp_type == 12 || grp_type == 13
	|| grp_type == 14 || grp_type == 16 || grp_type == 24) {

        is = 1;
    }
    return is;
}
/*---------------------------------------------------------------------*/

/*
void insert(item1 ** head, int sd){
    item1 ** ptr2=head;
    
    if(*ptr2==NULL){
	(*ptr2)=(item1*)malloc(sizeof(item1));
	(*ptr2)->sd=sd;
	(*ptr2)->next=NULL;
    }else{
	while(*ptr2 != NULL){
		ptr2 = &(*ptr2)->next;
	}
	(*ptr2)=(item1*)malloc(sizeof(item1));
	(*ptr2)->sd=sd;
	(*ptr2)->next=NULL;
    }
}
*/

/*
double getMinDistance(int n, double[] linx, double[] liny, double texx, double texy, int n, double distance){
    double temp = 0;
    int i=0;
    for (i=0; i<n; i++){
    	temp = (linx[i] - texx)*(linx[i] - texx) + (liny[n+i] - texy)*(liny[n+i] - texy);
	if (i==0)
	    distance  = temp;
	else if (temp < distance)
	    distance = temp;
    }
    return distance;
}
*/
