#include "cvgcmn.h"

/* Local prototype */
char*	category (VG_DBStruct *el );
char*	getColorTag (int major );
char*	getIalign (int align );
char*	getSztext (float sztext );
char*   getFontStyle(int txfn, char *font, char *style);
char*	fill_pattern (VG_DBStruct *el );
char*   getEscape(char* content, char* esContent);
char*   trimwhitespace(char *str);
char*	time_rp ( char* sin, char* sout );
char*	time_watch ( char* sin, char *sout );
char*	time_watch_stat ( char* sin, char *sout );
float   checkLongitude(float lon);

void	fhed2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	hdr2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	frt2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	lin2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	spl2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	wbx2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	wsm2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	sym2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	wnd2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	txt2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	spt2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	cir2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	trk2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	sig2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	ccf2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	lst2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	ash2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	vol2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	jet2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	gfa2tag ( VG_DBStruct *el, char *buffer, int *iret );
void    sgwx2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	tca2tag ( VG_DBStruct *el, char *buffer, int *iret );


int	cvg_v2x ( VG_DBStruct *el, char *buffer, int *iret )
/********************************************************************************
 * cvg_v2t                                                          		*
 *                                                                      	*
 * This function converts a VGF element into Xml strings.	        	*
 *                                                                      	*
 * Input parameters:                                                    	*
 *      *buffer         char          Element in tag format			*
 *                                                                      	*
 * Output parameters:                                                   	*
 *      *el             VG_DBStruct   PGEN Element				*
 *      *iret           int           Return code                       	*
 *                                                                      	*
 *										*
 * Q. Zhou/Chug         02/10	re-write from cvgv2t.c to convert to xml	*
 * Q. Zhou/Chug         03/10   added func & code for group			*
 * Q. Zhou/Chug         10/10   added indentation for group       		*
 * Q. Zhou/Chug         11/10   added volc, ccfp...               		*
 * Q. Zhou/Chug         12/10   Modified front type, width        		*
 *                              Modified Cloud text, all text font		*
 * Q. Zhou/Chug		02/11	Modified tca's numbrkPoint			*
 *				Modified gfa for each hazard      		*
 *                              Modified watch to relocate status msg   	*
 *                              Added 0 points checking, and \r\n handle	*
 * Q. Zhou/Chug		04/11   Added getEscape(). Added conversion for 	*
 *				symbols not on screen. Got text offset		*
 *				converted. Fixed color minor affection. 	* 
 *                      06/01   Add isOutlook                           	* 
 * Q.Zhou /Chug		11/11   Added displayType to Text, flipSide to line   	*
 *                              Changed CCF due to collection           	*
 *                              Handled vector width>100 situation              *
 * Q.Zhou /Chug		12/11   Handled |longitude|>180 situation               *
 * L. Hinson/AWC        07/12   Add sgwx2tag for SGWX_ELM.                      *
 * J. Wu/SGT        	11/13   Set special line 24/25 to KINK_LINE_1 & 2.      *
 * Q.Zhou /SGT          02/14   Adjusted for 64 bit				*
 * J. Wu/SGT        	10/14   Sanity check for line/front/special line with.  *
 *                              points less than 2.				*
 * J. Wu/SGT        	02/15   R6158 preserve iwidth/ithw for Text		*
 ********************************************************************************/
{
int	ier, npts;
/*---------------------------------------------------------------------*/

    *iret = 0;
   
    switch ( el->hdr.vg_type )  {

	case	FILEHEAD_ELM: /*22*/
		fhed2tag ( el, buffer, &ier );
		break;

	case	LINE_ELM:     /*1*/ 
                npts = el->elem.lin.info.numpts; 
                if ( npts >= 2 ) {
		    lin2tag ( el, buffer, &ier );
                }
		break;

	case	FRONT_ELM:    /*2*/
                npts = el->elem.frt.info.numpts; 
                if ( npts >= 2 ) {
		   frt2tag ( el, buffer, &ier ); 
                }
		break;

	case	SPLN_ELM:     /*20*/
                npts = el->elem.spl.info.numpts; 
                if ( npts >= 2 ) {
		    spl2tag ( el, buffer, &ier );
                }
		break;

	case	WBOX_ELM:     /*6*/
		wbx2tag ( el, buffer, &ier );
		break;

	case	WSM_ELM:
		wsm2tag ( el, buffer, &ier );
		break;

	case	WXSYM_ELM: /*5*/
	case	CTSYM_ELM: /*10*/
	case	ICSYM_ELM: /*11*/
	case	PTSYM_ELM: /*12*/
	case	PWSYM_ELM: /*13*/
	case	SKSYM_ELM: /*14*/
	case	SPSYM_ELM: /*15*/
	case	TBSYM_ELM: /*16*/
	case	MARK_ELM:  /*19*/
	case	CMBSY_ELM: /*25*/
		sym2tag ( el, buffer, &ier );
		break;

	case	BARB_ELM: /*8*/ 
	case	ARROW_ELM:/*9*/
	case	DARR_ELM: /*23*/
	case	HASH_ELM: /*24*/
		wnd2tag ( el, buffer, &ier );
		break;

	case	TEXT_ELM:
	case	TEXTC_ELM:
		txt2tag ( el, buffer, &ier );
		break;

	case	SPTX_ELM: /*21*/
		spt2tag ( el, buffer, &ier );
		break;

	case	CIRCLE_ELM: /*4*/
		cir2tag ( el, buffer, &ier );
		break;

	case	TRKSTORM_ELM: /*26*/
		trk2tag ( el, buffer, &ier );
		break;

	case	SIGINTL_ELM:  /*27*/
	case	SIGNCON_ELM:  /*28*/
	case	SIGCONV_ELM:  /*29*/
	case	SIGOUTL_ELM:  /*30*/
	case	SIGAIRM_ELM:  /*31*/
		sig2tag ( el, buffer, &ier );
		break;

	case	SIGCCF_ELM:   /*32*/
		ccf2tag ( el, buffer, &ier );
		break;

	case	LIST_ELM:  /*34*/
		lst2tag ( el, buffer, &ier );
		break;

	case	ASHCLD_ELM: /*36*/
		ash2tag ( el, buffer, &ier );
		break;

	case	VOLC_ELM: /*35*/
		vol2tag ( el, buffer, &ier );
		break;

	case	JET_ELM:  /*37*/
		jet2tag ( el, buffer, &ier );
		break;

	case	GFA_ELM:  /*38*/
		gfa2tag ( el, buffer, &ier );
		break;

        case    SGWX_ELM: /*43*/
                sgwx2tag ( el, buffer, &ier );
                break;

	case	TCA_ELM:  /*39*/
		tca2tag ( el, buffer, &ier );
		break;

	default:
		*iret = -1;
	break;

    }

    return ( 0 );

}

/*---------------------------------------------------------------------*/
char*	category (VG_DBStruct *el )
{
	char * cate;

	if ( el->hdr.vg_type == LINE_ELM )
	    cate = "Lines";
	else if ( el->hdr.vg_type == FRONT_ELM )
	    cate = "Front";
	else if ( el->hdr.vg_type == CIRCLE_ELM )
	    cate = "Arc";
	else if ( el->hdr.vg_type == WXSYM_ELM )
	    cate = "Symbol"; 
	else if ( el->hdr.vg_type == WBOX_ELM )
	    cate = "Watch"; 
	else if ( el->hdr.vg_type == BARB_ELM )
	    cate = "Vector";
	else if ( el->hdr.vg_type == ARROW_ELM )
	    cate = "Vector";
	else if ( el->hdr.vg_type == DARR_ELM )
	    cate = "Vector";
	else if ( el->hdr.vg_type == HASH_ELM )
	    cate = "Vector";
	else if ( el->hdr.vg_type == CTSYM_ELM )
	    cate = "Symbol"; //"CLOUD SYMBOL";
	else if ( el->hdr.vg_type == ICSYM_ELM )
	    cate = "Symbol"; //"ICING SYMBOL";
	else if ( el->hdr.vg_type == PTSYM_ELM )
	    cate = "Symbol"; //"PRESSURE TENDENCY SYMBOL";
	else if ( el->hdr.vg_type == PWSYM_ELM )
	    cate = "Symbol"; //"PAST WEATHER SYMBOL";
	else if ( el->hdr.vg_type == SKSYM_ELM )
	    cate = "Symbol"; //"SKY COVER SYMBOL";
	else if ( el->hdr.vg_type == SPSYM_ELM )
	    cate = "Symbol"; //SPECIAL SYMBOL
	else if ( el->hdr.vg_type == TBSYM_ELM )
	    cate = "Symbol"; //"TURBULENCE SYMBOL"; 
	else if ( el->hdr.vg_type == TEXT_ELM )
	    cate = "Text";  
	else if ( el->hdr.vg_type == TEXTC_ELM )
	    cate = "CENTERED TEXT";
	else if ( el->hdr.vg_type == MARK_ELM )
	    cate = "Marker";
	else if ( el->hdr.vg_type == SPLN_ELM )
	    cate = "Lines";
	else if ( el->hdr.vg_type == SPTX_ELM )
	    cate = "Text"; 
	else if ( el->hdr.vg_type == FILEHEAD_ELM)
	    cate = "File Header";  
	else if ( el->hdr.vg_type == CMBSY_ELM)
	    cate = "Combo"; 
	else if ( el->hdr.vg_type == TRKSTORM_ELM)
	    cate = "Track";
	else if ( el->hdr.vg_type == SIGAIRM_ELM || el->hdr.vg_type == SIGCONV_ELM
		|| el->hdr.vg_type == SIGINTL_ELM || el->hdr.vg_type == SIGNCON_ELM
		|| el->hdr.vg_type == SIGOUTL_ELM || el->hdr.vg_type == SIGCCF_ELM
		|| el->hdr.vg_type == VOLC_ELM || el->hdr.vg_type == ASHCLD_ELM )
	    cate = "SIGMET";
		
	else if ( el->hdr.vg_type == LIST_ELM)
	    cate = "LIST";
	
	else if ( el->hdr.vg_type == GFA_ELM
	    || el->hdr.vg_type == TCA_ELM
	    || el->hdr.vg_type == JET_ELM)
	    cate = "MET";

	else if ( el->hdr.vg_type == TCERR_ELM)
	    cate = "TC";
	else if ( el->hdr.vg_type == TCTRK_ELM)
	    cate = "TCT";
	else if ( el->hdr.vg_type == TCBKL_ELM)
	    cate = "TCB";
	else
	    cate = "UNKNOWN";
	return cate;
}

/*---------------------------------------------------------------------*/
char*	getColorTag (int major )
{
	char *col_maj;

	if ( major == 1 )
	    col_maj = "<Color red=\"255\" green=\"228\" blue=\"220\" alpha=\"255\"/>";
	else if ( major == 2 )
	    col_maj = "<Color red=\"255\" green=\"0\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 3 )
	    col_maj = "<Color red=\"0\" green=\"255\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 4 )
	    col_maj = "<Color red=\"0\" green=\"0\" blue=\"255\" alpha=\"255\"/>";
	else if ( major == 5 )
	    col_maj = "<Color red=\"255\" green=\"255\" blue=\"0\" alpha=\"255\"/>";

	else if ( major == 6 )
	    col_maj = "<Color red=\"0\" green=\"255\" blue=\"255\" alpha=\"255\"/>";
	else if ( major == 7 )
	    col_maj = "<Color red=\"255\" green=\"0\" blue=\"255\" alpha=\"255\"/>";
	else if ( major == 8 )
	    col_maj = "<Color red=\"139\" green=\"71\" blue=\"38\" alpha=\"255\"/>";
        else if ( major == 9 )
	    col_maj = "<Color red=\"255\" green=\"130\" blue=\"71\" alpha=\"255\"/>";
	else if ( major == 10 )
	    col_maj = "<Color red=\"255\" green=\"165\" blue=\"79\" alpha=\"255\"/>";

	else if ( major == 11 )
	    col_maj = "<Color red=\"255\" green=\"174\" blue=\"185\" alpha=\"255\"/>";
	else if ( major == 12 )
	    col_maj = "<Color red=\"255\" green=\"106\" blue=\"106\" alpha=\"255\"/>";
	else if ( major == 13 )
	    col_maj = "<Color red=\"238\" green=\"44\" blue=\"44\" alpha=\"255\"/>";
	else if ( major == 14 )
	    col_maj = "<Color red=\"139\" green=\"0\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 15 )
	    col_maj = "<Color red=\"205\" green=\"0\" blue=\"0\" alpha=\"255\"/>";

	else if ( major == 16 )
	    col_maj = "<Color red=\"238\" green=\"64\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 17 )
	    col_maj = "<Color red=\"255\" green=\"127\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 18 )
	    col_maj = "<Color red=\"205\" green=\"133\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 19 )
	    col_maj = "<Color red=\"255\" green=\"215\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 20 )
	    col_maj = "<Color red=\"238\" green=\"238\" blue=\"0\" alpha=\"255\"/>";

	else if ( major == 21 )
	    col_maj = "<Color red=\"127\" green=\"255\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 22 )
	    col_maj = "<Color red=\"0\" green=\"205\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 23 )
	    col_maj = "<Color red=\"0\" green=\"139\" blue=\"0\" alpha=\"255\"/>";
	else if ( major == 24 )
	    col_maj = "<Color red=\"16\" green=\"78\" blue=\"139\" alpha=\"255\"/>";
	else if ( major == 25 )
	    col_maj = "<Color red=\"30\" green=\"144\" blue=\"255\" alpha=\"255\"/>";

	else if ( major == 26 )
	    col_maj = "<Color red=\"0\" green=\"178\" blue=\"238\" alpha=\"255\"/>";
	else if ( major == 27 )
	    col_maj = "<Color red=\"0\" green=\"238\" blue=\"238\" alpha=\"255\"/>";
	else if ( major == 28 )
	    col_maj = "<Color red=\"137\" green=\"104\" blue=\"205\" alpha=\"255\"/>";
	else if ( major == 29 )
	    col_maj = "<Color red=\"145\" green=\"44\" blue=\"238\" alpha=\"255\"/>";
	else if ( major == 30 )
	    col_maj = "<Color red=\"139\" green=\"0\" blue=\"139\" alpha=\"255\"/>";

	else if ( major == 31 )
	    col_maj = "<Color red=\"255\" green=\"255\" blue=\"255\" alpha=\"255\"/>";
	else if ( major == 32 )
	    col_maj = "<Color red=\"0\" green=\"0\" blue=\"0\" alpha=\"255\"/>";
	else
	    col_maj = "<Color red=\"255\" green=\"255\" blue=\"255\" alpha=\"255\"/>";

	return col_maj;
}

/*---------------------------------------------------------------------*/

char*	fill_pattern (VG_DBStruct *el )
{
	char *pattern;

	if ( el->hdr.filled == 0 )      /*filled !=1  */
	    pattern = "SOLID"; 		/*default, should be NONE, but can't read it*/
	else if ( el->hdr.filled == 2 )
	    pattern = "SOLID";
	else if ( el->hdr.filled == 3 )
	    pattern = "FILL_PATTERN_1";
	else if ( el->hdr.filled == 4 )
	    pattern = "FILL_PATTERN_2";
	else if ( el->hdr.filled == 5 )
	    pattern = "FILL_PATTERN_3";
	else if ( el->hdr.filled == 6 )
	    pattern = "FILL_PATTERN_4";
	else if ( el->hdr.filled == 7 )
	    pattern = "FILL_PATTERN_5";
	else if ( el->hdr.filled == 8 )
	    pattern = "FILL_PATTERN_6";
	else if ( el->hdr.filled == 9 )
	    pattern = "TRANSPARENCY";
	else if ( el->hdr.filled == 10 )
	    pattern = "FILL_PATTERN_0";
	else
	    pattern = "SOLID"; 

	return pattern;
}

char*	getEscape(char* content, char* esContent) 
{
	char* s = malloc(2);
	int contentSize = strlen(content);
	int i = 0;

	sprintf(esContent, "%s", "");
    	for( i = 0; i < contentSize; i++)
    	{
       	    char c = content[i]; 
       	    if(c == '<')
		sprintf(&(esContent[strlen(esContent)]), "%s", "&lt;");
            else if(c == '>')
          	sprintf(&(esContent[strlen(esContent)]), "%s", "&gt;");
            else if(c == '&')
          	sprintf(&(esContent[strlen(esContent)]), "%s", "&amp;");
            else if(c == '\"')
          	sprintf(&(esContent[strlen(esContent)]), "%s", "&quot;");
            else if(c == '\'')
          	sprintf(&(esContent[strlen(esContent)]), "%s", "&apos;");
            else {
		s[0] = c;
		s[1] = '\0';
		sprintf(&(esContent[strlen(esContent)]), "%s", s);		
          	//strcat (esContent, s);
	    }
    	}
    	free(s);
	return esContent;
}

char*	getIalign (int align )
{
	char *s_align;
    	if (align == 0)
            s_align = "CENTER";
    	else if (align == -1)
            s_align = "LEFT_JUSTIFY";
    	else if (align == 1)
            s_align = "RIGHT_JUSTIFY";
    	else 
            s_align = "CENTER";

    	return s_align;
}

char*	getSztext (float sztext )
{
	char *s_sztext;
    	if (sztext<=0.785) //0.714
	    s_sztext = "10.0";
    	else if (sztext>0.785 && sztext<=0.928) //0.857
	    s_sztext = "12.0";
    	else if (sztext>0.928 && sztext<=1.143) //1.0
	    s_sztext = "14.0";
    	else if (sztext>1.143 && sztext<=1.50) //1.286
	    s_sztext = "18.0";
    	else if (sztext>1.50 && sztext<=2.071) //1.714
	    s_sztext = "24.0";
    	else if (sztext>2.071 ) //2.429
	    s_sztext = "34.0";
    	else
	    s_sztext = "14.0";

    	return s_sztext;
}

char*   getFontStyle(int txfn, char *font, char *style) {
    if (txfn ==1) {
        strcpy(font, "Courier");
      	strcpy(style, "REGULAR");
    }
    else if (txfn ==11 ){
        strcpy(font, "Courier");
      	strcpy(style, "ITALIC");
    }
    else if (txfn ==21 ){
        strcpy(font, "Courier");
      	strcpy(style, "BOLD");
    }
    else if (txfn ==31 ){
        strcpy(font, "Courier");
      	strcpy(style, "BOLD_ITALIC");
    }
    else if (txfn ==2 ){
        strcpy(font, "Nimbus Sans L"); //Helvetica
      	strcpy(style, "REGULAR");
    }
    else if (txfn ==12 ){
        strcpy(font, "Nimbus Sans L");
      	strcpy(style, "ITALIC");
    }
    else if (txfn ==22 ){
        strcpy(font, "Nimbus Sans L");
      	strcpy(style, "BOLD");
    }
    else if (txfn ==32 ){
        strcpy(font, "Nimbus Sans L");
      	strcpy(style, "BOLD_ITALIC");
    }
    else if (txfn == 3) {
        strcpy(font, "Liberation Serif"); //Times
      	strcpy(style, "REGULAR");
    }
    else if (txfn == 13) {
        strcpy(font, "Liberation Serif");
      	strcpy(style, "ITALIC");
    }
    else if (txfn == 23) {
        strcpy(font, "Liberation Serif");
      	strcpy(style, "BOLD");
    }
    else if (txfn == 33) {
        strcpy(font, "Liberation Serif");
      	strcpy(style, "BOLD_ITALIC");
    }
    else {
        strcpy(font, "Courier");
      	strcpy(style, "REGULAR");
    }
    return 0;
}

char*	time_rp ( char* sin, char *sout )
{
    char *f;

    if (sin == NULL || *sin == '\0' || strcmp(sin, " ") == 0  )
	return strcpy(sout, "");

 
    f = (char *)malloc(2*sizeof(char));
    strncpy(f, sin, 2);
    *(f+2) = '\0';

    if (atoi(f) > 20)
    	strncpy(sout, "19", 2);
    else
    	strncpy(sout, "20", 2);
    free(f);

    strncpy( sout+2, sin, 2);
    strncpy( sout+4, "-", 1 );
    strncpy( sout+5, sin+2, 2 );
    strncpy( sout+7, "-", 1 );
    strncpy( sout+8, sin+4, 2 );
    strncpy( sout+10, "T", 1 );
    strncpy( sout+11, sin+7, 2 );
    strncpy( sout+13, ":", 1 );
    strncpy( sout+14, sin+9, 2 );
    strncpy( sout+16, ":", 1 );
    strncpy( sout+17, "00", 2 );

    *(sout+19) = '\0';  
    
    return sout;
}

char*	time_watch ( char* sin, char *sout ) /*sin=04/13/2010/0300/ */
{
    if (sin == NULL || *sin == '\0' || strcmp(sin, " ") == 0  )
	return strcpy(sout, "");

    memcpy( sout, sin+6, 4);
    memcpy( sout+4, "-", 1 );
    memcpy( sout+5, sin, 2 );
    memcpy( sout+7, "-", 1 );
    memcpy( sout+8, sin+3, 2 );
    memcpy( sout+10, "T", 1 );
    memcpy( sout+11, sin+11, 2 );
    memcpy( sout+13, ":", 1 );
    memcpy( sout+14, sin+13, 2 );
    memcpy( sout+16, ":00.000Z", 8 );
    
    *(sout+24) = '\0';  

    return sout;
}

char*	time_watch_stat ( char* sin, char *sout ) /*sin=130300 ddhhmm */
{
    if (sin == NULL || *sin == '\0' || strcmp(sin, " ") == 0  )
	return strcpy(sout, "");

    memcpy( sout, sin, 2 );
    memcpy( sout+2, "T", 1 );
    memcpy( sout+3, sin+2, 2 );
    memcpy( sout+5, ":", 1 );
    memcpy( sout+6, sin+4, 2 );
    memcpy( sout+8, ":00.000Z", 8 );
    *(sout+16) = '\0';

    return sout;
}


char *trimwhitespace(char *str)
{
  char *end;

  // Trim leading space
  while(isspace(*str)) str++;

  // Trim trailing space
  end = str + strlen(str) - 1;
  while(end > str && isspace(*end)) end--;

  // Write new null terminator
  *(end+1) = '\0';

  return str;

}

float   checkLongitude(float lon) {
    if (lon >180)
	lon = lon - 360;
    else if (lon < -180)
	lon = 360 - lon;

    return lon;
}

void	hdr2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  ELEMENT HEADER
     */
	sprintf( buffer,
	"          <vg_type>%d"
	"          <vg_class>%d"
	"          <delete>%d"
	"          <filled>%d"
	"          <closed>%d"
	"          <smooth>%d"
	"          <version>%d"
	"          <grptyp>%d"
	"          <grpnum>%d"
	"          <maj_col>%d"
	"          <min_col>%d"
	"          <recsz>%d"
	"          <range_min_lat>%6.2f"
	"          <range_min_lon>%7.2f"
	"          <range_max_lat>%6.2f"
	"          <range_max_lon>%7.2f",
	(int)el->hdr.vg_type,
	(int)el->hdr.vg_class,
	(int)el->hdr.delete,
	(int)el->hdr.filled,
	(int)el->hdr.closed,
	(int)el->hdr.smooth,
	(int)el->hdr.version,
	(int)el->hdr.grptyp,
	el->hdr.grpnum,
	el->hdr.maj_col,
	el->hdr.min_col,
	el->hdr.recsz,
	el->hdr.range_min_lat,
	el->hdr.range_min_lon,
	el->hdr.range_max_lat,
	el->hdr.range_max_lon
	);

}

void	fhed2tag ( VG_DBStruct *el, char *buffer, int *iret )
{

size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  FILEHEAD_ELM
     */
    char * xml_head = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>";
    sprintf( buffer, "%s\n", xml_head);
    blen = strlen(buffer);
    buffer = buffer + blen;

    char *products = "  <Products>\n";
    sprintf( buffer, "%s\n", products);
    blen = strlen(products);
    buffer = buffer + blen;

    char *product = "    <Product name=\"Default\" type=\"Default\" forecaster=\"Default\" center=\"Default\" onOff=\"true\" useFile=\"true\" filePath=\"\" fileName=\"\">\n";
   
    sprintf( buffer, "%s\n", product);
    blen = strlen(product);
    buffer = buffer + blen;

    char *layer = "      <Layer name=\"Default\" onOff=\"true\" monoColor=\"false\" filled=\"false\">\n";
    sprintf( buffer, "%s\n", layer);
    blen = strlen(layer);
    buffer = buffer + blen;

    char *color = "        <Color red=\"255\" green=\"255\" blue=\"0\" alpha=\"255\"/>\n";
    sprintf( buffer, "%s\n", color);
    blen = strlen(color);
    buffer = buffer + blen;

    char *de = "        <DrawableElement>\n";
    sprintf( buffer, "%s", de);
    blen = strlen(de);
    buffer = buffer + blen;

}

/*=====================================================================*/

void	frt2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
    int	ii, npts;
    size_t	blen;
    char *pgenType;
    int code;
    char *indent = "";

    /*
     *  FRONT_ELM
     */

    if (el->hdr.grptyp ==3 )
	indent = "    ";

    code = el->elem.frt.info.fcode;

    if ( code/100 == 4 && code%10 == 0 ) 
	pgenType = "COLD_FRONT";
    else if ( code/100 == 4 && code%10 == 5 ) 
	pgenType = "COLD_FRONT_FORM";
    else if ( code/100 == 4 && code%10 == 8 ) 
	pgenType = "COLD_FRONT_DISS";
    else if ( code/100 == 2 && code%10 == 0 )
	pgenType = "WARM_FRONT";
    else if ( code/100 == 2 && code%10 == 5 )
	pgenType = "WARM_FRONT_FORM";
    else if ( code/100 == 2 && code%10 == 8 )
	pgenType = "WARM_FRONT_DISS";
    else if ( code/100 == 0 && code%10 == 0 )
	pgenType = "STATIONARY_FRONT";
    else if ( code/100 == 0 && code%10 == 5 )
	pgenType = "STATIONARY_FRONT_FORM";
    else if ( code/100 == 0 && code%10 == 8 )
	pgenType = "STATIONARY_FRONT_DISS";
    else if ( code/100 == 6 && code%10 == 0 )
	pgenType = "OCCLUDED_FRONT";
    else if ( code/100 == 6 && code%10 == 5 )
	pgenType = "OCCLUDED_FRONT_FORM";
    else if ( code/100 == 6 && code%10 == 8 )
	pgenType = "OCCLUDED_FRONT_DISS";
    else if ( code/100 == 7 && code%10 == 0 )
	pgenType = "DRY_LINE";
    else if ( code/100 == 8 && code%10 == 0 ) 
	pgenType = "TROF";
    else if ( code/100 == 8 && code%10 == 9 )
	pgenType = "TROPICAL_TROF";
    else if ( code/100 == 9 && code%10 == 0 )
	pgenType = "INSTABILITY";   
    else
	pgenType = "COLD_FRONT";
    
    
    sprintf( buffer, "%s          <Line pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" pgenType=\"%s\">\n", 
indent, category(el), (code%100)/10, ( ((float)el->elem.frt.info.fpipsz)/100),
el->hdr.smooth, el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true",
fill_pattern(el), pgenType );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s            %s\n", indent, getColorTag(el->hdr.maj_col));

    if (strncmp(pgenType, "STATIONARY", 10) ==0) {
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "%s            %s\n", indent, getColorTag(el->hdr.min_col));
    }

    npts = el->elem.frt.info.numpts;

    if  ( npts > 0 )  {
	if (el->elem.frt.info.fpipdr >=0) {
	    for ( ii = 0; ii < npts; ii++ )  {
	    	blen = strlen(buffer);
	    	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.frt.latlon[ii], checkLongitude(el->elem.frt.latlon[ii+npts]) );
	    }
	}
	else {
	    for ( ii = npts-1; ii >=0; ii-- )  {
	    	blen = strlen(buffer);
	    	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.frt.latlon[ii], checkLongitude(el->elem.frt.latlon[ii+npts]) );
	    }
	}
	
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, 0.0, 0.0 );
	printf("+ The front line has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s          </Line>\n", indent);

}

/*=====================================================================*/
void	lin2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
    int	ii, npts; 
    size_t	blen;
    char *pgenType;
    char *indent = "";

    /*
     *  LINE_ELM
     */
    if (el->hdr.grptyp == 98)
	indent = "    ";
    else if (el->hdr.grptyp ==1 || el->hdr.grptyp ==2 )
	indent = "    ";
    else if ((el->hdr.grptyp >=5 && el->hdr.grptyp <=9) || el->hdr.grptyp ==27) 
	indent = "      ";
    else if ((el->hdr.grptyp >=12 && el->hdr.grptyp <=16) || el->hdr.grptyp ==24) 
	indent = "      ";


    if ( (el->elem.lin.info.lintyp) %10 == 1 )
	pgenType = "LINE_SOLID";
    else if ( (el->elem.lin.info.lintyp) %10 == 0 )
	pgenType = "LINE_DASHED_2";
    else if ( (el->elem.lin.info.lintyp) %10 == 2 )
	pgenType = "LINE_DASHED_3";
    else if ( (el->elem.lin.info.lintyp) %10 == 3 )
	pgenType = "LINE_DASHED_4";
    else if ( (el->elem.lin.info.lintyp) %10 == 4 )
	pgenType = "LINE_DASHED_5";
    else if ( (el->elem.lin.info.lintyp) %10 == 5 )
	pgenType = "LINE_DASHED_6";
    else if ( (el->elem.lin.info.lintyp) %10 == 6 )
	pgenType = "LINE_DASHED_7";
    else if ( (el->elem.lin.info.lintyp) %10 == 7 )
	pgenType = "LINE_DASHED_8";
    else if ( (el->elem.lin.info.lintyp) %10 == 8 )
	pgenType = "LINE_DASHED_9";
    else if ( (el->elem.lin.info.lintyp) %10 == 9 )
	pgenType = "LINE_DASHED_10";
    else
	pgenType = "LINE_DASHED_2";


    sprintf( buffer, "%s          <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\">\n", 
    indent, pgenType, category(el), el->elem.lin.info.width, 1.0, el->hdr.smooth, 
    el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true", 
    fill_pattern(el), "false" 
    );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s            %s\n", indent, getColorTag(el->hdr.maj_col));
    
    npts = el->elem.lin.info.numpts;

    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.lin.latlon[ii], checkLongitude(el->elem.lin.latlon[ii+npts]));
	}
	
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, 0.0, 0.0 );
	printf("+ The line has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s          </Line>\n", indent);

}


/*=====================================================================*/

void	spl2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *pgenType;
char *indent = "";

    /*
     *  SPLN_ELM
     */
    if (el->hdr.grptyp == 98)
	indent = "    ";
    else if (el->hdr.grptyp ==1 || el->hdr.grptyp ==2 )
	if ( el->elem.spl.info.spltyp == 3 )
	    indent = "    ";
	else
	    indent = "        ";
    else if (el->hdr.grptyp >= 7) 
	indent = "      ";


    if ( el->elem.spl.info.spltyp == 4 )
	pgenType = "POINTED_ARROW";
    else if ( el->elem.spl.info.spltyp == 6 )
	pgenType = "FILLED_ARROW";
    else if ( el->elem.spl.info.spltyp == 20 )
	pgenType = "DASHED_ARROW";
    else if ( el->elem.spl.info.spltyp == 21 )
	pgenType = "DASHED_ARROW_FILLED";
    else if ( el->elem.spl.info.spltyp == 1 )
	pgenType = "BALL_CHAIN";
    else if ( el->elem.spl.info.spltyp == 2 )
	pgenType = "ZIGZAG";
    else if ( el->elem.spl.info.spltyp == 3 )
	pgenType = "SCALLOPED";
    else if ( el->elem.spl.info.spltyp == 5 )
	pgenType = "ANGLED_TICKS_ALT";
    else if ( el->elem.spl.info.spltyp == 9 )
	pgenType = "FILLED_CIRCLES";
    else if ( el->elem.spl.info.spltyp == 17 )
	pgenType = "LINE_WITH_CARETS";
    else if ( el->elem.spl.info.spltyp == 18 )
	pgenType = "LINE_CARET_LINE";
    else if ( el->elem.spl.info.spltyp == 19 )
	pgenType = "SINE_CURVE";
    else if ( el->elem.spl.info.spltyp == 7 )
	pgenType = "BOX_CIRCLE";
    else if ( el->elem.spl.info.spltyp == 13 )
	pgenType = "FILL_OPEN_BOX";
    else if ( el->elem.spl.info.spltyp == 12 )
	pgenType = "LINE_X_LINE";
    else if ( el->elem.spl.info.spltyp == 8 )
	pgenType = "LINE_XX_LINE";
    else if ( el->elem.spl.info.spltyp == 14 )
	pgenType = "FILL_CIRCLE_X";
    else if ( el->elem.spl.info.spltyp == 15 )
	pgenType = "BOX_X";
    else if ( el->elem.spl.info.spltyp == 16 )
	pgenType = "LINE_CIRCLE_ARROW";
    else if ( el->elem.spl.info.spltyp == 23 )
	pgenType = "DOUBLE_LINE";
    else if ( el->elem.spl.info.spltyp == 26 )
	pgenType = "ZZZ_LINE";
    else if ( el->elem.spl.info.spltyp == 11 )
	pgenType = "TICK_MARKS";
    else if ( el->elem.spl.info.spltyp == 22 )
	pgenType = "STREAM_LINE";
    else if ( el->elem.spl.info.spltyp == 10 )
	pgenType = "LINE_FILLED_CIRCLE_ARROW";
    else if ( el->elem.spl.info.spltyp == 24 )
	pgenType = "KINK_LINE_1";
    else if ( el->elem.spl.info.spltyp == 25 )
	pgenType = "KINK_LINE_2";
    else
	pgenType = "POINTED_ARROW";

    if ( el->elem.spl.info.spltyp == 24 || el->elem.spl.info.spltyp == 25 ) {
        
	float kinkPos = el->elem.spl.info.splstr / 100.0;
	char* arrowhead = "OPEN";
	if ( el->elem.spl.info.spltyp == 25 ) arrowhead = "FILLED";
        sprintf( buffer, "%s          <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%4.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\" kinkPosition=\"%4.2f\" arrowHeadType=\"%s\">\n", 
            indent, pgenType, category(el), el->elem.spl.info.splwid, el->elem.spl.info.splsiz,
            el->hdr.smooth, el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true",
            fill_pattern(el), "false", kinkPos, arrowhead );
	
    }
    else {
        sprintf( buffer, "%s          <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%4.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\">\n", 
            indent, pgenType, category(el), el->elem.spl.info.splwid, el->elem.spl.info.splsiz,
            el->hdr.smooth, el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true",
            fill_pattern(el), "false" );
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s            %s\n", indent, getColorTag(el->hdr.maj_col));
    
    npts = el->elem.spl.info.numpts;

    if  ( npts > 0 )  {
	if (el->elem.spl.info.spldir >=0) {
	    for ( ii = 0; ii < npts; ii++ )  {
	    	blen = strlen(buffer);
	    	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.spl.latlon[ii], checkLongitude(el->elem.spl.latlon[ii+npts]) );
	    }
	}
	else {
	    for ( ii = npts-1; ii >=0; ii-- )  {
	    	blen = strlen(buffer);
	    	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.spl.latlon[ii], checkLongitude(el->elem.spl.latlon[ii+npts]) );
	    }
	}
    }

    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, 0.0, 0.0 );
	printf("+ The line has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s          </Line>\n", indent);

}


/*=====================================================================*/

void	wbx2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *mktype, *wtype, *shape, *severity, *stat;
char *wsmExpT, *wsmIssT, *wExpT, *wIssT;
char pointVor[30], pointAnc[30];
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  WBOX_ELM
     */

    if (el->elem.wbx.info.w_shape ==1)
	shape = "NS";
    else if (el->elem.wbx.info.w_shape ==2)
	shape = "EW";
    else if (el->elem.wbx.info.w_shape ==3)
	shape = "ESOL";
    else
	shape = "";

    if (el->elem.wbx.info.w_mrktyp ==1)
	mktype = "PLUS_SIGN";
    else if (el->elem.wbx.info.w_mrktyp ==2)
	mktype = "OCTAGON";
    else if (el->elem.wbx.info.w_mrktyp ==3)
	mktype = "TRIANGLE";
    else if (el->elem.wbx.info.w_mrktyp ==4)
	mktype = "BOX";
    else if (el->elem.wbx.info.w_mrktyp ==5)
	mktype = "SMALL_X";
    else if (el->elem.wbx.info.w_mrktyp ==6)
	mktype = "";
    else if (el->elem.wbx.info.w_mrktyp ==7)
	mktype = "UP_ARROW";
    
 /*   else if (el->elem.wbx.info.w_mrktyp ==8)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==9)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==10)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==11)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==12)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==13)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==14)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==15)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==16)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==17)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==18)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==19)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==20)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==21)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==22)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==23)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==24)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==25)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==26)
	type = "";
    else if (el->elem.wbx.info.w_mrktyp ==27)
	type = "";*/
    else
	mktype = " ";


    if (el->elem.wbx.info.w_severity ==1)
	severity = "PDS";
    else if (el->elem.wbx.info.w_severity ==0)
	severity = "NORMAL";
    else
	severity = "NORMAL";

    if (el->elem.wbx.info.w_istat ==1)
	stat = "Active";
    else if (el->elem.wbx.info.w_istat ==0)
	stat = "Test";
    else 
	stat = "Test";

    if (el->elem.wbx.info.w_type ==6)
	wtype = "SEVERE THUNDERSTORM";
    else if (el->elem.wbx.info.w_type ==2 )
	wtype = "TORNADO";
    else if (el->elem.wbx.info.w_type ==7)
	wtype = "";

    sprintf(pointAnc, "%s %d %s %s %s %d %s %s", " ", el->elem.wbx.info.w_a0dis, el->elem.wbx.info.w_a0dir, el->elem.wbx.info.w_a0id, "-", el->elem.wbx.info.w_a1dis, el->elem.wbx.info.w_a1dir, el->elem.wbx.info.w_a1id);
    sprintf(pointVor, "%s %d %s %s %s %d %s %s", " ", el->elem.wbx.info.w_a0dis, el->elem.wbx.info.w_a0dir, el->elem.wbx.info.w_a0id, "-", el->elem.wbx.info.w_a1dis, el->elem.wbx.info.w_a1dir, el->elem.wbx.info.w_a1id);


/* change time format to 2010-03-30T15:15:00.000Z*/
    wExpT = (char*)malloc(32);		// was 04/13/2010/0300/
    wIssT = (char*)malloc(32);
    wsmExpT = (char*)malloc(32);	// was 130300
    wsmIssT = (char*)malloc(32);
    char* timeExp = (char*)malloc(16);   
    char* timeIss = (char*)malloc(16);
    char* yearMonExp = (char*)malloc(16);
    char* yearMonIss = (char*)malloc(16);

    time_watch ( el->elem.wbx.info.w_exp_t, wExpT ); 
    time_watch ( el->elem.wbx.info.w_iss_t, wIssT );
     
    if (strcmp(wExpT, "") ==0)
    	strcpy(wsmExpT, "");
    else {
	memcpy( yearMonExp, wExpT, 8 );
	//strncpy(yearMonExp, wExpT, 8);	
	*(yearMonExp+8) = '\0';
    }
    if (strcmp(wIssT, "") ==0)
    	strcpy(wsmIssT, "");
    else {
    	memcpy(yearMonIss, wIssT, 8);
	//strncpy(yearMonIss, wIssT, 8);
	*(yearMonIss+8) = '\0';
    }

    time_watch_stat ( el->elem.wbx.info.wsm_exp_t, timeExp ); // 121955
    time_watch_stat ( el->elem.wbx.info.wsm_iss_t, timeIss );

    if (strcmp(timeExp, "") !=0)
	sprintf(wsmExpT, "%s%s", yearMonExp, timeExp);
    else 
	strcpy(wsmExpT, "");
	

    if (strcmp(timeIss, "") !=0)
	sprintf(wsmIssT, "%s%s", yearMonIss, timeIss);
    else
	strcpy(wsmIssT, "");
	    
    free(timeExp);
    free(timeIss);
    free(yearMonExp);
    free(yearMonIss);

    char *hail;
    if (el->elem.wbx.info.w_issued ==0) 
	hail = "0.0"; 
    else 
	hail = el->elem.wbx.info.w_hailsz;


	sprintf( buffer, "              <WatchBox pgenCategory=\"%s\" pgenType=\"%s\" wfos=\"%s\" watchAreaNm=\"%s\" halfWidthSm=\"%s\" halfWidthNm=\"%s\" endPointVor=\"%s\" endPointAnc=\"%s\" forecaster=\"%s\" contWatch=\"%d\" watchNumber=\"%d\" watchType=\"%s\" issueFlag=\"%d\" replWatch=\"%s\" adjAreas=\"%s\" moveSpeed=\"%s\" moveDir=\"%s\" top=\"%s\" gust=\"%s\" hailSize=\"%s\" timeZone=\"%s\" severity=\"%s\"  expTime=\"%s\" issueTime=\"%s\" issueStatus=\"%s\" symbolSize=\"%f\" symbolWidth=\"%d\" symbolType=\"%s\" fillFlag=\"%s\" boxShape=\"%s\">\n",

category(el), "watchbox", "", "", "", "", "", pointAnc,
el->elem.wbx.info.w_fcstr, 0, el->elem.wbx.info.w_number, wtype, 
el->elem.wbx.info.w_issued==2 ?1 :el->elem.wbx.info.w_issued, el->elem.wbx.info.w_replw, 
el->elem.wbx.info.w_adjarea, el->elem.wbx.info.w_msmv_s, el->elem.wbx.info.w_msmv_d, 
el->elem.wbx.info.w_tops, el->elem.wbx.info.w_windg, hail, 
el->elem.wbx.info.w_timezone, severity, wExpT, wIssT, stat, el->elem.wbx.info.w_mrksiz,    
el->elem.wbx.info.w_mrkwid, mktype, el->hdr.filled==0 ?"false":"true", shape );

    free(wExpT);
    free(wIssT);
    wExpT = NULL;
    wIssT = NULL;

	
    npts = el->elem.wbx.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->hdr.maj_col));
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->hdr.min_col));
    
    if (npts > 0) {
    	for ( ii = 0; ii < npts; ii++ )  {
            blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", el->elem.wbx.latlon[ii], checkLongitude(el->elem.wbx.latlon[ii+npts]));
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	printf("+ The watch box line has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "                <AnchorPoints>%s</AnchorPoints>\n", strcmp(el->elem.wbx.info.w_a0id, "---")==0 ?"" :el->elem.wbx.info.w_a0id);
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "                <AnchorPoints>%s</AnchorPoints>\n", strcmp(el->elem.wbx.info.w_a1id, "---")==0 ?"" :el->elem.wbx.info.w_a1id);

    
    npts = el->elem.wbx.info.numcnty;
    for ( ii = 0; ii < npts; ii++ )  {
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                <Counties>%d</Counties>\n", el->elem.wbx.info.cn_fips[ii]);
    }

    char* result;
    char *temp = (char*)(el->elem.wbx.info.w_states);
    const char delimiter[] = " ";
    while ((result = strsep(&temp, delimiter)) != NULL) {
    	
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                <States>%s</States>\n", result);
    }

    if (el->elem.wbx.info.wsm_fcstr != NULL && strcmp(el->elem.wbx.info.wsm_fcstr, "") !=0 ) {
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                <Status statusForecaster=\"%s\" statusExpTime=\"%s\" statusValidTime=\"%s\"  mesoDiscussionNumber=\"%s\" fromLine=\"%s\"/>\n", el->elem.wbx.info.wsm_fcstr, wsmExpT, wsmIssT, el->elem.wbx.info.wsm_meso, el->elem.wbx.info.wsm_from);
    }
 
    free(wsmExpT);
    free(wsmIssT);
    wsmExpT = NULL;
    wsmIssT = NULL;


    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "              </WatchBox>\n");


}

/*=====================================================================*/

void	wsm2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  WSM_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]),

	"<numpts>%d"
	"<spltyp>%d"
	"<splstr>%d"
	"<spldir>%d"
	"<splsiz>%f"
	"<splwid>%d",

	el->elem.wsm.info.numpts,
	el->elem.wsm.info.spltyp,
	el->elem.wsm.info.splstr,
	el->elem.wsm.info.spldir,
	el->elem.wsm.info.splsiz,
	el->elem.wsm.info.splwid

	);

    npts = el->elem.wsm.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wsm.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.wsm.latlon[ii+npts] );
    }

    /*  remove final comma before ending	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	sym2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *pgenType;
char *indent = "";
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  WXSYM_ELM, CTSYM_ELM, ICSYM_ELM, PTSYM_ELM, PWSYM_ELM,
     *  SKSYM_ELM, SPSYM_ELM, CMBSY_ELM, TBSYM_ELM, MARK_ELM
     */

    if (el->hdr.grptyp ==5 ||el->hdr.grptyp ==6 ||el->hdr.grptyp ==8 ||el->hdr.grptyp ==9) 
	indent = "      ";	/* contour */
    else if (el->hdr.grptyp >=7) /* outlook */
	indent = "      ";
    else if (el->hdr.grptyp >0) /* symLabel */
	indent = "    ";

    if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 0.0 )
	pgenType = "PRESENT_WX_000";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "PRESENT_WX_001";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "PRESENT_WX_002";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "PRESENT_WX_003";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "PRESENT_WX_004";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "PRESENT_WX_005";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "PRESENT_WX_006";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "PRESENT_WX_007";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "PRESENT_WX_008";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "PRESENT_WX_009";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "PRESENT_WX_010";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 11.0 )
	pgenType = "PRESENT_WX_011";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "PRESENT_WX_012";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "PRESENT_WX_013";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 14.0 )
	pgenType = "PRESENT_WX_014";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 15.0 )
	pgenType = "PRESENT_WX_015";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 16.0 )
	pgenType = "PRESENT_WX_016";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 17.0 )
	pgenType = "PRESENT_WX_017";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 18.0 )
	pgenType = "PRESENT_WX_018";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 19.0 )
	pgenType = "PRESENT_WX_019";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 20.0 )
	pgenType = "PRESENT_WX_020";
    
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 21.0 )
	pgenType = "PRESENT_WX_021";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 22.0 )
	pgenType = "PRESENT_WX_022";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 23.0 )
	pgenType = "PRESENT_WX_023";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 24.0 )
	pgenType = "PRESENT_WX_024";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 25.0 )
	pgenType = "PRESENT_WX_025";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 26.0 )
	pgenType = "PRESENT_WX_026";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 27.0 )
	pgenType = "PRESENT_WX_027";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 28.0 )
	pgenType = "PRESENT_WX_028";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 29.0 )
	pgenType = "PRESENT_WX_029";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 30.0 )
	pgenType = "PRESENT_WX_030";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 31.0 )
	pgenType = "PRESENT_WX_031";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 32.0 )
	pgenType = "PRESENT_WX_032";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 33.0 )
	pgenType = "PRESENT_WX_033";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 34.0 )
	pgenType = "PRESENT_WX_034";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 35.0 )
	pgenType = "PRESENT_WX_035";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 36.0 )
	pgenType = "PRESENT_WX_036";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 37.0 )
	pgenType = "PRESENT_WX_037";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 38.0 )
	pgenType = "PRESENT_WX_038";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 39.0 )
	pgenType = "PRESENT_WX_039";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 40.0 )
	pgenType = "PRESENT_WX_040";

    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 41.0 )
	pgenType = "PRESENT_WX_041";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 42.0 )
	pgenType = "PRESENT_WX_042";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 43.0 )
	pgenType = "PRESENT_WX_043";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 44.0 )
	pgenType = "PRESENT_WX_044";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 45.0 )
	pgenType = "PRESENT_WX_045";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 46.0 )
	pgenType = "PRESENT_WX_046";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 47.0 )
	pgenType = "PRESENT_WX_047";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 48.0 )
	pgenType = "PRESENT_WX_048";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 49.0 )
	pgenType = "PRESENT_WX_049";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 50.0 )
	pgenType = "PRESENT_WX_050";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 51.0 )
	pgenType = "PRESENT_WX_051";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 52.0 )
	pgenType = "PRESENT_WX_052";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 53.0 )
	pgenType = "PRESENT_WX_053";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 54.0 )
	pgenType = "PRESENT_WX_054";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 55.0 )
	pgenType = "PRESENT_WX_055";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 56.0 )
	pgenType = "PRESENT_WX_056";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 57.0 )
	pgenType = "PRESENT_WX_057";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 58.0 )
	pgenType = "PRESENT_WX_058";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 59.0 )
	pgenType = "PRESENT_WX_059";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 60.0 )
	pgenType = "PRESENT_WX_060"; 
   
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 61.0 )
	pgenType = "PRESENT_WX_061";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 62.0 )
	pgenType = "PRESENT_WX_062";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 63.0 )
	pgenType = "PRESENT_WX_063";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 64.0 )
	pgenType = "PRESENT_WX_064";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 65.0 )
	pgenType = "PRESENT_WX_065";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 66.0 )
	pgenType = "PRESENT_WX_066";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 67.0 )
	pgenType = "PRESENT_WX_067";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 68.0 )
	pgenType = "PRESENT_WX_068";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 69.0 )
	pgenType = "PRESENT_WX_069";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 70.0 )
	pgenType = "PRESENT_WX_070";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 71.0 )
	pgenType = "PRESENT_WX_071";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 72.0 )
	pgenType = "PRESENT_WX_072";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 73.0 )
	pgenType = "PRESENT_WX_073";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 74.0 )
	pgenType = "PRESENT_WX_074";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 75.0 )
	pgenType = "PRESENT_WX_075";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 76.0 )
	pgenType = "PRESENT_WX_076";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 77.0 )
	pgenType = "PRESENT_WX_077";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 78.0 )
	pgenType = "PRESENT_WX_078";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 79.0 )
	pgenType = "PRESENT_WX_079";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 80.0 )
	pgenType = "PRESENT_WX_080";

    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 81.0 )
	pgenType = "PRESENT_WX_081";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 82.0 )
	pgenType = "PRESENT_WX_082";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 83.0 )
	pgenType = "PRESENT_WX_083";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 84.0 )
	pgenType = "PRESENT_WX_084";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 85.0 )
	pgenType = "PRESENT_WX_085";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 86.0 )
	pgenType = "PRESENT_WX_086";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 87.0 )
	pgenType = "PRESENT_WX_087";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 88.0 )
	pgenType = "PRESENT_WX_088";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 89.0 )
	pgenType = "PRESENT_WX_089";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 90.0 )
	pgenType = "PRESENT_WX_080";    
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 91.0 )
	pgenType = "PRESENT_WX_091";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 92.0 )
	pgenType = "PRESENT_WX_092";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 93.0 )
	pgenType = "PRESENT_WX_093";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 94.0 )
	pgenType = "PRESENT_WX_094";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 95.0 )
	pgenType = "PRESENT_WX_095";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 96.0 )
	pgenType = "PRESENT_WX_096";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 97.0 )
	pgenType = "PRESENT_WX_097";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 98.0 )
	pgenType = "PRESENT_WX_098";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 99.0 )
	pgenType = "PRESENT_WX_099";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 103.0 )
	pgenType = "PRESENT_WX_103";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 104.0 )
	pgenType = "PRESENT_WX_104";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 105.0 )
	pgenType = "PRESENT_WX_105";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 107.0 )
	pgenType = "PRESENT_WX_107";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 201.0 )
	pgenType = "PRESENT_WX_201";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 202.0 )
	pgenType = "PRESENT_WX_202";
    else if ( el->hdr.vg_type == 5 && el->elem.sym.data.code[0] == 203.0 )
	pgenType = "PRESENT_WX_203";

    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 0.0 )
	pgenType = "SQUARE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "FILLED_SQUARE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "CIRCLE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "FILLED_CIRCLE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "TRIANGLE_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "FILLED_TRIANGLE_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "DIAMOND_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "FILLED_DIAMOND_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "STAR_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "FILLED_STAR_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "HIGH_PRESSURE_H";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 11.0 )
	pgenType = "LOW_PRESSURE_L";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "FILLED_HIGH_PRESSURE_H";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "FILLED_LOW_PRESSURE_L";
    
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 14.0 )
	pgenType = "SINGLE_BRACKET";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 15.0 )
	pgenType = "BOTTOM_HALF_OF_BRACKET";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 16.0 )
	pgenType = "TOP_HALF_OF_BRACKET";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 17.0 )
	pgenType = "LEFT_ADJUSTED_VERTICAL_BAR";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 18.0 )
	pgenType = "RIGHT_ADJUSTED_VERTICAL_BAR";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 19.0 )
	pgenType = "BRACKET_WITH_ONE_CIRCLE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 20.0 )
	pgenType = "BRACKET_WITH_TWO_CIRCLES";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 21.0 )
	pgenType = "BRACKET_WITH_ONE_ASTERISK";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 22.0 )
	pgenType = "BRACKET_WITH_TWO_ASTERISKS";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 23.0 )
	pgenType = "BRACKET_WITH_ONE_TRIANGLE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 24.0 )
	pgenType = "BRACKET_WITH_TWO_TRIANGLES";    
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 25.0 )
	pgenType = "TROPICAL_STORM_NH";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 26.0 )
	pgenType = "HURRICANE_NH";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 27.0 )
	pgenType = "TROPICAL_STORM_SH";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 28.0 )
	pgenType = "HURRICANE_SH";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 29.0 )
	pgenType = "TRIANGLE_WITH_ANTENNA";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 30.0 )
	pgenType = "SIDEWAYS_S";

    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 31.0 )
	pgenType = "SLASH";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 32.0 )
	pgenType = "STORM_CENTER";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 33.0 )
	pgenType = "TROPICAL_DEPRESSION";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 34.0 )
	pgenType = "TROPICAL_CYCLONE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 35.0 )
	pgenType = "FLAME";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 36.0 )
	pgenType = "X_CROSS";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 37.0 )
	pgenType = "LOW_X_OUTLINE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 38.0 )
	pgenType = "LOW_X_FILLED";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 39.0 )
	pgenType = "TROPICAL_STORM_NH_WPAC";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 40.0 )
	pgenType = "TROPICAL_STORM_SH_WPAC";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 41.0 )
	pgenType = "NUCLEAR_FALLOUT";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 42.0 )
	pgenType = "LETTER_A";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 43.0 )
	pgenType = "LETTER_A_FILLED";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 44.0 )
	pgenType = "LETTER_C";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 45.0 )
	pgenType = "LETTER_C_FILLED";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 46.0 )
	pgenType = "LETTER_X";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 47.0 )
	pgenType = "LETTER_X_FILLED";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 48.0 )
	pgenType = "LETTER_N";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 49.0 )
	pgenType = "LETTER_N_FILLED";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 50.0 )
	pgenType = "30_KT_BARB";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 51.0 )
	pgenType = "MT_WAVE";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 52.0 )
	pgenType = "MT_OBSC";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 53.0 )
	pgenType = "SFC_WND_20K";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 54.0 )
	pgenType = "SFC_WND_30K";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 55.0 )
	pgenType = "LETTER_B";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 56.0 )
	pgenType = "LETTER_B_FILLED";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 57.0 )
	pgenType = "UP_ARROW_SPCL";
    else if ( el->hdr.vg_type == 15 && el->elem.sym.data.code[0] == 58.0 )
	pgenType = "DOWN_ARROW_SPCL";

    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 0.0 )
	pgenType = "ICING_00";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "ICING_01";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "ICING_02";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "ICING_03";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "ICING_04";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "ICING_05";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "ICING_06";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "ICING_07";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "ICING_08";    
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "ICING_09";
    else if ( el->hdr.vg_type == 11 && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "ICING_10";

    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 0.0 )
	pgenType = "PRESSURE_TENDENCY_00";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "PRESSURE_TENDENCY_01";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "PRESSURE_TENDENCY_02";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "PRESSURE_TENDENCY_03";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "PRESSURE_TENDENCY_04";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "PRESSURE_TENDENCY_05";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "PRESSURE_TENDENCY_06";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "PRESSURE_TENDENCY_07";
    else if ( el->hdr.vg_type == 12 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "PRESSURE_TENDENCY_08";    
    
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 0.0 )
	pgenType = "SKY_COVER_00";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "SKY_COVER_01";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "SKY_COVER_02";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "SKY_COVER_03";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "SKY_COVER_04";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "SKY_COVER_05";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "SKY_COVER_06";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "SKY_COVER_07";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "SKY_COVER_08";    
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "SKY_COVER_09";
    else if ( el->hdr.vg_type == 14 && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "SKY_COVER_10";

    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "PAST_WX_03";
    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "PAST_WX_04";
    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "PAST_WX_05";
    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "PAST_WX_06";
    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "PAST_WX_07";
    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "PAST_WX_08";
    else if ( el->hdr.vg_type == 13 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "PAST_WX_09";

    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "CLOUD_TYPE_01";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "CLOUD_TYPE_02";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "CLOUD_TYPE_03";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "CLOUD_TYPE_04";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "CLOUD_TYPE_05";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "CLOUD_TYPE_06";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "CLOUD_TYPE_07";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "CLOUD_TYPE_08";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "CLOUD_TYPE_09";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 11.0 )
	pgenType = "CLOUD_TYPE_11";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "CLOUD_TYPE_12";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "CLOUD_TYPE_13";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 14.0 )
	pgenType = "CLOUD_TYPE_14";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 15.0 )
	pgenType = "CLOUD_TYPE_15";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 16.0 )
	pgenType = "CLOUD_TYPE_16";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 17.0 )
	pgenType = "CLOUD_TYPE_17";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 18.0 )
	pgenType = "CLOUD_TYPE_18";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 19.0 )
	pgenType = "CLOUD_TYPE_19";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 21.0 )
	pgenType = "CLOUD_TYPE_21";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 22.0 )
	pgenType = "CLOUD_TYPE_22";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 23.0 )
	pgenType = "CLOUD_TYPE_23";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 24.0 )
	pgenType = "CLOUD_TYPE_24";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 25.0 )
	pgenType = "CLOUD_TYPE_25";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 26.0 )
	pgenType = "CLOUD_TYPE_26";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 27.0 )
	pgenType = "CLOUD_TYPE_27";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 28.0 )
	pgenType = "CLOUD_TYPE_28";
    else if ( el->hdr.vg_type == 10 && el->elem.sym.data.code[0] == 29.0 )
	pgenType = "CLOUD_TYPE_29";


    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 0.0 )
	pgenType = "TURBULENCE_0";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "TURBULENCE_1";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "TURBULENCE_2";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "TURBULENCE_3";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "TURBULENCE_4";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "TURBULENCE_5";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "TURBULENCE_6";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "TURBULENCE_7";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "TURBULENCE_8";    
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 46.0 )
	pgenType = "TURBULENCE_46";
    else if ( el->hdr.vg_type == 16 && el->elem.sym.data.code[0] == 67.0 )
	pgenType = "TURBULENCE_67";

    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_051";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_063";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_080";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 21.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_095";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "PRESENT_WX_063|PRESENT_WX_065";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 11.0 )
	pgenType = "PRESENT_WX_063|PRESENT_WX_080";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 15.0 )
	pgenType = "PRESENT_WX_063|PRESENT_WX_095";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 14.0 )
	pgenType = "PRESENT_WX_065|PRESENT_WX_080";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 16.0 )
	pgenType = "PRESENT_WX_065|PRESENT_WX_095";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "PRESENT_WX_051|PRESENT_WX_056";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_056";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 26.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_066";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_071";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 18.0 )
	pgenType = "PRESENT_WX_071|PRESENT_WX_073";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 19.0 )
	pgenType = "PRESENT_WX_073|PRESENT_WX_075";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "PRESENT_WX_071|PRESENT_WX_085";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 17.0 )
	pgenType = "PRESENT_WX_073|PRESENT_WX_085";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 25.0 )
	pgenType = "PRESENT_WX_075|PRESENT_WX_085";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 23.0 )
	pgenType = "PRESENT_WX_071|PRESENT_WX_105";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 20.0 )
	pgenType = "PRESENT_WX_073|PRESENT_WX_105";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 24.0 )
	pgenType = "PRESENT_WX_075|PRESENT_WX_105";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 22.0 )
	pgenType = "PRESENT_WX_080|PRESENT_WX_095";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "PRESENT_WX_080|PRESENT_WX_085";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 27.0 )
	pgenType = "PRESENT_WX_061|PRESENT_WX_079";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "PRESENT_WX_079|PRESENT_WX_066";
    else if ( el->hdr.vg_type == 25 && el->elem.sym.data.code[0] == 28.0 )
	pgenType = "PRESENT_WX_071|PRESENT_WX_079";

    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 1.0 )
	pgenType = "PLUS_SIGN";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 2.0 )
	pgenType = "OCTAGON";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 3.0 )
	pgenType = "TRIANGLE";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 4.0 )
	pgenType = "BOX";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 5.0 )
	pgenType = "SMALL_X";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 9.0 )
	pgenType = "Z_WITH_BAR";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 8.0 )
	pgenType = "X_WITH_TOP_BAR";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 6.0 )
	pgenType = "DIAMOND";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 7.0 )
	pgenType = "UP_ARROW";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 10.0 )
	pgenType = "Y";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 11.0 )
	pgenType = "BOX_WITH_DIAGONALS";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 12.0 )
	pgenType = "ASTERISK";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 13.0 )
	pgenType = "HOURGLASS_X";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 14.0 )
	pgenType = "STAR";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 15.0 )
	pgenType = "DOT";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 16.0 )
	pgenType = "LARGE_X";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 17.0 )
	pgenType = "FILLED_OCTAGON";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 18.0 )
	pgenType = "FILLED_TRIANGLE";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 19.0 )
	pgenType = "FILLED_BOX";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 20.0 )
	pgenType = "FILLED_DIAMOND";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 21.0 )
	pgenType = "FILLED_STAR";
    else if ( el->hdr.vg_type == 19 && el->elem.sym.data.code[0] == 22.0 )
	pgenType = "MINUS_SIGN";

    else if ( el->hdr.vg_type == 19) /* default */
	pgenType = "MINUS_SIGN";
    else if ( el->hdr.vg_type == 25)
	pgenType = "PRESENT_WX_061|PRESENT_WX_051";
    else
	pgenType = "STORM_CENTER";


    sprintf( buffer, "%s          <Symbol pgenCategory=\"%s\"" " pgenType=\"%s\"" " lineWidth=\"%d\"" " sizeScale=\"%5.1f\"" " clear=\"%s\">\n",
indent, category(el), pgenType, 
el->elem.sym.info.width>=100 ?el->elem.sym.info.width-800 :el->elem.sym.info.width, 
el->elem.sym.info.size, (el->elem.sym.info.width)>=100 ?"true":"false");


    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s            %s\n", indent, getColorTag(el->hdr.maj_col));
    
    npts = el->elem.sym.info.numsym;
    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.sym.data.latlon[ii], checkLongitude(el->elem.sym.data.latlon[ii+npts]) );
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, 0.0, 0.0 );
	printf("+ The symbol has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s          </Symbol>\n", indent);

}

/*=====================================================================*/

void	wnd2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *pgenType;
char *Directional;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  BARB_ELM, ARROW_ELM, DARR_ELM, HASH_ELM
     */
    if ( el->hdr.vg_type == 9 ) {
	pgenType = "Arrow";
	Directional = "false";
    } else if ( el->hdr.vg_type == 8 ) {
	pgenType = "Barb";
	Directional = "false";
    } else if ( el->hdr.vg_type == 23 ) {
	pgenType = "Directional";
	Directional = "true";
    } else if ( el->hdr.vg_type == 24 ) {
	pgenType = "Hash";
	Directional = "false";
    }

    int width = el->elem.wnd.info.width; //801
   
    sprintf( buffer, "          <Vector pgenCategory=\"%s\"" " pgenType=\"%s\"" " lineWidth=\"%d\" " "sizeScale=\"%5.1f\"" " direction=\"%10.6f\"" " speed=\"%10.6f\"" " arrowHeadSize=\"%5.2f\"" " directionOnly=\"%s\"" " clear=\"%s\">\n",
category(el), pgenType, width>=100 ?(width%100) :width, el->elem.wnd.info.size, (el->elem.wnd.data.spddir[1]), (el->elem.wnd.data.spddir[0]), el->elem.wnd.info.hdsiz, Directional, el->elem.wnd.info.wndtyp==114 ?"true":"false" ); //112, 1: false 

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));
  
    npts = el->elem.wnd.info.numwnd;
    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", el->elem.wnd.data.latlon[ii], checkLongitude(el->elem.wnd.data.latlon[ii+npts]) );
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	printf("+ The vector has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Vector>\n");

}

/*=====================================================================*/

void	txt2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ier;
char	*tstr, sstr[2];
size_t	blen;
char *pgenType;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  TEXT_ELM, TEXTC_ELM
     */
    if ( el->hdr.vg_type == 21 )
	pgenType = "Arrow";
    else if ( el->hdr.vg_type == 8 )
	pgenType = "Barb";

    hdr2tag ( el, buffer, &ier );

    tstr = (char *)malloc( strlen(el->elem.txt.text)*2 * sizeof(char) );
    strcpy ( tstr, el->elem.txt.text );
    while ( strstr(tstr,"\n") != (char *)NULL )
        cst_rpst ( tstr, "\n", "$$", tstr, &ier );
    sstr[0] = CHLF;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );
    sstr[0] = CHCR;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]),

	"<rotn>%f"
	"<sztext>%f"
	"<itxfn>%d"
	"<ithw>%d"
	"<iwidth>%d"
	"<ialign>%d"
	"<lat>%f"
	"<lon>%f"
	"<offset_x>%d"
	"<offset_y>%d"
	"<text>%s",

	el->elem.txt.info.rotn,
	el->elem.txt.info.sztext,
	el->elem.txt.info.itxfn,
	el->elem.txt.info.ithw,
	el->elem.txt.info.iwidth,
	el->elem.txt.info.ialign,
	el->elem.txt.info.lat,
	el->elem.txt.info.lon,
	el->elem.txt.info.offset_x,
	el->elem.txt.info.offset_y,
	tstr

	);

    free ( tstr );

}

/*=====================================================================*/

void	spt2tag ( VG_DBStruct *el, char *buffer, int *iret )
{

size_t	blen;
char * pgenType = "Not Applicable";
char * symbolPatternName = "TURBULENCE_0";
char * avnTextType = "LOW_LEVEL_TURBULENCE";
char * mask = "false";
char * displayType = "NORMAL";
char * font;
char * style;
char * indent = "";

char *result = NULL, *result1, *result2, *result3, *result4, *result5, *result6, *result7;  //text token
char *r1="", *r2="", *r7="", *t;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  SPTX_ELM
     */
    if (el->hdr.grptyp == 98)
	indent = "    ";
    else if (el->hdr.grptyp ==1 || el->hdr.grptyp ==2 )
	indent = "        ";
    else if (el->hdr.grptyp == 3 || el->hdr.grptyp ==4) 
	indent = "    ";
    else if ((el->hdr.grptyp >=5 && el->hdr.grptyp <=9) || el->hdr.grptyp ==27) 
	indent = "      ";
    else if ((el->hdr.grptyp >=12 && el->hdr.grptyp <=16) || el->hdr.grptyp ==24) 
	indent = "      ";

    

    if ( el->elem.spt.info.sptxtyp == 0) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 1) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "LOW_PRESSURE_BOX";
    } else if ( el->elem.spt.info.sptxtyp == 2) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "HIGH_PRESSURE_BOX";
    } else if ( el->elem.spt.info.sptxtyp == 4) {
	pgenType = "General Text";
	
    } else if ( el->elem.spt.info.sptxtyp == 3) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 5) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 6) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "FREEZING_LEVEL";
    } else if ( el->elem.spt.info.sptxtyp == 7) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "LOW_LEVEL_TURBULENCE";
    } else if ( el->elem.spt.info.sptxtyp == 8) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "CLOUD_LEVEL";
    } else if ( el->elem.spt.info.sptxtyp == 9) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "HIGH_LEVEL_TURBULENCE";
    } else if ( el->elem.spt.info.sptxtyp == 10) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 11) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 12) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "MID_LEVEL_ICING";
    } else if ( el->elem.spt.info.sptxtyp == 13) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 14) {
	pgenType = "General Text";

    } else if ( el->elem.spt.info.sptxtyp == 15) {
	pgenType = "MID_LEVEL_CLOUD";
	avnTextType = "Middle Level Cloud Text";
    } else if ( el->elem.spt.info.sptxtyp == 16) {
	pgenType = "AVIATION_TEXT";
	avnTextType = "FLIGHT_LEVEL";
    }
    else {
	pgenType = "General Text";
    }

    if ( (el->elem.spt.info.sptxtyp == 9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym == 0)
	symbolPatternName = "TURBULENCE_0";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==1)
	symbolPatternName = "TURBULENCE_1";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==2)
	symbolPatternName = "TURBULENCE_2";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==3)
	symbolPatternName = "TURBULENCE_3";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==4)
	symbolPatternName = "TURBULENCE_4";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==56)
	symbolPatternName = "TURBULENCE_4|TURBULENCE_6" ;
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==5)
	symbolPatternName = "TURBULENCE_5";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==6)
	symbolPatternName = "TURBULENCE_6";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==77)
	symbolPatternName = "TURBULENCE_6|TURBULENCE_7" ;
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==7)
	symbolPatternName = "TURBULENCE_7";
    else if ( (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7) && el->elem.spt.info.turbsym ==8)
	symbolPatternName = "TURBULENCE_8";

    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 0)
	symbolPatternName = "ICING_00";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 1)
	symbolPatternName = "ICING_01";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 2)
	symbolPatternName = "ICING_02";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 3)
	symbolPatternName = "ICING_03";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 4)
	symbolPatternName = "ICING_04";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 5)
	symbolPatternName = "ICING_05";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 6)
	symbolPatternName = "ICING_06";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 7)
	symbolPatternName = "ICING_07";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 8)
	symbolPatternName = "ICING_08";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 9) 
	symbolPatternName = "ICING_09";
    else if ( el->elem.spt.info.sptxtyp == 12  && el->elem.spt.info.turbsym == 10)
	symbolPatternName = "ICING_10";
    else if (el->elem.spt.info.sptxtyp ==9 ||el->elem.spt.info.sptxtyp ==7)
	symbolPatternName = "TURBULENCE_0";
    else if ( el->elem.spt.info.sptxtyp == 12)
	symbolPatternName = "ICING_00";
    

    if ( el->elem.spt.info.sptxtyp ==4 || el->elem.spt.info.sptxtyp ==5
	|| el->elem.spt.info.sptxtyp ==11 || el->elem.spt.info.sptxtyp ==14 ) {
	mask = "true";
    }

    if ( el->elem.spt.info.sptxtyp ==3 || el->elem.spt.info.sptxtyp ==4) {
	displayType = "BOX";
    }
    else if ( el->elem.spt.info.sptxtyp ==10 || el->elem.spt.info.sptxtyp ==11) {
	displayType = "UNDERLINE";
    }
    else if ( el->elem.spt.info.sptxtyp ==13 || el->elem.spt.info.sptxtyp ==14) {
	displayType = "OVERLINE";
    } /*0, 5 are NORMAL */

    font = malloc(17*sizeof(char));
    style = malloc(9*sizeof(char));
    getFontStyle(el->elem.spt.info.itxfn, font, style) ;

//mid-lvl-cld
    if (el->elem.spt.info.sptxtyp == 15 ) {
	char *temp = (char*)(el->elem.spt.text);
	const char delimiter[] = "|";

    	result1 = strsep(&temp, delimiter); //cloud
	if ( strcmp(result1, " ") == 0 || strcmp(result1, "") == 0) //result is not NULL
	    r1 = "";
	else {
		while ( (t = strsep(&result1, "; ")) ) {
		    if (strcmp(r1, "")==0)
			r1 = t;
		    else  
			sprintf(r1, "%s%s%s", r1, "|", t); //strcat(r1,t);
		}			
	}

   	result2 = strsep(&temp, delimiter);
	if ( strcmp(result2, " ") == 0 || strcmp(result2, "") == 0)
	    r2 = "";
	else {
		while ( (t = strsep(&result2, "; ")) ) {
		    if (strcmp(r2, "")==0)
			r2 = t;
		    else
			sprintf(r2, "%s%s%s", r2, "|", t);
		}
	}

   	result3 = strsep(&temp, delimiter); //icing
	if ( strcmp(result3, "0") ==0)
	    result3 = "ICING_00";
	else if ( strcmp(result3, "1") ==0)
	    result3 = "ICING_01";
	else if ( strcmp(result3, "2") ==0)
	    result3 = "ICING_02";
	else if ( strcmp(result3, "3") ==0)
	    result3 = "ICING_03";
	else if ( strcmp(result3, "4") ==0)
	    result3 = "ICING_04";
	else if ( strcmp(result3, "5") ==0)
	    result3 = "ICING_05";
	else if ( strcmp(result3, "6") ==0)
	    result3 = "ICING_06";
	else if ( strcmp(result3, "7") ==0)
	    result3 = "ICING_07";
	else if ( strcmp(result3, "8") ==0)
	    result3 = "ICING_08";
	else
	    result3 = "";

   	result4 = strsep(&temp, delimiter);
	if ( strcmp(result4, " ") == 0 || strcmp(result4, "") == 0) {
	    result4 = "";
	    /*result3 = "";*/
	}

   	result5 = strsep(&temp, delimiter); //turb
	if ( strcmp(result5, "0") ==0)
	    result5 = "TURBULENCE_0";
	else if ( strcmp(result5, "1") == 0)
	    result5 = "TURBULENCE_1";
	else if ( strcmp(result5, "2") == 0)
	    result5 = "TURBULENCE_2";
	else if ( strcmp(result5, "3") == 0)
	    result5 = "TURBULENCE_3";
	else if ( strcmp(result5, "4") == 0)
	    result5 = "TURBULENCE_4";
	else if ( strcmp(result5, "56") == 0)
	    result5 = "TURBULENCE_4|TURBULENCE_6";
	else if ( strcmp(result5, "5") == 0)
	    result5 = "TURBULENCE_5";
	else if ( strcmp(result5, "6") == 0)
	    result5 = "TURBULENCE_6";
	else if ( strcmp(result5, "77") == 0)
	    result5 = "TURBULENCE_6|TURBULENCE_7";
	else if ( strcmp(result5, "7") == 0)
	    result5 = "TURBULENCE_7";
	else if ( strcmp(result5, "8") == 0)
	    result5 = "TURBULENCE_8";
	else
	    result5 = "";

   	result6 = strsep(&temp, delimiter);
	if ( strcmp(result6, " ") == 0 || strcmp(result6, "") == 0) {
	    result6 = "";
	    /*result5 = "";*/
	}

   	result7 = strsep(&temp, delimiter);  //thunder
	if ( strcmp(result7, " ") == 0 || strcmp(result7, "") == 0)
	    r7 = "";
	else {
		while ( (t = strsep(&result7, "; ")) ) {
		    if (strcmp(r7, "")==0)
			r7 = t;
		    else
			sprintf(r7, "%s%s%s", r7, "|", t);
		}
	}

   	result = strsep(&temp, delimiter);
	if ( strcmp(result, " ") == 0 || strcmp(result, "") == 0) {
	    result = "";
	}

    	sprintf( buffer, "%s          <MidCloudText pgenCategory=\"%s\" pgenType=\"%s\" fontSize=\"%s\" fontName=\"%s\" style=\"%s\" justification=\"%s\" iwidth=\"%d\""  " ithw=\"%d\""  " cloudAmounts=\"%s\" cloudTypes=\"%s\" icingLevels=\"%s\" icingType=\"%s\" turbulenceLevels=\"%s\" turbulenceType=\"%s\" tstormLevels=\"%s\" tstormTypes=\"%s\">\n",
indent, category(el), pgenType, getSztext(el->elem.spt.info.sztext),  
font, style, getIalign(el->elem.spt.info.ialign), el->elem.spt.info.iwidth, el->elem.spt.info.ithw,r2, r1,  result4, result3, result6, result5, result, r7);
    }

//general text
    else if (el->elem.spt.info.sptxtyp == 0 || el->elem.spt.info.sptxtyp == 3
	|| el->elem.spt.info.sptxtyp == 4 || el->elem.spt.info.sptxtyp == 5
	|| el->elem.spt.info.sptxtyp == 10 || el->elem.spt.info.sptxtyp == 11
	|| el->elem.spt.info.sptxtyp == 13 || el->elem.spt.info.sptxtyp == 14 ) {

     	if (el->hdr.grptyp ==1 ) { //change general text to mid_level text for cloud
	    char *temp = (char*)(el->elem.spt.text);
	    const char delimiter[] = "\n";
	    char cloudAmn[50]="", cloudType[50]="", tstormLevel[50]="", tstormType[50]="";
	    
	    while ((result = strsep(&temp, delimiter)) != NULL) {

		if (strcmp(result, "CU")==0 || strcmp(result, "ST")==0  /* CU|ST */
		    || strcmp(result, "SC")==0 || strcmp(result, "NS")==0
		    || strcmp(result, "AS")==0 || strcmp(result, "AC")==0
	 	    || strcmp(result, "CS")==0 || strcmp(result, "CC")==0 
		    || strcmp(result, "CI")==0)
		    sprintf( &(cloudType[strlen(cloudType)]), "%s%s", result, "|");
		else if (strcmp(result, "SKC")==0 || strcmp(result, "SCT")==0 
		    || strcmp(result, "BKN")==0 || strcmp(result, "OVC")==0
		    || strcmp(result, "LYR")==0 )
		    sprintf( &(cloudAmn[strlen(cloudAmn)]), "%s%s", result, "|");
		else if (strncmp(result, "ISO", 3)==0 || strncmp(result, "OCN", 3)==0 
		    || strncmp(result, "FRQ", 3)==0 || strncmp(result, "EMB", 3)==0 )
		    sprintf( &(tstormType[strlen(tstormType)]), "%s%s", result, "|");
	 	else if (strcmp(result, "XXX")==0 || strcmp(result, "xxx")==0)  
		    sprintf( &(tstormLevel[strlen(tstormLevel)]), "%s%s", result, "/");
		else if (strcmp(result, "CB") !=0 ) 
		    sprintf( &(tstormLevel[strlen(tstormLevel)]), "%s%s", result, "/"); 
		//  else  
		//    sprintf( &(tstormLevel[strlen(tstormLevel)]), "%s%s", result, "/");  
    	    }
		
	    /*remove ending |, / */
	    if (strlen(cloudType) >0)
		cloudType[strlen(cloudType)-1] = '\0';
	    if (strlen(cloudAmn) >0)
		cloudAmn[strlen(cloudAmn)-1] = '\0';
	    if (strlen(tstormType) >0)
		tstormType[strlen(tstormType)-1] = '\0';
	    if (strlen(tstormLevel) >0)
		tstormLevel[strlen(tstormLevel)-1] = '\0';


	    sprintf( buffer, "%s          <MidCloudText pgenCategory=\"%s\" pgenType=\"%s\" fontSize=\"%s\" fontName=\"%s\" style=\"%s\" justification=\"%s\" iwidth=\"%d\""  " ithw=\"%d\""  " cloudAmounts=\"%s\" cloudTypes=\"%s\" icingLevels=\"%s\" icingType=\"%s\" turbulenceLevels=\"%s\" turbulenceType=\"%s\" tstormLevels=\"%s\" tstormTypes=\"%s\">\n",
indent, category(el), "MID_LEVEL_CLOUD", getSztext(el->elem.spt.info.sztext), font, style,
getIalign(el->elem.spt.info.ialign), el->elem.spt.info.iwidth, el->elem.spt.info.ithw, cloudAmn, cloudType, "","","","", tstormLevel, tstormType);

	}

	else if (el->hdr.grptyp ==2) { //change general text to avn text for turb, required by cave.
	    result = strtok( el->elem.spt.text, "/\n" ); // /\n shouldn't be existed. should be / 
	    result2 = strtok( NULL, "/\n" );
		if (result2 == NULL)
	    	    result2 = "";

	    sprintf( buffer, "%s          <AvnText pgenCategory=\"%s\"" " pgenType=\"%s\"" "  symbolPatternName=\"%s\"" " fontSize=\"%s\"" " fontName=\"%s\"" " style=\"%s\"" " justification=\"%s\"" " iwidth=\"%d\""  " ithw=\"%d\""  " bottomValue=\"%s\""  " topValue=\"%s\""  " avnTextType=\"%s\">\n",
indent, category(el), "AVIATION_TEXT", symbolPatternName, getSztext(el->elem.spt.info.sztext), //symbolPatternName is default
font, style, getIalign(el->elem.spt.info.ialign),  el->elem.spt.info.iwidth, el->elem.spt.info.ithw, result2, result, avnTextType); //avnTextType is default
	}

	else {
	    /* (el->elem.spt.info.rotn -1000) is to keep drawing the same */
	    sprintf( buffer, "%s          <Text pgenCategory=\"%s\"" " pgenType=\"%s\"" "  rotation=\"%f\"" " fontSize=\"%s\"" " fontName=\"%s\"" " style=\"%s\"" " justification=\"%s\"" " rotationRelativity=\"%s\""  " mask=\"%s\""  " displayType=\"%s\""  " iwidth=\"%d\""  " ithw=\"%d\""  " hide=\"%s\""  " auto=\"%s\""  " yOffset=\"%d\""  " xOffset=\"%d\">\n",
indent, category(el), pgenType, el->elem.spt.info.rotn>=1000 ?(el->elem.spt.info.rotn -1000) :el->elem.spt.info.rotn, 
getSztext(el->elem.spt.info.sztext), font, style, getIalign(el->elem.spt.info.ialign),
el->elem.spt.info.rotn>=1000 ?"NORTH_RELATIVE" :"SCREEN_RELATIVE", mask, displayType, el->elem.spt.info.iwidth, el->elem.spt.info.ithw, "false", "true",
el->elem.spt.info.offset_y, el->elem.spt.info.offset_x );
	}
    }

//avntext
    else {
	result = strtok( el->elem.spt.text, "/\n" ); /* /\n shouldn't be existed. should be \ */
	result2 = strtok( NULL, "/\n" );
	if (result2 == NULL)
	    result2 = "XXX";


    	sprintf( buffer, "%s          <AvnText pgenCategory=\"%s\"" " pgenType=\"%s\"" "  symbolPatternName=\"%s\"" " fontSize=\"%s\"" " fontName=\"%s\"" " style=\"%s\"" " justification=\"%s\"" " iwidth=\"%d\""  " ithw=\"%d\""  " bottomValue=\"%s\""  " topValue=\"%s\""  " avnTextType=\"%s\">\n",
indent, category(el), pgenType, symbolPatternName, getSztext(el->elem.spt.info.sztext), 
font, style, getIalign(el->elem.spt.info.ialign), el->elem.spt.info.iwidth, el->elem.spt.info.ithw, result2, result, avnTextType);
    }

    free(font);
    free(style); 

// points, text and ending tag
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s            %s\n", indent, getColorTag(el->elem.spt.info.txtcol));//cave read 1st color
    
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\"" " Lon=\"%11.6f\"/>\n", indent, el->elem.spt.info.lat, checkLongitude(el->elem.spt.info.lon) );

    // ending text or textline
    if (el->elem.spt.info.sptxtyp == 15 ) {
	blen = strlen(buffer);
        sprintf( &(buffer[blen]), "%s          </MidCloudText>\n", indent);
    }

    else if (el->elem.spt.info.sptxtyp == 0 || el->elem.spt.info.sptxtyp == 3 
	|| el->elem.spt.info.sptxtyp == 4 || el->elem.spt.info.sptxtyp == 5
	|| el->elem.spt.info.sptxtyp == 10 || el->elem.spt.info.sptxtyp == 11
	|| el->elem.spt.info.sptxtyp == 13 || el->elem.spt.info.sptxtyp == 14 ) {

	if (el->hdr.grptyp ==1 ) {
	    blen = strlen(buffer);
            sprintf( &(buffer[blen]), "%s          </MidCloudText>\n", indent);
	}
	else if (el->hdr.grptyp ==2) {
	    blen = strlen(buffer);
            sprintf( &(buffer[blen]), "%s          </AvnText>\n", indent);
	}
	else {

	    result = strtok( el->elem.spt.text, "\r\n" ); //printf("result %s\n",result);
	    // check escape char 
	    char* escape = (char*)malloc(200);

	    while( result != NULL ) {
    	        blen = strlen(buffer);
		getEscape(result, escape); 
    	        sprintf( &(buffer[blen]), "%s            <textLine>%s</textLine>\n", indent, escape);
                result = strtok( NULL, "\r\n" ); 
	    }
	    free(escape);
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%s          </Text>\n", indent);
	}
    }

    else {
	blen = strlen(buffer);
        sprintf( &(buffer[blen]), "%s          </AvnText>\n", indent);
    }

    /*hdr2tag ( el, buffer, &ier );

    tstr = (char *)malloc( strlen(el->elem.spt.text)*2 * sizeof(char) );
    strcpy ( tstr, el->elem.spt.text );
    while ( strstr(tstr,"\n") != (char *)NULL )
        cst_rpst ( tstr, "\n", "$$", tstr, &ier );
    sstr[0] = CHLF;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );
    sstr[0] = CHCR;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );

    free ( tstr );
*/
}

/*=====================================================================*/

void	cir2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *  indent = "";
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  CIRCLE_ELM
     */
    if (el->hdr.grptyp == 8 )
	indent = "      ";
    

    sprintf( buffer, "          <Arc pgenCategory=\"%s\"" " pgenType=\"%s\"" "  endAngle=\"%s\" " "startAngle=\"%s\"" " axisRatio=\"%s\"" " lineWidth=\"%d\"" " sizeScale=\"%s\"" " smoothFactor=\"%d\"" " closed=\"%s\"" " filled=\"%s\"" " fillPattern=\"%s\">\n",
category(el), "Circle", "360", "0.0", "1.0",  el->elem.cir.info.width, "1.0", //el->elem.cir.info.lintyp, 
el->hdr.smooth, el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true", fill_pattern(el)
 	);

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));

    npts = 2;

    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, el->elem.cir.data.latlon[ii], checkLongitude(el->elem.cir.data.latlon[ii+npts]) );
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", indent, 0.0, 0.0 );
	printf("+ The circle has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Arc>\n");

}

/*=====================================================================*/

void	trk2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int   i, ii, npts, ipts;
size_t	blen;
char *font, *style; 
char *interval, *time;
char *text_sz;
char *option;
char *ltyp1="", *ltyp2="", *mtyp1="", *mtyp2="";
int intervalSelectedIndex;
int fontSizeSelectedIndex, fontStyleSelectedIndex,fontNameSelectedIndex;


/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  TRKSTORM_ELM
     */

/*    if (el->elem.trk.info.itxfn ==1 && el->elem.trk.info.ithw ==2 ){
        font = "Courier";
	style = "REGULAR";
	fontStyleSelectedIndex = 0;
	fontNameSelectedIndex = 0;
    }
*/
    if (el->elem.trk.info.itxfn ==1 ){
        font = "Courier";
	style = "REGULAR";
	fontStyleSelectedIndex = 0;
	fontNameSelectedIndex = 0;
    }
    else if (el->elem.trk.info.itxfn ==11 ){
        font = "Courier";
	style = "ITALIC";
	fontStyleSelectedIndex = 1;
	fontNameSelectedIndex = 0;
    }
    else if (el->elem.trk.info.itxfn ==21 ){
        font = "Courier";
	style = "BOLD";
	fontStyleSelectedIndex = 2;
	fontNameSelectedIndex = 0;
    }
    else if (el->elem.trk.info.itxfn ==31 ){
        font = "Courier";
	style = "BOLD_ITALIC";
	fontStyleSelectedIndex = 3;
	fontNameSelectedIndex = 0;
    }
    else if (el->elem.trk.info.itxfn ==2 ){
        font = "Helvetica";
	style = "REGULAR";
	fontStyleSelectedIndex = 0;
	fontNameSelectedIndex = 1;
    }
    else if (el->elem.trk.info.itxfn ==12 ){
        font = "Helvetica";
	style = "ITALIC";
	fontStyleSelectedIndex = 1;
	fontNameSelectedIndex = 1;
    }
    else if (el->elem.trk.info.itxfn ==22 ){
        font = "Helvetica";
	style = "BOLD";
	fontStyleSelectedIndex = 2;
	fontNameSelectedIndex = 1;
    }
    else if (el->elem.trk.info.itxfn ==32 ){
        font = "Helvetica";
	style = "BOLD_ITALIC";
	fontStyleSelectedIndex = 3;
	fontNameSelectedIndex = 1;
    }
    else if (el->elem.trk.info.itxfn == 3) {
        font = "Times";
	style = "REGULAR";
	fontStyleSelectedIndex = 0;
	fontNameSelectedIndex = 2;
    }
    else if (el->elem.trk.info.itxfn == 13) {
        font = "Times";
	style = "ITALIC";
	fontStyleSelectedIndex = 1;
	fontNameSelectedIndex = 2;
    }
    else if (el->elem.trk.info.itxfn == 23) {
        font = "Times";
	style = "BOLD";
	fontStyleSelectedIndex = 2;
	fontNameSelectedIndex = 2;
    }
    else if (el->elem.trk.info.itxfn == 33) {
        font = "Times";
	style = "BOLD_ITALIC";
	fontStyleSelectedIndex = 3;
	fontNameSelectedIndex = 2;
    }
    else {
        font = "Courier";
	style = "REGULAR";
	fontStyleSelectedIndex = 0;
	fontNameSelectedIndex = 0;
    }


    if (el->elem.trk.info.sztext<=0.785){ //0.71
	text_sz = "10.0";
  	fontSizeSelectedIndex = 0;
    } else if (el->elem.trk.info.sztext>0.785 && el->elem.trk.info.sztext<=0.928){ //0.86
	text_sz = "12.0";
  	fontSizeSelectedIndex = 1;
    } else if (el->elem.trk.info.sztext>0.928 && el->elem.trk.info.sztext<=1.143 ){ //1.0
	text_sz = "14.0";
  	fontSizeSelectedIndex = 2;
    } else if (el->elem.trk.info.sztext>1.143 && el->elem.trk.info.sztext<=1.5){ //1.29
	text_sz = "18.0";
  	fontSizeSelectedIndex = 3;
    } else if (el->elem.trk.info.sztext>1.5 && el->elem.trk.info.sztext<=2.071){ //1.71
	text_sz = "24.0";
  	fontSizeSelectedIndex = 4;
    } else if (el->elem.trk.info.sztext>2.071 ){ //2.43
	text_sz = "34.0";
  	fontSizeSelectedIndex = 5;
    } else {
	text_sz = "14.0";
  	fontSizeSelectedIndex = 2;
    }


    interval = (char *)malloc( 5  );
    int h = el->elem.trk.info.incr/60;
    if (h < 10)
	sprintf(interval, "%s%d%s", "0", h, ":");
    else
	sprintf(interval, "%d%s", h, ":");

    h = el->elem.trk.info.incr%60;
    if (h < 10)
	sprintf(interval, "%s%s%d", interval, "0", h);
    else
	sprintf(interval, "%s%d", interval, h);
    

  
    if (el->elem.trk.info.incr == 15)
   	intervalSelectedIndex = 0;
    else if (el->elem.trk.info.incr == 30)
   	intervalSelectedIndex = 1;
    else if (el->elem.trk.info.incr == 60)
   	intervalSelectedIndex = 2;
    else if (el->elem.trk.info.incr == 120)
   	intervalSelectedIndex = 3;
    else if (el->elem.trk.info.incr == 360)
   	intervalSelectedIndex = 4;
    else if (el->elem.trk.info.incr == 720)
   	intervalSelectedIndex = 5;
    else
   	intervalSelectedIndex = 6;

// not implemented in CAVE. hard coded
    if (el->elem.trk.info.ltype1==1 )
 	ltyp1 = "LINE_SOLID";
    if (el->elem.trk.info.ltype2==2 )
 	ltyp2 = "LINE_SOLID";
    if (el->elem.trk.info.mtype1==20)
 	mtyp1 = "FILLED_DIAMOND";
    if (el->elem.trk.info.mtype2==20)
 	mtyp2 = "FILLED_DIAMOND";

    if (el->elem.trk.info.skip==-1)
 	option = "SHOW_FIRST_LAST";
    else if (el->elem.trk.info.skip==-2)
 	option = "ON_ONE_HOUR";
    else if (el->elem.trk.info.skip==-3)
 	option = "ON_HALF_HOUR";
    else
 	option = "SKIP_FACTOR";

    char* skipFactorText = (char*)malloc(1);
    if (el->elem.trk.info.skip >=0)
	sprintf(skipFactorText, "%d",  el->elem.trk.info.skip);
    else 
	skipFactorText = "";


    sprintf( buffer, "          <Track pgenCategory=\"%s\"" " pgenType=\"%s\"" "  skipFactorTextString=\"%s\" " "setTimeButtonSelected=\"%s\"" " intervalTimeTextString=\"%s\"" " lineWidth=\"%d\"" " intervalComboSelectedIndex=\"%d\"" " initialMarker=\"%s\"" " initialLinePattern=\"%s\"" " fontStyleComboSelectedIndex=\"%d\"" " fontSizeComboSelectedIndex=\"%d\"" " fontSize=\"%s\"" " fontNameComboSelectedIndex=\"%d\"" " fontName=\"%s\"" " fontStyle=\"%s\"" " extrapMarker=\"%s\"" " extrapLinePattern=\"%s\"" " extraPointTimeDisplayOptionName=\"%s\">\n",
category(el), "STORM_TRACK", skipFactorText, "true", interval,  el->elem.trk.info.width, intervalSelectedIndex, mtyp1, ltyp1, fontStyleSelectedIndex, fontSizeSelectedIndex, text_sz, fontNameSelectedIndex, font, style, mtyp2,  ltyp2, option);

    //free(skipFactorText); //can't do free
    free(interval);

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            <initialColor>\n              %s\n            </initialColor>\n", getColorTag(el->hdr.maj_col));
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            <extrapColor>\n              %s\n            </extrapColor>\n", getColorTag(el->hdr.min_col));


    ipts = el->elem.trk.info.nipts;
    npts = el->elem.trk.info.npts;

//init points
    if  ( ipts > 0 )  {
	time = (char*)malloc(19);

	for ( ii = 0; ii < ipts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <initialPoints>\n");
	    blen = strlen(buffer);
	    if (ii >= ipts-2) {
	        blen = strlen(buffer);    
		time = time_rp(el->elem.trk.info.times[ii], time );     
		sprintf( &(buffer[blen]), "              <time>%s</time>\n", time);
  	    }

	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "              <location latitude=\"%10.6f\"", el->elem.trk.latlon[ii] );
            blen = strlen(buffer);
            sprintf( &(buffer[blen]), " longitude=\"%11.6f\"/>\n            </initialPoints>\n", checkLongitude(el->elem.trk.latlon[ii+npts]) );
	}

    	free(time);
    }
    

//extra points
    int showSize = npts- ipts;

    if  ( npts >ipts )  {
	time = (char*)malloc(19);

	for ( ii = 0; ii < showSize; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <extrapPoints>\n");

	    blen = strlen(buffer);
	    time = time_rp (el->elem.trk.info.times[ii+ipts], time );
	    sprintf( &(buffer[blen]), "              <time>%s</time>\n", time);	 

	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "              <location latitude=\"%10.6f\"", el->elem.trk.latlon[ii+ipts] );
            blen = strlen(buffer);
            sprintf( &(buffer[blen]), " longitude=\"%11.6f\"/>\n            </extrapPoints>\n", checkLongitude(el->elem.trk.latlon[ii+ipts+npts]) );
	}

	free(time);
    }

//extra points text
char *display[showSize];
int  count=1;    //count=0, display[0] ="true"
char *t ;
char *f = (char *)malloc( 2  );

    for (ii=0; ii<showSize; ii++) {
	display[ii] = "true";         //skip=0 & default
    }

    if (el->elem.trk.info.skip > 0) {  //skip>0
	if (showSize- el->elem.trk.info.skip >=2) {

	    for (i=1; i<showSize; i+= el->elem.trk.info.skip+1) { //display[0] is true
 		for (ii=0; ii< el->elem.trk.info.skip; ii++) {
		    if (count < showSize) {       //count is similar to i
	    		display[i+ii] = "false";
			count++;
	    	    }
		}
	    }

	    display[showSize-1] = "true"; //last is true
	}
    }

    else if ((el->elem.trk.info.skip) == -1) {

	for ( ii = 0; ii < showSize; ii++ )  {
	    if (ii !=0 && ii != showSize-1)
	    	display[ii] = "false";
	}
    }

    else if (el->elem.trk.info.skip== -2) {
	for ( ii = 0; ii < showSize; ii++ )  {
	    t = el->elem.trk.info.times[ii+ipts];
    	    strncpy(f, t+9, 2);

	    if (  atoi(f ) %60 !=0)
	    	display[ii] = "false";
	}
    }
    else if (el->elem.trk.info.skip== -3) {
	for ( ii = 0; ii < showSize; ii++ )  {
	    t = el->elem.trk.info.times[ii+ipts];
    	    strncpy(f, t+9, 2);

	    if ( atoi(f ) %30 !=0)
	        display[ii] = "false";
    	}
    }

    free(f);

    for ( ii = 0; ii < showSize; ii++ )  {
	blen = strlen(buffer);
        sprintf( &(buffer[blen]), "            <extraPointTimeTextDisplayIndicator>%s</extraPointTimeTextDisplayIndicator>\n", display[ii]);
	
    }
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Track>\n");

}

/*=====================================================================*/

void	sig2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *pgenType, *subType, *sol, *tem;
char *result, *result1, *result2, *result3, *result4;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  SIGINTL_ELM, SIGNCON_ELM, SIGCONV_ELM, SIGOUTL_ELM, SIGAIRM_ELM
     */

    if (el->hdr.vg_type == 27)
    	pgenType = "INTL_SIGMET";
    else if (el->hdr.vg_type == 28)
	pgenType = "NCON_SIGMET";
    else if (el->hdr.vg_type == 29)
	pgenType = "CONV_SIGMET";
    else if (el->hdr.vg_type == 30)
	pgenType = "OUTL_SIGMET";
    else if (el->hdr.vg_type == 31)
	pgenType = "AIRM_SIGMET";

    if (el->elem.sig.info.subtype == 0)
    	subType = "Area";
    else if (el->elem.sig.info.subtype == 1) {
	
	tem = malloc(11);
	if (el->elem.sig.info.sol ==0)
	    sol = "ESOL";
	else if (el->elem.sig.info.sol ==1)
	    sol = "NOF";
	else if (el->elem.sig.info.sol ==2)
	    sol = "SOF";
	else if (el->elem.sig.info.sol ==3)
	    sol = "EOF";
	else if (el->elem.sig.info.sol ==4)
	    sol = "WOF";
	else
	    sol = "";

	sprintf(tem, "%s%s", "Line:::", sol);
	subType = tem;
	
    }

    else if (el->elem.sig.info.subtype == 2)
	subType = "Isolated";
    else
	subType = "";


	char *temp = (char*)(el->elem.sig.info.tops);
	const char delimiter[] = "|";

    	result1 = strsep(&temp, delimiter); //tops
	if ( result1 ==NULL || strcmp(result1, "") == 0 || strcmp(result1, " ") == 0)
	    result1 = " ";
   	result2 = strsep(&temp, delimiter); //TO
	if ( result2 ==NULL || strcmp(result2, "") == 0 || strcmp(result2, " ") == 0)
	    result2 = " ";
    	result3 = strsep(&temp, delimiter);
	if ( result3 ==NULL || strcmp(result3, "") == 0 || strcmp(result3, " ") == 0)
	    result3 = " ";
   	result4 = strsep(&temp, delimiter); //AND
	if ( result4 ==NULL || strcmp(result4, "") == 0 || strcmp(result4, " ") == 0)
	    result4 = " ";
   	result = strsep(&temp, delimiter);
	if ( result ==NULL || strcmp(result, "") == 0 || strcmp(result, " ") == 0)
	    result = " ";


    if (el->hdr.vg_type == 27) { //sigInt
    	sprintf( buffer, "          <Sigmet pgenCategory=\"%s\" pgenType=\"%s\" width=\"%f\" type=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" editableAttrFromLine=\"%s\" editableAttrLevelText2=\"%s\" editableAttrLevelText1=\"%s\" editableAttrLevelInfo2=\"%s\" editableAttrLevelInfo1=\"%s\" editableAttrLevel=\"%s\" editableAttrPhenomDirection=\"%s\" editableAttrPhenomSpeed=\"%d\" editableAttrTrend=\"%s\" editableAttrPhenom=\"%s\" editableAttrPhenom2=\"%s\" editableAttrPhennam=\"%s\" editableAttrPhenomLat=\"%s\" editableAttrPhenomLon=\"%s\" editableAttrPhenomPressure=\"%d\" editableAttrPhenomMaxWind=\"%d\" editableAttrMovement=\"%s\" editableAttrRemarks=\"%s\" editableAttrEndTime=\"%s\" editableAttrStartTime=\"%s\" editableAttrSeqNum=\"%d\" editableAttrId=\"%s\" editableAttrStatus=\"%d\" editableAttrArea=\"%s\">\n",
category(el), pgenType, el->elem.sig.info.distance, subType,
el->elem.sig.info.linwid,  2.0, el->hdr.smooth,
el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true", fill_pattern(el),
" ", result, result3, result4, result2, result1, trimwhitespace(el->elem.sig.info.dir), 
el->elem.sig.info.spd, trimwhitespace(el->elem.sig.info.trend), 
trimwhitespace(el->elem.sig.info.phenom), trimwhitespace(el->elem.sig.info.phenom2), 
trimwhitespace(el->elem.sig.info.phennam), el->elem.sig.info.phenlat, 
el->elem.sig.info.phenlon, el->elem.sig.info.pres, el->elem.sig.info.maxwind, 
el->elem.sig.info.move, trimwhitespace(el->elem.sig.info.remarks), el->elem.sig.info.etime, 
el->elem.sig.info.stime, el->elem.sig.info.seqnum, trimwhitespace(el->elem.sig.info.msgid), 
el->elem.sig.info.status, trimwhitespace(el->elem.sig.info.area) ); 
    }

    else {
    	sprintf( buffer, "          <Sigmet pgenCategory=\"%s\" pgenType=\"%s\" width=\"%f\" type=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" editableAttrFromLine=\"%s\"  editableAttrSeqNum=\"%d\" editableAttrId=\"%s\" editableAttrArea=\"%s\">\n",
category(el), pgenType, el->elem.sig.info.distance, subType,
el->elem.sig.info.linwid,  2.0, el->hdr.smooth,
el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true", fill_pattern(el), " ",  
el->elem.sig.info.seqnum, trimwhitespace(el->elem.sig.info.msgid), 
trimwhitespace(el->elem.sig.info.area) );
}
    if (el->elem.sig.info.subtype == 1) 
	free(tem);

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.min_col));

    npts = el->elem.sig.info.npts;
    if (npts >0) {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%11.6f\"/>\n",
	el->elem.sig.latlon[ii], checkLongitude(el->elem.sig.latlon[ii+npts]) );
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	printf("+ The sigmet has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Sigmet>\n");

}

/*=====================================================================*/

void	ccf2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char    *fillPtn, *cover, *tops, *prob, *growth ;
//char    textDir[20], arrowLen[20];

/*---------------------------------------------------------------------*/

    /*
     *  SIGCCF_ELM
     */
    
    if (el->hdr.filled == 2) 		//should be 3,5,7
	fillPtn = "FILL_PATTERN_1";	
    else if (el->hdr.filled == 4)
	fillPtn = "FILL_PATTERN_3";
    else if (el->hdr.filled == 6)
	fillPtn = "FILL_PATTERN_5";
    else 
	fillPtn = "SOLID";

    if (el->elem.ccf.info.cover == 3)
	cover = "CVRG 25-39%";
    else if (el->elem.ccf.info.cover == 2)
	cover = "CVRG 40-74%";
    else if (el->elem.ccf.info.cover == 1)
	cover = "CVRG 75-100%";

    if (el->elem.ccf.info.tops == 4)
	tops = "TOPS 250-290";
    else if (el->elem.ccf.info.tops == 3)
	tops = "TOPS 300-340";
    else if (el->elem.ccf.info.tops == 2)
	tops = "TOPS 350-390";
    else if (el->elem.ccf.info.tops == 1)
	tops = "TOPS 400+";

    if (el->elem.ccf.info.prob == 3)
	prob = "CONF LOW";
    else if (el->elem.ccf.info.prob == 1)
	prob = "CONF HIGH";
    
    if (el->elem.ccf.info.growth == 4)
	growth = "GWTH -";
    else if (el->elem.ccf.info.growth == 3)
	growth = "GWTH NC";
    else if (el->elem.ccf.info.growth == 2)
	growth = "GWTH +";

    /*if (el->elem.ccf.info.subtype == 0)
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

sprintf(textDir, "%f", (atan((el->elem.ccf.info.textlon - el->elem.ccf.info.arrowlon)/(el->elem.ccf.info.textlat - el->elem.ccf.info.arrowlat)) * 180 / PI));
// arrowLen is not exactly  
sprintf(arrowLen, "%f", 1852*50*sqrt( pow(el->elem.ccf.info.textlon - el->elem.ccf.info.arrowlon, 2) 
	+ pow(el->elem.ccf.info.textlat - el->elem.ccf.info.arrowlat, 2) ));
sprintf(textDir, "%s%s%s", textDir, ":::", arrowLen); 
 


    sprintf( buffer, "          <Sigmet pgenCategory=\"%s\" pgenType=\"%s\" width=\"%f\" type=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" editableAttrFromLine=\"%s\" editableAttrPhenomDirection=\"%s\" editableAttrPhenomSpeed=\"%d\" editableAttrFreeText=\"%s\" editableAttrPhenomLon=\"%s\" editableAttrPhenomLat=\"%s\" editableAttrPhenom2=\"%s\" editableAttrPhenom=\"%s\" editableAttrEndTime=\"%s\" editableAttrStartTime=\"%s\" editableAttrLevelText2=\"%f\" editableAttrLevelText1=\"%f\" editableAttrLevelInfo2=\"%f\" editableAttrLevelInfo1=\"%f\">\n",
category(el), "CCFP_SIGMET", 18520.0, stype, 2, 2.0, el->hdr.smooth,
el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true", filled,
"CCFP_SIGMET", dir, (int)(el->elem.ccf.info.spd), "", //textDir, 
growth, prob, tops, cover, "", "", el->elem.ccf.info.textlon, 
el->elem.ccf.info.textlat, el->elem.ccf.info.arrowlon, el->elem.ccf.info.arrowlat); 
    
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));
    
    npts = el->elem.ccf.info.npts;
    if (npts > 0) {
    	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%11.6f\"/>\n",
	el->elem.ccf.latlon[ii], el->elem.ccf.latlon[ii+npts] );
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	printf("+ The ccf has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Sigmet>\n");*/
 
// line and lineMed

    if (el->elem.ccf.info.subtype == 1 || el->elem.ccf.info.subtype == 2) {
    	if (el->elem.ccf.info.subtype == 1) {   
    	    sprintf( buffer, "              <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\">\n", 
    	    "LINE_SOLID", "Lines", 2, 1.0, el->hdr.smooth, el->hdr.closed==0 ?"false":"true",
    	    el->hdr.filled==0 ?"false":"true", fillPtn, "false" 
    	    );
    
    	} else if (el->elem.ccf.info.subtype == 2) {
	
	    sprintf( buffer, "              <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\">\n", 
    	    "LINE_DASHED_3", "Lines", 2, 1.0, el->hdr.smooth, el->hdr.closed==0 ?"false":"true",
    	    el->hdr.filled==0 ?"false":"true", fillPtn, "false" 
    	    );
    	}

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->hdr.maj_col));
    
    	npts = el->elem.ccf.info.npts;
    	if  ( npts > 0 )  {
	    for ( ii = 0; ii < npts; ii++ )  {
	    	blen = strlen(buffer);
	    	sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 
		el->elem.ccf.latlon[ii], checkLongitude(el->elem.ccf.latlon[ii+npts])  );
	    }
	    	
    	} else {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	    printf("+ The ccf has zero point.\n");
    	}

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              </Line>\n");
    }

// Area
    else if (el->elem.ccf.info.subtype == 0) { 

	// print sub collection

	sprintf( buffer, "              <DECollection collectionName=\"Label\">\n                <DrawableElement>\n");
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                  <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\">\n",
	"POINTED_ARROW", "Lines", 2, 1.0, el->hdr.smooth, el->hdr.closed==0 ?"false":"true",
    	el->hdr.filled==0 ?"false":"true", fillPtn, "false" 
    	);

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                    <Color red=\"255\" green=\"255\" blue=\"255\" alpha=\"255\"/>\n");
    
    	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 
		el->elem.ccf.info.arrowlat, checkLongitude(el->elem.ccf.info.arrowlon) );
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 
		el->elem.ccf.info.textlat, checkLongitude(el->elem.ccf.info.textlon) );	
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  </Line>\n");
    
	//text in sub collection
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                  <Text pgenType=\"%s\" pgenCategory=\"%s\" fontSize=\"%s\" fontName=\"%s\" style=\"%s\" justification=\"%s\" rotation=\"%s\" rotationRelativity=\"%s\" mask=\"%s\" displayType=\"%s\" yOffset=\"%s\" xOffset=\"%s\">\n", 
    	"Text", "Text", "14.0", "Courier", "REGULAR", "CENTER", "0.0", "SCREEN_RELATIVE", "true", "BOX", "0", "0");
    	
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                    %s\n", getColorTag(el->hdr.maj_col));
    
    	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 
		el->elem.ccf.info.textlat, checkLongitude(el->elem.ccf.info.textlon)  );
	
    	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <textLine>%s</textLine>\n", tops);
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <textLine>%s</textLine>\n", growth);
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <textLine>%s</textLine>\n", prob);
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "                    <textLine>%s</textLine>\n", cover);
    	    
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  </Text>\n                </DrawableElement>\n              </DECollection>\n");

	// print Area as line
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "              <Line pgenType=\"%s\" pgenCategory=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" flipSide=\"%s\">\n", 
    	"LINE_SOLID", "Lines", 2, 1.0, el->hdr.smooth, el->hdr.closed==0 ?"false":"true",
    	el->hdr.filled==0 ?"false":"true", fillPtn, "false" 
    	);
    
	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->hdr.maj_col));
    
    	npts = el->elem.ccf.info.npts;

    	if  ( npts > 0 )  {
	    for ( ii = 0; ii < npts; ii++ )  {
	    	blen = strlen(buffer);
	    	sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 
		el->elem.ccf.latlon[ii], checkLongitude(el->elem.ccf.latlon[ii+npts])  );
	    }
	} else {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	    printf("+ The ccf has zero point.\n");
    	}
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              </Line>\n");
    }
}

/*=====================================================================*/

void	lst2tag ( VG_DBStruct *el, char *buffer, int *iret )
{

/*---------------------------------------------------------------------*/

/*    iret = 0;*/

    /*
     *  LIST_ELM
     */
/*    
    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]),

	"<subtyp>%d"
	"<mrktyp>%d"
	"<mrksiz>%f"
	"<mrkwid>%d"
	"<nitems>%d",

	el->elem.lst.info.subtyp,
	el->elem.lst.info.mrktyp,
	el->elem.lst.info.mrksiz,
	el->elem.lst.info.mrkwid,
	el->elem.lst.data.nitems

	);

    npts = el->elem.lst.data.nitems;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<item>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s,", el->elem.lst.data.item[ii] );
    }
    //  remove final comma 	
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<lat>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.lst.data.lat[ii] );
    }
    //  remove final comma 	
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<lon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.lst.data.lon[ii] );
    }
    //  remove final comma 	
    blen = strlen(buffer);
    buffer[blen-1] = '\0';*/
}

/*=====================================================================*/

void	ash2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char freeText[100];
char *fhr;
char subtype[100];
char text2[40], text4[20];
/*---------------------------------------------------------------------*/

    /*
     *  ASHCLD_ELM
     */

    fhr = "";
    if (el->elem.ash.info.fhr ==0) 
	fhr = "F00";
    else if (el->elem.ash.info.fhr ==6) 
	fhr = "F06";
    else if (el->elem.ash.info.fhr ==12) 
	fhr = "F12";
    else if (el->elem.ash.info.fhr ==18) 
	fhr = "F18";


    if (strcmp(el->elem.ash.info.flvl1, "SFC") == 0) {
    	sprintf(text2, "%s%s%s%s%s%s%s%s", el->elem.ash.info.flvl1, "/FL", el->elem.ash.info.flvl2,
	" ", el->elem.ash.info.dir, "/", el->elem.ash.info.spds, "KT");
    
    	sprintf(text4, "%s%s%s", el->elem.ash.info.flvl1, "/FL", el->elem.ash.info.flvl2);
    }
    else {
    	sprintf(text2, "%s%s%s%s%s%s%s%s%s", "FL", el->elem.ash.info.flvl1, "/FL", el->elem.ash.info.flvl2,
	" ", el->elem.ash.info.dir, "/", el->elem.ash.info.spds, "KT");

	sprintf(text4, "%s%s%s%s", "FL", el->elem.ash.info.flvl1, "/FL", el->elem.ash.info.flvl2);
    }

    
    if (el->elem.ash.info.subtype == 0)
	sprintf(subtype, "%s", "Area");
    else if (el->elem.ash.info.subtype == 1)
	sprintf(subtype, "%s", "Line:::ESOL");
    else if (el->elem.ash.info.subtype == 2)
	if (el->elem.ash.info.fhr ==0)
	    sprintf(subtype, "%s%s", "Text:::VA NOTIDENTIFIABLE FROM SATELLITE DATA\n       WINDS ", text2);
	else
	    sprintf(subtype, "%s%s", "Text:::WINDS ", text2);
    else if (el->elem.ash.info.subtype == 3)
	sprintf(subtype, "%s", "Text:::NOT AVAILABLE");
    else if (el->elem.ash.info.subtype == 4)
	sprintf(subtype, "%s%s%s", "Text:::", text4, " ASH DISSIPATING");
    else if (el->elem.ash.info.subtype == 5)
	sprintf(subtype, "%s", "Text:::ASH DISSIPATING");
    else if (el->elem.ash.info.subtype == 6)
	sprintf(subtype, "%s", "Text:::NO ASH EXPECTED");
    else
	sprintf(subtype, "%s", "Area");
 
    sprintf( freeText, "%s%s%s%s%s%s%s%s%s", fhr, ":::", el->elem.ash.info.flvl1, ":::", el->elem.ash.info.flvl2, ":::", el->elem.ash.info.dir, ":::", el->elem.ash.info.spds);


    sprintf( buffer, "          <Sigmet editableAttrFreeText=\"%s\" pgenCategory=\"%s\" pgenType=\"%s\" width=\"%f\" type=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\">\n",
freeText, category(el), "VACL_SIGMET", el->elem.ash.info.distance, subtype,
el->elem.ash.info.linwid,  2.0, el->hdr.smooth,
el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true", "FILL_PATTERN_2" ); 
    
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.min_col));

    npts = el->elem.ash.info.npts;
    if (npts > 0) {
    	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%11.6f\"/>\n",
	el->elem.ash.latlon[ii], checkLongitude(el->elem.ash.latlon[ii+npts]) );
    	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	printf("+ The volcano ash has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Sigmet>\n");
}

/*=====================================================================*/

void	vol2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char origStnVAAC[50];
/*---------------------------------------------------------------------*/
	
    /*
     *  VOLC_ELM
     */

    sprintf( origStnVAAC, "%s%s%s", el->elem.vol.info.origstn, "/", el->elem.vol.info.vaac);

    char *code = "";
    if (el->elem.vol.info.code ==201)
	code = "NORMAL";


    sprintf( buffer, "          <Volcano pgenCategory=\"%s\" pgenType=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" forecasters=\"%s\" nextAdv=\"%s\" remarks=\"%s\" obsFcstAshCloudInfo18=\"%s\" obsFcstAshCloudInfo12=\"%s\" obsFcstAshCloudInfo6=\"%s\" obsFcstAshCloudInfo=\"%s\" nil=\"%s\" obsAshTime=\"%s\" obsAshDate=\"%s\" erupDetails=\"%s\" aviColorCode=\"%s\" addInfoSource=\"%s\" infoSource=\"%s\" corr=\"%s\" advNum=\"%s\" year=\"%s\" hdrNum=\"%s\" wmoId=\"%s\" origStnVAAC=\"%s\" product=\"%s\" elev=\"%s\" area=\"%s\" txtLoc=\"%s\" number=\"%s\"  name=\"%s\" clear=\"%s\">\n",
category(el), "VOLC_SIGMET", el->elem.vol.info.width, el->elem.vol.info.size,
el->elem.vol.info.fcstrs, el->elem.vol.info.nextadv, el->elem.vol.info.remarks,
el->elem.vol.info.fcst_18, el->elem.vol.info.fcst_12, el->elem.vol.info.fcst_06, el->elem.vol.info.obsashcld, 
"" , el->elem.vol.info.obstime, el->elem.vol.info.obsdate, el->elem.vol.info.details,
"", el->elem.vol.info.addlsorc, el->elem.vol.info.infosorc, el->elem.vol.info.corr, el->elem.vol.info.advnum, 
el->elem.vol.info.year, el->elem.vol.info.hdrnum, el->elem.vol.info.wmoid, origStnVAAC, 
code, el->elem.vol.info.elev, el->elem.vol.info.area, el->elem.vol.info.location, 
el->elem.vol.info.number, el->elem.vol.info.name, "false" 
); 

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));
  
    npts = 1;
    for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", el->elem.vol.latlon[ii], checkLongitude(el->elem.vol.latlon[ii+npts]) );
	
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Volcano>\n");
}

/*=====================================================================*/

void	jet2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts;
size_t	blen;
char *result;
char * mask = "false";
char * displayType = "NORMAL";
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  JET_ELM
     */

//subtop
    sprintf( buffer, "          <DECollection pgenCategory=\"MET\" pgenType=\"JET\" collectionName=\"Jet\">\n            <DrawableElement>\n");

//sub1
    npts = el->elem.jet.nbarb;

    for ( ii = 0; ii < npts; ii++ )  {  //ends before Line
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              <DECollection collectionName=\"WindInfo\">\n                <DrawableElement>\n");

//sub2 -- barb

    	if ( el->elem.jet.barb[ii].spt.info.sptxtyp ==4 || el->elem.jet.barb[ii].spt.info.sptxtyp ==5
	|| el->elem.jet.barb[ii].spt.info.sptxtyp ==11 || el->elem.jet.barb[ii].spt.info.sptxtyp ==14 ) {
	    mask = "true";
    	} else {
	    mask = "false";
    	}	

    	if ( el->elem.jet.barb[ii].spt.info.sptxtyp ==3 || el->elem.jet.barb[ii].spt.info.sptxtyp ==4) {
	    displayType = "BOX";
    	}
    	else if ( el->elem.jet.barb[ii].spt.info.sptxtyp ==10 || el->elem.jet.barb[ii].spt.info.sptxtyp ==11) {
	    displayType = "UNDERLINE";
    	}
    	else if ( el->elem.jet.barb[ii].spt.info.sptxtyp ==13 || el->elem.jet.barb[ii].spt.info.sptxtyp ==14) {
	    displayType = "OVERLINE";
    	} /*0, 5 are NORMAL */

    	char *font = malloc(17*sizeof(char));
    	char *style = malloc(9*sizeof(char));
	float rotn;
    	getFontStyle(el->elem.jet.barb[ii].spt.info.itxfn, font, style) ;

	if (el->elem.jet.barb[ii].spt.info.rotn>=1000)
	    rotn = el->elem.jet.barb[ii].spt.info.rotn - 1000;
	else
	    rotn = el->elem.jet.barb[ii].spt.info.rotn;

	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  <Text pgenCategory=\"Text\" pgenType=\"General Text\"  rotation=\"%10.6f\" fontSize=\"%s\" fontName=\"%s\" style=\"%s\" justification=\"%s\" iwidth=\"%d\""  " ithw=\"%d\""  " rotationRelativity=\"%s\" mask=\"%s\" displayType=\"%s\" yOffset=\"%d\" xOffset=\"%d\" hide=\"%s\" auto=\"%s\">\n",
rotn, getSztext(el->elem.jet.barb[ii].spt.info.sztext), font, style, 
getIalign(el->elem.jet.barb[ii].spt.info.ialign), el->elem.jet.barb[ii].spt.info.iwidth,  el->elem.jet.barb[ii].spt.info.ithw, el->elem.jet.barb[ii].spt.info.rotn >=1000 ?"NORTH_RELATIVE" :"SCREEN_RELATIVE", 
mask, displayType, el->elem.jet.barb[ii].spt.info.offset_y, el->elem.jet.barb[ii].spt.info.offset_x, "false", "false");

	free(font);
    	free(style); 


//sub3
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                    %s\n", getColorTag(el->elem.jet.barb[ii].sptcol));
    	blen = strlen(buffer);

	/* calculate shifted text from barb */
	float lat, lon;
	float radians = rotn *PI/180;
	lon = checkLongitude(el->elem.jet.barb[ii].spt.info.lon);

	lon = el->elem.jet.barb[ii].spt.info.lon - (el->elem.jet.barb[ii].spt.info.offset_y/2) *sin(radians) - (el->elem.jet.barb[ii].spt.info.offset_x/2) *sin(radians);  
	lat = el->elem.jet.barb[ii].spt.info.lat + (el->elem.jet.barb[ii].spt.info.offset_y/2) *cos(radians) + (el->elem.jet.barb[ii].spt.info.offset_x/2) *cos(radians); 
	
	/*if (sin(radians)>=0 && cos(radians)>=0){
	lon = el->elem.jet.barb[ii].spt.info.lon - el->elem.jet.barb[ii].spt.info.offset_y/1.75 *sin(radians);  
	lat = el->elem.jet.barb[ii].spt.info.lat + el->elem.jet.barb[ii].spt.info.offset_y/1.75 *cos(radians); 
	}
	else if (sin(radians)>=0 && cos(radians)<0){
	lon = el->elem.jet.barb[ii].spt.info.lon + el->elem.jet.barb[ii].spt.info.offset_y/1.75 *cos(radians); 
	lat = el->elem.jet.barb[ii].spt.info.lat - el->elem.jet.barb[ii].spt.info.offset_y/1.75 *sin(radians);
	}
	else if (sin(radians)<0 && cos(radians)<0){
	lon = el->elem.jet.barb[ii].spt.info.lon - el->elem.jet.barb[ii].spt.info.offset_y/1.75 *sin(radians); 
	lat = el->elem.jet.barb[ii].spt.info.lat + el->elem.jet.barb[ii].spt.info.offset_y/1.75 *cos(radians);
	}	
	else if (sin(radians)<0 && cos(radians)>=0){
	lon = el->elem.jet.barb[ii].spt.info.lon + el->elem.jet.barb[ii].spt.info.offset_y/1.75 *cos(radians); 
	lat = el->elem.jet.barb[ii].spt.info.lat - el->elem.jet.barb[ii].spt.info.offset_y/1.75 *sin(radians);
	}*/	


	sprintf( &(buffer[blen]), "                    <Point Lat=\"%10.6f\"" " Lon=\"%11.6f\"/>\n", lat, lon);

    	//sprintf( &(buffer[blen]), "                    <Point Lat=\"%10.6f\"" " Lon=\"%11.6f\"/>\n",
//el->elem.jet.barb[ii].spt.info.lat, el->elem.jet.barb[ii].spt.info.lon );

    	result = strtok( el->elem.jet.barb[ii].spt.text, "\n" );
   	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                    <textLine>%s</textLine>\n", result);

    	result = strtok( NULL, "\n" );
    	if (result != NULL) {
	    char *r1 = strtok( result, "/" );
	    char *r2 = strtok( NULL, "/" );
	    char *r = (char*)malloc(strlen(el->elem.jet.barb[ii].spt.text));
	    sprintf(r, "%s%s%s", r2, "/", r1);

    	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "                    <textLine>%s</textLine>\n", r);
   	    free(r);
    	}

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  </Text>\n");


//sub2
    	blen = strlen(buffer);
	//width =801, 803
    	sprintf( &(buffer[blen]), "                  <Vector pgenCategory=\"Vector\" pgenType=\"%s\" lineWidth=\"%d\" sizeScale=\"%5.1f\" direction=\"%10.6f\" speed=\"%10.6f\" arrowHeadSize=\"%5.2f\" directionOnly=\"%s\" clear=\"%s\">\n",
"Barb", el->elem.jet.barb[ii].wnd.info.width >=100?(el->elem.jet.barb[ii].wnd.info.width%100) :el->elem.jet.barb[ii].wnd.info.width, el->elem.jet.barb[ii].wnd.info.size, el->elem.jet.barb[ii].wnd.data.spddir[1], el->elem.jet.barb[ii].wnd.data.spddir[0], el->elem.jet.barb[ii].wnd.info.hdsiz, "false", el->elem.jet.barb[ii].wnd.info.width >=800 ?"true" :"false"
 	);

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                    %s\n", getColorTag(el->elem.jet.barb[ii].wndcol));
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                    <Point Lat=\"%10.6f\" Lon=\"%11.6f\"/>\n", el->elem.jet.barb[ii].wnd.data.latlon[0], checkLongitude(el->elem.jet.barb[ii].wnd.data.latlon[1]) );

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  </Vector>\n                </DrawableElement>\n              </DECollection>\n");

    }

//sub1
    blen = strlen(buffer);

    sprintf( &(buffer[blen]), "              <Line pgenCategory=\"Lines\"" " lineWidth=\"%d\"" " sizeScale=\"%5.1f\"" " smoothFactor=\"%d\"" " closed=\"%s\"" " filled=\"%s\"" " fillPattern=\"%s\"" " pgenType=\"%s\">\n",
el->elem.jet.line.spl.info.splwid, el->elem.jet.line.spl.info.splsiz,
el->hdr.smooth, el->hdr.closed==0 ?"false":"true", el->hdr.filled==0 ?"false":"true",
fill_pattern(el), "FILLED_ARROW" 
	);

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->elem.jet.line.splcol));
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->elem.jet.line.splcol));

    npts = el->elem.jet.line.spl.info.numpts;

    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", el->elem.jet.line.spl.latlon[ii], checkLongitude(el->elem.jet.line.spl.latlon[ii+npts]) );
	}
    }
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "              </Line>\n");

//sub1
    npts = el->elem.jet.nhash;

    for ( ii = 0; ii < npts; ii++ )  {
     	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              <Vector pgenCategory=\"Vector\"" " pgenType=\"%s\"" " lineWidth=\"%d\" " "sizeScale=\"%5.1f\"" " direction=\"%10.6f\"" " speed=\"%10.6f\"" " arrowHeadSize=\"%5.2f\"" " directionOnly=\"%s\"" " clear=\"%s\">\n",
"Hash", el->elem.jet.hash[ii].wnd.info.width, el->elem.jet.hash[ii].wnd.info.size, (180 - el->elem.jet.hash[ii].wnd.data.spddir[1]), el->elem.jet.hash[ii].wnd.data.spddir[0], el->elem.jet.hash[ii].wnd.info.hdsiz, "false", el->elem.jet.hash[ii].wnd.info.wndtyp ==114 ? "true" : "false"
 	);

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                %s\n", getColorTag(el->elem.jet.hash[ii].wndcol));

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                <Point Lat=\"%10.6f\" Lon=\"%11.6f\"/>\n", el->elem.jet.hash[ii].wnd.data.latlon[0], checkLongitude(el->elem.jet.hash[ii].wnd.data.latlon[1]) );
       	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              </Vector>\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            </DrawableElement>\n          </DECollection>\n");

}

/*=====================================================================*/

void	gfa2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    /*
     *  GFA_ELM
     */
//size too small causes wrong tags
char areaType[200]="",fcstHr[200]="",lineWidth[200]="",cycle[200]="",tagDesk[200]="",status[200]="";
char areas[200]="", states[200]="", condsBegin[200]="", condsEnd[200]="", txtLayout[200]="";
char lat[200]="", lon[200]="", arrow_lat[200]="", arrow_lon[200]="", rg[200]="", type[200]="";
char top[200]="", bottom[200]="", category[200]="", frequency[200]="", contour[200]="", level[200]="", fzlRange[200]="";
char intensity[200]="", speed[200]="", dueto[200]="", severity[200]="", coverage[200]="", fzlTop[200]="", fzlBottom[200]="";
//char subType[10]="",linelm[10]="",linetype[10]="",arrowSize[10]="",txtColor[10]="",txtSize[10]="", txtFont[10]="",txtHardware[10]="",txtWidth[10]="",txtAlign[10]="";

char* str = (char*)malloc(1024*el->elem.gfa.info.nblocks); // or char str[10240]="";
for ( ii = 0; ii < el->elem.gfa.info.nblocks; ii++ ) {
	sprintf ( str, *(el->elem.gfa.info.blockPtr[ ii ]) );
	//printf("###str %s\n", str);
}

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

/*cst_gtag ( "gfa_subType", str, "", subType, &ier ); 
cst_gtag ( "gfa_linelm", str, "", linelm, &ier );
cst_gtag ( "gfa_linetype", str, "", linetype, &ier );
cst_gtag ( "gfa_arrowSize", str, "", arrowSize, &ier );
cst_gtag ( "gfa_txtColor", str, "", txtColor, &ier ); 
cst_gtag ( "gfa_txtSize", str, "", txtSize, &ier );
cst_gtag ( "gfa_txtFont", str, "", txtFont, &ier ); 
cst_gtag ( "gfa_txtHardware", str, "", txtHardware, &ier );
cst_gtag ( "gfa_txtWidth", str, "", txtWidth, &ier );
cst_gtag ( "gfa_txtAlign", str, "", txtAlign, &ier );*/ 

// Special fields for hazard
cst_gtag ( "Type", str, "", type, &ier );
cst_gtag ( "gfa_top", str, "", top, &ier );
cst_gtag ( "gfa_bottom", str, "", bottom, &ier );
cst_gtag ( "Coverage", str, "", coverage, &ier );
cst_gtag ( "Category", str, "", category, &ier );
cst_gtag ( "Frequency", str, "", frequency, &ier );
cst_gtag ( "Contour", str, "", contour, &ier );
cst_gtag ( "Level", str, "", level, &ier );
cst_gtag ( "gfa_fzlRange", str, "", fzlRange, &ier );
cst_gtag ( "Intensity", str, "", intensity, &ier );
cst_gtag ( "Speed", str, "", speed, &ier );
cst_gtag ( "DUE TO", str, "", dueto, &ier );
cst_gtag ( "Severity", str, "", severity, &ier );
cst_gtag ( "gfa_fzlTop", str, "", fzlTop, &ier );
cst_gtag ( "gfa_fzlBottom", str, "", fzlBottom, &ier );
if (fzlTop!=NULL && *fzlTop !='\0' && fzlBottom !=NULL && *fzlBottom !='\0')
    	sprintf(fzlTop, "%s%s%s", fzlTop, "/", fzlBottom);

free (str);


//check if it is Outlook
char* isOutlook = "false";
char hr[6];
char *token, *token2, *token3, *token4;

strncpy(hr, fcstHr, sizeof(fcstHr));

token = strtok (hr, "-"); 
token2 = strtok (NULL, "-"); 
//printf("token %s %s\n", token, token2);	
if (token2 != NULL) {
    token3 = strtok (token2, ":");
    token4 = strtok (NULL, ":");
    if (atoi(token3) >6)
	isOutlook = "true";
    else if (atoi(token3) ==6 && token4 != NULL && atoi(token4) >0)
	isOutlook = "true";
}	    
/*else if (token2 == NULL && token != NULL) {
    token3 = strtok (token, ":");
    token4 = strtok (NULL, ":");
    if (atoi(token3) >6)
	isOutlook = "true";	
    else if (atoi(token3) ==6 && token4 != NULL && atoi(token4) >0)
	isOutlook = "true";
}*/
//printf("fcstHr %s %s\n", fcstHr, hr);


//char tag[10]; /* tagDesk=1W tag=1 desk=W */
//strncpy( tag, tagDesk, 1);  /* desk=(tagDesk+1) */
//*(tag +1) ='\0';

//check cycle hour vs dst time
int cyc;  
char newCycle[3];
time_t* cycTime = malloc(sizeof(time_t));

//enum CYCLES_SUMMER{2, 8, 14, 20}; // must be ascending 
//enum CYCLES_WINTER{3, 9, 15, 21}; // must be ascending 

time(cycTime);
struct tm *tm_struct = localtime(cycTime);

if (cycle == NULL || strcmp(cycle, "") ==0 || strcmp(cycle, "0") ==0) {
    sprintf( newCycle, "%s", "");
    /*if (tm_struct->tm_isdst == 0)
    	sprintf( newCycle, "%s", "3");
    else
	sprintf( newCycle, "%s", "2");*/
}
	
if (tm_struct->tm_isdst == 0) {// non dst time 
    if (atoi(cycle) ==2 || atoi(cycle) ==8 || atoi(cycle) ==14 || atoi(cycle) ==20) {
	cyc = atoi(cycle) +1;
    	sprintf( newCycle, "%d", cyc);
    }
    else
    	sprintf( newCycle, "%s", cycle);
}
else if (tm_struct->tm_isdst > 0) {// dst time
    if (atoi(cycle) ==3 || atoi(cycle) ==9 || atoi(cycle) ==15 || atoi(cycle) ==21) {
    	cyc = atoi(cycle) -1;
    	sprintf( newCycle, "%d", cyc);
    }
    else
    	sprintf( newCycle, "%s", cycle);
}
free(cycTime);
//printf("fcstHr %s %s\n", fcstHr, hr);
//printf("newCycle %s\n", newCycle);

    	sprintf( buffer, "          <Gfa pgenCategory=\"%s\" pgenType=\"%s\" lineWidth=\"%s\" sizeScale=\"%5.1f\" smoothFactor=\"%d\" closed=\"%s\" filled=\"%s\" fillPattern=\"%s\" latText=\"%s\"  lonText=\"%s\" hazard=\"%s\" fcstHr=\"%s\" tag=\"%c\"  desk=\"%c\" issueType=\"%s\" cycleDay=\"%s\"  cycleHour=\"%s\" area=\"%s\" rg=\"%s\"  beginning=\"%s\" ending=\"%s\" states=\"%s\" type=\"%s\" top=\"%s\" bottom=\"%s\" coverage=\"%s\"  fzlTopBottom=\"%s\" level=\"%s\"  fzlRange=\"%s\" contour=\"%s\" severity=\"%s\"  dueto=\"%s\" speed=\"%s\" intensity=\"%s\" category=\"%s\"  frequency=\"%s\" isOutlook=\"%s\" textVor=\"%s\">\n",
"MET", "GFA", lineWidth, 1.0, el->hdr.smooth, el->hdr.closed==0 ?"false":"true", 
el->hdr.filled==0 ?"false":"true", fill_pattern(el), 
lat, lon, areaType, fcstHr, (char)tagDesk[0], (char)tagDesk[1], status, "", newCycle, areas, rg, condsBegin, condsEnd, states,
type, top, bottom, coverage, fzlTop, level, fzlRange, contour, severity, dueto, speed, intensity, category, frequency, isOutlook, "" );


    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.maj_col));
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "            %s\n", getColorTag(el->hdr.min_col));

    npts = el->elem.gfa.info.npts;

    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", el->elem.gfa.latlon[ii], checkLongitude(el->elem.gfa.latlon[ii+npts]) );
	}
    }
    else {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "            <Point Lat=\"%10.6f\" Lon=\"%10.6f\"/>\n", 0.0, 0.0 );
	printf("+ The gfa has zero point.\n");
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </Gfa>\n");

/*    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s%d", TAG_GFA_NBLOCKS, el->elem.gfa.info.nblocks );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s%d", TAG_GFA_NPTS, el->elem.gfa.info.npts );

    for ( ii = 0; ii < el->elem.gfa.info.nblocks; ii++ ) {
	strcat ( buffer, *(el->elem.gfa.info.blockPtr[ ii ]) );
    }

    npts = el->elem.gfa.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s", TAG_GFA_POINTS );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.gfa.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.gfa.latlon[ii+npts] );
    }

    //  remove final comma 
    blen = strlen(buffer);
    buffer[blen-1] = '\0';*/

}

/*=====================================================================*/

void	sgwx2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  SIGWX_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]),

	"<subtype>%d"
	"<npts>%d",

	el->elem.ccf.info.subtype,
	el->elem.ccf.info.npts
	);

    npts = el->elem.sgwx.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.sgwx.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.sgwx.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	tca2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	counter, ii, ier, blen;
char	str_tmp[ STD_STRLEN ];
char txLocation[13];
char *issue, *basin, *stormType;
char *severity, *advisType, *geoType;
char *time;
/*---------------------------------------------------------------------*/

    *iret = 0;
    ier = 0;
    str_tmp[ 0 ] = '\0';

    /*
     *  TCA_ELM
     */
    
    sprintf(txLocation, "%5.1f%s%5.1f", el->elem.tca.info.text_lon, ",", el->elem.tca.info.text_lat);

    if ( el->elem.tca.info.issueStatus =='E' || el->elem.tca.info.issueStatus =='e')
	issue = "Experimental";
    else if (el->elem.tca.info.issueStatus =='O' || el->elem.tca.info.issueStatus =='o')
	issue = "Operational";
    else if (el->elem.tca.info.issueStatus =='X' || el->elem.tca.info.issueStatus =='x')
	issue = "Experimental Operational";
    else if ( el->elem.tca.info.issueStatus =='T' || el->elem.tca.info.issueStatus =='t')
	issue = "Test";
    else
	issue = " ";

    if  (el->elem.tca.info.basin == 0)
	basin = "Atlantic";
    else if  (el->elem.tca.info.basin == 1)
	basin = "E. Pacific";
    else if  (el->elem.tca.info.basin == 2)
	basin = "C. Pacific";
    else if  (el->elem.tca.info.basin == 3)
	basin = "W. Pacific";

    if  (el->elem.tca.info.stormType == 0)
          stormType = "Hurricane";
    else if  (el->elem.tca.info.stormType == 1)
          stormType = "Tropical Storm";
    else if  (el->elem.tca.info.stormType == 2)
          stormType = "Tropical Depression";
    else if  (el->elem.tca.info.stormType == 3)
          stormType = "Subtropical Storm";
    else if  (el->elem.tca.info.stormType == 4)
          stormType = "Subtropical Depression";

  
    if (el->elem.tca.info.validTime == NULL || strcmp(el->elem.tca.info.validTime, "")==0
	|| strcmp(el->elem.tca.info.validTime, " ")==0) {

	time = "2000-01-01T00:00:00"; //default, otherwise Tca can't read
    	sprintf( buffer, "          <TCA pgenCategory=\"%s\" pgenType=\"%s\" textLocation=\"%s\" timeZone=\"%s\" advisoryTime=\"%s\" advisoryNumber=\"%s\" stormType=\"%s\" issueStatus=\"%s\" basin=\"%s\" stormName=\"%s\" stormNumber=\"%d\">\n",
category(el), "TCA", txLocation, el->elem.tca.info.timezone,
time, el->elem.tca.info.advisoryNum, stormType,
issue, basin, el->elem.tca.info.stormName, el->elem.tca.info.stormNum );
    }

    else {
	time = (char*)malloc(19);
    	sprintf( buffer, "          <TCA pgenCategory=\"%s\" pgenType=\"%s\" textLocation=\"%s\" timeZone=\"%s\" advisoryTime=\"%s\" advisoryNumber=\"%s\" stormType=\"%s\" issueStatus=\"%s\" basin=\"%s\" stormName=\"%s\" stormNumber=\"%d\">\n",
category(el), "TCA", txLocation, el->elem.tca.info.timezone,
time_rp(el->elem.tca.info.validTime, time), el->elem.tca.info.advisoryNum, stormType,
issue, basin, el->elem.tca.info.stormName, el->elem.tca.info.stormNum );

	free(time);
    }
 
//In an advisory, there are break points, they could be several islands, or an coast
    for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ ) {

    	if  (el->elem.tca.info.tcaww[ counter ].advisoryType == 0)
          advisType = "Watch";
    	else if  (el->elem.tca.info.tcaww[ counter ].advisoryType == 1)
          advisType = "Warning";

    	if  (el->elem.tca.info.tcaww[ counter ].severity == 0)
          severity = "Tropical Storm";
    	else if  (el->elem.tca.info.tcaww[ counter ].severity == 1)
          severity = "Hurricane";

    	if (el->elem.tca.info.tcaww[ counter ].specialGeog == 0)
    	  geoType = "None";
    	else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 1)
     	  geoType = "Islands";
    	else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 2)
    	  geoType = "Water";

	if (el->elem.tca.info.tcaww[ counter ].specialGeog == 0) {
	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "            <advisory>\n              <severity>%s</severity>\n", severity );
    	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "              <advisoryType>%s</advisoryType>\n", advisType);
    	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "              <geographyType>%s</geographyType>\n", geoType);

    	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "              <coast>\n");

	    //get first break point
	    sprintf(txLocation, "%f%s%f", checkLongitude(el->elem.tca.info.tcaww[ counter ].breakPnt[0].lon), ",", el->elem.tca.info.tcaww[ counter ].breakPnt[0].lat);

	    //currently put first and last break points to path
    	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "                <path>%s %f%s%f</path>\n", txLocation, checkLongitude(el->elem.tca.info.tcaww[ counter ].breakPnt[1].lon), ",", el->elem.tca.info.tcaww[ counter ].breakPnt[1].lat);
    	    blen = strlen(buffer);
    	    sprintf( &(buffer[blen]), "                <landZones>%s</landZones>\n", "");

	    blen = strlen(buffer); 	    
	    sprintf( &(buffer[blen]), "                <breakpoints official=\"%s\" location=\"%s\" country=\"%s\" state=\"%s\" name=\"%s\"/>\n",
" ", txLocation," "," ", el->elem.tca.info.tcaww[ counter ].breakPnt[0].breakPtName);

	    blen = strlen(buffer);
 	    sprintf(txLocation, "%10.6f%s%11.6f", el->elem.tca.info.tcaww[ counter ].breakPnt[1].lon, ",", el->elem.tca.info.tcaww[ counter ].breakPnt[1].lat);
	    sprintf( &(buffer[blen]), "                <breakpoints official=\"%s\" location=\"%s\" country=\"%s\" state=\"%s\" name=\"%s\"/>\n",
" ", txLocation," "," ", el->elem.tca.info.tcaww[ counter ].breakPnt[1].breakPtName);
	    blen = strlen(buffer);    	
    	    sprintf( &(buffer[blen]), "              </coast>\n            </advisory>\n");
	}

	else {
	    for ( ii = 0; ii < el->elem.tca.info.tcaww[ counter ].numBreakPts; ii++ ) {
		blen = strlen(buffer);
    	    	sprintf( &(buffer[blen]), "            <advisory>\n              <severity>%s</severity>\n", severity );
    	    	blen = strlen(buffer);
    	    	sprintf( &(buffer[blen]), "              <advisoryType>%s</advisoryType>\n", advisType);
    	    	blen = strlen(buffer);
    	    	sprintf( &(buffer[blen]), "              <geographyType>%s</geographyType>\n", geoType);

    	   	blen = strlen(buffer);
		if (el->elem.tca.info.tcaww[ counter ].specialGeog == 1)
    	    	    sprintf( &(buffer[blen]), "              <island>\n");
		else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 2)
    	    	    sprintf( &(buffer[blen]), "              <waterway>\n");

    	 	blen = strlen(buffer);
    		sprintf( &(buffer[blen]), "                <path>%f%s%f %f%s%f</path>\n", checkLongitude(el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon), ",", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat, checkLongitude(el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon), ",", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat);
    		blen = strlen(buffer);
    		sprintf( &(buffer[blen]), "                <landZones>%s</landZones>\n", "");

		blen = strlen(buffer);
 	    	sprintf(txLocation, "%10.6f%s%11.6f", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon, ",", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat);

    	    	sprintf( &(buffer[blen]), "                <breakpoint official=\"%s\" location=\"%s\" country=\"%s\" state=\"%s\" name=\"%s\"/>\n",
" ", txLocation," "," ", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName);

		blen = strlen(buffer); 
		if (el->elem.tca.info.tcaww[ counter ].specialGeog == 1)
    	    	    sprintf( &(buffer[blen]), "              </island>\n            </advisory>\n");
		else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 2)
    	    	    sprintf( &(buffer[blen]), "              </waterway>\n            </advisory>\n");
	    }
	}
/*    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "            <advisory>\n              <severity>%s</severity>\n", severity );
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              <advisoryType>%s</advisoryType>\n", advisType);
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "              <geographyType>%s</geographyType>\n", geoType);

    	blen = strlen(buffer);
    	if (el->elem.tca.info.tcaww[ counter ].specialGeog == 0)
    	    sprintf( &(buffer[blen]), "                 <coast>\n");
	else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 1)
    	    sprintf( &(buffer[blen]), "                 <island>\n");
	else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 2)
    	    sprintf( &(buffer[blen]), "                 <waterway>\n");
	else
    	    sprintf( &(buffer[blen]), "                 <None>\n");

    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  <path>%s</path>\n", "");
    	blen = strlen(buffer);
    	sprintf( &(buffer[blen]), "                  <landZones>%s</landZones>\n", "");

	if (el->elem.tca.info.tcaww[ counter ].specialGeog == 0) {
	    for ( ii = 0; ii < el->elem.tca.info.tcaww[ counter ].numBreakPts; ii++ ) {
    	    	blen = strlen(buffer);
 	    	sprintf(txLocation, "%10.6f%s%11.6f", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon, ",", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat);

    	    	sprintf( &(buffer[blen]), "                  <breakpoints official=\"%s\" location=\"%s\" country=\"%s\" state=\"%s\" name=\"%s\"/>\n",
" ", txLocation," "," ", el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName);
	    }

    	    blen = strlen(buffer);    	
    	    sprintf( &(buffer[blen]), "               </coast>\n            </advisory>\n");
	}

	else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 1) {
	    blen = strlen(buffer);
 	    sprintf(txLocation, "%10.6f%s%11.6f", el->elem.tca.info.tcaww[ counter ].breakPnt[0].lon, ",", el->elem.tca.info.tcaww[ counter ].breakPnt[0].lat);

    	    sprintf( &(buffer[blen]), "                  <breakpoint official=\"%s\" location=\"%s\" country=\"%s\" state=\"%s\" name=\"%s\"/>\n",
" ", txLocation," "," ", el->elem.tca.info.tcaww[ counter ].breakPnt[0].breakPtName);
    	    blen = strlen(buffer); 
    	    sprintf( &(buffer[blen]), "               </island>\n            </advisory>\n");
	}

	else if (el->elem.tca.info.tcaww[ counter ].specialGeog == 2) {
	    blen = strlen(buffer);
 	    sprintf(txLocation, "%10.6f%s%11.6f", el->elem.tca.info.tcaww[ counter ].breakPnt[0].lon, ",", el->elem.tca.info.tcaww[ counter ].breakPnt[0].lat);

    	    sprintf( &(buffer[blen]), "                  <breakpoint official=\"%s\" location=\"%s\" country=\"%s\" state=\"%s\" name=\"%s\"/>\n",
" ", txLocation," "," ", el->elem.tca.info.tcaww[ counter ].breakPnt[0].breakPtName);
    	    blen = strlen(buffer); 
    	    sprintf( &(buffer[blen]), "               </waterway>\n            </advisory>\n");
	}*/	
    }

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "          </TCA>\n");

}
/*void trackSkip(int* repeat, int* count, const int showSize, int skip, char display[]) 
{
    int ii;
    for (ii= count; ii <repeat+ skip; ii++) {
	if (count<showSize) {
		display[ii] = "false";
		count++;
		}
	}
	    count++;
	    repeat = count;
}*/
/*=====================================================================*/
