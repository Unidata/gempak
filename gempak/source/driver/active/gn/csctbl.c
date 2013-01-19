#include "geminc.h"
#include "gemprm.h"
#include "color.h"
#include "proto_xw.h"


#define TOKENDM "\t\n "

#define BUF_SIZE 80

typedef enum {
	CNAME, GMC, RED, GREEN, BLUE
}colorInfo;

GemColor gemColrs[GRAPH_COLORS];
int	 ngemC;

void csctbl ( char *tblnam, int *iret )
/************************************************************************
 * csctbl								*
 *									*
 * This routine checks the GEMPAK graphic color table by searching the  *
 * local (current) dir and $GEMTBL/color directory then creates the 	*
 * structure for colors currently used in driver.  Default color table	*
 * is coltbl.xwp.							*
 *									*
 * csctbl (tblnam, iret)						*
 *									*
 * Input parameters:							*
 *	*tblnam		char	color table file			*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 *				  0 for successful			*
 *		 		 -1 for file error			*
 *		  		  2 for number of colors exceeded the 	*
 *					maximum				*
 *									*
 **									*
 * Log:									*
 * L. Williams		1/96						*
 * G. Krueger/EAI	8/96	Changed CFL_TOPN->TBOP			*
 * S. Wang/GSC		11/97	Change coltbl.xw to coltbl.xwp		*
 * S. Jacobs/NCEP	12/97	Fixed check for NULL table name		*
 * T. Piper/SAIC	1/02	Initialized iret 			*
 * T. Piper/SAIC	07/04	Used CLR_DIR and CLR_TBL		*
 ***********************************************************************/
{
GmcColor	*gmc;
FILE		*fp;
char		*token, buf[BUF_SIZE], fname[256], tgcname[16];
char		tmpabv[4], tmpname[40], txname[40], xcolr[50];
int		ary_index, c_element, found, ier, ii, in_range;
int		pos, rgbval, rc;

/*---------------------------------------------------------------------*/

	rc = 0;
	ier = 0;
	*iret = 0;
	ary_index = 0;

	gmc = &gemCmap.color[0];

	/*
	 * If table name is NULL use the default name. 
	 */
	if ( tblnam && tblnam[0] != '\0' ) {
	   strcpy(fname, tblnam);
	}
	else {
	   strcpy(fname, CLR_TBL);
	}

	/*
	 * open color file
	 */
	fp = cfl_tbop(fname, CLR_DIR, &ier);
	if (ier != 0) {
	   *iret = -1;
	   return;
	}

	/*
	 * read and process each line
	 */
	while (fgets(buf, BUF_SIZE, fp) != NULL) {

	   if (ary_index > GRAPH_COLORS) {
		ngemC = GRAPH_COLORS; /* array overflow */
		fclose(fp);
		*iret = 2;
		return;
	   }	

	   if ( (token=strtok(buf, TOKENDM)) == NULL )
		continue; /* empty line */

	   if (*token == '#' || *token == ';' || *token == '!') 
		continue; /* comments */
 
	   c_element = 0;
	   in_range = 0;
	   rgbval = 1;
	   found = 0;
	   pos = 0;

	   while (token) {

		switch ( c_element ) {

			case CNAME:	/* color name */

			   strcpy( gemColrs[ary_index].name, token );
			   break;

			case GMC:       /* abbreviated color name */

			   break;

			case RED:       /* red color component */

			   if ( (*token == 'N') || (*token == 'n') )
				rgbval = 0;
			   else 
			   	gemColrs[ary_index].red = atoi ( token );
			   break;

			case GREEN:     /* green color component */

			   if ( (*token == 'N') || (*token == 'n') )
				rgbval = 0;
			   else 
			   	gemColrs[ary_index].green = atoi ( token );
			   break;

			case BLUE:      /* blue color component */

			   if ( (*token == 'N') || (*token == 'n') )
				rgbval = 0;
			   else 
			   	gemColrs[ary_index].blue = atoi ( token );
			   break;

			default:

			   strncpy( &(xcolr[pos]), token, strlen(token));
			   pos = pos + strlen(token);
			   ++pos;
			   xcolr[pos] = ' ';
			   break;


		}

		token = strtok(NULL, TOKENDM);
		c_element++;
	   }

	   xcolr[pos-1] = '\0';

	   /*
	    * if RGB is avaliable check its range
	    */
	   if ( rgbval ) {
	   	if ( ( gemColrs[ary_index].red >= 0 && 
				gemColrs[ary_index].red <= 255 ) &&
		     ( gemColrs[ary_index].green >= 0 && 
				gemColrs[ary_index].green <= 255 ) &&
		     ( gemColrs[ary_index].blue >= 0 && 
				gemColrs[ary_index].blue <= 255 ) ) {

			in_range = 1;
		}
	   }

	   /*
	    * Check if xw color name is in Gempak color table
	    */
	   for (ii=0; ii < gemCmap.nc; ii++) {

	       cst_lcuc( gemColrs[ary_index].name, tmpname, &rc );
	       cst_lcuc( gmc[ii].gcname, tgcname, &rc);
	       cst_lcuc( gmc[ii].abvname, tmpabv, &rc );
	       cst_lcuc( gmc[ii].xname, txname, &rc );

	       if ( (strcmp( tmpname, tgcname ) == 0) ||
	            (strcmp( tmpname, tmpabv ) == 0) ||
	            (strcmp( tmpname, txname ) == 0) ) {

		  found = 1;

		  if ( ( in_range ) && 
			( ( gemColrs[ary_index].red != gmc[ii].red ) ||
			  ( gemColrs[ary_index].green != gmc[ii].green ) ||
			  ( gemColrs[ary_index].blue != gmc[ii].blue ) ) ) {

			gemColrs[ary_index].index = -1;
		  }
		  else {
		     gemColrs[ary_index].index = ii;
		     gemColrs[ary_index].red = -1;
		     gemColrs[ary_index].green = -1;
		     gemColrs[ary_index].blue = -1;
		  }

		  ary_index++;
		  break;

	       }

	   } /* end for loop */

	   /*
	    * color name not found in Gempak table
	    */
	   if ( !found ) {

		/*
		 * if range is ok set index
		 */
		if (in_range) {

			gemColrs[ary_index].index = -1;
			ary_index++;

		}
	   }


   	} /* end while loop */ 

   	ngemC = ary_index;

	if ( fp != NULL ) cfl_clos ( fp, &ier );

	return;

}
