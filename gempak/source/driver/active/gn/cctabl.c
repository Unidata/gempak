#include "geminc.h"
#include "gemprm.h"
#include "color.h"
#include "proto_xw.h"


#define TBLDIR "colors"     /* directory of color tables */
#define TOKENDM "\t\n "
#define BUF_SIZE 80

typedef enum {
	GCNAME, GMC, RED, GREEN, BLUE
}colrInfo;

GemCmap gemCmap;

void cctabl ( char *coltbl, int *iret )
/************************************************************************
 * cctabl								*
 *									*
 * This routine checks the GEMPAK color map (default coltbl.tbl) table	*
 * existence by searching the local (current) dir and $GEMTBL/color	*
 * directory then creates the GEMPAK color map structure.		*
 *									*
 * cctabl (coltbl, iret)						*
 *									*
 * Input parameters:							*
 *	*coltbl		char	color table file       			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				   0 for successful			*
 *				  -1 for file error			*
 *				   2 for number of colors exceeded the  *
 *					maximum				*
 *									*
 **									*
 * Log:									*
 * L. Williams		1/96						*
 * G. Krueger/EAI	8/96	Changed CFL_TOPN->TBOP			*
 * E. Wehner/EAi	12/96	Remove local string reference		*
 ***********************************************************************/
{
GmcColor	*gmc;
FILE		*fp;
char		buf[BUF_SIZE], *token, fname[256];
char 		direct_nm[20];
int		ary_index, colr_elmt;
int		ier, pos;

/* ------------------------------------------------------------------ */

	ier = 0;
	*iret = 0;
	ary_index = 0;

	gmc = &gemCmap.color[0];

	/*
	 * If the table name is NULL use the default table.
	 */
	if ( coltbl )
	   strcpy(fname, coltbl);
	else
	   strcpy(fname, "coltbl.tbl");

	/*
	 * open color file
	 */
	sprintf(direct_nm, "%s", TBLDIR);
	fp = cfl_tbop(fname, direct_nm, &ier);
	if (ier != 0) {
	   *iret = -1;
	   return;
	}

	/*
	 * read and process each line
	 */
	while (fgets(buf, BUF_SIZE, fp) != NULL) {

	   if ( ary_index > MAX_GEMCTAB ) {
		gemCmap.nc = MAX_GEMCTAB;
		fclose(fp);
		*iret = 2;
		return;
	   }

	   if ((token=strtok(buf, TOKENDM)) == NULL)
		continue; /* empty line */

	   if (*token == '#' || *token == ';' || *token == '!') 
		continue; /* comments */

	   colr_elmt = 0;
	   pos = 0;

	   /*
	    * Process each color element.
	    */
	   while (token) {

		switch ( colr_elmt ) {

			case GCNAME:	/* GEMPAK color name */

			   strcpy( gmc[ary_index].gcname, token ); 
			   break;

			case GMC:	/* abbreviated color name */

			   strcpy( gmc[ary_index].abvname, token );
			   break;

			case RED:	/* red color component */

			   gmc[ary_index].red = atoi ( token );
			   break;

			case GREEN:	/* blue color component */

			   gmc[ary_index].green = atoi ( token );
			   break;

			case BLUE:	/* green color component */

			   gmc[ary_index].blue = atoi ( token );
			   break;

			default:	/* x color name */
			   strncpy( &(gmc[ary_index].xname[pos]), token,
								strlen(token));
			   pos = pos + strlen(token);
			   ++pos;
			   gmc[ary_index].xname[pos] = ' ';
			   break;
		}

		token = strtok(NULL, TOKENDM);
		colr_elmt++;
	   }

	   gmc[ary_index].xname[pos-1] = '\0';

	   /*
	    * Check if RGB is within range ( 0 - 255 ).
	    */
	   if ( ( gmc[ary_index].red >= 0 && gmc[ary_index].red <= 255 ) &&
		( gmc[ary_index].green >= 0 && gmc[ary_index].green <= 255 ) &&
	  	( gmc[ary_index].blue >= 0 && gmc[ary_index].blue <= 255 ) ) {

                        ary_index++;
                }

	} /* end of while loop */ 

	if ( fp != NULL ) fclose(fp);

	gemCmap.nc = ary_index;

	return;

}
