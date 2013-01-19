#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

#define SCALE_MIN 	0.001F
#define SCALE_MAX 	50.0F

void cds_scal ( char *fname, int *iret )
/************************************************************************
 * cds_scal								*
 *									*
 * This function sets the scaling factors for the objects.		*
 *									*
 * cds_scal ( fname, iret )						*
 *									*
 * Input parameters:							*
 *	*fname		char		Scaling factor file name 	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - successful			* 
 *			       	       -1 - file open error    		*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	        03/98						*
 * I. Durham/GSC	05/98	Changed underscore decl. to an include  *
 * F. J. Yen/NCEP	05/98	Removed pip stroke			*
 ***********************************************************************/
{
int   ier;
char  buffer[256], pname[64], ignore[20];
float val;
FILE  *fp;

/*---------------------------------------------------------------------*/

	*iret = 0;

	fp = (FILE *)cfl_tbop( fname, "pgen", &ier );
	if ( fp == NULL || ier != 0 ) {
		*iret = -1;
		return;
	}

        while ( !feof(fp) ) {

            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 && strchr(buffer, '=') ) {

                sscanf(buffer, "%s %s %f", pname, ignore, &val );

		if ( val <= SCALE_MIN || val > SCALE_MAX )
		    val = 1.0F;
		
		if ( strcmp(pname, "PIPSIZE") == 0 ) 
		    cdsPipSiz    = val;
		else if ( strcmp(pname, "FRONTSTRENGTH") == 0 )
		    cdsFrntStrngth = val;
		else if ( strcmp(pname, "SYMWIDTH") == 0 )
		    cdsSymWdth   = val;
		else if ( strcmp(pname, "SYMSIZE") == 0 )
		    cdsSymSiz    = val;
		else if ( strcmp(pname, "WINDWIDTH") == 0 )
		    cdsWindWdth  = val;
		else if ( strcmp(pname, "WINDSIZE") == 0 )
		    cdsWindSiz   = val;
		else if ( strcmp(pname, "WINDHDSIZ") == 0 )
		    cdsWindHdsiz = val;
		else if ( strcmp(pname, "LINEWIDTH") == 0 )
		    cdsLineWdth  = val;
		else if ( strcmp(pname, "SPLWIDTH") == 0 )
		    cdsSplWdth   = val;
		else if ( strcmp(pname, "SPLSIZE") == 0 )
		    cdsSplSiz    = val;
		else if ( strcmp(pname, "TEXTSIZE") == 0 )
		    cdsTxtSiz    = val;
		else if ( strcmp(pname, "TEXTWIDTH") == 0 )
		    cdsTxtWdth   = val;
		else if ( strcmp(pname, "SPTXTSIZE") == 0 )
		    cdsSptSiz    = val;
		else if ( strcmp(pname, "SPTXTWIDTH") == 0 )
		    cdsSptWdth   = val;

            }
        }

	fclose(fp);
    
}
